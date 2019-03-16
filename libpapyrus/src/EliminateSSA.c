/* This file implements IR SSA-form elimination and physical register
allocation. First liveness analysis is performed using the by-variable
algorithm described in "Computing Liveness Sets for SSA-Form Programs" by
Florian Brander et al. The live-out sets produced in the previous step are then
used in construction of the virtual register interference graph. With both the
liveness sets and the interference graph available, phi resource interference
can be eliminated using the third method described in "Translating Out of
Static Single Assignment Form" by Vugranam C. Sreedhar et al. After
interference between phi resources has been eliminated, they can be coalesced
and finally physical register allocation is performed using greedy graph
coloring. */

#include "EliminateSSA.h"

#include "IR.h"

#include "Common/Arena.h"
#include "Common/Array.h"
#include "Common/Intrinsics.h"
#include "Common/Macros.h"

#include <stdlib.h>
#include <stdio.h>

Array_DEFINE(ArrayArray, struct Array);
Array_DEFINE(PhysRegArray, struct IR_PhysReg);
Array_DEFINE(BlockArray, struct IR_Block*);
Array_DEFINE(InstArray, struct IR_Inst*);
Array_DEFINE(RegArray, struct IR_Reg*);
Array_DEFINE(ValArray, struct IR_Val);


enum { Bits = sizeof(uintptr_t) * 8 };

typedef struct BitSet BitSet;

static inline bool
BitSet_Get(const BitSet* set, intptr_t index)
{
	const uintptr_t* word = (const uintptr_t*)((const char*)set + index / 8);
	return *word & (uintptr_t)1 << (index % (8 * sizeof(uintptr_t)));

	//return *((uintptr_t*)set + index / Bits) & ((uintptr_t)1 << (index % Bits));
}

static inline void
BitSet_Set(BitSet* set, intptr_t index)
{
	uintptr_t* word = (uintptr_t*)((char*)set + index / 8);
	*word |= (uintptr_t)1 << (index % (8 * sizeof(uintptr_t)));

	//*((uintptr_t*)set + index / Bits) |= ((uintptr_t)1 << (index % Bits));
}

static inline void
BitSet_Unset(BitSet* set, intptr_t index)
{
	uintptr_t* word = (uintptr_t*)((char*)set + index / 8);
	*word &= ~((uintptr_t)1 << (index % (8 * sizeof(uintptr_t))));

	//*((uintptr_t*)set + index / Bits) &= ~(1 << (index % Bits));
}

static inline void
BitSet_Zero(BitSet* set, uintptr_t setSize)
{
	memset(set, 0, setSize);
}

static inline void
BitSet_Copy(BitSet* dst, const BitSet* src, uintptr_t setSize)
{
	memcpy(dst, src, setSize);
}

static inline void
BitSet_Union(BitSet* dst, const BitSet* src, uintptr_t setSize)
{
	for (uintptr_t i = 0; i < setSize; ++i)
		((uint8_t*)dst)[i] |= ((const uint8_t*)src)[i];
}

static inline bool
BitSet_CheckIntersection(const BitSet* set1,
	const BitSet* set2, uintptr_t setSize)
{
	for (uintptr_t i = 0; i < setSize; ++i)
		if (((uint8_t*)set1)[i] & ((uint8_t*)set2)[i])
			return true;
	return false;
}

struct BitSet_Iterator
{
	const uintptr_t* set;
	uintptr_t size;
	uintptr_t index;
	uintptr_t mask;
};
typedef struct BitSet_Iterator BitSet_Iterator;


#if 1
typedef struct BitMat BitMat;

static inline BitSet*
BitMat_Row(BitMat* mat, uintptr_t rowShift, intptr_t row)
{
	return (BitSet*)((char*)mat + (row << rowShift));
}

static inline void
BitMat_Zero(BitMat* mat, uintptr_t rowShift, intptr_t rowCount)
{
	memset(mat, 0, rowCount << rowShift);
}
#endif


typedef struct {
	struct {
		struct Arena ir;
		struct Arena local;
	} arenas;

	struct IR ir;

	intptr_t maxRegCount;

	// left-shift for computing matrix row offsets
	uintptr_t rowShift;

	// size of a matrix row in bytes
	uintptr_t rowSize;

	struct Reg* regs;

	BitSet* unresolvedRegisterSet;
	BitMat* unresolvedNeighbourMatrix;
	BitSet* candidateResourceSet;

	// IR_Reg*
	struct Array candidateResources;

	// IR_Reg*
	struct Array unresolved;

	struct CongruenceClass* congruenceClassList;

	struct {
		struct CongruenceClass* congruenceClass;
	} freeList;
} Ctx;

struct CongruenceClass
{
	struct {
		struct CongruenceClass* prev;
		struct CongruenceClass* next;
	} list;

	struct IR_Reg* singleton;
	struct Array regs;
	BitSet* congruenceSet;
	BitSet* interferenceSet;
};

#if 1
static BitMat*
CreateRegisterMatrix(Ctx* ctx, intptr_t rowCount)
{
	uintptr_t matSize = (uintptr_t)rowCount << ctx->rowShift;
	void* mat = Arena_Allocate(&ctx->arenas.local, matSize);

	memset(mat, 0, matSize);

	return (BitMat*)mat;
}
#endif

static BitSet*
CreateRegisterSet(Ctx* ctx)
{
	uintptr_t size = ctx->rowSize;
	void* set = Arena_Allocate(&ctx->arenas.local, size);
	memset(set, 0, size);
	return (BitSet*)set;
}

static struct IR_Val*
SetArgs(Ctx* ctx, struct IR_Inst* inst, intptr_t count)
{
	ValArray_Resize(&inst->args, count,
		Arena_CreateAllocator(&ctx->arenas.ir));
	return ValArray_Data(&inst->args);
}

static struct CongruenceClass*
CreateCongruenceClass(Ctx* ctx)
{
	struct CongruenceClass* class = ctx->freeList.congruenceClass;
	if (class != NULL)
	{
		//TODO: bitset freelist
		ctx->freeList.congruenceClass = *(struct CongruenceClass**)class;
		Array_Clear(&class->regs);
	}
	else
	{
		//TODO: proper alloc
		class = (struct CongruenceClass*)malloc(sizeof(struct CongruenceClass));
		Array_Init(&class->regs);
	}
	class->singleton = NULL;
	class->congruenceSet = NULL;
	class->interferenceSet = NULL;

	class->list.prev = NULL;
	if (ctx->congruenceClassList != NULL)
		ctx->congruenceClassList->list.prev = class;
	class->list.next = ctx->congruenceClassList;
	ctx->congruenceClassList = class;

	return class;
}

static void
DeleteCongruenceClass(Ctx* ctx, struct CongruenceClass* class)
{
	if (ctx->congruenceClassList == class)
	{
		struct CongruenceClass* next = class->list.next;
		ctx->congruenceClassList = next;

		if (next != NULL)
			next->list.prev = NULL;
	}
	else
	{
		struct CongruenceClass* prev = class->list.prev;
		struct CongruenceClass* next = class->list.next;

		if (prev != NULL) prev->list.next = next;
		if (next != NULL) next->list.prev = prev;
	}
}


static intptr_t
BinarySearch(void** ptrs, intptr_t size, void* ptr)
{
	if (size == 0)
		return 0;

	if (size == 1)
		return ptrs[0] > ptr ? 0 : 1;

	intptr_t i = 0;
	for (intptr_t s = size; s > 1;)
	{
		intptr_t half = s >> 1;
		bool b = ptrs[i + half] > ptr;

		i = b ? i : i + half;
		s = b ? s - half : s;
	}
	return i;
}

static void
LiveSet_Insert(Ctx* ctx, BitSet* liveSet,
	struct Array* live, struct IR_Reg* reg)
{
	intptr_t regIndex = reg->index;
	if (!BitSet_Get(liveSet, regIndex))
	{
		BitSet_Set(liveSet, regIndex);

		intptr_t index = BinarySearch(
			(void**)RegArray_Data(live), RegArray_Size(live), reg);

		RegArray_Insert(live, index, &reg,
			Arena_CreateAllocator(&ctx->arenas.local));
	}
}

static void
LiveSet_Remove(BitSet* liveSet,
	struct Array* live, struct IR_Reg* reg)
{
	intptr_t regIndex = reg->index;
	if (BitSet_Get(liveSet, regIndex))
	{
		BitSet_Unset(liveSet, regIndex);

		intptr_t index = BinarySearch(
			(void**)RegArray_Data(live), RegArray_Size(live), reg);

		RegArray_Remove(live, index);
	}
}


static struct Array*
BuildUsageLists(Ctx* ctx)
{
	intptr_t regCount = RegArray_Size(&ctx->ir.regs);

	struct Array* usageLists =
		(struct Array*)Arena_Allocate(
			&ctx->arenas.local, sizeof(struct Array) * regCount);

	FOREACH(arr, arr_i, usageLists, regCount)
	{
		Array_Init(arr);
	}

	//struct Arena_Pos pos = Arena_GetPos(&ctx->arenas.local);

	BitMat* usageMatrix = CreateRegisterMatrix(ctx,
		BlockArray_Size(&ctx->ir.blocks));

	uintptr_t rowShift = ctx->rowShift;

	// for each phi
	Array_FOREACHV(phi, phi_i, struct IR_Inst*, &ctx->ir.phis)
	{
		Array_FOREACH(arg, arg_i, struct IR_Val, &phi->args)
		{
			struct IR_Block* block = arg->phi.block;

			BitSet* usageSet = BitMat_Row(usageMatrix, rowShift, block->index);

			intptr_t srcIndex = arg->phi.reg->index;
			if (!BitSet_Get(usageSet, srcIndex))
			{
				BitSet_Set(usageSet, srcIndex);

				struct IR_Block* tagged =
					(struct IR_Block*)((uintptr_t)block | 1);

				BlockArray_Append(&usageLists[srcIndex],
					&tagged, Arena_CreateAllocator(&ctx->arenas.local));
			}
		}
	}

	// for each non-phi
	Array_FOREACHV(inst, inst_i, struct IR_Inst*, &ctx->ir.inst)
	{
		if (inst->inst == IR_Inst_Phi)
			continue;

		struct IR_Block* block = inst->block;
		
		BitSet* usageSet = BitMat_Row(
			usageMatrix, rowShift, block->index);

		Array_FOREACH(arg, arg_i, struct IR_Val, &inst->args)
		{
			if (arg->type != IR_Val_Reg)
				continue;

			intptr_t argIndex = arg->reg->index;
			if (!BitSet_Get(usageSet, argIndex))
			{
				BitSet_Set(usageSet, argIndex);

				BlockArray_Append(&usageLists[argIndex],
					&block, Arena_CreateAllocator(&ctx->arenas.local));
			}
		}
	}

	BitMat_Zero(usageMatrix, rowShift,
		BlockArray_Size(&ctx->ir.blocks));

	//Arena_SetPos(&ctx->arenas.local, pos);
	return usageLists;
}


static void
AnalyzeLiveness_UpAndMark(Ctx* ctx, struct IR_Block* block, struct IR_Reg* reg)
{
	bool local = reg->block == block;
	if (local && (reg->flags & IR_RegFlags_PhiTarget) == 0)
		return;

	intptr_t regIndex = reg->index;
	BitSet* liveInSet = block->liveInSet;
	if (BitSet_Get(liveInSet, regIndex))
		return;

	// LiveIn[block] += reg
	LiveSet_Insert(ctx, liveInSet, &block->liveIn, reg);

	if (local) return;

	Array_FOREACHV(pred, pred_i, struct IR_Block*, &block->pred)
	{
		// LiveOut[pred] += reg
		LiveSet_Insert(ctx, pred->liveOutSet, &pred->liveOut, reg);

		AnalyzeLiveness_UpAndMark(ctx, pred, reg);
	}
}

static void
AnalyzeLiveness(Ctx* ctx)
{
	//struct Arena_Pos pos = Arena_GetPos(&ctx->arenas.local);

	struct Array* usageLists = BuildUsageLists(ctx);

	Array_FOREACHV(block, block_i, struct IR_Block*, &ctx->ir.blocks)
	{
		block->liveInSet = CreateRegisterSet(ctx);
		Array_Init(&block->liveIn);

		block->liveOutSet = CreateRegisterSet(ctx);
		Array_Init(&block->liveOut);
	}

	Array_FOREACHV(reg, reg_i, struct IR_Reg*, &ctx->ir.regs)
	{
		intptr_t regIndex = reg->index;

		struct Array* usageList = &usageLists[regIndex];

		Array_FOREACHV(tagged, tagged_i, struct IR_Block*, usageList)
		{
			struct IR_Block* block =
				(struct IR_Block*)((uintptr_t)tagged & -2);

			if ((uintptr_t)tagged & 1)
			{
				// LiveOut[block] += reg
				LiveSet_Insert(ctx, block->liveOutSet, &block->liveOut, reg);
			}

			AnalyzeLiveness_UpAndMark(ctx, block, reg);
		}
	}

	//Arena_SetPos(&ctx->arenas.local, pos);
}


static void
InitializeInterference(Ctx* ctx, struct IR_Reg* reg)
{
	reg->interferenceSet = CreateRegisterSet(ctx);
	Array_Init(&reg->interference);
}

static void
BuildInterference(Ctx* ctx)
{
	uintptr_t rowSize = ctx->rowSize;

	//struct Arena_Pos pos = Arena_GetPos(&ctx->arenas.local);

	intptr_t regCount = RegArray_Size(&ctx->ir.regs);
	struct IR_Reg** live = (struct IR_Reg**)Arena_Allocate(
		&ctx->arenas.local, sizeof(struct IR_Reg*) * regCount);

	intptr_t* liveIndices = (intptr_t*)Arena_Allocate(
		&ctx->arenas.local, sizeof(intptr_t) * regCount);

	BitSet* liveSet = CreateRegisterSet(ctx);

	Array_FOREACHV(block, block_i, struct IR_Block*, &ctx->ir.blocks)
	{
		BitSet_Copy(liveSet, block->liveOutSet, rowSize);

		Array_FOREACHV(x, i, struct IR_Reg*, &block->liveOut)
		{
			live[i] = x;
			liveIndices[x->index] = i;
		}
		intptr_t liveCount = RegArray_Size(&block->liveOut);

		struct IR_Inst** insts = InstArray_Data(&block->inst);
		for (intptr_t i = InstArray_Size(&block->inst); i-- > 0;)
		{
			struct IR_Inst* inst = insts[i];
			struct IR_Reg* reg = inst->reg;

			if (reg != NULL)
			{
				// live -= reg
				intptr_t regIndex = reg->index;
				if (BitSet_Get(liveSet, regIndex))
				{
					BitSet_Unset(liveSet, regIndex);

					struct IR_Reg* last = live[liveCount - 1];

					live[regIndex] = last;
					liveIndices[last->index] = regIndex;
					--liveCount;
				}

				InitializeInterference(ctx, reg);

				// interference[reg] += live
				BitSet_Copy(reg->interferenceSet, liveSet, rowSize);

				Array_Append(&reg->interference, live,
					liveCount * sizeof(struct IR_Reg*), Arena_CreateAllocator(&ctx->arenas.local));
			}

			if (inst->inst != IR_Inst_Phi)
			{
				// for each reg used in the instruction
				Array_FOREACH(arg, arg_i, struct IR_Val, &inst->args)
				{
					if (arg->type != IR_Val_Reg)
						continue;

					struct IR_Reg* use = arg->reg;

					// live += reg
					intptr_t useIndex = use->index;
					if (!BitSet_Get(liveSet, useIndex))
					{
						BitSet_Set(liveSet, useIndex);

						live[liveCount] = use;
						liveIndices[useIndex] = liveCount;
						++liveCount;
					}
				}
			}
		}
	}

	Array_FOREACHV(reg, reg_i, struct IR_Reg*, &ctx->ir.regs)
	{
		intptr_t regIndex = reg->index;
		Array_FOREACHV(x, x_i, struct IR_Reg*, &reg->interference)
		{
			BitSet* set = x->interferenceSet;
			if (!BitSet_Get(set, regIndex))
			{
				// interference[x] += reg
				BitSet_Set(set, regIndex);
				RegArray_Append(&x->interference,
					&reg, Arena_CreateAllocator(&ctx->arenas.local));
			}
		}
	}

	//Arena_SetPos(&ctx->arenas.local, pos);
}


static bool
CheckPhiUsage(struct IR_Block* block, struct IR_Reg* reg)
{
	FOREACHV(inst, inst_i, InstArray_Data(&block->inst), block->phiCount)
	{
		Array_FOREACH(arg, arg_i, struct IR_Val, &inst->args)
		{
			if (arg->phi.reg == reg)
				return true;
		}
	}
	return false;
}

static struct IR_Inst*
CreateInst(Ctx* ctx, uint32_t opcode, struct Papyrus_Type* type)
{
	assert(RegArray_Size(&ctx->ir.regs) < ctx->maxRegCount);

	struct IR_Inst* inst = Papyrus_IR_CreateInst(
		&ctx->ir, &ctx->arenas.ir, opcode, &type);

	return inst;
}


struct UnresolvedNeighbour
{
	struct IR_Reg* reg;
	struct IR_Reg* neighbour;
};

static void
AddInterferenceEdges(Ctx* ctx, struct IR_Reg* reg, struct Array* live)
{
	intptr_t regIndex = reg->index;
	BitSet* interferenceSet = reg->interferenceSet;

	// for each reg in live
	Array_FOREACHV(x, i, struct IR_Reg*, live)
	{
		if (x == reg)
			continue;

		intptr_t index = x->index;
		if (!BitSet_Get(interferenceSet, index))
		{
			// interference[reg] += x
			BitSet_Set(interferenceSet, index);
			RegArray_Append(&reg->interference, &x, Arena_CreateAllocator(&ctx->arenas.local));

			// interference[x] += reg
			BitSet_Set(x->interferenceSet, regIndex);
			RegArray_Append(&x->interference, &reg, Arena_CreateAllocator(&ctx->arenas.local));
		}
	}
}

static bool
CheckCongruenceClassSetIntersection(
	struct CongruenceClass* class, BitSet* set, uintptr_t setSize)
{
	struct IR_Reg* singleton = class->singleton;
	if (singleton != NULL)
	{
		return BitSet_Get(set, singleton->index);
	}
	else
	{
		return BitSet_CheckIntersection(class->congruenceSet, set, setSize);
	}
}

static void
AddUnresolvedRegister(Ctx* ctx, struct IR_Reg* xi, struct IR_Reg* xj)
{
	BitSet* neighbourSet = BitMat_Row(
		ctx->unresolvedNeighbourMatrix, ctx->rowShift, xi->index);

	intptr_t xj_index = xj->index;
	if (!BitSet_Get(neighbourSet, xj_index))
	{
		BitSet_Set(neighbourSet, xj_index);
		if (xi->unresolvedNeighbourCount++ == 0)
		{
			RegArray_Append(&ctx->unresolved, &xi, Arena_CreateAllocator(&ctx->arenas.local));
		}
	}
}

static void
HandlePhiResourcePair(Ctx* ctx,
	struct IR_Reg* xi, struct IR_Block* li,
	struct IR_Reg* xj, struct IR_Block* lj)
{
	if (xi == xj)
		return;

	struct CongruenceClass* xic = xi->congruenceClass;
	struct CongruenceClass* xjc = xj->congruenceClass;

	uintptr_t rowSize = ctx->rowSize;

	if (!BitSet_CheckIntersection(
		xic->interferenceSet, xjc->interferenceSet, rowSize))
	{
		return;
	}

	bool xi_lj_intersection =
		CheckCongruenceClassSetIntersection(xic, lj->liveOutSet, rowSize);

	bool xj_li_intersection =
		CheckCongruenceClassSetIntersection(xjc, li->liveOutSet, rowSize);

	if (xi_lj_intersection || xj_li_intersection)
	{
		if (xi_lj_intersection)
		{
			intptr_t index = xi->index;
			if (!BitSet_Get(ctx->candidateResourceSet, index))
			{
				RegArray_Append(&ctx->candidateResources,
					&xi, Arena_CreateAllocator(&ctx->arenas.local));

				BitSet_Set(ctx->candidateResourceSet, index);
			}
		}

		if (xj_li_intersection)
		{
			intptr_t index = xj->index;
			if (!BitSet_Get(ctx->candidateResourceSet, index))
			{
				RegArray_Append(&ctx->candidateResources,
					&xj, Arena_CreateAllocator(&ctx->arenas.local));

				BitSet_Set(ctx->candidateResourceSet, index);
			}
		}
	}
	else
	{
		AddUnresolvedRegister(ctx, xi, xj);
		AddUnresolvedRegister(ctx, xj, xi);
	}
}

static int
CompareUnresolvedRegisters(const struct IR_Reg* a, const struct IR_Reg* b)
{
	uint32_t ac = a->unresolvedNeighbourCount;
	uint32_t bc = b->unresolvedNeighbourCount;

	if (ac > bc) return -1;
	if (ac < bc) return 1;

	return 0;
}

static void
ProcessUnresolvedRegisters(Ctx* ctx)
{
	uintptr_t rowShift = ctx->rowShift;
	uintptr_t rowSize = ctx->rowSize;

	qsort(RegArray_Data(&ctx->unresolved),
		RegArray_Size(&ctx->unresolved), sizeof(struct IR_Reg*),
		(int(*)(const void*, const void*))&CompareUnresolvedRegisters);

	BitSet* candidateResourceSet = ctx->candidateResourceSet;

	BitSet* unresolvedSet = ctx->unresolvedRegisterSet;
	BitMat* neighbourMatrix = ctx->unresolvedNeighbourMatrix;
	Array_FOREACHV(reg, reg_i, struct IR_Reg*, &ctx->unresolved)
	{
		intptr_t reg_index = reg->index;

		BitSet* neighbourSet = BitMat_Row(
			neighbourMatrix, rowShift, reg_index);

		if (BitSet_CheckIntersection(unresolvedSet, neighbourSet, rowSize))
		{
			BitSet_Unset(unresolvedSet, reg_index);
		}

		reg->unresolvedNeighbourCount = 0;
	}
	Array_FOREACHV(reg, reg_i, struct IR_Reg*, &ctx->unresolved)
	{
		intptr_t reg_index = reg->index;

		if (!BitSet_Get(unresolvedSet, reg_index))
			continue;

		BitSet* neighbourSet = BitMat_Row(
			neighbourMatrix, rowShift, reg_index);

		if (BitSet_CheckIntersection(unresolvedSet, neighbourSet, rowSize))
		{
			if (!BitSet_Get(candidateResourceSet, reg_index))
			{
				RegArray_Append(&ctx->candidateResources,
					&reg, Arena_CreateAllocator(&ctx->arenas.local));
			}
		}
	}

	Array_Clear(&ctx->unresolved);
	BitSet_Zero(unresolvedSet, rowSize);
	BitMat_Zero(neighbourMatrix, rowShift, ctx->maxRegCount);
}

static struct CongruenceClass*
CreateSingletonCongruenceClass(Ctx* ctx, struct IR_Reg* reg)
{
	struct CongruenceClass* class = CreateCongruenceClass(ctx);

	class->singleton = reg;
	class->interferenceSet = reg->interferenceSet;

	return class;
}

static void
InsertCopy(Ctx* ctx, struct IR_Reg* reg, struct IR_Inst* phi)
{
	struct IR_Inst* inst = reg->def;

	if (reg == phi->reg)
	{
		struct IR_Block* block = reg->block;

		struct IR_Inst* new = CreateInst(ctx, IR_Inst_Mov, reg->type);

		struct IR_Reg* newReg = new->reg;
		InitializeInterference(ctx, newReg);

		new->block = block;
		newReg->block = block;
		InstArray_Insert(&block->inst, block->phiCount++,
			&new, Arena_CreateAllocator(&ctx->arenas.ir));

		SWAP(&inst->reg, &new->reg);
		SWAP(&reg->def, &newReg->def);

		*SetArgs(ctx, new, 1) = RegVal(newReg);

		// CongruenceClass[newReg] = { newReg }
		newReg->congruenceClass = CreateSingletonCongruenceClass(ctx, newReg);

		BitSet* liveInSet = block->liveInSet;

		// LiveIn[block] -= reg
		LiveSet_Remove(liveInSet, &block->liveIn, reg);

		// LiveIn[block] += new
		LiveSet_Insert(ctx, liveInSet, &block->liveIn, reg);

		// add interference edges between new and LiveOut[block]
		AddInterferenceEdges(ctx, newReg, &block->liveOut);
	}
	else
	{
		// for each phi source register
		Array_FOREACH(arg, arg_i, struct IR_Val, &phi->args)
		{
			if (arg->phi.reg != reg)
				continue;

			struct IR_Block* block = arg->phi.block;

			struct IR_Inst* new = CreateInst(ctx, IR_Inst_Mov, reg->type);

			struct IR_Reg* newReg = new->reg;
			InitializeInterference(ctx, newReg);

			*SetArgs(ctx, new, 1) = RegVal(reg);

			new->block = block;
			//TODO: make sure all blocks end in jmp, br, or ret
			InstArray_Insert(&block->inst, InstArray_Size(
				&block->inst) - 1, &new, Arena_CreateAllocator(&ctx->arenas.local));

			arg->phi.reg = newReg;

			// CongruenceClass[new] = { new }
			newReg->congruenceClass =
				CreateSingletonCongruenceClass(ctx, newReg);

			// LiveOut[block] += new
			LiveSet_Insert(ctx, block->liveOutSet, &block->liveOut, newReg);

			intptr_t regIndex = reg->index;
			// Find reg usage in successors
			for (intptr_t i = 0; i < 2; ++i)
			{
				struct IR_Block* succ = block->succ[i];
				if (succ == NULL) break;

				if (BitSet_Get(block->liveInSet, regIndex)
					|| CheckPhiUsage(succ, reg))
				{
					goto used_in_succ;
				}
			}

			// not used in succ: LiveOut[block] -= reg
			LiveSet_Remove(block->liveOutSet, &block->liveOut, reg);

		used_in_succ:
			// add interference edges between new and LiveOut[block]
			AddInterferenceEdges(ctx, newReg, &block->liveOut);
		}
	}
}

static void
CongruenceClass_Add(Ctx* ctx,
	struct CongruenceClass* class, struct IR_Reg* reg)
{
	assert(class->singleton == NULL);

	BitSet* congruenceSet = class->congruenceSet;
	assert(congruenceSet != NULL);

	intptr_t regIndex = reg->index;
	if (!BitSet_Get(congruenceSet, regIndex))
	{
		BitSet_Set(congruenceSet, regIndex);
		RegArray_Append(&class->regs, &reg, Arena_CreateAllocator(&ctx->arenas.local));

		BitSet* interferenceSet = class->interferenceSet;
		assert(congruenceSet != NULL);

		BitSet_Union(interferenceSet, reg->interferenceSet, ctx->rowSize);
	}
}

static void
CongruenceClass_Union(Ctx* ctx,
	struct CongruenceClass* dst, struct CongruenceClass* src)
{
	struct IR_Reg* singleton = src->singleton;
	if (singleton != NULL)
	{
		CongruenceClass_Add(ctx, dst, singleton);
	}
	else
	{
		Array_FOREACHV(reg, reg_i, struct IR_Reg*, &src->regs)
		{
			CongruenceClass_Add(ctx, dst, reg);
		}
	}
}

static void
EliminatePhiResourceInterference(Ctx* ctx)
{
	uintptr_t rowSize = ctx->rowSize;

	BitSet* candidateResourceSet = CreateRegisterSet(ctx);
	ctx->candidateResourceSet = candidateResourceSet;

	ctx->unresolvedRegisterSet = CreateRegisterSet(ctx);

	ctx->unresolvedNeighbourMatrix =
		CreateRegisterMatrix(ctx, ctx->maxRegCount);
	
	Array_Init(&ctx->candidateResources);
	Array_Init(&ctx->unresolved);

	// for each phi resource R: CongruenceClass[R] = { R }
	Array_FOREACHV(phi, phi_i, struct IR_Inst*, &ctx->ir.phis)
	{
		{
			struct IR_Reg* reg = phi->reg;
			if (reg->congruenceClass == NULL)
			{
				reg->congruenceClass =
					CreateSingletonCongruenceClass(ctx, reg);
			}
		}

		Array_FOREACH(arg, arg_i, struct IR_Val, &phi->args)
		{
			struct IR_Reg* reg = arg->phi.reg;
			if (reg->congruenceClass == NULL)
			{
				reg->congruenceClass =
					CreateSingletonCongruenceClass(ctx, reg);
			}
		}
	}

	Array_FOREACHV(block, block_i, struct IR_Block*, &ctx->ir.blocks)
	{
		FOREACHV(phi, phi_i, InstArray_Data(&block->inst), block->phiCount)
		{
			struct IR_Reg* reg = phi->reg;

			assert(ValArray_Size(&phi->args) > 1);

			Array_FOREACH(arg, arg_i, struct IR_Val, &phi->args)
			{
				HandlePhiResourcePair(ctx, reg,
					block, arg->phi.reg, arg->phi.block);
			}

			{
				struct IR_Val* args = ValArray_Data(&phi->args);
				intptr_t argCount = ValArray_Size(&phi->args);

				for (intptr_t i = 0, c = argCount - 1; i < c; ++i)
				{
					struct IR_Reg* xi = args[i].phi.reg;
					struct IR_Block* li = args[i].phi.block;

					for (intptr_t j = i + 1; j < argCount; ++j)
					{
						//TODO: some are handled twice...
						HandlePhiResourcePair(ctx, xi, li,
							args[j].phi.reg, args[j].phi.block);
					}
				}
			}

			ProcessUnresolvedRegisters(ctx);

			Array_FOREACHV(candidate, candidate_i,
				struct IR_Reg*, &ctx->candidateResources)
			{
				InsertCopy(ctx, candidate, phi);
			}

			struct CongruenceClass* newClass = CreateCongruenceClass(ctx);
			newClass->congruenceSet = CreateRegisterSet(ctx);
			newClass->interferenceSet = CreateRegisterSet(ctx);

			// newClass += congruenceClass[reg]
			CongruenceClass_Union(ctx, newClass, reg->congruenceClass);
			DeleteCongruenceClass(ctx, reg->congruenceClass);

			// congruenceClass[reg] = newClass
			reg->congruenceClass = newClass;

			// for each phi source register
			Array_FOREACH(arg, arg_i, struct IR_Val, &phi->args)
			{
				struct IR_Reg* src = arg->reg;

				struct CongruenceClass* class = src->congruenceClass;

				// newClass += congruenceClass[src]
				CongruenceClass_Union(ctx, newClass, class);

				// congruenceClass[src] = newClass
				src->congruenceClass = newClass;

				DeleteCongruenceClass(ctx, class);
			}
		}

		BitSet_Zero(candidateResourceSet, rowSize);
		//BitMat_Zero(ctx->unresolvedNeighbourMatrix, ctx->rowShift);
		Array_Clear(&ctx->candidateResources);
	}

	// delete singleton classes
	for (struct CongruenceClass* class =
		ctx->congruenceClassList; class != NULL;)
	{
		struct CongruenceClass* next = class->list.next;
		struct IR_Reg* singleton = class->singleton;
		if (singleton != NULL)
		{
			struct CongruenceClass* prev = class->list.prev;

			if (prev != NULL) prev->list.next = next;
			if (next != NULL) next->list.prev = prev;

			singleton->congruenceClass = NULL;
		}
		class = next;
	}

	//Arena_SetPos(&ctx->arenas.local, pos);
}

static struct IR_Reg**
ComputeRegisterOrdering(Ctx* ctx)
{
	struct IR_Reg** regs = RegArray_Data(&ctx->ir.regs);
	intptr_t regCount = RegArray_Size(&ctx->ir.regs);

	struct IR_Reg** ordering = (struct IR_Reg**)Arena_Allocate(
		&ctx->arenas.local, sizeof(struct IR_Reg*) * regCount);

	//struct Arena_Pos pos = Arena_GetPos(&ctx->arenas.local);

	BitSet* visited = CreateRegisterSet(ctx);

	intptr_t queueSize = regCount * 2;
	struct IR_Reg** queue = (struct IR_Reg**)Arena_Allocate(
		&ctx->arenas.local, sizeof(struct IR_Reg*) * queueSize);

	intptr_t index = 0;
	FOREACHV(root, root_i, regs, regCount)
	{
		intptr_t rootIndex = root->index;

		if (BitSet_Get(visited, rootIndex))
			continue;

		intptr_t head = 0;
		intptr_t tail = 0;

		queue[head++] = root;
		while (head != tail)
		{
			struct IR_Reg* reg = queue[tail++];
			tail = tail < queueSize ? tail : 0;

			intptr_t regIndex = reg->index;
			if (BitSet_Get(visited, regIndex))
				continue;

			BitSet_Set(visited, regIndex);
			ordering[index++] = reg;

			Array_FOREACHV(x, x_i, struct IR_Reg*, &reg->interference)
			{
				if (BitSet_Get(visited, x->index))
					continue;

				queue[head++] = x;
				head = head < queueSize ? head : 0;
			}
		}
	}

	//Arena_SetPos(&ctx->arenas.local, pos);

	return ordering;
}

static void
AllocatePhysicalRegisters(Ctx* ctx)
{
	//struct Arena_Pos pos = Arena_GetPos(&ctx->arenas.local);

	struct IR_Reg** regs = ComputeRegisterOrdering(ctx);
	intptr_t regCount = RegArray_Size(&ctx->ir.regs);

	struct Array physRegs;
	Array_Init(&physRegs);

	uintptr_t setSize = ctx->rowSize;
	BitSet* set = CreateRegisterSet(ctx);
	FOREACHV(reg, reg_i, regs, regCount)
	{
		struct Papyrus_Type* regType = reg->type;

		BitSet_Zero(set, setSize);
		Array_FOREACHV(x, x_i, struct IR_Reg*, &reg->interference)
		{
			intptr_t index = x->physicalIndex;
			if (x->physicalIndex != -1u)
			{
				BitSet_Set(set, index);
			}
		}

		for (uintptr_t offset = 0;
			offset < setSize; offset += sizeof(uintptr_t))
		{
			uintptr_t word = ~*(uintptr_t*)((char*)set + offset);

			intptr_t wordIndex = (intptr_t)(offset * 8);
			for (uintptr_t mask = ~(uintptr_t)0, wm; (wm = word & mask) != 0;)
			{
				intptr_t bitIndex = bsf64(wm);
				intptr_t index = wordIndex + bitIndex;

				struct IR_PhysReg* physReg;
				if (index < PhysRegArray_Size(&physRegs))
				{
					physReg = &PhysRegArray_Data(&physRegs)[index];
					if (physReg->type != regType)
					{
						mask = ~(uintptr_t)0 << (bitIndex + 1);
						continue;
					}
				}
				else
				{
					physReg = PhysRegArray_Extend(&physRegs, 1,
						Arena_CreateAllocator(&ctx->arenas.ir));

					physReg->parameter = false;
					physReg->type = regType;
				}

				struct IR_Reg** classRegs;
				intptr_t classRegCount;

				struct CongruenceClass* class = reg->congruenceClass;
				if (class != NULL)
				{
					assert(class->singleton == NULL);
					classRegs = RegArray_Data(&class->regs);
					classRegCount = RegArray_Size(&class->regs);
				}
				else
				{
					classRegs = &reg;
					classRegCount = 1;
				}

				FOREACHV(n, n_i, classRegs, classRegCount)
				{
					n->physicalIndex = (uint32_t)index;

					if (n->def->inst == IR_Inst_Arg)
						physReg->parameter = true;
				}

				goto reg_assigned;
			}
		}
	reg_assigned:;
	}

	ctx->ir.physRegs = physRegs;

	//Arena_SetPos(&ctx->arenas.local, pos);
}

void
Papyrus_EliminateSSA(struct IR* ir,
	struct Arena* irArena, struct Papyrus_ArenaPool* pool)
{
	Ctx ctx;
	ctx.arenas.ir = *irArena;
	Arena_Init(&ctx.arenas.local, pool);

	ctx.ir = *ir;

	Array_Init(&ctx.candidateResources);

	ctx.congruenceClassList = NULL;
	ctx.maxRegCount = RegArray_Size(&ctx.ir.regs) * 2;

	ctx.freeList.congruenceClass = NULL;
	
	assert(ctx.maxRegCount > 0);
	// Lowest power of two multiple of sizeof(uintptr_t)
	// with no fewer bits than maxRegCount
	ctx.rowSize = MAX((~((uintptr_t)-1 >> 1) >>
		(clz64(ctx.maxRegCount) - 1)) / 8, sizeof(uintptr_t));

	ctx.rowShift = bsf64(ctx.rowSize);

	AnalyzeLiveness(&ctx);
	BuildInterference(&ctx);
	EliminatePhiResourceInterference(&ctx);
	AllocatePhysicalRegisters(&ctx);

	*ir = ctx.ir;

	Arena_Destroy(&ctx.arenas.local);
	*irArena = ctx.arenas.ir;
}
