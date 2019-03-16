/* This file implements SSA-form intermediate representation generation from
function semantic trees produced in the semantic pass. Phi insertion is done
using the algorithm described in "Simple and Efficient Construction of Static
Single Assignment Form by Matthias Braun et al. */

#include "IR.h"

#include "Eval.h"
#include "Script.h"

#include "Common/Arena.h"
#include "Common/Array.h"
#include "Common/FNV.h"
#include "Common/HashTable.h"
#include "Common/Intrinsics.h"
#include "Common/Macros.h"

#include <assert.h>
#include <stdlib.h>

Array_DEFINE(BlockArray, struct IR_Block*);
Array_DEFINE(InstArray, struct IR_Inst*);
Array_DEFINE(RegArray, struct IR_Reg*);


struct IR_Block*
Papyrus_IR_CreateBlock(struct IR* ir, struct Arena* arena)
{
	struct IR_Block* block = (struct IR_Block*)
		Arena_Allocate(arena, sizeof(struct IR_Block));

	block->dummy = 0;
	block->index = (uint32_t)BlockArray_Size(&ir->blocks);
	block->flags = 0;
	block->phiCount = 0;
	Array_Init(&block->inst);
	Array_Init(&block->pred);
	block->succ[0] = NULL;
	block->succ[1] = NULL;

	BlockArray_Append(&ir->blocks, &block, Arena_CreateAllocator(arena));

	return block;
}

struct IR_Inst*
Papyrus_IR_CreateInst(struct IR* ir, struct Arena* arena,
	uint32_t opcode, struct Papyrus_Type* const* type)
{
	struct IR_Inst* inst = (struct IR_Inst*)
		Arena_Allocate(arena, sizeof(struct IR_Inst));

	inst->inst = opcode;
	inst->flags = 0;
	inst->block = NULL;

	if (type)
	{
		struct IR_Reg* reg = (struct IR_Reg*)
			Arena_Allocate(arena, sizeof(struct IR_Reg));

		reg->block = NULL;
		reg->def = inst;
		reg->index = (uint32_t)RegArray_Size(&ir->regs);
		reg->flags = 0;
		reg->type = *type;
		reg->physicalIndex = -1;
		reg->coalesce = NULL;
		reg->congruenceClass = NULL;

		if (opcode == IR_Inst_Phi)
		{
			InstArray_Append(&ir->phis, &inst, Arena_CreateAllocator(arena));

			reg->flags |= IR_RegFlags_PhiTarget;
		}

		RegArray_Append(&ir->regs, &reg, Arena_CreateAllocator(arena));

		inst->reg = reg;
	}
	else inst->reg = NULL;

	Array_Init(&inst->args);

	InstArray_Append(&ir->inst, &inst, Arena_CreateAllocator(arena));
	return inst;
}


struct IncompletePhi
{
	struct IR_Inst* phi;
	uint32_t localIndex;
};

typedef struct {
	struct {
		struct Arena ir;
		struct Arena local;
	} arenas;

	struct IR ir;

	struct IR_Block* block;

	struct Papyrus_Type** locals;

	// IndexPair -> IR_Val
	struct HashTable localTable;

	// struct IncompletePhi
	struct Array incompletePhis;
} Ctx;


struct IndexPair
{
	uint32_t a;
	uint32_t b;
};

static uintptr_t
IndexPair_Hash(const struct IndexPair* key)
{
	return Papyrus_FNV(key, sizeof(struct IndexPair));
}

static bool
IndexPair_Equal(const struct IndexPair* a, const struct IndexPair* b)
{
	return a->a == b->a && a->b == b->b;
}

static uintptr_t
U32_Hash(const uint32_t* key)
{
	return *key;
}

static bool
U32_Equal(const uint32_t* a, const uint32_t* b)
{
	return *a == *b;
}

Array_DEFINE(ValArray, struct IR_Val);

Array_DEFINE(IPhiArray, struct IncompletePhi);

HashTable_DEFINE_MAP(LocalMap, struct IndexPair,
	struct IR_Reg*, IndexPair_Hash, IndexPair_Equal);

HashTable_DEFINE_SET(U32Set, uint32_t, U32_Hash, U32_Equal);


enum
{
	BBFlags_Filled = 0x4000,
	BBFlags_Sealed = 0x8000,
};

static inline bool
IsFilled(const struct IR_Block* block)
{
	return block->flags & BBFlags_Filled;
}

static inline bool
IsSealed(const struct IR_Block* block)
{
	return block->flags & BBFlags_Sealed;
}


static struct IR_Inst*
EmitPhi(Ctx* ctx, struct IR_Block* block, struct Papyrus_Type* const* type)
{
	struct IR_Inst* inst = Papyrus_IR_CreateInst(
		&ctx->ir, &ctx->arenas.ir, IR_Inst_Phi, type);

	inst->block = block;
	inst->reg->block = block;

	InstArray_Insert(&block->inst, block->phiCount++,
		&inst, Arena_CreateAllocator(&ctx->arenas.ir));

	return inst;
}


static struct IR_Inst*
Emit(Ctx* ctx, uint32_t opcode, struct Papyrus_Type* const* type)
{
	struct IR_Block* block = ctx->block;

	assert(!IsFilled(block));
	assert(opcode != IR_Inst_Phi);

	struct IR_Inst* inst = Papyrus_IR_CreateInst(
		&ctx->ir, &ctx->arenas.ir, opcode, type);

	struct IR_Reg* reg = inst->reg;

	inst->block = block;
	if (reg != NULL)
		reg->block = block;

	InstArray_Append(&block->inst,
		&inst, Arena_CreateAllocator(&ctx->arenas.ir));

	return inst;
}

static struct IR_Val*
SetArgs(Ctx* ctx, struct IR_Inst* inst, intptr_t count)
{
	ValArray_Resize(&inst->args, count, Arena_CreateAllocator(&ctx->arenas.ir));
	return ValArray_Data(&inst->args);
}


static struct IR_Reg*
GetLocalValue(Ctx* ctx, uint32_t local, uint32_t block)
{
	struct IR_Reg** mapreg = LocalMap_Find(
		&ctx->localTable, &(struct IndexPair) { local, block });
	return mapreg ? *mapreg : NULL;
}

static void
SetLocalValue(Ctx* ctx, uint32_t local, uint32_t block, struct IR_Reg* reg)
{
	struct IR_Reg** mapreg;
	struct Papyrus_Allocator allocator =
		Arena_CreateAllocator(&ctx->arenas.local);
	LocalMap_Insert(&ctx->localTable,
		&(struct IndexPair) { local, block }, &allocator, &mapreg);
	*mapreg = reg;
}


static struct IR_Reg*
ReadLocal_(Ctx* ctx, uint32_t local, struct IR_Block* block);

static struct IR_Reg*
TryRemoveTrivialPhi(Ctx* ctx, struct IR_Inst* phi)
{
	(void)ctx;

	while (true)
	{
		struct IR_Reg* phiReg = phi->reg;

		struct IR_Reg* unique = NULL;
		Array_FOREACH(arg, arg_i, struct IR_Val, &phi->args)
		{
			struct IR_Reg* reg = arg->phi.reg;

			if (reg == unique || reg == phiReg)
				continue;

			if (unique != NULL)
				return phiReg;

			unique = reg;
		}

		phi = unique->def;
		if (phi->inst != IR_Inst_Phi)
			return unique;
	}
}

static struct IR_Reg*
AddPhiOperands(Ctx* ctx, struct IR_Inst* phi, uint32_t local)
{
	struct IR_Block* block = phi->block;
	Array_FOREACHV(pred, pred_i, struct IR_Block*, &block->pred)
	{
		struct IR_Val val;
		val.phi.block = pred;
		val.phi.reg = ReadLocal_(ctx, local, pred);

		ValArray_Append(&phi->args, &val,
			Arena_CreateAllocator(&ctx->arenas.ir));
	}
	return TryRemoveTrivialPhi(ctx, phi);
}

static struct IR_Reg*
ReadLocalRecursive(Ctx* ctx, uint32_t local, struct IR_Block* block)
{
	struct IR_Reg* reg;

	if (!IsSealed(block))
	{
		struct IR_Inst* phi = EmitPhi(ctx, block, &ctx->locals[local]);
		IPhiArray_Append(&ctx->incompletePhis,
			&(struct IncompletePhi) {
				.phi = phi,
				.localIndex = local
			}, Arena_CreateAllocator(&ctx->arenas.local));
		reg = phi->reg;
	}
	else if (BlockArray_Size(&block->pred) == 1)
	{
		return ReadLocal_(ctx, local, *BlockArray_Data(&block->pred));
	}
	else
	{
		struct IR_Inst* phi = EmitPhi(ctx, block, &ctx->locals[local]);
		SetLocalValue(ctx, local, block->index, phi->reg);
		reg = AddPhiOperands(ctx, phi, local);
	}
	SetLocalValue(ctx, local, block->index, reg);

	return reg;
}

static struct IR_Reg*
ReadLocal_(Ctx* ctx, uint32_t local, struct IR_Block* block)
{
	struct IR_Reg* reg = GetLocalValue(ctx, local, block->index);

	if (reg != NULL)
		return reg;

	return ReadLocalRecursive(ctx, local, block);
}


static struct IR_Reg*
ReadLocal(Ctx* ctx, uint32_t index)
{
	struct IR_Reg* reg = ReadLocal_(ctx, index, ctx->block);

#if 0
	if (value.type == IR_Val_Phi)
	{
		struct IR_Reg* reg = Emit(ctx, IR_Inst_Phi, true);
		ValArray_Append(&reg->args, &value, &ctx->allocator);
		value = RegVal(reg);
	}
#endif

	return reg;
}

static void
WriteLocal(Ctx* ctx, uint32_t index, struct IR_Reg* reg)
{
	SetLocalValue(ctx, index, ctx->block->index, reg);
}


static void
SealBlock(Ctx* ctx, struct IR_Block* block)
{
	assert(!IsSealed(block));

	Array_FOREACH(x, i, struct IncompletePhi, &ctx->incompletePhis)
	{
		if (x->phi->block == block)
			AddPhiOperands(ctx, x->phi, x->localIndex);
	}

	block->flags = block->flags | BBFlags_Sealed;
}

static void
Connect(Ctx* ctx, struct IR_Block* block, struct IR_Block* br)
{
	assert(!IsSealed(br));
	BlockArray_Append(&br->pred, &block,
		Arena_CreateAllocator(&ctx->arenas.ir));
}

static void
Finalize_(Ctx* ctx, struct IR_Block* block,
	struct IR_Block* br1, struct IR_Block* br2)
{
	assert(!IsFilled(block));
	block->flags = block->flags | BBFlags_Filled;

	struct IR_Block** succ = block->succ;
	if (br1 != NULL)
	{
		*succ++ = br1;
		Connect(ctx, block, br1);
	}
	if (br2 != NULL)
	{
		*succ = br2;
		Connect(ctx, block, br2);
	}
}

static inline void
Finalize(Ctx* ctx)
{
	Finalize_(ctx, ctx->block, NULL, NULL);
}


static void
Branch(Ctx* ctx, struct IR_Reg* condition,
	struct IR_Block* br1, struct IR_Block* br2)
{
	struct IR_Inst* inst = Emit(ctx, IR_Inst_Br, false);
	struct IR_Val* args = SetArgs(ctx, inst, 3);
	args[0] = RegVal(condition);
	args[1] = JmpVal(br1);
	args[2] = JmpVal(br2);

	Finalize_(ctx, ctx->block, br1, br2);
}

static void
Jump(Ctx* ctx, struct IR_Block* br)
{
	struct IR_Inst* inst = Emit(ctx, IR_Inst_Jmp, false);
	*SetArgs(ctx, inst, 1) = JmpVal(br);

	Finalize_(ctx, ctx->block, br, NULL);
}

static void
TryJump(Ctx* ctx, struct IR_Block* br)
{
	if (!IsFilled(ctx->block))
		Jump(ctx, br);
}


static struct IR_Reg*
GenerateExpr(Ctx* ctx, struct Papyrus_Expr* expr);

static struct IR_Reg*
GenerateExpr_Assign(Ctx* ctx, struct Papyrus_Expr* expr)
{
	struct IR_Reg* src = GenerateExpr(ctx, expr->assign.expr);
	switch (expr->ekind)
	{
	case Papyrus_Expr_WriteField:
		{
			struct IR_Inst* inst = Emit(ctx, IR_Inst_Store, NULL);
			struct IR_Val* args = SetArgs(ctx, inst, 2);
			args[0] = SymVal(expr->assign.symbol);
			args[1] = RegVal(src);
		}
		break;

	case Papyrus_Expr_WriteProperty:
		{
			//TODO: null object (self)
			struct IR_Reg* obj = GenerateExpr(ctx, expr->assign.object);
			struct IR_Inst* inst = Emit(ctx, IR_Inst_Set, NULL);
			struct IR_Val* args = SetArgs(ctx, inst, 3);
			args[0] = RegVal(obj);
			args[1] = SymVal(expr->assign.symbol);
			args[2] = RegVal(src);
		}
		break;
	}
	return src;
}

static struct IR_Reg*
GenerateExpr_ReadLocal(Ctx* ctx, struct Papyrus_Expr* expr)
{
	return ReadLocal(ctx, expr->localIndex);
}

static struct IR_Reg*
GenerateExpr_WriteLocal(Ctx* ctx, struct Papyrus_Expr* expr)
{
	struct IR_Reg* reg = GenerateExpr(ctx, expr->assign.expr);
	WriteLocal(ctx, expr->assign.localIndex, reg);
	return reg;
}

static struct IR_Reg*
GenerateExpr_ReadField(Ctx* ctx, struct Papyrus_Expr* expr)
{
	struct IR_Inst* inst = Emit(ctx, IR_Inst_Load, &expr->type);
	*SetArgs(ctx, inst, 1) = SymVal(expr->symbol);
	return inst->reg;
}

static struct IR_Reg*
GenerateExpr_Unary(Ctx* ctx, struct Papyrus_Expr* expr, uint32_t opcode)
{
	struct IR_Reg* src = GenerateExpr(ctx, expr->oper.expr);
	struct IR_Inst* inst = Emit(ctx, opcode, &expr->type);
	*SetArgs(ctx, inst, 1) = RegVal(src);
	return inst->reg;
}

static struct IR_Reg*
GenerateExpr_Binary(Ctx* ctx, struct Papyrus_Expr* expr, uint32_t opcode)
{
	struct IR_Reg* lhs = GenerateExpr(ctx, expr->oper.lhs);
	struct IR_Reg* rhs = GenerateExpr(ctx, expr->oper.rhs);
	struct IR_Inst* inst = Emit(ctx, opcode, &expr->type);
	struct IR_Val* args = SetArgs(ctx, inst, 2);
	args[0] = RegVal(lhs);
	args[1] = RegVal(rhs);
	return inst->reg;
}

static struct IR_Reg*
GenerateExpr_ReadArray(Ctx* ctx, struct Papyrus_Expr* expr)
{
	struct IR_Reg* arr = GenerateExpr(ctx, expr->oper.lhs);
	struct IR_Reg* idx = GenerateExpr(ctx, expr->oper.rhs);
	struct IR_Inst* inst = Emit(ctx, IR_Inst_Array_Load, &expr->type);
	struct IR_Val* args = SetArgs(ctx, inst, 2);
	args[0] = RegVal(arr);
	args[1] = RegVal(idx);
	return inst->reg;
}

static struct IR_Reg*
GenerateExpr_WriteArray(Ctx* ctx, struct Papyrus_Expr* expr)
{
	struct IR_Reg* arr = GenerateExpr(ctx, expr->assign.array);
	struct IR_Reg* idx = GenerateExpr(ctx, expr->assign.subscript);
	struct IR_Reg* src = GenerateExpr(ctx, expr->assign.expr);
	struct IR_Inst* inst = Emit(ctx, IR_Inst_Array_Store, NULL);
	struct IR_Val* args = SetArgs(ctx, inst, 3);
	args[0] = RegVal(arr);
	args[1] = RegVal(idx);
	args[2] = RegVal(src);
	return src;
}

static struct IR_Reg*
GenerateExpr_Cast(Ctx* ctx, struct Papyrus_Expr* expr)
{
	struct IR_Reg* obj = GenerateExpr(ctx, expr->cast.expr);
	struct IR_Inst* inst = Emit(ctx, IR_Inst_Cast, &expr->type);
	*SetArgs(ctx, inst, 1) = RegVal(obj);
	return inst->reg;
}

static struct IR_Reg*
GenerateExpr_Call(Ctx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* object = expr->call.object;
	struct Papyrus_Symbol* symbol = expr->call.symbol;

	struct Papyrus_Function* func = (struct Papyrus_Function*)symbol;

	struct IR_Inst* inst;
	struct IR_Val* args;
	intptr_t argIndex;

	if (object != NULL)
	{
		struct IR_Reg* obj = GenerateExpr(ctx, object);
		inst = Emit(ctx, IR_Inst_CallVirt, &func->signature.returnType);
		args = SetArgs(ctx, inst, 2 + expr->call.args.size);
		args[0] = RegVal(obj);
		args[1] = SymVal(symbol);
		argIndex = 2;

	}
	else
	{
		inst = Emit(ctx, IR_Inst_Call, &func->signature.returnType);
		args = SetArgs(ctx, inst, 1 + expr->call.args.size);
		args[0] = SymVal(symbol);
		argIndex = 1;
	}

	FOREACHV_S(arg, arg_i, &expr->call.args)
	{
		args[argIndex] = RegVal(GenerateExpr(ctx, arg));
	}

	return inst->reg;
}

static struct IR_Reg*
GenerateExpr(Ctx* ctx, struct Papyrus_Expr* expr)
{
	if (expr->flags & Papyrus_ExprFlags_Const)
	{
		struct Eval eval = Papyrus_Eval(expr, &ctx->arenas.local);

		struct IR_Val src;
		src.type = IR_Val_Imm;
		src.flags = 0;

		switch (eval.type)
		{
		case Eval_None:
			src.etype = IR_Val_Imm_None;
			break;

		case Eval_Int:
			src.etype = IR_Val_Imm_Int;
			src.imm.int_ = eval.int_;
			break;

		case Eval_Bool:
			src.etype = IR_Val_Imm_Bool;
			src.imm.bool_ = eval.bool_;
			break;

		case Eval_Float:
			src.etype = IR_Val_Imm_Float;
			src.imm.float_ = eval.float_;
			break;

		case Eval_String:
			src.etype = IR_Val_Imm_String;
			src.string_size = eval.string_size;
			src.imm.string_data = eval.string_data;
			break;
		}

		struct IR_Inst* inst = Emit(ctx, IR_Inst_Mov, &expr->type);
		*SetArgs(ctx, inst, 1) = src;
		return inst->reg;
	}

	switch (expr->kind)
	{
#define EXPR2(f, x, ...) \
	case Papyrus_Expr_ ## x: \
		return GenerateExpr_ ## f(ctx, expr, ##__VA_ARGS__);

#define EXPR1(x, ...) EXPR2(x, x, ##__VA_ARGS__)

		EXPR1(Assign);

		EXPR1(ReadLocal);
		EXPR1(WriteLocal);
		EXPR1(ReadField);

		EXPR2(Unary, Neg, IR_Inst_INeg);
		EXPR2(Unary, Not, IR_Inst_Not);

		EXPR2(Binary, Add, IR_Inst_IAdd);
		EXPR2(Binary, Sub, IR_Inst_ISub);
		EXPR2(Binary, Mul, IR_Inst_IMul);
		EXPR2(Binary, Div, IR_Inst_IDiv);
		EXPR2(Binary, Mod, IR_Inst_IMod);

		EXPR2(Binary, Eq, IR_Inst_CmpEq);
		EXPR2(Binary, Ne, IR_Inst_CmpNe);
		EXPR2(Binary, Lt, IR_Inst_CmpLt);
		EXPR2(Binary, Gt, IR_Inst_CmpGt);
		EXPR2(Binary, Le, IR_Inst_CmpLe);
		EXPR2(Binary, Ge, IR_Inst_CmpGe);

		EXPR1(ReadArray);
		EXPR1(WriteArray);

		EXPR1(Cast);
		EXPR1(Call);

#undef EXPR
#undef EXPR2
	}

	assert(false);
	return NULL;
}


static void
GenerateScope(Ctx* ctx, struct Papyrus_Scope scope);

static void
GenerateStmt_Expr(Ctx* ctx, struct Papyrus_Stmt* stmt)
{
	GenerateExpr(ctx, stmt->expr);
}

static void
GenerateStmt_Branch(Ctx* ctx, struct Papyrus_Stmt* stmt)
{
	//TODO: elif, else
	struct IR_Block* then = Papyrus_IR_CreateBlock(&ctx->ir, &ctx->arenas.ir);
	struct IR_Block* exit = Papyrus_IR_CreateBlock(&ctx->ir, &ctx->arenas.ir);

	struct IR_Reg* condition =
		GenerateExpr(ctx, stmt->branch.data[0].expr);
	Branch(ctx, condition, then, exit);

	SealBlock(ctx, then);

	ctx->block = then;
	GenerateScope(ctx, stmt->branch.data[0].scope);

	TryJump(ctx, exit);

	SealBlock(ctx, exit);
	ctx->block = exit;
}

static void
GenerateStmt_Loop(Ctx* ctx, struct Papyrus_Stmt* stmt)
{
	struct IR_Block* loop = Papyrus_IR_CreateBlock(&ctx->ir, &ctx->arenas.ir);
	struct IR_Block* exit = Papyrus_IR_CreateBlock(&ctx->ir, &ctx->arenas.ir);
	
	struct IR_Reg* condition =
		GenerateExpr(ctx, stmt->loop.expr);
	Branch(ctx, condition, loop, exit);

	ctx->block = loop;
	GenerateScope(ctx, stmt->loop.scope);

	if (!IsFilled(loop))
	{
		condition = GenerateExpr(ctx, stmt->expr);
		Branch(ctx, condition, loop, exit);
	}

	SealBlock(ctx, loop);
	ctx->block = exit;
}

static void
GenerateStmt_Return(Ctx* ctx, struct Papyrus_Stmt* stmt)
{
	struct IR_Reg* src;

	struct Papyrus_Expr* expr = stmt->expr;
	if (expr != NULL)
		src = GenerateExpr(ctx, expr);

	struct IR_Inst* inst = Emit(ctx, IR_Inst_Ret, false);
	if (expr != NULL)
		*SetArgs(ctx, inst, 1) = RegVal(src);

	//TODO: better handling of early return
	Finalize(ctx);
}

static void
GenerateStmt(Ctx* ctx, struct Papyrus_Stmt* stmt)
{
	switch (stmt->kind)
	{
#define STMT(x) \
	case Papyrus_Stmt_ ## x: \
		GenerateStmt_ ## x(ctx, stmt); \
		break

		STMT(Expr);
		STMT(Branch);
		STMT(Loop);
		STMT(Return);
#undef STMT
	}
}

static void
GenerateScope(Ctx* ctx, struct Papyrus_Scope scope)
{
	FOREACHV_S(x, i, &scope)
	{
		GenerateStmt(ctx, x);
	}
}

struct IR*
Papyrus_GenerateIR(struct Papyrus_Function* function,
	struct Arena* arena, struct Papyrus_ArenaPool* pool)
{
	Ctx ctx;
	ctx.arenas.ir = *arena;
	Arena_Init(&ctx.arenas.local, pool);

	Array_Init(&ctx.ir.blocks);
	Array_Init(&ctx.ir.regs);
	Array_Init(&ctx.ir.inst);
	Array_Init(&ctx.ir.phis);

	ctx.locals = function->locals.data;
	
	HashTable_Init(&ctx.localTable);
	Array_Init(&ctx.incompletePhis);

	ctx.block = Papyrus_IR_CreateBlock(&ctx.ir, &ctx.arenas.ir);
	ctx.block->flags |= BBFlags_Sealed;

	FOREACH_S(x, x_i, &function->signature.paramTypes)
	{
		struct IR_Inst* inst = Emit(&ctx, IR_Inst_Arg, x);
		WriteLocal(&ctx, (uint32_t)x_i, inst->reg);
	}
	GenerateScope(&ctx, function->scope);

	struct IR* ir = (struct IR*)
		Arena_Allocate(&ctx.arenas.ir, sizeof(struct IR));
	*ir = ctx.ir;

	Arena_Destroy(&ctx.arenas.local);
	*arena = ctx.arenas.ir;
	return ir;
}
