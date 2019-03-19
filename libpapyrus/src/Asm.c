#include "Asm.h"

#include "EliminateSSA.h"
#include "IR.h"
#include "Script.h"

#include "Common/Array.h"
#include "Common/Arena.h"
#include "Common/HashTable.h"
#include "Common/Macros.h"
#include "Common/StringHash.h"

#include <stdint.h>
#include <stdio.h>

#define Ctx AsmCtx

Array_DEFINE(U32Array, uint32_t);
Array_DEFINE(InstArray, struct Asm_Inst*);
Array_DEFINE(TypeArray, struct Papyrus_Type*);
Array_DEFINE(IR_ValArray, struct IR_Val);
Array_DEFINE(IR_InstArray, struct IR_Inst*);
Array_DEFINE(IR_BlockArray, struct IR_Block*);
Array_DEFINE(IR_PhysRegArray, struct IR_PhysReg);

typedef struct {
	struct {
		struct Papyrus_ArenaPool* pool;
		struct Arena asm;
		struct Arena local;
		struct Arena global;
	} arenas;

	// Papyrus_String
	struct Array strings;

	// Papyrus_String -> uint32_t
	struct HashTable stringTable;

	uint32_t selfString;

	struct {
		// uint32_t stringIndex
		struct Array varStrings;

		// uint32_t
		uint32_t* blockOffsets;
	} func;
} Ctx;

Array_DEFINE(StringArray, struct Papyrus_String);

HashTable_DEFINE_MAP(StringMap,
	struct Papyrus_String, uint32_t, String_Hash, String_Equal);

#define ALLOCATE(arena, type, n) \
	((type*)Arena_Allocate((arena), sizeof(type) * (n)))

static void*
AllocateAsm_(Ctx* ctx, uintptr_t size)
{
	return Arena_Allocate(&ctx->arenas.asm, size);
}

#define AllocateAsm(ctx, type, count) \
	((type*)AllocateAsm_((ctx), sizeof(type) * (count)))

static uintptr_t
CommitArray_(Ctx* ctx, struct Array* array, void** out)
{
	uintptr_t size = Array_Size(array);
	void* data = Arena_Allocate(&ctx->arenas.asm, size);
	memcpy(data, Array_Data(array), size);
	*out = data;
	return size;
}

#define CommitArray(ctx, array, out) \
	((void)((out)->size = (intptr_t)(CommitArray_((ctx), \
		(array), (void**)&(out)->data) / sizeof(*(out)->data))))

static uint32_t
AllocateString(Ctx* ctx, struct Papyrus_String string)
{
	struct Papyrus_Allocator allocator =
		Arena_CreateAllocator(&ctx->arenas.global);

	uint32_t index;
	uint32_t* mapval;
	if (StringMap_Insert(&ctx->stringTable, &string, &allocator, &mapval))
	{
		*mapval = index = (uint32_t)StringArray_Size(&ctx->strings);
		StringArray_Append(&ctx->strings, &string, allocator);
	}
	else index = *mapval;
	return index;
}

static void
ReserveRegisterStrings(Ctx* ctx, intptr_t regCount)
{
	intptr_t varStringCount = U32Array_Size(&ctx->func.varStrings);

	if (regCount <= varStringCount)
		return;

	U32Array_Resize(&ctx->func.varStrings, regCount,
		Arena_CreateAllocator(&ctx->arenas.global));

	uint32_t* indices = U32Array_Data(&ctx->func.varStrings);
	for (intptr_t i = varStringCount; i < regCount; ++i)
	{
		struct Papyrus_String string;

		char buffer[64];
		string.size = sprintf(buffer, "::v%u", (uint32_t)i);
		string.data = AllocateAsm(ctx, char, string.size);
		memcpy((char*)string.data, buffer, string.size);

		indices[i] = AllocateString(ctx, string);
	}
}

static uint32_t
GetRegisterString(Ctx* ctx, uint32_t registerIndex)
{
	return U32Array_Data(&ctx->func.varStrings)[registerIndex];
}

static uint32_t
GetTypeString(Ctx* ctx, struct Papyrus_Type* type)
{
	assert(type->type != Papyrus_TypeFlags_Array);
	return AllocateString(ctx, type->symbol->name);
}

static uint32_t
GetSelfString(Ctx* ctx)
{
	uint32_t index = ctx->selfString;
	if (index == -1u)
	{
		index = AllocateString(ctx, Papyrus_String_CREATE("self"));
		ctx->selfString = index;
	}
	return index;
}


static struct Asm_Arg
DstArg(Ctx* ctx, struct IR_Inst* ir)
{
	assert(ir->reg != NULL);
	return (struct Asm_Arg) {
		.type = Asm_Arg_Reg,
		.index = GetRegisterString(ctx, ir->reg->physicalIndex),
	};
}

static struct Asm_Arg
RegArg(Ctx* ctx, struct IR_Reg* reg)
{
	return (struct Asm_Arg) {
		.type = Asm_Arg_Reg,
		.index = GetRegisterString(ctx, reg->physicalIndex),
	};
}

static struct Asm_Arg
SymArg(Ctx* ctx, struct Papyrus_Symbol* symbol)
{
	assert(symbol->kind != Papyrus_Symbol_Extern);
	return (struct Asm_Arg) {
		.type = Asm_Arg_Symbol,
		.index = AllocateString(ctx, symbol->name),
	};
}

static struct Asm_Arg
JmpArg(Ctx* ctx, struct IR_Block* block)
{
	(void)ctx;
	return (struct Asm_Arg) {
		.type = Asm_Arg_Index,
		.label = true,
		.index = block->index,
	};
}

static struct Asm_Arg
ValArg(Ctx* ctx, struct IR_Val ir)
{
	switch (ir.type)
	{
	case IR_Val_Imm:
		switch (ir.etype)
		{
		case IR_Val_Imm_None:
			assert(false); //TODO: implement NONE local var
			return (struct Asm_Arg) {
				.type = Asm_Arg_Symbol,
				.index = AllocateString(ctx, Papyrus_String_CREATE("::none")),
			};

		case IR_Val_Imm_Self:
			return (struct Asm_Arg) {
				.type = Asm_Arg_Index,
				.index = GetSelfString(ctx),
			};

		case IR_Val_Imm_Int:
			return (struct Asm_Arg) {
				.type = Asm_Arg_Int,
				.int_ = ir.imm.int_,
			};

		case IR_Val_Imm_Bool:
			return (struct Asm_Arg) {
				.type = Asm_Arg_Bool,
				.bool_ = ir.imm.bool_,
			};

		case IR_Val_Imm_Float:
			return (struct Asm_Arg) {
				.type = Asm_Arg_Float,
				.float_ = ir.imm.float_,
			};

		case IR_Val_Imm_String:
			return (struct Asm_Arg) {
				.type = Asm_Arg_String,
				.index = AllocateString(ctx, IR_Val_GetString(&ir)),
			};
		}

	case IR_Val_Reg:
		return RegArg(ctx, ir.reg);

	default:
		assert(false); // TODO: implement
		return (struct Asm_Arg) { 0 };
	}
}


static struct Papyrus_Symbol*
GetSym(struct IR_Val ir)
{
	assert(ir.type == IR_Val_Sym);
	return ir.sym;
}


static struct Asm_Inst*
CreateInstruction(Ctx* ctx, uint32_t opcode, uint32_t argsCount)
{
	struct Asm_Inst* inst = (struct Asm_Inst*)Arena_Allocate(&ctx->arenas.asm,
		sizeof(struct Asm_Inst) + sizeof(struct Asm_Arg) * argsCount);

	inst->opcode = opcode;
	inst->jumpTarget = false;
	inst->argsCount = argsCount;

	return inst;
}

static void
GenerateInstruction(Ctx* ctx, struct IR_Inst* irInst,
	struct Array* array, struct IR_Block* nextBlock)
{
	struct IR_Val* args = IR_ValArray_Data(&irInst->args);
	intptr_t argCount = IR_ValArray_Size(&irInst->args);

	struct Asm_Inst* inst = NULL;
	switch (irInst->inst)
	{
	case IR_Inst_Arg:
		break;

	case IR_Inst_Mov:
		assert(argCount == 1);
		inst = CreateInstruction(ctx, Asm_Assign, 2);
		inst->args[0] = DstArg(ctx, irInst);
		inst->args[1] = ValArg(ctx, args[0]);
		break;

	case IR_Inst_Load:
		assert(argCount == 1);
		inst = CreateInstruction(ctx, Asm_Assign, 2);
		inst->args[0] = DstArg(ctx, irInst);
		inst->args[1] = SymArg(ctx, GetSym(args[0]));
		break;
		

	case IR_Inst_Store:
		assert(argCount == 2);
		inst = CreateInstruction(ctx, Asm_Assign, 2);
		inst->args[0] = SymArg(ctx, GetSym(args[0]));
		inst->args[1] = ValArg(ctx, args[1]);
		break;

	case IR_Inst_Get:
		assert(argCount == 2);
		inst = CreateInstruction(ctx, Asm_PropGet, 3);
		inst->args[0] = SymArg(ctx, GetSym(args[1]));
		inst->args[1] = ValArg(ctx, args[0]);
		inst->args[2] = DstArg(ctx, irInst);
		break;

	case IR_Inst_Set:
		assert(argCount == 3);
		inst = CreateInstruction(ctx, Asm_PropSet, 3);
		inst->args[0] = SymArg(ctx, GetSym(args[1]));
		inst->args[1] = ValArg(ctx, args[0]);
		inst->args[2] = ValArg(ctx, args[2]);
		break;

		{
			uint32_t opcode;

#ifndef __INTELLISENSE__
#define UNARY(x) \
	case IR_Inst_ ## x: \
		opcode = Asm_ ## x; \
		goto unary

		UNARY(INeg);
		UNARY(FNeg);
		UNARY(Not);
#undef UNARY
#endif
		unary:
			assert(argCount == 1);
			inst = CreateInstruction(ctx, opcode, 2);
			inst->args[0] = DstArg(ctx, irInst);
			inst->args[1] = ValArg(ctx, args[0]);
		}
		break;

		{
			uint32_t opcode;

#ifndef __INTELLISENSE__
#define BINARY(x) \
	case IR_Inst_ ## x: \
		opcode = Asm_ ## x; \
		goto binary
			
			BINARY(IAdd);
			BINARY(FAdd);
			BINARY(ISub);
			BINARY(FSub);
			BINARY(IMul);
			BINARY(FMul);
			BINARY(IDiv);
			BINARY(FDiv);
			BINARY(IMod);

			BINARY(CmpEq);
			BINARY(CmpLt);
			BINARY(CmpGt);
			BINARY(CmpLe);
			BINARY(CmpGe);
#undef BINARY
#endif

		binary:
			assert(argCount == 2);
			inst = CreateInstruction(ctx, opcode, 3);
			inst->args[0] = DstArg(ctx, irInst);
			inst->args[1] = ValArg(ctx, args[0]);
			inst->args[2] = ValArg(ctx, args[1]);
		}
		break;

	case IR_Inst_CmpNe:
		assert(argCount == 2);
		{
			struct Asm_Arg reg = DstArg(ctx, irInst);

			inst = CreateInstruction(ctx, Asm_CmpEq, 3);
			inst->args[0] = reg;
			inst->args[1] = ValArg(ctx, args[0]);
			inst->args[2] = ValArg(ctx, args[1]);

			InstArray_Append(array, &inst,
				Arena_CreateAllocator(&ctx->arenas.local));

			inst = CreateInstruction(ctx, Asm_Not, 2);
			inst->args[0] = reg;
			inst->args[1] = reg;
		}
		break;

	case IR_Inst_Array_Load:
		assert(argCount == 2);
		inst = CreateInstruction(ctx, Asm_ArrGet, 3);
		inst->args[0] = DstArg(ctx, irInst);
		inst->args[1] = ValArg(ctx, args[0]);
		inst->args[2] = ValArg(ctx, args[1]);
		break;

	case IR_Inst_Array_Store:
		assert(argCount == 3);
		inst = CreateInstruction(ctx, Asm_ArrSet, 3);
		inst->args[0] = ValArg(ctx, args[0]);
		inst->args[1] = ValArg(ctx, args[1]);
		inst->args[2] = ValArg(ctx, args[2]);
		break;

	case IR_Inst_Cast:
		assert(argCount == 2);
		inst = CreateInstruction(ctx, Asm_Cast, 2);
		inst->args[0] = DstArg(ctx, irInst);
		inst->args[1] = ValArg(ctx, args[0]);
		break;

		{
			intptr_t fargOffset;

	case IR_Inst_Call:
			assert(argCount >= 1);
			{
				struct Papyrus_Function* func =
					(struct Papyrus_Function*)args[0].sym;

				struct Papyrus_Script* script = func->symbol.script;

				inst = CreateInstruction(ctx, Asm_CallStatic, argCount + 2);
				inst->args[0] = SymArg(ctx, &script->symbol);
				inst->args[1] = SymArg(ctx, &func->symbol);
				inst->args[2] = DstArg(ctx, irInst);

				fargOffset = 1;
				goto call_args;
			}

	case IR_Inst_CallVirt:
			assert(argCount >= 2);
			inst = CreateInstruction(ctx, Asm_CallMethod, argCount + 1);
			inst->args[0] = SymArg(ctx, GetSym(args[1]));
			inst->args[1] = ValArg(ctx, args[0]);
			inst->args[2] = DstArg(ctx, irInst);

			fargOffset = 2;
			goto call_args;

		call_args:;
			intptr_t fargCount = argCount - fargOffset;
			inst->args[3] = (struct Asm_Arg) {
				.type = Asm_Arg_Index,
				.hidden = true,
				.index = fargCount,
			};

			intptr_t argIndex = 4;
			FOREACHV(arg, arg_i, args + fargOffset, fargCount)
				inst->args[argIndex++] = ValArg(ctx, arg);
		}
		break;

	case IR_Inst_Ret:
		assert(argCount == 1);
		inst = CreateInstruction(ctx, Asm_Return, 1);
		inst->args[0] = ValArg(ctx, args[0]);
		break;

	case IR_Inst_Jmp:
		assert(argCount == 1);
		{
			assert(args[0].type == IR_Val_Jmp);
			struct IR_Block* target = args[0].jmp;

			if (target != nextBlock)
			{
				inst = CreateInstruction(ctx, Asm_Jmp, 1);
				inst->args[0] = JmpArg(ctx, target);
			}
		}
		break;

	case IR_Inst_Br:
		assert(argCount == 3);
		{
			assert(args[1].type == IR_Val_Jmp);
			struct IR_Block* targetT = args[1].jmp;

			assert(args[2].type == IR_Val_Jmp);
			struct IR_Block* targetF = args[2].jmp;

			assert(args[0].type == IR_Val_Reg);
			struct IR_Reg* reg = args[0].reg;

			if (targetT != nextBlock)
			{
				inst = CreateInstruction(ctx, Asm_JmpT, 2);
				inst->args[0] = RegArg(ctx, reg);
				inst->args[1] = JmpArg(ctx, targetT);
			}

			if (targetF != nextBlock)
			{
				inst = CreateInstruction(ctx, Asm_JmpF, 2);
				inst->args[0] = RegArg(ctx, reg);
				inst->args[1] = JmpArg(ctx, targetF);
			}
		}
		break;

	case IR_Inst_Phi:
		//TODO: remove phis and get rid of this maybe
		break;

	default:
		assert(false);
	}

	if (inst != NULL)
	{
		InstArray_Append(array, &inst,
			Arena_CreateAllocator(&ctx->arenas.local));
	}
}

static struct Asm_Variable*
GenerateVariable(Ctx* ctx, struct Papyrus_Variable* variable)
{
	struct Asm_Variable* asm = AllocateAsm(ctx, struct Asm_Variable, 1);

	asm->name = AllocateString(ctx, variable->symbol.name);
	asm->type = GetTypeString(ctx, variable->type);
	asm->uflags = 0;

	struct Papyrus_Expr* expr = variable->expr;
	if (expr != NULL)
	{
		struct Asm_Arg init;
		switch (expr->kind)
		{
		case Papyrus_Expr_LitNone:
			init.type = Asm_Arg_None;
			break;

		case Papyrus_Expr_LitInt:
			init.type = Asm_Arg_Int;
			init.int_ = expr->lit.int_;
			break;

		case Papyrus_Expr_LitBool:
			init.type = Asm_Arg_Bool;
			init.bool_ = expr->lit.bool_;
			break;

		case Papyrus_Expr_LitFloat:
			init.type = Asm_Arg_Float;
			init.float_ = expr->lit.float_;
			break;

		case Papyrus_Expr_LitString:
			init.type = Asm_Arg_String;
			init.index = AllocateString(ctx, expr->lit.string);
			break;
		}
		asm->init = init;
	}
	else assert(false);

	return asm;
}

static struct Asm_Function*
GenerateFunction(Ctx* ctx, struct Papyrus_Function* function)
{
	struct Arena_Pos pos = Arena_GetPos(&ctx->arenas.local);

	struct Asm_Function* asm = AllocateAsm(ctx, struct Asm_Function, 1);
	asm->name = AllocateString(ctx, function->symbol.name);
	asm->docstring = 0;
	asm->global = false;
	asm->native = false;
	asm->uflags = 0;

	struct IR* ir = Papyrus_GenerateIR(function,
		&ctx->arenas.local, ctx->arenas.pool);

	Papyrus_EliminateSSA(ir, &ctx->arenas.local, ctx->arenas.pool);

	asm->returnType = GetTypeString(ctx, function->signature.returnType);

	struct IR_Block** blocks = IR_BlockArray_Data(&ir->blocks);
	intptr_t blockCount = IR_BlockArray_Size(&ir->blocks);

	{
		intptr_t physRegCount = IR_PhysRegArray_Size(&ir->physRegs);
		ReserveRegisterStrings(ctx, physRegCount);

		intptr_t paramCount = function->signature.paramTypes.size;
		intptr_t localCount = physRegCount - paramCount;

		struct Asm_Local* params = ALLOCATE(
			&ctx->arenas.asm, struct Asm_Local, paramCount);

		struct Asm_Local* locals = ALLOCATE(
			&ctx->arenas.asm, struct Asm_Local, localCount);

		asm->params.data = params;
		asm->params.size = paramCount;

		asm->locals.data = locals;
		asm->locals.size = localCount;

		assert(blockCount > 0);
		Array_FOREACH(reg, reg_i, struct IR_PhysReg, &ir->physRegs)
		{
			struct Asm_Local* var;
			if (reg->parameter)
			{
				assert(paramCount-- > 0);
				var = params++;
			}
			else
			{
				assert(localCount-- > 0);
				var = locals++;
			}

			var->name = GetRegisterString(ctx, (uint32_t)reg_i);
			var->type = GetTypeString(ctx, reg->type);
		}
	}

	struct Array array;
	Array_Init(&array);

	// pointers to last instruction of each block
	struct Asm_Inst** jumps = ALLOCATE(
		&ctx->arenas.local, struct Asm_Inst*, blockCount);

	// indices of the first instruction of each block
	uint32_t* blockOffsets = ALLOCATE(
		&ctx->arenas.local, uint32_t, blockCount);

	FOREACHV(block, block_i, blocks, blockCount)
	{
		assert(block->index < blockCount);

		blockOffsets[block->index] = (uint32_t)InstArray_Size(&array);

		struct IR_Block* nextBlock = NULL; {
			intptr_t nextIndex = block_i + 1;
			if (nextIndex < blockCount)
				nextBlock = blocks[nextIndex];
		}

		Array_FOREACHV(irInst, irInst_i, struct IR_Inst*, &block->inst)
		{
			GenerateInstruction(ctx, irInst, &array, nextBlock);
		}

		jumps[block->index] = *InstArray_Last(&array);
	}

	// fix jump instruction target indices
	FOREACHV(jump, jump_i, jumps, blockCount)
	{
		intptr_t argIndex;
		switch (jump->opcode)
		{
		case Asm_Jmp:
			argIndex = 0;
			break;

		case Asm_JmpT:
		case Asm_JmpF:
			argIndex = 1;
			break;

		default:
			continue;
		}

		intptr_t instIndex = blockOffsets[jump->args[argIndex].index];
		InstArray_Data(&array)[instIndex]->jumpTarget = true;
		jump->args[argIndex].index = (uint32_t)instIndex;
	}

	CommitArray(ctx, &array, &asm->code);

	Arena_SetPos(&ctx->arenas.local, pos);
	return asm;
}

static struct Asm_Object*
GenerateScript(Ctx* ctx, struct Papyrus_Script* script)
{
	struct Arena_Pos pos = Arena_GetPos(&ctx->arenas.local);

	struct Asm_Object* object = AllocateAsm(ctx, struct Asm_Object, 1);
	object->name = AllocateString(ctx, script->symbol.name);
	object->base = 0;
	object->autoState = 0;
	object->docstring = 0;
	object->properties.size = 0;
	object->uflags = 0;

	struct Asm_State* state = AllocateAsm(ctx, struct Asm_State, 1);
	state->name = 0;

	/* create state array, just temporary until states are implemented */ {
		struct Asm_State** states =
			AllocateAsm_(ctx, sizeof(struct Asm_State*));

		*states = state;

		object->states.data = states;
		object->states.size = 1;
	}

	struct Array array;
	Array_Init(&array);

	FOREACHV_S(var, var_i, &script->variables)
	{
		struct Asm_Variable* asm = GenerateVariable(ctx, var);

		Array_Append(&array, &asm,
			sizeof(asm), Arena_CreateAllocator(&ctx->arenas.local));
	}
	CommitArray(ctx, &array, &object->variables);

	Array_Clear(&array);
	FOREACHV_S(func, func_i, &script->functions)
	{
		struct Asm_Function* asm = GenerateFunction(ctx, func);

		Array_Append(&array, &asm,
			sizeof(asm), Arena_CreateAllocator(&ctx->arenas.local));
	}
	CommitArray(ctx, &array, &state->functions);

	Arena_SetPos(&ctx->arenas.local, pos);
	return object;
}

struct Asm*
Papyrus_GenerateAsm(struct Papyrus_Script* const* scripts_,
	intptr_t scriptCount, struct Arena* asmArena,
	struct Papyrus_ArenaPool* pool)
{
	struct Papyrus_Script** scripts = (struct Papyrus_Script**)scripts_;

	Ctx ctx;
	ctx.arenas.pool = pool;
	ctx.arenas.asm = *asmArena;
	Arena_Init(&ctx.arenas.local, pool);
	Arena_Init(&ctx.arenas.global, pool);

	Array_Init(&ctx.strings);
	HashTable_Init(&ctx.stringTable);
	ctx.selfString = -1;
	Array_Init(&ctx.func.varStrings);


	// string at index 0 is empty
	AllocateString(&ctx, Papyrus_String_CREATE(""));

	struct Asm* asm = AllocateAsm(&ctx, struct Asm, 1);

	struct Array array;
	Array_Init(&array);

	asm->info.source = 0;
	asm->info.username = 0;
	asm->info.computer = 0;
	asm->info.time = 0;

	FOREACHV(script, script_i, scripts, scriptCount)
	{
		struct Asm_Object* object = GenerateScript(&ctx, script);

		Array_Append(&array, &object,
			sizeof(object), Arena_CreateAllocator(&ctx.arenas.local));
	}
	CommitArray(&ctx, &array, &asm->objects);

	CommitArray(&ctx, &ctx.strings, &asm->strings);

	Arena_Destroy(&ctx.arenas.global);
	Arena_Destroy(&ctx.arenas.local);
	*asmArena = ctx.arenas.asm;
	return asm;
}
