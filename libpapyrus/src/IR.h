#pragma once

#include "Papyrus/Arena.h"
#include "Papyrus/String.h"

#include "Common/Array.h"

#include <stdbool.h>
#include <stdint.h>

struct Arena;
struct Papyrus_ArenaPool;
struct Papyrus_Type;
struct Papyrus_Function;

enum
{
	IR_Inst_Arg,

	IR_Inst_Mov,

	IR_Inst_Load,
	IR_Inst_Store,

	IR_Inst_Get,
	IR_Inst_Set,

	IR_Inst_INeg,
	IR_Inst_FNeg,
	IR_Inst_Not,

	IR_Inst_IAdd,
	IR_Inst_FAdd,
	IR_Inst_ISub,
	IR_Inst_FSub,
	IR_Inst_IMul,
	IR_Inst_FMul,
	IR_Inst_IDiv,
	IR_Inst_FDiv,
	IR_Inst_IMod,

	IR_Inst_CmpEq,
	IR_Inst_CmpNe,
	IR_Inst_CmpLt,
	IR_Inst_CmpGt,
	IR_Inst_CmpLe,
	IR_Inst_CmpGe,

	IR_Inst_Array_Load,
	IR_Inst_Array_Store,

	IR_Inst_Cast,

	IR_Inst_Call,
	IR_Inst_CallVirt,

	IR_Inst_Jmp,
	IR_Inst_Br,
	IR_Inst_Ret,

	IR_Inst_Phi,
};

enum
{
	IR_Val_None,

	IR_Val_Reg,
	IR_Val_Imm,
	IR_Val_Jmp,
	IR_Val_Sym,
};

enum
{
	IR_Val_Imm_None,
	IR_Val_Imm_Self,
	IR_Val_Imm_Int,
	IR_Val_Imm_Bool,
	IR_Val_Imm_Float,
	IR_Val_Imm_String,
};

struct IR_Val
{
	union {
		struct {
			uint8_t type;
			uint8_t etype;
			uint16_t flags;
			union {
				uint32_t eflags;
				uint32_t string_size;
			};
			union {
				struct IR_Reg* reg;
				struct IR_Block* jmp;
				struct Papyrus_Symbol* sym;
				union {
					int32_t int_;
					bool bool_;
					float float_;
					const char* string_data;
				} imm;
			};
		};
		struct {
			struct IR_Block* block;
			struct IR_Reg* reg;
		} phi;
	};
};

static inline struct Papyrus_String
IR_Val_GetString(const struct IR_Val* value)
{
	return (struct Papyrus_String) {
		.data = value->imm.string_data,
		.size = value->string_size,
	};
}


enum
{
	IR_RegFlags_PhiSource = 0x1,
	IR_RegFlags_PhiTarget = 0x2,
};

struct IR_Reg
{
	struct IR_Block* block;
	struct IR_Inst* def;

	uint32_t index;
	uint32_t flags;

	struct Papyrus_Type* type;

	uint32_t unresolvedNeighbourCount;
	uint32_t physicalIndex;

	struct CongruenceClass* congruenceClass;

	struct BitSet* interferenceSet;
	struct Array interference;

	struct IR_Reg* coalesce;
};

struct IR_Inst
{
	uint16_t inst;
	uint16_t flags;

	struct IR_Block* block;

	struct IR_Reg* reg;

	// IR_Val
	struct Array args;
};

struct IR_Block
{
	uintptr_t dummy;

	uint32_t index;
	uint32_t flags;

	intptr_t phiCount;

	// struct IR_Reg*
	struct Array inst;

	// struct IR_Block*
	struct Array pred;

	struct IR_Block* succ[2];

	struct BitSet* liveInSet;
	struct Array liveIn;

	struct BitSet* liveOutSet;
	struct Array liveOut;
};

static inline struct IR_Val
RegVal(struct IR_Reg* reg)
{
	return (struct IR_Val) {
		.type = IR_Val_Reg,
		.flags = 0,
		.reg = reg,
	};
}

static inline struct IR_Val
JmpVal(struct IR_Block* block)
{
	return (struct IR_Val) {
		.type = IR_Val_Jmp,
		.flags = 0,
		.jmp = block,
	};
}

static inline struct IR_Val
SymVal(struct Papyrus_Symbol* symbol)
{
	return (struct IR_Val) {
		.type = IR_Val_Sym,
		.flags = 0,
		.sym = symbol,
	};
}

struct IR_PhysReg
{
	bool parameter;
	struct Papyrus_Type* type;
};

struct IR
{
	// struct IR_Block*
	struct Array blocks;

	// struct IR_Reg*
	struct Array regs;

	// struct IR_Inst*
	struct Array inst;

	// struct IR_Inst*
	struct Array phis;

	// struct IR_PhysReg
	struct Array physRegs;
};

struct IR_Block*
Papyrus_IR_CreateBlock(struct IR* ir, struct Arena* arena);

struct IR_Inst*
Papyrus_IR_CreateInst(struct IR* ir, struct Arena* arena,
	uint32_t inst, struct Papyrus_Type* const* type);

struct IR*
Papyrus_GenerateIR(struct Papyrus_Function* function,
	struct Arena* arena, struct Papyrus_ArenaPool* pool);
