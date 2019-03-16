#pragma once

#include "Papyrus/String.h"

#include <stdint.h>
#include <stdbool.h>

struct Papyrus_ArenaPool;
struct Arena;
struct Papyrus_Script;

enum
{
	Asm_Nop,
	Asm_IAdd,
	Asm_FAdd,
	Asm_ISub,
	Asm_FSub,
	Asm_IMul,
	Asm_FMul,
	Asm_IDiv,
	Asm_FDiv,
	Asm_IMod,
	Asm_Not,
	Asm_INeg,
	Asm_FNeg,
	Asm_Assign,
	Asm_Cast,
	Asm_CmpEq,
	Asm_CmpLt,
	Asm_CmpLe,
	Asm_CmpGt,
	Asm_CmpGe,
	Asm_Jmp,
	Asm_JmpT,
	Asm_JmpF,
	Asm_CallMethod,
	Asm_CallParent,
	Asm_CallStatic,
	Asm_Return,
	Asm_StrCat,
	Asm_PropGet,
	Asm_PropSet,
	Asm_ArrNew,
	Asm_ArrLen,
	Asm_ArrGet,
	Asm_ArrSet,
	Asm_ArrFind,
	Asm_ArrRFind,
};

enum
{
	Asm_Arg_None,
	Asm_Arg_Index,
	Asm_Arg_String,
	Asm_Arg_Int,
	Asm_Arg_Float,
	Asm_Arg_Bool,

	Asm_Arg_Reg = Asm_Arg_Index,
	Asm_Arg_Symbol = Asm_Arg_Index,
};

struct Asm_Arg
{
	uint8_t type;
	bool hidden;
	bool label;
	union {
		uint32_t index;
		int32_t int_;
		bool bool_;
		float float_;
	};
};

struct Asm_Inst
{
	uint8_t opcode;
	bool jumpTarget;
	uint16_t argsCount;
	struct Asm_Arg args[];
};

struct Asm_Variable
{
	uint16_t name;
	uint16_t type;
	uint32_t uflags;
	struct Asm_Arg init;
};

struct Asm_Property
{
	uint16_t name;
	uint16_t type;
	uint16_t docstring;
	bool auto_;
	uint32_t uflags;

	union {
		uint16_t varname;
		struct {
			struct Asm_Function* get;
			struct Asm_Function* set;
		};
	};
};

struct Asm_Local
{
	uint16_t name;
	uint16_t type;
};

struct Asm_Function
{
	uint16_t name;
	uint16_t docstring;
	uint16_t returnType;
	bool global;
	bool native;
	uint32_t uflags;
	struct {
		struct Asm_Local* data;
		intptr_t size;
	} params;
	struct {
		struct Asm_Local* data;
		intptr_t size;
	} locals;
	struct {
		struct Asm_Inst** data;
		intptr_t size;
	} code;
};

struct Asm_State
{
	uint16_t name;

	struct {
		struct Asm_Function** data;
		intptr_t size;
	} functions;
};

struct Asm_Object
{
	uint16_t name;
	uint16_t base;
	uint16_t docstring;
	uint16_t autoState;
	uint32_t uflags;

	struct {
		struct Asm_Variable** data;
		intptr_t size;
	} variables;

	struct {
		struct Asm_Property** data;
		intptr_t size;
	} properties;

	struct {
		struct Asm_State** data;
		intptr_t size;
	} states;
};

struct Asm_UserFlag
{
	uint16_t name;
	uint16_t index;
};

struct Asm
{
	struct {
		uint16_t source;
		uint16_t username;
		uint16_t computer;
		uint64_t time;
	} info;

	struct {
		struct Papyrus_String* data;
		intptr_t size;
	} strings;

	struct {
		struct Asm_UserFlags* data;
		intptr_t size;
	} userFlags;

	struct {
		struct Asm_Object** data;
		intptr_t size;
	} objects;
};

struct Asm*
Papyrus_GenerateAsm(struct Papyrus_Script* const* scripts,
	intptr_t scriptCount, struct Arena* asmArena,
	struct Papyrus_ArenaPool* pool);
