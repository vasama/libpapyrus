/* This file declares the script, a part of the semantic program analysis API.
The script is a semantic representation of a single Papyrus script. A script is
built from a syntax tree and can be linked into a program for external symbol
resolution. Once linked, semantic analysis of the script, such as type checking
may be performed, producing errors and warnings. A script that has been
analyzed and found to be valid may be passed to the codegen APIs. */

#pragma once

#include "Papyrus/String.h"

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

struct Papyrus_Allocator;
struct Papyrus_ArenaPool;
struct Papyrus_Diagnostics;
struct Papyrus_SyntaxTree;


enum
{
	Papyrus_Symbol_Intrinsic,
	Papyrus_Symbol_Extern,
	Papyrus_Symbol_Missing,
	Papyrus_Symbol_Script,
	Papyrus_Symbol_Function,
	Papyrus_Symbol_Variable,
	Papyrus_Symbol_Property,
};

enum
{
	Papyrus_SymbolFlags_Export = 0x1,
	Papyrus_SymbolFlags_Hidden = 0x2,
};

struct Papyrus_Symbol
{
	uint16_t kind;
	uint16_t flags;
	uint32_t eflags;
	struct Papyrus_String name;
};


enum
{
	Papyrus_Type_Error,
	Papyrus_Type_None,
	Papyrus_Type_Int,
	Papyrus_Type_Bool,
	Papyrus_Type_Float,
	Papyrus_Type_String,
	Papyrus_Type_Array,
	Papyrus_Type_Script,
//	Papyrus_Type_Struct,
	Papyrus_Type_Extern,

#ifdef Papyrus_Type_N
#	undef Papyrus_Type_N
	Papyrus_Type_N,
#endif
};

enum
{
	Papyrus_TypeKind_Error,
	Papyrus_TypeKind_None,
	Papyrus_TypeKind_Primitive,
	Papyrus_TypeKind_Array,
	Papyrus_TypeKind_Script,
//	Papyrus_TypeKind_Struct,
	Papyrus_TypeKind_Extern,
};

enum
{
	Papyrus_TypeFlags_Error = 0x1,
	Papyrus_TypeFlags_Intrinsic = 0x2,
	Papyrus_TypeFlags_Primitive = 0x4,
	Papyrus_TypeFlags_Array = 0x8,
	Papyrus_TypeFlags_Arithmetic = 0x10,
	Papyrus_TypeFlags_Composite = 0x20,
	Papyrus_typeFlags_Extern = 0x40,
};

struct Papyrus_Type
{
	uint8_t type;
	uint8_t kind;
	struct {
		uint8_t type;
		uint8_t kind;
	} array;
	uint32_t flags;
	struct Papyrus_Symbol* symbol;
};

struct Papyrus_TypX
{
	uint8_t type;
	uint8_t kind;
	uint16_t flags;
	uint32_t eflags;
	union {
		struct Papyrus_TypX* element;
		struct Papyrus_Symbol* symbol;
	};
	struct Papyrus_TypX* array;
};


enum
{
	Papyrus_Expr_Symbol,
	Papyrus_Expr_Access,
	Papyrus_Expr_Assign,

	Papyrus_Expr_ReadLocal,
	Papyrus_Expr_WriteLocal,

	Papyrus_Expr_LitNone,
	Papyrus_Expr_LitInt,
	Papyrus_Expr_LitBool,
	Papyrus_Expr_LitFloat,
	Papyrus_Expr_LitString,

	Papyrus_Expr_Neg,
	Papyrus_Expr_Not,

	Papyrus_Expr_Add,
	Papyrus_Expr_Sub,
	Papyrus_Expr_Mul,
	Papyrus_Expr_Div,
	Papyrus_Expr_Mod,

	Papyrus_Expr_Eq,
	Papyrus_Expr_Ne,
	Papyrus_Expr_Lt,
	Papyrus_Expr_Gt,
	Papyrus_Expr_Le,
	Papyrus_Expr_Ge,

	Papyrus_Expr_Con,
	Papyrus_Expr_Dis,

	Papyrus_Expr_ReadArray,
	Papyrus_Expr_WriteArray,

	Papyrus_Expr_Cast,
	Papyrus_Expr_Call,
};

enum
{
	Papyrus_Expr_ReadField,
	Papyrus_Expr_ReadProperty,

	Papyrus_Expr_WriteField,
	Papyrus_Expr_WriteProperty,
};

enum
{
	Papyrus_ExprFlags_Error = 0x1,
	Papyrus_ExprFlags_Const = 0x2,
	Papyrus_ExprFlags_Implicit = 0x4,
};

struct Papyrus_Expr
{
	uint8_t kind;
	uint8_t ekind;
	uint32_t flags;
	struct Papyrus_TypX* type;
	union {
		struct Papyrus_Symbol* symbol;

		union {
			int32_t int_;
			bool bool_;
			float float_;
			struct Papyrus_String string;
		} lit;

		struct {
			struct Papyrus_Expr* expr;
			struct Papyrus_PascalString* name;
			struct Papyrus_Symbol* symbol;
		} access;

		struct {
			union {
				struct {
					struct Papyrus_Expr* object;
					struct Papyrus_PascalString* name;
					struct Papyrus_Symbol* symbol;
				};
				struct {
					struct Papyrus_Expr* array;
					struct Papyrus_Expr* subscript;
				};
				uint32_t localIndex;
			};
			struct Papyrus_Expr* expr;
		} assign;

		union {
			struct Papyrus_Expr* expr;
			struct {
				struct Papyrus_Expr* lhs;
				struct Papyrus_Expr* rhs;
			};
		} oper;

		struct {
			struct Papyrus_Expr* expr;
			struct Papyrus_TypX* type;
		} cast;

		struct {
			struct Papyrus_Expr* object;
			struct Papyrus_PascalString* name;
			struct Papyrus_Symbol* symbol;
			struct {
				struct Papyrus_Expr** data;
				intptr_t size;
			} args;
		} call;

		uint32_t localIndex;
	};
	uint32_t source;
};


struct Papyrus_Scope
{
	struct Papyrus_Stmt** data;
	intptr_t size;
};

struct Papyrus_Branch
{
	struct Papyrus_Expr* expr;
	struct Papyrus_Scope scope;
};

enum
{
	Papyrus_Stmt_Expr,
	Papyrus_Stmt_Branch,
	Papyrus_Stmt_Loop,
	Papyrus_Stmt_Return,
};

struct Papyrus_Stmt
{
	uint32_t kind;
	uint32_t flags;
	union {
		struct Papyrus_Expr* expr;
		struct {
			struct Papyrus_Branch* data;
			intptr_t size;
		} branch;
		struct Papyrus_Branch loop;
	};
	uint32_t source;
};

struct Papyrus_Function
{
	struct Papyrus_Symbol symbol;

	bool global;

	struct {
		struct Papyrus_TypX* returnType;
		struct {
			struct Papyrus_TypX** data;
			intptr_t size;
		} paramTypes;
	} signature;

	struct {
		struct Papyrus_TypX** data;
		intptr_t size;
	} locals;

	struct Papyrus_Scope scope;
};

struct Papyrus_Variable
{
	struct Papyrus_Symbol symbol;

	struct Papyrus_TypX* type;
	struct Papyrus_Expr* expr;
};

struct Papyrus_Property
{
	struct Papyrus_Symbol symbol;

	struct Papyrus_TypX* type;

	struct Papyrus_Variable* variable;
	struct Papyrus_Function* get;
	struct Papyrus_Function* set;
};

enum
{
	Papyrus_ScriptFlags_Analyzed = 0x1,
};

struct Papyrus_Script
{
	struct Papyrus_Symbol symbol;

	struct Papyrus_TypX* type;

	struct {
		struct Papyrus_Script* script;

		// depth of the base hierarchy
		intptr_t depth;
	} base;

	struct {
		struct Papyrus_Extern** data;
		intptr_t size;
	} imports;

	struct {
		struct Papyrus_Symbol** data;
		intptr_t size;
	} exports;

	struct {
		struct Papyrus_Function** data;
		intptr_t size;
	} functions;

	struct {
		struct Papyrus_Variable** data;
		intptr_t size;
	} variables;

	struct {
		struct Papyrus_Property** data;
		intptr_t size;
	} properties;
};

struct Papyrus_Script*
Papyrus_Script_Create(
	struct Papyrus_SyntaxTree* tree, struct Papyrus_Allocator allocator,
	struct Papyrus_ArenaPool* pool, struct Papyrus_Diagnostics* diag);

void
Papyrus_Script_Delete(struct Papyrus_Script* script);

void
Papyrus_Script_Analyze(
	struct Papyrus_Script* script, struct Papyrus_Diagnostics* diag);
