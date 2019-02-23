#pragma once

#include "Papyrus/String.h"

#include <stdbool.h>
#include <stdint.h>

enum Papyrus_Syntax_Kind
{
	Papyrus_Syntax_Symbol,
	Papyrus_Syntax_Type,
	Papyrus_Syntax_Scope,
	Papyrus_Syntax_IfClause,
	Papyrus_Syntax_Param,
	Papyrus_Syntax_ParamList,

	Papyrus_Syntax_NameExpr,
	Papyrus_Syntax_ConstExpr,
	Papyrus_Syntax_NewExpr,

	Papyrus_Syntax_UnaryExpr,
	Papyrus_Syntax_BinaryExpr,
	Papyrus_Syntax_AccessExpr,
	Papyrus_Syntax_CastExpr,
	Papyrus_Syntax_CallExpr,

	Papyrus_Syntax_ExprStmt,
	Papyrus_Syntax_AssignStmt,
	Papyrus_Syntax_ReturnStmt,
	Papyrus_Syntax_IfStmt,
	Papyrus_Syntax_WhileStmt,

	Papyrus_Syntax_Event,
	Papyrus_Syntax_Function,
	Papyrus_Syntax_Import,
	Papyrus_Syntax_Property,
	Papyrus_Syntax_ScriptHeader,
	Papyrus_Syntax_State,
	Papyrus_Syntax_Variable,

	Papyrus_Syntax_Script,
};

enum Papyrus_Syntax_EKind
{
	Papyrus_Syntax_EKind_None,

	Papyrus_Syntax_Type_None,
	Papyrus_Syntax_Type_Int,
	Papyrus_Syntax_Type_Bool,
	Papyrus_Syntax_Type_Float,
	Papyrus_Syntax_Type_String,

	Papyrus_Syntax_ConstExpr_None,
	Papyrus_Syntax_ConstExpr_Int,
	Papyrus_Syntax_ConstExpr_True,
	Papyrus_Syntax_ConstExpr_False,
	Papyrus_Syntax_ConstExpr_Float,
	Papyrus_Syntax_ConstExpr_String,

	Papyrus_Syntax_UnaryExpr_Neg,
	Papyrus_Syntax_UnaryExpr_Not,

	Papyrus_Syntax_BinaryExpr_Add,
	Papyrus_Syntax_BinaryExpr_Sub,
	Papyrus_Syntax_BinaryExpr_Mul,
	Papyrus_Syntax_BinaryExpr_Div,
	Papyrus_Syntax_BinaryExpr_Mod,

	Papyrus_Syntax_BinaryExpr_Eq,
	Papyrus_Syntax_BinaryExpr_Ne,
	Papyrus_Syntax_BinaryExpr_Lt,
	Papyrus_Syntax_BinaryExpr_Gt,
	Papyrus_Syntax_BinaryExpr_Le,
	Papyrus_Syntax_BinaryExpr_Ge,

	Papyrus_Syntax_BinaryExpr_Con,
	Papyrus_Syntax_BinaryExpr_Dis,

	Papyrus_Syntax_BinaryExpr_Index,

	Papyrus_Syntax_InvokeExpr_Call,

	Papyrus_Syntax_CastExpr_As,
	Papyrus_Syntax_CastExpr_Is,
};

enum
{
	Papyrus_SyntaxFlags_Error = 0x1,
};

struct Papyrus_Syntax
{
	uint8_t kind;
	uint8_t ekind;
	uint16_t flags;
	uint32_t eflags;
	uint32_t offset;
};


struct Papyrus_Syntax_Symbol
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_String* data;
	intptr_t size;
};

enum
{
	Papyrus_Syntax_TypeFlags_Fundamental = 0x1,
	Papyrus_Syntax_TypeFlags_Array = 0x2,
};

struct Papyrus_Syntax_Type
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Symbol* symbol;
};

struct Papyrus_Syntax_Scope
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* const* data;
	intptr_t size;
};

struct Papyrus_Syntax_IfClause
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* cond;
	const struct Papyrus_Syntax_Scope* scope;
};

struct Papyrus_Syntax_Param
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	const struct Papyrus_Syntax_Type* type;
	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Syntax_ParamList
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Param* const* data;
	intptr_t size;
};


struct Papyrus_Syntax_NameExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Symbol* symbol;
};

struct Papyrus_Syntax_ConstExpr
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String string;
};

struct Papyrus_Syntax_NewExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Symbol* name;
	const struct Papyrus_Syntax* extent;
};


struct Papyrus_Syntax_UnaryExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Syntax_BinaryExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* left;
	const struct Papyrus_Syntax* right;
};

struct Papyrus_Syntax_AccessExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
	struct Papyrus_String name;
};

struct Papyrus_Syntax_CastExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Type* type;
	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Syntax_CallExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
	struct {
		const struct Papyrus_Syntax* const* data;
		intptr_t size;
	} args;
};


struct Papyrus_Syntax_ExprStmt
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Syntax_AssignStmt
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* object;
	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Syntax_ReturnStmt
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Syntax_IfStmt
{
	struct Papyrus_Syntax syntax;

	struct {
		const struct Papyrus_Syntax_IfClause* const* data;
		intptr_t size;
	} clauses;
	const struct Papyrus_Syntax_Scope* elseScope;
};

struct Papyrus_Syntax_WhileStmt
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* cond;
	const struct Papyrus_Syntax_Scope* scope;
};


enum
{
	// property
	Papyrus_Syntax_DeclFlags_Auto = 0x1,
	Papyrus_Syntax_DeclFlags_AutoReadOnly = 0x2,

	// function
	Papyrus_Syntax_DeclFlags_Global = 0x4,

	// script, function
	Papyrus_Syntax_DeclFlags_Native = 0x8,
};

struct Papyrus_Syntax_Event
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	const struct Papyrus_Syntax_ParamList* params;
	const struct Papyrus_Syntax_Scope* scope;
	uint32_t uflags;
};

struct Papyrus_Syntax_Function
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	const struct Papyrus_Syntax_Type* type;
	const struct Papyrus_Syntax_ParamList* params;
	const struct Papyrus_Syntax_Scope* scope;
	uint32_t uflags;
};

struct Papyrus_Syntax_Property
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	const struct Papyrus_Syntax_Type* type;
	union {
		const struct Papyrus_Syntax* expr;
		const struct Papyrus_Syntax_Scope* scope;
	};
	uint32_t uflags;
};

struct Papyrus_Syntax_State
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	const struct Papyrus_Syntax_Scope* scope;
	uint32_t uflags;
};

struct Papyrus_Syntax_Variable
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	const struct Papyrus_Syntax_Type* type;
	const struct Papyrus_Syntax* expr;
	uint32_t uflags;
};


struct Papyrus_Syntax_Import
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Symbol* symbol;
};

struct Papyrus_Syntax_ScriptHeader
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	const struct Papyrus_Syntax_Symbol* base;
	uint32_t uflags;
};


struct Papyrus_Syntax_Script
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Scope* scope;
};
