#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

enum
{
	Papyrus_Error_None = 0,

	Papyrus_Error_InvalidArgument,
	Papyrus_Error_OutOfMemory,
};
typedef uint32_t Papyrus_Error;

struct Papyrus_String
{
	const char* data;
	intptr_t size;
};


struct Papyrus_SyntaxArray
{
	const struct Papyrus_Syntax* const* data;
	intptr_t size;
};

struct Papyrus_StringArray
{
	const struct Papyrus_String* data;
	intptr_t size;
};

struct Papyrus_FullName
{
	struct Papyrus_StringArray parts;
};


struct Papyrus_Param
{
	struct Papyrus_String name;
	const struct Papyrus_Syntax_Type* type;
	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Params
{
	const struct Papyrus_Param* data;
	intptr_t size;
};


enum
{
	Papyrus_Syntax_ArrayType,
	Papyrus_Syntax_CustomEvent,
	Papyrus_Syntax_Event,
	Papyrus_Syntax_Function,
	Papyrus_Syntax_Group,
	Papyrus_Syntax_Import,
	Papyrus_Syntax_Property,
	Papyrus_Syntax_Script,
	Papyrus_Syntax_State,
	Papyrus_Syntax_Struct,
	Papyrus_Syntax_Type,
	Papyrus_Syntax_Variable,

	Papyrus_Syntax_ExprStmt,
	Papyrus_Syntax_ReturnStmt,
	Papyrus_Syntax_IfStmt,
	Papyrus_Syntax_WhileStmt,

	Papyrus_Syntax_ElseIfClause,
	Papyrus_Syntax_ElseClause,

	Papyrus_Syntax_NoneExpr,
	Papyrus_Syntax_NameExpr,
	Papyrus_Syntax_IntExpr,
	Papyrus_Syntax_FloatExpr,
	Papyrus_Syntax_StringExpr,
	Papyrus_Syntax_BoolExpr_True,
	Papyrus_Syntax_BoolExpr_False,
	Papyrus_Syntax_NewExpr,

	Papyrus_Syntax_AsExpr,
	Papyrus_Syntax_IsExpr,

	Papyrus_Syntax_CallExpr,
	Papyrus_Syntax_IndexExpr,

	Papyrus_Syntax_AccessExpr,

	Papyrus_Syntax_NegExpr,

	Papyrus_Syntax_AddExpr,
	Papyrus_Syntax_SubExpr,
	Papyrus_Syntax_MulExpr,
	Papyrus_Syntax_DivExpr,
	Papyrus_Syntax_ModExpr,

	Papyrus_Syntax_EqExpr,
	Papyrus_Syntax_NeExpr,
	Papyrus_Syntax_LtExpr,
	Papyrus_Syntax_GtExpr,
	Papyrus_Syntax_LeExpr,
	Papyrus_Syntax_GeExpr,

	Papyrus_Syntax_NotExpr,
	Papyrus_Syntax_ConExpr,
	Papyrus_Syntax_DisExpr,

	Papyrus_Syntax_AssignExpr,
	Papyrus_Syntax_AddAssignExpr,
	Papyrus_Syntax_SubAssignExpr,
	Papyrus_Syntax_MulAssignExpr,
	Papyrus_Syntax_DivAssignExpr,
	Papyrus_Syntax_ModAssignExpr,
};

enum
{
	Papyrus_SyntaxFlags_Error = 0x1,
};

struct Papyrus_Syntax
{
	uint16_t kind;
	uint16_t flags;
};


enum
{
	Papyrus_TypeFlags_Array = 0x1,
};

struct Papyrus_Syntax_Type
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_FullName name;
	uint32_t flags;
};


enum
{
	Papyrus_DeclFlags_Auto = 0x1,
	Papyrus_DeclFlags_BetaOnly = 0x2,
	Papyrus_DeclFlags_Conditional = 0x4,
	Papyrus_DeclFlags_Const = 0x8,
	Papyrus_DeclFlags_DebugOnly = 0x10,
	Papyrus_DeclFlags_Default = 0x20,
	Papyrus_DeclFlags_Global = 0x40,
	Papyrus_DeclFlags_Hidden = 0x80,
	Papyrus_DeclFlags_Mandatory = 0x100,
	Papyrus_DeclFlags_Native = 0x200,
};

struct Papyrus_Syntax_State
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	struct Papyrus_SyntaxArray defs;
};

struct Papyrus_Syntax_Variable
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Type* type;
	struct Papyrus_String name;
	const struct Papyrus_Syntax* expr;
	uint32_t flags;
};

struct Papyrus_Syntax_Function
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Type* type;
	struct Papyrus_String name;
	struct Papyrus_Params params;
	struct Papyrus_SyntaxArray scope;
	uint32_t flags;
};

struct Papyrus_Syntax_Property
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Type* type;
	struct Papyrus_String name;
	uint32_t flags;

	union {
		const struct Papyrus_Syntax* expr;
		struct {
			const struct Papyrus_Syntax_Function* get;
			const struct Papyrus_Syntax_Function* set;
		};
	};
};

struct Papyrus_Syntax_Event
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_FullName typename;
	struct Papyrus_String name;
	struct Papyrus_Params params;
	struct Papyrus_SyntaxArray scope;
	uint32_t flags;
};

struct Papyrus_Syntax_CustomEvent
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
};

struct Papyrus_Syntax_Struct
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	struct Papyrus_SyntaxArray vars;
};

struct Papyrus_Syntax_Group
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	struct Papyrus_SyntaxArray props;
	uint32_t flags;
};


struct Papyrus_Syntax_ExprStmt
{
	struct Papyrus_Syntax syntax;

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

	const struct Papyrus_Syntax* cond;
	struct Papyrus_SyntaxArray scope;
	struct {
		const struct Papyrus_Syntax_ElseIfClause* const* data;
		intptr_t size;
	} elifs;
	const struct Papyrus_Syntax_ElseClause* else_;
};

struct Papyrus_Syntax_WhileStmt
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* cond;
	struct Papyrus_SyntaxArray scope;
};

struct Papyrus_Syntax_ElseIfClause
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* cond;
	struct Papyrus_SyntaxArray scope;
};

struct Papyrus_Syntax_ElseClause
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_SyntaxArray scope;
};


struct Papyrus_Syntax_AccessExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
	struct Papyrus_String name;
};

struct Papyrus_Syntax_NameExpr
{
	struct Papyrus_Syntax syntax;
	struct Papyrus_FullName name;
};

struct Papyrus_Syntax_NewExpr
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_FullName name;
	const struct Papyrus_Syntax* extent;
};

struct Papyrus_Syntax_StringExpr
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String string;
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

struct Papyrus_Syntax_CastExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax_Type* type;
	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Syntax_InvokeExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
	struct Papyrus_SyntaxArray args;
};


struct Papyrus_Syntax_Import
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_FullName name;
};

struct Papyrus_Syntax_Script
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	struct Papyrus_FullName base;
	struct Papyrus_SyntaxArray defs;
	uint32_t flags;
};


struct Papyrus_Allocator
{
	void*(*allocate)(void*, uintptr_t);
	void(*deallocate)(void*, void*, uintptr_t);
	void* context;
};

struct Papyrus_ParserOptions
{
	struct Papyrus_Allocator allocator;
	uintptr_t lexerBufferSize;
	uintptr_t syntaxBufferInitialSize;
};

struct Papyrus_Parser;

Papyrus_Error
Papyrus_Parser_Create(const struct Papyrus_ParserOptions* options, struct Papyrus_Parser** out);

void
Papyrus_Parser_Destroy(struct Papyrus_Parser* parser);

struct Papyrus_ParseOptions
{
	// parameters: context, size
	void*(*allocateSyntax)(void*, uintptr_t);
	void* allocateSyntaxContext;

	// parameters: context, line, column, message
	void(*reportSyntaxError)(void*, int32_t, int32_t, struct Papyrus_String);
	void* reportSyntaxErrorContext;
};

Papyrus_Error
Papyrus_Parser_Parse(struct Papyrus_Parser* parser, const char* string, intptr_t stringSize,
	const struct Papyrus_ParseOptions* options, const struct Papyrus_Syntax_Script** out);
