#pragma once

#include <stddef.h>
#include <stdint.h>

enum
{
	Papyrus_Error_None = 0,

	Papyrus_Error_InvalidArgument,
	Papyrus_Error_OutOfMemory,

	Papyrus_Error_InvalidSyntax,
};
typedef uint32_t Papyrus_Error;

struct Papyrus_String
{
	const char* data;
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
	Papyrus_Syntax_BoolExpr,
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
	Papyrus_Flags_Auto = 0x1,
	Papyrus_Flags_BetaOnly = 0x2,
	Papyrus_Flags_Conditional = 0x4,
	Papyrus_Flags_Const = 0x8,
	Papyrus_Flags_DebugOnly = 0x10,
	Papyrus_Flags_Default = 0x20,
	Papyrus_Flags_Global = 0x40,
	Papyrus_Flags_Hidden = 0x80,
	Papyrus_Flags_Mandatory = 0x100,
	Papyrus_Flags_Native = 0x200,
};

struct Papyrus_Syntax
{
	uint32_t kind : 8;
	uint32_t flags : 4;
	uint32_t length : 20;
	uint32_t offset;
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
	const struct Papyrus_Syntax* type;
	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Params
{
	const struct Papyrus_Param* data;
	intptr_t size;
};

struct Papyrus_Scope
{
	struct Papyrus_SyntaxArray stmts;
};


struct Papyrus_Type
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_FullName name;
};

struct Papyrus_ArrayType
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* type;
};

struct Papyrus_Expr
{
	struct Papyrus_Syntax syntax;
};

struct Papyrus_Import
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_FullName name;
};

struct Papyrus_Script
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	struct Papyrus_FullName base;
	struct Papyrus_SyntaxArray defs;
};

struct Papyrus_State
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	struct Papyrus_SyntaxArray defs;
};

struct Papyrus_Variable
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* type;
	struct Papyrus_String name;
	const struct Papyrus_Syntax* expr;
};

struct Papyrus_Function
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* type;
	struct Papyrus_String name;
	struct Papyrus_Params params;
	struct Papyrus_Scope scope;
	uint32_t flags;
};

struct Papyrus_Property
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* type;
	struct Papyrus_String name;

	union {
		const struct Papyrus_Expr* expr;
		struct {
			const struct Papyrus_Function* get;
			const struct Papyrus_Function* set;
		};
	};
};

struct Papyrus_Event
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* type;
	struct Papyrus_String name;
	struct Papyrus_Params params;
	struct Papyrus_Scope scope;
	uint32_t flags;
};

struct Papyrus_CustomEvent
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
};

struct Papyrus_Struct
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	struct Papyrus_SyntaxArray vars;
};

struct Papyrus_Group
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_String name;
	struct Papyrus_SyntaxArray props;
	uint32_t flags;
};


struct Papyrus_ExprStmt
{
	struct Papyrus_Syntax syntax;
	const struct Papyrus_Syntax* expr;
};

struct Papyrus_ReturnStmt
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
};

struct Papyrus_IfStmt
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* cond;
	struct Papyrus_Scope scope;
	struct {
		const struct Papyrus_ElseIfClause* const* data;
		intptr_t size;
	} elifs;
	const struct Papyrus_ElseClause* else_;
};

struct Papyrus_WhileStmt
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* cond;
	struct Papyrus_Scope scope;
};

struct Papyrus_ElseIfClause
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* cond;
	struct Papyrus_Scope scope;
};

struct Papyrus_ElseClause
{
	struct Papyrus_Syntax syntax;

	struct Papyrus_Scope scope;
};


struct Papyrus_NoneExpr
{
	struct Papyrus_Syntax syntax;
};

struct Papyrus_AccessExpr
{
	struct Papyrus_Syntax syntax;

	const struct Papyrus_Syntax* expr;
	struct Papyrus_String name;
};

struct Papyrus_NameExpr
{
	struct Papyrus_Syntax syntax;
	struct Papyrus_FullName name;
};

struct Papyrus_NewExpr
{
	struct Papyrus_Syntax syntax;
	struct Papyrus_FullName name;
	const struct Papyrus_Syntax* extent;
};

#define Papyrus_DEFINE_EXPR(name) \
	struct Papyrus_ ## name ## Expr \
	{ \
		struct Papyrus_Syntax syntax; \
		struct Papyrus_String string; \
	}
Papyrus_DEFINE_EXPR(Int);
Papyrus_DEFINE_EXPR(Float);
Papyrus_DEFINE_EXPR(String);
Papyrus_DEFINE_EXPR(Bool);
#undef Papyrus_DEFINE_EXPR

#define Papyrus_DEFINE_EXPR(name) \
	struct Papyrus_ ## name ## Expr \
	{ \
		struct Papyrus_Syntax syntax; \
		const struct Papyrus_Syntax* expr; \
	}
Papyrus_DEFINE_EXPR(Unary);
Papyrus_DEFINE_EXPR(Neg);
Papyrus_DEFINE_EXPR(Not);
#undef Papyrus_DEFINE_EXPR

#define Papyrus_DEFINE_EXPR(name) \
	struct Papyrus_ ## name ## Expr \
	{ \
		struct Papyrus_Syntax syntax; \
		const struct Papyrus_Syntax* left; \
		const struct Papyrus_Syntax* right; \
	}
Papyrus_DEFINE_EXPR(Binary);
Papyrus_DEFINE_EXPR(Add);
Papyrus_DEFINE_EXPR(Sub);
Papyrus_DEFINE_EXPR(Mul);
Papyrus_DEFINE_EXPR(Div);
Papyrus_DEFINE_EXPR(Mod);
Papyrus_DEFINE_EXPR(Assign);
Papyrus_DEFINE_EXPR(AddAssign);
Papyrus_DEFINE_EXPR(SubAssign);
Papyrus_DEFINE_EXPR(MulAssign);
Papyrus_DEFINE_EXPR(DivAssign);
Papyrus_DEFINE_EXPR(ModAssign);
Papyrus_DEFINE_EXPR(Con);
Papyrus_DEFINE_EXPR(Dis);
#undef Papyrus_DEFINE_EXPR

#define Papyrus_DEFINE_EXPR(name) \
	struct Papyrus_ ## name ## Expr \
	{ \
		struct Papyrus_Syntax syntax; \
		const struct Papyrus_Syntax* expr; \
		const struct Papyrus_Syntax* type; \
	}
Papyrus_DEFINE_EXPR(Cast);
Papyrus_DEFINE_EXPR(As);
Papyrus_DEFINE_EXPR(Is);
#undef Papyrus_DEFINE_EXPR

#define Papyrus_DEFINE_EXPR(name) \
	struct Papyrus_ ## name ## Expr \
	{ \
		struct Papyrus_Syntax syntax; \
		const struct Papyrus_Syntax* expr; \
		struct Papyrus_SyntaxArray args; \
	}
Papyrus_DEFINE_EXPR(Invoke);
Papyrus_DEFINE_EXPR(Call);
Papyrus_DEFINE_EXPR(Index);
#undef Papyrus_DEFINE_EXPR


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
};

struct Papyrus_Parser;

Papyrus_Error
Papyrus_Parser_Create(const struct Papyrus_ParserOptions* options, struct Papyrus_Parser** out);

void
Papyrus_Parser_Destroy(struct Papyrus_Parser* parser);

struct Papyrus_ParseOptions
{
	void*(*allocateSyntax)(void*, uintptr_t);
	void* allocateSyntaxContext;
};

struct Papyrus_ParseResult
{
	union {
		const struct Papyrus_Script* script;

		struct {
			int32_t line;
			int32_t column;
		} syntaxError;
	};
};

Papyrus_Error
Papyrus_Parser_Parse(struct Papyrus_Parser* parser, const char* string, intptr_t stringSize,
	const struct Papyrus_ParseOptions* options, struct Papyrus_ParseResult* out);
