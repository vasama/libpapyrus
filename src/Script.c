/* This file implements construction and analysis of script semantic
descriptions. Extern symbols are used as placeholders for external symbols.
Linking the script with a program associates each extern with a symbol in
another script, if available. */

#define Papyrus_Type_N
#include "Script.h"

#include "Papyrus/Parser.h"
#include "Papyrus/PascalString.h"
#include "Papyrus/Syntax.h"

#include "Arena.h"
#include "Array.h"
#include "Except.h"
#include "HashTable.h"
#include "Macros.h"
#include "ObjectGraph.h"
#include "Program.h"
#include "StringHash.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

typedef struct Papyrus_Syntax Syntax;
#define SYNTAX(x) struct Papyrus_Syntax_ ## x

struct Function
{
	struct Papyrus_Function public;

	struct Papyrus_IR* ir;

	SYNTAX(Function)* syntax;
};

struct Variable
{
	struct Papyrus_Variable public;

	SYNTAX(Type)* typeSyntax;
	Syntax* initSyntax;
};

struct Property
{
	struct Papyrus_Property public;
	
	SYNTAX(Property)* syntax;
};

struct Script
{
	struct ScriptInternal internal;

	// Papyrus_String -> Papyrus_Symbol*
	struct HashTable symbolTable;

	struct {
		struct Papyrus_Expr* expr;
	} freelist;
};

Array_DEFINE(FunctionArray, struct Function*);
Array_DEFINE(VariableArray, struct Variable*);
Array_DEFINE(PropertyArray, struct Property*);
Array_DEFINE(ExternArray, struct Papyrus_Extern*);

Array_DEFINE_STACK(IntptrStack, intptr_t);
Array_DEFINE_STACK(StmtStack, struct Papyrus_Stmt*);

Array_DEFINE(SymbolArray, struct Papyrus_Symbol*);

HashTable_DEFINE_MAP(SymbolMap, struct Papyrus_String,
	struct Papyrus_Symbol*, String_Hash, String_Equal);

HashTable_DEFINE_MAP(ExternMap, struct Papyrus_String,
	struct Papyrus_Extern*, String_Hash, String_Equal);

Array_DEFINE(TypeArray, struct Papyrus_Type*);
HashTable_DEFINE_MAP(LocalMap, struct Papyrus_String,
	intptr_t, String_Hash, String_Equal);
Array_DEFINE_STACK(TypeStack, struct Papyrus_Type*);


#define INTRINSIC_TYPES(X) \
	X(Error, Error, "<error-type>", Papyrus_TypeFlags_Error) \
	X(None, None, "none", 0) \
	X(Int, Primitive, "int", \
		Papyrus_TypeFlags_Primitive | Papyrus_TypeFlags_Arithmetic) \
	X(Bool, Primitive, "bool", Papyrus_TypeFlags_Primitive) \
	X(Float, Primitive, "float", \
		Papyrus_TypeFlags_Primitive | Papyrus_TypeFlags_Arithmetic) \
	X(String, Primitive, "string", Papyrus_TypeFlags_Primitive) \

#define X_ENTRY(x, k, n, f) \
	static const struct Papyrus_Symbol IntrinsicSymbol_ ## x = { \
		.kind = Papyrus_Symbol_Intrinsic, \
		.name = Papyrus_String_INIT(n), \
	};
INTRINSIC_TYPES(X_ENTRY)
#undef X_ENTRY

#define X_ENTRY(x, k, n, f) \
	static const struct Papyrus_Type IntrinsicArrayType_ ## x;
INTRINSIC_TYPES(X_ENTRY)
#undef X_ENTRY

#define X_ENTRY(x, k, n, f) \
	[Papyrus_Type_ ## x] = { \
		.type = Papyrus_Type_ ## x, \
		.kind = Papyrus_TypeKind_ ## k, \
		.flags = Papyrus_TypeFlags_Intrinsic | (f), \
		.symbol = (struct Papyrus_Symbol*)&IntrinsicSymbol_ ## x, \
		.array = (struct Papyrus_Type*)&IntrinsicArrayType_ ## x, \
	},

static const struct Papyrus_Type IntrinsicTypes[] = {
	INTRINSIC_TYPES(X_ENTRY)
};
#undef X_ENTRY

#define X_ENTRY(x, k, n, f) \
	static const struct Papyrus_Type IntrinsicArrayType_ ## x = { \
		.type = Papyrus_Type_Array, \
		.kind = Papyrus_TypeKind_Array, \
		.flags = Papyrus_TypeFlags_Intrinsic | Papyrus_TypeFlags_Array, \
		.element = (struct Papyrus_Type*)&IntrinsicTypes[Papyrus_Type_ ## x], \
		.array = NULL, \
	};
INTRINSIC_TYPES(X_ENTRY)
#undef X_ENTRY

#undef INTRINSIC_TYPES

#define IntrinsicType(x) \
	((struct Papyrus_Type*)&IntrinsicTypes[Papyrus_Type_ ## x])


typedef struct {
	struct Script* script;
	struct Papyrus_Allocator allocator;
	struct Arena arena;
	struct Papyrus_Diagnostics* diag;
	struct Papyrus_Except except;
} Ctx;

static inline void NORETURN
ICE_(Ctx* ctx, const char* file, int line)
{
	struct Papyrus_String string;

	char buffer[1024];
	string.data = buffer;
	string.size = snprintf(buffer, sizeof(buffer),
		"internal compiler error at %s:%d", file, line);

	struct Papyrus_Diagnostics* diag = ctx->diag;
	diag->report(diag, 0, string);

	Papyrus_Throw(&ctx->except);
}

#define ICE(ctx) \
	ICE_(&(ctx)->common, __FILE__, __LINE__)

#define ICEA(ctx, cond) \
	((cond) ? (void)0 : ICE(ctx))


typedef struct {
	Ctx common;

	struct Papyrus_String scriptName;

	struct Papyrus_Program* program;

	struct Array exports;
	struct Array functions;
	struct Array variables;
	struct Array properties;
	struct Array externs;

	// Papyrus_Syntax_Symbol* -> Papyrus_Extern*
	struct HashTable externTable;

	struct Papyrus_Type* returnType;

	struct Array statements;
	intptr_t scopeStatementCount;

	struct {
		// Papyrus_Type*
		struct Array types;

		// Papyrus_String -> intptr_t
		struct HashTable indexTable;

		// intptr_t, Type
		struct Array shadowStack;
		intptr_t shadowCount;
	} locals;
} BCtx;

static inline void*
Allocate_(Ctx* ctx, uintptr_t size)
{
	return ObjectGraph_Allocate(ctx->script, size);
}

#define Allocate(ctx, type) \
	((type*)Allocate_(&(ctx)->common, sizeof(type)))

#define AllocateArray(ctx, type, size) \
	((type*)Allocate_(&(ctx)->common, sizeof(type) * (uintptr_t)(size)))

static inline void*
Commit(Ctx* ctx, const void* data, uintptr_t size)
{
	void* buf = Allocate_(ctx, size);
	memcpy(buf, data, size);
	return buf;
}

static inline struct Papyrus_PascalString*
CreatePascalString(BCtx* ctx, struct Papyrus_String string)
{
	struct Papyrus_PascalString* pString =
		(struct Papyrus_PascalString*)Allocate_(&ctx->common,
			sizeof(struct Papyrus_PascalString) + string.size);

	memcpy(pString->data, string.data, string.size);
	pString->size = string.size;
	return pString;
}

static inline struct Papyrus_Symbol*
CreateSymbol_(BCtx* ctx, uintptr_t size)
{
	struct Symbol* symbol = (struct Symbol*)
		Allocate_(&ctx->common, sizeof(struct Symbol) + size);

	symbol->link = NULL;
	
	struct Papyrus_Symbol* public = symbol->symbol;
	public->script = &ctx->common.script->internal.public;

	return public;
}

#define CreateSymbol(ctx, type) \
	((type*)CreateSymbol_((ctx), sizeof(type)))


static void
ReportError_(Ctx* ctx, uint32_t source, const char* fmt, ...)
{
	struct Papyrus_String string;
	
	char buffer[1024];
	string.data = buffer;

	va_list vlist;
	va_start(vlist, fmt);
	string.size = vsnprintf(buffer, sizeof(buffer), fmt, vlist);
	va_end(vlist);

	struct Papyrus_Diagnostics* diag = ctx->diag;
	diag->report(diag, source, string);
}

#define ReportError(ctx, source, fmt, ...) \
	ReportError_(&(ctx)->common, (source), (fmt), ##__VA_ARGS__)


static struct Papyrus_Symbol*
ResolveExtern(BCtx* ctx, SYNTAX(Symbol)* syntax)
{
	assert(syntax->size == 1);
	struct Papyrus_String name = syntax->data[0];

	struct Papyrus_Extern** mapSymbol;
	if (!ExternMap_Insert(&ctx->externTable,
		&name, &ctx->common.allocator, &mapSymbol))
	{
		return &(*mapSymbol)->symbol;
	}

	struct Papyrus_Extern* externSymbol =
		CreateSymbol(ctx, struct Papyrus_Extern);
	externSymbol->symbol.kind = Papyrus_Symbol_Extern;
	externSymbol->symbol.flags = 0;
	externSymbol->symbol.name = name;
	externSymbol->link = NULL;

	ExternArray_Append(&ctx->externs, &externSymbol,
		Arena_CreateAllocator(&ctx->common.arena));

	*mapSymbol = externSymbol;
	return &externSymbol->symbol;
}

static struct Papyrus_Symbol*
ResolveSymbol(BCtx* ctx, SYNTAX(Symbol)* syntax)
{
	assert(syntax->size == 1);
	struct Papyrus_String name = syntax->data[0];

	struct Script* script = ctx->common.script;

	if (Papyrus_String_ICompare(name, ctx->scriptName) == 0)
		return &ctx->common.script->internal.public.symbol;

	// check script symbols
	struct Papyrus_Symbol** mapSymbol =
		SymbolMap_Find(&script->symbolTable, &name);

	if (mapSymbol != NULL)
		return *mapSymbol;

	return ResolveExtern(ctx, syntax);
}

static struct Papyrus_Type*
ResolveType(BCtx* ctx, SYNTAX(Type)* syntax)
{
	switch (syntax->syntax.ekind)
	{
#ifndef __INTELLISENSE__
#define FUNDAMENTAL(x) \
	case Papyrus_Syntax_Type_ ## x: \
		return IntrinsicType(x)

		FUNDAMENTAL(Int);
		FUNDAMENTAL(Bool);
		FUNDAMENTAL(Float);
		FUNDAMENTAL(String);
#undef FUNDAMENTAL
#endif
	}

	struct Papyrus_Symbol* symbol = ResolveSymbol(ctx, syntax->symbol);

	switch (symbol->kind)
	{
	case Papyrus_Symbol_Extern:
		{
			struct Papyrus_Extern* externSymbol =
				(struct Papyrus_Extern*)symbol;

			struct Papyrus_Type* type = externSymbol->type;
			if (type == NULL)
			{
				type = Allocate(ctx, struct Papyrus_Type);
				type->type = Papyrus_Type_Extern;
				type->kind = Papyrus_TypeKind_Extern;
				type->flags = 0;
				type->eflags = 0;
				type->symbol = symbol;

				struct Papyrus_Type* arrayType =
					Allocate(ctx, struct Papyrus_Type);
				arrayType->type = Papyrus_Type_Array;
				arrayType->kind = Papyrus_TypeKind_Array;
				arrayType->flags =
					Papyrus_TypeFlags_Intrinsic | Papyrus_TypeFlags_Array;
				arrayType->eflags = 0;
				arrayType->element = type;

				type->array = arrayType;
			}
			return type;
		}
		ICE(ctx);

	case Papyrus_Symbol_Script:
		return ((struct Papyrus_Script*)symbol)->type;

	default:
		return IntrinsicType(Error);
	}
}


static intptr_t
CreateLocal(BCtx* ctx, struct Papyrus_String name,
	struct Papyrus_Type* type, uint32_t sourceOffset)
{
	intptr_t index;

	intptr_t* mapIndex;
	if (LocalMap_Insert(&ctx->locals.indexTable,
		&name, &ctx->common.allocator, &mapIndex))
	{
		index = TypeArray_Size(&ctx->locals.types);
		*mapIndex = index;
		TypeArray_Append(&ctx->locals.types, &type, ctx->common.allocator);
	}
	else
	{
		index = *mapIndex;
		struct Papyrus_Type** pType =
			&TypeArray_Data(&ctx->locals.types)[index];

		if (*pType != NULL)
		{
			ReportError(ctx, sourceOffset, "local variable shadowing");

			IntptrStack_Push(&ctx->locals.shadowStack,
				&index, ctx->common.allocator);
			TypeStack_Push(&ctx->locals.shadowStack,
				pType, ctx->common.allocator);
			++ctx->locals.shadowCount;
		}
		*pType = type;
	}

	return index;
}

static intptr_t
GetLocal(BCtx* ctx, struct Papyrus_String name)
{
	intptr_t* mapIndex = LocalMap_Find(&ctx->locals.indexTable, &name);

	if (mapIndex == NULL)
		return -1;

	intptr_t index = *mapIndex;
	struct Papyrus_Type* type = TypeArray_Data(&ctx->locals.types)[index];

	if (type == NULL)
		return -1;

	return index;
}


static struct Papyrus_String
CommitString(BCtx* ctx, struct Papyrus_String string)
{
	string.data = (const char*)Commit(&ctx->common, string.data, string.size);
	return string;
}

static uintptr_t
CommitArray_(BCtx* ctx, struct Array* array, void** out)
{
	uintptr_t size = Array_Size(array);
	void* data = Allocate_(&ctx->common, size);
	memcpy(data, Array_Data(array), size);
	*out = data;
	return size;
}

#define CommitArray(ctx, array, out) \
	((void)((out)->size = (intptr_t)(CommitArray_((ctx), \
		(array), (void**)&(out)->data) / sizeof(*(out)->data))))


static struct Papyrus_Expr*
BuildExpr(BCtx* ctx, Syntax* syntax);

static struct Papyrus_Expr*
BuildExpr_NameExpr(BCtx* ctx, SYNTAX(NameExpr)* syntax)
{
	SYNTAX(Symbol)* symbolSyntax = syntax->symbol;

	struct Papyrus_Expr* expr = Allocate(ctx, struct Papyrus_Expr);

	if (symbolSyntax->size == 1)
	{
		intptr_t localIndex = GetLocal(ctx, symbolSyntax->data[0]);

		if (localIndex < 0)
			goto global_symbol;

		expr->kind = Papyrus_Expr_ReadLocal;
		expr->flags = 0;
		expr->localIndex = (uint32_t)localIndex;
	}
	else
	{
	global_symbol:;
		struct Papyrus_Symbol* symbol = ResolveSymbol(ctx, symbolSyntax);

		expr->kind = Papyrus_Expr_Symbol;
		expr->flags = 0;
		expr->symbol = symbol;
	}

	return expr;
}

static struct Papyrus_Expr*
BuildExpr_ConstExpr(BCtx* ctx, SYNTAX(ConstExpr)* syntax)
{
	struct Papyrus_Expr* expr = Allocate(ctx, struct Papyrus_Expr);

	switch (syntax->syntax.ekind)
	{
	case Papyrus_Syntax_ConstExpr_None:
		expr->kind = Papyrus_Expr_LitNone;
		expr->type = IntrinsicType(None);
		break;

	case Papyrus_Syntax_ConstExpr_Int:
		{
			expr->kind = Papyrus_Expr_LitInt;
			expr->type = IntrinsicType(Int);

			char buf[256];
			intptr_t size =
				MIN(syntax->string.size, (intptr_t)sizeof(buf) - 1);
			memcpy(buf, syntax->string.data, size);
			buf[size] = 0;

			expr->lit.int_ = strtol(buf, NULL, 0);
		}
		break;

	case Papyrus_Syntax_ConstExpr_True:
		expr->kind = Papyrus_Expr_LitBool;
		expr->type = IntrinsicType(Bool);
		expr->lit.bool_ = true;
		break;

	case Papyrus_Syntax_ConstExpr_False:
		expr->kind = Papyrus_Expr_LitBool;
		expr->type = IntrinsicType(Bool);
		expr->lit.bool_ = false;
		break;

	case Papyrus_Syntax_ConstExpr_Float:
		{
			expr->kind = Papyrus_Expr_LitFloat;
			expr->type = IntrinsicType(Float);

			char buf[256];
			intptr_t size =
				MIN(syntax->string.size, (intptr_t)sizeof(buf) - 1);
			memcpy(buf, syntax->string.data, size);
			buf[size] = 0;

			expr->lit.float_ = strtof(buf, NULL);
		}
		break;

	case Papyrus_Syntax_ConstExpr_String:
		expr->kind = Papyrus_Expr_LitString;
		expr->type = IntrinsicType(String);
		expr->lit.string = CommitString(ctx, syntax->string);
		break;
	}
	expr->flags = Papyrus_ExprFlags_Const;
	return expr;
}

static struct Papyrus_Expr*
BuildExpr_UnaryExpr(BCtx* ctx, SYNTAX(UnaryExpr)* syntax)
{
	struct Papyrus_Expr* expr = Allocate(ctx, struct Papyrus_Expr);
	struct Papyrus_Expr* sub = BuildExpr(ctx, syntax->expr);

	uint32_t kind;
	struct Papyrus_Type* type = expr->type;
	switch (syntax->syntax.ekind)
	{
	case Papyrus_Syntax_UnaryExpr_Neg:
		kind = Papyrus_Expr_Not;
		type = IntrinsicType(Error);
		break;
		
	case Papyrus_Syntax_UnaryExpr_Not:
		kind = Papyrus_Expr_Not;
		type = IntrinsicType(Bool);
		break;

	default:
		ICE(ctx);
	}

	expr->kind = kind;
	expr->flags = sub->flags;
	expr->oper.expr = sub;
	expr->type = type;
	return expr;
}

static struct Papyrus_Expr*
BuildExpr_BinaryExpr(BCtx* ctx, SYNTAX(BinaryExpr)* syntax)
{
	struct Papyrus_Expr* expr = Allocate(ctx, struct Papyrus_Expr);

	struct Papyrus_Expr* lhs = BuildExpr(ctx, syntax->left);
	struct Papyrus_Expr* rhs = BuildExpr(ctx, syntax->right);

	uint32_t kind;
	struct Papyrus_Type* type;
	switch (syntax->syntax.ekind)
	{
#ifndef __INTELLISENSE__
#define OPERATOR(x, t) \
	case Papyrus_Syntax_BinaryExpr_ ## x: \
		kind = Papyrus_Expr_ ## x; \
		type = IntrinsicType(t); \
		break;

		OPERATOR(Add, Error);
		OPERATOR(Sub, Error);
		OPERATOR(Mul, Error);
		OPERATOR(Div, Error);
		OPERATOR(Mod, Error);

		OPERATOR(Eq, Bool);
		OPERATOR(Ne, Bool);
		OPERATOR(Lt, Bool);
		OPERATOR(Gt, Bool);
		OPERATOR(Le, Bool);
		OPERATOR(Ge, Bool);
#undef OPERATOR
#endif
	}

	expr->kind = kind;
	expr->flags =
		(lhs->flags & rhs->flags) & Papyrus_ExprFlags_Const;
	expr->type = type;
	expr->oper.lhs = lhs;
	expr->oper.rhs = rhs;
	return expr;
}

static struct Papyrus_Expr*
BuildExpr_AccessExpr(BCtx* ctx, SYNTAX(AccessExpr)* syntax)
{
	struct Papyrus_Expr* expr = Allocate(ctx, struct Papyrus_Expr);

	struct Papyrus_Expr* object = BuildExpr(ctx, syntax->expr);

	expr->kind = Papyrus_Expr_Access;
	expr->ekind = 0;
	expr->flags = object->flags;
	expr->type = IntrinsicType(Error);
	expr->access.expr = object;
	expr->access.name = CreatePascalString(ctx, syntax->name);
	expr->access.symbol = NULL;
	return expr;
}

static struct Papyrus_Expr*
BuildExpr_CallExpr(BCtx* ctx, SYNTAX(CallExpr)* syntax)
{
	struct Papyrus_Expr* expr = Allocate(ctx, struct Papyrus_Expr);

	Syntax* funcSyntax = syntax->expr;
	switch (funcSyntax->kind)
	{
	case Papyrus_Syntax_NameExpr:
		{
			SYNTAX(NameExpr)* exprSyntax = (SYNTAX(NameExpr)*)funcSyntax;
			expr->call.object = NULL;
			expr->call.name = NULL;
			expr->call.symbol = ResolveSymbol(ctx, exprSyntax->symbol);
		}
		break;

	case Papyrus_Syntax_AccessExpr:
		{
			SYNTAX(AccessExpr)* exprSyntax = (SYNTAX(AccessExpr)*)funcSyntax;
			expr->call.object = BuildExpr(ctx, exprSyntax->expr);
			expr->call.name = CreatePascalString(ctx, exprSyntax->name);
			expr->call.symbol = NULL;
		}
		break;
	}

	intptr_t argCount = syntax->args.size;
	struct Papyrus_Expr** args = AllocateArray(
		ctx, struct Papyrus_Expr*, argCount);

	FOREACHV_S(argSyntax, arg_i, &syntax->args)
	{
		args[arg_i] = BuildExpr(ctx, argSyntax);
	}

	expr->kind = Papyrus_Expr_Call;
	expr->ekind = 0;
	expr->flags = 0;
	expr->type = IntrinsicType(Error);
	expr->call.args.data = args;
	expr->call.args.size = argCount;
	return expr;
}

static struct Papyrus_Expr*
BuildExpr(BCtx* ctx, Syntax* syntax)
{
	struct Papyrus_Expr* expr;
	switch (syntax->kind)
	{
#define EXPR(x) \
	case Papyrus_Syntax_ ## x: \
		expr = BuildExpr_ ## x(ctx, (SYNTAX(x)*)syntax); \
		break

		EXPR(NameExpr);
		EXPR(ConstExpr);
		EXPR(UnaryExpr);
		EXPR(BinaryExpr);
		EXPR(AccessExpr);
		EXPR(CallExpr);
#undef EXPR

	default:
		ICE(ctx);
	}

	expr->source = syntax->offset;
	return expr;
}


static struct Papyrus_Scope
BuildScope(BCtx* ctx, SYNTAX(Scope)* syntax);

static struct Papyrus_Stmt*
BuildStmt_ExprStmt(BCtx* ctx, SYNTAX(ExprStmt)* syntax)
{
	struct Papyrus_Stmt* stmt = Allocate(ctx, struct Papyrus_Stmt);
	stmt->kind = Papyrus_Stmt_Expr;
	stmt->flags = 0;
	stmt->expr = BuildExpr(ctx, syntax->expr);
	return stmt;
}

static struct Papyrus_Stmt*
BuildStmt_AssignStmt(BCtx* ctx, SYNTAX(AssignStmt)* syntax)
{
	struct Papyrus_Expr* expr = Allocate(ctx, struct Papyrus_Expr);
	expr->source = syntax->syntax.offset;

	Syntax* dstSyntax = syntax->object;
	switch (dstSyntax->kind | dstSyntax->ekind << 8)
	{
	case Papyrus_Syntax_NameExpr:
		{
			SYNTAX(Symbol)* syntax = ((SYNTAX(NameExpr)*)dstSyntax)->symbol;
			if (syntax->size == 1)
			{
				intptr_t localIndex = GetLocal(ctx, syntax->data[0]);

				if (localIndex < 0)
					goto global_symbol;

				expr->kind = Papyrus_Expr_WriteLocal;
				expr->assign.localIndex = (uint32_t)localIndex;
			}
			else
			{
			global_symbol:;
				struct Papyrus_Symbol* symbol = ResolveSymbol(ctx, syntax);
				expr->kind = Papyrus_Expr_Assign;
				expr->assign.object = NULL;
				expr->assign.symbol = symbol;
			}
		}
		break;

	case Papyrus_Syntax_AccessExpr:
		{
			SYNTAX(AccessExpr)* syntax = (SYNTAX(AccessExpr)*)dstSyntax;
			expr->kind = Papyrus_Expr_Assign;
			expr->assign.object = BuildExpr(ctx, syntax->expr);
			expr->assign.name = CreatePascalString(ctx, syntax->name);
			expr->assign.symbol = NULL;
		}
		break;

	case Papyrus_Syntax_BinaryExpr | Papyrus_Syntax_BinaryExpr_Index << 8:
		{
			SYNTAX(BinaryExpr)* syntax = (SYNTAX(BinaryExpr)*)dstSyntax;
			expr->kind = Papyrus_Expr_Assign;
			expr->assign.array = BuildExpr(ctx, syntax->left);
			expr->assign.subscript = BuildExpr(ctx, syntax->right);
		}
		break;

	default:
		ICE(ctx);
	}

	expr->flags = 0;
	expr->assign.expr = BuildExpr(ctx, syntax->expr);

	struct Papyrus_Stmt* stmt = Allocate(ctx, struct Papyrus_Stmt);
	stmt->source = syntax->syntax.offset;
	stmt->kind = Papyrus_Stmt_Expr;
	stmt->flags = 0;
	stmt->expr = expr;
	stmt->source = syntax->syntax.offset;

	return stmt;
}

static struct Papyrus_Stmt*
BuildStmt_ReturnStmt(BCtx* ctx, SYNTAX(ReturnStmt)* syntax)
{
	struct Papyrus_Stmt* stmt = Allocate(ctx, struct Papyrus_Stmt);

	struct Papyrus_Expr* expr; {
		Syntax* exprSyntax = syntax->expr;
		if (exprSyntax != NULL)
		{
			expr = BuildExpr(ctx, exprSyntax);
		}
		else expr = NULL;
	}

	stmt->kind = Papyrus_Stmt_Return;
	stmt->flags = 0;
	stmt->expr = expr;
	return stmt;
}

static struct Papyrus_Stmt*
BuildStmt_IfStmt(BCtx* ctx, SYNTAX(IfStmt)* syntax)
{
	struct Papyrus_Stmt* stmt = Allocate(ctx, struct Papyrus_Stmt);

	intptr_t clauseCount = syntax->clauses.size;
	SYNTAX(Scope)* elseScope = syntax->elseScope;

	intptr_t branchCount = syntax->clauses.size + elseScope != NULL;
	struct Papyrus_Branch* branches =
		AllocateArray(ctx, struct Papyrus_Branch, branchCount);

	FOREACHV_S(x, i, &syntax->clauses)
	{
		struct Papyrus_Branch* branch = branches + i;
		branch->expr = BuildExpr(ctx, x->cond);
		branch->scope = BuildScope(ctx, x->scope);
	}

	if (elseScope != NULL)
	{
		struct Papyrus_Branch* branch = branches + clauseCount;
		branch->expr = NULL;
		branch->scope = BuildScope(ctx, elseScope);
	}

	stmt->kind = Papyrus_Stmt_Branch;
	stmt->flags = 0;
	stmt->branch.data = branches;
	stmt->branch.size = branchCount;
	return stmt;
}

static struct Papyrus_Stmt*
BuildStmt_WhileStmt(BCtx* ctx, SYNTAX(WhileStmt)* syntax)
{
	struct Papyrus_Stmt* stmt = Allocate(ctx, struct Papyrus_Stmt);
	stmt->kind = Papyrus_Stmt_Loop;
	stmt->flags = 0;
	stmt->loop.expr = BuildExpr(ctx, syntax->cond);
	stmt->loop.scope = BuildScope(ctx, syntax->scope);
	return stmt;
}

static void
BuildStmt(BCtx* ctx, Syntax* syntax)
{
	switch (syntax->kind)
	{
	case Papyrus_Syntax_Variable:
		{
			SYNTAX(Variable)* variableSyntax = (SYNTAX(Variable)*)syntax;
			struct Papyrus_Type* type = ResolveType(ctx, variableSyntax->type);

			intptr_t index = CreateLocal(ctx,
				variableSyntax->name, type, syntax->offset);

			Syntax* exprSyntax = variableSyntax->expr;
			if (exprSyntax != NULL)
			{
				struct Papyrus_Expr* expr = Allocate(ctx, struct Papyrus_Expr);
				expr->kind = Papyrus_Expr_WriteLocal;
				expr->flags = 0;
				expr->assign.localIndex = (uint32_t)index;
				expr->assign.expr = BuildExpr(ctx, exprSyntax);

				struct Papyrus_Stmt* stmt = Allocate(ctx, struct Papyrus_Stmt);
				stmt->kind = Papyrus_Stmt_Expr;
				stmt->flags = 0;
				stmt->expr = expr;

				StmtStack_Push(&ctx->statements, &stmt, ctx->common.allocator);
				++ctx->scopeStatementCount;
			}
		}
		break;

		{
			struct Papyrus_Stmt* stmt;

#ifndef __INTELLISENSE__
#define STMT(x) \
	case Papyrus_Syntax_ ## x: \
		stmt = BuildStmt_ ## x(ctx, (SYNTAX(x)*)syntax); \
		goto push_stmt

			STMT(ExprStmt);
			STMT(AssignStmt);
			STMT(ReturnStmt);
			STMT(IfStmt);
			STMT(WhileStmt);
#undef STMT
#endif

		push_stmt:
			stmt->source = syntax->offset;
			StmtStack_Push(&ctx->statements, &stmt, ctx->common.allocator);
			++ctx->scopeStatementCount;
		}
		break;

	default:
		ICE(ctx);
	}
}

static struct Papyrus_Scope
BuildScope(BCtx* ctx, SYNTAX(Scope)* syntax)
{
	struct Papyrus_Scope scope;

	/* enter scope */ {
		IntptrStack_Push(&ctx->locals.shadowStack,
			&ctx->locals.shadowCount, ctx->common.allocator);
		ctx->locals.shadowCount = 0;
	}

	/* build scope */ {
		IntptrStack_Push(&ctx->statements,
			&ctx->scopeStatementCount, ctx->common.allocator);
		ctx->scopeStatementCount = 0;

		FOREACHV_S(stmt, stmt_i, syntax)
		{
			BuildStmt(ctx, stmt);
		}

		intptr_t scopeStatementCount = ctx->scopeStatementCount;

		struct Papyrus_Stmt** statements = AllocateArray(
			ctx, struct Papyrus_Stmt*, scopeStatementCount);

		StmtStack_PopRange(&ctx->statements, scopeStatementCount, statements);

		scope.data = statements;
		scope.size = scopeStatementCount;

		ctx->scopeStatementCount = IntptrStack_Pop(&ctx->statements);
	}

	/* exit scope */ {
		intptr_t shadowCount = ctx->locals.shadowCount;
		for (intptr_t i = 0; i < shadowCount; ++i)
		{
			intptr_t index = IntptrStack_Pop(&ctx->locals.shadowStack);
			struct Papyrus_Type* type =
				TypeStack_Pop(&ctx->locals.shadowStack);
			TypeArray_Data(&ctx->locals.types)[index] = type;
		}
		ctx->locals.shadowCount = IntptrStack_Pop(&ctx->locals.shadowStack);
	}

	return scope;
}


static struct Function*
CreateFunction(BCtx* ctx, struct Papyrus_String name)
{
	struct Function* function = CreateSymbol(ctx, struct Function);
	function->public.symbol.kind = Papyrus_Symbol_Function;
	function->public.symbol.flags = 0;
	function->public.symbol.name = CommitString(ctx, name);
	function->syntax = NULL;

	FunctionArray_Append(&ctx->functions, &function,
		Arena_CreateAllocator(&ctx->common.arena));

	return function;
}

static struct Variable*
CreateVariable(BCtx* ctx, struct Papyrus_String name)
{
	struct Variable* variable = CreateSymbol(ctx, struct Variable);
	variable->public.symbol.kind = Papyrus_Symbol_Variable;
	variable->public.symbol.flags = 0;
	variable->public.symbol.name = CommitString(ctx, name);
	variable->typeSyntax = NULL;
	variable->initSyntax = NULL;

	VariableArray_Append(&ctx->variables, &variable,
		Arena_CreateAllocator(&ctx->common.arena));

	return variable;
}


static struct Papyrus_Symbol*
BuildFunction(BCtx* ctx, SYNTAX(Function)* syntax)
{
	struct Function* function = CreateFunction(ctx, syntax->name);
	function->syntax = syntax;
	function->public.symbol.flags |= Papyrus_SymbolFlags_Export;

	return &function->public.symbol;
}

static struct Papyrus_Symbol*
BuildVariable(BCtx* ctx, SYNTAX(Variable)* syntax)
{
	struct Variable* variable = CreateVariable(ctx, syntax->name);
	variable->typeSyntax = syntax->type;
	variable->initSyntax = syntax->expr;

	return &variable->public.symbol;
}

static struct Papyrus_Symbol*
BuildProperty(BCtx* ctx, SYNTAX(Property)* syntax)
{
	struct Property* property = CreateSymbol(ctx, struct Property);
	property->public.symbol.kind = Papyrus_Symbol_Property;
	property->public.symbol.flags = Papyrus_SymbolFlags_Export;
	property->public.symbol.name = syntax->name;
	property->syntax = syntax;

	PropertyArray_Append(&ctx->properties, &property,
		Arena_CreateAllocator(&ctx->common.arena));

	struct Variable* var = NULL;
	struct Function* get = NULL;
	struct Function* set = NULL;

	if (syntax->syntax.eflags & Papyrus_Syntax_DeclFlags_Auto)
	{
		var = CreateVariable(ctx,
			Papyrus_String_CREATE("<property-backing-variable>"));

		var->public.symbol.flags |= Papyrus_SymbolFlags_Hidden;
		var->typeSyntax = syntax->type;
		var->initSyntax = syntax->expr;
	}
	else
	{
		FOREACHV_S(x, x_i, syntax->scope)
		{
			assert(x->kind == Papyrus_Syntax_Function);
			SYNTAX(Function)* functionSyntax = (SYNTAX(Function)*)x;

			struct Papyrus_String name = functionSyntax->name;
			
			if (name.size != 3)
				goto accessor_name_error;

			struct Function** pf;
			switch (name.data[0])
			{
				char c;

			case 'G':
			case 'g':
				pf = &get;
				goto check_accessor_name;

			case 'S':
			case 's':
				pf = &set;
				goto check_accessor_name;

			check_accessor_name:
				if ((c = name.data[1]) != 'E' && c != 'e')
					goto accessor_name_error;

				if ((c = name.data[2]) != 'T' && c != 't')
					goto accessor_name_error;

			default:
			accessor_name_error:
				ReportError(ctx, functionSyntax->syntax.offset,
					"function does not name a property accessor");

				pf = NULL;
			}

			struct Function* func = CreateFunction(ctx,
				Papyrus_String_CREATE("<property-accessor-function>"));

			func->public.symbol.flags |= Papyrus_SymbolFlags_Hidden;
			func->syntax = functionSyntax;

			if (pf != NULL)
			{
				if (*pf != NULL)
				{
					ReportError(ctx, functionSyntax->syntax.offset,
						"duplicate property accessor");
				}
				else *pf = func;
			}

			if (pf != NULL && *pf != NULL)
				*pf = func;
		}
	}

	return &property->public.symbol;
}


static void
TypeFunction(BCtx* ctx, struct Function* function)
{
	SYNTAX(Function)* syntax = function->syntax;
	function->syntax = NULL;
	
	function->public.signature.returnType = ResolveType(ctx, syntax->type);

	/* parameter types */ {
		SYNTAX(ParamList)* paramListSyntax = syntax->params;
		intptr_t paramCount = paramListSyntax->size;
		if (paramCount > 0)
		{
			struct Papyrus_Type** paramTypes =
				AllocateArray(ctx, struct Papyrus_Type*, paramCount);

			FOREACHV_S(x, i, paramListSyntax)
			{
				struct Papyrus_Type* type = ResolveType(ctx, x->type);

				paramTypes[i] = type;
				CreateLocal(ctx, x->name, type, x->syntax.offset);
			}

			function->public.signature.paramTypes.data = paramTypes;
		}
		function->public.signature.paramTypes.size = paramCount;
	}

	function->public.scope = BuildScope(ctx, syntax->scope);

	CommitArray(ctx, &ctx->locals.types, &function->public.locals);

	Array_Clear(&ctx->statements);
	Array_Clear(&ctx->locals.types);
	Array_Clear(&ctx->locals.shadowStack);
	LocalMap_Clear(&ctx->locals.indexTable);
}

static void
TypeVariable(BCtx* ctx, struct Variable* variable)
{
	SYNTAX(Type)* typeSyntax = variable->typeSyntax;
	Syntax* initSyntax = variable->initSyntax;

	variable->public.type = ResolveType(ctx, typeSyntax);

	if (initSyntax != NULL)
	{
		variable->public.expr = BuildExpr(ctx, initSyntax);
	}
	else variable->public.expr = NULL;
}

static void
TypeProperty(BCtx* ctx, struct Property* property)
{
	SYNTAX(Property)* syntax = property->syntax;
	property->syntax = NULL;

	property->public.type = ResolveType(ctx, syntax->type);

	
}


static void
BuildScript(BCtx* ctx, struct Script* script, SYNTAX(Script)* scriptSyntax)
{
	struct Array imports;
	Array_Init(&imports);
	Array_Reserve(&imports, 32, ctx->common.allocator);

	SYNTAX(ScriptHeader)* headerSyntax = NULL;

	SYNTAX(Scope)* scopeSyntax = scriptSyntax->scope;
	FOREACHV_S(syntax, syntax_i, scopeSyntax)
	{
		switch (syntax->kind)
		{
		case Papyrus_Syntax_ScriptHeader:
			{
				if (syntax_i != 0)
				{
					ReportError(ctx, syntax->offset,
						"script header must be at "
						"the beginning of the script");
				}

				if (headerSyntax != NULL)
				{
					ReportError(ctx, syntax->offset,
						"duplicate script header");
				}
				else headerSyntax = (SYNTAX(ScriptHeader)*)syntax;
			}
			break;

		case Papyrus_Syntax_Import:
			{
				SYNTAX(Import)* importSyntax = (SYNTAX(Import)*)syntax;

				struct Papyrus_Symbol* externSymbol =
					ResolveExtern(ctx, importSyntax->symbol);

				SymbolArray_Append(&imports,
					&externSymbol, ctx->common.allocator);
			}
			break;

			{
				struct Papyrus_Symbol* symbol;

#ifndef __INTELLISENSE__
#define DECLARATION(x) \
	case Papyrus_Syntax_ ## x: \
		symbol = Build ## x(ctx, (SYNTAX(x)*)syntax); \
		goto declaration

				DECLARATION(Function);
				DECLARATION(Variable);
				DECLARATION(Property);
#undef DECLARATION
#endif

			declaration:
				if ((symbol->flags & Papyrus_SymbolFlags_Hidden) == 0)
				{
					SymbolArray_Append(&ctx->exports, &symbol,
						Arena_CreateAllocator(&ctx->common.arena));

					struct Papyrus_Symbol** mapSymbol;
					if (SymbolMap_Insert(&script->symbolTable,
						&symbol->name, &ctx->common.allocator, &mapSymbol))
					{
						*mapSymbol = symbol;
					}
					else
					{
						ReportError(ctx, syntax->offset, "duplicate symbol");
					}
				}
			}
			break;
		}
	}

	if (headerSyntax != NULL)
	{
		ctx->scriptName = headerSyntax->name;
		script->internal.public.symbol.name = headerSyntax->name;
	}
	else
	{
		ReportError(ctx, 0, "missing script header");

		ctx->scriptName = Papyrus_String_CREATE("");
		script->internal.public.symbol.name = Papyrus_String_CREATE("<error>");
	}

	CommitArray(ctx, &imports, &script->internal.public.imports);
	CommitArray(ctx, &ctx->exports, &script->internal.public.exports);
	CommitArray(ctx, &ctx->functions, &script->internal.public.functions);
	CommitArray(ctx, &ctx->variables, &script->internal.public.variables);
	CommitArray(ctx, &ctx->properties, &script->internal.public.properties);
	CommitArray(ctx, &ctx->externs, &script->internal.externs);

	FOREACHV_S(x, x_i, &script->internal.public.variables)
	{
		TypeVariable(ctx, (struct Variable*)x);
	}

	FOREACHV_S(x, x_i, &script->internal.public.functions)
	{
		TypeFunction(ctx, (struct Function*)x);
	}

	FOREACHV_S(x, x_i, &script->internal.public.properties)
	{
		TypeProperty(ctx, (struct Property*)x);
	}
}

struct Papyrus_Script*
Papyrus_Script_Create(
	struct Papyrus_SyntaxTree* tree, struct Papyrus_Allocator allocator,
	struct Papyrus_ArenaPool* pool, struct Papyrus_Diagnostics* diag)
{
	struct Script* script = ObjectGraph_CREATE(struct Script, 4096, allocator);
	script->internal.public.symbol.kind = Papyrus_Symbol_Script;
	script->internal.public.symbol.flags = 0;
	script->internal.public.symbol.eflags = 0;

	HashTable_Init(&script->symbolTable);

	script->freelist.expr = NULL;

	BCtx ctx;
	ctx.common.script = script;
	ctx.common.allocator = allocator;
	ctx.common.diag = diag;
	Arena_Init(&ctx.common.arena, pool);

	Array_Init(&ctx.exports);
	Array_Init(&ctx.functions);
	Array_Init(&ctx.variables);
	Array_Init(&ctx.properties);
	Array_Init(&ctx.statements);
	Array_Init(&ctx.externs);
	Array_Init(&ctx.locals.types);
	Array_Init(&ctx.locals.shadowStack);
	HashTable_Init(&ctx.locals.indexTable);
	ctx.locals.shadowCount = 0;

	Papyrus_TRY(&ctx.common.except)
	{
		BuildScript(&ctx, script, tree->script);
	}
	
	Array_Destroy(&ctx.statements, allocator);
	Array_Destroy(&ctx.locals.types, allocator);
	Array_Destroy(&ctx.locals.shadowStack, allocator);
	LocalMap_Destroy(&ctx.locals.indexTable, allocator);

	return &script->internal.public;
}

void
Papyrus_Script_Delete(struct Papyrus_Script* script)
{
	ObjectGraph_Delete(script);
}


void
Papyrus_Script_SetExtern(struct Papyrus_Script* script,
	struct Papyrus_Extern* externSymbol, struct Papyrus_Symbol* symbol)
{
	(void)script;

	static const struct Papyrus_Symbol ErrorSymbol = {
		.kind = Papyrus_Symbol_Intrinsic,
		.name = Papyrus_String_INIT("<undefined-extern-symbol>"),
	};

	externSymbol->link = symbol;

	struct Papyrus_Type* type = externSymbol->type;
	if (type != NULL)
	{
		switch (symbol != NULL ? symbol->kind : -1)
		{
		case Papyrus_Symbol_Script:
			type->type = Papyrus_Type_Script;
			type->kind = Papyrus_TypeKind_Script;
			type->flags = Papyrus_TypeFlags_Composite;
			type->eflags = 0;
			type->symbol = symbol;
			break;

		default:
			type->type = Papyrus_Type_Error;
			type->kind = Papyrus_TypeKind_Error;
			type->flags =
				Papyrus_TypeFlags_Intrinsic | Papyrus_TypeFlags_Error;
			type->eflags = 0;
			type->symbol = (struct Papyrus_Symbol*)&ErrorSymbol;
			break;
		}
	}
}

void
Papyrus_Script_Invalidate(struct Papyrus_Script* script)
{
	script->symbol.eflags &= ~Papyrus_ScriptFlags_Analyzed;
}


typedef struct {
	Ctx common;

	struct Papyrus_Type* returnType;
	struct Papyrus_Type** locals;

	struct {
		struct Papyrus_Expr* expr;
	} freelist;
} ACtx;

static struct Papyrus_Symbol*
LinkSymbol(ACtx* ctx, struct Papyrus_Symbol* symbol)
{
	static const struct Papyrus_Symbol MissingSymbol = {
		.kind = Papyrus_Symbol_Missing,
		.flags = 0,
		.eflags = 0,
		.name = Papyrus_String_INIT("<missing-symbol>"),
	};

	if (symbol->kind != Papyrus_Symbol_Extern)
		return symbol;

	struct Symbol* internal = GetSymbol(symbol);
	struct LinkSymbol* linkSymbol = internal->link;

	ICEA(ctx, linkSymbol != NULL);
	if (List_IsEmpty(&linkSymbol->scripts))
	{
		ReportError(ctx, 0, "undefined external symbol %.*s",
			(int)symbol->name.size, symbol->name.data);

		return (struct Papyrus_Symbol*)&MissingSymbol;
	}

	if (!List_IsSingleton(&linkSymbol->scripts))
	{
		ReportError(ctx, 0, "ambiguous external symbol %.*s",
			(int)symbol->name.size, symbol->name.data);
	}

	return List_GetObject(linkSymbol->scripts.next,
		struct Symbol, list)->symbol;
}

static struct Papyrus_Script*
FindCommonBase(struct Papyrus_Script* a, struct Papyrus_Script* b)
{
	if (b->base.depth > a->base.depth)
		SWAP(&a, &b);

	for (intptr_t i = a->base.depth - b->base.depth; i > 0; --i)
		a = a->base.script;

	for (intptr_t depth = a->base.depth; depth > 0; --depth)
	{
		a = a->base.script;
		b = b->base.script;

		if (a == b)
			return a;
	}

	return NULL;
}

static struct Papyrus_Type*
CommonType(struct Papyrus_Type* a, struct Papyrus_Type* b)
{
	static_assert(Papyrus_Type_Error == 0, "");
	static const uint8_t Conversions[Papyrus_Type_N * Papyrus_Type_N] = {
#define CONVERSION(a, b) \
	[Papyrus_Type_N * Papyrus_Type_ ## a + Papyrus_Type_ ## b]

		CONVERSION(Int, Int) = Papyrus_Type_Int,
		CONVERSION(Int, Float) = Papyrus_Type_Float,
		CONVERSION(Int, Bool) = Papyrus_Type_Bool,
		CONVERSION(Int, String) = Papyrus_Type_String,

		CONVERSION(Bool, Bool) = Papyrus_Type_Bool,
		CONVERSION(Bool, Int) = Papyrus_Type_Bool,
		CONVERSION(Bool, Float) = Papyrus_Type_Bool,
		CONVERSION(Bool, String) = Papyrus_Type_String,

		CONVERSION(Float, Int) = Papyrus_Type_Float,
		CONVERSION(Float, Bool) = Papyrus_Type_Bool,
		CONVERSION(Float, Float) = Papyrus_Type_Float,
		CONVERSION(Float, String) = Papyrus_Type_String,

		CONVERSION(String, Int) = Papyrus_Type_String,
		CONVERSION(String, Bool) = Papyrus_Type_String,
		CONVERSION(String, Float) = Papyrus_Type_String,
		CONVERSION(String, String) = Papyrus_Type_String,

		CONVERSION(Bool, Array) = Papyrus_Type_Bool,
		CONVERSION(Array, Bool) = Papyrus_Type_Bool,

		CONVERSION(Bool, Script) = Papyrus_Type_Bool,
		CONVERSION(Script, Bool) = Papyrus_Type_Bool,

		CONVERSION(String, Array) = Papyrus_Type_String,
		CONVERSION(Array, String) = Papyrus_Type_String,

		CONVERSION(String, Script) = Papyrus_Type_String,
		CONVERSION(Script, String) = Papyrus_Type_String,

		CONVERSION(Script, Script) = -1,
#undef CONVERSION
	};

	uint8_t common = Conversions[a->type * Papyrus_Type_N + b->type];

	if (LIKELY(common != (uint8_t)-1))
		return (struct Papyrus_Type*)&IntrinsicTypes[common];

	assert(a->type == Papyrus_Type_Script && b->type == Papyrus_Type_Script);

	struct Papyrus_Script* commonScript = FindCommonBase(
		(struct Papyrus_Script*)a->symbol, (struct Papyrus_Script*)b->symbol);

	if (commonScript != NULL)
	{
		return commonScript->type;
	}

	return IntrinsicType(Error);
}

static bool
IsBaseOf(struct Papyrus_Script* a, struct Papyrus_Script* b)
{
	for (intptr_t i = b->base.depth; i > 0; --i, b = b->base.script)
	{
		if (a == b)
			return true;
	}
	return false;
}

static bool
CanCastTo(struct Papyrus_Type* src, struct Papyrus_Type* dst, bool explicit)
{
	switch (dst->type)
	{
	case Papyrus_Type_Int:
		if (explicit == false)
		{
			return false;
		}
		switch (src->kind)
		{
		case Papyrus_Type_Float:
		case Papyrus_Type_String:
			return true;
		}
		return false;

	case Papyrus_Type_Bool:
		return true;

	case Papyrus_Type_Float:
		if (explicit == false)
		{
			return src->kind == Papyrus_Type_Int;
		}
		switch (src->kind)
		{
		case Papyrus_Type_Int:
		case Papyrus_Type_String:
			return true;
		}
		return false;

	case Papyrus_Type_String:
		return true;

	case Papyrus_Type_Array:
		if (explicit == false)
			return false;

		if (src->type != Papyrus_Type_Array)
			return false;

		return CanCastTo(dst->element, src->element, true);

	case Papyrus_Type_Script:
		{
			struct Papyrus_Script* srcs = (struct Papyrus_Script*)src->symbol;
			struct Papyrus_Script* dsts = (struct Papyrus_Script*)dst->symbol;

			if (IsBaseOf(dsts, srcs))
				return true;

			if (explicit == false)
				return false;
			
			return IsBaseOf(srcs, dsts);
		}
	}
	return false;
}

static void
ImplicitCast(ACtx* ctx, struct Papyrus_Expr* expr, struct Papyrus_Type* type)
{
	struct Papyrus_Type* exprType = expr->type;

	if (exprType == type)
		return;

	if ((exprType->flags | type->flags) & Papyrus_TypeFlags_Error)
		return;

	if (CanCastTo(exprType, type, false))
	{
		struct Papyrus_Expr* new = ctx->freelist.expr;
		if (new != NULL)
		{
			ctx->freelist.expr = *(struct Papyrus_Expr**)new;
		}
		else
		{
			new = Allocate(ctx, struct Papyrus_Expr);
		}

		*new = *expr;

		expr->kind = Papyrus_Expr_Cast;
		expr->flags = expr->flags | Papyrus_ExprFlags_Implicit;
		expr->type = type;
		expr->cast.expr = new;
		expr->cast.type = type;
	}
	else
	{
		ReportError(ctx, expr->source, "type conversion not available");
	}
}



static void
AnalyzeExpr(ACtx* ctx, struct Papyrus_Expr* expr);

static void
AnalyzeExpr_Symbol(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Symbol* symbol = expr->symbol;

	switch (symbol->kind)
	{
	case Papyrus_Symbol_Variable:
		{
			struct Papyrus_Variable* variable =
				(struct Papyrus_Variable*)symbol;

			expr->ekind = Papyrus_Expr_ReadField;
			expr->flags &= ~Papyrus_ExprFlags_Error;
			expr->type = variable->type;
		}
		break;

	case Papyrus_Symbol_Property:
		{
			struct Papyrus_Property* property =
				(struct Papyrus_Property*)symbol;

			expr->ekind = Papyrus_Expr_ReadProperty;
			expr->flags &= ~Papyrus_ExprFlags_Error;
			expr->type = property->type;
		}
		break;

	default:
		ReportError(ctx, expr->source, "expected an expression");
		expr->flags |= Papyrus_ExprFlags_Error;
		expr->type = IntrinsicType(Error);
	}
}

static void
AnalyzeExpr_Assign(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* object = expr->assign.object;
	struct Papyrus_Type* type = NULL;
	if (object != NULL)
	{
		AnalyzeExpr(ctx, object);
		
		struct Papyrus_String name =
			Papyrus_PascalString_GetString(expr->assign.name);

		//TODO: resolve member
		(void)name;
		ICE(ctx);

		struct Papyrus_Symbol* symbol;
		if (symbol->kind == Papyrus_Symbol_Property)
		{
			struct Papyrus_Property* property =
				(struct Papyrus_Property*)symbol;

			type = property->type;
		}
		else
		{
			ReportError(ctx, expr->source, "member is not a property");
		}
	}
	else
	{
		struct Papyrus_Symbol* symbol = expr->assign.symbol;
		struct Papyrus_Symbol* linkSymbol = LinkSymbol(ctx, symbol);

		switch (linkSymbol->kind)
		{
			struct Papyrus_Type* type;

		case Papyrus_Symbol_Variable:
			ICEA(ctx, symbol->kind != Papyrus_Symbol_Extern);
			expr->ekind = Papyrus_Expr_WriteField;
			type = ((struct Papyrus_Variable*)linkSymbol)->type;
			break;

		case Papyrus_Symbol_Property:
			expr->ekind = Papyrus_Expr_WriteProperty;
			type = ((struct Papyrus_Property*)linkSymbol)->type;
			break;

		default:
			ReportError(ctx, expr->source, "member is not a property");
		}
	}

	struct Papyrus_Expr* value = expr->assign.expr;
	AnalyzeExpr(ctx, value);

	if (type != NULL)
	{
		ImplicitCast(ctx, value, type);
		expr->flags =
			(expr->flags & ~Papyrus_ExprFlags_Error) |
			(value->flags & Papyrus_ExprFlags_Error);
		expr->type = type;
	}
	else
	{
		expr->flags |= Papyrus_ExprFlags_Error;
		expr->type = IntrinsicType(Error);
	}
}

static void
AnalyzeExpr_ReadLocal(ACtx* ctx, struct Papyrus_Expr* expr)
{
	expr->type = ctx->locals[expr->localIndex];
}

static void
AnalyzeExpr_WriteLocal(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* value = expr->assign.expr;
	AnalyzeExpr(ctx, value);

	struct Papyrus_Type* type = ctx->locals[expr->assign.localIndex];
	ImplicitCast(ctx, value, type);

	expr->type = type;
}

static void
AnalyzeExpr_Unary(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* subexpr = expr->oper.expr;
	AnalyzeExpr(ctx, subexpr);

	if (expr->kind == Papyrus_Expr_Neg)
	{
		struct Papyrus_Type* type = subexpr->type;
		if ((type->flags & Papyrus_TypeFlags_Error) == 0 &&
			(type->flags & Papyrus_TypeFlags_Arithmetic) == 0)
		{
			ReportError(ctx, expr->source,
				"invalid operand to unary operator");
		}
		expr->type = type;
	}
}

static void
AnalyzeExpr_Binary(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* lhs = expr->oper.lhs;
	struct Papyrus_Expr* rhs = expr->oper.rhs;
	AnalyzeExpr(ctx, lhs);
	AnalyzeExpr(ctx, rhs);
	struct Papyrus_Type* lhsType = lhs->type;
	struct Papyrus_Type* rhsType = rhs->type;

	struct Papyrus_Type* commonType = CommonType(lhsType, rhsType);
	struct Papyrus_Type* resultType;

	if (commonType->kind == Papyrus_Type_Error)
	{
		if (((lhsType->flags | rhsType->flags) & Papyrus_TypeFlags_Error) == 0)
		{
		error:
			ReportError(ctx, expr->source,
				"invalid operands to binary operator");
		}
		resultType = IntrinsicType(Error);
	}
	else switch (expr->kind)
	{
#ifndef __INTELLISENSE__
#define ARITHMETIC(x, ...) \
	case Papyrus_Expr_ ## x: \
		switch (commonType->kind) \
		{ \
			__VA_ARGS__ \
		} \
		resultType = IntrinsicType(Error); \
		goto error

#define TYPE(x) \
	case Papyrus_Type_ ## x: \
		resultType = IntrinsicType(x); \
		goto got_operator;

		ARITHMETIC(Add, TYPE(Int) TYPE(Float) TYPE(String));
		ARITHMETIC(Sub, TYPE(Int) TYPE(Float));
		ARITHMETIC(Div, TYPE(Int) TYPE(Float));
		ARITHMETIC(Mul, TYPE(Int) TYPE(Float));
		ARITHMETIC(Mod, TYPE(Int));
#undef ARITHMETIC
#undef TYPE

#define COMPARISON(x, ...) \
	case Papyrus_Expr_ ## x: \
		resultType = IntrinsicType(Bool); \
		switch (commonType->kind) \
		{ \
			__VA_ARGS__ \
		} \
		goto error

#define ANY \
	default: goto got_operator;

#define TYPE(x) \
	case Papyrus_Type_ ## x: \
		goto got_operator;

		COMPARISON(Eq, ANY);
		COMPARISON(Ne, ANY);
		COMPARISON(Lt, TYPE(Int) TYPE(Float) TYPE(String));
		COMPARISON(Gt, TYPE(Int) TYPE(Float) TYPE(String));
		COMPARISON(Le, TYPE(Int) TYPE(Float) TYPE(String));
		COMPARISON(Ge, TYPE(Int) TYPE(Float) TYPE(String));
#undef COMPARISON
#undef TYPE
#undef ANY
#endif

	got_operator:
		ImplicitCast(ctx, lhs, commonType);
		ImplicitCast(ctx, rhs, commonType);
	}
	expr->type = resultType;
}

static void
AnalyzeExpr_Cast(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* sub = expr->cast.expr;
	AnalyzeExpr(ctx, sub);

	struct Papyrus_Type* type = expr->cast.type;

	if (type->kind != Papyrus_Type_Error)
		if (!CanCastTo(sub->type, type, true))
		{
			ReportError(ctx, expr->source, "invalid cast");
		}

	expr->type = type;
}

static void
AnalyzeExpr_Call(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* object = expr->call.object;
	struct Papyrus_Symbol* symbol;

	if (object != NULL)
	{
		AnalyzeExpr(ctx, object);
		assert(false); // get symbol
	}
	else
	{
		symbol = expr->call.symbol;
	}

	FOREACHV_S(x, i, &expr->call.args)
	{
		AnalyzeExpr(ctx, x);
	}

	if (symbol->kind == Papyrus_Symbol_Function)
	{
		struct Papyrus_Function* function = (struct Papyrus_Function*)symbol;

		if (function->global)
		{
			if (object == NULL)
			{
				ReportError(ctx, expr->source,
					"call to global function with an object argument");
			}
		}
		else
		{
			if (object != NULL)
			{
				ReportError(ctx, expr->source,
					"call to method without an object argument");
			}
		}

		intptr_t argsCount = expr->call.args.size; {
			intptr_t paramCount = function->signature.paramTypes.size;
			if (argsCount > paramCount)
			{
				ReportError(ctx, expr->call.args.data[paramCount]->source,
					"too many arguments to function");
			}
			else if (argsCount < paramCount)
			{
				ReportError(ctx, expr->source,
					"too few arguments to function");
			}
			argsCount = MIN(paramCount, argsCount);
		}

		struct Papyrus_Type** params = function->signature.paramTypes.data;
		FOREACHV(x, i, expr->call.args.data, argsCount)
		{
			struct Papyrus_Type* argType = x->type;
			if ((argType->flags & Papyrus_TypeFlags_Error) == 0)
			{
				if (!CanCastTo(argType, params[i], false))
				{
					ReportError(ctx, x->source,
						"invalid argument to function");
				}
			}
		}

		expr->type = function->signature.returnType;
	}
	else
	{
		ReportError(ctx, expr->source, "expression is not callable");
	}
}

static void
AnalyzeExpr_ReadArray(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* array = expr->assign.array;
	AnalyzeExpr(ctx, array);

	struct Papyrus_Type* arrayType = array->type;
	bool isArray = arrayType->type == Papyrus_Type_Array;
	if (!isArray)
	{
		ReportError(ctx, expr->source, "object is not an array");
	}

	struct Papyrus_Expr* subscript = expr->assign.subscript;
	AnalyzeExpr(ctx, subscript);
	ImplicitCast(ctx, subscript, IntrinsicType(Int));

	if (isArray)
	{
		expr->type = arrayType->element;
	}
	else
	{
		expr->type = IntrinsicType(Error);
	}
}

static void
AnalyzeExpr_WriteArray(ACtx* ctx, struct Papyrus_Expr* expr)
{
	struct Papyrus_Expr* array = expr->assign.array;
	AnalyzeExpr(ctx, array);

	struct Papyrus_Type* arrayType = array->type;
	bool isArray = arrayType->type == Papyrus_Type_Array;
	if (!isArray)
	{
		ReportError(ctx, expr->source, "object is not an array");
	}

	struct Papyrus_Expr* subscript = expr->assign.subscript;
	AnalyzeExpr(ctx, subscript);
	ImplicitCast(ctx, subscript, IntrinsicType(Int));

	struct Papyrus_Expr* value = expr->assign.expr;
	AnalyzeExpr(ctx, value);

	if (isArray)
	{
		struct Papyrus_Type* type = arrayType->element;
		ImplicitCast(ctx, value, type);
		expr->type = type;
	}
	else
	{
		expr->type = IntrinsicType(Error);
	}
}

static void
AnalyzeExpr(ACtx* ctx, struct Papyrus_Expr* expr)
{
reset:
	switch (expr->kind)
	{
	case Papyrus_Expr_Symbol:
		AnalyzeExpr_Symbol(ctx, expr);
		break;

	case Papyrus_Expr_Assign:
		AnalyzeExpr_Assign(ctx, expr);
		break;

	case Papyrus_Expr_ReadLocal:
		AnalyzeExpr_ReadLocal(ctx, expr);
		break;

	case Papyrus_Expr_WriteLocal:
		AnalyzeExpr_WriteLocal(ctx, expr);
		break;

	case Papyrus_Expr_LitNone:
	case Papyrus_Expr_LitInt:
	case Papyrus_Expr_LitBool:
	case Papyrus_Expr_LitFloat:
	case Papyrus_Expr_LitString:
		break;

	case Papyrus_Expr_Neg:
	case Papyrus_Expr_Not:
		AnalyzeExpr_Unary(ctx, expr);
		break;

	case Papyrus_Expr_Add:
	case Papyrus_Expr_Sub:
	case Papyrus_Expr_Mul:
	case Papyrus_Expr_Div:
	case Papyrus_Expr_Mod:
		AnalyzeExpr_Binary(ctx, expr);
		break;

	case Papyrus_Expr_Eq:
	case Papyrus_Expr_Ne:
	case Papyrus_Expr_Lt:
	case Papyrus_Expr_Gt:
	case Papyrus_Expr_Le:
	case Papyrus_Expr_Ge:
		AnalyzeExpr_Binary(ctx, expr);
		break;

	case Papyrus_Expr_ReadArray:
		AnalyzeExpr_ReadArray(ctx, expr);
		break;

	case Papyrus_Expr_WriteArray:
		AnalyzeExpr_WriteArray(ctx, expr);
		break;

	case Papyrus_Expr_Cast:
		// Previous implicit casts are stripped
		if (expr->flags & Papyrus_ExprFlags_Implicit)
		{
			struct Papyrus_Expr* sub = expr->cast.expr;
			*expr = *sub;

			*(struct Papyrus_Expr**)sub = ctx->freelist.expr;
			ctx->freelist.expr = sub;

			goto reset;
		}
		else AnalyzeExpr_Cast(ctx, expr);
		break;

	case Papyrus_Expr_Call:
		AnalyzeExpr_Call(ctx, expr);
		break;

	default:
		ICE(ctx);
	}
}


static void
AnalyzeScope(ACtx* ctx, struct Papyrus_Scope scope);

static void
AnalyzeStmt_Expr(ACtx* ctx, struct Papyrus_Stmt* stmt)
{
	AnalyzeExpr(ctx, stmt->expr);
}

static void
AnalyzeStmt_Return(ACtx* ctx, struct Papyrus_Stmt* stmt)
{
	AnalyzeExpr(ctx, stmt->expr);
}

static void
AnalyzeStmt_Branch(ACtx* ctx, struct Papyrus_Stmt* stmt)
{
	FOREACH_S(x, i, &stmt->branch)
	{
		struct Papyrus_Expr* expr = x->expr;
		if (expr != NULL)
		{
			AnalyzeExpr(ctx, x->expr);
		}
		AnalyzeScope(ctx, x->scope);
	}
}

static void
AnalyzeStmt_Loop(ACtx* ctx, struct Papyrus_Stmt* stmt)
{
	AnalyzeExpr(ctx, stmt->loop.expr);
	AnalyzeScope(ctx, stmt->loop.scope);
}

static void
AnalyzeStmt(ACtx* ctx, struct Papyrus_Stmt* stmt)
{
	switch (stmt->kind)
	{
#define STMT(x) \
	case Papyrus_Stmt_ ## x: \
		AnalyzeStmt_ ## x(ctx, stmt); \
		break

		STMT(Expr);
		STMT(Return);
		STMT(Branch);
		STMT(Loop);
#undef STMT
	}
}

static void
AnalyzeScope(ACtx* ctx, struct Papyrus_Scope scope)
{
	FOREACHV_S(x, i, &scope)
	{
		AnalyzeStmt(ctx, x);
	}
}


static void
AnalyzeVariable(ACtx* ctx, struct Papyrus_Variable* variable)
{
	struct Papyrus_Expr* expr = variable->expr;
	AnalyzeExpr(ctx, expr);

	if ((expr->flags & Papyrus_ExprFlags_Const) == 0)
	{
		ReportError(ctx, 0, "field initializer must be constant");
	}

	ImplicitCast(ctx, expr, variable->type);
}

static void
AnalyzeFunction(ACtx* ctx, struct Papyrus_Function* function)
{
	ctx->returnType = function->signature.returnType;
	ctx->locals = function->locals.data;
	AnalyzeScope(ctx, function->scope);
}

void
Papyrus_Script_Analyze(
	struct Papyrus_Script* script, struct Papyrus_Diagnostics* diag)
{
	assert(script->symbol.eflags & Papyrus_ScriptFlags_Linked);

	ACtx ctx;
	ctx.common.diag = diag;
	ctx.common.script = (struct Script*)script;
	ctx.freelist.expr = ctx.common.script->freelist.expr;

	Papyrus_TRY(&ctx.common.except)
	{
		FOREACHV_S(x, x_i, &script->variables)
		{
			AnalyzeVariable(&ctx, x);
		}

		FOREACHV_S(x, x_i, &script->functions)
		{
			AnalyzeFunction(&ctx, x);
		}
	}

	ctx.common.script->freelist.expr = ctx.freelist.expr;
}
