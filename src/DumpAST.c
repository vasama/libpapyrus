#include "Papyrus/DumpAST.h"

#include "Papyrus/Parser.h"
#include "Papyrus/Syntax.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef struct
{
	int32_t virtualIndent;
	int32_t indent;
	bool printedIndent;
} Ctx;

static const char IndentString[] =
	"                                                                "
	"                                                                "
	"                                                                "
	"                                                                ";
enum { MaxIndent = (sizeof(IndentString) - 1) / 2 };

static void
Enter(Ctx* ctx)
{
	int32_t indent = ++ctx->virtualIndent;
	ctx->indent = indent <= MaxIndent ? indent : MaxIndent;
}

static void
Exit(Ctx* ctx)
{
	int32_t indent = --ctx->virtualIndent;
	ctx->indent = indent <= MaxIndent ? indent : MaxIndent;
}

static void
PrintIndent(Ctx* ctx)
{
	fwrite(IndentString, ctx->indent * 2 , 1, stdout);
}

static void
Print(Ctx* ctx, const char* fmt, ...)
{
	if (ctx->printedIndent == false)
	{
		PrintIndent(ctx);
		ctx->printedIndent = true;
	}

	va_list vlist;
	va_start(vlist, fmt);
	vprintf(fmt, vlist);
	va_end(vlist);
}

static void
Print_String(Ctx* ctx, struct Papyrus_String string)
{
	if (ctx->printedIndent == false)
	{
		PrintIndent(ctx);
		ctx->printedIndent = true;
	}

	fwrite(string.data, string.size, 1, stdout);
}

static void
PrintLine(Ctx* ctx, const char* fmt, ...)
{
	if (ctx->printedIndent == false)
		PrintIndent(ctx);
	ctx->printedIndent = false;

	va_list vlist;
	va_start(vlist, fmt);
	vprintf(fmt, vlist);
	va_end(vlist);

	putchar('\n');
}

static void
PrintLine_String(Ctx* ctx, struct Papyrus_String string)
{
	if (ctx->printedIndent == false)
		PrintIndent(ctx);
	ctx->printedIndent = false;

	fwrite(string.data, string.size, 1, stdout);

	putchar('\n');
}


#define AS(type) \
	((const struct Papyrus_Syntax_ ## type*)syntax)

#define ALIAS(type, name) \
	const struct Papyrus_Syntax_ ## type* name = \
		(const struct Papyrus_Syntax_ ## type*)syntax

static void
PrintSyntax(Ctx* ctx, const struct Papyrus_Syntax* syntax);

static void
PrintSymbol(Ctx* ctx, const struct Papyrus_Syntax_Symbol* symbol)
{
	Print_String(ctx, symbol->data[0]);
	for (intptr_t i = 1, c = symbol->size; i < c; ++i)
	{
		struct Papyrus_String string = symbol->data[i];
		Print(ctx, ":%.*s", (int)string.size, string.data);
	}
}

static void
PrintType(Ctx* ctx, const struct Papyrus_Syntax_Type* type)
{
	PrintSymbol(ctx, type->symbol);

	if (type->syntax.eflags & Papyrus_Syntax_TypeFlags_Array)
		Print(ctx, "[]");
}

static void
PrintScope(Ctx* ctx, const struct Papyrus_Syntax_Scope* scope)
{
	struct Papyrus_Syntax* const* data = scope->data;
	for (intptr_t i = 0, c = scope->size; i < c; ++i)
		PrintSyntax(ctx, data[i]);
}


static void
PrintSyntax_NameExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	Print(ctx, "name: ");
	PrintSymbol(ctx, AS(NameExpr)->symbol);
	PrintLine(ctx, "");
}

static void
PrintSyntax_ConstExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	PrintLine_String(ctx, AS(ConstExpr)->string);
}

static void
PrintSyntax_NewExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(NewExpr, new);

	Print(ctx, "type: ");
	PrintSymbol(ctx, new->name);
	PrintLine(ctx, "");

	PrintSyntax(ctx, new->extent);
}

static void
PrintSyntax_UnaryExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	PrintSyntax(ctx, AS(UnaryExpr)->expr);
}

static void
PrintSyntax_BinaryExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	PrintSyntax(ctx, AS(BinaryExpr)->left);
	PrintSyntax(ctx, AS(BinaryExpr)->right);
}

static void
PrintSyntax_AccessExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(AccessExpr, expr);

	Print(ctx, "name: ");
	PrintLine_String(ctx, expr->name);

	PrintSyntax(ctx, expr->expr);
}

static void
PrintSyntax_CastExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(CastExpr, cast);

	Print(ctx, "type: ");
	PrintType(ctx, cast->type);
	PrintLine(ctx, "");

	PrintSyntax(ctx, cast->expr);
}

static void
PrintSyntax_CallExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(CallExpr, expr);

	PrintSyntax(ctx, expr->expr);

	intptr_t argsCount = expr->args.size;
	if (argsCount > 0)
	{
		PrintLine(ctx, "args:");
		Enter(ctx);

		struct Papyrus_Syntax* const* args = expr->args.data;
		for (intptr_t i = 0; i < argsCount; ++i)
			PrintSyntax(ctx, args[i]);

		Exit(ctx);
	}
}


static void
PrintSyntax_ExprStmt(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	PrintSyntax(ctx, AS(ExprStmt)->expr);
}

static void
PrintSyntax_AssignStmt(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	//TODO: operator

	Print(ctx, "dst: ");
	Enter(ctx);
	PrintSyntax(ctx, AS(AssignStmt)->object);
	Exit(ctx);

	Print(ctx, "src: ");
	Enter(ctx);
	PrintSyntax(ctx, AS(AssignStmt)->expr);
	Exit(ctx);
}

static void
PrintSyntax_ReturnStmt(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	PrintSyntax(ctx, AS(ReturnStmt)->expr);
}

static void
PrintSyntax_IfStmt(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(IfStmt, stmt);

	const char* header = "if:";

	struct Papyrus_Syntax_IfClause* const* clauses = stmt->clauses.data;
	for (intptr_t i = 0, c = stmt->clauses.size; i < c; ++i)
	{
		const struct Papyrus_Syntax_IfClause* clause = clauses[i];

		PrintLine(ctx, header);
		Enter(ctx);

		PrintLine(ctx, "cond:");
		Enter(ctx);
		PrintSyntax(ctx, clause->cond);
		Exit(ctx);

		PrintScope(ctx, clause->scope);

		Exit(ctx);

		header = "elif:";
	}

	const struct Papyrus_Syntax_Scope* elseScope = stmt->elseScope;
	if (elseScope != NULL)
	{
		PrintLine(ctx, "else:");
		Enter(ctx);
		PrintScope(ctx, stmt->elseScope);
		Exit(ctx);
	}
}

static void
PrintSyntax_WhileStmt(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(WhileStmt, stmt);

	PrintLine(ctx, "cond:");
	Enter(ctx);
	PrintSyntax(ctx, stmt->cond);
	Exit(ctx);

	PrintScope(ctx, stmt->scope);
}


static void
PrintSyntax_Function(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(Function, func);

	Print(ctx, "name: ");
	PrintLine_String(ctx, func->name);

	struct Papyrus_Syntax_Param* const* params = func->params->data;
	for (intptr_t i = 0, c = func->params->size; i < c; ++i)
	{
		const struct Papyrus_Syntax_Param* param = params[i];

		Print(ctx, "param: ");
		PrintLine_String(ctx, param->name);

		Enter(ctx);

		Print(ctx, "type: ");
		PrintType(ctx, param->type);
		PrintLine(ctx, "");

		Exit(ctx);
	}

	PrintScope(ctx, func->scope);
}

static void
PrintSyntax_Import(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	Print(ctx, "name: ");
	PrintSymbol(ctx, AS(Import)->symbol);
	PrintLine(ctx, "");
}

static void
PrintSyntax_Property(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(Property, prop);

	Print(ctx, "name: ");
	PrintLine_String(ctx, prop->name);

	Print(ctx, "type: ");
	PrintType(ctx, prop->type);
	PrintLine(ctx, "");

	uint32_t autoflags =
		Papyrus_Syntax_DeclFlags_Auto |
		Papyrus_Syntax_DeclFlags_AutoReadOnly;

	if (prop->syntax.eflags & autoflags)
	{
		const struct Papyrus_Syntax* expr = prop->expr;
		if (expr != NULL)
		{
			PrintLine(ctx, "expr:");
			Enter(ctx);
			PrintSyntax(ctx, expr);
			Exit(ctx);
		}
	}
	else
	{
		PrintScope(ctx, prop->scope);
	}
}

static void
PrintSyntax_ScriptHeader(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(ScriptHeader, header);

	Print(ctx, "name: ");
	PrintLine_String(ctx, header->name);

	const struct Papyrus_Syntax_Symbol* base = header->base;
	if (base != NULL)
	{
		Print(ctx, "base: ");
		PrintSymbol(ctx, base);
		PrintLine(ctx, "");
	}
}

static void
PrintSyntax_Variable(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(Variable, var);

	Print(ctx, "name: ");
	PrintLine_String(ctx, var->name);

	Print(ctx, "type: ");
	PrintType(ctx, var->type);
	PrintLine(ctx, "");

	const struct Papyrus_Syntax* expr = var->expr;
	if (expr != NULL)
	{
		PrintLine(ctx, "expr:");
		Enter(ctx);
		PrintSyntax(ctx, expr);
		Exit(ctx);
	}
}


static void
PrintSyntax(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	void(*print)(Ctx*, const struct Papyrus_Syntax*);
	struct Papyrus_String kindString;

	switch (syntax->kind)
	{
#define KIND(kind, name) \
	case Papyrus_Syntax_ ## kind: \
		kindString.data = name; \
		kindString.size = sizeof(name) - 1; \
		print = &PrintSyntax_ ## kind; \
		goto print_syntax;

		KIND(NameExpr, "name-expr");
		KIND(ConstExpr, "*expr");
		KIND(NewExpr, "new-expr");

		KIND(UnaryExpr, "*expr");
		KIND(BinaryExpr, "*expr");
		KIND(AccessExpr, "access-expr");
		KIND(CastExpr, "*expr");
		KIND(CallExpr, "call-expr");

		KIND(ExprStmt, "expr-stmt");
		KIND(AssignStmt, "*assign-stmt");
		KIND(ReturnStmt, "return-stmt");
		KIND(IfStmt, "if-stmt");
		KIND(WhileStmt, "while-stmt");

		KIND(Function, "function");
		KIND(Import, "import");
		KIND(Property, "property");
		KIND(ScriptHeader, "script-header");
		KIND(Variable, "variable");

#undef KIND

	print_syntax:
		if (kindString.data[0] == '*')
		{
			++kindString.data;
			--kindString.size;

			switch (syntax->ekind)
			{
				struct Papyrus_String ekindString;

#define EKIND(ekind, name) \
	case Papyrus_Syntax_ ## ekind: \
		ekindString.data = name; \
		ekindString.size = sizeof(name) - 1; \
		goto print_ekind;

				EKIND(ConstExpr_None, "none-");
				EKIND(ConstExpr_Int, "int-");
				EKIND(ConstExpr_True, "bool-");
				EKIND(ConstExpr_False, "bool-");
				EKIND(ConstExpr_Float, "float-");
				EKIND(ConstExpr_String, "string-");

				EKIND(UnaryExpr_Neg, "neg-");
				EKIND(UnaryExpr_Not, "not-");

				EKIND(BinaryExpr_Add, "add-");
				EKIND(BinaryExpr_Sub, "sub-");
				EKIND(BinaryExpr_Mul, "mul-");
				EKIND(BinaryExpr_Div, "div-");
				EKIND(BinaryExpr_Mod, "mod-");
				EKIND(BinaryExpr_Eq, "eq-");
				EKIND(BinaryExpr_Ne, "ne-");
				EKIND(BinaryExpr_Lt, "lt-");
				EKIND(BinaryExpr_Gt, "gt-");
				EKIND(BinaryExpr_Le, "le-");
				EKIND(BinaryExpr_Ge, "ge-");
				EKIND(BinaryExpr_Con, "and-");
				EKIND(BinaryExpr_Dis, "or-");
				EKIND(BinaryExpr_Index, "index-");

				EKIND(CastExpr_As, "as-");
				EKIND(CastExpr_Is, "is-");

#undef EKIND

			print_ekind:
				Print_String(ctx, ekindString);
			}
		}
		PrintLine_String(ctx, kindString);
		Enter(ctx);
		print(ctx, syntax);
		Exit(ctx);
		break;

	default:
		PrintLine(ctx, "?");
		return;
	}
}


void
Papyrus_DumpAST(struct Papyrus_SyntaxTree* tree)
{
	Ctx ctx;
	ctx.virtualIndent = 0;
	ctx.indent = 0;
	ctx.printedIndent = false;

	PrintLine(&ctx, "script");
	Enter(&ctx);
	PrintScope(&ctx, tree->script->scope);
	Exit(&ctx);
}
