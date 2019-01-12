#include "Papyrus/DumpAST.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

struct Printer
{
	int32_t virtualIndent;
	int32_t indent;
	bool printedIndent;
};

typedef struct Printer Ctx;

static const char IndentString[] =
	"                                                                "
	"                                                                "
	"                                                                "
	"                                                                ";
enum { MaxIndent = sizeof(IndentString) - 1 };

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
	fwrite(IndentString, ctx->indent, 1, stdout);
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
PrintFullName(Ctx* ctx, const struct Papyrus_FullName* name)
{
	Print_String(ctx, name->parts.data[0]);
	for (intptr_t i = 1, c = name->parts.size; i < c; ++i)
	{
		struct Papyrus_String string = name->parts.data[i];
		Print(ctx, ":%.*s", (int)string.size, string.data);
	}
}

static void
PrintType(Ctx* ctx, const struct Papyrus_Syntax_Type* syntax)
{
	ALIAS(Type, type);

	PrintFullName(ctx, &type->name);
	
	if (type->flags & Papyrus_TypeFlags_Array)
		Print(ctx, "[]");
}

static void
PrintSyntaxes(Ctx* ctx, const struct Papyrus_SyntaxArray* array)
{
	const struct Papyrus_Syntax* const* data = array->data;
	for (intptr_t i = 0, c = array->size; i < c; ++i)
		PrintSyntax(ctx, data[i]);
}


static void
PrintSyntax_Function(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(Function, func);

	Print(ctx, "name: ");
	PrintLine_String(ctx, func->name);

	const struct Papyrus_Param* params = func->params.data;
	for (intptr_t i = 0, c = func->params.size; i < c; ++i)
	{
		const struct Papyrus_Param* param = params + i;

		Print(ctx, "param: ");
		PrintLine_String(ctx, param->name);

		Enter(ctx);

		Print(ctx, "type: ");
		PrintType(ctx, param->type);
		PrintLine(ctx, "");

		Exit(ctx);
	}

	const struct Papyrus_Syntax* const* stmts = func->scope.data;
	for (intptr_t i = 0, c = func->scope.size; i < c; ++i)
		PrintSyntax(ctx, stmts[i]);
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

	if (prop->flags & Papyrus_DeclFlags_Auto)
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
		const struct Papyrus_Syntax_Function* get = prop->get;
		if (get != NULL) PrintSyntax(ctx, &get->syntax);

		const struct Papyrus_Syntax_Function* set = prop->set;
		if (set != NULL) PrintSyntax(ctx, &set->syntax);
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
PrintSyntax_Struct(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(Struct, struct_);

	Print(ctx, "name: ");
	PrintLine_String(ctx, struct_->name);

	PrintSyntaxes(ctx, &struct_->vars);
}

static void
PrintSyntax_Import(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	Print(ctx, "name: ");
	PrintFullName(ctx, &AS(Import)->name);
	PrintLine(ctx, "");
}

static void
PrintSyntax_Script(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(Script, script);

	Print(ctx, "name: ");
	PrintLine_String(ctx, script->name);

	if (script->base.parts.size > 0)
	{
		Print(ctx, "base: ");
		PrintFullName(ctx, &script->base);
		PrintLine(ctx, "");
	}

	PrintSyntaxes(ctx, &script->defs);
}


static void
PrintSyntax_ExprStmt(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	PrintSyntax(ctx, AS(ExprStmt)->expr);
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

	PrintLine(ctx, "cond:");
	Enter(ctx);
	PrintSyntax(ctx, stmt->cond);
	Exit(ctx);

	PrintLine(ctx, "body:");
	Enter(ctx);
	PrintSyntaxes(ctx, &stmt->scope);
	Exit(ctx);

	const struct Papyrus_Syntax_ElseIfClause* const* elifs = stmt->elifs.data;
	for (intptr_t i = 0, c = stmt->elifs.size; i < c; ++i)
	{
		const struct Papyrus_Syntax_ElseIfClause* elif = elifs[i];

		PrintLine(ctx, "elif:");
		Enter(ctx);

		PrintLine(ctx, "cond:");
		Enter(ctx);
		PrintSyntax(ctx, elif->cond);
		Exit(ctx);

		PrintSyntaxes(ctx, &elif->scope);

		Exit(ctx);
	}

	const struct Papyrus_Syntax_ElseClause* else_ = stmt->else_;
	if (else_ != NULL)
	{
		PrintLine(ctx, "else:");
		Enter(ctx);
		PrintSyntaxes(ctx, &else_->scope);
		Exit(ctx);
	}
}

static void
PrintSyntax_WhileStmt(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(IfStmt, stmt);

	PrintLine(ctx, "cond:");
	Enter(ctx);
	PrintSyntax(ctx, stmt->cond);
	Exit(ctx);

	PrintSyntaxes(ctx, &stmt->scope);
}


static void
PrintSyntax_NameExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	Print(ctx, "name: ");
	PrintFullName(ctx, &AS(NameExpr)->name);
	PrintLine(ctx, "");
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
PrintSyntax_StringExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	PrintLine_String(ctx, AS(StringExpr)->string);
}

static void
PrintSyntax_NewExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(NewExpr, new);

	Print(ctx, "type: ");
	PrintFullName(ctx, &new->name);
	PrintLine(ctx, "");

	PrintSyntax(ctx, new->extent);
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
PrintSyntax_InvokeExpr(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	ALIAS(InvokeExpr, expr);

	PrintSyntax(ctx, expr->expr);

	PrintLine(ctx, "args:");
	Enter(ctx);

	const struct Papyrus_Syntax* const* args = expr->args.data;
	for (intptr_t i = 0, c = expr->args.size; i < c; ++i)
		PrintSyntax(ctx, args[i]);

	Exit(ctx);
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
PrintSyntax_Empty(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
}


static void
PrintSyntax(Ctx* ctx, const struct Papyrus_Syntax* syntax)
{
	switch (syntax->kind)
	{
		void(*print)(Ctx*, const struct Papyrus_Syntax*);
		struct Papyrus_String kindString;

#define KIND2(kind, type) \
	case Papyrus_Syntax_ ## kind: \
		kindString.data = #kind; \
		kindString.size = sizeof(#kind) - 1; \
		print = &PrintSyntax_ ## type; \
		goto print_syntax;

#define KIND(kind) KIND2(kind, kind)

		KIND(Function);
		KIND(Property);
		KIND(Variable);
		KIND(Struct);
		KIND(Import);
		KIND(Script);

		KIND(ExprStmt);
		KIND(ReturnStmt);
		KIND(IfStmt);
		KIND(WhileStmt);

		KIND2(NoneExpr, Empty);
		KIND(NameExpr);
		KIND2(NegExpr, UnaryExpr);
		KIND2(NotExpr, UnaryExpr);
		KIND2(DisExpr, UnaryExpr);
		KIND2(ConExpr, UnaryExpr);
		KIND2(AddExpr, BinaryExpr);
		KIND2(SubExpr, BinaryExpr);
		KIND2(MulExpr, BinaryExpr);
		KIND2(DivExpr, BinaryExpr);
		KIND2(ModExpr, BinaryExpr);
		KIND2(EqExpr, BinaryExpr);
		KIND2(NeExpr, BinaryExpr);
		KIND2(LtExpr, BinaryExpr);
		KIND2(GtExpr, BinaryExpr);
		KIND2(LeExpr, BinaryExpr);
		KIND2(GeExpr, BinaryExpr);
		KIND2(AssignExpr, BinaryExpr);
		KIND2(AddAssignExpr, BinaryExpr);
		KIND2(SubAssignExpr, BinaryExpr);
		KIND2(MulAssignExpr, BinaryExpr);
		KIND2(DivAssignExpr, BinaryExpr);
		KIND2(ModAssignExpr, BinaryExpr);
		KIND2(IntExpr, StringExpr);
		KIND2(FloatExpr, StringExpr);
		KIND2(StringExpr, StringExpr);
		KIND(NewExpr);
		KIND2(AsExpr, CastExpr);
		KIND2(IsExpr, CastExpr);
		KIND2(CallExpr, InvokeExpr);
		KIND2(IndexExpr, InvokeExpr);
		KIND(AccessExpr);

	print_syntax:
		PrintLine_String(ctx, kindString);
		Enter(ctx);
		print(ctx, syntax);
		Exit(ctx);
		break;

#undef KIND

	default:
		PrintLine(ctx, "?");
		break;
	}
}


void
Papyrus_DumpAST(const struct Papyrus_Syntax_Script* script)
{
	struct Printer printer;
	printer.virtualIndent = 0;
	printer.indent = 0;
	printer.printedIndent = false;

	PrintSyntax(&printer, &script->syntax);
}
