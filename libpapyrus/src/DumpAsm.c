#include "DumpAsm.h"

#include "Papyrus/Emit.h"
#include "Papyrus/String.h"

#include "Asm.h"

#include "Common/Array.h"
#include "Common/Macros.h"

#include <stdarg.h>
#include <stdio.h>

Array_DEFINE(StringArray, struct Papyrus_String);

#define ENTER(ctx) ((void)((ctx)->indent += 2))
#define EXIT(ctx) ((void)((ctx)->indent -= 2))

typedef struct {
	struct Asm* asm;
	uint32_t indent;

	struct {
		char* cur;
		char* end;
	} buffer;

	struct Papyrus_EmitBuffer* emitbuf;
} Ctx;

static void
Flush(Ctx* ctx)
{
	struct Papyrus_EmitBuffer* emitbuf = ctx->emitbuf;

	uintptr_t bufsize = emitbuf->size;
	emitbuf->flush(emitbuf, bufsize -
		(uintptr_t)(ctx->buffer.end - ctx->buffer.cur));

	char* cur = (char*)emitbuf->buffer;
	ctx->buffer.cur = cur;
	ctx->buffer.end = ctx->buffer.cur + bufsize;
}

static void
Write(Ctx* ctx, const char* fmt, ...)
{
reset:;
	char* cur = ctx->buffer.cur;
	char* end = ctx->buffer.end;
	uintptr_t space = end - cur;

	va_list vlist;
	va_start(vlist, fmt);
	uintptr_t size = vsnprintf(cur, space, fmt, vlist);
	va_end(vlist);

	if (size > space)
	{
		Flush(ctx);
		assert(size < (uintptr_t)(ctx->buffer.end - ctx->buffer.cur));
		goto reset;
	}

	ctx->buffer.cur = cur + size;
}

static void
WriteIndent(Ctx* ctx)
{
	char* cur = ctx->buffer.cur;
	char* end = ctx->buffer.end;

	uintptr_t indent = ctx->indent;
	char* new = cur + indent;
	if (new > end)
	{
		Flush(ctx);
		cur = ctx->buffer.cur;
		end = ctx->buffer.end;
	}
	memset(cur, ' ', indent);

	ctx->buffer.cur = new;
}

#define WRITE(ctx, fmt, ...) \
	(WriteIndent(ctx), Write(ctx, fmt, ##__VA_ARGS__))

#define STR "%.*s"
#define STRFMT(x) (int)(x).size, (x).data

typedef const char* Indent;

static const char* const InstStrings[] = {
#define INST(x, n) \
	[Asm_ ## x] = n

	INST(Nop, "nop"),
	INST(IAdd, "iadd"),
	INST(FAdd, "fadd"),
	INST(ISub, "isub"),
	INST(FSub, "fsub"),
	INST(IMul, "imul"),
	INST(FMul, "fmul"),
	INST(IDiv, "idiv"),
	INST(FDiv, "fdiv"),
	INST(IMod, "imod"),
	INST(Not, "not"),
	INST(INeg, "ineg"),
	INST(FNeg, "fneg"),
	INST(Assign, "assign"),
	INST(Cast, "cast"),
	INST(CmpEq, "compareeq"),
	INST(CmpLt, "comparelt"),
	INST(CmpLe, "comparele"),
	INST(CmpGt, "comparegt"),
	INST(CmpGe, "comparege"),
	INST(Jmp, "jump"),
	INST(JmpT, "jumpt"),
	INST(JmpF, "jumpf"),
	INST(CallMethod, "callmethod"),
	INST(CallParent, "callparent"),
	INST(CallStatic, "callstatic"),
	INST(Return, "return"),
	INST(StrCat, "strcat"),
	INST(PropGet, "propget"),
	INST(PropSet, "propset"),
	INST(ArrNew, "arraycreate"),
	INST(ArrLen, "arraylength"),
	INST(ArrGet, "arraygetelement"),
	INST(ArrSet, "arraysetelement"),
	INST(ArrFind, "arrayfindelement"),
	INST(ArrRFind, "arrayrfindelement"),
#undef INST
};

static struct Papyrus_String
GetString(Ctx* ctx, uint16_t index)
{
	return ctx->asm->strings.data[index];
}

static void
PrintArg(Ctx* ctx, struct Asm_Arg* arg)
{
	if (arg->hidden)
		return;

	switch (arg->type)
	{
		struct Papyrus_String string;

	case Asm_Arg_Index:
		if (arg->label)
		{
			Write(ctx, " label_%u", arg->index);
		}
		else
		{
			string = GetString(ctx, arg->index);
			Write(ctx, " "STR, STRFMT(string));
		}
		break;

	case Asm_Arg_String:
		string = GetString(ctx, arg->index);
		Write(ctx, " \""STR"\"", STRFMT(string));
		break;

	case Asm_Arg_Int:
		Write(ctx, " %d", arg->int_);
		break;

	case Asm_Arg_Float:
		Write(ctx, " %f", arg->float_);
		break;

	case Asm_Arg_Bool:
		Write(ctx, arg->bool_ ? " true" : " false");
		break;
	}
}

static void
PrintFunction(Ctx* ctx, struct Asm_Function* function)
{
	struct Papyrus_String name, type;

	name = GetString(ctx, function->name);
	WRITE(ctx, ".function "STR"\n", STRFMT(name));
	ENTER(ctx);

	WRITE(ctx, ".userFlags 0\n"); //TODO
	WRITE(ctx, ".docString \"\"\n"); //TODO

	type = GetString(ctx, function->returnType);
	WRITE(ctx, ".return "STR"\n", STRFMT(type)); //TODO

	WRITE(ctx, ".paramTable\n");
	ENTER(ctx);
	FOREACH_S(x, i, &function->params)
	{
		name = GetString(ctx, x->name);
		type = GetString(ctx, x->type);
		WRITE(ctx, ".param "STR" "STR"\n", STRFMT(name), STRFMT(type));
	}
	EXIT(ctx);
	WRITE(ctx, ".endParamTable\n");

	WRITE(ctx, ".localTable\n");
	ENTER(ctx);
	FOREACH_S(x, i, &function->locals)
	{
		name = GetString(ctx, x->name);
		type = GetString(ctx, x->type);
		WRITE(ctx, ".local "STR" "STR"\n", STRFMT(name), STRFMT(type));
	}
	EXIT(ctx);
	WRITE(ctx, ".endLocalTable\n");

	WRITE(ctx, ".code\n");
	ENTER(ctx);
	FOREACHV_S(x, i, &function->code)
	{
		if (x->jumpTarget)
			WRITE(ctx, "label_%u:\n", (uint32_t)i);

		WRITE(ctx, "%s", InstStrings[x->opcode]);
		FOREACH(y, j, x->args, x->argsCount)
		{
			PrintArg(ctx, y);
		}
		WRITE(ctx, "\n");
	}
	EXIT(ctx);
	WRITE(ctx, ".endCode\n");

	EXIT(ctx);
	WRITE(ctx, ".endFunction\n");
}

static void
PrintObject(Ctx* ctx, struct Asm_Object* object)
{
	struct Papyrus_String name, type;

	name = GetString(ctx, object->name);
	WRITE(ctx, ".object "STR"\n", STRFMT(name));
	ENTER(ctx);

	WRITE(ctx, ".userFlags 0\n"); //TODO
	WRITE(ctx, ".docString \"\"\n"); //TODO
	WRITE(ctx, ".autoState\n"); //TODO

	WRITE(ctx, ".variableTable\n");
	ENTER(ctx);
	FOREACHV_S(x, i, &object->variables)
	{
		name = GetString(ctx, x->name);
		type = GetString(ctx, x->type);
		WRITE(ctx, ".variable "STR" "STR"\n", STRFMT(name), STRFMT(type));

		WRITE(ctx, ".userFlags 0\n"); //TODO
		WRITE(ctx, ".initialValue None\n"); //TODO

		WRITE(ctx, ".endVariable\n");
	}
	EXIT(ctx);
	WRITE(ctx, ".endVariableTable\n");

	WRITE(ctx, ".propertyTable\n");
	ENTER(ctx);
	FOREACHV_S(x, i, &object->properties)
	{
		bool auto_ = x->auto_;

		name = GetString(ctx, x->name);
		type = GetString(ctx, x->type);
		WRITE(ctx, ".property "STR" "STR, STRFMT(name), STRFMT(type));
		puts(auto_ ? " auto" : "");

		WRITE(ctx, ".userFlags 0\n"); //TODO
		WRITE(ctx, ".docString \"\"\n"); //TODO

		if (auto_)
		{
			name = GetString(ctx, x->varname);
			WRITE(ctx, ".autoVar "STR"\n", STRFMT(name));
		}
		else
		{
			struct Asm_Function* get = x->get;
			if (get != NULL) PrintFunction(ctx, get);

			struct Asm_Function* set = x->set;
			if (set != NULL) PrintFunction(ctx, set);
		}
	}
	EXIT(ctx);
	WRITE(ctx, ".endPropertyTable\n");

	WRITE(ctx, ".stateTable\n");
	ENTER(ctx);
	FOREACHV_S(x, i, &object->states)
	{
		WRITE(ctx, ".state\n");
		ENTER(ctx);
		FOREACHV_S(y, j, &x->functions)
		{
			PrintFunction(ctx, y);
		}
		EXIT(ctx);
		WRITE(ctx, ".endState\n");
	}
	EXIT(ctx);
	WRITE(ctx, ".endStateTable\n");

	EXIT(ctx);
	WRITE(ctx, ".endObject\n");
}

static void
PrintAsm(Ctx* ctx, struct Asm* asm)
{
	WRITE(ctx, ".info\n");
	ENTER(ctx);
	{
		struct Papyrus_String str;

		str = GetString(ctx, asm->info.source);
		WRITE(ctx, ".source \""STR"\"\n", STRFMT(str));

		WRITE(ctx, ".modifyTime 0\n");
		WRITE(ctx, ".compileTime %llu\n", asm->info.time);

		str = GetString(ctx, asm->info.username);
		WRITE(ctx, ".user \""STR"\"\n", STRFMT(str));

		str = GetString(ctx, asm->info.computer);
		WRITE(ctx, ".computer \""STR"\"\n", STRFMT(str));
	}
	EXIT(ctx);
	WRITE(ctx, ".endInfo\n");

	WRITE(ctx, ".userFlagsRef\n");
	WRITE(ctx, ".endUserFlagsRef\n");

	WRITE(ctx, ".objectTable\n");
	ENTER(ctx);
	FOREACHV_S(x, i, &asm->objects)
	{
		PrintObject(ctx, x);
	}
	EXIT(ctx);
	WRITE(ctx, ".endObjectTable\n");
}

void
Papyrus_DumpAsm(struct Asm* asm, struct Papyrus_EmitBuffer* buffer)
{
	Ctx ctx;
	ctx.asm = asm;
	ctx.indent = 0;
	ctx.buffer.cur = (char*)buffer->buffer;
	ctx.buffer.end = ctx.buffer.cur + buffer->size;
	ctx.emitbuf = buffer;

	PrintAsm(&ctx, asm);
	Flush(&ctx);
}
