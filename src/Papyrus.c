#pragma once

#include "Papyrus.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>


enum Token
{
	Tok_Whitespace,
	Tok_LineComment,
	Tok_BlockComment,
	Tok_DocComment,

	Tok_Error,

	Tok_Newline,

	Tok_Word,

	Tok_IntDec,
	Tok_IntHex,
	Tok_Float,
	Tok_String,

	Tok_LParen,
	Tok_RParen,
	Tok_LBrack,
	Tok_RBrack,

	Tok_Colon,
	Tok_Comma,
	Tok_Dot,

	Tok_Add,
	Tok_Sub,
	Tok_Mul,
	Tok_Div,
	Tok_Mod,

	Tok_Eq,
	Tok_Ne,
	Tok_Lt,
	Tok_Gt,
	Tok_Le,
	Tok_Ge,

	Tok_Not,
	Tok_Con,
	Tok_Dis,

	Tok_Assign,
	Tok_AddAssign,
	Tok_SubAssign,
	Tok_MulAssign,
	Tok_DivAssign,
	Tok_ModAssign,

	Tok_Eof,
};

#define ulex_token(name) Tok_ ## name

#include "ulex_papyrus.h"


#define MIN_ALIGN 8

// round x up to a, where a = 2^n
#define ALIGN(x, a) ((x) + (a) - 1 & -(a))

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

#if defined(__clang__) || defined(__GNUC__)
#	define LIKELY(...) __builtin_expect((__VA_ARGS__), 1)
#	define UNLIKELY(...) __builtin_expect((__VA_ARGS__), 0)
#else
#	define LIKELY(...) (__VA_ARGS__)
#	define UNLIKELY(...) (__VA_ARGS__)
#endif


static inline struct Papyrus_String
EmptyString()
{
	struct Papyrus_String string;
	string.size = 0;
	return string;
}


enum
{
	Key_As,
	Key_Auto,
	Key_AutoReadOnly,
	Key_BetaOnly,
	Key_Bool,
	Key_Const,
	Key_CustomEvent,
	Key_CustomEventName,
	Key_DebugOnly,
	Key_Else,
	Key_ElseIf,
	Key_EndEvent,
	Key_EndFunction,
	Key_EndGroup,
	Key_EndIf,
	Key_EndProperty,
	Key_EndState,
	Key_EndStruct,
	Key_EndWhile,
	Key_Event,
	Key_Extends,
	Key_False,
	Key_Float,
	Key_Function,
	Key_Global,
	Key_Group,
	Key_If,
	Key_Import,
	Key_Is,
	Key_Int,
	Key_Length,
	Key_Native,
	Key_New,
	Key_None,
	Key_Property,
	Key_Return,
	Key_ScriptName,
	Key_ScriptEventName,
	Key_State,
	Key_String,
	Key_Struct,
	Key_StructVarName,
	Key_True,
	Key_Var,
	Key_While,
};

typedef uint32_t Keyword;

#include "Keyword_Hash.i"

static int32_t
Keyword_Hash(const uint8_t* string, intptr_t size)
{
	if (size < 2 || size > 15)
		return -1;

	uint32_t f0 = 0, f1 = 0;
	for (intptr_t i = 0, j = 0; i < size; ++i, j += 256)
	{
		f0 += Keyword_Hash_t0[string[i] + j];
		f1 += Keyword_Hash_t1[string[i] + j];
	}

	return (Keyword_Hash_g[f0 % 56] + Keyword_Hash_g[f1 % 56]) % 45;
}

static const uint8_t Keyword_Strings[][16] = {
	"as",
	"auto",
	"autoreadonly",
	"betaonly",
	"bool",
	"const",
	"customevent",
	"customeventname",
	"debugonly",
	"else",
	"elseif",
	"endevent",
	"endfunction",
	"endgroup",
	"endif",
	"endproperty",
	"endstate",
	"endstruct",
	"endwhile",
	"event",
	"extends",
	"false",
	"float",
	"function",
	"global",
	"group",
	"if",
	"import",
	"is",
	"int",
	"length",
	"native",
	"new",
	"none",
	"property",
	"return",
	"scriptname",
	"scripteventname",
	"state",
	"string",
	"struct",
	"structvarname",
	"true",
	"var",
	"while",
};

static Keyword
GetKeyword(struct Papyrus_String string)
{
	if (string.size < 2 || string.size > 15)
		return -1;

	uint8_t buffer[16] = { 0 };
	memcpy(buffer, string.data, string.size);

	for (intptr_t i = 0; i < 16; ++i)
	{
		uint8_t c = buffer[i];
		if ((c >= 'A') & (c <= 'Z'))
			c = c - 'A' + 'a';
		buffer[i] = c;
	}

	int32_t hash = Keyword_Hash(buffer, string.size);
	if (memcmp(buffer, Keyword_Strings[hash], 16) == 0)
		return hash;

	return -1;
}

static bool
IsKeyword(struct Papyrus_String string)
{
	return GetKeyword(string) != -1;
}

static bool
IsSpecificKeyword(struct Papyrus_String string, Keyword key)
{
	if (string.size < 2 || string.size > 15)
		return false;

	uint8_t buffer[16] = { 0 };
	memcpy(buffer, string.data, string.size);

	for (intptr_t i = 0; i < 16; ++i)
	{
		uint8_t c = buffer[i];
		if ((c >= 'A') & (c <= 'Z'))
			c = c - 'A' + 'a';
		buffer[i] = c;
	}

	return memcmp(buffer, Keyword_Strings[key], 16) == 0;
}



struct Papyrus_Parser
{
	const char* source;

	struct {
		struct ulex_lexer lexer;

		uint32_t* tokens;
		uint32_t* offsets;

		intptr_t index;
		intptr_t count;

		uintptr_t bufferSize;
	} lex;

	struct {
		char* cur;
		char* end;

		void*(*allocate)(void*, uintptr_t);
		void* context;
	} syntax;

	struct {
		char* beg;
		char* cur;
		char* end;

		uintptr_t size;
	} synbuf;

	struct Papyrus_Allocator allocator;
};

typedef struct Papyrus_Parser Context;

static void*
AllocateSyntaxInternal(Context* ctx, uintptr_t size)
{
	const size_t blockSize = 4 * 1024;

	if (size > blockSize)
		return ctx->syntax.allocate(ctx->syntax.context, size);

	char* block = (char*)ctx->syntax
		.allocate(ctx->syntax.context, blockSize);
	if (block == NULL) return NULL;
	
	ctx->syntax.cur = block + size;
	ctx->syntax.end = block + blockSize;

	return block;
}

static void*
AllocateSyntax(Context* ctx, size_t size)
{
	size = ALIGN(size, MIN_ALIGN);

	char* cur = ctx->syntax.cur;
	char* newCur = cur + size;
	
	if (newCur > ctx->syntax.end)
		return AllocateSyntaxInternal(ctx, size);

	ctx->syntax.cur = newCur;
	return cur;
}

static int
CreateSyntax_(Context* ctx, size_t size, int kind, struct Papyrus_Syntax** out)
{
	struct Papyrus_Syntax* syntax = AllocateSyntax(ctx, size);

	if (syntax == NULL)
		return Papyrus_Error_OutOfMemory;

	syntax->kind = kind;
	syntax->flags = 0;

	*out = syntax;
	return Papyrus_Error_None;
}

#define CreateSyntax(ctx, type, out) \
	CreateSyntax_((ctx), sizeof(struct Papyrus_ ## type), \
		Papyrus_Syntax_ ## type, (struct Papyrus_Syntax**)(out))

#define CreateSyntax2(ctx, type, kind, out) \
	CreateSyntax_((ctx), sizeof(struct Papyrus_ ## type), \
		(kind), (struct Papyrus_Syntax**)(out))

static void*
CommitSyntax(Context* ctx, const void* data, size_t size)
{
	char* new = AllocateSyntax(ctx, size);

	if (new == NULL)
		return NULL;

	memcpy(new, data, size);

	return new;
}

static int
CommitString(Context* ctx, struct Papyrus_String string, struct Papyrus_String* out)
{
	if (string.size)
	{
		string.data = CommitSyntax(ctx, string.data, string.size);
		if (string.data == NULL) return Papyrus_Error_OutOfMemory;
	}

	*out = string;
	return Papyrus_Error_None;
}


/* syntax buffers:

allocates memory in blocks, linked together through the headers:
|-block-0 hdr-|----buffer-0 data----|-buffer-0 size-|----buffer-1 data----|
|-block-1 hdr-|------buffer-1 data------|-----------unused space----------|
|-block-2 hdr-|------------------------unused space-----------------------|

each new buffer is placed after the last with the size of the previous buffer
saved in between

the context holds pointers to the active block, and the size of the active
buffer
*/

struct SynBuf_Header
{
	uintptr_t size;
	struct SynBuf_Header* prev;
	struct SynBuf_Header* next;
};

static Papyrus_Error
SynBuf_Expand(Context* ctx)
{
	char* curBeg = ctx->synbuf.beg;

	struct SynBuf_Header* curHdr =
		(struct SynBuf_Header*)curBeg - 1;

	struct SynBuf_Header* newHdr = curHdr->next;
	uintptr_t newSize;

	if (newHdr == NULL)
	{
		newSize = MIN(curHdr->size * 2, 1024 * 1024);

		newHdr = ctx->allocator.allocate(ctx->allocator.context, newSize);
		if (newHdr == NULL) return Papyrus_Error_OutOfMemory;

		newHdr->size = newSize;
		newHdr->prev = curHdr;
		newHdr->next = NULL;

		curHdr->next = newHdr;
	}
	else newSize = newHdr->size;

	ctx->synbuf.beg = (char*)(newHdr + 1);
	ctx->synbuf.end = (char*)newHdr + newSize;

	return Papyrus_Error_None;
}

static Papyrus_Error
SynBuf_Create(Context* ctx)
{
	Papyrus_Error error;

	uintptr_t space = MAX(sizeof(uintptr_t), MIN_ALIGN);

	char* cur = ctx->synbuf.cur;
	char* end = ctx->synbuf.end;

	if (UNLIKELY((uintptr_t)(end - cur) < space))
	{
		if (error = SynBuf_Expand(ctx))
			return error;

		cur = ctx->synbuf.cur;
	}

	*(uintptr_t*)cur = ctx->synbuf.size;
	ctx->synbuf.size = 0;

	ctx->synbuf.cur = cur + space;

	return Papyrus_Error_None;
}

static Papyrus_Error
SynBuf_AppendInternal(Context* ctx, const char* data, uintptr_t size)
{
	Papyrus_Error error;

	char* begSave = ctx->synbuf.beg;
	char* curSave = ctx->synbuf.cur;
	char* endSave = ctx->synbuf.end;

	char* cur = curSave;
	char* end = endSave;

	uintptr_t remSize = size;
	uintptr_t space = end - cur;

	do
	{
		memcpy(cur, data, space);

		data += space;
		remSize -= space;

		if (error = SynBuf_Expand(ctx))
		{
			ctx->synbuf.beg = begSave;
			ctx->synbuf.cur = curSave;
			ctx->synbuf.end = endSave;
			return error;
		}

		cur = ctx->synbuf.cur;
		end = ctx->synbuf.end;
	} while ((space = end - cur) < remSize);
	
	memcpy(cur, data, remSize);

	ctx->synbuf.cur = cur + remSize;
	ctx->synbuf.size += size;

	return Papyrus_Error_None;
}

static Papyrus_Error
SynBuf_Append(Context* ctx, const void* data, uintptr_t size)
{
	if (size == 0)
		return Papyrus_Error_None;

	ALIGN(size, MIN_ALIGN);

	char* cur = ctx->synbuf.cur;
	char* end = ctx->synbuf.end;

	char* newCur = cur + size;

	if (UNLIKELY(newCur > end))
		return SynBuf_AppendInternal(ctx, (const char*)data, size);

	memcpy(cur, data, size);
	ctx->synbuf.cur = newCur;
	ctx->synbuf.size += size;

	return Papyrus_Error_None;
}

static Papyrus_Error
SynBuf_Commit(Context* ctx, uintptr_t objSize, const void** outdata, uintptr_t* outsize)
{
	uintptr_t size = ctx->synbuf.size;

	char* beg = ctx->synbuf.beg;
	char* cur = ctx->synbuf.cur;
	char* end = ctx->synbuf.end;

	if (size > 0)
	{
		void* data = AllocateSyntax(ctx, size);
		if (data == NULL) return Papyrus_Error_OutOfMemory;

		uintptr_t remSize = size;
		char* dataWrite = (char*)data + size;

		for (uintptr_t space; remSize > (space = cur - beg); remSize -= space)
		{
			memcpy(dataWrite -= space, beg, space);

			struct SynBuf_Header* curHdr = (struct SynBuf_Header*)beg - 1;
			struct SynBuf_Header* newHdr = curHdr->prev;

			beg = (char*)(newHdr + 1);
			cur = (char*)newHdr + newHdr->size;
			end = cur;
		}

		memcpy(data, cur -= remSize, remSize);

		*outdata = data;
	}

	if (cur == beg)
	{
		struct SynBuf_Header* curHdr = (struct SynBuf_Header*)beg - 1;
		struct SynBuf_Header* newHdr = curHdr->prev;

		beg = (char*)(newHdr + 1);
		cur = (char*)newHdr + newHdr->size;
		end = cur;
	}

	ctx->synbuf.size = *(uintptr_t*)
		(cur -= MAX(sizeof(uintptr_t), MIN_ALIGN));
	ctx->synbuf.beg = beg;
	ctx->synbuf.cur = cur;

	*outsize = size / objSize;
	return Papyrus_Error_None;

}

#define SynBuf_Commit(ctx, outdata, outsize) \
	SynBuf_Commit((ctx), sizeof(**outdata), (void**)(outdata), (void*)(outsize))


static enum Token
PeekInternal(Context* ctx)
{
	struct ulex_lexer* lexer = &ctx->lex.lexer;

	intptr_t tokenCount;
	ulex_error error = ulex_lex(lexer, 0, &tokenCount);

	uint32_t* __restrict tokens = ulex_get_tokens(lexer);
	uint32_t* __restrict offsets = ulex_get_offsets(lexer);

	switch (error)
	{
	case ulex_error_insufficient_buffer:
		break;

	default:
		{
			// token buffers have space for two extra tokens (newline, eof)
			uint32_t offset = offsets[tokenCount - 1];

			tokens[tokenCount] = Tok_Newline;
			offsets[tokenCount++] = offset;

			tokens[tokenCount] = Tok_Eof;
			offsets[tokenCount++] = offset;
		}
		break;
	}

	ctx->lex.index = 0;
	ctx->lex.count = tokenCount;

	ctx->lex.tokens = tokens;
	ctx->lex.offsets = offsets;

	return tokens[0];
}

static inline enum Token
Peek(Context* ctx)
{
	intptr_t index = ctx->lex.index;
	if (LIKELY(index < ctx->lex.count))
		return ctx->lex.tokens[index];
	return PeekInternal(ctx);
}

static inline struct Papyrus_String
PeekString(Context* ctx)
{
	intptr_t index = ctx->lex.index;
	uint32_t* offsets = ctx->lex.offsets;
	// offsets[-1] is set to 0 at parser init
	intptr_t offset = offsets[index - 1];

	struct Papyrus_String string;
	string.data = ctx->source + offset;
	string.size = offsets[index] - offset;
	return string;
}

static inline void
Consume(Context* ctx)
{
	intptr_t newIndex = ctx->lex.index + 1;
	if (LIKELY(newIndex < ctx->lex.count))
	{
		for (uint32_t* tokens = ctx->lex.tokens; 1;)	
		{
			switch (tokens[newIndex])
			{
			case Tok_Whitespace:
			case Tok_LineComment:
			case Tok_BlockComment:
			case Tok_DocComment:
				++newIndex;
				continue;
			}
			break;
		}
		ctx->lex.index = newIndex;
	}
}


enum
{
	Def_Event = 0x1,
	Def_Function = 0x2,
	Def_Group = 0x4,
	Def_Import = 0x8,
	Def_Property = 0x10,
	Def_State = 0x20,
	Def_Struct = 0x40,
	Def_Variable = 0x80,
};

enum
{
	Prec_Assign,
	Prec_Disjunction,
	Prec_Conjunction,
	Prec_Multiplicative,
	Prec_Additive,
	Prec_Comparison,
	Prec_Unary,
	Prec_Cast,

	Prec_Default = Prec_Assign,
};


static Papyrus_Error
ParseDef(Context* ctx, uint32_t mask, const struct Papyrus_Syntax** out);

static Papyrus_Error
ParseExpr(Context* ctx, uint32_t prec, const struct Papyrus_Syntax** out);

static Papyrus_Error
ParseStmt(Context* ctx, const struct Papyrus_Syntax** out);


static Papyrus_Error
ParseFullName(Context* ctx, struct Papyrus_String string, struct Papyrus_FullName* out)
{
	Papyrus_Error error;

	if (error = SynBuf_Create(ctx))
		return error;

	if (string.size > 0)
		if (error = SynBuf_Append(ctx, &string, sizeof(string)))
			return error;

reset:
	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(string = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	if (error = SynBuf_Append(ctx, &string, sizeof(string)))
		return error;

	if (Peek(ctx) == Tok_Colon)
	{
		Consume(ctx);
		goto reset;
	}

	if (error = SynBuf_Commit(ctx, &out->parts.data, &out->parts.size))
		return error;

	return Papyrus_Error_None;
}

static Papyrus_Error
ParseFlags(Context* ctx, uint32_t mask, uint32_t* inout)
{
	uint32_t flags = *inout;

reset:
	if (Peek(ctx) == Tok_Word)
	{
		switch (GetKeyword(PeekString(ctx)))
		{
			uint32_t flag;

		case Key_Auto:
			flag = Papyrus_Flags_Auto;
			goto got_flag;

		case Key_BetaOnly:
			flag = Papyrus_Flags_BetaOnly;
			goto got_flag;

	//	case Key_Conditional:
	//		flag = Papyrus_Flags_Conditional;
	//		goto got_flag;

		case Key_Const:
			flag = Papyrus_Flags_Const;
			goto got_flag;

		case Key_DebugOnly:
			flag = Papyrus_Flags_DebugOnly;
			goto got_flag;

	//	case Key_Default:
	//		flag = Papyrus_Flags_Default;
	//		goto got_flag;

	//	case Key_Hidden:
	//		flag = Papyrus_Flags_Hidden;
	//		goto got_flag;

		got_flag:
			if ((mask & flag) == 0)
				break;

			if (flags & flag)
				return Papyrus_Error_InvalidSyntax;

			flags |= flag;

			Consume(ctx);
			goto reset;
		}
	}

	*inout = flags;
	return Papyrus_Error_None;
}

static const struct Papyrus_String VarString = { .data = "var", .size = 3 };
static const struct Papyrus_String IntString = { .data = "int", .size = 3 };
static const struct Papyrus_String BoolString = { .data = "bool", .size = 4 };
static const struct Papyrus_String FloatString = { .data = "float", .size = 5 };
static const struct Papyrus_String StringString = { .data = "string", .size = 6 };

static Papyrus_Error
ParseType(Context* ctx, const struct Papyrus_Syntax** out)
{
	Papyrus_Error error;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;

	struct Papyrus_FullName name;
	switch (GetKeyword(PeekString(ctx)))
	{
	case Key_Var: name.parts.data = &VarString; goto keyword_type;
	case Key_Int: name.parts.data = &IntString; goto keyword_type;
	case Key_Bool: name.parts.data = &BoolString; goto keyword_type;
	case Key_Float: name.parts.data = &FloatString; goto keyword_type;
	case Key_String: name.parts.data = &StringString; goto keyword_type;

	keyword_type:
		name.parts.size = 1;
		Consume(ctx);
		break;

	default:
		return Papyrus_Error_InvalidSyntax;

	case -1: // identifier
		if (error = ParseFullName(ctx, EmptyString(), &name))
			return error;
		break;
	}

	struct Papyrus_Syntax* type;
	{
		struct Papyrus_Type* typename;
		if (error = CreateSyntax(ctx, Type, &typename))
			return error;

		typename->name = name;
		type = (struct Papyrus_Syntax*)typename;
	}

	while (true)
	{
		if (Peek(ctx) != Tok_LBrack)
			break;
		Consume(ctx);

		if (Peek(ctx) != Tok_RBrack)
			return Papyrus_Error_None;
		Consume(ctx);

		struct Papyrus_ArrayType* array;
		if (error = CreateSyntax(ctx, ArrayType, &array))
			return error;

		array->type = type;
		type = (struct Papyrus_Syntax*)array;
	}

	*out = type;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseExprList(Context* ctx, struct Papyrus_SyntaxArray* out)
{
	Papyrus_Error error;

	if (error = SynBuf_Create(ctx))
		return error;

reset:
	switch (Peek(ctx))
	{
	case Tok_RParen:
	case Tok_RBrack:
		break;

	default:
		{
			struct Papyrus_Syntax* expr;
			if (error = ParseExpr(ctx, Prec_Default, &expr))
				return error;

			if (error = SynBuf_Append(ctx, &expr, sizeof(expr)))
				return error;
		}
		goto reset;
	}

	if (error = SynBuf_Commit(ctx, &out->data, &out->size))
		return error;

	return Papyrus_Error_None;
}

static Papyrus_Error
ParseExpr(Context* ctx, uint32_t prec, const struct Papyrus_Syntax** out)
{
	Papyrus_Error error;

	struct Papyrus_Syntax* expr;
	switch (Peek(ctx))
	{
		{
			uint32_t opKind;

	case Tok_Sub:
			opKind = Papyrus_Syntax_NegExpr;
			goto unary;

	case Tok_Not:
			opKind = Papyrus_Syntax_NotExpr;
			goto unary;

		unary:
			Consume(ctx);

			if (error = ParseExpr(ctx, Prec_Unary, &expr))
				return error;

			struct Papyrus_UnaryExpr* new;
			if (error = CreateSyntax2(ctx, UnaryExpr, opKind, &new))
				return error;

			new->expr = expr;

			if (prec >= Prec_Unary)
				goto exit;
		}
		break;

	case Tok_Word:
		switch (GetKeyword(PeekString(ctx)))
		{
		case Key_New:
			{
				Consume(ctx);

				struct Papyrus_FullName name;
				if (error = ParseFullName(ctx, EmptyString(), &name))
					return error;

				if (Peek(ctx) != Tok_LBrack)
					return Papyrus_Error_InvalidSyntax;
				Consume(ctx);

				struct Papyrus_Syntax* extent;
				if (error = ParseExpr(ctx, Prec_Default, &extent))
					return error;

				if (Peek(ctx) != Tok_RBrack)
					return Papyrus_Error_InvalidSyntax;
				Consume(ctx);

				struct Papyrus_NewExpr* new;
				if (error = CreateSyntax(ctx, NewExpr, &new))
					return error;

				new->name = name;
				new->extent = extent;

				expr = (struct Papyrus_Syntax*)new;
			}
			break;

		case Key_None:
			{
				Consume(ctx);

				struct Papyrus_NoneExpr* none;
				if (error = CreateSyntax(ctx, NoneExpr, &none))
					return error;

				expr = (struct Papyrus_Syntax*)none;
			}
			break;

			{
		case Key_True:
		case Key_False:;
				//TODO: content

				Consume(ctx);

				struct Papyrus_Syntax* new;
				if (error = CreateSyntax(ctx, BoolExpr, &new))
					return error;

				expr = (struct Papyrus_Syntax*)new;
			}
			break;

		default:
		case -1: // identifier
			{
				struct Papyrus_FullName name;
				if (error = ParseFullName(ctx, EmptyString(), &name))
					return error;

				struct Papyrus_NameExpr* new;
				if (error = CreateSyntax(ctx, NameExpr, &new))
					return error;

				new->name = name;

				expr = (struct Papyrus_Syntax*)new;
			}
			break;
		}
		goto postfix;

	case Tok_IntDec:
		{
			//TODO: content

			Consume(ctx);

			struct Papyrus_Syntax* new;
			if (error = CreateSyntax(ctx, IntExpr, &new))
				return error;

			expr = (struct Papyrus_Syntax*)new;
		}
		goto postfix;

	case Tok_IntHex:
		{
			//TODO: content

			Consume(ctx);

			struct Papyrus_Syntax* new;
			if (error = CreateSyntax(ctx, IntExpr, &new))
				return error;

			expr = (struct Papyrus_Syntax*)new;
		}
		goto postfix;

	case Tok_Float:
		{
			//TODO: content

			Consume(ctx);

			struct Papyrus_Syntax* new;
			if (error = CreateSyntax(ctx, FloatExpr, &new))
				return error;

			expr = (struct Papyrus_Syntax*)new;
		}
		goto postfix;

	case Tok_String:
		{
			//TODO: content

			Consume(ctx);

			struct Papyrus_Syntax* new;
			if (error = CreateSyntax(ctx, StringExpr, &new))
				return error;

			expr = (struct Papyrus_Syntax*)new;
		}
		goto postfix;

	case Tok_LParen:
		{
			Consume(ctx);

			if (error = ParseExpr(ctx, Prec_Default, &expr))
				return error;

			if (Peek(ctx) != Tok_RParen)
				return Papyrus_Error_InvalidSyntax;
			Consume(ctx);
		}
		goto postfix;

	postfix:
		while (true)
		{
			switch (Peek(ctx))
			{
				{
					uint32_t opKind;
					uint32_t close;

			case Tok_LParen:
					opKind = Papyrus_Syntax_CallExpr;
					close = Tok_RParen;
					goto invoke;

			case Tok_LBrack:
					opKind = Papyrus_Syntax_IndexExpr;
					close = Tok_RBrack;
					goto invoke;

				invoke:
					Consume(ctx);

					struct Papyrus_SyntaxArray args;
					if (error = ParseExprList(ctx, &args))
						return error;

					if (Peek(ctx) != close)
						return Papyrus_Error_InvalidSyntax;
					Consume(ctx);

					struct Papyrus_InvokeExpr* new;
					if (error = CreateSyntax2(ctx, InvokeExpr, opKind, &new))
						return error;

					new->expr = expr;
					new->args = args;

					expr = (struct Papyrus_Syntax*)new;
				}
				continue;

			case Tok_Dot:
				{
					struct Papyrus_String string;

					Consume(ctx);

					if (Peek(ctx) != Tok_Word)
						return Papyrus_Error_InvalidSyntax;
					if (IsKeyword(string = PeekString(ctx)))
						return Papyrus_Error_InvalidSyntax;
					Consume(ctx);

					struct Papyrus_String name = string; 

					struct Papyrus_AccessExpr* new;
					if (error = CreateSyntax(ctx, AccessExpr, &new))
						return error;

					new->expr = expr;
					new->name = name;

					expr = (struct Papyrus_Syntax*)new;
				}
				continue;
			}
			break;
		}
		break;

	default:
		return Papyrus_Error_InvalidSyntax;
	}

	while (true)
	{
		uint32_t opKind;
		uint32_t opPrec;

		switch (Peek(ctx))
		{
#define OPERATOR(name, prec) \
	case Tok_ ## name: opKind = Papyrus_Syntax_ ## name ## Expr; opPrec = Prec_ ## prec; goto bin_left_assoc;
		OPERATOR(Eq, Comparison);
		OPERATOR(Ne, Comparison);
		OPERATOR(Lt, Comparison);
		OPERATOR(Gt, Comparison);
		OPERATOR(Le, Comparison);
		OPERATOR(Ge, Comparison);
		OPERATOR(Con, Conjunction);
		OPERATOR(Dis, Disjunction);
#undef OPERATOR

#define OPERATOR(name, prec) \
	case Tok_ ## name: opKind = Papyrus_Syntax_ ## name ## Expr; opPrec = Prec_ ## prec; goto bin_left_assoc; \
	case Tok_ ## name ## Assign: opKind = Papyrus_Syntax_ ## name ## AssignExpr; opPrec = Prec_Assign; goto bin_right_assoc;
		OPERATOR(Add, Additive);
		OPERATOR(Sub, Additive);
		OPERATOR(Mul, Multiplicative);
		OPERATOR(Div, Multiplicative);
		OPERATOR(Mod, Multiplicative);
#undef OPERATOR
		
	case Tok_Assign: opKind = Papyrus_Syntax_AssignExpr; opPrec = Prec_Assign; goto bin_right_assoc;

	case Tok_Word:
		switch (GetKeyword(PeekString(ctx)))
		{
		case Key_As:
			opKind = Papyrus_Syntax_AsExpr;
			opPrec = Prec_Cast;
			goto bin_left_assoc;

		case Key_Is:
			opKind = Papyrus_Syntax_IsExpr;
			opPrec = Prec_Cast;
			goto bin_left_assoc;
		}

		bin_right_assoc:
			if (opPrec < prec)
				goto exit;
			break;

		bin_left_assoc:
			if (opPrec <= prec)
				goto exit;
			break;

		default:
			goto exit;
		}

		Consume(ctx);

		struct Papyrus_Syntax* right;
		if (error = ParseExpr(ctx, prec, &right))
			return error;

		struct Papyrus_BinaryExpr* bin;
		if (error = CreateSyntax2(ctx, BinaryExpr, opKind, &bin))
			return error;

		bin->left = expr;
		bin->right = right;

		expr = (struct Papyrus_Syntax*)bin;
	}

exit:
	*out = expr;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseScope(Context* ctx, struct Papyrus_Scope* out)
{
	Papyrus_Error error;

	if (error = SynBuf_Create(ctx))
		return error;

	while (true)
	{
		switch (Peek(ctx))
		{
		case Tok_Newline:
			Consume(ctx);
			continue;

		case Tok_Word:
			switch (GetKeyword(PeekString(ctx)))
			{
			case Key_Else:
			case Key_ElseIf:
			case Key_EndEvent:
			case Key_EndFunction:
			case Key_EndIf:
			case Key_EndWhile:
				goto exit;
			}

		default:;
			struct Papyrus_Syntax* stmt;
			if (error = ParseStmt(ctx, &stmt))
				return error;

			if (error = SynBuf_Append(ctx, &stmt, sizeof(stmt)))
				return error;
		}
	}
exit:

	if (error = SynBuf_Commit(ctx, &out->stmts.data, &out->stmts.size))
		return error;

	return Papyrus_Error_None;
}

static Papyrus_Error
ParseStmt(Context* ctx, const struct Papyrus_Syntax** out)
{
	Papyrus_Error error;

	switch (Peek(ctx))
	{
	case Tok_Word:
		switch (GetKeyword(PeekString(ctx)))
		{
		case Key_Return:
			{
				Consume(ctx);

				struct Papyrus_Syntax* expr;

				if (Peek(ctx) != Tok_Newline)
				{
					if (error = ParseExpr(ctx, Prec_Default, &expr))
						return error;

					if (Peek(ctx) != Tok_Newline)
						return Papyrus_Error_InvalidSyntax;
				}
				else expr = NULL;
				Consume(ctx);

				struct Papyrus_ReturnStmt* stmt;
				if (error = CreateSyntax(ctx, ReturnStmt, &stmt))
					return error;

				stmt->expr = expr;

				*out = (struct Papyrus_Syntax*)expr;
				return Papyrus_Error_None;
			}

		case Key_If:
			{
				Consume(ctx);

				struct Papyrus_Syntax* expr;
				if (error = ParseExpr(ctx, Prec_Default, &expr))
					return error;

				if (Peek(ctx) != Tok_Newline)
					return Papyrus_Error_InvalidSyntax;
				Consume(ctx);

				struct Papyrus_Scope scope;
				if (error = ParseScope(ctx, &scope))
					return error;

				if (error = SynBuf_Create(ctx))
					return error;

				struct Papyrus_ElseClause* else_ = NULL;

			elif_reset:
				if (Peek(ctx) != Tok_Word)
					return Papyrus_Error_InvalidSyntax;

				switch (GetKeyword(PeekString(ctx)))
				{
				case Key_ElseIf:
					{
						if (else_ != NULL)
							return Papyrus_Error_InvalidSyntax;

						Consume(ctx);

						struct Papyrus_Syntax* expr;
						if (error = ParseExpr(ctx, Prec_Default, &expr))
							return error;

						if (Peek(ctx) != Tok_Newline)
							return Papyrus_Error_InvalidSyntax;
						Consume(ctx);

						struct Papyrus_Scope scope;
						if (error = ParseScope(ctx, &scope))
							return error;

						struct Papyrus_ElseIfClause* elif;
						if (error = CreateSyntax(ctx, ElseIfClause, &elif))
							return error;

						elif->cond = expr;
						elif->scope = scope;
						
						if (error = SynBuf_Append(ctx, &elif, sizeof(elif)))
							return error;

						goto elif_reset;
					}
					break;

				case Key_Else:
					{
						Consume(ctx);

						if (Peek(ctx) != Tok_Newline)
							return Papyrus_Error_InvalidSyntax;
						Consume(ctx);

						struct Papyrus_Scope scope;
						if (error = ParseScope(ctx, &scope))
							return error;

						if (error = CreateSyntax(ctx, ElseClause, &else_))
							return error;

						else_->scope = scope;

						if (Peek(ctx) != Tok_Word)
							return Papyrus_Error_InvalidSyntax;
						if (!IsSpecificKeyword(PeekString(ctx), Key_EndIf))
							return Papyrus_Error_InvalidSyntax;
					}
					break;

				case Key_EndIf:
					break;

				default:
					return Papyrus_Error_InvalidSyntax;
				}
				Consume(ctx);

				if (Peek(ctx) != Tok_Newline)
					return Papyrus_Error_InvalidSyntax;
				Consume(ctx);

				struct Papyrus_SyntaxArray elifs;
				if (error = SynBuf_Commit(ctx, &elifs.data, &elifs.size))
					return error;

				struct Papyrus_IfStmt* stmt;
				if (error = CreateSyntax(ctx, IfStmt, &stmt))
					return error;

				stmt->cond = expr;
				stmt->scope = scope;
				stmt->elifs.data = (struct Papyrus_ElseIfClause**)elifs.data;
				stmt->elifs.size = elifs.size;
				stmt->else_ = else_;

				*out = (struct Papyrus_Syntax*)stmt;
				return Papyrus_Error_None;
			}

		case Key_While:
			{
				Consume(ctx);

				struct Papyrus_Syntax* expr;
				if (error = ParseExpr(ctx, Prec_Default, &expr))
					return error;

				if (Peek(ctx) != Tok_Newline)
					return Papyrus_Error_InvalidSyntax;
				Consume(ctx);

				struct Papyrus_Scope scope;
				if (error = ParseScope(ctx, &scope))
					return error;

				if (Peek(ctx) != Tok_Word)
					return Papyrus_Error_InvalidSyntax;
				if (!IsSpecificKeyword(PeekString(ctx), Key_EndWhile))
					return Papyrus_Error_InvalidSyntax;
				Consume(ctx);

				if (Peek(ctx) != Tok_Newline)
					return Papyrus_Error_InvalidSyntax;
				Consume(ctx);

				struct Papyrus_WhileStmt* stmt;
				if (error = CreateSyntax(ctx, WhileStmt, &stmt))
					return error;

				stmt->cond = expr;
				stmt->scope = scope;

				*out = (struct Papyrus_Syntax*)stmt;
				return Papyrus_Error_None;
			}
		}

	default:
		{
			//TODO: are expression statements allowed?

			struct Papyrus_Syntax* expr;
			if (error = ParseExpr(ctx, Prec_Default, &expr))
				return error;

			if (Peek(ctx) != Tok_Newline)
				return Papyrus_Error_InvalidSyntax;
			Consume(ctx);

			struct Papyrus_ExprStmt* stmt;
			if (error = CreateSyntax(ctx, ExprStmt, &stmt))
				return error;

			stmt->expr = expr;

			*out = (struct Papyrus_Syntax*)stmt;
			return Papyrus_Error_None;
		}
	}
}

static Papyrus_Error
ParseDefs(Context* ctx, uint32_t mask, struct Papyrus_SyntaxArray* out)
{
	Papyrus_Error error;

	if (error = SynBuf_Create(ctx))
		return error;

	while (true)
	{
		switch (Peek(ctx))
		{
		case Tok_Newline:
			Consume(ctx);
			continue;

		case Tok_Eof:
			goto exit;

		case Tok_Word:
			switch (GetKeyword(PeekString(ctx)))
			{
			case Key_EndEvent:
			case Key_EndFunction:
			case Key_EndGroup:
			case Key_EndProperty:
			case Key_EndState:
			case Key_EndStruct:
				goto exit;
			}

		default:;
			struct Papyrus_Syntax* def;
			if (error = ParseDef(ctx, mask, &def))
				return error;

			if (error = SynBuf_Append(ctx, &def, sizeof(def)))
				return error;
		}
	}
exit:;

	if (error = SynBuf_Commit(ctx, &out->data, &out->size))
		return error;

	return Papyrus_Error_None;
}

static Papyrus_Error
ParseState(Context* ctx, uint32_t flags, const struct Papyrus_State** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_String name;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(name = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	if (error = SynBuf_Create(ctx))
		return error;

	while (true)
	{
		if (Peek(ctx) == Tok_Word && IsSpecificKeyword(PeekString(ctx), Key_EndState))
			break;

		struct Papyrus_Syntax* def;
		if (error = ParseDef(ctx, Def_Function | Def_Event, &def))
			return error;

		if (error = SynBuf_Append(ctx, &def, sizeof(def)))
			return error;
	}
	Consume(ctx);

	struct Papyrus_SyntaxArray defs;
	if (error = SynBuf_Commit(ctx, &defs.data, &defs.size))
		return error;

	struct Papyrus_State* state;
	if (error = CreateSyntax(ctx, State, &state))
		return error;

	state->syntax.flags = flags;

	state->name = name;
	state->defs = defs;

	*out = state;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseParams(Context* ctx, struct Papyrus_Params* out)
{
	Papyrus_Error error;

	if (error = SynBuf_Create(ctx))
		return error;

	if (Peek(ctx) != Tok_RParen)
	{
	reset:;
		struct Papyrus_Param param;

		if (error = ParseType(ctx, &param.type))
			return error;

		if (Peek(ctx) != Tok_Word)
			return Papyrus_Error_InvalidSyntax;
		if (IsKeyword(param.name = PeekString(ctx)))
			return Papyrus_Error_InvalidSyntax;
		Consume(ctx);

		if (Peek(ctx) == Tok_Assign)
		{
			Consume(ctx);
			if (error = ParseExpr(ctx, Prec_Default, &param.expr))
				return error;
		}
		else param.expr = NULL;

		if (error = SynBuf_Append(ctx, &param, sizeof(param)))
			return error;

		if (Peek(ctx) == Tok_Comma)
		{
			Consume(ctx);
			goto reset;
		}
	}

	if (error = SynBuf_Commit(ctx, &out->data, &out->size))
		return error;

	return Papyrus_Error_None;
}

static Papyrus_Error
ParseFunction(Context* ctx, struct Papyrus_Syntax* type, const struct Papyrus_Function** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_String name;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(name = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	if (Peek(ctx) != Tok_LParen)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_Params params;
	if (error = ParseParams(ctx, &params))
		return error;

	if (Peek(ctx) != Tok_RParen)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	uint32_t flags = 0;

flags_reset:
	if (Peek(ctx) == Tok_Word)
		switch (GetKeyword(PeekString(ctx)))
		{
			uint32_t flag;

		case Key_Global:
			flag = Papyrus_Flags_Global;
			goto got_flag;

		case Key_Native:
			flag = Papyrus_Flags_Native;
			goto got_flag;

		got_flag:
			if (flags & flag)
				return Papyrus_Error_InvalidSyntax;

			flags |= flag;

			Consume(ctx);
			goto flags_reset;
		}

	if (error = ParseFlags(ctx, Papyrus_Flags_BetaOnly | Papyrus_Flags_DebugOnly, &flags))
		return error;

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_Scope scope;
	if (flags & Papyrus_Flags_Native)
	{
		scope.stmts.data = NULL;
		scope.stmts.size = 0;
	}
	else
	{
		if (error = ParseScope(ctx, &scope))
			return error;

		if (Peek(ctx) != Tok_Word)
			return Papyrus_Error_InvalidSyntax;
		if (!IsSpecificKeyword(PeekString(ctx), Key_EndFunction))
			return Papyrus_Error_InvalidSyntax;
		Consume(ctx);

		if (Peek(ctx) != Tok_Newline)
			return Papyrus_Error_InvalidSyntax;
		Consume(ctx);
	}

	struct Papyrus_Function* function;
	if (error = CreateSyntax(ctx, Function, &function))
		return error;

	function->name = name;
	function->params = params;
	function->type = type;
	function->scope = scope;
	function->flags = flags;

	*out = function;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseVariable(Context* ctx, struct Papyrus_Syntax* type, const struct Papyrus_Variable** out)
{
	Papyrus_Error error;

	struct Papyrus_String name;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(name = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_Syntax* expr;
	if (Peek(ctx) == Tok_Assign)
	{
		Consume(ctx);

		if (error = ParseExpr(ctx, Prec_Default, &expr))
			return error;
	}
	else expr = NULL;

	uint32_t flagsmask =
		Papyrus_Flags_Conditional |
		Papyrus_Flags_Const |
		Papyrus_Flags_Hidden;

	uint32_t flags = 0;
	if (error = ParseFlags(ctx, flagsmask, &flags))
		return error;

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_Variable* variable;
	if (error = CreateSyntax(ctx, Variable, &variable))
		return error;

	variable->syntax.flags = flags;

	variable->type = type;
	variable->name = name;
	variable->expr = expr;

	*out = variable;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseProperty(Context* ctx, struct Papyrus_Syntax* type, const struct Papyrus_Property** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_String name;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(name = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	uint32_t flagsmask =
		Papyrus_Flags_Auto |
		Papyrus_Flags_Conditional |
		Papyrus_Flags_Const |
		Papyrus_Flags_Hidden |
		Papyrus_Flags_Mandatory;

	uint32_t flags = 0;
	if (error = ParseFlags(ctx, flagsmask, &flags))
		return error;

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_Function* get = NULL;
	struct Papyrus_Function* set = NULL;

	if ((flags & Papyrus_Flags_Auto) == 0)
	{
		while (true)
		{
			struct Papyrus_Syntax* functype;

			switch (Peek(ctx))
			{
			case Tok_Newline:
				Consume(ctx);
				continue;

			case Tok_Word:
				switch (GetKeyword(PeekString(ctx)))
				{
				case Key_EndProperty:
					goto exit;

				case Key_Function:
					functype = NULL;
					goto no_type;
				}

			default:
				if (error = ParseType(ctx, &functype))
					return error;

				if (Peek(ctx) != Tok_Word || !IsSpecificKeyword(PeekString(ctx), Key_Function))
					return Papyrus_Error_InvalidSyntax;

			no_type:;
				struct Papyrus_Function* function;
				if (error = ParseFunction(ctx, functype, &function))
					return error;

				struct Papyrus_String funcname = function->name;

				switch (funcname.data[0])
				{
				case 'G':
				case 'g':
					if (get != NULL)
						return Papyrus_Error_InvalidSyntax;
					get = function;
					goto got_func;

				case 'S':
				case 's':
					if (set != NULL)
						return Papyrus_Error_InvalidSyntax;
					set = function;
					goto got_func;

				got_func:
					if (funcname.size != 3)
						return Papyrus_Error_InvalidSyntax;

					char c;
					if ((c = funcname.data[1]) != 'E' && c != 'e')
						return Papyrus_Error_InvalidSyntax;
					if ((c = funcname.data[2]) != 'T' && c != 't')
						return Papyrus_Error_InvalidSyntax;

					break;

				default:
					return Papyrus_Error_InvalidSyntax;
				}
			}
		}
	exit:
		Consume(ctx);

		if (Peek(ctx) != Tok_Newline)
			return Papyrus_Error_InvalidSyntax;
		Consume(ctx);
	}

	struct Papyrus_Property* property;
	if (error = CreateSyntax(ctx, Property, &property))
		return error;

	property->syntax.flags = flags;

	property->type = type;
	property->name = name;
	property->get = get;
	property->set = set;

	*out = property;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseGroup(Context* ctx, const struct Papyrus_Group** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_String name;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(name = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	uint32_t flagsmask =
		Papyrus_Flags_Conditional |
		Papyrus_Flags_Const |
		Papyrus_Flags_Hidden;

	uint32_t flags = 0;
	if (error = ParseFlags(ctx, flagsmask, &flags))
		return error;

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	if (error = SynBuf_Create(ctx))
		return error;

	while (true)
	{
		if (Peek(ctx) == IsSpecificKeyword(PeekString(ctx), Key_EndGroup))
			break;

		struct Papyrus_Syntax* type;
		if (error = ParseType(ctx, &type))
			return error;

		struct Papyrus_Property* prop;
		if (error = ParseProperty(ctx, type, &prop))
			return error;

		if (error = SynBuf_Append(ctx, &prop, sizeof(prop)))
			return error;
	}
	Consume(ctx);

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_SyntaxArray props;
	if (error = SynBuf_Commit(ctx, &props.data, &props.size))
		return error;

	struct Papyrus_Group* group;
	if (error = CreateSyntax(ctx, Group, &group))
		return error;

	group->syntax.flags = flags;

	group->name = name;
	group->props = props;

	*out = group;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseEvent(Context* ctx, const struct Papyrus_Event** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_FullName typename;
	struct Papyrus_String name;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(name = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	switch (Peek(ctx))
	{
	case Tok_Colon:
		Consume(ctx);
		if (error = ParseFullName(ctx, name, &typename))
			return error;

		if (Peek(ctx) != Tok_Dot)
			return Papyrus_Error_InvalidSyntax;

	case Tok_Dot:
		Consume(ctx);

		if (Peek(ctx) != Tok_Word)
			return Papyrus_Error_InvalidSyntax;
		if (IsKeyword(name = PeekString(ctx)))
			return Papyrus_Error_InvalidSyntax;
		Consume(ctx);
		break;

	default:
		typename.parts.size = 0;
		break;
	}

	if (Peek(ctx) != Tok_LParen)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_Params params;
	if (error = ParseParams(ctx, &params))
		return error;

	if (Peek(ctx) != Tok_RParen)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	bool native = false;
	if (Peek(ctx) == Tok_Word && IsSpecificKeyword(PeekString(ctx), Key_Native))
	{
		native = true;
		Consume(ctx);
	}

	//TODO: wiki doesn't describe event flags...
	uint32_t flags = 0;

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_Scope scope;
	if (flags & Papyrus_Flags_Native)
	{
		scope.stmts.data = NULL;
		scope.stmts.size = 0;
	}
	else
	{
		if (error = ParseScope(ctx, &scope))
			return error;

		if (Peek(ctx) != Tok_Word || !IsSpecificKeyword(PeekString(ctx), Key_EndEvent))
			return Papyrus_Error_InvalidSyntax;
		Consume(ctx);

		if (Peek(ctx) != Tok_Newline)
			return Papyrus_Error_InvalidSyntax;
		Consume(ctx);
	}

	struct Papyrus_Event* event;
	if (error = CreateSyntax(ctx, Event, &event))
		return error;

	event->name = name;
	event->params = params;
	event->scope = scope;
	event->flags = flags;

	*out = event;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseStruct(Context* ctx, const struct Papyrus_Struct** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_String name;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(name = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_SyntaxArray vars;
	if (error = ParseDefs(ctx, Def_Variable, &vars))
		return error;

	if (Peek(ctx) != Tok_Word || !IsSpecificKeyword(PeekString(ctx), Key_EndStruct))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_Struct* struct_;
	if (error = CreateSyntax(ctx, Struct, &struct_))
		return error;

	struct_->name = name;
	struct_->vars = vars;

	*out = struct_;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseDef(Context* ctx, uint32_t mask, const struct Papyrus_Syntax** out)
{
	Papyrus_Error error;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;

	switch (GetKeyword(PeekString(ctx)))
	{
	case Key_Import:
		{
			if ((mask & Def_Import) == 0)
				return Papyrus_Error_InvalidSyntax;

			Consume(ctx);

			struct Papyrus_FullName name;
			if (error = ParseFullName(ctx, EmptyString(), &name))
				return error;

			if (Peek(ctx) != Tok_Newline)
				return Papyrus_Error_InvalidSyntax;

			struct Papyrus_Import* import;
			if (error = CreateSyntax(ctx, Import, &import))
				return error;

			import->name = name;

			*out = (struct Papyrus_Syntax*)import;
			return Papyrus_Error_None;
		}
		
	case Key_Event:
		if ((mask & Def_Event) == 0)
			return Papyrus_Error_InvalidSyntax;

		return ParseEvent(ctx, (struct Papyrus_Event**)out);

		{
			uint32_t flags;

	case Key_Auto:
			Consume(ctx);

			if (Peek(ctx) != Tok_Word && !IsSpecificKeyword(PeekString(ctx), Key_State))
				return Papyrus_Error_InvalidSyntax;

			flags = Papyrus_Flags_Auto;

			if (false)
			{
	case Key_State:
				flags = 0;
			}

			if ((mask & Def_State) == 0)
				return Papyrus_Error_InvalidSyntax;

			return ParseState(ctx, flags, (struct Papyrus_State**)out);
		}

	case Key_Group:
		if ((mask & Def_Group) == 0)
			return Papyrus_Error_InvalidSyntax;

		return ParseGroup(ctx, (struct Papyrus_Group**)out);

	case Key_Function:
		if ((mask & Def_Function) == 0)
			return Papyrus_Error_InvalidSyntax;

		return ParseFunction(ctx, NULL, (struct Papyrus_Function**)out);

	case Key_Struct:
		if ((mask & Def_Struct) == 0)
			return Papyrus_Error_InvalidSyntax;

		return ParseStruct(ctx, (struct Papyrus_Struct**)out);

	case Key_Int:
	case Key_Bool:
	case Key_Float:
	case Key_String:
	case -1: // identifier
		{
			struct Papyrus_Syntax* type;
			if (error = ParseType(ctx, &type))
				return error;

			if (Peek(ctx) != Tok_Word)
				return Papyrus_Error_InvalidSyntax;

			switch (GetKeyword(PeekString(ctx)))
			{
			case Key_Function:
				if ((mask & Def_Function) == 0)
					return Papyrus_Error_InvalidSyntax;

				return ParseFunction(ctx, type, (struct Papyrus_Function**)out);

			case Key_Property:
				if ((mask & Def_Property) == 0)
					return Papyrus_Error_InvalidSyntax;

				return ParseProperty(ctx, type, (struct Papyrus_Property**)out);

			case -1: // identifier
				return ParseVariable(ctx, type, (struct Papyrus_Variable**)out);
			}
		}

	default:
		return Papyrus_Error_InvalidSyntax;
	}
}

static Papyrus_Error
ParseScript(Context* ctx, const struct Papyrus_Script** out)
{
	Papyrus_Error error;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (!IsSpecificKeyword(PeekString(ctx), Key_ScriptName))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_String name;

	if (Peek(ctx) != Tok_Word)
		return Papyrus_Error_InvalidSyntax;
	if (IsKeyword(name = PeekString(ctx)))
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_FullName base;

	if (Peek(ctx) == Tok_Word && IsSpecificKeyword(PeekString(ctx), Key_Extends))
	{
		Consume(ctx);

		if (error = ParseFullName(ctx, EmptyString(), &base))
			return error;
	}
	else
	{
		base.parts.data = NULL;
		base.parts.size = 0;
	}

	uint32_t flags = 0;

	if (Peek(ctx) == Tok_Word && IsSpecificKeyword(PeekString(ctx), Key_Native))
	{
		Consume(ctx);

		flags |= Papyrus_Flags_Native;
	}

	uint32_t flagsmask =
		Papyrus_Flags_Conditional |
		Papyrus_Flags_Const |
		Papyrus_Flags_DebugOnly |
		Papyrus_Flags_BetaOnly |
		Papyrus_Flags_Hidden |
		Papyrus_Flags_Default;

	if (error = ParseFlags(ctx, flagsmask, &flags))
		return error;

	if (Peek(ctx) != Tok_Newline)
		return Papyrus_Error_InvalidSyntax;
	Consume(ctx);

	struct Papyrus_SyntaxArray defs;
	if (error = ParseDefs(ctx, -1, &defs))
		return error;

	struct Papyrus_Script* script;
	if (error = CreateSyntax(ctx, Script, &script))
		return error;

	script->syntax.flags = flags;

	script->name = name;
	script->base = base;
	script->defs = defs;

	*out = script;
	return Papyrus_Error_None;
}


static void*
Malloc(void* ctx, uintptr_t size)
{
	(void)ctx;
	return malloc(size);
}

static void
Free(void* ctx, void* data, uintptr_t size)
{
	(void)ctx;
	(void)size;
	free(data);
}

#define LEXBUF_DEFAULT_SIZE (64 * 1024)
#define LEXBUF_MINIMUM_SIZE (4 * 1024)

#define SYNBUF_INITIAL_SIZE (4 * 1024)

Papyrus_Error
Papyrus_Parser_Create(const struct Papyrus_ParserOptions* options, struct Papyrus_Parser** out)
{
	struct Papyrus_Allocator allocator;
	uintptr_t lexbufSize;

	if (options != NULL)
	{
		allocator = options->allocator;
		if (allocator.allocate == NULL)
		{
			if (allocator.deallocate != NULL)
				return Papyrus_Error_InvalidArgument;

			allocator.allocate = &Malloc;
			allocator.deallocate = &Free;
			allocator.context = NULL;
		}
		else if (allocator.deallocate == NULL)
		{
			return Papyrus_Error_InvalidArgument;
		}

		lexbufSize = options->lexerBufferSize;
		if (lexbufSize == 0)
		{
			lexbufSize = LEXBUF_DEFAULT_SIZE;
		}
		else
		{
			lexbufSize = lexbufSize & -8;
			if (lexbufSize < LEXBUF_MINIMUM_SIZE)
				return Papyrus_Error_InvalidArgument;
		}
	}
	else
	{
		allocator.allocate = &Malloc;
		allocator.deallocate = &Free;
		allocator.context = NULL;

		lexbufSize = LEXBUF_DEFAULT_SIZE;
	}

	// parser allocation: |-parser-|----initial syntax buffer----|----lexer buffer----|
	struct Papyrus_Parser* parser = (struct Papyrus_Parser*)allocator.allocate(
		allocator.context, sizeof(struct Papyrus_Parser) + SYNBUF_INITIAL_SIZE + lexbufSize);
	
	if (parser == NULL)
		return Papyrus_Error_OutOfMemory;

	parser->allocator = allocator;

	/* initialize syntax buffer */ {
		struct SynBuf_Header* hdr = (struct SynBuf_Header*)(parser + 1);

		hdr->prev = NULL;
		hdr->next = NULL;
		hdr->size = SYNBUF_INITIAL_SIZE;

		char* beg = (char*)(hdr + 1);
		parser->synbuf.beg = beg;
		parser->synbuf.cur = beg;
		parser->synbuf.end = (char*)hdr + SYNBUF_INITIAL_SIZE;
		parser->synbuf.size = 0;
	}

	/* initialize lexer */ {
		parser->lex.bufferSize = lexbufSize;

		parser->lex.index = 0;
		parser->lex.count = 0;

		struct ulex_lexer* lexer = &parser->lex.lexer;

		ulex_init(lexer);

		uint32_t* buffer = (uint32_t*)((char*)(parser + 1) + SYNBUF_INITIAL_SIZE);

		// -20: reserve space for offset at [-1] and two tokens (newline, eof)
		uintptr_t bufferSize = (lexbufSize - 20) / 8;

		uint32_t* tokenBuffer = buffer;

		// +3: two tokens and the offset at [-1]
		uint32_t* offsetBuffer = buffer + bufferSize + 3;

		offsetBuffer[-1] = 0;

		ulex_set_buffer_size(lexer, bufferSize);
		ulex_set_token_buffer(lexer, tokenBuffer);
		ulex_set_offset_buffer(lexer, offsetBuffer);
	}

	*out = parser;
	return Papyrus_Error_None;
}

void
Papyrus_Parser_Destroy(struct Papyrus_Parser* parser)
{
	struct Papyrus_Allocator allocator = parser->allocator;

	/* free syntax buffer */ {
		struct SynBuf_Header* hdr = (struct SynBuf_Header*)parser->synbuf.beg - 1;
		for (struct SynBuf_Header* next; hdr != NULL && (next = hdr->prev) != NULL; hdr = next)
			allocator.deallocate(allocator.context, hdr, hdr->size);
	}

	allocator.deallocate(allocator.context, parser,
		sizeof(struct Papyrus_Parser) + SYNBUF_INITIAL_SIZE + parser->lex.bufferSize);
}

Papyrus_Error
Papyrus_Parser_Parse(struct Papyrus_Parser* parser, const char* string, intptr_t stringSize,
	const struct Papyrus_ParseOptions* options, struct Papyrus_ParseResult* out)
{
	parser->source = string;
	ulex_set_source(&parser->lex.lexer, string, stringSize);

	parser->syntax.cur = NULL;
	parser->syntax.end = NULL;
	parser->syntax.allocate = options->allocateSyntax;
	parser->syntax.context = options->allocateSyntaxContext;

	struct Papyrus_Script* script;
	Papyrus_Error error = ParseScript(parser, &script);

	switch (error)
	{
	case Papyrus_Error_None:
		out->script = script;
		break;

	case Papyrus_Error_InvalidSyntax:
		{
			intptr_t offset = parser->lex.offsets[parser->lex.index - 1];

			int32_t lineCount = 0;
			intptr_t lineOffset = 0;

			intptr_t i = 0;
			for (; i < offset; ++i)
				if (string[i] == '\n')
				{
					++lineCount;
					lineOffset = i;
				}

			out->syntaxError.line = lineCount + 1;
			out->syntaxError.column = (int32_t)(i - lineOffset + 1);
		}
		goto cleanup;

	cleanup: // syntax buffer must be reset when an error occurs
		{
			struct SynBuf_Header* hdr = (struct SynBuf_Header*)(parser + 1);

			char* beg = (char*)(hdr + 1);
			parser->synbuf.beg = beg;
			parser->synbuf.cur = beg;
			parser->synbuf.end = (char*)hdr + SYNBUF_INITIAL_SIZE;
			parser->synbuf.size = 0;
		}
		break;
	}

	return error;
}
