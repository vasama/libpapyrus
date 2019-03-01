/* This file implements a Papyrus parser. Lexical analysis is done by a fast
generated state machine based lexer. The lexer does not produce separate tokens
for keywords, instead a perfect hash is used to lookup keywords during parsing.
The parser is a simple hand-written LL(k). The parser uses a stack-like data
structure for building syntax lists before committing them into the syntax
tree. */

#pragma once

#include "Papyrus/Parser.h"

#include "Papyrus/Diagnostics.h"
#include "Papyrus/Syntax.h"

#include "Arena.h"
#include "Macros.h"
#include "ObjectGraph.h"

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
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
#include "ulex/ulex_papyrus.h"


enum Keyword
{
	Key_As,
	Key_Auto,
	Key_AutoReadOnly,
	Key_Bool,
	Key_Else,
	Key_ElseIf,
	Key_EndEvent,
	Key_EndFunction,
	Key_EndIf,
	Key_EndProperty,
	Key_EndState,
	Key_EndWhile,
	Key_Event,
	Key_Extends,
	Key_False,
	Key_Float,
	Key_Function,
	Key_Global,
	Key_If,
	Key_Import,
	Key_Int,
	Key_Length,
	Key_Native,
	Key_New,
	Key_None,
	Key_Parent,
	Key_Property,
	Key_Return,
	Key_ScriptName,
	Key_Self,
	Key_State,
	Key_String,
	Key_True,
	Key_While,
};

typedef uint32_t Keyword;

#include "Keyword_Hash.i"

/* 16 chars is enough for all keywords, including null terminators, and enables
easy SSE manipulation. */
static const uint8_t Keyword_Strings[][16] = {
	"as",
	"auto",
	"autoreadonly",
	"bool",
	"else",
	"elseif",
	"endevent",
	"endfunction",
	"endif",
	"endproperty",
	"endstate",
	"endwhile",
	"event",
	"extends",
	"false",
	"float",
	"function",
	"global",
	"if",
	"import",
	"int",
	"length",
	"native",
	"new",
	"none",
	"parent",
	"property",
	"return",
	"scriptname",
	"self",
	"state",
	"string",
	"true",
	"while",
};

enum { Keyword_MinSize = 2 };
enum { Keyword_MaxSize = 12 };

static int32_t
Keyword_Hash(const uint8_t* string, intptr_t size)
{
	uint32_t f0 = 0, f1 = 0;
	for (intptr_t i = 0, j = 0; i < size; ++i, j += 256)
	{
		f0 += Keyword_Hash_t0[string[i] + j];
		f1 += Keyword_Hash_t1[string[i] + j];
	}

	f0 %= Keyword_Hash_c;
	f1 %= Keyword_Hash_c;

	return (Keyword_Hash_g[f0] + Keyword_Hash_g[f1]) % SIZE(Keyword_Strings);
}

static Keyword
GetKeyword(struct Papyrus_String string)
{
	if (string.size < Keyword_MinSize || string.size > Keyword_MaxSize)
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
	if (string.size < Keyword_MinSize || string.size > Keyword_MaxSize)
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


struct Synbuf_Header
{
	struct Synbuf_Header* next;
	uintptr_t size;
	char data[];
};

struct Synbuf_Reference
{
	char* cur;
	char* end;
	uintptr_t size;
	struct Synbuf_Header* hdr;
	struct Synbuf* synbuf;
};

struct Synbuf
{
	struct Synbuf_Reference prev;
	struct Arena_Pos pos;
	struct Synbuf_Header header;
};

typedef struct {
	struct Papyrus_SyntaxTree* tree;
	struct ObjectGraph_Cache cache;
	struct Papyrus_Allocator allocator;

	const char* source;

	struct {
		intptr_t index;
		intptr_t count;

		uint32_t* tokens;
		uint32_t* offsets;
		intptr_t bufferSize;

		struct ulex_lexer lexer;
	} lex;

	struct Synbuf_Reference synbuf;
	struct Arena arena;

	struct Papyrus_Diagnostics* diag;
} Ctx;

static void*
AllocateSyntax(Ctx* ctx, uintptr_t size)
{
	return ObjectGraph_CacheAllocate(
		ctx->tree, size, ctx->allocator, &ctx->cache);
}

static struct Papyrus_Syntax*
CreateSyntax_(Ctx* ctx, uintptr_t size, uint32_t kind)
{
	struct Papyrus_Syntax* syntax = AllocateSyntax(ctx, size);

	syntax->kind = (uint8_t)kind;
	syntax->ekind = 0;
	syntax->flags = 0;
	syntax->eflags = 0;

	return syntax;
}

struct Papyrus_Syntax_Empty
{
	struct Papyrus_Syntax syntax;
};

#define CreateSyntax2(kind, type) \
	((struct Papyrus_Syntax_ ## type*)CreateSyntax_(ctx, \
		sizeof(struct Papyrus_Syntax_ ## type), Papyrus_Syntax_ ## kind))

#define CreateSyntax(kind) CreateSyntax2(kind, kind)

static void*
CommitSyntax(Ctx* ctx, const void* data, uintptr_t size)
{
	char* new = AllocateSyntax(ctx, size);
	memcpy(new, data, size);
	return new;
}


static inline void
Synbuf_Create_(Ctx* ctx)
{
	struct Arena_Pos pos = Arena_GetPos(&ctx->arena);

	uintptr_t size = 4096;
	struct Synbuf* synbuf = (struct Synbuf*)
		Arena_Allocate(&ctx->arena, size);
	size -= sizeof(struct Synbuf);

	synbuf->prev = ctx->synbuf;
	synbuf->pos = pos;
	synbuf->header.next = NULL;
	synbuf->header.size = size;

	ctx->synbuf.cur = synbuf->header.data;
	ctx->synbuf.end = synbuf->header.data + size;
	ctx->synbuf.size = 0;
	ctx->synbuf.hdr = &synbuf->header;
	ctx->synbuf.synbuf = synbuf;
}

#define Synbuf_Create() Synbuf_Create_(ctx)

static char*
Synbuf_Allocate(Ctx* ctx, uintptr_t size)
{
	uintptr_t newSize = MAX(size + sizeof(struct Synbuf_Header), 4096);

	struct Synbuf_Header* new =
		(struct Synbuf_Header*)Arena_Allocate(&ctx->arena, newSize);
	newSize = newSize - sizeof(struct Synbuf_Header);

	new->next = NULL;
	new->size = newSize;

	ctx->synbuf.hdr->next = new;
	ctx->synbuf.hdr = new;

	ctx->synbuf.cur = new->data + size;
	ctx->synbuf.end = new->data + newSize;

	return new->data;
}

static inline void
Synbuf_Append_(Ctx* ctx, const void* data, uintptr_t size)
{
	char* cur = ctx->synbuf.cur;
	char* new = cur + size;
	
	if (new > ctx->synbuf.end)
	{
		cur = Synbuf_Allocate(ctx, size);
	}
	else
	{
		ctx->synbuf.cur = new;
	}
	memcpy(cur, data, size);
	ctx->synbuf.size += size;
}

#define Synbuf_Append(data) \
	Synbuf_Append_(ctx, data, sizeof(*data))

static inline void
Synbuf_Commit_(Ctx* ctx, uintptr_t objSize,
	const void** outdata, intptr_t* outsize)
{
	struct Synbuf* synbuf = ctx->synbuf.synbuf;

	uintptr_t size = ctx->synbuf.size;
	if (size > 0)
	{
		char* data = (char*)AllocateSyntax(ctx, size);
		*outdata = data;

		uintptr_t rem = size;
		struct Synbuf_Header* hdr = &synbuf->header;
		for (uintptr_t s; (s = hdr->size) < rem; hdr = hdr->next)
		{
			memcpy(data, hdr->data, s);
			data += s;
			rem -= s;
		}
		memcpy(data, hdr->data, rem);
	}
	*outsize = (intptr_t)(size / objSize);

	ctx->synbuf = synbuf->prev;
	Arena_SetPos(&ctx->arena, synbuf->pos);
}

#define Synbuf_Commit(out) \
	Synbuf_Commit_(ctx, sizeof(*(out)->data), \
		(void**)&(out)->data, (intptr_t*)&(out)->size)


//TODO: lexer buffer documentation

enum { MaxLookahead = 2 };

static enum Token
PeekInternal(Ctx* ctx, intptr_t lookahead)
{
	assert(lookahead <= MaxLookahead);

	struct ulex_lexer* lexer = &ctx->lex.lexer;

	uint32_t* tokens = ctx->lex.tokens;
	uint32_t* offsets = ctx->lex.offsets;

	if (lookahead > 0)
	{
		intptr_t index = ctx->lex.index;
		memcpy(tokens, tokens + index, lookahead * sizeof(uint32_t));
		memcpy(offsets, offsets + index * 2, lookahead * 2 * sizeof(uint32_t));
	}

	intptr_t tokenCount = ctx->lex.count;

	intptr_t bufferSize = ctx->lex.bufferSize;
	uint32_t* tokens2 = tokens + lookahead;
	uint32_t* offsets2 = offsets + lookahead + bufferSize;

	ulex_error error = ulex_lex(lexer, 0, tokens2,
		offsets2, bufferSize - lookahead - 2, &tokenCount);

	tokenCount += lookahead;

	switch (error)
	{
	case ulex_error_insufficient_buffer:
		break;

	default:
		{
			// insert newline and eof at the end

			uint32_t lastOffset = offsets2[tokenCount - 1];

			tokens2[tokenCount] = Tok_Newline;
			offsets2[tokenCount++] = lastOffset;

			tokens2[tokenCount] = Tok_Eof;
			offsets2[tokenCount++] = lastOffset;
		}
		break;
	}

	/* skip whitespace and comments */ {
		uint32_t* tokenOut = tokens + lookahead;
		uint32_t* offsetOut = offsets + lookahead * 2;

		offsets2[-1] = 0;

		intptr_t index = lookahead;
		for (; index < tokenCount; ++index)
		{
			uint32_t token = *tokens2;

			*tokenOut = token;
			*offsetOut = *(offsets2 - 1);
			*(offsetOut + 1) = *offsets2;

			switch (token)
			{
			case Tok_Whitespace:
			case Tok_LineComment:
			case Tok_BlockComment:
			case Tok_DocComment:
				break;

			default:
				tokenOut += 1;
				offsetOut += 2;
				break;
			}

			tokens2 += 1;
			offsets2 += 1;
		}
		tokenCount = index;
	}

	ctx->lex.index = 0;
	ctx->lex.count = tokenCount;

	return tokens[lookahead];
}

static inline enum Token
Peek_(Ctx* ctx, intptr_t lookahead)
{
	intptr_t index = ctx->lex.index + lookahead;
	if (LIKELY(index < ctx->lex.count))
		return ctx->lex.tokens[index];
	return PeekInternal(ctx, lookahead);
}

#define Peek(lookahead) Peek_(ctx, (lookahead))

static inline struct Papyrus_String
PeekString_(Ctx* ctx, intptr_t lookahead)
{
	intptr_t index = (ctx->lex.index + lookahead) * 2;
	uint32_t* offsets = ctx->lex.offsets;

	intptr_t offset = offsets[index];

	struct Papyrus_String string;
	string.data = ctx->source + offset;
	string.size = offsets[index + 1] - offset;
	return string;
}

#define PeekString(lookahead) PeekString_(ctx, (lookahead))

static inline void
Consume_(Ctx* ctx, intptr_t count)
{
	assert(ctx->lex.index + count <= ctx->lex.count);
	ctx->lex.index += count;
}

#define Consume(count) Consume_(ctx, (count))


static void
ReportError(Ctx* ctx, const char* fmt, ...)
{
	struct Papyrus_String string;

	char buffer[1024];
	string.data = buffer;

	va_list vlist;
	va_start(vlist, fmt);
	string.size = vsnprintf(buffer, sizeof(buffer), fmt, vlist);
	va_end(vlist);

	struct Papyrus_Diagnostics* diag = ctx->diag;
	diag->report(diag, ctx->lex.offsets[ctx->lex.index * 2], string);
}

static void
ReportError_UnexpectedToken(Ctx* ctx, const struct Papyrus_String* expected)
{
	struct Papyrus_String tokenString = PeekString_(ctx, 0);

	intptr_t length;
	const char* suffix;

	switch (Peek_(ctx, 0))
	{
	case Tok_IntDec:
	case Tok_IntHex:
	case Tok_Float:
	case Tok_Word:
		length = 3;
		suffix = "...'";

		if (false)
		{
	case Tok_String:
			length = 4;
			suffix = "...\"'";
		}

		const intptr_t maxlen = 20;
		if (tokenString.size <= maxlen)
		{
	default:
			length = tokenString.size;
			suffix = "'";
		}
		else length = maxlen - length;
	}

	if (expected != NULL)
	{
		ReportError(ctx, "unexpected token '%.*s%s, expected %.*s",
			(int)length, tokenString.data, suffix,
			(int)expected->size, expected->data);
	}
	else
	{
		ReportError(ctx, "unexpected token '%.*s%s",
			(int)length, tokenString.data, suffix);
	}
}

static void
ReportError_ExpectedToken(Ctx* ctx, uint32_t token)
{
	struct Papyrus_String string;

	switch (token)
	{
#define TOKEN_STRING(tok, str) \
	case tok: string = Papyrus_String_CREATE(str); break;

		TOKEN_STRING(Tok_Newline, "end of line");
		TOKEN_STRING(Tok_LParen, "'('");
		TOKEN_STRING(Tok_RParen, "')'");
		TOKEN_STRING(Tok_LBrack, "'['");
		TOKEN_STRING(Tok_RBrack, "']'");
		TOKEN_STRING(Tok_Colon, "':'");
		TOKEN_STRING(Tok_Comma, "','");
		TOKEN_STRING(Tok_Dot, "'.'");

#undef TOKEN_STRING

	default:
		/* other tokens are not actually ever expected by the parser
		or get handled elsewhere (e.g. keywords, identifiers) */
		string.size = 0;
	}

	ReportError_UnexpectedToken(ctx, &string);
}

static void
ReportError_ExpectedKeyword(Ctx* ctx, uint32_t keyword)
{
	char string[18];

	memcpy(string + 1, Keyword_Strings[keyword], 16);
	intptr_t size = strlen(string + 1);

	string[0] = '\'';
	string[size + 1] = '\'';

	ReportError_UnexpectedToken(ctx,
		&(struct Papyrus_String){ string, size + 2 });
}


static const struct Papyrus_String ErrorString =
	Papyrus_String_INIT("<error>");

static inline void
SetError_(struct Papyrus_Syntax* syntax)
{
	syntax->flags |= Papyrus_SyntaxFlags_Error;
}

#define SetError(x) SetError_((struct Papyrus_Syntax*)(x))

#define IgnoreSyntaxError(error, syntax) \
	IgnoreSyntaxError((error), (struct Papyrus_Syntax*)(syntax))

static inline void
PropagateError_(struct Papyrus_Syntax* parent,
	const struct Papyrus_Syntax* child)
{
	parent->flags |= child->flags & Papyrus_SyntaxFlags_Error;
}

#define PropagateError(parent, child) \
	PropagateError_((struct Papyrus_Syntax*)(parent), \
		(struct Papyrus_Syntax*)(child))

static inline bool
TryConsume_(Ctx* ctx, uint32_t token)
{
	if (Peek_(ctx, 0) != token)
		return false;

	Consume_(ctx, 1);
	return true;
}

#define TryConsume(token) TryConsume_(ctx, (token))

static inline bool
ConsumeOrSetError(Ctx* ctx, uint32_t token, struct Papyrus_Syntax* syntax)
{
	if (TryConsume_(ctx, token))
		return true;

	SetError(syntax);
	ReportError_ExpectedToken(ctx, token);
	return false;
}

#define ConsumeOrSetError(token, syntax) \
	ConsumeOrSetError(ctx, (token), (struct Papyrus_Syntax*)(syntax))

static inline bool
TryConsumeKeyword_(Ctx* ctx, uint32_t keyword)
{
	if (Peek_(ctx, 0) != Tok_Word)
		return false;

	if (!IsSpecificKeyword(PeekString_(ctx, 0), keyword))
		return false;

	Consume_(ctx, 1);
	return true;
}

#define TryConsumeKeyword(keyword) TryConsumeKeyword_(ctx, (keyword))

static inline bool
ConsumeKeywordOrSetError_(Ctx* ctx,
	uint32_t keyword, struct Papyrus_Syntax* syntax)
{
	if (TryConsumeKeyword_(ctx, keyword))
		return true;

	SetError(syntax);
	ReportError_ExpectedKeyword(ctx, keyword);
	return false;
}

#define ConsumeKeywordOrSetError(keyword, syntax) \
	ConsumeKeywordOrSetError_(ctx, (keyword), (struct Papyrus_Syntax*)(syntax))

static inline bool
TryConsumeName_(Ctx* ctx, struct Papyrus_String* out)
{
	struct Papyrus_String string;

	if (Peek_(ctx, 0) == Tok_Word && !IsKeyword(string = PeekString_(ctx, 0)))
	{
		Consume_(ctx, 1);
		*out = string;
		return true;
	}

	*out = ErrorString;
	return false;
}

#define TryConsumeName(out) TryConsumeName_(ctx, (out))

static inline bool
ConsumeNameOrSetError(Ctx* ctx,
	struct Papyrus_Syntax* syntax, struct Papyrus_String* out)
{
	if (TryConsumeName_(ctx, out))
		return true;

	SetError(syntax);
	ReportError_UnexpectedToken(ctx, &Papyrus_String_CREATE("identifier"));
	return false;
}

#define ConsumeNameOrSetError(syntax, out) \
	ConsumeNameOrSetError(ctx, (struct Papyrus_Syntax*)(syntax), (out))

static inline void
ConsumeLine_(Ctx* ctx)
{
	while (Peek_(ctx, 0) != Tok_Newline)
		Consume_(ctx, 1);
	Consume_(ctx, 1);
}

#define ConsumeLine() ConsumeLine_(ctx)

static inline void
ConsumeLineAndSetError_(Ctx* ctx, struct Papyrus_Syntax* syntax)
{
	if (Peek_(ctx, 0) != Tok_Newline)
	{
		ReportError_ExpectedToken(ctx, Tok_Newline);
		SetError(syntax);

		ConsumeLine_(ctx);
	}
	else Consume_(ctx, 1);
}

#define ConsumeLineAndSetError(syntax) \
	ConsumeLineAndSetError_(ctx, (struct Papyrus_Syntax*)(syntax))


static struct Papyrus_Syntax_Scope*
ParseScope(Ctx* ctx);

static struct Papyrus_Syntax_Scope*
ParseDeclScope(Ctx* ctx, uint32_t mask, uint32_t close);


enum
{
	Decl_Event = 0x1,
	Decl_Function = 0x2,
	Decl_Import = 0x4,
	Decl_Property = 0x8,
	Decl_ScriptHeader = 0x10,
	Decl_State = 0x20,
	Decl_Variable = 0x40,
};

enum
{
	Prec_Disjunction,
	Prec_Conjunction,
	Prec_Comparison,
	Prec_Additive,
	Prec_Multiplicative,
	Prec_Unary,
	Prec_Cast,

	Prec_Default = 0,
};

static struct Papyrus_Syntax_Symbol*
ParseSymbol(Ctx* ctx)
{
	struct Papyrus_Syntax_Symbol* new = CreateSyntax(Symbol);

	Synbuf_Create();

	do
	{
		struct Papyrus_String name;
		ConsumeNameOrSetError(new, &name);

		Synbuf_Append(&name);

	} while (TryConsume(Tok_Colon));

	Synbuf_Commit(new);

	return new;
}

static struct Papyrus_Syntax_Type*
ParseType(Ctx* ctx)
{
	static const struct Papyrus_String ErrorString =
		Papyrus_String_INIT("<error-type>");
	static const struct Papyrus_Syntax_Symbol ErrorSymbol = {
		.syntax = {
			.kind = Papyrus_Syntax_Type,
			.ekind = Papyrus_Syntax_EKind_None,
			.flags = Papyrus_SyntaxFlags_Error,
			.eflags = 0,
		},
		.data = &ErrorString,
		.size = 1,
	};
	static const struct Papyrus_Syntax_Type ErrorType = {
		.syntax = {
			.kind = Papyrus_Syntax_Type,
			.ekind = Papyrus_Syntax_EKind_None,
			.flags = Papyrus_SyntaxFlags_Error,
			.eflags = 0,
		},
		.symbol = &ErrorSymbol,
	};

	struct Papyrus_Syntax_Type* new;

	if (Peek(0) == Tok_Word)
	{
		switch (GetKeyword(PeekString(0)))
		{
#ifndef __INTELLISENSE__
#define FUNDAMENTAL(x, n) \
	case Key_ ## x: \
		{ \
			static const struct Papyrus_String String = \
				Papyrus_String_INIT(n); \
			static const struct Papyrus_Syntax_Symbol Symbol = { \
				.syntax = { \
					.kind = Papyrus_Syntax_Symbol, \
					.ekind = Papyrus_Syntax_EKind_None, \
					.flags = 0, \
					.eflags = 0, \
				}, \
				.data = &String, \
				.size = 1, \
			}; \
			static const struct Papyrus_Syntax_Type Type = { \
				.syntax = { \
					.kind = Papyrus_Syntax_Type, \
					.ekind = Papyrus_Syntax_Type_ ## x, \
					.flags = 0, \
					.eflags = Papyrus_Syntax_TypeFlags_Fundamental, \
				}, \
				.symbol = &Symbol, \
			}; \
			new = (struct Papyrus_Syntax_Type*)&Type; \
		} \
		goto fundamental_type

			FUNDAMENTAL(Int, "int");
			FUNDAMENTAL(Bool, "bool");
			FUNDAMENTAL(Float, "float");
			FUNDAMENTAL(String, "string");
#undef FUNDAMENTAL
#endif

		fundamental_type:
			Consume(1);
			break;

		case -1: // identifier
			new = CreateSyntax(Type);
			new->symbol = ParseSymbol(ctx);
			PropagateError(new, new->symbol);
			break;

		default:
			goto no_name;
		}

		if (TryConsume(Tok_LBrack))
		{
			new->syntax.eflags |= Papyrus_Syntax_TypeFlags_Array;

			ConsumeOrSetError(Tok_RBrack, new);
		}
	}
	else
	{
	no_name:
		ReportError_UnexpectedToken(ctx, &Papyrus_String_CREATE("type"));
		new = (struct Papyrus_Syntax_Type*)&ErrorType;
	}

	return new;
}


static struct Papyrus_Syntax*
ParseExpr(Ctx* ctx, uint32_t prec)
{
	struct Papyrus_Syntax* expr;

	switch (Peek(0))
	{
		{
			uint32_t ekind;

	case Tok_Sub:
			ekind = Papyrus_Syntax_UnaryExpr_Neg;
			goto unary;

	case Tok_Not:
			ekind = Papyrus_Syntax_UnaryExpr_Not;
			goto unary;

		unary:
			Consume(1);

			struct Papyrus_Syntax_UnaryExpr* new = CreateSyntax(UnaryExpr);

			new->syntax.ekind = ekind;
			new->expr = ParseExpr(ctx, Prec_Unary);
			PropagateError(new, new->expr);

			if (prec >= Prec_Unary)
				goto exit;
		}
		break;

	case Tok_Word:
		{
			struct Papyrus_String string = PeekString(0);

			switch (GetKeyword(string))
			{
			case Key_New:
				{
					Consume(1);

					struct Papyrus_Syntax_NewExpr* new = CreateSyntax(NewExpr);

					new->name = ParseSymbol(ctx);
					PropagateError(new, new->name);

					if (ConsumeOrSetError(Tok_LBrack, new))
					{
						new->extent = ParseExpr(ctx, Prec_Default);
						PropagateError(new, new->extent);

						ConsumeOrSetError(Tok_RBrack, new);
					}
					else new->extent = NULL;

					expr = &new->syntax;
				}
				break;

			case Key_None:
				{
					Consume(1);

					expr = &CreateSyntax2(ConstExpr, Empty)->syntax;
				}
				break;
			
				{
					uint32_t ekind;

			case Key_True:
					ekind = Papyrus_Syntax_ConstExpr_True;
					goto got_bool;

			case Key_False:
					ekind = Papyrus_Syntax_ConstExpr_False;
					goto got_bool;

				got_bool:
					expr = &CreateSyntax2(ConstExpr, Empty)->syntax;
					expr->ekind = ekind;
				}
				break;

			case -1: // identifier
				{
					Consume(1);

					struct Papyrus_Syntax_NameExpr* new =
						CreateSyntax(NameExpr);
					
					struct Papyrus_Syntax_Symbol* symbol =
						CreateSyntax(Symbol);
					symbol->data = (struct Papyrus_String*)
						CommitSyntax(ctx, &string, sizeof(string));
					symbol->size = 1;

					new->symbol = symbol;
					expr = &new->syntax;
				}
				break;

			default:
				goto not_expr;
			}
		}
		goto postfix;
		
		{
			uint32_t ekind;

	case Tok_IntDec:
	case Tok_IntHex:
			ekind = Papyrus_Syntax_ConstExpr_Int;
			goto got_const_expr;

	case Tok_Float:
			ekind = Papyrus_Syntax_ConstExpr_Float;
			goto got_const_expr;

	case Tok_String:
			ekind = Papyrus_Syntax_ConstExpr_String;
			goto got_const_expr;

		got_const_expr:;
			struct Papyrus_Syntax_ConstExpr* new = CreateSyntax(ConstExpr);

			new->syntax.ekind = ekind;
			new->string = PeekString(0);
			Consume(1);

			expr = &new->syntax;
		}
		goto postfix;

	case Tok_LParen:
		{
			Consume(1);
			expr = ParseExpr(ctx, Prec_Default);
			ConsumeOrSetError(Tok_RParen, expr);
		}
		goto postfix;

	postfix:
		switch (Peek(0))
		{
		case Tok_LParen:
			{
				Consume(1);

				struct Papyrus_Syntax_CallExpr* new = CreateSyntax(CallExpr);

				new->expr = expr;
				PropagateError(new, expr);

				Synbuf_Create();

				if (Peek(0) != Tok_RParen)
				{
					do
					{
						struct Papyrus_Syntax* expr =
							ParseExpr(ctx, Prec_Default);
						PropagateError(new, expr);

						Synbuf_Append(&expr);
					} while (TryConsume(Tok_Comma));
				}
				ConsumeOrSetError(Tok_RParen, new);

				Synbuf_Commit(&new->args);

				expr = &new->syntax;
			}
			goto postfix;

		case Tok_LBrack:
			{
				Consume(1);

				struct Papyrus_Syntax_BinaryExpr* new =
					CreateSyntax(BinaryExpr);

				new->left = expr;
				PropagateError(new, expr);

				new->right = ParseExpr(ctx, Prec_Default);
				PropagateError(new, new->right);

				ConsumeOrSetError(Tok_RBrack, new);

				expr = &new->syntax;
			}
			goto postfix;

		case Tok_Dot:
			{
				if (Peek(1) == Tok_Word)
					switch (Peek(2))
					{
					case Tok_Assign:
					case Tok_AddAssign:
					case Tok_SubAssign:
					case Tok_MulAssign:
					case Tok_DivAssign:
					case Tok_ModAssign:
						goto exit;
					}

				Consume(1);

				struct Papyrus_Syntax_AccessExpr* new =
					CreateSyntax(AccessExpr);

				new->expr = expr;
				PropagateError(new, expr);

				ConsumeNameOrSetError(new, &new->name);

				expr = &new->syntax;
			}
			goto postfix;
		}
		break;

	default:
	not_expr:
		{
			static const struct Papyrus_Syntax ErrorExpr  = {
				.kind = Papyrus_Syntax_ConstExpr,
				.ekind = Papyrus_Syntax_ConstExpr_None,
				.flags = Papyrus_SyntaxFlags_Error,
				.eflags = 0,
			};

			expr = (struct Papyrus_Syntax*)&ErrorExpr;
		}
		goto exit;
	}

	while (true)
	{
		switch (Peek(0))
		{
			{
				uint32_t ekind;
				uint32_t nprec;

#ifndef __INTELLISENSE__
#define OPERATOR(name, prec) \
	case Tok_ ## name: \
		ekind = Papyrus_Syntax_BinaryExpr_ ## name; \
		nprec = Prec_ ## prec; \
		goto got_binary_operator;

				OPERATOR(Add, Additive);
				OPERATOR(Sub, Additive);
				OPERATOR(Mul, Multiplicative);
				OPERATOR(Div, Multiplicative);
				OPERATOR(Mod, Multiplicative);
				OPERATOR(Eq, Comparison);
				OPERATOR(Ne, Comparison);
				OPERATOR(Lt, Comparison);
				OPERATOR(Gt, Comparison);
				OPERATOR(Le, Comparison);
				OPERATOR(Ge, Comparison);
				OPERATOR(Con, Conjunction);
				OPERATOR(Dis, Disjunction);
#undef OPERATOR
#endif

			got_binary_operator:
				if (nprec <= prec)
					goto exit;

				Consume(1);

				struct Papyrus_Syntax_BinaryExpr* new =
					CreateSyntax(BinaryExpr);

				new->syntax.ekind = ekind;

				new->left = expr;
				PropagateError(new, expr);

				new->right = ParseExpr(ctx, nprec);
				PropagateError(new, new->right);

				expr = &new->syntax;
			}
			break;

		case Tok_Word:
			if (prec <= Prec_Cast)
				goto exit;

			switch (GetKeyword(PeekString(0)))
			{
				uint32_t ekind;

			case Key_As:
				ekind = Papyrus_Syntax_CastExpr_As;
				goto got_cast_operator;

#if 0
			case Key_Is:
				ekind = Papyrus_Syntax_CastExpr_Is;
				goto got_cast_operator;
#endif

			got_cast_operator:
				Consume(1);

				struct Papyrus_Syntax_CastExpr* new = CreateSyntax(CastExpr);

				new->syntax.ekind = ekind;

				new->expr = expr;
				PropagateError(new, expr);

				new->type = ParseType(ctx);
				PropagateError(new, new->type);

				expr = &new->syntax;
			}
			break;

		default:
			goto exit;
		}
	}

exit:
	return expr;
}


static bool
ParseFlags(Ctx* ctx, uint32_t mask,uint32_t umask,
	uint32_t* flagsOut, uint32_t* uflagsOut)
{
	bool result = true;

	uint32_t flags = 0;
	uint32_t uflags = 0;

reset:
	if (Peek(0) == Tok_Word)
	{
		switch (GetKeyword(PeekString(0)))
		{
			uint32_t flag;

#ifndef __INTELLISENSE__
#define FLAG(x) \
	case Key_ ## x: \
		flag = Papyrus_Syntax_DeclFlags_ ## x; \
		goto got_flag

			FLAG(Auto);
			FLAG(AutoReadOnly);
			FLAG(Global);
			FLAG(Native);

#undef FLAG
#endif

		got_flag:
			if ((mask & flag) == 0)
			{
				ReportError(ctx,
					"flag is not applicable to the current declaration");
				result = false;
			}

			if (flags & flag)
			{
				ReportError(ctx, "each flag may only be specified once");
				result = false;
			}

			flags |= flag;
			Consume(1);

			goto reset;
		}
	}

	*flagsOut = (uint8_t)flags;
	*uflagsOut = uflags;
	return result;
}

static struct Papyrus_Syntax_ParamList*
ParseParams(Ctx* ctx)
{
	struct Papyrus_Syntax_ParamList* new = CreateSyntax(ParamList);

	Synbuf_Create();

	if (Peek(0) != Tok_RParen)
	{
		do
		{
			struct Papyrus_Syntax_Param* param = CreateSyntax(Param);

			param->type = ParseType(ctx);
			PropagateError(param, param->type);

			ConsumeNameOrSetError(param, &param->name);

			if (TryConsume(Tok_Assign))
			{
				param->expr = ParseExpr(ctx, Prec_Default);
				PropagateError(param, param->expr);
			}
			else param->expr = NULL;

			PropagateError(new, param);

			Synbuf_Append(&param);
		} while (TryConsume(Tok_Comma));
	}

	Synbuf_Commit(new);

	return new;
}


static struct Papyrus_Syntax_Event*
ParseEvent(Ctx* ctx)
{
	struct Papyrus_Syntax_Event* new = CreateSyntax(Event);

	Consume(1);
	ConsumeNameOrSetError(new, &new->name);

	if (ConsumeOrSetError(Tok_LParen, new))
	{
		new->params = ParseParams(ctx);
		PropagateError(new, new->params);

		ConsumeOrSetError(Tok_RParen, new);
	}
	else new->params = NULL;

	uint32_t flagsmask =
		Papyrus_Syntax_DeclFlags_Native;

	if (!ParseFlags(ctx, flagsmask, 0, &new->syntax.eflags, &new->uflags))
		SetError(new);

	ConsumeLineAndSetError(new);

	if ((new->syntax.eflags & Papyrus_Syntax_DeclFlags_Native) == 0)
	{
		new->scope = ParseScope(ctx);
		PropagateError(new, new->scope);

		ConsumeKeywordOrSetError(Key_EndEvent, new);
		ConsumeLineAndSetError(new);
	}
	else new->scope = NULL;

	return new;
}

static struct Papyrus_Syntax_Function*
ParseFunction(Ctx* ctx, struct Papyrus_Syntax_Type* type)
{
	struct Papyrus_Syntax_Function* new = CreateSyntax(Function);

	new->type = type;
	if (type != NULL)
		PropagateError(new, type);

	Consume(1);

	ConsumeNameOrSetError(new, &new->name);

	if (ConsumeOrSetError(Tok_LParen, new))
	{
		new->params = ParseParams(ctx);
		PropagateError(new, new->params);

		ConsumeOrSetError(Tok_RParen, new);
	}

	uint32_t flagsmask =
		Papyrus_Syntax_DeclFlags_Global |
		Papyrus_Syntax_DeclFlags_Native;

	if (!ParseFlags(ctx, flagsmask, 0, &new->syntax.eflags, &new->uflags))
		SetError(new);

	ConsumeLineAndSetError(new);

	if ((new->syntax.eflags & Papyrus_Syntax_DeclFlags_Native) == 0)
	{
		new->scope = ParseScope(ctx);
		PropagateError(new, new->scope);

		ConsumeKeywordOrSetError(Key_EndFunction, new);
		ConsumeLineAndSetError(new);
	}
	else new->scope = NULL;

	return new;
}

static struct Papyrus_Syntax_Property*
ParseProperty(Ctx* ctx, struct Papyrus_Syntax_Type* type)
{
	struct Papyrus_Syntax_Property* new = CreateSyntax(Property);

	new->type = type;
	PropagateError(new, type);

	Consume(1);

	ConsumeNameOrSetError(new, &new->name);

	uint32_t flagsmask =
		Papyrus_Syntax_DeclFlags_Auto |
		Papyrus_Syntax_DeclFlags_AutoReadOnly;

	uint32_t autoflagsmask =
		Papyrus_Syntax_DeclFlags_Auto |
		Papyrus_Syntax_DeclFlags_AutoReadOnly;

	struct Papyrus_Syntax* expr;
	if (TryConsume(Tok_Assign))
	{
		expr = ParseExpr(ctx, Prec_Default);
		flagsmask &= ~autoflagsmask;
	}
	else expr = NULL;

	if (!ParseFlags(ctx, flagsmask, 0, &new->syntax.eflags, &new->uflags))
		SetError(new);

	ConsumeLineAndSetError(new);

	uint32_t autoflags = new->syntax.eflags & autoflagsmask;

	if (autoflags != 0)
	{
		if ((autoflags & (autoflags - 1)) != 0)
		{
			ReportError(ctx,
				"'auto' and 'autoreadonly' may not be used together");
			SetError(new);
		}

		new->expr = expr;

		if (expr != NULL)
			PropagateError(new, expr);
	}
	else
	{
		if (expr != NULL)
		{
			ReportError(ctx, "non-auto properties may not have initializers");
			SetError(new);
		}

		new->scope = ParseDeclScope(ctx, Decl_Function, Key_EndProperty);
		PropagateError(new, new->scope);

		ConsumeKeywordOrSetError(Key_EndProperty, new);
	}

	return new;
}

static struct Papyrus_Syntax_ScriptHeader*
ParseScriptHeader(Ctx* ctx)
{
	struct Papyrus_Syntax_ScriptHeader* new = CreateSyntax(ScriptHeader);

	Consume(1);

	ConsumeNameOrSetError(new, &new->name);

	if (TryConsumeKeyword(Key_Extends))
	{
		new->base = ParseSymbol(ctx);
		PropagateError(new, new->base);
	}
	else new->base = NULL;

	uint32_t flagsmask =
		Papyrus_Syntax_DeclFlags_Native;

	if (!ParseFlags(ctx, flagsmask, 0, &new->syntax.eflags, &new->uflags))
		SetError(new);

	ConsumeLineAndSetError(new);

	return new;
}

static struct Papyrus_Syntax_State*
ParseState(Ctx* ctx, uint32_t flags)
{
	struct Papyrus_Syntax_State* new = CreateSyntax(State);

	ConsumeNameOrSetError(new, &new->name);

	new->scope = ParseDeclScope(ctx, Decl_Function | Decl_Event, Key_EndState);
	PropagateError(new, new->scope);

	ConsumeKeywordOrSetError(Key_EndState, new);

	return new;
};

static struct Papyrus_Syntax_Variable*
ParseVariable(Ctx* ctx, struct Papyrus_Syntax_Type* type)
{
	struct Papyrus_Syntax_Variable* new = CreateSyntax(Variable);

	new->type = type;
	PropagateError(new, type);

	ConsumeNameOrSetError(new, &new->name);

	if (Peek(0) == Tok_Assign)
	{
		Consume(1);

		new->expr = ParseExpr(ctx, Prec_Default);
		PropagateError(new, new->expr);
	}
	else new->expr = NULL;

	uint32_t flagsmask = 0;
	if (!ParseFlags(ctx, 0, 0, &new->syntax.eflags, &new->uflags))
		SetError(new);

	ConsumeLineAndSetError(new);

	return new;
}


static struct Papyrus_Syntax_Scope*
ParseScope(Ctx* ctx)
{
	struct Papyrus_Syntax_Scope* scope = CreateSyntax(Scope);

	Synbuf_Create();

	while (true)
	{
		struct Papyrus_Syntax* stmt;

		switch (Peek(0))
		{
		case Tok_Newline:
			Consume(1);
			continue;

		case Tok_Eof:
			goto exit;

		case Tok_Word:
			switch (GetKeyword(PeekString(0)))
			{
			case Key_Else:
			case Key_ElseIf:
			case Key_EndEvent:
			case Key_EndFunction:
			case Key_EndIf:
			case Key_EndWhile:
				goto exit;

			case Key_Return:
				{
					Consume(1);

					struct Papyrus_Syntax_ReturnStmt* new =
						CreateSyntax(ReturnStmt);

					if (!TryConsume(Tok_Newline))
					{
						new->expr = ParseExpr(ctx, Prec_Default);
						PropagateError(new, new->expr);

						ConsumeLineAndSetError(new);
					}
					else new->expr = NULL;

					stmt = &new->syntax;
				}
				break;

			case Key_If:
				{
					Consume(1);

					struct Papyrus_Syntax_IfStmt* new =
						CreateSyntax(IfStmt);

					Synbuf_Create();

					struct Papyrus_Syntax_IfClause* clause =
						CreateSyntax(IfClause);
					clause->cond = ParseExpr(ctx, Prec_Default);
					PropagateError(clause, clause->cond);

					ConsumeLineAndSetError(new);

					clause->scope = ParseScope(ctx);
					PropagateError(clause, clause->scope);

					Synbuf_Append(&clause);

				elif_reset:
					switch (GetKeyword(PeekString(0)))
					{
					case Key_ElseIf:
						{
							Consume(1);

							clause = CreateSyntax(IfClause);
							clause->cond = ParseExpr(ctx, Prec_Default);
							PropagateError(new, clause->cond);

							ConsumeLineAndSetError(new);

							clause->scope = ParseScope(ctx);
							PropagateError(clause, clause->scope);

							Synbuf_Append(&clause);
						}
						goto elif_reset;

					case Key_Else:
						{
							Consume(1);

							ConsumeLineAndSetError(new);

							new->elseScope = ParseScope(ctx);
							PropagateError(new, new->elseScope);

							ConsumeKeywordOrSetError(Key_EndIf, new);
						}
						break;

					case Key_EndIf:
						Consume(1);
						new->elseScope = NULL;
						ConsumeLineAndSetError(new);
						break;

					default:
						ReportError_ExpectedKeyword(ctx, Key_EndIf);
						SetError(&new->syntax);

						//TODO: improve error recovery
						break;
					}

					Synbuf_Commit(&new->clauses);

					stmt = &new->syntax;
				}
				break;

			case Key_While:
				{
					Consume(1);

					struct Papyrus_Syntax_WhileStmt* new =
						CreateSyntax(WhileStmt);

					new->cond = ParseExpr(ctx, Prec_Default);
					PropagateError(new, new->cond);

					ConsumeLineAndSetError(new);

					new->scope = ParseScope(ctx);
					PropagateError(new, new->scope);

					ConsumeKeywordOrSetError(Key_EndWhile, new);
					ConsumeLineAndSetError(new);

					stmt = &new->syntax;
				}
				break;

			case Key_Int:
			case Key_Bool:
			case Key_Float:
			case Key_String:
			parse_variable:
				{
					struct Papyrus_Syntax_Type* type = ParseType(ctx);
					stmt = &ParseVariable(ctx, type)->syntax;
				}
				break;

			case -1: // identifier
				switch (Peek(1))
				{
				case Tok_Word:
					if (IsKeyword(PeekString(1)))
						goto parse_expr;

				case Tok_Colon:
					goto parse_variable;
				}
				goto parse_expr;
			}
			break;

		default:
		parse_expr:
			{
				struct Papyrus_Syntax* expr = ParseExpr(ctx, Prec_Default);

				switch (Peek(0))
				{
					{
						uint32_t ekind;

				case Tok_Assign:
						ekind = Papyrus_Syntax_EKind_None;
						goto parse_assignment;

				case Tok_AddAssign:
						ekind = Papyrus_Syntax_BinaryExpr_Add;
						goto parse_assignment;

				case Tok_SubAssign:
						ekind = Papyrus_Syntax_BinaryExpr_Sub;
						goto parse_assignment;

				case Tok_MulAssign:
						ekind = Papyrus_Syntax_BinaryExpr_Mul;
						goto parse_assignment;

				case Tok_DivAssign:
						ekind = Papyrus_Syntax_BinaryExpr_Div;
						goto parse_assignment;

				case Tok_ModAssign:
						ekind = Papyrus_Syntax_BinaryExpr_Mod;
						goto parse_assignment;

					parse_assignment:
						Consume(1);

						struct Papyrus_Syntax_AssignStmt* new =
							CreateSyntax(AssignStmt);
						new->syntax.ekind = ekind;

						new->object = expr;
						PropagateError(new, expr);

						new->expr = ParseExpr(ctx, Prec_Default);
						PropagateError(new, new->expr);

						ConsumeLineAndSetError(new);

						stmt = &new->syntax;
					}
					break;

				default:
					{
						struct Papyrus_Syntax_ExprStmt* new = CreateSyntax(ExprStmt);

						new->expr = expr;
						PropagateError(new, expr);

						ConsumeLineAndSetError(new);

						stmt = &new->syntax;
					}
					break;
				}
			}
			break;
		}

		PropagateError(scope, stmt);
		Synbuf_Append(&stmt);
	}

exit:
	Synbuf_Commit(scope);

	return scope;
}

static struct Papyrus_Syntax_Scope*
ParseDeclScope(Ctx* ctx, uint32_t mask, uint32_t close)
{
	struct Papyrus_Syntax_Scope* new = CreateSyntax(Scope);

	Synbuf_Create();

#define CHECK(x, n) \
	if ((mask & Decl_ ## x) == 0) \
	{ \
		ReportError(ctx, n " is not allowed in the current context"); \
		SetError(new); \
	}

	while (true)
	{
		struct Papyrus_Syntax* decl;

		switch (Peek(0))
		{
		case Tok_Newline:
			Consume(1);
			continue;

		case Tok_Eof:
			goto exit;

		case Tok_Word:;
			uint32_t keyword = GetKeyword(PeekString(0));
			switch (keyword)
			{
			case Key_EndEvent:
			case Key_EndFunction:
			case Key_EndProperty:
			case Key_EndState:
				if (mask == -1) // root
				{
					ReportError_UnexpectedToken(ctx, NULL);
					Consume(1);
					continue;
				}
				else if (keyword != close)
				{
					//TODO: improve error recovery by inspecting indentation
					Consume(1);
					continue;
				}
				else goto exit;

			case Key_Event:
				CHECK(Event, "event declaration");
				decl = &ParseEvent(ctx)->syntax;
				break;

			case Key_Function:
				CHECK(Function, "function declaration");
				decl = &ParseFunction(ctx, NULL)->syntax;
				break;

			case Key_Import:
				{
					CHECK(Import, "import directive");

					Consume(1);

					struct Papyrus_Syntax_Import* new = CreateSyntax(Import);

					new->symbol = ParseSymbol(ctx);
					PropagateError(new, new->symbol);

					ConsumeLineAndSetError(new);

					decl = &new->syntax;
				}
				break;

			case Key_ScriptName:
				CHECK(ScriptHeader, "script header");
				decl = &ParseScriptHeader(ctx)->syntax;
				break;

				// state
				{
					uint32_t flags;

			case Key_Auto:
					Consume(1);
					flags = Papyrus_Syntax_DeclFlags_Auto;

					if (!TryConsumeKeyword(Key_State))
					{
						ReportError_ExpectedKeyword(ctx, Key_State);
						SetError(new);
						break;
					}

					if (false)
					{
			case Key_State:
						flags = 0;
					}

					CHECK(State, "state declaration");
					decl = &ParseState(ctx, flags)->syntax;
				}
				break;

			case Key_Int:
			case Key_Bool:
			case Key_Float:
			case Key_String:
			case -1: // identifier
				{
					struct Papyrus_Syntax_Type* type = ParseType(ctx);

					if (Peek(0) == Tok_Word)
					{
						switch (GetKeyword(PeekString(0)))
						{
						case Key_Function:
							CHECK(Function, "function declaration");
							decl = &ParseFunction(ctx, type)->syntax;
							break;

						case Key_Property:
							CHECK(Property, "property declaration");
							decl = &ParseProperty(ctx, type)->syntax;
							break;

						case -1: // identifier
							CHECK(Variable, "variable declaration");
							decl = &ParseVariable(ctx, type)->syntax;
							break;

						default:
							goto no_name;
						}
					}
					else
					{
					no_name:
						ReportError_UnexpectedToken(ctx,
							&Papyrus_String_CREATE("declaration"));
						SetError(new);

						ConsumeLine();
						continue;
					}
				}
				break;

			default:
				goto no_decl;
			}
			break;

		default:
		no_decl:
			ReportError_UnexpectedToken(ctx,
				&Papyrus_String_CREATE("declaration"));
			SetError(new);
			ConsumeLine();
			continue;
		}

		Synbuf_Append(&decl);
		PropagateError(new, decl);
	}
	
#undef CHECK

exit:
	Synbuf_Commit(new);

	return new;
}


static struct Papyrus_Syntax_Script*
ParseScript(Ctx* ctx)
{
	struct Papyrus_Syntax_Script* new = CreateSyntax(Script);

	new->scope = ParseDeclScope(ctx, -1, 0);
	PropagateError(new, new->scope);

	return new;
}


struct Papyrus_SyntaxTree*
Papyrus_Parse(struct Papyrus_String source,
	const struct Papyrus_ParserOptions* options)
{
	struct Papyrus_SyntaxTree* tree = ObjectGraph_CREATE(
		struct Papyrus_SyntaxTree, 4096, options->allocator);

	Ctx ctx;
	ctx.tree = tree;
	ctx.allocator = options->allocator;
	ctx.source = source.data;

	Arena_Init(&ctx.arena, options->pool);

	/* initialize lexer */ {
		uint32_t* buffer = (uint32_t*)options->lexerBuffer;

		uintptr_t bufferSize =
			options->lexerBufferSize / (3 * sizeof(uint32_t));

		ctx.lex.tokens = buffer;
		ctx.lex.offsets = buffer + bufferSize;
		ctx.lex.bufferSize = bufferSize;

		ctx.lex.index = 0;
		ctx.lex.count = 0;

		ulex_init(&ctx.lex.lexer);
		ulex_set_source(&ctx.lex.lexer, source.data, source.size);
	}

	ctx.diag = options->diag;

	ctx.cache = ObjectGraph_CreateCache(tree);
	tree->script = ParseScript(&ctx);
	ObjectGraph_Synchronize(tree, &ctx.cache);
	
	return tree;
}
