#pragma once

#include "Papyrus.h"

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


#define STRING_INIT(x) { .data = x, .size = sizeof(x) - 1 }
#define STRING(x) ((struct Papyrus_String) STRING_INIT(x))


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

/* 16: big enough for all keywords including null terminators
also makes SSE manipulation easy */
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

		struct SynBuf_Header* head;
	} synbuf;

	struct {
		void(*report)(void*, int32_t, int32_t, struct Papyrus_String);
		void* context;
	} errors;

	struct Papyrus_Allocator allocator;
	uintptr_t bufferSize;
};

typedef struct Papyrus_Parser Context;

static void*
AllocateSyntaxInternal(Context* ctx, uintptr_t size)
{
	const uintptr_t blockSize = 4 * 1024;

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
AllocateSyntax(Context* ctx, uintptr_t size)
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
CreateSyntax_(Context* ctx, uintptr_t size,
	uint32_t kind, struct Papyrus_Syntax** out)
{
	struct Papyrus_Syntax* syntax = AllocateSyntax(ctx, size);

	if (syntax == NULL)
		return Papyrus_Error_OutOfMemory;

	syntax->kind = kind;
	syntax->flags = 0;

	*out = syntax;
	return Papyrus_Error_None;
}

struct Papyrus_Syntax_Dummy
{
	struct Papyrus_Syntax syntax;
};

#define CreateSyntax(ctx, kind, out) \
	CreateSyntax_((ctx), sizeof(struct Papyrus_Syntax_ ## kind), \
		Papyrus_Syntax_ ## kind, (struct Papyrus_Syntax**)(out))

#define CreateSyntax2(ctx, type, kind, out) \
	CreateSyntax_((ctx), sizeof(struct Papyrus_Syntax_ ## type), \
		(kind), (struct Papyrus_Syntax**)(out))

static void*
CommitSyntax(Context* ctx, const void* data, uintptr_t size)
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


/*
the syntax buffers are a stack of temporary buffers for arbitrary syntax data.

the stack of buffers is built up in memory with the size of the previous buffer
placed in between two buffers. the parser holds the size of the topmost buffer
in the stack.

for the purpose of the stack, memory can be thought of as a contiguous block,
but in reality is implemented as a linked list of blocks. a buffer may span
more than one block, and one block may contain many buffers. the parser holds
pointers to the memory of the current block, which may not be the last block in
the list, as well as a pointer to the first block. the blocks are linked
together through the headers in front of each block.

an example of the memory layout of two buffers laid out in three blocks:
|-block-0 hdr-|----buffer-0 data----|-buffer-0 size-|----buffer-1 data----|
|-block-1 hdr-|------buffer-1 data------|-----------unused space----------|
|-block-2 hdr-|------------------------unused space-----------------------|
*/

struct SynBuf_Header
{
	// total size of the buffer in bytes, including header
	uintptr_t size;
	struct SynBuf_Header* prev;
	struct SynBuf_Header* next;
};

static Papyrus_Error
SynBuf_Expand(Context* ctx)
{
	struct SynBuf_Header* curHdr =
		(struct SynBuf_Header*)ctx->synbuf.beg - 1;

	struct SynBuf_Header* newHdr = curHdr->next;
	uintptr_t newSize;

	if (newHdr == NULL)
	{
		uintptr_t curSize = curHdr->size - sizeof(struct SynBuf_Header);
		uintptr_t maxSize = (1024 * 1024 - sizeof(struct SynBuf_Header)) & -8;
		newSize = MIN(curSize * 2, maxSize) + sizeof(struct SynBuf_Header);

		newHdr = ctx->allocator.allocate(ctx->allocator.context, newSize);
		if (newHdr == NULL) return Papyrus_Error_OutOfMemory;

		newHdr->size = newSize;
		newHdr->prev = curHdr;
		newHdr->next = NULL;

		curHdr->next = newHdr;
	}
	else newSize = newHdr->size;

	char* beg = (char*)(newHdr + 1);
	ctx->synbuf.beg = beg;
	ctx->synbuf.cur = beg;
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

	uint32_t* tokens = ulex_get_tokens(lexer);
	uint32_t* offsets = ulex_get_offsets(lexer);

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


struct LineColumn
{
	int32_t line;
	int32_t column;
};

static struct LineColumn
GetTokenLineColumn(Context* ctx)
{
	const char* source = ctx->source;
	intptr_t offset = ctx->lex.offsets[ctx->lex.index - 1];

	int32_t line = 0;
	intptr_t lineOffset = 0;

	for (intptr_t i = 0; i < offset; ++i)
	{
		bool newline = source[i] == '\n';
		line += newline;
		lineOffset = newline ? i + 1: lineOffset;
	}

	return (struct LineColumn) { .line = (int32_t)(line + 1),
		.column = (int32_t)(offset - lineOffset + 1) };
}

static void
ReportError(Context* ctx, const char* fmt, ...)
{
	if (ctx->errors.report == NULL)
		return;

	struct LineColumn pos = GetTokenLineColumn(ctx);

	char buffer[256];
	intptr_t size;

	{
		va_list vlist;
		va_start(vlist, fmt);
		size = vsnprintf(buffer, sizeof(buffer), fmt, vlist);
		va_end(vlist);
	}

	ctx->errors.report(ctx->errors.context, pos.line, pos.column,
		(struct Papyrus_String) { .data = buffer, .size = size });
}

static void
ReportError_UnexpectedToken(
	Context* ctx, const struct Papyrus_String* expected)
{
	struct Papyrus_String tokenString = PeekString(ctx);

	intptr_t length;
	const char* suffix;

	switch (Peek(ctx))
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
			(int)length, tokenString.data, suffix, (int)expected->size, expected->data);
	}
	else
	{
		ReportError(ctx, "unexpected token '%.*s%s",
			(int)length, tokenString.data, suffix);
	}
}

static void
ReportError_ExpectedToken(Context* ctx, uint32_t token)
{
	struct Papyrus_String string;

	switch (token)
	{
#define TOKEN_STRING(tok, str) \
	case tok: string = STRING(str); break;

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
ReportError_ExpectedKeyword(Context* ctx, uint32_t keyword)
{
	char string[18];

	memcpy(string + 1, Keyword_Strings[keyword], 16);
	intptr_t size = strlen(string + 1);

	string[0] = '\'';
	string[size] = '\'';

	ReportError_UnexpectedToken(ctx,
		&(struct Papyrus_String){ string, size + 2 });
}



enum
{
	SyntaxError = -1,
};

static const struct Papyrus_String ErrorString = STRING_INIT("<error>");

static inline void
SetError(struct Papyrus_Syntax* syntax)
{
	syntax->flags |= Papyrus_SyntaxFlags_Error;
}

static inline Papyrus_Error
IgnoreSyntaxError(Papyrus_Error error, struct Papyrus_Syntax* syntax)
{
	if (error != SyntaxError)
		return error;

	SetError(syntax);
	return Papyrus_Error_None;
}

#define IgnoreSyntaxError(error, syntax) \
	IgnoreSyntaxError((error), (struct Papyrus_Syntax*)(syntax))

static inline void
PropagateError(struct Papyrus_Syntax* parent, const struct Papyrus_Syntax* child)
{
	parent->flags |= child->flags & Papyrus_SyntaxFlags_Error;
}

#define PropagateError(parent, child) \
	PropagateError((struct Papyrus_Syntax*)(parent), (struct Papyrus_Syntax*)(child))

static inline bool
TryConsume(Context* ctx, uint32_t token)
{
	if (Peek(ctx) != token)
		return false;

	Consume(ctx);
	return true;
}

static inline bool
ConsumeOrSetError(Context* ctx, uint32_t token, struct Papyrus_Syntax* syntax)
{
	if (TryConsume(ctx, token))
		return true;

	SetError(syntax);
	ReportError_ExpectedToken(ctx, token);
	return false;
}

#define ConsumeOrSetError(ctx, token, syntax) \
	ConsumeOrSetError((ctx), (token), (struct Papyrus_Syntax*)(syntax))

static inline bool
TryConsumeKeyword(Context* ctx, uint32_t keyword)
{
	if (Peek(ctx) != Tok_Word)
		return false;

	if (!IsSpecificKeyword(PeekString(ctx), keyword))
		return false;

	Consume(ctx);
	return true;
}

static inline bool
ConsumeKeywordOrSetError(Context* ctx, uint32_t keyword, struct Papyrus_Syntax* syntax)
{
	if (TryConsumeKeyword(ctx, keyword))
		return true;

	SetError(syntax);
	ReportError_ExpectedKeyword(ctx, keyword);
	return false;
}

#define ConsumeKeywordOrSetError(ctx, keyword, syntax) \
	ConsumeKeywordOrSetError((ctx), (keyword), (struct Papyrus_Syntax*)(syntax))

static inline bool
TryConsumeName(Context* ctx, struct Papyrus_String* out)
{
	struct Papyrus_String string;

	if (Peek(ctx) == Tok_Word &&
		!IsKeyword(string = PeekString(ctx)))
	{
		Consume(ctx);
		*out = string;
		return true;
	}

	*out = ErrorString;
	return false;
}

static inline bool
ConsumeNameOrSetError(Context* ctx, struct Papyrus_Syntax* syntax, struct Papyrus_String* out)
{
	if (TryConsumeName(ctx, out))
		return true;

	SetError(syntax);
	ReportError_UnexpectedToken(ctx, &STRING("identifier"));
	return false;
}

#define ConsumeNameOrSetError(ctx, syntax, out) \
	ConsumeNameOrSetError((ctx), (struct Papyrus_Syntax*)(syntax), (out))

static inline void
ConsumeLineAndSetError(Context* ctx, struct Papyrus_Syntax* syntax)
{
	if (Peek(ctx) != Tok_Newline)
	{
		SetError(syntax);

		do
		{
			ReportError_ExpectedToken(ctx, Tok_Newline);
			Consume(ctx);
		} while (Peek(ctx) != Tok_Newline);
	}
	Consume(ctx);
}

#define ConsumeLineAndSetError(ctx, syntax) \
	ConsumeLineAndSetError((ctx), (struct Papyrus_Syntax*)(syntax))


static Papyrus_Error
ParseExpr(Context* ctx, uint32_t prec, const struct Papyrus_Syntax** out);

static Papyrus_Error
ParseStmt(Context* ctx, const struct Papyrus_Syntax** out);

static Papyrus_Error
ParseType(Context* ctx, const struct Papyrus_Syntax_Type** out);

static Papyrus_Error // may return SyntaxError
ParseDefs(Context* ctx, uint32_t mask, struct Papyrus_SyntaxArray* out);


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

static Papyrus_Error // may return SyntaxError
ParseFullName(Context* ctx, const struct Papyrus_String* firstPart, struct Papyrus_FullName* out)
{
	Papyrus_Error error;
	Papyrus_Error result = 0;

	if (error = SynBuf_Create(ctx))
		return error;

	struct Papyrus_String string;

	if (firstPart == NULL)
	{
		if (!TryConsumeName(ctx, &string))
			result = SyntaxError;
	}
	else string = *firstPart;

	while (true)
	{
		if (error = SynBuf_Append(ctx, &string, sizeof(string)))
			return error;

		if (!TryConsume(ctx, Tok_Colon))
			break;

		if (!TryConsumeName(ctx, &string))
			result = SyntaxError;
	}

	if (error = SynBuf_Commit(ctx, &out->parts.data, &out->parts.size))
		return error;

	return result;
}

static Papyrus_Error // may return SyntaxError
ParseFlags(Context* ctx, uint32_t mask, uint32_t flags, uint32_t* out)
{
reset:
	if (Peek(ctx) == Tok_Word)
	{
		switch (GetKeyword(PeekString(ctx)))
		{
			uint32_t flag;

		case Key_Auto:
			flag = Papyrus_DeclFlags_Auto;
			goto got_flag;

		case Key_BetaOnly:
			flag = Papyrus_DeclFlags_BetaOnly;
			goto got_flag;

	//	case Key_Conditional:
	//		flag = Papyrus_DeclFlags_Conditional;
	//		goto got_flag;

		case Key_Const:
			flag = Papyrus_DeclFlags_Const;
			goto got_flag;

		case Key_DebugOnly:
			flag = Papyrus_DeclFlags_DebugOnly;
			goto got_flag;

	//	case Key_Default:
	//		flag = Papyrus_DeclFlags_Default;
	//		goto got_flag;

		case Key_Global:
			flag = Papyrus_DeclFlags_Global;
			goto got_flag;

	//	case Key_Hidden:
	//		flag = Papyrus_DeclFlags_Hidden;
	//		goto got_flag;

		case Key_Native:
			flag = Papyrus_DeclFlags_Native;
			goto got_flag;

		got_flag:
			if ((mask & flag) == 0)
				break;

			if (flags & flag)
				return SyntaxError;

			flags |= flag;

			Consume(ctx);
			goto reset;
		}
	}

	*out = flags;
	return Papyrus_Error_None;
}

static Papyrus_Error // may return SyntaxError
ParseExprList(Context* ctx, struct Papyrus_SyntaxArray* out)
{
	Papyrus_Error error;
	Papyrus_Error result = 0;

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

			if (expr->flags & Papyrus_SyntaxFlags_Error)
				result = SyntaxError;

			if (TryConsume(ctx, Tok_Comma))
				goto reset;
		}
	}

	if (error = SynBuf_Commit(ctx, &out->data, &out->size))
		return error;

	return result;
}

static Papyrus_Error // may return SyntaxError
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

			struct Papyrus_Syntax_UnaryExpr* new;
			if (error = CreateSyntax2(ctx, UnaryExpr, opKind, &new))
				return error;

			if (error = ParseExpr(ctx, Prec_Unary, &new->expr))
				return error;
			PropagateError(new, new->expr);

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

				struct Papyrus_Syntax_NewExpr* new;
				if (error = CreateSyntax(ctx, NewExpr, &new))
					return error;

				if (error = ParseFullName(ctx, NULL, &new->name))
					if (error = IgnoreSyntaxError(error, new))
						return error;

				if (ConsumeOrSetError(ctx, Tok_LBrack, new))
				{
					if (error = ParseExpr(ctx, Prec_Default, &new->extent))
						return error;
					PropagateError(new, new->extent);

					ConsumeOrSetError(ctx, Tok_RBrack, new);
				}

				expr = &new->syntax;
			}
			break;

		case Key_None:
			{
				Consume(ctx);

				if (error = CreateSyntax2(ctx, Dummy, Papyrus_Syntax_NoneExpr, &expr))
					return error;
			}
			break;

			{
				uint32_t kind;

		case Key_True:
				kind = Papyrus_Syntax_BoolExpr_True;

				if (false)
				{
		case Key_False:
					kind = Papyrus_Syntax_BoolExpr_False;
				}

				if (error = CreateSyntax2(ctx, Dummy, kind, &expr))
					return error;
			}
			break;

		default:
		case -1: // identifier
			{
				struct Papyrus_Syntax_NameExpr* new;
				if (error = CreateSyntax(ctx, NameExpr, &new))
					return error;

				if (error = ParseFullName(ctx, NULL, &new->name))
					if (error = IgnoreSyntaxError(error, new))
						return error;

				expr = &new->syntax;
			}
			break;
		}
		goto postfix;

		{
			uint32_t kind;

	case Tok_IntDec:
	case Tok_IntHex:
			kind = Papyrus_Syntax_IntExpr;

			if (false)
			{
	case Tok_Float:
				kind = Papyrus_Syntax_FloatExpr;
			}
			
			if (false)
			{
	case Tok_String:
				kind = Papyrus_Syntax_StringExpr;
			}

			struct Papyrus_Syntax_StringExpr* new;
			if (error = CreateSyntax2(ctx, StringExpr, Papyrus_Syntax_IntExpr, &new))
				return error;

			new->string = PeekString(ctx);
			expr = &new->syntax;

			Consume(ctx);
		}
		goto postfix;

	case Tok_LParen:
		{
			Consume(ctx);

			if (error = ParseExpr(ctx, Prec_Default, &expr))
				return error;

			ConsumeOrSetError(ctx, Tok_RParen, expr);
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

					struct Papyrus_Syntax_InvokeExpr* new;
					if (error = CreateSyntax2(ctx, InvokeExpr, opKind, &new))
						return error;

					if (error = ParseExprList(ctx, &new->args))
						if (error = IgnoreSyntaxError(error, new))
							return error;

					ConsumeOrSetError(ctx, close, new);

					new->expr = expr;
					expr = &new->syntax;
				}
				continue;

			case Tok_Dot:
				{
					Consume(ctx);

					struct Papyrus_Syntax_AccessExpr* new;
					if (error = CreateSyntax(ctx, AccessExpr, &new))
						return error;

					ConsumeNameOrSetError(ctx, new, &new->name);

					new->expr = expr;
					expr = &new->syntax;
				}
				continue;
			}
			break;
		}
		break;

	default:
		{
			struct Papyrus_Syntax_NameExpr* new;
			if (error = CreateSyntax(ctx, NameExpr, &new))
				return error;

			new->name.parts.data = &ErrorString;
			new->name.parts.size = 1;

			expr = &new->syntax;
		}
		goto exit;
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

		struct Papyrus_Syntax_BinaryExpr* bin;
		if (error = CreateSyntax2(ctx, BinaryExpr, opKind, &bin))
			return error;
		PropagateError(bin, expr);

		struct Papyrus_Syntax* right;
		if (error = ParseExpr(ctx, prec, &right))
			return error;
		PropagateError(bin, right);

		bin->left = expr;
		bin->right = right;

		expr = &bin->syntax;
	}

exit:
	*out = expr;
	return Papyrus_Error_None;
}

static Papyrus_Error // may return SyntaxError
ParseScope(Context* ctx, struct Papyrus_SyntaxArray* out)
{
	Papyrus_Error error;
	Papyrus_Error result = 0;

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

		default:
			{
				struct Papyrus_Syntax* stmt;
				if (error = ParseStmt(ctx, &stmt))
					return error;

				if (stmt->flags & Papyrus_SyntaxFlags_Error)
					result = SyntaxError;

				if (error = SynBuf_Append(ctx, &stmt, sizeof(stmt)))
					return error;
			}
			break;
		}
	}
exit:

	if (error = SynBuf_Commit(ctx, &out->data, &out->size))
		return error;

	return result;
}

static Papyrus_Error // may return SyntaxError
ParseParams(Context* ctx, struct Papyrus_Params* out)
{
	Papyrus_Error error;
	Papyrus_Error result = 0;

	if (error = SynBuf_Create(ctx))
		return error;

	if (Peek(ctx) != Tok_RParen)
	{
	reset:;
		struct Papyrus_Param param;

		if (error = ParseType(ctx, &param.type))
			return error;

		if (param.type->flags & Papyrus_SyntaxFlags_Error)
			result = SyntaxError;

		if (!TryConsumeName(ctx, &param.name))
			result = SyntaxError;

		if (TryConsume(ctx, Tok_Assign))
		{
			if (error = ParseExpr(ctx, Prec_Default, &param.expr))
				return error;

			if (param.expr->flags & Papyrus_SyntaxFlags_Error)
				result = SyntaxError;
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

	return result;
}

static Papyrus_Error
ParseType(Context* ctx, const struct Papyrus_Syntax_Type** out)
{
	static const struct Papyrus_String VarString = STRING_INIT("var");
	static const struct Papyrus_String IntString = STRING_INIT("int");
	static const struct Papyrus_String BoolString = STRING_INIT("bool");
	static const struct Papyrus_String FloatString = STRING_INIT("float");
	static const struct Papyrus_String StringString = STRING_INIT("string");

	Papyrus_Error error;

	struct Papyrus_Syntax_Type* type;
	if (error = CreateSyntax(ctx, Type, &type))
		return error;

	type->flags = 0;

	if (Peek(ctx) == Tok_Word)
	{
		switch (GetKeyword(PeekString(ctx)))
		{
		case Key_Var: type->name.parts.data = &VarString; goto keyword_type;
		case Key_Int: type->name.parts.data = &IntString; goto keyword_type;
		case Key_Bool: type->name.parts.data = &BoolString; goto keyword_type;
		case Key_Float: type->name.parts.data = &FloatString; goto keyword_type;
		case Key_String: type->name.parts.data = &StringString; goto keyword_type;

		keyword_type:
			type->name.parts.size = 1;
			Consume(ctx);
			break;

		default:
			goto name_error;

		case -1: // identifier
			if (error = ParseFullName(ctx, NULL, &type->name))
				if (error = IgnoreSyntaxError(error, type))
					return error;
			break;
		}
	}
	else
	{
	name_error:
		type->name.parts.data = &ErrorString;
		type->name.parts.size = 1;
	}

	if (TryConsume(ctx, Tok_LBrack))
	{
		type->flags |= Papyrus_TypeFlags_Array;
		ConsumeOrSetError(ctx, Tok_RBrack, type);
	}

	*out = type;
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

				struct Papyrus_Syntax_ReturnStmt* stmt;
				if (error = CreateSyntax(ctx, ReturnStmt, &stmt))
					return error;

				if (!TryConsume(ctx, Tok_Newline))
				{
					if (error = ParseExpr(ctx, Prec_Default, &stmt->expr))
						return error;
					PropagateError(stmt, stmt->expr);

					ConsumeLineAndSetError(ctx, stmt);
				}
				else stmt->expr = NULL;

				*out = &stmt->syntax;
				return Papyrus_Error_None;
			}

		case Key_If:
			{
				Consume(ctx);

				struct Papyrus_Syntax_IfStmt* stmt;
				if (error = CreateSyntax(ctx, IfStmt, &stmt))
					return error;

				if (error = ParseExpr(ctx, Prec_Default, &stmt->cond))
					return error;
				PropagateError(stmt, stmt->cond);

				ConsumeLineAndSetError(ctx, stmt);

				if (error = ParseScope(ctx, &stmt->scope))
					if (error = IgnoreSyntaxError(error, stmt))
						return error;

				if (error = SynBuf_Create(ctx))
					return error;

				stmt->else_ = NULL;

			elif_reset:
				switch (GetKeyword(PeekString(ctx)))
				{
				case Key_ElseIf:
					{
						Consume(ctx);

						struct Papyrus_Syntax_ElseIfClause* elif;
						if (error = CreateSyntax(ctx, ElseIfClause, &elif))
							return error;

						if (error = ParseExpr(ctx, Prec_Default, &elif->cond))
							return error;
						PropagateError(elif, elif->cond);

						ConsumeLineAndSetError(ctx, elif);

						if (error = ParseScope(ctx, &elif->scope))
							if (error = IgnoreSyntaxError(error, elif))
								return error;

						if (error = SynBuf_Append(ctx, &elif, sizeof(elif)))
							return error;

						goto elif_reset;
					}
					break;

				case Key_Else:
					{
						Consume(ctx);

						struct Papyrus_Syntax_ElseClause* else_;
						if (error = CreateSyntax(ctx, ElseClause, &else_))
							return error;

						ConsumeLineAndSetError(ctx, else_);

						if (error = ParseScope(ctx, &else_->scope))
							if (error = IgnoreSyntaxError(error, else_))
								return error;

						ConsumeKeywordOrSetError(ctx, Key_EndIf, stmt);
					}
					break;

				case Key_EndIf:
					Consume(ctx);
					ConsumeLineAndSetError(ctx, stmt);
					break;

				default:
					SetError(&stmt->syntax);
					ReportError_ExpectedKeyword(ctx, Key_EndIf);
					break;
				}

				if (error = SynBuf_Commit(ctx, &stmt->elifs.data, &stmt->elifs.size))
					return error;

				*out = &stmt->syntax;
				return Papyrus_Error_None;
			}

		case Key_While:
			{
				Consume(ctx);

				struct Papyrus_Syntax_WhileStmt* stmt;
				if (error = CreateSyntax(ctx, WhileStmt, &stmt))
					return error;

				if (error = ParseExpr(ctx, Prec_Default, &stmt->cond))
					return error;
				PropagateError(stmt, stmt->cond);

				ConsumeLineAndSetError(ctx, stmt);

				if (error = ParseScope(ctx, &stmt->scope))
					if (error = IgnoreSyntaxError(error, stmt))
						return error;

				ConsumeKeywordOrSetError(ctx, Key_EndWhile, stmt);
				ConsumeLineAndSetError(ctx, stmt);

				*out = &stmt->syntax;
				return Papyrus_Error_None;
			}
		}

	default:
		{
			struct Papyrus_Syntax_ExprStmt* stmt;
			if (error = CreateSyntax(ctx, ExprStmt, &stmt))
				return error;

			if (error = ParseExpr(ctx, Prec_Default, &stmt->expr))
				return error;
			PropagateError(stmt, stmt->expr);

			ConsumeLineAndSetError(ctx, stmt);

			*out = &stmt->syntax;
			return Papyrus_Error_None;
		}
	}
}

static Papyrus_Error
ParseState(Context* ctx, uint32_t flags, const struct Papyrus_Syntax_State** out)
{
	Papyrus_Error error;

	struct Papyrus_Syntax_State* state;
	if (error = CreateSyntax(ctx, State, &state))
		return error;

	ConsumeNameOrSetError(ctx, state, &state->name);

	if (error = ParseDefs(ctx, Def_Function | Def_Event, &state->defs))
		if (error = IgnoreSyntaxError(error, state))
			return error;

	ConsumeKeywordOrSetError(ctx, Key_EndState, state);

	*out = state;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseFunction(Context* ctx, struct Papyrus_Syntax_Type* type, const struct Papyrus_Syntax_Function** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_Syntax_Function* function;
	if (error = CreateSyntax(ctx, Function, &function))
		return error;

	ConsumeNameOrSetError(ctx, function, &function->name);

	if (ConsumeOrSetError(ctx, Tok_LParen, function))
	{
		if (error = ParseParams(ctx, &function->params))
			if (error = IgnoreSyntaxError(error, function))
				return error;

		ConsumeOrSetError(ctx, Tok_RParen, function);
	}

	uint32_t flagsmask =
		Papyrus_DeclFlags_BetaOnly |
		Papyrus_DeclFlags_DebugOnly |
		Papyrus_DeclFlags_Global |
		Papyrus_DeclFlags_Native;

	if (error = ParseFlags(ctx, flagsmask, 0, &function->flags))
		if (error = IgnoreSyntaxError(error, function))
			return error;

	ConsumeLineAndSetError(ctx, function);

	if ((function->flags & Papyrus_DeclFlags_Native) == 0)
	{
		if (error = ParseScope(ctx, &function->scope))
			if (error = IgnoreSyntaxError(error, function))
				return error;

		ConsumeKeywordOrSetError(ctx, Key_EndFunction, function);
		ConsumeLineAndSetError(ctx, function);
	}
	else function->scope.size = 0;

	function->type = type;
	if (type) PropagateError(function, type);

	*out = function;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseVariable(Context* ctx, struct Papyrus_Syntax_Type* type, const struct Papyrus_Syntax_Variable** out)
{
	Papyrus_Error error;

	struct Papyrus_Syntax_Variable* variable;
	if (error = CreateSyntax(ctx, Variable, &variable))
		return error;

	ConsumeNameOrSetError(ctx, variable, &variable->name);

	if (Peek(ctx) == Tok_Assign)
	{
		Consume(ctx);

		if (error = ParseExpr(ctx, Prec_Default, &variable->expr))
			return error;
		PropagateError(variable, variable->expr);
	}
	else variable->expr = NULL;

	uint32_t flagsmask =
		Papyrus_DeclFlags_Conditional |
		Papyrus_DeclFlags_Const |
		Papyrus_DeclFlags_Hidden;

	if (error = ParseFlags(ctx, flagsmask, 0, &variable->flags))
		if (error = IgnoreSyntaxError(error, variable))
			return error;

	ConsumeLineAndSetError(ctx, variable);

	variable->type = type;
	PropagateError(variable, type);

	*out = variable;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseProperty(Context* ctx, struct Papyrus_Syntax_Type* type, const struct Papyrus_Syntax_Property** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_Syntax_Property* property;
	if (error = CreateSyntax(ctx, Property, &property))
		return error;

	ConsumeNameOrSetError(ctx, property, &property->name);

	uint32_t flagsmask =
		Papyrus_DeclFlags_Auto |
		Papyrus_DeclFlags_Conditional |
		Papyrus_DeclFlags_Const |
		Papyrus_DeclFlags_Hidden |
		Papyrus_DeclFlags_Mandatory;

	if (error = ParseFlags(ctx, flagsmask, 0, &property->flags))
		if (error = IgnoreSyntaxError(error, property))
			return error;

	ConsumeLineAndSetError(ctx, property);

	struct Papyrus_Syntax_Function* get = NULL;
	struct Papyrus_Syntax_Function* set = NULL;

	if ((property->flags & Papyrus_DeclFlags_Auto) == 0)
	{
		while (true)
		{
			struct Papyrus_Syntax_Type* functype;

			switch (Peek(ctx))
			{
			case Tok_Newline:
				Consume(ctx);
				continue;

			case Tok_Word:
				switch (GetKeyword(PeekString(ctx)))
				{
				case Key_EndProperty:
					Consume(ctx);
					ConsumeLineAndSetError(ctx, property);
					goto exit;

				case Key_Function:
					functype = NULL;
					goto no_type;
				}

			default:
				if (error = ParseType(ctx, &functype))
					return error;

			no_type:;
				struct Papyrus_Syntax_Function* function;
				if (error = ParseFunction(ctx, functype, &function))
					return error;
				PropagateError(property, function);

				struct Papyrus_String funcname = function->name;

				switch (funcname.data[0])
				{
				case 'G':
				case 'g':
					if (get == NULL) get = function;
					else goto accessor_name_error;
					goto got_func;

				case 'S':
				case 's':
					if (set == NULL) set = function;
					else goto accessor_name_error;
					goto got_func;

				got_func:
					if (funcname.size != 3)
						goto accessor_name_error;

					char c;
					if ((c = funcname.data[1]) != 'E' && c != 'e')
						goto accessor_name_error;
					if ((c = funcname.data[2]) != 'T' && c != 't')
						goto accessor_name_error;

					break;

				default:
				accessor_name_error:
					SetError(&property->syntax);
					ReportError(ctx, "invalid property accessor '%.*s'",
						(int)funcname.size, funcname.data);
				}
			}
		}
	exit:;
	}

	property->type = type;
	PropagateError(property, type);

	property->get = get;
	property->set = set;

	*out = property;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseGroup(Context* ctx, const struct Papyrus_Syntax_Group** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_Syntax_Group* group;
	if (error = CreateSyntax(ctx, Group, &group))
		return error;

	ConsumeNameOrSetError(ctx, group, &group->name);

	uint32_t flagsmask =
		Papyrus_DeclFlags_Conditional |
		Papyrus_DeclFlags_Const |
		Papyrus_DeclFlags_Hidden;

	if (error = ParseFlags(ctx, flagsmask, 0, &group->flags))
		if (error = IgnoreSyntaxError(error, group))
			return error;

	ConsumeLineAndSetError(ctx, group);

	if (error = SynBuf_Create(ctx))
		return error;

	while (true)
	{
		if (TryConsumeKeyword(ctx, Key_EndGroup))
			break;

		struct Papyrus_Syntax_Type* type;
		if (error = ParseType(ctx, &type))
			return error;

		struct Papyrus_Syntax_Property* prop;
		if (error = ParseProperty(ctx, type, &prop))
			return error;
		PropagateError(group, prop);

		if (error = SynBuf_Append(ctx, &prop, sizeof(prop)))
			return error;
	}

	ConsumeLineAndSetError(ctx, group);

	if (error = SynBuf_Commit(ctx, &group->props.data, &group->props.size))
		return error;

	*out = group;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseEvent(Context* ctx, const struct Papyrus_Syntax_Event** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_Syntax_Event* event;
	if (error = CreateSyntax(ctx, Event, &event))
		return error;

	if (ConsumeNameOrSetError(ctx, event, &event->name))
	{
		switch (Peek(ctx))
		{
		case Tok_Colon:
			Consume(ctx);
			if (error = ParseFullName(ctx, &event->name, &event->typename))
				if (error = IgnoreSyntaxError(error, event))
					return error;

			if (!ConsumeOrSetError(ctx, Tok_Dot, event))
			{
				event->name = ErrorString;
				break;
			}

			if (false)
			{
		case Tok_Dot:
				Consume(ctx);
				event->typename.parts.size = 0;
			}

			ConsumeNameOrSetError(ctx, event, &event->name);
			break;

		default:
			event->typename.parts.size = 0;
			break;
		}
	}
	else event->typename.parts.size = 0;

	if (ConsumeOrSetError(ctx, Tok_LParen, event))
	{
		if (error = ParseParams(ctx, &event->params))
			if (error = IgnoreSyntaxError(error, event))
				return error;

		ConsumeOrSetError(ctx, Tok_RParen, event);
	}
	else event->params.size = 0;

	uint32_t flagsmask = Papyrus_DeclFlags_Native;

	if (error = ParseFlags(ctx, flagsmask, 0, &event->flags))
		if (error = IgnoreSyntaxError(error, event))
			return error;

	ConsumeLineAndSetError(ctx, event);

	if ((event->flags & Papyrus_DeclFlags_Native) == 0)
	{
		if (error = ParseScope(ctx, &event->scope))
			if (error = IgnoreSyntaxError(error, event))
				return error;

		ConsumeKeywordOrSetError(ctx, Key_EndEvent, event);
		ConsumeLineAndSetError(ctx, event);
	}
	else event->scope.size = 0;

	*out = event;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseStruct(Context* ctx, const struct Papyrus_Syntax_Struct** out)
{
	Papyrus_Error error;

	Consume(ctx);

	struct Papyrus_Syntax_Struct* struct_;
	if (error = CreateSyntax(ctx, Struct, &struct_))
		return error;

	ConsumeNameOrSetError(ctx, struct_, &struct_->name);
	ConsumeLineAndSetError(ctx, struct_);

	if (error = ParseDefs(ctx, Def_Variable, &struct_->vars))
		if (error = IgnoreSyntaxError(error, struct_))
			return error;

	ConsumeKeywordOrSetError(ctx, Key_EndStruct, struct_);
	ConsumeLineAndSetError(ctx, struct_);

	*out = struct_;
	return Papyrus_Error_None;
}

static Papyrus_Error
ParseScript(Context* ctx, const struct Papyrus_Syntax_Script** out)
{
	Papyrus_Error error;

	struct Papyrus_Syntax_Script* script;
	if (error = CreateSyntax(ctx, Script, &script))
		return error;

	if (ConsumeKeywordOrSetError(ctx, Key_ScriptName, script))
	{
		ConsumeNameOrSetError(ctx, script, &script->name);

		if (TryConsumeKeyword(ctx, Key_Extends))
		{
			if (error = ParseFullName(ctx, NULL, &script->base))
				if (error = IgnoreSyntaxError(error, script))
					return error;
		}
		else script->base.parts.size = 0;

		uint32_t flagsmask =
			Papyrus_DeclFlags_Native |
			Papyrus_DeclFlags_Conditional |
			Papyrus_DeclFlags_Const |
			Papyrus_DeclFlags_DebugOnly |
			Papyrus_DeclFlags_BetaOnly |
			Papyrus_DeclFlags_Hidden |
			Papyrus_DeclFlags_Default;

		if (error = ParseFlags(ctx, flagsmask, 0, &script->flags))
			if (error = IgnoreSyntaxError(error, script))
				return error;

		ConsumeLineAndSetError(ctx, script);
	}
	else
	{
		script->name = ErrorString;
		script->base.parts.size = 0;
		script->flags = 0;
	}

	if (error = ParseDefs(ctx, -1, &script->defs))
		if (error = IgnoreSyntaxError(error, script))
			return error;

	*out = script;
	return Papyrus_Error_None;
}

static Papyrus_Error // may return SyntaxError
ParseDefs(Context* ctx, uint32_t mask, struct Papyrus_SyntaxArray* out)
{
	Papyrus_Error error;
	Papyrus_Error result = 0;

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
			case Key_Import:
				{
					if ((mask & Def_Import) == 0)
						result = SyntaxError;

					Consume(ctx);

					struct Papyrus_Syntax_Import* import;
					if (error = CreateSyntax(ctx, Import, &import))
						return error;

					if (error = ParseFullName(ctx, NULL, &import->name))
						if (error = IgnoreSyntaxError(error, import))
							return error;

					ConsumeLineAndSetError(ctx, import);

					if (error = SynBuf_Append(ctx, &import, sizeof(import)))
						return error;
				}
				break;
		
			case Key_Event:
				{
					if ((mask & Def_Event) == 0)
						result = SyntaxError;

					struct Papyrus_Syntax_Event* event;
					if (error = ParseEvent(ctx, &event))
						return error;

					if (event->syntax.flags & Papyrus_SyntaxFlags_Error)
						result = SyntaxError;

					if (error = SynBuf_Append(ctx, &event, sizeof(event)))
						return error;
				}
				break;

				{
					uint32_t flags;

			case Key_Auto:
					Consume(ctx);
					flags = Papyrus_DeclFlags_Auto;

					if (!TryConsumeKeyword(ctx, Key_State))
					{
						result = SyntaxError;
						break;
					}

					if (false)
					{
			case Key_State:
						flags = 0;
					}

					if ((mask & Def_State) == 0)
						result = SyntaxError;

					struct Papyrus_Syntax_State* state;
					if (error = ParseState(ctx, flags, &state))
						return error;

					if (state->syntax.flags & Papyrus_SyntaxFlags_Error)
						result = SyntaxError;

					if (error = SynBuf_Append(ctx, &state, sizeof(state)))
						return error;
				}
				break;

			case Key_Group:
				{
					if ((mask & Def_Group) == 0)
						result = SyntaxError;

					struct Papyrus_Syntax_Group* group;
					if (error = ParseGroup(ctx, &group))
						return error;

					if (group->syntax.flags & Papyrus_SyntaxFlags_Error)
						result = SyntaxError;

					if (error = SynBuf_Append(ctx, &group, sizeof(group)))
						return error;
				}
				break;

			case Key_Function:
				{
					if ((mask & Def_Function) == 0)
						result = SyntaxError;

					struct Papyrus_Syntax_Function* function;
					if (error = ParseFunction(ctx, NULL, &function))
						return error;

					if (function->syntax.flags & Papyrus_SyntaxFlags_Error)
						result = SyntaxError;

					if (error = SynBuf_Append(ctx, &function, sizeof(function)))
						return error;
				}
				break;

			case Key_Struct:
				{
					if ((mask & Def_Struct) == 0)
						result = SyntaxError;

					struct Papyrus_Syntax_Struct* struct_;
					if (error = ParseStruct(ctx, &struct_))
						return error;

					if (struct_->syntax.flags & Papyrus_SyntaxFlags_Error)
						result = SyntaxError;

					if (error = SynBuf_Append(ctx, &struct_, sizeof(struct_)))
						return error;
				}
				break;

			case Key_Int:
			case Key_Bool:
			case Key_Float:
			case Key_String:
			case -1: // identifier
				{
					struct Papyrus_Syntax_Type* type;
					if (error = ParseType(ctx, &type))
						return error;

					if (Peek(ctx) == Tok_Word)
					{
						switch (GetKeyword(PeekString(ctx)))
						{
						case Key_Function:
							{
								if ((mask & Def_Function) == 0)
									result = SyntaxError;

								struct Papyrus_Syntax_Function* function;
								if (error = ParseFunction(ctx, type, &function))
									return error;

								if (function->syntax.flags & Papyrus_SyntaxFlags_Error)
									result = SyntaxError;

								if (error = SynBuf_Append(ctx, &function, sizeof(function)))
									return error;
							}
							break;

						case Key_Property:
							{
								if ((mask & Def_Property) == 0)
									result = SyntaxError;

								struct Papyrus_Syntax_Property* property;
								if (error = ParseProperty(ctx, type, &property))
									return error;

								if (property->syntax.flags & Papyrus_SyntaxFlags_Error)
									result = SyntaxError;

								if (error = SynBuf_Append(ctx, &property, sizeof(property)))
									return error;
							}
							break;

						case -1: // identifier
							{
								if ((mask & Def_Variable) == 0)
									result = SyntaxError;

								struct Papyrus_Syntax_Variable* variable;
								if (error = ParseVariable(ctx, type, &variable))
									return error;

								if (variable->syntax.flags & Papyrus_SyntaxFlags_Error)
									result = SyntaxError;

								if (error = SynBuf_Append(ctx, &variable, sizeof(variable)))
									return error;
							}
							break;
						}
					}
					else result = SyntaxError;
				}
				break;

			case Key_EndEvent:
			case Key_EndFunction:
			case Key_EndGroup:
			case Key_EndProperty:
			case Key_EndState:
			case Key_EndStruct:
				goto exit;
			}
			break;

		default:
			result = SyntaxError;
			Consume(ctx);
		}
	}
exit:;

	if (error = SynBuf_Commit(ctx, &out->data, &out->size))
		return error;

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

#define LEXBUF_DEFAULT_SIZE (8 * 1024)
#define LEXBUF_MINIMUM_SIZE (1 * 1024)

#define SYNBUF_DEFAULT_SIZE (7 * 1024)
#define SYNBUF_MINIMUM_SIZE (1 * 1024)

static inline uintptr_t
TruncateBufferSize(uintptr_t size,
	uintptr_t headerSize, uintptr_t elementSize)
{
	return size - ((size - headerSize) % elementSize);
}

Papyrus_Error
Papyrus_Parser_Create(const struct Papyrus_ParserOptions* options, struct Papyrus_Parser** out)
{
	struct Papyrus_Allocator allocator;

	uintptr_t lexbufSize;
	uintptr_t synbufSize;

	if (options != NULL)
	{
		allocator = options->allocator;

		if ((bool)allocator.allocate != (bool)allocator.deallocate)
			return Papyrus_Error_InvalidArgument;

		if (allocator.allocate == NULL)
		{
			allocator.allocate = &Malloc;
			allocator.deallocate = &Free;
			allocator.context = NULL;
		}

		if ((lexbufSize = options->lexerBufferSize) != 0)
		{
			lexbufSize = lexbufSize & -8;
			if (lexbufSize < LEXBUF_MINIMUM_SIZE)
				return Papyrus_Error_InvalidArgument;
		}
		else lexbufSize = LEXBUF_DEFAULT_SIZE;

		if ((synbufSize = options->syntaxBufferInitialSize) != 0)
		{
			if (synbufSize < SYNBUF_MINIMUM_SIZE)
				return Papyrus_Error_InvalidArgument;

			// elementSize 8 is for basic conservative data alignment
			synbufSize = TruncateBufferSize(synbufSize, sizeof(struct SynBuf_Header), 8);
		}
		else synbufSize = SYNBUF_DEFAULT_SIZE;
	}
	else
	{
		allocator.allocate = &Malloc;
		allocator.deallocate = &Free;
		allocator.context = NULL;

		lexbufSize = LEXBUF_DEFAULT_SIZE;
		synbufSize = SYNBUF_DEFAULT_SIZE;
	}

	// parser allocation: |-parser-|--lexbuf--|--synbuf--|

	uintptr_t bufferSize = sizeof(struct Papyrus_Parser) + lexbufSize + synbufSize;

	struct Papyrus_Parser* parser = (struct Papyrus_Parser*)
		allocator.allocate(allocator.context, bufferSize);
	
	if (parser == NULL)
		return Papyrus_Error_OutOfMemory;

	parser->allocator = allocator;
	parser->bufferSize = bufferSize;

	void* nextBuffer = parser + 1;

	/* initialize lexer */ {
		parser->lex.index = 0;
		parser->lex.count = 0;

		struct ulex_lexer* lexer = &parser->lex.lexer;

		ulex_init(lexer);

		uint32_t* buffer = (uint32_t*)nextBuffer;
		nextBuffer = (char*)nextBuffer + lexbufSize;

		// -20: reserve space for offset at [-1] and two tokens (newline, eof)
		uintptr_t bufferSize = (lexbufSize - 20) / 8;

		uint32_t* tokenBuffer = buffer;

		// +3: two tokens (newline, eof) and the offset at [-1]
		uint32_t* offsetBuffer = buffer + bufferSize + 3;

		offsetBuffer[-1] = 0;

		ulex_set_buffer_size(lexer, bufferSize);
		ulex_set_token_buffer(lexer, tokenBuffer);
		ulex_set_offset_buffer(lexer, offsetBuffer);
	}

	/* initialize syntax buffer */ {
		struct SynBuf_Header* hdr = (struct SynBuf_Header*)nextBuffer;
		nextBuffer = (char*)nextBuffer + synbufSize;

		hdr->prev = NULL;
		hdr->next = NULL;
		hdr->size = synbufSize;

		char* beg = (char*)(hdr + 1);
		parser->synbuf.beg = beg;
		parser->synbuf.cur = beg;
		parser->synbuf.end = nextBuffer;
		parser->synbuf.size = 0;

		parser->synbuf.head = hdr;
	}

	*out = parser;
	return Papyrus_Error_None;
}

void
Papyrus_Parser_Destroy(struct Papyrus_Parser* parser)
{
	struct Papyrus_Allocator allocator = parser->allocator;

	/* free syntax buffer */ {
		// free all buffers in the list after the first
		for (struct SynBuf_Header* hdr = parser->synbuf.head; hdr = hdr->next;)
			allocator.deallocate(allocator.context, hdr, hdr->size);
	}

	allocator.deallocate(allocator.context, parser, parser->bufferSize);
}

Papyrus_Error
Papyrus_Parser_Parse(struct Papyrus_Parser* parser, const char* string, intptr_t stringSize,
	const struct Papyrus_ParseOptions* options, const struct Papyrus_Syntax_Script** out)
{
	parser->source = string;
	ulex_set_source(&parser->lex.lexer, string, stringSize);

	parser->syntax.cur = NULL;
	parser->syntax.end = NULL;
	parser->syntax.allocate = options->allocateSyntax;
	parser->syntax.context = options->allocateSyntaxContext;

	parser->errors.report = options->reportSyntaxError;
	parser->errors.context = options->reportSyntaxErrorContext;

	struct Papyrus_Syntax_Script* script;
	Papyrus_Error error;

	if (error = ParseScript(parser, &script))
	{
		/* reset syntax buffers */ {
			/* when no errors occur, all syntax buffers are used up,
			and end up in the initial default state again.
			otherwise when an error occurs they must be reset. */

			struct SynBuf_Header* hdr = parser->synbuf.head;

			char* beg = (char*)(hdr + 1);
			parser->synbuf.beg = beg;
			parser->synbuf.cur = beg;
			parser->synbuf.end = (char*)hdr + hdr->size;
			parser->synbuf.size = 0;
		}
	}
	else *out = script;

	return error;
}
