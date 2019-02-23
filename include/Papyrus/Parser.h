/* This file declares the Papyrus syntactic analysis API. The parser takes a
source string, along with some options, and produces a syntax tree object. */

#pragma once

#include "Papyrus/Allocator.h"
#include "Papyrus/Diagnostics.h"
#include "Papyrus/String.h"

#include <stdint.h>

struct Papyrus_SyntaxTree
{
	const struct Papyrus_Syntax_Script* script;
};

struct Papyrus_ParserOptions
{
	void* lexerBuffer;
	uintptr_t lexerBufferSize;

	struct Papyrus_ArenaPool* pool;
	struct Papyrus_Allocator allocator;
	struct Papyrus_Diagnostics* diag;
};

struct Papyrus_SyntaxTree*
Papyrus_Parse(struct Papyrus_String source,
	const struct Papyrus_ParserOptions* options);
