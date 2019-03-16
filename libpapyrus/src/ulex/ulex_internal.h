#pragma once

#include <stdint.h>

struct ulex_lexer
{
	const uint32_t* equiv_table;
	const uint32_t* trans_table;
	const uint16_t* eof_table;

	uint32_t state;

	const uint8_t* source;
	intptr_t source_index;
	intptr_t source_size;
};

enum
{
	ulex_error_none = 0,
	ulex_error_insufficient_buffer = 1,
};
typedef uint32_t ulex_error;

enum
{
	// input is partial, no eof
	ulex_flags_chunk = 0x1,
};
typedef uint32_t ulex_flags;

ulex_error
ulex_lex(struct ulex_lexer* lexer, ulex_flags flags, uint32_t* token_buffer,
	uint32_t* offset_buffer, intptr_t buffer_size, intptr_t* out_token_count);
