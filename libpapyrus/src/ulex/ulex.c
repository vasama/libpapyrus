#include "ulex_internal.h"

static intptr_t
lex(struct ulex_lexer* lexer, uint32_t* __restrict trans_buffer,
	uint32_t* __restrict offset_buffer, intptr_t buffer_size)
{
	const uint32_t* __restrict equiv_table = lexer->equiv_table;
	const uint32_t* __restrict trans_table = lexer->trans_table;

	uint32_t state = lexer->state;

	const uint8_t* source = lexer->source;
	intptr_t source_offset = lexer->source_index;
	intptr_t source_size = lexer->source_size;

	intptr_t buffer_offset = 0;
	buffer_size *= 4;

	while ((source_offset < source_size) & (buffer_offset < buffer_size))
	{
		uint32_t equiv = equiv_table[source[source_offset]];
		uint32_t trans = *(const uint32_t*)((const char*)trans_table + equiv + state);

		*(uint32_t*)((char*)trans_buffer + buffer_offset) = trans;
		*(uint32_t*)((char*)offset_buffer + buffer_offset) = (uint32_t)source_offset;

		state = trans & 0xffff;
		buffer_offset += trans >> 29;
		source_offset += 1;
	}

	lexer->state = state;
	lexer->source_index = source_offset;

	return buffer_offset / 4;
}

ulex_error
ulex_lex(struct ulex_lexer* lexer, ulex_flags flags, uint32_t* token_buffer,
	uint32_t* offset_buffer, intptr_t buffer_size, intptr_t* out_token_count)
{
	if (lexer->state == (uint32_t)-1)
	{
		// first transition gets special handling
		uint32_t equiv = lexer->equiv_table[lexer->source[lexer->source_index++]];
		lexer->state = *(const uint32_t*)((const char*)lexer->trans_table + equiv) & 0xffff;
	}

	intptr_t token_count = lex(lexer, token_buffer, offset_buffer, buffer_size);

	// transform transitions to tokens
	for (intptr_t i = 0; i < token_count; ++i)
		token_buffer[i] = (token_buffer[i] >> 16) & 0xfff;

	int source_end = lexer->source_index == lexer->source_size;

	// handle end of file
	if ((flags & ulex_flags_chunk) == 0 && source_end)
	{
		token_buffer[token_count] = lexer->eof_table[lexer->state / 4];
		offset_buffer[token_count] = (uint32_t)lexer->source_size;
		++token_count;
	}

	*out_token_count = token_count;
	return source_end ? ulex_error_none : ulex_error_insufficient_buffer;
}
