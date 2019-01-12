#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#ifndef ulex_tables
#	error include the generated header instead
#endif

#include "ulex_internal.h"

static inline void
ulex_reset(struct ulex_lexer* lexer)
{
	lexer->state = (uint32_t)-1;
}

static inline void
ulex_init(struct ulex_lexer* lexer)
{
	lexer->equiv_table = ulex_equiv_table;
	lexer->trans_table = ulex_trans_table;
	lexer->eof_table = ulex_eof_table;

	ulex_reset(lexer);
}

static inline void
ulex_set_source(struct ulex_lexer* lexer,
	const char* source, intptr_t source_size)
{
	lexer->source = (const uint8_t*)source;
	lexer->source_index = 0;
	lexer->source_size = source_size;
}

static inline void
ulex_set_buffer_size(struct ulex_lexer* lexer, intptr_t buffer_size)
{
	lexer->buffer_size = buffer_size;
}

static inline void
ulex_set_token_buffer(struct ulex_lexer* lexer, uint32_t* buffer)
{
	lexer->trans_buffer = buffer;
}

static inline void
ulex_set_offset_buffer(struct ulex_lexer* lexer, uint32_t* buffer)
{
	lexer->offset_buffer = buffer;
}

static inline ulex_token_type*
ulex_get_tokens(const struct ulex_lexer* lexer)
{
	return (ulex_token_type*)lexer->trans_buffer;
}

static inline uint32_t*
ulex_get_offsets(const struct ulex_lexer* lexer)
{
	return lexer->offset_buffer;
}

#ifdef __cplusplus
}
#endif
