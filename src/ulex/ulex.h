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

#ifdef __cplusplus
}
#endif
