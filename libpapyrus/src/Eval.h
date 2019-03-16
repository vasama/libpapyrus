#pragma once

#include "Papyrus/String.h"

#include <stdbool.h>
#include <stdint.h>

struct Arena;
struct Papyrus_Expr;

enum
{
	Eval_None,
	Eval_Int,
	Eval_Bool,
	Eval_Float,
	Eval_String,
};

struct Eval
{
	uint32_t type;
	uint32_t string_size;
	union {
		int32_t int_;
		bool bool_;
		float float_;
		const char* string_data;
	};
};

struct Eval
Papyrus_Eval(const struct Papyrus_Expr* expr, struct Arena* arena);
