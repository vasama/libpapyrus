#pragma once

#include "Papyrus/String.h"

#include <stdint.h>

struct Papyrus_PascalString
{
	intptr_t size;
	char data[];
};

static inline struct Papyrus_String
Papyrus_PascalString_GetString(
	const struct Papyrus_PascalString* pascalString)
{
	return (struct Papyrus_String) {
		.data = pascalString->data,
		.size = pascalString->size,
	};
}
