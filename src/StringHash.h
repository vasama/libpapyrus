#pragma once

#include "Papyrus/String.h"

#include "FNV.h"

#include <stdbool.h>
#include <stdint.h>

static uintptr_t
String_Hash(const struct Papyrus_String* key)
{
	return Papyrus_FNV(key->data, key->size);
}

static bool
String_Equal(const struct Papyrus_String* a, const struct Papyrus_String* b)
{
	return Papyrus_String_Compare(*a, *b) == 0;
}

static bool
String_IEqual(const struct Papyrus_String* a, const struct Papyrus_String* b)
{
	return Papyrus_String_ICompare(*a, *b) == 0;
}
