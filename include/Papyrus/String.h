#pragma once

#include <stdint.h>
#include <string.h>

struct Papyrus_String
{
	const char* data;
	intptr_t size;
};

#define Papyrus_String_INIT(strlit) \
	{ .data = strlit, .size = sizeof(strlit) - 1 }

#define Papyrus_String_CREATE(strlit) \
	((struct Papyrus_String) Papyrus_String_INIT(strlit))

static inline int32_t
Papyrus_String_Compare(struct Papyrus_String a, struct Papyrus_String b)
{
	intptr_t size = a.size < b.size ? a.size : b.size;
	return strncmp(a.data, b.data, size);
}

int32_t
Papyrus_String_ICompare(struct Papyrus_String a, struct Papyrus_String b);
