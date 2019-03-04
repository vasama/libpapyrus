#pragma once

#include <stdint.h>
#include <string.h>
#include <limits.h>

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
	intptr_t min = a.size < b.size ? a.size : b.size;
	int32_t cmp = memcmp(a.data, b.data, min);

	if (cmp == 0)
	{
		intptr_t diff = a.size - b.size;
		if (diff > INT_MAX) return INT_MAX;
		if (diff < INT_MIN) return INT_MIN;
		return (int32_t)diff;
	}

	return cmp;
}

int32_t
Papyrus_String_ICompare(struct Papyrus_String a, struct Papyrus_String b);
