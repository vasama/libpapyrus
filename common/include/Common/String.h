#pragma once

#include "Papyrus/String.h"

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
