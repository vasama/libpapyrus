#include "Common/String.h"

#ifdef __unix__
#	include <strings.h>
#endif

int32_t
Papyrus_String_ICompare(struct Papyrus_String a, struct Papyrus_String b)
{
	intptr_t size = a.size < b.size ? a.size : b.size;

#ifdef __unix__
	int32_t cmp = strncasecmp(a.data, b.data, size);
#endif

#ifdef _WIN32
	int32_t cmp = _strnicmp(a.data, b.data, size);
#endif

	if (cmp == 0)
	{
		intptr_t diff = a.size - b.size;
		if (diff > INT_MAX) return INT_MAX;
		if (diff < INT_MIN) return INT_MIN;
		return (int32_t)diff;
	}

	return cmp;
}
