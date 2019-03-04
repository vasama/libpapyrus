#include "Papyrus/String.h"

#ifdef __unix__
#	include <strings.h>
#endif

int32_t
Papyrus_String_ICompare(struct Papyrus_String a, struct Papyrus_String b)
{
	intptr_t size = a.size < b.size ? a.size : b.size;

#ifdef __unix__
	return strncasecmp(a.data, b.data, size);
#endif

#ifdef _WIN32
	return _strnicmp(a.data, b.data, size);
#endif
}
