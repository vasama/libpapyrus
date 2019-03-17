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

static inline struct Papyrus_String
Papyrus_String_FromCString(const char* string)
{
	return (struct Papyrus_String) {
		string, strlen(string)
	};
}
