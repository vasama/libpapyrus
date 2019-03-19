#pragma once

#include "Papyrus/String.h"

#include <stdbool.h>
#include <stdint.h>

struct Papyrus_Allocator;
struct Papyrus_ArenaPool;

struct Args_Option
{
	const char* name;
	const char* desc;
	const char* conf;
	const char* help;

	union {
		struct {
			struct Papyrus_String* data;
			intptr_t size;
		} values;
		bool flag;
	};

	struct {
		struct Papyrus_String disp;
		uint32_t flags;
		int32_t min, max;
		int32_t count;
	} private;
};

struct Args;

struct Args*
Papyrus_Args_Parse(struct Args_Option** options,
	intptr_t optionCount, const char* const* args, intptr_t argCount,
	struct Papyrus_ArenaPool* pool, struct Papyrus_Allocator allocator);

void
Papyrus_Args_Delete(struct Args* args, struct Papyrus_Allocator allocator);
