/* This file declares a map type providing mapping from a source file offset to
a source position consisting of line and column indices. The map does not keep
references to the source string. */

#pragma once

#include "Papyrus/Allocator.h"
#include "Papyrus/String.h"

#include <stdint.h>

struct Papyrus_SourcePos
{
	int32_t line;
	int32_t column;
};

struct Papyrus_SourceMap
{
	uint32_t* beg;
	uint32_t* mid;
	uint32_t* end;
};

static inline void
Papyrus_SourceMap_Init(struct Papyrus_SourceMap* map)
{
	map->beg = NULL;
	map->mid = NULL;
	map->end = NULL;
}

static inline void
Papyrus_SourceMap_Destroy(struct Papyrus_SourceMap* map,
	struct Papyrus_Allocator allocator)
{
	uint32_t* beg = map->beg;
	uint32_t* end = map->end;

	if (beg != end)
		allocator.func(allocator.context, beg, (char*)end - (char*)beg, 0);
}

/* Build a map for the given source string. */
void
Papyrus_SourceMap_SetSource(struct Papyrus_SourceMap* map,
	struct Papyrus_String source, struct Papyrus_Allocator allocator);

/* Find source position given source string offset. */
struct Papyrus_SourcePos
Papyrus_SourceMap_GetSourcePos(
	const struct Papyrus_SourceMap* map, uint32_t offset);
