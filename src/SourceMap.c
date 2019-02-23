/* This file implements a source string offset to source position mapping. The
map uses an array of line beginning offsets, and performs lookups using a
binary search in said array for the given offset.

A simple finite state machine is used for parsing the source string when
building the line offset array. */

#include "Papyrus/SourceMap.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static const uint8_t Equivalence[256] = { ['\r'] = 3, ['\n'] = 6 };

static const uint8_t Transitions[9] = {
	/* transition table
	(x) accepts
	
	        input
	        v
	state >      0   1   2
	          +------------
	        * |  0  (0) (0)
	       \r |  1  (1) (1)
	       \n |  2   2  (2)

	bits: AAASSSSS

	AAA:
		100: accept
		000: no accept

	SSSSS: new state
	*/

	0x00, 0x80, 0x80,
	0x01, 0x81, 0x81,
	0x02, 0x02, 0x82,
};

void
Papyrus_SourceMap_SetSource(struct Papyrus_SourceMap* map,
	struct Papyrus_String source, struct Papyrus_Allocator allocator)
{
	if (source.size == 0)
	{
		map->mid = map->beg;
		return;
	}

	uint32_t* beg = map->beg;
	uint32_t* mid;
	uint32_t* end = map->end;

	if (beg == end)
	{
		uintptr_t initialSize = 4096;
		beg = allocator.func(allocator.context, NULL, 0, initialSize);
		end = (uint32_t*)((char*)beg + initialSize);
	}

	mid = beg;

	// always reserve space for one more offset
	uint32_t* last = end - 1;

	uint32_t state = 0;
	intptr_t offset = 0;

	while (true)
	{
		while ((offset < source.size) & (mid < last))
		{
			uint32_t eqc = Equivalence[source.data[offset]];
			uint32_t trans = Transitions[eqc + state];
			state = trans & 0x1F;

			*mid = (uint32_t)offset;
			mid = (uint32_t*)((char*)mid + (trans >> 5));

			offset += 1;
		}

		if (offset < source.size)
		{
			uintptr_t bufferSize = (char*)end - (char*)beg;
			uintptr_t newSize = bufferSize * 2;

			uint32_t* new = (uint32_t*)allocator.func(
				allocator.context, beg, bufferSize, newSize);
			
			beg = new;
			mid = (uint32_t*)((char*)new + bufferSize);
			end = (uint32_t*)((char*)new + newSize);

			last = end - 1;
		}
		else break;
	}

	if (state != 0)
		*mid++ = (uint32_t)offset;

	map->beg = beg;
	map->mid = mid;
	map->end = end;
}

struct Papyrus_SourcePos
Papyrus_SourceMap_GetSourcePos(
	const struct Papyrus_SourceMap* map, uint32_t offset)
{
	const uint32_t* lines = map->beg;

	int32_t line;
	int32_t column;

	if (lines != map->end)
	{
		if (offset < lines[0])
		{
			line = 0;
			column = offset;
		}
		else
		{
			intptr_t i = 0;
			intptr_t s = map->mid - lines;

			while (s > 1)
			{
				intptr_t half = s >> 1;

				bool b = lines[i + half] > offset;

				i = b ? i : i + half;
				s = b ? s - half : s;
			}

			line = (int32_t)(i + 1);
			column = (int32_t)(offset - lines[i]);
		}
	}
	else
	{
		line = 0;
		column = 0;
	}

	return (struct Papyrus_SourcePos) { .line = line, .column = column };
}
