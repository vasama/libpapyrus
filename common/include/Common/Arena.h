/* This file declares a linear arena allocator for temporary memory allocation.
An arena is backed by a pool supplied by the user of the library. The arena
allows for cheap allocation, and even cheaper deallocation, at the expense of
potentially higher memory usage.

At any time the current position of the arena may be saved and later restored,
cheaply discarding any allocations made after the position was saved. */

#pragma once

#include "Common/Macros.h"

#include "Papyrus/Arena.h"

#ifndef NDEBUG
#	include <string.h>
#endif

struct Papyrus_ArenaPool;

struct Arena
{
	char* beg;
	char* cur;
	char* end;
	struct Papyrus_Arena_Block* head;
	struct Papyrus_Arena_Block* tail;
	struct Papyrus_ArenaPool* pool;
	struct Papyrus_Arena_LargeBlock* large;
};

static inline void
Arena_Init(struct Arena* arena, struct Papyrus_ArenaPool* pool)
{
	void
	Papyrus_Arena_Init(struct Arena* arena, struct Papyrus_ArenaPool* pool);

	Papyrus_Arena_Init(arena, pool);
}

static inline void
Arena_Destroy(struct Arena* arena)
{
	void
	Papyrus_Arena_Destroy(struct Arena* arena);

	Papyrus_Arena_Destroy(arena);
}

struct Arena_Pos
{
	char* beg;
	char* cur;
};

// Save the arena position
static inline struct Arena_Pos
Arena_GetPos(struct Arena* arena)
{
	return (struct Arena_Pos) {
		.beg = arena->beg,
		.cur = arena->cur,
	};
}

// Restore a previously saved position
static inline void
Arena_SetPos(struct Arena* arena, struct Arena_Pos pos)
{
#ifndef NDEBUG
	void
	Papyrus_Arena_SetPos(struct Arena* arena, struct Arena_Pos pos);

	Papyrus_Arena_SetPos(arena, pos);
#endif

	arena->beg = pos.beg;
	arena->cur = pos.cur;
	arena->end = pos.beg + arena->pool->blockSize;
}

// Allocate memory from the arena
static inline void*
Arena_Allocate(struct Arena* arena, uintptr_t size)
{
	void*
	Papyrus_Arena_Allocate(struct Arena* arena, uintptr_t size);

	size = ALIGN(size, 8);

	char* cur = arena->cur;
	char* new = cur + size;

	if (new > arena->end)
		return Papyrus_Arena_Allocate(arena, size);

#ifndef NDEBUG
	memset(cur, 0xAC, size);
#endif

	arena->cur = new;
	return cur;
}

/* Reset the arena, discarding all memory allocated from it, and releasing
extra blocks back to the backing pool. */
static inline void
Arena_Reset(struct Arena* arena)
{
	void
	Papyrus_Arena_Reset(struct Arena* arena);

	Papyrus_Arena_Reset(arena);
}

// Create a virtual allocator that allocates from the arena
static inline struct Papyrus_Allocator
Arena_CreateAllocator(struct Arena* arena)
{
	void*
	Papyrus_Arena_AllocatorFunc(void* context,
		void* block, uintptr_t size, uintptr_t newSize);

	return (struct Papyrus_Allocator) {
		.func = &Papyrus_Arena_AllocatorFunc,
		.context = (struct TempShit*)arena,
	};
}
