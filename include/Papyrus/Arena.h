/* This file declares a memory pool for internal arenas used for temporary
memory allocation by various parts of the library. The pool allocates fixed
size blocks of memory, the size of which is decided by the user at the time of
creation of the pool, and leases them out to the internal arenas.

For best performance, the chosen block size should be larger than any
individual allocation requested by arenas using the pool. */

#pragma once

#include "Papyrus/Allocator.h"

#include <stdint.h>

struct Papyrus_ArenaPool
{
	uintptr_t blockSize;
	struct Papyrus_Allocator allocator;
	struct Papyrus_Arena_Block* freeList;
};

void
Papyrus_ArenaPool_Init(struct Papyrus_ArenaPool* pool,
	uintptr_t blockSize, struct Papyrus_Allocator allocator);

void
Papyrus_ArenaPool_Destroy(struct Papyrus_ArenaPool* pool);
