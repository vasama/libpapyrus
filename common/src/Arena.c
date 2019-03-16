/* This file implements a linear arena allocator. See Papyrus/Arena.h and
Arena.h for descriptions of the public and internal interfaces respectively.

An arena holds a linked list of blocks supplied by the backing pool, and
allocates linearly from the currently active block, until it is exhausted and
the arena moves to the next block in the list, acquiring new blocks from the
backing pool as necessary.

Restoring a saved position resets the current block and position within that
block using pointers in the position structure. Further allocation continues
as usual.

Allocations larger than the block size are allocated separately using the
pool's backing allocator, and held in a separate linked list.

In debug mode the arena fills allocated bytes with 0xAC, deallocated bytes
with 0xAD, and so far untouched bytes with 0xAF. */

#include "Common/Arena.h"

#include "Common/Macros.h"

#include <string.h>

struct Papyrus_Arena_Block
{
	struct Papyrus_Arena_Block* next;
	char data[];
};
typedef struct Papyrus_Arena_Block Block;

struct Papyrus_Arena_LargeBlock {
	struct Papyrus_Arena_LargeBlock* next;
	uintptr_t size;
	char data[];
};
typedef struct Papyrus_Arena_LargeBlock LargeBlock;

#define BLOCK(beg) ((Block*)(beg) - 1)

void
Papyrus_ArenaPool_Init(struct Papyrus_ArenaPool* pool,
	uintptr_t blockSize, struct Papyrus_Allocator allocator)
{
	pool->blockSize = blockSize - sizeof(Block);
	pool->allocator = allocator;
	pool->freeList = NULL;
}

void
Papyrus_ArenaPool_Destroy(struct Papyrus_ArenaPool* pool)
{
	struct Papyrus_Allocator allocator = pool->allocator;

	uintptr_t blockSize = sizeof(Block) + pool->blockSize;
	for (Block* block = pool->freeList, *next; block != NULL; block = next)
	{
		next = block->next;
#ifndef NDEBUG
		memset(block, 0xAD, blockSize);
#endif
		allocator.func(allocator.context, block, blockSize, 0);
	}
}


static inline void*
Allocate(struct Papyrus_ArenaPool* pool, uintptr_t size)
{
	struct Papyrus_Allocator allocator = pool->allocator;
	return allocator.func(allocator.context, NULL, 0, size);
}

#define ALLOCATE(pool, type, fam) \
	Allocate((pool), sizeof(type) + (fam))

static inline Block*
GetBlock(struct Papyrus_ArenaPool* pool)
{
	uintptr_t blockSize = pool->blockSize;

	Block* block = pool->freeList;
	if (block != NULL)
	{
		pool->freeList = block->next;
	}
	else
	{
		block = ALLOCATE(pool, Block, blockSize);
	}
	return block;
}

void
Papyrus_Arena_Init(struct Arena* arena, struct Papyrus_ArenaPool* pool)
{
	Block* block = GetBlock(pool);
	block->next = NULL;

	arena->beg = block->data;
	arena->cur = block->data;
	arena->end = block->data + pool->blockSize;
	arena->head = block;
	arena->tail = block;
	arena->pool = pool;
	arena->large = NULL;
}

void
Papyrus_Arena_Destroy(struct Arena* arena)
{
	struct Papyrus_ArenaPool* pool = arena->pool;

	/* free large blocks */ {
		struct Papyrus_Allocator allocator = pool->allocator;

		for (LargeBlock* large = arena->large,
			*next; large != NULL; large = next)
		{
			next = large->next;
			allocator.func(allocator.context, large, large->size, 0);
		}
	}

	arena->tail->next = pool->freeList;
	pool->freeList = arena->head;
}

void*
Papyrus_Arena_Allocate(struct Arena* arena, uintptr_t size)
{
	struct Papyrus_ArenaPool* pool = arena->pool;
	uintptr_t blockSize = pool->blockSize;

	if (size > blockSize)
	{
		LargeBlock* large = ALLOCATE(pool, LargeBlock, size);
		large->size = size;
		large->next = arena->large;
		arena->large = large;
		return large->data;
	}

	Block* block = BLOCK(arena->beg)->next;
	if (block == NULL)
	{
		Block* new = GetBlock(pool);
		new->next = NULL;
		block->next = new;
		block = new;

		arena->tail = new;
	}

	arena->beg = block->data;
	arena->cur = block->data + size;
	arena->end = block->data + blockSize;

#ifndef NDEBUG
	memset(block->data + size, 0xAF, blockSize - size);
	memset(block->data, 0xAC, size);
#endif

	return block->data;
}

void
Papyrus_Arena_Reset(struct Arena* arena)
{
	struct Papyrus_ArenaPool* pool = arena->pool;

	Block* head = arena->head;

#ifndef NDEBUG
	{
		uintptr_t blockSize = pool->blockSize;
		for (Block* b = head; b != NULL; b = b->next)
			memset(b->data, 0xAD, blockSize);
	}
#endif

	{
		Block* tail = arena->tail;
		if (head != tail)
		{
			tail->next = pool->freeList;
			pool->freeList = head->next;
			head->next = NULL;
		}
	}
	arena->tail = head;

	arena->beg = head->data;
	arena->cur = head->data;
	arena->end = head->data + pool->blockSize;
}

void*
Papyrus_Arena_AllocatorFunc(void* context,
	void* block, uintptr_t size, uintptr_t newSize)
{
	if (newSize == 0)
		return NULL;

	//TODO: optimize realloc buffer resize

	void* new = Arena_Allocate((struct Arena*)context, newSize);

	if (size != 0)
		memcpy(new, block, size);

	return new;
}

#ifndef NDEBUG
void
Papyrus_Arena_SetPos(struct Arena* arena, struct Arena_Pos pos)
{
	uintptr_t blockSize = arena->pool->blockSize;

	Block* block = BLOCK(pos.beg);
	memset(pos.cur, 0xAD, blockSize - (pos.cur - pos.beg));

	while ((block = block->next) != NULL)
		memset(block->data, 0xAD, blockSize);
}
#endif
