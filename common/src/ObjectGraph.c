/* This file implements the object graph memory allocation mechanism.

Memory for graph objects is allocated linearly from blocks of geometrically
increasing size allocated from the backing allocator. The blocks form a linked
list with the root object being placed at the front of the first block. */

#include "Common/ObjectGraph.h"

#include "Common/Intrinsics.h"
#include "Common/Macros.h"

#include <assert.h>
#include <stdint.h>

// Block header
struct Header
{
	struct Header* next;
	uintptr_t size;
};

// Root block header
struct Root
{
	struct Header header;
	struct Papyrus_Allocator allocator;
	struct {
		char* beg;
		char* cur;
		char* end;
	} block;
	char rootObject[];
};

// Derive a pointer to the root header from a pointer to the root object
#define ROOT(rootObject) ((struct Root*)rootObject - 1)

void*
Papyrus_ObjectGraph_Create(uintptr_t rootObjectSize,
	uintptr_t initialSize, struct Papyrus_Allocator allocator)
{
	uintptr_t rootSize = sizeof(struct Root) + rootObjectSize;
	assert(initialSize > rootSize);

	struct Root* root = (struct Root*)
		allocator.func(allocator.context, NULL, 0, initialSize);

	root->header.next = NULL;
	root->header.size = initialSize;

	root->allocator = allocator;

	char* beg = (char*)root + rootSize;
	root->block.beg = beg;
	root->block.cur = beg;
	root->block.end = (char*)root + initialSize;

	return &root->rootObject;
}

void
Papyrus_ObjectGraph_Delete(void* rootObject)
{
	struct Root* root = ROOT(rootObject);
	struct Header* header = &root->header;
	struct Papyrus_Allocator allocator = root->allocator;
	do {
		struct Header* next = header->next;
		allocator.func(allocator.context, header, header->size, 0);
		header = next;
	} while (header != NULL);
}

static inline uintptr_t
Po2(uintptr_t x)
{
	uintptr_t y = (uintptr_t)1 << bsr64(x);
	return y == x ? y : y << 1;
}

char*
Papyrus_ObjectGraph_Allocate(void* rootObject,
	uintptr_t size, struct ObjectGraph_Cache* cache)
{
	struct Root* root = ROOT(rootObject);
	struct Header* header = (struct Header*)root->block.beg;

	uintptr_t newSize = MIN(Po2(size), header->size * 2);

	struct Header* new = (struct Header*)
		root->allocator.func(root->allocator.context, NULL, 0, newSize);

	new->next = NULL;
	new->size = newSize;
	header->next = new;

	char* beg = (char*)(new + 1);
	root->block.beg = beg;
	cache->cur = beg + size;
	cache->end = beg + newSize;

	return beg;
}
