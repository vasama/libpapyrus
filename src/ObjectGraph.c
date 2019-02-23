/* This file implements the object graph memory allocation mechanism.

Memory for graph objects is allocated linearly from blocks of geometrically
increasing size allocated from the backing allocator. The blocks form a linked
list with the root object being placed at the front of the first block. */

#include "ObjectGraph.h"

#include "Macros.h"

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

	char* beg = (char*)root + rootSize;
	root->block.beg = beg;
	root->block.cur = beg;
	root->block.end = (char*)root + initialSize;

	return &root->rootObject;
}

void
Papyrus_ObjectGraph_Delete(void* rootObject,
	struct Papyrus_Allocator allocator)
{
	struct Header* header = &ROOT(rootObject)->header;
	do {
		struct Header* next = header->next;
		allocator.func(allocator.context, header, header->size, 0);
		header = next;
	} while (header != NULL);
}

char*
Papyrus_ObjectGraph_Allocate(void* rootObject,
	uintptr_t size, struct Papyrus_Allocator allocator,
	struct ObjectGraph_Cache* cache)
{
	//TODO: use cache

	struct Root* root = ROOT(rootObject);
	struct Header* header = (struct Header*)root->block.beg;

	//TODO: do this better
	uintptr_t newSize = MIN(size, header->size * 2);

	struct Header* new = (struct Header*)
		allocator.func(allocator.context, NULL, 0, newSize);

	new->next = NULL;
	new->size = newSize;
	header->next = new;

	char* beg = (char*)(new + 1);
	root->block.beg = beg;
	root->block.cur = beg + size;
	root->block.end = beg + newSize;

	return beg;
}
