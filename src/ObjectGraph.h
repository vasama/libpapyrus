/* This file declares a mechanism for simplifying memory management of large
object graphs with a single root object that outlives all other objects.

A graph is created by allocating the root object, with further objects being
allocated through the root object. The entire graph is deallocated through the
root object. Allocated objects cannot be deallocated individually.

Part of the object graph memory allocation mechanism can be cached by the user
to improve performance by eliminating an indirection. While a cache is active,
allocating memory directly through the root object causes undefined behaviour.
Once the cache is no longer needed, and before deleting the graph, the graph
must be synchronized with the cache through the root object. */

#pragma once

#include "Papyrus/Allocator.h"

#include <stdint.h>

#ifndef NDEBUG
#	include <string.h>
#endif

/* Create an object graph using the specified root object size.
Size of the initial allocation, and the allocator must also be specified.
Returns a pointer to the root object memory. */
static inline void*
ObjectGraph_Create(uintptr_t rootObjectSize,
	uintptr_t initialSize, struct Papyrus_Allocator allocator)
{
	void*
	Papyrus_ObjectGraph_Create(uintptr_t rootObjectSize,
		uintptr_t initialSize, struct Papyrus_Allocator allocator);

	return Papyrus_ObjectGraph_Create(rootObjectSize, initialSize, allocator);
}

/* Create an object graph using the specified root object type.
Returns a pointer to the root object. */
#define ObjectGraph_CREATE(type, size, allocator) \
	((type*)ObjectGraph_Create(sizeof(type), (size), (allocator)))

/* Delete the entire object graph. */
static inline void
ObjectGraph_Delete(void* rootObject, struct Papyrus_Allocator allocator)
{
	void
	Papyrus_ObjectGraph_Delete(void* rootObject, 
		struct Papyrus_Allocator allocator);

	Papyrus_ObjectGraph_Delete(rootObject, allocator);
}

// Allocation cache
struct ObjectGraph_Cache
{
	char* cur;
	char* end;
};

// Create an allocation cache
static inline struct ObjectGraph_Cache
ObjectGraph_CreateCache(void* rootObject)
{
	return ((struct ObjectGraph_Cache*)rootObject)[-1];
}

// Synchronize the graph with the cache, invalidating the cache
static inline void
ObjectGraph_Synchronize(void* rootObject,
	struct ObjectGraph_Cache* cache)
{
	((struct ObjectGraph_Cache*)rootObject)[-1] = *cache;
}

// Allocate memory using a cache
static inline void*
ObjectGraph_CacheAllocate(void* rootObject,
	uintptr_t size, struct Papyrus_Allocator allocator,
	struct ObjectGraph_Cache* cache)
{
	char*
	Papyrus_ObjectGraph_Allocate(void* rootObject,
		uintptr_t size, struct Papyrus_Allocator allocator,
		struct ObjectGraph_Cache* cache);

	// round up to align
	size = size + 7 & -8;

	char* cur = cache->cur;
	char* new = cur + size;

	if (new > cache->end)
	{
		return Papyrus_ObjectGraph_Allocate(
			rootObject, size, allocator, cache);
	}

#ifndef NDEBUG
	memset(cur, 0xEC, size);
#endif

	cache->cur = new;
	return cur;
}

// Allocate memory through the root object
static inline void*
ObjectGraph_Allocate(void* rootObject,
	uintptr_t size, struct Papyrus_Allocator allocator)
{
	return ObjectGraph_CacheAllocate(rootObject, size,
		allocator, (struct ObjectGraph_Cache*)rootObject - 1);
}
