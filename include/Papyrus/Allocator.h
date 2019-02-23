/* This file declares a simple virtual allocator, consisting of callback and
context pointers. */

#pragma once

#include <stdint.h>

/* The allocator callback signature is similar to the standard library function
realloc, and is used for allocation, deallocation and reallocation. The first
parameter is the user context pointer. The rest of the parameters are pointer
to a block, size of the block, and size of the new block, as in realloc. */
typedef void* Papyrus_AllocatorFunc(void*, void*, uintptr_t, uintptr_t);

/* The allocator holds no resources, other than what is supplied by the user,
and no initialization or cleanup functions are necessary. Accessing the fields
of the allocator structure by the user is safe. */
struct Papyrus_Allocator
{
	Papyrus_AllocatorFunc* func;
	void* context;
};
