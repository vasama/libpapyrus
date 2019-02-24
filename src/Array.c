#include "Array.h"

#include "Macros.h"

#include <string.h>

void
Papyrus_Array_Reserve(struct Array* array,
	uintptr_t newSize, struct Papyrus_Allocator allocator)
{
	char* beg = array->beg;
	uintptr_t bufferSize = array->end - beg;
	uintptr_t newbufSize = MAX(bufferSize * 2, newSize);

	char* newbeg = allocator.func(
		allocator.context, beg, bufferSize, newbufSize);
	char* newcur = newbeg + (array->cur - beg);

	array->beg = newbeg;
	array->cur = newcur;
	array->end = newbeg + newbufSize;
}

char*
Papyrus_Array_Extend(struct Array* array,
	uintptr_t size, struct Papyrus_Allocator allocator)
{
	char* beg = array->beg;
	uintptr_t bufferSize = array->end - beg;
	uintptr_t newbufSize = MAX(bufferSize * 2, size);

	char* newbeg = allocator.func(
		allocator.context, beg, bufferSize, newbufSize);
	char* newcur = newbeg + (array->cur - beg);

	array->beg = newbeg;
	array->cur = newcur + size;
	array->end = newbeg + newbufSize;

	return newcur;
}
