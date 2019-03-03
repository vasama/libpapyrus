/* This file declares a resizing byte array, as well as macros for generating
user friendly interface wrappers for typed arrays and stacks.

For a given array, in all calls where an allocator may be provided, compatible
allocators must be given. */

#pragma once

#include "Papyrus/Allocator.h"

#include "Macros.h"

#include <stdint.h>
#include <string.h>

struct Array
{
	char* beg;
	char* cur;
	char* end;
};

static inline void
Array_Init(struct Array* array)
{
	array->beg = 0;
	array->cur = 0;
	array->end = 0;
}

static inline void
Array_Destroy(struct Array* array, struct Papyrus_Allocator allocator)
{
	char* beg = array->beg;
	char* end = array->end;

	if (beg != end)
		allocator.func(allocator.context, beg, end - beg, 0);
}

/* Returns a pointer to the first byte in the array. */
static inline void*
Array_Data(struct Array* array)
{
	return array->beg;
}

static inline const void*
Array_DataConst(const struct Array* array)
{
	return array->beg;
}

/* Returns the size of the array in bytes. */
static inline uintptr_t
Array_Size(const struct Array* array)
{
	return array->cur - array->beg;
}

/* Returns the capacity of the array in bytes. */
static inline uintptr_t
Array_Capacity(const struct Array* array)
{
	return array->end - array->beg;
}

/* Reserve a minimum array capacity in bytes. */
static inline void
Array_Reserve(struct Array* array,
	uintptr_t minCapacity, struct Papyrus_Allocator allocator)
{
	void
	Papyrus_Array_Reserve(struct Array* array,
		uintptr_t size, struct Papyrus_Allocator allocator);

	if ((uintptr_t)(array->end - array->beg) < minCapacity)
		Papyrus_Array_Reserve(array, minCapacity, allocator);
}

/* Change the size of the array in bytes. */
static inline void
Array_Resize(struct Array* array,
	uintptr_t size, struct Papyrus_Allocator allocator)
{
	Array_Reserve(array, size, allocator);
	array->cur = array->beg + size;
}

/* Increase the size of the array in bytes.
Return a pointer to the first new byte. */
static inline void*
Array_Extend(struct Array* array,
	uintptr_t size, struct Papyrus_Allocator allocator)
{
	char*
	Papyrus_Array_Extend(struct Array* array,
		uintptr_t size, struct Papyrus_Allocator allocator);

	char* cur = array->cur;
	char* new = cur + size;

	if (new > array->end)
		return Papyrus_Array_Extend(array, size, allocator);

	array->cur = new;
	return cur;
}

/* Append bytes to the end of the array.
Return a pointer to the first appended byte. */
static inline void*
Array_Append(struct Array* array, const void* data,
	uintptr_t size, struct Papyrus_Allocator allocator)
{
	void* area = Array_Extend(array, size, allocator);
	memcpy(area, data, size);
	return area;
}

/* Insert bytes into a given index in the array. Return a pointer to the first
inserted byte. If the given index is negative, or greater than the size of the
array, behaviour is undefined .*/
static inline void*
Array_Insert(struct Array* array, uintptr_t pos, const void* data,
	uintptr_t size, struct Papyrus_Allocator allocator)
{
	Array_Extend(array, size, allocator);

	char* beg = array->beg;
	char* area = beg + pos;

	memmove(area + size, area, (array->cur - beg) - size - pos);
	memcpy(area, data, size);

	return area;
}

/* Remove a range of bytes from the array. If index of the given range is
negative or extends past the end of the array, behaviour is undefined. */
static inline void
Array_Remove(struct Array* array, uintptr_t pos, uintptr_t size)
{
	char* area = array->beg + pos;
	char* area_end = area + size;
	char* cur = array->cur;

	memmove(area, area_end, cur - area_end);
	array->cur = cur - size;
}

/* Resize the array to size zero. */
static inline void
Array_Clear(struct Array* array)
{
	array->cur = array->beg;
}

/* Defines a typed array with the given name. The defined functions are typed
versions of the generic functions. */
#define Array_DEFINE(name, type) \
	Array_DEFINE_(name, type) \
	static inline UNUSED type* name ## _Data(struct Array* array) \
	{ \
		return (type*)array->beg; \
	} \
	static inline UNUSED type const* name ## _DataConst( \
		const struct Array* array) \
	{ \
		return (type const*)array->beg; \
	} \
	static inline UNUSED type* name ## _Last(struct Array* array) \
	{ \
		return (type*)array->cur - 1; \
	} \
	static inline UNUSED type* name ## _Extend(struct Array* array, \
		intptr_t count, struct Papyrus_Allocator allocator) \
	{ \
		return (type*)Array_Extend(array, \
			(uintptr_t)count * sizeof(type), allocator); \
	} \
	static inline UNUSED type* name ## _Append(struct Array* array, \
		type const* element, struct Papyrus_Allocator allocator) \
	{ \
		return (type*)Array_Append( \
			array, element, sizeof(type), allocator); \
	} \
	static inline UNUSED type* name ## _Insert( \
		struct Array* array, intptr_t index, type const* element, \
		struct Papyrus_Allocator allocator) \
	{ \
		return (type*)Array_Insert(array, \
			(uintptr_t)index * sizeof(type), \
			element, sizeof(type), allocator); \
	} \
	static inline UNUSED void name ## _Remove( \
		struct Array* array, intptr_t index) \
	{ \
		Array_Remove(array, \
			(uintptr_t)index * sizeof(type), sizeof(type)); \
	} \
	static inline UNUSED void name ## _Resize(struct Array* array, \
		intptr_t size, struct Papyrus_Allocator allocator) \
	{ \
		Array_Resize(array, \
			(uintptr_t)size * sizeof(type), allocator); \
	}

#define Array_DEFINE_STACK(name, type) \
	Array_DEFINE_(name, type) \
	static inline UNUSED type* name ## _Peek(struct Array* array) \
	{ \
		return (type*)array->cur - 1; \
	} \
	static inline UNUSED type const* name ## _PeekConst( \
		const struct Array* array) \
	{ \
		return (type const*)array->cur - 1; \
	} \
	static inline UNUSED type* name ## _PeekRange( \
		struct Array* array, intptr_t size) \
	{ \
		return (type*)array->cur - size; \
	} \
	static inline UNUSED type const* name ## _PeekRangeConst( \
		const struct Array* array, intptr_t size) \
	{ \
		return (type const*)array->cur - size; \
	} \
	static inline UNUSED void name ## _Push(struct Array* array, \
		type const* element, struct Papyrus_Allocator allocator) \
	{ \
		Array_Append(array, element, sizeof(type), allocator); \
	} \
	static inline UNUSED void name ## _PushRange( \
		struct Array* array, type const* data, \
		intptr_t size, struct Papyrus_Allocator allocator) \
	{ \
		Array_Append(array, data, \
			(uintptr_t)size * sizeof(type), allocator); \
	} \
	static inline UNUSED type name ## _Pop(struct Array* array) \
	{ \
		type* element = (type*)array->cur - 1; \
		array->cur = (char*)element; \
		return *element; \
	} \
	static inline UNUSED void name ## _PopRange( \
		struct Array* array, intptr_t size, void* out) \
	{ \
		type* data = (type*)array->cur - size; \
		if (out != NULL) \
			memcpy(out, data, (uintptr_t)size * sizeof(type)); \
		array->cur = (char*)data; \
	}

#define Array_DEFINE_(name, type) \
	static inline UNUSED intptr_t name ## _Size(const struct Array* array) \
	{ \
		return (intptr_t)((uintptr_t)( \
			array->cur - array->beg) / sizeof(type)); \
	} \
	static inline UNUSED intptr_t name ## _Capacity( \
		const struct Array* array) \
	{ \
		return (intptr_t)((uintptr_t)( \
			array->end - array->beg) / sizeof(type)); \
	} \
	static inline UNUSED void name ## _Reserve(struct Array* array, \
		intptr_t minCapacity, struct Papyrus_Allocator allocator) \
	{ \
		Array_Reserve(array, \
			(uintptr_t)minCapacity * sizeof(type), allocator); \
	}

#define Array_FOREACH_pf(x, t) (t*)Array_Data(x)
#define Array_FOREACH_cf(x, t) Array_Size(x) / sizeof(t)

#define Array_FOREACH(xvar, ivar, type, array) \
	FOREACHP_(xvar, ivar, &*(array), \
		Array_FOREACH_pf, type, Array_FOREACH_cf, type)

#define Array_FOREACHV(xvar, ivar, type, array) \
	FOREACHV_(xvar, ivar, &*(array), \
		Array_FOREACH_pf, type, Array_FOREACH_cf, type)
