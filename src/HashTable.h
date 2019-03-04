/* This file declares a generic void pointer based hash table, as well as
macros for generating user friendly interface wrappers for typed maps and sets.

For a given table, in all calls where an allocator may be provided, compatible
allocators must be given. Similarly, in all calls where an element size is
required, the same element size must be given. */

#pragma once

#include "Papyrus/Allocator.h"

#include "Macros.h"

#include <stdbool.h>
#include <stdint.h>

struct HashTable
{
	void* data;
	intptr_t size;
	intptr_t capa;
};

static inline void
HashTable_Init(struct HashTable* table)
{
	extern const uint8_t Papyrus_HashTable_EmptyBlock[];

	table->data = (void*)Papyrus_HashTable_EmptyBlock;
	table->size = 0;
	table->capa = 0;
}

static inline void
HashTable_Destroy(struct HashTable* table,
	uintptr_t elemSize, struct Papyrus_Allocator allocator)
{
	void
	Papyrus_HashTable_Destroy(struct HashTable* table,
		uintptr_t elemSize, struct Papyrus_Allocator allocator);

	if (table->capa > 0)
		Papyrus_HashTable_Destroy(table, elemSize, allocator);
}

/* Returns the number of elements in the hash table. */
static inline intptr_t
HashTable_Size(const struct HashTable* table)
{
	return table->size;
}

/* Returns the maximum capacity of the hash table. Maximum load factor is not
taken into account. */
static inline intptr_t
HashTable_Capacity(const struct HashTable* table)
{
	return table->capa;
}

static inline void
HashTable_Reserve(struct HashTable* table, uintptr_t elemSize,
	intptr_t minCapacity, struct Papyrus_Allocator allocator)
{
	void
	Papyrus_HashTable_Reserve(struct HashTable* table, uintptr_t elemSize,
		intptr_t minCapacity, struct Papyrus_Allocator allocator);

	Papyrus_HashTable_Reserve(table, elemSize, minCapacity, allocator);
}

static inline void*
HashTable_Find(struct HashTable* table, const void* key,
	uintptr_t elemSize, uintptr_t(*hash)(const void*),
	bool(*compare)(const void*, const void*))
{
	void*
	Papyrus_HashTable_Find(struct HashTable* table,
		const void* key, uintptr_t elemSize, uintptr_t hash,
		bool(*compare)(const void*, const void*));

	return Papyrus_HashTable_Find(table, key, elemSize, hash(key), compare);
}

struct HashTable_InsertResult
{
	void* elem;
	bool inserted;
};

static inline struct HashTable_InsertResult
HashTable_Insert(struct HashTable* table, const void* key,
	uintptr_t elemSize, uintptr_t(*hash)(const void*),
	bool(*compare)(const void*, const void*),
	struct Papyrus_Allocator* allocator)
{
	struct HashTable_InsertResult
	Papyrus_HashTable_Insert(struct HashTable* table, const void* key,
		uintptr_t elemSize, uintptr_t(*hash)(const void*),
		bool(*compare)(const void*, const void*),
		struct Papyrus_Allocator* allocator);

	return Papyrus_HashTable_Insert(table,
		key, elemSize, hash, compare, allocator);
}

static inline bool
HashTable_Remove(struct HashTable* table,
	const void* key, uintptr_t elemSize, uintptr_t(*hash)(const void*),
	bool(*compare)(const void*, const void*), void* out)
{
	bool
	Papyrus_HashTable_Remove(struct HashTable* table,
		const void* key, uintptr_t elemSize, uintptr_t hash,
		bool(*compare)(const void*, const void*), void* out);

	return Papyrus_HashTable_Remove(
		table, key, elemSize, hash(key), compare, out);
}

static inline void
HashTable_Clear(struct HashTable* table, uintptr_t elemSize)
{
	void
	Papyrus_HashTable_Clear(
		struct HashTable* table, uintptr_t elemSize);

	if (table->size > 0)
		Papyrus_HashTable_Clear(table, elemSize);
}

#define HashTable_DEFINE_MAP(name, keytype, valtype, fnhash, fncmp) \
	struct name ## _Element { keytype key; valtype value; }; \
	static inline void UNUSED name ## _Destroy(struct HashTable* table, \
		struct Papyrus_Allocator allocator) \
	{ \
		HashTable_Destroy(table, \
			sizeof(struct name ## _Element), allocator); \
	} \
	static inline void UNUSED name ## _Reserve(struct HashTable* table, \
		intptr_t minCapacity, struct Papyrus_Allocator allocator) \
	{ \
		HashTable_Reserve(table, \
			sizeof(struct name ## _Element), minCapacity, allocator); \
	} \
	static inline valtype* UNUSED name ## _Find( \
		struct HashTable* table, keytype const* key) \
	{ \
		struct name ## _Element* elem = \
			(struct name ## _Element*)HashTable_Find( \
				table, key, sizeof(struct name ## _Element), \
				(uintptr_t(*)(const void*))&(fnhash), \
				(bool(*)(const void*, const void*))&(fncmp)); \
		return elem != NULL ? &elem->value : NULL; \
	} \
	static inline bool UNUSED name ## _Insert( \
		struct HashTable* table, keytype const* key, \
		struct Papyrus_Allocator* allocator, valtype** out) \
	{ \
		struct HashTable_InsertResult result = \
			HashTable_Insert(table, key, \
				sizeof(struct name ## _Element), \
				(uintptr_t(*)(const void*))&(fnhash), \
				(bool(*)(const void*, const void*))&(fncmp), allocator); \
		if (result.inserted) \
		{ \
			((struct name ## _Element*)result.elem)->key = *key; \
			*out = &((struct name ## _Element*)result.elem)->value; \
			return true; \
		} \
		*out = &((struct name ## _Element*)result.elem)->value; \
		return false; \
	} \
	static inline bool UNUSED name ## _Remove( \
		struct HashTable* table, keytype const* key, valtype* out) \
	{ \
		return HashTable_Remove(table, key, \
			sizeof(struct name ## _Element), \
			(uintptr_t(*)(const void*))&(fnhash), \
			(bool(*)(const void*, const void*))&(fncmp), out); \
	} \
	static inline void UNUSED name ## _Clear(struct HashTable* table) \
	{ \
		HashTable_Clear(table, sizeof(struct name ## _Element)); \
	} \
	EXPECT_SEMICOLON

#define HashTable_DEFINE_SET(name, type, fnhash, fncmp) \
	static inline void UNUSED name ## _Destroy(struct HashTable* table, \
		struct Papyrus_Allocator allocator) \
	{ \
		HashTable_Destroy(table, sizeof(type), allocator); \
	} \
	static inline void UNUSED name ## _Reserve(struct HashTable* table, \
		intptr_t minCapacity, struct Papyrus_Allocator allocator) \
	{ \
		HashTable_Reserve(table, \
			sizeof(type), minCapacity, allocator); \
	} \
	static inline type* UNUSED name ## _Find( \
		struct HashTable* table, type const* key) \
	{ \
		return (type*)HashTable_Find(table, key, \
			sizeof(type), (uintptr_t(*)(const void*))&(fnhash), \
			(bool(*)(const void*, const void*))&(fncmp)); \
	} \
	static inline bool UNUSED name ## _Insert( \
		struct HashTable* table, type const* key, \
		struct Papyrus_Allocator* allocator, type** out) \
	{ \
		struct HashTable_InsertResult result = \
			HashTable_Insert(table, key, sizeof(type), \
				(uintptr_t(*)(const void*))&(fnhash), \
				(bool(*)(const void*, const void*))&(fncmp), allocator); \
		if (result.inserted) \
		{ \
			*(type*)result.elem = *key; \
			*out = (type*)result.elem; \
			return true; \
		} \
		*out = (type*)result.elem; \
		return false; \
	} \
	static inline bool UNUSED name ## _Remove( \
		struct HashTable* table, type const* key, type* out) \
	{ \
		return HashTable_Remove(table, key, \
			sizeof(type), (uintptr_t(*)(const void*))&(fnhash), \
			(bool(*)(const void*, const void*))&(fncmp), out); \
	} \
	static inline void UNUSED name ## _Clear(struct HashTable* table) \
	{ \
		HashTable_Clear(table, sizeof(type)); \
	} \
	EXPECT_SEMICOLON
