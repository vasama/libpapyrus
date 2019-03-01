/* This file implements a generic hash table. See HashTable.h for interface
descriptions. The table combines chaining and open addressing. A single memory
allocation is divided into fixed size blocks. Each block contains N control
bytes followed by N elements, where N is the block size.

struct Block
{
	uint8_t ctrls[BlockSize];
	element elems[BlockSize];
};

Control bytes and elements in the same block form pairs, and each pair together
forms a slot. For each unique hash present in the table, the table contains a
linked list of slots. Slots are linked together using the control bytes.

The first slot in a chain is at the index given by the hash of the elements
within. The control value for the first slot in a chain has a flag (Ctrl_Flag)
set to indicate this. Information required to derive the index of the next slot
in the chain is given by masking the control byte with Ctrl_Mask. The special
value Ctrl_Last indicates the last slot in a chain. To derive the index of the
next slot in a chain, the masked control byte is used as an index to a static
table yielding an offset between the indices of the two slots. For unused slots
the value Ctrl_Null is used.

Calculating the index of the next slot in a chain:
index1 = (index0 + JumpOffsets[ctrl0 & Ctrl_Mask]) % tableSize */

#include "HashTable.h"

#include "Intrinsics.h"
#include "Macros.h"

#include <string.h>

static_assert(sizeof(void*) == 8, "only 64-bit is supported");

enum
{
	Ctrl_Null = 0x00,
	Ctrl_Last = 0x7F,
	Ctrl_Flag = 0x80,
	Ctrl_Mask = 0x7F,
};

enum { BlockSize = 16 };

/* Compute the size of a block in bytes, given element size. */
static inline uintptr_t
GetBlockSize(uintptr_t elemSize)
{
	return BlockSize + BlockSize * elemSize;
}

/* The empty block consists of N zero (Ctrl_Null) control bytes emulating an
empty block. It is used as the default block in empty tables. Lookups will not
find a chain with the correct (or any) hash and will never try to touch the
actual elements. */
const uint8_t Papyrus_HashTable_EmptyBlock[BlockSize];

/* Compute the maximum number of elements with acceptable load factor for a
given table size. Maximum load factor is defined as 15/16 or 93.75%. */
static inline intptr_t
GetMaxSize(intptr_t capacity)
{
	return capacity - (intptr_t)((uintptr_t)capacity / 16);
}

/* The mask is a bitmask used for fast table size modulo. The table size is
always a power of two. The shift is the number of bits any native word must be
shifted right for the resulting index to be less than the table size. */
static inline void
GetMaskAndShift(intptr_t capacity, uintptr_t* maskOut, uintptr_t* shiftOut)
{
	uintptr_t mask = (uintptr_t)(capacity != 0 ? capacity - 1 : 1);
	*maskOut = mask;
	*shiftOut = clz64(mask);
}

/* Compute index from a hash value using Fibonacci hashing. The hash value is
multiplied by 2^N / phi, where N is the size of the native word, and phi is the
golden ratio. The resulting value is then shifted right to fit into the table.
*/
static inline intptr_t
HashToIndex(uintptr_t hash, uintptr_t shift)
{
	uint64_t phi = 11400714819323198485u;
	return (intptr_t)((hash * phi) >> shift);
}

/* JumpOffsets provides offsets between the indices of two slots in chain. The
offset at [0] is not used, as the zero control byte value is used to indicate
empty slots.*/
static const uintptr_t JumpOffsets[126] = {
	/* Linear growth for the first 15 offsets to promote staying in the same
	block for optimal cache performance. */
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,

	/* Next 66 triangular numbers. */
	21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210, 231,
	253, 276, 300, 325, 351, 378, 406, 435, 465, 496, 528, 561, 595, 630,
	666, 703, 741, 780, 820, 861, 903, 946, 990, 1035, 1081, 1128, 1176,
	1225, 1275, 1326, 1378, 1431, 1485, 1540, 1596, 1653, 1711, 1770, 1830,
	1891, 1953, 2016, 2080, 2145, 2211, 2278, 2346, 2415, 2485, 2556,

	/* 44 more triangular numbers at a steeper growth rate. */
	3741, 8385, 18915, 42486, 95703, 215496, 485605, 1091503, 2456436,
	5529475, 12437578, 27986421, 62972253, 141700195, 318819126, 717314626,
	1614000520, 3631437253, 8170829695, 18384318876, 41364501751,
	93070021080, 209407709220, 471167588430, 1060127437995, 2385287281530,
	5366895564381, 12075513791265, 27169907873235, 61132301007778,
	137547673121001, 309482258302503, 696335090510256, 1566753939653640,
	3525196427195653, 7931691866727775, 17846306747368716,
	40154190394120111, 90346928493040500, 203280588949935750,
	457381324898247375, 1029107980662394500, 2315492957028380766,
	5209859150892887590,
};

static inline intptr_t
Jump(intptr_t index, uint8_t ctrl, uintptr_t mask)
{
	return (intptr_t)(((uintptr_t)index + JumpOffsets[ctrl]) & mask);
}

struct Slot
{
	uint8_t* ctrl;
	void* elem;
};

/* Compute pointers to the control byte and element of a slot at the specified
index. */
static inline struct Slot
GetSlot(void* data, uintptr_t elemSize, intptr_t index)
{
	uintptr_t i = (uintptr_t)index / BlockSize;
	uintptr_t j = (uintptr_t)index % BlockSize;

	data = (char*)data + i * (BlockSize + BlockSize * elemSize);

	return (struct Slot) {
		.ctrl = (uint8_t*)((char*)data + j),
		.elem = (char*)data + BlockSize + j * elemSize
	};
}

void
Papyrus_HashTable_Destroy(struct HashTable* table,
	uintptr_t elemSize, struct Papyrus_Allocator allocator)
{
	intptr_t capa = table->capa;
	allocator.func(allocator.context, table->data, capa + capa * elemSize, 0);
}

/* Returns a pointer to the element with the given key, or NULL if not it is
not present.

Operation: Use the hash of the input key mod table size to find the hash chain.
If the hash chain exists, loop through it and compare keys in each element to
the input key. */
void*
Papyrus_HashTable_Find(struct HashTable* table,
	const void* key, uintptr_t elemSize, uintptr_t hash,
	bool(*fnCompare)(const void*, const void*))
{
	void* data = table->data;

	uintptr_t mask, shift;
	GetMaskAndShift(table->capa, &mask, &shift);

	intptr_t index = HashToIndex(hash, shift);
	struct Slot slot = GetSlot(data, elemSize, index);

	uint8_t ctrl = *slot.ctrl;
	if ((ctrl & Ctrl_Flag) == 0)
	{
		return NULL;
	}

	ctrl = ctrl & Ctrl_Mask;
	while (true)
	{
		if (fnCompare(slot.elem, key))
		{
			return slot.elem;
		}

		if (LIKELY(ctrl == Ctrl_Last))
		{
			return NULL;
		}

		index = Jump(index, ctrl, mask);
		slot = GetSlot(data, elemSize, index);
		ctrl = *slot.ctrl;
	}
}

/* A new table is created and all elements from the existing table are inserted
one by one. The existing table is destroyed and replaced by the new table. */
static void
Rehash(struct HashTable* table, intptr_t newCapacity,
	uintptr_t elemSize, uintptr_t(*fnHash)(const void*),
	bool(*fnCompare)(const void*, const void*),
	struct Papyrus_Allocator allocator)
{
	void* data = table->data;
	intptr_t capa = table->capa;

	struct HashTable newTable;

	uintptr_t newdataSize = newCapacity + newCapacity * elemSize;
	newTable.data = allocator.func(allocator.context, NULL, 0, newdataSize);

	uintptr_t blockSize = BlockSize + BlockSize * elemSize;
	for (char* f = (char*)newTable.data,
		*l = f + newdataSize; f < l; f += blockSize)
	{
		memset(f, Ctrl_Null, BlockSize);
	}

	newTable.size = 0;
	newTable.capa = newCapacity;

	uintptr_t dataSize;
	if (table->size > 0)
	{
		// rehash into the new table

		dataSize = capa + capa * elemSize;

		char* ctrl;
		char* elem = (char*)data;
		char* last = (char*)data + dataSize;

		while (elem < last)
		{
			ctrl = elem;
			elem += BlockSize;

			for (intptr_t i = 0; i < BlockSize; ++i)
			{
				if (*(uint8_t*)ctrl != Ctrl_Null)
				{
					struct HashTable_InsertResult result =
						HashTable_Insert(&newTable, elem,
							elemSize, fnHash, fnCompare, &allocator);
					assert(result.inserted);

					memcpy(result.elem, elem, elemSize);
				}
				ctrl += 1;
				elem += elemSize;
			}
		}

		goto free;
	}
	else if (capa > 0)
	{
		dataSize = capa + capa * elemSize;

	free:
		allocator.func(allocator.context, data, dataSize, 0);
	}

	*table = newTable;
}

/* Attempts to find an empty slot that may be connected to the slot at the
specified index. Returns the control jump value on success, or zero on failure.
On success, pointers to the empty slot are returned via the out parameter. */
static uint8_t
FindEmptySlot(void* data, uintptr_t elemSize,
	intptr_t index, uintptr_t mask, struct Slot* out)
{
	for (intptr_t i = 1; i < 126; ++i)
	{
		intptr_t nextIndex = Jump(index, (uint8_t)i, mask);
		struct Slot slot = GetSlot(data, elemSize, nextIndex);

		if (*slot.ctrl == Ctrl_Null)
		{
			*out = slot;
			return (uint8_t)i;
		}
	}
	return Ctrl_Null;
}

/* Returns a pointer to the element with the input key and a bool indicating
whether a new element was inserted into the table.

If at any point during insertion a rehash is required but no allocator is
provided (allocator argument is NULL), (NULL, false) is returned.

Operation: If the hash chain exists, append to it. Otherwise create a new
chain, relocating existing elements from other chains as needed. */
struct HashTable_InsertResult
Papyrus_HashTable_Insert(struct HashTable* table, const void* key,
	uintptr_t elemSize, uintptr_t(*fnHash)(const void*),
	bool(*fnCompare)(const void*, const void*),
	struct Papyrus_Allocator* allocator)
{
	intptr_t capa = table->capa;
	intptr_t size = table->size;

	if (UNLIKELY(size >= GetMaxSize(capa)))
	{
		if (LIKELY(allocator != NULL))
		{
		grow:
			capa = MAX(capa * 2, BlockSize);
			Rehash(table, capa, elemSize, fnHash, fnCompare, *allocator);
		}
		else if (UNLIKELY(size == capa))
		{
			return (struct HashTable_InsertResult)
				{ .elem = NULL, .inserted = false };
		}
	}

	uintptr_t hash = fnHash(key);

	void* data = table->data;

	uintptr_t mask, shift;
	GetMaskAndShift(table->capa, &mask, &shift);

	intptr_t index = HashToIndex(hash, shift);
	struct Slot slot = GetSlot(data, elemSize, index);

	uint8_t ctrl = *slot.ctrl;
	if ((ctrl & Ctrl_Flag) == 0)
	{
		if (ctrl != Ctrl_Null)
		{
			// slot is in use in another chain

			intptr_t parentIndex;
			struct Slot parentSlot;
			uint8_t parentCtrl;

			/* find parent in other chain */ {
				uintptr_t parentHash = fnHash(slot.elem);
				parentIndex = HashToIndex(parentHash, shift);

				while (true)
				{
					parentSlot = GetSlot(data, elemSize, parentIndex);
					parentCtrl = *parentSlot.ctrl;

					intptr_t nextIndex = Jump(
						parentIndex, parentCtrl & Ctrl_Mask, mask);

					if (nextIndex == index)
						break;

					parentIndex = nextIndex;
				}
			}

			intptr_t srcIndex = index;
			struct Slot srcSlot = slot;
			uint8_t srcCtrl = ctrl;

			intptr_t newIndex = parentIndex;
			struct Slot newSlot;

			while (true)
			{
				uint8_t jump = FindEmptySlot(data,
					elemSize, newIndex, mask, &newSlot);

				if (jump == Ctrl_Null)
				{
					if (LIKELY(allocator != NULL))
						goto grow;

					return (struct HashTable_InsertResult)
						{ .elem = NULL, .inserted = false };
				}

				newIndex = Jump(newIndex, jump, mask);
				*(uint8_t*)newSlot.elem = jump;

				if (srcCtrl == Ctrl_Last)
					break;

				srcIndex = Jump(srcIndex, srcCtrl, mask);
				srcSlot = GetSlot(data, elemSize, srcIndex);
				srcCtrl = *srcSlot.ctrl;
			}

			srcIndex = index;
			srcSlot = slot;
			srcCtrl = ctrl;

			while (true)
			{
				uint8_t jump = *(uint8_t*)newSlot.elem;
				memcpy(newSlot.elem, srcSlot.elem, elemSize);

				if (srcCtrl == Ctrl_Last)
					break;

				// jump backwards
				newIndex = (intptr_t)(((uintptr_t)newIndex
					- JumpOffsets[ctrl]) & mask);
				newSlot = GetSlot(data, elemSize, newIndex);

				// link slots together
				*newSlot.ctrl = (*newSlot.ctrl & Ctrl_Flag) | jump;

				srcIndex = Jump(srcIndex, srcCtrl, mask);
				srcSlot = GetSlot(data, elemSize, srcIndex);
				srcCtrl = *srcSlot.ctrl;
			}
		}

		*slot.ctrl = Ctrl_Flag | Ctrl_Last;
		table->size = size + 1;

		return (struct HashTable_InsertResult)
			{ .elem = slot.elem, .inserted = true };
	}

	while (true)
	{
		if (fnCompare(slot.elem, key))
		{
			return (struct HashTable_InsertResult)
				{ .elem = slot.elem, .inserted = false };
		}

		uint8_t jump = ctrl & Ctrl_Mask;
		if (LIKELY(jump == Ctrl_Last))
		{
			struct Slot nextSlot;
			jump = FindEmptySlot(data,
				elemSize, index, mask, &nextSlot);

			if (UNLIKELY(jump == Ctrl_Null))
			{
				if (allocator != NULL)
					goto grow;
				
				return (struct HashTable_InsertResult)
					{ .elem = NULL, .inserted = false };
			}

			*slot.ctrl = (ctrl & Ctrl_Flag) | jump;
			*nextSlot.ctrl = Ctrl_Last;
			table->size = size + 1;

			return (struct HashTable_InsertResult)
				{ .elem = nextSlot.elem, .inserted = true };
		}

		index = Jump(index, jump, mask);
		slot = GetSlot(data, elemSize, index);
		ctrl = *slot.ctrl;
	}
}

/* Returns true if an element with the input key was found and removed,
otherwise returns false.

Operation: If the element exists, move the last element in the chain to the
vacated slot, and remove the last slot, or remove the entire chain if the
element being removed is the only one in the chain. */
bool
Papyrus_HashTable_Remove(struct HashTable* table,
	const void* elem, uintptr_t elemSize, uintptr_t hash,
	bool(*fnCompare)(const void*, const void*), void* out)
{
	void* data = table->data;

	uintptr_t mask, shift;
	GetMaskAndShift(table->capa, &mask, &shift);

	intptr_t index = HashToIndex(hash, shift);
	struct Slot slot = GetSlot(data, elemSize, index);

	uint8_t ctrl = *slot.ctrl;
	if ((ctrl & Ctrl_Flag) == 0)
	{
		return false;
	}

	for (uint8_t* prevCtrl = NULL;;)
	{
		// extract the jump offset index
		uint8_t jump = ctrl & Ctrl_Mask;

		// if element matches
		if (fnCompare(slot.elem, elem))
		{
			// move out the current element
			if (out != NULL)
				memcpy(out, slot.elem, elemSize);

			// if slot is last in the chain
			if (LIKELY(jump == Ctrl_Last))
			{
				// mark the current slot as null
				*slot.ctrl = Ctrl_Null;

				// if slot is not first in the chain
				if (prevCtrl != NULL)
				{
					// mark the previous slot as last
					*prevCtrl = (*prevCtrl & Ctrl_Flag) | Ctrl_Last;
				}
			}
			else
			{
				// save pointer to the removed element
				void* elem = slot.elem;

				// find the last slot in the chain
				while (true)
				{
					prevCtrl = slot.ctrl;
					index = Jump(index, jump, mask);
					slot = GetSlot(data, elemSize, index);

					ctrl = *slot.ctrl;
					if (LIKELY(ctrl == Ctrl_Last))
						break;

					jump = ctrl & Ctrl_Mask;
				}

				// move the element from the last slot
				// to the slot of the removed element
				memcpy(elem, slot.elem, elemSize);

				// mark the penultimate slot as last
				*prevCtrl = (*prevCtrl & Ctrl_Flag) | Ctrl_Last;

				// mark the last slot as null
				*slot.ctrl = Ctrl_Null;
			}

			--table->size;
			return true;
		}

		// if slot is last in the chain
		if (LIKELY(jump == Ctrl_Last))
		{
			return false;
		}

		// save pointer to the previous ctrl
		prevCtrl = slot.ctrl;

		// jump to the next slot in the chain
		index = Jump(index, jump, mask);
		slot = GetSlot(data, elemSize, index);
		ctrl = *slot.ctrl;
	}
}

void
Papyrus_HashTable_Clear(struct HashTable* table, uintptr_t elemSize)
{
	intptr_t capa = table->capa;
	uintptr_t dataSize = capa + capa * elemSize;
	uintptr_t blockSize = BlockSize + BlockSize * elemSize;
	for (char* f = (char*)table->data,
		*l = f + dataSize; f < l; f += blockSize)
	{
		// Zero out control bytes in all blocks
		memset(f, Ctrl_Null, BlockSize);
	}
	table->size = 0;
}
