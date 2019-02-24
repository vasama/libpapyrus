#include "FNV.h"

uintptr_t
Papyrus_FNV(const void* key, uintptr_t size)
{
	const uint8_t* k = (const uint8_t*)key;
	uintptr_t h = 0xcbf29ce484222325;
	for (uintptr_t i = 0; i < size; ++i)
	{
		h = h ^ k[i];
		h = h * 0x100000001b3;
	}
	return h;
}
