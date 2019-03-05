#pragma once

#include <stdint.h>

struct Papyrus_ArenaPool;
struct Papyrus_Script;

struct Papyrus_EmitBuffer;

typedef void Papyrus_EmitBuffer_FnFlush(
	struct Papyrus_EmitBuffer*, uintptr_t);

struct Papyrus_EmitBuffer
{
	void* buffer;
	uintptr_t size;
	Papyrus_EmitBuffer_FnFlush* flush;
};

struct Papyrus_EmitOptions
{
	struct Papyrus_EmitBuffer* asmbuf;
	struct Papyrus_EmitBuffer* pexbuf;
};

void
Papyrus_Emit(struct Papyrus_Script* const* scripts, intptr_t scriptCount,
	struct Papyrus_EmitOptions* options, struct Papyrus_ArenaPool* pool);
