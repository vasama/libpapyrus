#include "Papyrus/Emit.h"

#include "Arena.h"
#include "Asm.h"
#include "DumpAsm.h"

void
Papyrus_Emit(struct Papyrus_Script* const* scripts, intptr_t scriptCount,
	struct Papyrus_EmitOptions* options, struct Papyrus_ArenaPool* pool)
{
	struct Arena arena;
	Arena_Init(&arena, pool);

	struct Asm* asm = Papyrus_GenerateAsm(scripts, scriptCount, &arena, pool);
	Papyrus_DumpAsm(asm, options->asmbuf);

	Arena_Destroy(&arena);
}
