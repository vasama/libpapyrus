#pragma once

struct Arena;
struct Papyrus_ArenaPool;
struct IR;

void
Papyrus_EliminateSSA(struct IR* ir,
	struct Arena* irArena, struct Papyrus_ArenaPool* pool);
