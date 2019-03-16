#pragma once

struct Papyrus_EmitBuffer;
struct Asm;

void
Papyrus_DumpAsm(struct Asm*, struct Papyrus_EmitBuffer* buffer);
