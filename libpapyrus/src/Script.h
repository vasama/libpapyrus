#pragma once

#include "Papyrus/Script.h"

#include "Common/List.h"

struct Papyrus_Extern
{
	struct Papyrus_Symbol symbol;
	struct Papyrus_Symbol* link;
	struct Papyrus_Type* type;
};

struct LinkSymbol
{
	struct List scripts;
	intptr_t scriptCount;
	struct List symbols;
	intptr_t symbolCount;
	struct List externs;
	intptr_t externCount;
};

struct Symbol
{
	struct LinkSymbol* link;
	struct List list;
	struct Papyrus_Symbol symbol[];
};

static inline struct Symbol*
GetSymbol(struct Papyrus_Symbol* symbol)
{
	return (struct Symbol*)symbol - 1;
}

struct ScriptInternal
{
	struct Papyrus_Script public;

	struct {
		struct Papyrus_Extern** data;
		intptr_t size;
	} externs;
};

void
Papyrus_Script_SetExtern(struct Papyrus_Script* script,
	struct Papyrus_Extern* externSymbol, struct Papyrus_Symbol* symbol);

void
Papyrus_Script_Invalidate(struct Papyrus_Script* script);
