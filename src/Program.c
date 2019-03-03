#include "Program.h"

#include "Papyrus/String.h"

#include "Arena.h"
#include "HashTable.h"
#include "List.h"
#include "Macros.h"
#include "ObjectGraph.h"
#include "Script.h"
#include "StringHash.h"

#include <assert.h>
#include <stdlib.h>

struct Papyrus_Program
{
	struct Papyrus_Allocator allocator;

	// Papyrus_String -> LinkSymbol*
	struct HashTable symbolTable;
};

HashTable_DEFINE_MAP(SymbolMap, struct Papyrus_String,
	struct LinkSymbol*, String_Hash, String_IEqual);

struct Papyrus_Program*
Papyrus_Program_Create(struct Papyrus_Allocator allocator)
{
	struct Papyrus_Program* program =
		ObjectGraph_CREATE(struct Papyrus_Program, 4096, allocator);

	program->allocator = allocator;
	HashTable_Init(&program->symbolTable);

	return program;
}

void
Papyrus_Program_Delete(struct Papyrus_Program* program)
{
	SymbolMap_Destroy(&program->symbolTable, program->allocator);

	ObjectGraph_Delete(program);
}

static struct LinkSymbol*
GetLinkSymbol(struct Papyrus_Program* program, struct Papyrus_String name)
{
	struct LinkSymbol* linkSymbol;

	struct LinkSymbol** mapSymbol;
	if (SymbolMap_Insert(&program->symbolTable,
		&name, &program->allocator, &mapSymbol))
	{
		linkSymbol = (struct LinkSymbol*)
			ObjectGraph_Allocate(program, sizeof(struct LinkSymbol));

		List_Init(&linkSymbol->scripts);
		linkSymbol->scriptCount = 0;
		List_Init(&linkSymbol->symbols);
		linkSymbol->symbolCount = 0;
		List_Init(&linkSymbol->externs);
		linkSymbol->externCount = 0;
	}
	else linkSymbol = *mapSymbol;

	return linkSymbol;
}

static struct Papyrus_Symbol*
GetRealSymbol(struct LinkSymbol* linkSymbol)
{
	if (linkSymbol->scriptCount + linkSymbol->symbolCount != 1)
		return NULL;

	struct List* list = linkSymbol->scriptCount > 0 ?
		&linkSymbol->scripts : &linkSymbol->symbols;

	return List_GetObject(list, struct Symbol, list)->symbol;
}

void
Papyrus_Program_AddScript(struct Papyrus_Program* program,
	struct Papyrus_Script* publicScript)
{
	struct ScriptInternal* script = (struct ScriptInternal*)publicScript;

	struct Papyrus_String name = script->public.symbol.name;
	struct LinkSymbol* linkSymbol = GetLinkSymbol(program, name);

	// insert the script into the list of scripts in the link symbol
	List_InsertAfter(&linkSymbol->scripts,
		&GetSymbol(&publicScript->symbol)->list);

	// for each symbol exported by the script
	FOREACHV_S(sym, sym_i, &script->public.exports)
	{
		struct LinkSymbol* linkSymbol = GetLinkSymbol(program, sym->name);

		struct Symbol* internal = GetSymbol(sym);
		
		// insert the symbol into the list of symbols in the 
		List_InsertAfter(&linkSymbol->symbols, &internal->list);
		linkSymbol->symbolCount += 1;
		internal->link = linkSymbol;

		struct Papyrus_Symbol* symbol = GetRealSymbol(linkSymbol);
		List_FOREACH(x, x_i, struct Symbol, list, &linkSymbol->externs)
		{
			struct Papyrus_Extern* externSymbol = 
				(struct Papyrus_Extern*)x->symbol;

			struct Papyrus_Script* script = externSymbol->script;
			Papyrus_Script_SetExtern(script, externSymbol, symbol);
			Papyrus_Script_Invalidate(script);
		}
	}

	// for each external symbol used in the script
	FOREACHV_S(ext, ext_i, &script->externs)
	{
		struct Papyrus_Symbol* symbol = &ext->symbol;
		struct LinkSymbol* linkSymbol = GetLinkSymbol(program, symbol->name);

		struct Symbol* internal = GetSymbol(symbol);
		List_InsertAfter(&linkSymbol->externs, &internal->list);
		internal->link = linkSymbol;
	}

#ifndef NDEBUG
	publicScript->symbol.eflags |= Papyrus_ScriptFlags_Debug_Linked;
#endif
}

void
Papyrus_Program_RemoveScript(struct Papyrus_Program* program,
	struct Papyrus_Script* publicScript)
{
	(void)program;

	struct ScriptInternal* script = (struct ScriptInternal*)publicScript;
	
	// for each external symbol used in the script
	FOREACHV_S(ext, ext_i, &script->externs)
	{
		List_Remove(&GetSymbol(&ext->symbol)->list);
	}

	// for each symbol exported by the script
	FOREACHV_S(sym, sym_i, &script->public.exports)
	{
		struct Symbol* internal = GetSymbol(sym);
		struct LinkSymbol* linkSymbol = internal->link;

		List_Remove(&internal->list);
		linkSymbol->scriptCount -= 1;

		List_FOREACH(x, x_i, struct Symbol, list, &linkSymbol->externs)
		{
			struct Papyrus_Extern* externSymbol =
				(struct Papyrus_Extern*)x->symbol;

			externSymbol->link = sym;

			Papyrus_Script_Invalidate(externSymbol->script);
		}
	}

	List_Remove(&GetSymbol(&publicScript->symbol)->list);

#ifndef NDEBUG
	publicScript->symbol.eflags &= ~Papyrus_ScriptFlags_Debug_Linked;
#endif
}
