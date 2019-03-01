/* This file declares the program, a part of the semantic program analysis
interface. The program groups together scripts and handles linking of external
symbols. */

#pragma once

#include "Papyrus/Allocator.h"

struct Papyrus_Program*
Papyrus_Program_Create(struct Papyrus_Allocator allocator);

void
Papyrus_Program_Delete(struct Papyrus_Program* program);

void
Papyrus_Program_AddScript(struct Papyrus_Program* program,
	struct Papyrus_Script* script);

void
Papyrus_Program_RemoveScript(struct Papyrus_Program* program,
	struct Papyrus_Script* script);
