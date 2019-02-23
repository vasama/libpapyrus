#pragma once

#include "Papyrus/String.h"

#include <stdint.h>

struct Papyrus_SourceRef
{
	const char* file;
	uint32_t offset;
};

struct Papyrus_Diagnostics
{
	void(*report)(struct Papyrus_Diagnostics*,
		struct Papyrus_SourceRef, uint32_t, struct Papyrus_String);
};
