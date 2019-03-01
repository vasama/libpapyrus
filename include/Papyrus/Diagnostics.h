#pragma once

#include "Papyrus/String.h"

#include <stdint.h>

struct Papyrus_Diagnostics
{
	void(*report)(struct Papyrus_Diagnostics*,
		uint32_t, struct Papyrus_String);
};
