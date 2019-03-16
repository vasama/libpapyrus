#pragma once

#include "Common/Macros.h"

#include <setjmp.h>

struct Papyrus_Except
{
	jmp_buf jmp;
};

void NORETURN
Papyrus_Throw(struct Papyrus_Except* except);

#define Papyrus_TRY(except) \
	if (setjmp((except)->jmp) == 0)

#define Papyrus_CATCH \
	else
