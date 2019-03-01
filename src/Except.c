#include "Except.h"

void
Papyrus_Throw(struct Papyrus_Except* except)
{
	longjmp(except->jmp, 1);
}
