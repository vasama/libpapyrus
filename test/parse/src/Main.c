#include "Papyrus/Arena.h"
#include "Papyrus/Diagnostics.h"
#include "Papyrus/DumpAST.h"
#include "Papyrus/Parser.h"
#include "Papyrus/SourceMap.h"

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void*
AllocatorCallback(void* ctx, void* block, uintptr_t size, uintptr_t newSize)
{
	return realloc(block, newSize);
}

struct Diagnostics
{
	struct Papyrus_Diagnostics diag;
	struct Papyrus_SourceMap srcmap;
};

static void
Report(struct Papyrus_Diagnostics* diag_, struct Papyrus_SourceRef src,
	uint32_t code, struct Papyrus_String message)
{
	struct Diagnostics* diag = (struct Diagnostics*)diag_;

	struct Papyrus_SourcePos srcpos =
		Papyrus_SourceMap_GetSourcePos(&diag->srcmap, src.offset);

	fprintf(stderr, "%d:%d %.*s\n", srcpos.line + 1,
		srcpos.column + 1, (int)message.size, message.data);
}

int main(int argc, const char* const* argv)
{
	if (argc != 2)
	{
		fputs("usage: file", stderr);
		return 1;
	}

	FILE* file = fopen(argv[1], "r");

	if (file == NULL)
	{
		fputs(strerror(errno), stderr);
		return 1;
	}

	struct Papyrus_Allocator allocator = { &AllocatorCallback };

	struct Papyrus_ArenaPool pool;
	Papyrus_ArenaPool_Init(&pool, 64 * 1024, allocator);

	struct Papyrus_String source;
	source.data = (char*)malloc(1024 * 1024);
	source.size = fread((char*)source.data, 1, 1024 * 1024 - 1, file);
	((char*)source.data)[source.size] = 0;

	struct Diagnostics diag;
	diag.diag.report = &Report;
	Papyrus_SourceMap_Init(&diag.srcmap);
	Papyrus_SourceMap_SetSource(&diag.srcmap, source, allocator);

	struct Papyrus_SyntaxTree* syntaxTree; {
		struct Papyrus_ParserOptions options;
		options.lexerBufferSize = 16 * 1024;
		options.lexerBuffer = malloc(options.lexerBufferSize);
		options.allocator = allocator;
		options.pool = &pool;
		options.diag = &diag.diag;
		syntaxTree = Papyrus_Parse(source, &options);
	}

	Papyrus_DumpAST(syntaxTree);

	Papyrus_ArenaPool_Destroy(&pool);

	return 0;
}
