#include "Papyrus.h"
#include "Papyrus/DumpAST.h"

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void*
AllocateSyntax(void* ctx, uintptr_t size)
{
	return malloc(size);
}

static void
ReportSyntaxError(void* ctx, int32_t line,
	int32_t column, struct Papyrus_String message)
{
	fprintf(stderr, "%d:%d %.*s\n", line, column, (int)message.size, message.data);
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

	char* source = (char*)malloc(1024 * 1024);
	uintptr_t sourceSize = fread(source, 1, 1024 * 1024, file);

	Papyrus_Error error;

	struct Papyrus_Parser* parser;
	if (error = Papyrus_Parser_Create(NULL, &parser))
	{
		fputs("failed to create parser", stderr);
		return 1;
	}

	struct Papyrus_ParseOptions options;
	options.allocateSyntax = &AllocateSyntax;
	options.allocateSyntaxContext = NULL;
	options.reportSyntaxError = &ReportSyntaxError;
	options.reportSyntaxErrorContext = NULL;

	const struct Papyrus_Syntax_Script* script;
	if (error = Papyrus_Parser_Parse(parser, source, sourceSize, &options, &script))
	{
		fputs("parsing failed", stderr);
		return 1;
	}

	Papyrus_DumpAST(script);

	return 0;
}
