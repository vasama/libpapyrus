#include "Common/Args.h"

#include "Common/Arena.h"
#include "Common/Array.h"
#include "Common/Except.h"
#include "Common/HashTable.h"
#include "Common/Macros.h"
#include "Common/StringHash.h"

#include "Papyrus/String.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _CRT_INTERNAL_NONSTDC_NAMES
#	undef min
#	undef max
#endif

#ifdef NDEBUG
#	define Verify(...) ((void)(__VA_ARGS__))
#else
#	define Verify(...) assert(__VA_ARGS__)
#endif

#define Ctx ArgsCtx

Array_DEFINE(OptArray, struct Args_Option*);

HashTable_DEFINE_MAP(OptMap, struct Papyrus_String,
	struct Args_Option*, String_Hash, String_Equal);

struct Value
{
	struct Args_Option* option;
	struct Papyrus_String value;
};
Array_DEFINE(ValArray, struct Value);

struct Args
{
	uintptr_t size;
	struct Papyrus_String values[];
};

typedef struct {
	struct Papyrus_Allocator allocator;
	struct Papyrus_Except except;
	struct Arena arena;

	struct {
		struct Args_Option** data;
		intptr_t size;
	} options;

	struct Array positional;
	struct HashTable optionTable;
	struct Args_Option* dashOption;
	struct Args_Option* shortOptions[96];
	struct Args_Option* helpOption;
	struct Papyrus_String helpText;
	
	bool result;
	struct Array values;
} Ctx;

enum
{
	OptionFlags_Value = 0x1,
	OptionFlags_Help = 0x2,
};

static struct Papyrus_String
TakeString(const char** pString, char chr)
{
	const char* string = *pString;
	const char* end = strchr(string, chr);
	if (end != NULL)
	{
		*pString = end + 1;
	}
	else
	{
		end = string + strlen(string);
		*pString = end;
	}
	return (struct Papyrus_String) {
		string, end - string
	};
}

static bool
SplitString(struct Papyrus_String string, char chr,
	struct Papyrus_String* out1, struct Papyrus_String* out2)
{
	const char* split = memchr(string.data, chr, string.size);
	if (split != NULL)
	{
		intptr_t rsize = split - string.data;
		out1->size = rsize;
		out2->data = split + 1;
		out2->size = string.size - rsize - 1;
		return true;
	}
	else
	{
		*out1 = string;
		out2->size = 0;
		return false;
	}
}

static bool
ParseInt(struct Papyrus_String string, int32_t* out)
{
	int64_t value = 0;
	FOREACHV_S(x_, x_i, &string)
	{
		uint8_t x = (uint8_t)x_ - '0';

		if (UNLIKELY(x > 9))
			return false;

		value = value * 10 + x;

		if (UNLIKELY(value > INT_MAX))
			return false;
	}
	*out = (int32_t)value;
	return true;
}

static void
RegisterOption(Ctx* ctx, struct Args_Option* option)
{
	uint32_t flags = OptionFlags_Value;
	int32_t min = -1, max = -1;

	for (const char* confString = option->conf; *confString != '\0';)
	{
		struct Papyrus_String conf = TakeString(&confString, ';');

		struct Papyrus_String value;
		bool gotval = SplitString(conf, '=', &conf, &value);

		if (strncmp(conf.data, "flag", conf.size) == 0)
		{
			assert(gotval == false);
			assert(flags & OptionFlags_Value);
			flags &= ~OptionFlags_Value;
		}
		else if (strncmp(conf.data, "help", conf.size) == 0)
		{
			assert(ctx->helpOption == NULL);
			ctx->helpOption = option;
			ctx->helpText = value;
		}
		else if (strncmp(conf.data, "min", conf.size) == 0)
		{
			assert(min == -1);
			Verify(gotval && ParseInt(value, &min));
		}
		else if (strncmp(conf.data, "max", conf.size) == 0)
		{
			assert(max == -1);
			Verify(gotval && ParseInt(value, &max));
		}
		else assert(false);
	}

	if (flags & OptionFlags_Value)
	{
		if (min == -1) min = 0;
		if (max == -1) max = INT_MAX;
	}
	else
	{
		assert(min == -1);
		assert(max == -1);
		min = 0;
		max = 1;
	}

	bool first = true;
	for (const char* nameString = option->name;;)
	{
		struct Papyrus_String string = TakeString(&nameString, ',');

		int32_t dashCount = 0;
		if (*string.data == '-')
			if (++dashCount, *++string.data == '-')
				++dashCount, ++string.data;
		string.size -= dashCount;

		assert(string.size > 0);

		switch (dashCount)
		{
		case 0:
			OptArray_Append(&ctx->positional, &option, ctx->allocator);
			break;

		case 1:
			{
				assert(string.size == 1);
				uint8_t index = *string.data - 32;
				assert(index < 96);
				assert(ctx->shortOptions[index] == NULL);
				ctx->shortOptions[index] = option;
			}
			break;

		case 2:
			{
				struct Args_Option** mapval;
				Verify(OptMap_Insert(&ctx->optionTable,
					&string, &ctx->allocator, &mapval));
				*mapval = option;
			}
			break;
		}

		if (first)
		{
			option->private.disp = string;
			first = false;
		}

		if (*nameString == '\0') break;
	}

	option->private.flags = flags;
	option->private.min = min;
	option->private.max = max;
	option->private.count = 0;
}


static void
ReportError(Ctx* ctx, const char* fmt, ...)
{
	ctx->result = false;

	va_list vlist;
	va_start(vlist, fmt);
	vfprintf(stderr, fmt, vlist);
	va_end(vlist);
}

static void
ProcessOption(Ctx* ctx,
	struct Args_Option* option, struct Papyrus_String value)
{
	if (++option->private.count > option->private.max)
		ReportError(ctx, "too many options\n");

	if (option->private.flags & OptionFlags_Value)
	{
		ValArray_Append(&ctx->values,
			&(struct Value) { option, value }, ctx->allocator);
	}
}

static void
Parse(Ctx* ctx, const char* const* argv, intptr_t argCount)
{
	ctx->result = true;
	Array_Init(&ctx->values);
	ValArray_Reserve(&ctx->values, argCount,
		Arena_CreateAllocator(&ctx->arena));

	bool acceptOption = true;

	intptr_t positionalIndex = 0;
	struct Args_Option* positional = NULL;
	if (OptArray_Size(&ctx->positional) > 0)
		positional = OptArray_Data(&ctx->positional)[0];

	struct Args_Option* dashOption = ctx->dashOption;

	intptr_t valueCount = 0;
	for (intptr_t i = 0; i < argCount; ++i)
	{
		struct Papyrus_String arg =
			Papyrus_String_FromCString(argv[i]);

		int32_t dashCount = 0;
		if (acceptOption)
		{
			if (*arg.data == '-')
				if (++dashCount, *++arg.data == '-')
					++dashCount, ++arg.data;
			arg.size -= dashCount;
		}

		switch (dashCount)
		{
		case 0: // positional
			if (positional != NULL)
			{
				ProcessOption(ctx, positional, arg);
				if (positional->private.count == positional->private.max)
				{
					if (++positionalIndex < OptArray_Size(&ctx->positional))
					{
						positional = OptArray_Data(
							&ctx->positional)[positionalIndex];
					}
					else positional = NULL;
				}
				++valueCount;
			}
			else
			{
				ReportError(ctx,
					"unexpected argument: '%.*s'\n",
					(int)arg.size, arg.data);
			}
			break;

		case 1: // short option
			if (arg.size == 0)
			{
				if (dashOption != NULL)
				{
					ProcessOption(ctx, dashOption,
						Papyrus_String_CREATE(""));
				}
				else
				{
					ReportError(ctx, "- unrecognized option\n");
				}
			}
			else do
			{
				char optChar = *arg.data++;
				--arg.size;

				struct Args_Option* option = NULL; {
					intptr_t index = (uint8_t)optChar - 32;

					if (index < 96)
						option = ctx->shortOptions[index];

					if (option == NULL)
					{
						ReportError(ctx, "-%c unrecognized option\n", optChar);
						continue;
					}
				}

				struct Papyrus_String value;
				if (option->private.flags & OptionFlags_Value)
				{
					if (arg.size > 0)
					{
						value = arg;
					}
					else
					{
						if (++i == argCount)
						{
							ReportError(ctx, "-%c expected value\n", optChar);
							continue;
						}

						value = Papyrus_String_FromCString(argv[i]);
					}
					++valueCount;
				}
				else value.size = 0;

				ProcessOption(ctx, option, value);
			} while (arg.size > 0);
			break;

		case 2: // long option
			if (arg.size == 0)
			{
				// after '--' rest are considered positional
				acceptOption = false;
			}
			else
			{
				struct Papyrus_String value;
				bool eq = SplitString(arg, '=', &arg, &value);

				struct Args_Option* option; {
					struct Args_Option** mapval =
						OptMap_Find(&ctx->optionTable, &arg);

					if (mapval == NULL)
					{
						ReportError(ctx,
							"--%.*s unrecognized option\n",
							(int)arg.size, arg.data);
						continue;
					}

					option = *mapval;
				}

				if (option->private.flags & OptionFlags_Value)
				{
					if (eq == false)
					{
						if (++i == argCount)
						{
							ReportError(ctx,
								"--%.*s expected value\n",
								(int)arg.size, arg.data);
							continue;
						}

						value = Papyrus_String_FromCString(argv[i]);
					}
					++valueCount;
				}
				else if (eq)
				{
					ReportError(ctx,
						"--%.*s value not expected\n",
						(int)arg.size, arg.data);
					continue;
				}

				ProcessOption(ctx, option, value);
			}
			break;
		}
	}

	FOREACHV_S(opt, opt_i, &ctx->options)
	{
		if (opt->private.count < opt->private.min)
		{
			ReportError(ctx,
				"not enough values for argument: '%.*s'\n",
				(int)opt->private.disp.size, opt->private.disp.data);
		}
	}
}

static int
Option_Compare(const void* ap, const void* bp)
{
	const struct Args_Option* a = *(const struct Args_Option**)ap;
	const struct Args_Option* b = *(const struct Args_Option**)bp;
	return Papyrus_String_Compare(a->private.disp, b->private.disp);
}

static void
PrintHelp(Ctx* ctx)
{
	qsort(ctx->options.data, ctx->options.size,
		sizeof(struct Args_Option*), &Option_Compare);

	fwrite(ctx->helpText.data, ctx->helpText.size, 1, stdout);

	FOREACHV_S(opt, opt_i, &ctx->options)
	{
		struct Papyrus_String name = Papyrus_String_FromCString(opt->name);
		printf("  %.*s", (int)name.size, name.data);

		const char* desc = opt->desc;
		if (desc != NULL && *desc != '\0')
		{
			char padding[] = "                        ";
			intptr_t paddingSize = sizeof(padding) - 1;

			paddingSize = MAX(paddingSize - name.size, 1);
			printf("%.*s%s", (int)paddingSize, padding, desc);
		}
		printf("\n");

		const char* help = opt->help;
		if (help != NULL && *help != '\0')
		{
			printf("      %s\n", help);
		}
	}
}

struct Args*
Papyrus_Args_Parse(struct Args_Option** options,
	intptr_t optionCount, const char* const* args, intptr_t argCount,
	struct Papyrus_ArenaPool* pool, struct Papyrus_Allocator allocator)
{
	Ctx ctx;
	ctx.allocator = allocator;
	Arena_Init(&ctx.arena, pool);
	ctx.options.data = options;
	ctx.options.size = optionCount;

	Array_Init(&ctx.positional);
	OptArray_Reserve(&ctx.positional, optionCount,
		Arena_CreateAllocator(&ctx.arena));

	HashTable_Init(&ctx.optionTable);
	OptMap_Reserve(&ctx.optionTable, optionCount,
		Arena_CreateAllocator(&ctx.arena));

	memset(ctx.shortOptions, 0, sizeof(ctx.shortOptions));

	ctx.helpOption = NULL;
	ctx.helpText.size = 0;

	FOREACHV(opt, opt_i, options, optionCount)
	{
		RegisterOption(&ctx, opt);
	}

	Parse(&ctx, args, argCount);

	struct Args* result = NULL;
	if (ctx.result)
	{
		intptr_t valueCount = ValArray_Size(&ctx.values);

		uintptr_t size = sizeof(struct Args) +
			valueCount * (sizeof(struct Papyrus_String));

		result = (struct Args*)allocator.func(
			allocator.context, NULL, 0, size);
		result->size = size;

		struct Papyrus_String* values = result->values;
		FOREACHV_S(opt, opt_i, &ctx.options)
		{
			if (opt->private.flags & OptionFlags_Value)
			{
				opt->values.data = values;
				opt->values.size = 0;

				values += opt->private.count;
			}
			else
			{
				opt->flag = opt->private.count > 0;
			}
		}

		Array_FOREACH(val, val_i, struct Value, &ctx.values)
		{
			struct Args_Option* option = val->option;
			option->values.data[option->values.size++] = val->value;
		}

		if (ctx.helpOption != NULL && ctx.helpOption->private.count > 0)
		{
			PrintHelp(&ctx);
		}
	}

	Arena_Destroy(&ctx.arena);
	return result;
}

void
Papyrus_Args_Delete(struct Args* args, struct Papyrus_Allocator allocator)
{
	allocator.func(allocator.context, args, args->size, 0);
}
