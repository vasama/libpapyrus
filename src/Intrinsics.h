#pragma once

#include <stdint.h>

#ifdef __INTELLISENSE__
#	define bsf32(x) ((uint32_t)(x))
#	define bsf64(x) ((uint32_t)(x))

#	define clz32(x) ((uint32_t)(x))
#	define clz64(x) ((uint32_t)(x))

#	define ctz32(x) ((uint32_t)(x))
#	define ctz64(x) ((uint32_t)(x))

#	define bsr32(x) ((uint32_t)(x))
#	define bsr64(x) ((uint32_t)(x))

#	define bswap16(x) ((uint16_t)(x))
#	define bswap32(x) ((uint32_t)(x))
#	define bswap64(x) ((uint64_t)(x))
#else
#	define bsf32(x) ((uint32_t)__builtin_ctz((unsigned)(x)))
#	define bsf64(x) ((uint32_t)__builtin_ctzll((unsigned long long)(x)))

#	define clz32(x) ((uint32_t)__builtin_clz((unsigned)(x)))
#	define clz64(x) ((uint32_t)__builtin_clzll((unsigned long long)(x)))

#	define ctz32(x) bsf32(x)
#	define ctz64(x) bsf64(x)

#	define bsr32(x) ((uint32_t)(clz32(x) ^ 31))
#	define bsr64(x) ((uint32_t)(clz64(x) ^ 63))

#	define bswap16(x) __builtin_bswap16(x)
#	define bswap32(x) __builtin_bswap32(x)
#	define bswap64(x) __builtin_bswap64(x)
#endif
