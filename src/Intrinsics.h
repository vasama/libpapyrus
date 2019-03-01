#pragma once

#include <stdint.h>

#if defined(__clang) || defined(__GNUC__)
#	define bsf32(x) ((uint32_t)__builtin_ctz((unsigned)(x)))
#	define bsf64(x) ((uint32_t)__builtin_ctzll((unsigned long long)(x)))

#	define bsr32(x) ((uint32_t)__builtin_clz((unsigned)(x)))
#	define bsr64(x) ((uint32_t)__builtin_clzll((unsigned long long)(x)))

#	define bswap16(x) __builtin_bswap16(x)
#	define bswap32(x) __builtin_bswap32(x)
#	define bswap64(x) __builtin_bswap64(x)
#elif defined(_MSC_VER)
#	include <intrin.h>
#	include <stdlib.h>

static inline uint32_t
bsf32(uint32_t x)
{
	unsigned long r;
	_BitScanForward(&r, x);
	return r;
}

static inline uint32_t
bsf64(uint64_t x)
{
	unsigned long r;
	_BitScanForward64(&r, x);
	return r;
}

static inline uint32_t
bsr32(uint32_t x)
{
	unsigned long r;
	_BitScanReverse(&r, x);
	return r;
}

static inline uint32_t
bsr64(uint64_t x)
{
	unsigned long r;
	_BitScanReverse64(&r, x);
	return r;
}

#	define bswap16(x) ((uint16_t)_byteswap_ushort((unsigned short)(x)))
#	define bswap32(x) ((uint32_t)_byteswap_ulong((unsigned long)(x)))
#	define bswap64(x) ((uint64_t)_byteswap_uint64((unsigned __int64)(x)))
#endif

#	define ctz32(x) bsf32(x)
#	define ctz64(x) bsf64(x)

#	define clz32(x) ((uint32_t)(bsr32(x) ^ 31))
#	define clz64(x) ((uint32_t)(bsr64(x) ^ 63))
