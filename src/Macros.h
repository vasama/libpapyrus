#pragma once

#include <assert.h>
#include <stdint.h>

#ifndef static_assert
#	define static_assert(expr, message) \
		struct static_assert_ ## __LINE__ { char x[(expr) ? 1 : -1]; }
#endif

#if defined(__clang__) || defined(__GNUC__)
#	define LIKELY(...) __builtin_expect((__VA_ARGS__), 1)
#	define UNLIKELY(...) __builtin_expect((__VA_ARGS__), 0)
#else
#	define LIKELY(...) (__VA_ARGS__)
#	define UNLIKELY(...) (__VA_ARGS__)
#endif

#define SIZE(x) (sizeof(x) / (sizeof(*(x))))

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

// round x up to a, where a = 2^n
#define ALIGN(x, a) ((x) + (a) - 1 & -(a))

// SWAP(T* a, T* b)
#define SWAP(a, b) \
	do { \
		void* SWAP_a = (a); \
		void* SWAP_b = (b); \
		enum { SWAP_s = sizeof(*(a)) }; \
		static_assert(SWAP_s == sizeof(*(b)), "SWAP: Mismatched operand size"); \
		char SWAP_t[SWAP_s]; \
		memcpy(SWAP_t, SWAP_a, SWAP_s); \
		memcpy(SWAP_a, SWAP_b, SWAP_s); \
		memcpy(SWAP_b, SWAP_t, SWAP_s); \
	} while (0)

/* FOREACH(element-var, index-var, type, ...)
A convenience macro for easy iteration over array elements.

element-var: names a variable containing, depending on the for-each variant,
	either a pointer to the current element, or a copy of the current element.
index-var: names a variable containing the index of the current element.
type: the type of the elements in the array.

The variants suffixed with V (FOREACHV, FOREACHV_*) copy the element to
element-var. */

// FOREACH(element-var, index-var, type, data, size)
// data: pointer to the elements of the array.
// size: size of the array, in elements.
#define FOREACH(xvar, ivar, type, data, size) \
	FOREACHP_(xvar, ivar, type, (data), (size))

#define FOREACHV(xvar, ivar, type, data, size) \
	FOREACHV_(xvar, ivar, type, (data), (size))

// FOREACH_S(element-var, index-var, type, span)
// span: pointer to a `struct { type* data; intptr_t size; }`.
#define FOREACH_S(xvar, ivar, type, span) \
	FOREACHP_(xvar, ivar, type, (span)->data, (span)->size)

#define FOREACHV_S(xvar, ivar, type, span) \
	FOREACHV_(xvar, ivar, type, (span)->data, (span)->size)

#define FOREACH_(xvar, ivar, pt, getp, getc, xt, getx) \
	for (intptr_t FOREACH_i = 0, FOREACH_c = getc, \
		FOREACH_x = 1, ivar; FOREACH_x; (void)ivar) \
	for (pt* FOREACH_p = getp; FOREACH_x; FOREACH_x = 0) \
	for (xt xvar; FOREACH_i < FOREACH_c && \
		(xvar = getx, ivar = FOREACH_i, 1); ++FOREACH_i)

#define FOREACHP_(xvar, ivar, t, getp, getc) \
	FOREACH_(xvar, ivar, t, getp, getc, t*, FOREACH_p + FOREACH_i)

#define FOREACHV_(xvar, ivar, t, getp, getc) \
	FOREACH_(xvar, ivar, t const, getp, getc, t, FOREACH_p[FOREACH_i])
