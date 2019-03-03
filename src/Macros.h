#pragma once

#include <assert.h>
#include <stdint.h>

#define CAT_(a, b) a ## b
#define CAT(a, b) CAT_(a, b)

#ifndef static_assert
#	define static_assert(expr, message) \
		struct CAT(static_assert_, __COUNTER__) { char x[(expr) ? 1 : -1]; }
#endif

#ifdef __INTELLISENSE__
#define LIKELY(...) __VA_ARGS__
#define UNLIKELY(...) __VA_ARGS__
#define NORETURN __declspec(noreturn)
#define UNUSED
#else
#define LIKELY(...) __builtin_expect((__VA_ARGS__), 1)
#define UNLIKELY(...) __builtin_expect((__VA_ARGS__), 0)
#define NORETURN __attribute__((noreturn))
#define UNUSED __attribute__((unused))
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
		static_assert(SWAP_s == sizeof(*(b)), \
			"SWAP: Mismatched operand size"); \
		char SWAP_t[SWAP_s]; \
		memcpy(SWAP_t, SWAP_a, SWAP_s); \
		memcpy(SWAP_a, SWAP_b, SWAP_s); \
		memcpy(SWAP_b, SWAP_t, SWAP_s); \
	} while (0)

/* FOREACH(element-var, index-var, ...)
A convenience macro for easy iteration over array elements.

element-var: names a variable containing, depending on the for-each variant,
	either a pointer to the current element, or a copy of the current element.
index-var: names a variable containing the index of the current element.

The variants suffixed with V (FOREACHV, FOREACHV_*) copy the element to
element-var. */

/* FOREACH(element-var, index-var, data, size)
data: pointer to the elements of the array.
size: size of the array, in elements. */
#define FOREACH(xvar, ivar, data, size) \
	FOREACHP_(xvar, ivar, 0, FOREACH_f, (data), FOREACH_f, (size))

#define FOREACHV(xvar, ivar, data, size) \
	FOREACHV_(xvar, ivar, 0, FOREACH_f, (data), FOREACH_f, (size))

#define FOREACH_f(x, c) c

#ifdef __INTELLISENSE__
#define FOREACH_(xvar, ivar, a, pf, pfc, cf, cfc, xf) \
	for (intptr_t ivar = cf((a), cfc);;) \
	for (auto xvar = xf(pf((a), pfc));;)
#else
#define FOREACH_(xvar, ivar, a, pf, pfc, cf, cfc, xf) \
	for (intptr_t FOREACH_i = 0, FOREACH_c, \
		FOREACH_x = 1, UNUSED ivar; FOREACH_x;) \
	for (__typeof__(a) UNUSED FOREACH_a = (a); \
		FOREACH_c = (cf(FOREACH_a, cfc)), FOREACH_x;) \
	for (__typeof__(*(pf(FOREACH_a, pfc)))* \
		FOREACH_p = pf(FOREACH_a, pfc); FOREACH_x; FOREACH_x = 0) \
	for (__typeof__(xf(FOREACH_p)) xvar; FOREACH_i < FOREACH_c && (xvar \
		= (xf((FOREACH_p + FOREACH_i))), ivar = FOREACH_i, 1); ++FOREACH_i)
#endif

#define FOREACHP_xf(x) x
#define FOREACHP_(xvar, ivar, a, pf, pfc, cf, cfc) \
	FOREACH_(xvar, ivar, a, pf, pfc, cf, cfc, FOREACHP_xf)

#define FOREACHV_xf(x) *x
#define FOREACHV_(xvar, ivar, a, pf, pfc, cf, cfc) \
	FOREACH_(xvar, ivar, a, pf, pfc, cf, cfc, FOREACHV_xf)


/* FOREACH_S(element-var, index-var, span)
span: pointer to struct { T* data; intptr_t size; } */
#define FOREACH_S(xvar, ivar, span) \
	FOREACHP_(xvar, ivar, &*(span), FOREACH_S_pf,, FOREACH_S_cf,)

#define FOREACHV_S(xvar, ivar, span) \
	FOREACHV_(xvar, ivar, &*(span), FOREACH_S_pf,, FOREACH_S_cf,)

#define FOREACH_S_pf(x, c) x->data
#define FOREACH_S_cf(x, c) x->size
