#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct List
{
	struct List* prev;
	struct List* next;
};

static inline void
List_Init(struct List* list)
{
	list->prev = list;
	list->next = list;
}

static inline bool
List_IsEmpty(const struct List* list)
{
	return list->next == list;
}

static inline bool
List_IsSingleton(const struct List* list)
{
	struct List* next = list->next;
	return next != list && next == list->prev;
}

static inline bool
List_HasMany(const struct List* list)
{
	return list->next != list->prev;
}

static inline void
List_InsertAfter(struct List* list, struct List* link)
{
	struct List* next = list->next;

	next->prev = link;
	link->next = next;
	list->next = link;
	link->prev = list;
}

static inline void
List_InsertBefore(struct List* list, struct List* link)
{
	struct List* prev = list->prev;

	prev->next = link;
	link->prev = prev;
	list->prev = link;
	link->next = list;
}

static inline void
List_Remove(struct List* link)
{
	struct List* prev = link->prev;
	struct List* next = link->next;

	prev->next = next;
	next->prev = prev;
}

#define List_GetObject(link, type, field) \
	((type*)((char*)link - offsetof(type, field)))

#define List_FOREACH_(xvar, ivar, type, field, list, xt, xo) \
	for (intptr_t List_FOREACH_i = 0, List_FOREACH_x = 1, ivar; \
		List_FOREACH_x; (void)ivar) \
	for (struct List *List_FOREACH_l = (list), \
		*List_FOREACH_n = List_FOREACH_l; List_FOREACH_x;) \
	for (xt xvar; (List_FOREACH_n = List_FOREACH_n->next) != \
		List_FOREACH_l && (xvar = xo List_GetObject(List_FOREACH_n, \
		type, field), ivar = List_FOREACH_i, 1); ++List_FOREACH_i)

#define List_FOREACH(xvar, ivar, type, field, list) \
	List_FOREACH_(xvar, ivar, type, field, list, type*,)

#define List_FOREACHV(xvar, ivar, type, field, list) \
	List_FOREACH_(xvar, ivar, type, field, list, type, *)
