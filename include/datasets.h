#ifndef _DATASETS_H
#define _DATASETS_H

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <br_utils.h>
#include <math.h>

#define DS_DEF static __attribute__((unused))

#define arrayForeach(T, item, array) \
const T##Array TEMPVAR=(array);for(T*item=TEMPVAR.data;(uint64_t)(item-TEMPVAR.data)<TEMPVAR.length;++item)
#define arrayRevForeach(T, item, array) \
const T##Array TEMPVAR=(array);for(T*item=TEMPVAR.data+TEMPVAR.length-1;item!=TEMPVAR.data;--item)
#define chainForeach(T, item, chain) \
const T##Chain TEMPVAR=(chain);if(TEMPVAR.start)for(T*item=&TEMPVAR.start->value;item!=&TEMPVAR.end->next->value;item=&((T##Node*)((void**)item-1))->next->value)
#define chainRevForeach(T, item, chain) \
const T##Chain TEMPVAR=(chain);if(TEMPVAR.start)for(T*item=&TEMPVAR.end->value;item!=&TEMPVAR.start->prev->value;item=&((T##Node*)((void**)item-1))->prev->value)

#define repeat(n) for(uint64_t TEMPVAR = (n); TEMPVAR > 0; --TEMPVAR)

#define _str(T) #T
#define ARRAY_GROWTH(x) (unsigned)ceilf((float)x / 2.0)

#define arrayhead(array) ((array).data + (array).length - 1)
#define declArray(T) \
	typedef struct { uint32_t length; uint32_t cap; T* data; } T##Array; \
	DS_DEF T##Array T##Array_new(int64_t n, ...); \
	DS_DEF T* T##Array_append(T##Array* array, T object); \
	DS_DEF T* T##Array_prepend(T##Array* array, T object); \
	DS_DEF T* T##Array_extend(T##Array* array, T##Array sub); \
	DS_DEF T* T##Array_get(T##Array array, int64_t index); \
	DS_DEF T* T##Array_set(T##Array* array, int64_t index, T object); \
	DS_DEF inline T##Array T##Array_slice(T##Array array, int64_t start, uint32_t length); \
	DS_DEF T* T##Array_move(T##Array* array, int64_t index, uint32_t n, T dst[]); \
	DS_DEF bool T##Array_del(T##Array* array, int64_t index, uint32_t n); \
	DS_DEF T T##Array_pop(T##Array* array, int64_t index); \
	DS_DEF bool T##Array_insert(T##Array* array, T##Array sub, int64_t index); \
	DS_DEF bool T##Array_clear(T##Array* array); \
	DS_DEF T* T##Array_incrlen(T##Array* array, uint32_t n); \
	DS_DEF T##Array T##Array_copy(T##Array array); \
	DS_DEF bool T##Array_incrcap(T##Array* array, uint32_t new_cap); \

#define defArray(T) \
	DS_DEF T##Array T##Array_new(int64_t n, ...) { \
		va_list args; \
		va_start(args, n); \
		T##Array res; \
		res.length = n < 0 ? 0 : n; \
		if (n < 0) n = -n; \
		res.cap = n + ARRAY_GROWTH(n); \
		res.data = malloc(res.cap * sizeof(T)); \
		for (uint32_t i = 0; i < res.length; ++i) { \
			res.data[i] = va_arg(args, T); \
		} \
		va_end(args); \
		return res; \
	} \
	DS_DEF T* T##Array_append(T##Array* array, T object) { \
		if (array->cap < ++array->length) \
			if (!T##Array_incrcap(array, ARRAY_GROWTH(array->length))) { \
				--array->length; \
				return NULL; \
			} \
		return memcpy(&array->data[array->length - 1], &object, sizeof(T)); \
	} \
	DS_DEF T* T##Array_prepend(T##Array* array, T object) { \
		if (array->cap < ++array->length) \
			if (!T##Array_incrcap(array, ARRAY_GROWTH(array->length))) { \
				--array->length; \
				return NULL; \
			} \
		return memcpy( \
			(T*)memmove(array->data + 1, array->data, array->length++ * sizeof(T)) - 1, \
			&object, \
			sizeof(T) \
		); \
	} \
	DS_DEF T* T##Array_extend(T##Array* array, T##Array sub) { \
		if (array->cap < (array->length += sub.length)) \
			if (!T##Array_incrcap(array, ARRAY_GROWTH(array->length))) { \
				array->length -= sub.length; \
				return NULL; \
			} \
		return memcpy(array->data + array->length, sub.data, sub.length * sizeof(T)); \
	} \
	DS_DEF T* T##Array_get(T##Array array, int64_t index) { \
		index = index >= 0 ? index : array.length + index; \
		if ((uint64_t)index >= array.length) return NULL; \
		return array.data + index; \
	} \
	DS_DEF T* T##Array_set(T##Array* array, int64_t index, T object) { \
		index = index >= 0 ? index : array->length + index; \
		if ((uint64_t)index >= array->length) return NULL; \
		array->data[index] = object; \
		return &array->data[index]; \
	} \
	DS_DEF T* T##Array_move(T##Array* array, int64_t index, uint32_t n, T dst[]) { \
		index = index >= 0 ? index : array->length + index; \
		if (index + n > array->length) return NULL; \
		memmove(dst, array->data + index, n * sizeof(T)); \
		memmove(array->data + index, array->data + index + n, ((array->length -= n) - index) * sizeof(T)); \
		if (array->cap > array->length + ARRAY_GROWTH(array->length)) { \
			void* res = realloc(array->data, (array->cap -= n) * sizeof(T)); \
			if (res || array->length == n) { \
				array->data = res; \
				return dst; \
			} \
			return NULL; \
		} \
		return dst; \
	} \
	DS_DEF bool T##Array_del(T##Array* array, int64_t index, uint32_t n) { \
		T deleted[n]; \
		index = index >= 0 ? index : array->length + index; \
		if (index + n > array->length) return false; \
		if (!T##Array_move(array, index, n, deleted)) { \
			for (uint64_t i = index; i < (uint32_t)(index + n); ++i) { \
				array->data[i + n] = array->data[i]; \
				array->data[i] = deleted[i - index]; \
			} \
			return false; \
		} \
		return true; \
	} \
	DS_DEF T T##Array_pop(T##Array* array, int64_t index) { \
		T res = (T){0}; \
		if (!T##Array_move(array, index, 1, &res)) return (T){0}; \
		return res; \
	} \
	DS_DEF bool T##Array_insert(T##Array* array, T##Array sub, int64_t index) { \
		index = index >= 0 ? index : array->length + index; \
		if ((uint64_t)index >= array->length) return false; \
		if (array->cap < (array->length += sub.length - 1)) \
			if (!T##Array_incrcap(array, ARRAY_GROWTH(array->length))) { \
				array->length -= sub.length - 1; \
				return false; \
			} \
		for (uint64_t i = array->length - sub.length - 1; i >= (uint64_t)index; --i) { \
			array->data[i + sub.length] = array->data[i]; \
		} \
		for (uint64_t i = index; i < (uint32_t)(index + sub.length); ++i) { \
			array->data[i] = sub.data[i - index]; \
		} \
		return true; \
	} \
	DS_DEF T* T##Array_incrlen(T##Array* array, uint32_t n) { \
		if ((array->length += n) > array->cap) \
			if (!T##Array_incrcap(array, ARRAY_GROWTH(array->length))) return NULL; \
		return &array->data[array->length - n]; \
	} \
	DS_DEF bool T##Array_clear(T##Array* array) { \
		if (!array->length) return true; \
		free(array->data); \
		*array = (T##Array){0}; \
		return true; \
	} \
	DS_DEF inline T##Array T##Array_slice(T##Array array, int64_t start, uint32_t length) { \
		if (start < 0) start = array.length - start;\
		array.data += start; \
		array.length = length; \
		array.cap = 0; \
		return array; \
	} \
	DS_DEF T##Array T##Array_copy(T##Array array) { \
		array.data = memcpy(malloc(array.cap * sizeof(T)), array.data, array.cap * sizeof(T)); \
		return array; \
	} \
	DS_DEF bool T##Array_incrcap(T##Array* array, uint32_t n) { \
		register T* new_ptr = realloc(array->data, (array->cap += n) * sizeof(T)); \
		if (!new_ptr) { \
			array->cap -= n; \
			return false; \
		} \
		array->data = new_ptr; \
		return true; \
	} \

#define declChain(T) \
	typedef struct T##_node { struct T##_node* prev; T value; struct T##_node* next; } T##Node; \
	typedef struct T##_chain { T##Node* start; T##Node* end; } T##Chain; \
	DS_DEF T##Chain T##Chain_new(uint64_t n, ...); \
	DS_DEF T* T##Chain_append(T##Chain* chain, T object); \
	DS_DEF T* T##Chain_extend(T##Chain* chain, T##Chain sub); \
	DS_DEF T T##Chain_get(T##Chain chain, int64_t index); \
	DS_DEF T* T##Chain_getref(T##Chain chain, int64_t index); \
	DS_DEF void T##Chain_set(T##Chain* chain, int64_t index, T object); \
	DS_DEF T T##Chain_pop(T##Chain* chain, int64_t index); \
	DS_DEF bool T##Chain_insert(T##Chain* chain, T##Chain sub, int64_t index); \
	DS_DEF bool T##Chain_clear(T##Chain* chain); \
	DS_DEF inline T##Chain T##Chain_slice(T##Chain chain, int64_t start, int64_t end); \

#define defChain(T) \
	DS_DEF T##Chain T##Chain_new(uint64_t n, ...) { \
		if (n == 0) return (T##Chain){0}; \
		va_list args; \
		va_start(args, n); \
		T##Chain res; \
		T##Node* _res = malloc(sizeof(T##Node)); \
		_res->prev = NULL; \
		_res->value = va_arg(args, T); \
		_res->next = NULL; \
		res.start = res.end = _res; \
		for (uint64_t i = 1; i < n; ++i) { \
			_res = malloc(sizeof(T##Node));  \
			_res->prev = res.end; \
			_res->value = va_arg(args, T); \
			_res->next = NULL; \
			res.end->next = _res; \
			res.end = _res; \
		} \
		return res; \
	} \
	DS_DEF T* T##Chain_append(T##Chain* chain, T obj) { \
		T##Node* _res = malloc(sizeof(T##Node)); \
		if (!_res) return NULL; \
		_res->prev = chain->end; \
		_res->next = NULL; \
		_res->value = obj; \
		if (!chain->start) { \
			chain->start = _res; \
		} else chain->end->next = _res; \
		chain->end = _res; \
		return &chain->end->value; \
	} \
	DS_DEF T##Node* T##Chain_getnode(T##Chain chain, int64_t index) { \
		T##Node* res; \
		if (index >= 0) { \
			res = chain.start; \
			for (uint64_t i = 0; i < (uint64_t)index && res != NULL; ++i) res = res->next; \
		} else { \
			res = chain.end; \
			index = index * -1 - 1; \
			for (uint64_t i = 0; i < (uint64_t)index && res != NULL; ++i) res = res->prev; \
		} \
		return res; \
	} \
	DS_DEF T T##Chain_get(T##Chain chain, int64_t index) { \
		T##Node* res = T##Chain_getnode(chain, index); \
		return res ? res->value : (T){0}; \
	} \
	DS_DEF T* T##Chain_getref(T##Chain chain, int64_t index) { \
		T##Node* res = T##Chain_getnode(chain, index); \
		return res ? &res->value : NULL; \
	} \
	DS_DEF void T##Chain_set(T##Chain* chain, int64_t index, T object) { \
		T##Node* res = T##Chain_getnode(*chain, index); \
		res->value = object; \
	} \
	DS_DEF bool T##Chain_insert(T##Chain* chain, T##Chain sub, int64_t index) { \
		T##Node* prev = T##Chain_getnode(*chain, index); \
		if (!sub.start) return true; \
		if (!chain->start) { \
			chain->start = sub.start; \
			chain->end = sub.end; \
		} else if (prev == chain->end) { \
			sub.start->prev = chain->end; \
			chain->end->next = sub.start; \
			chain->end = sub.end; \
		} else if (!prev) { \
			sub.end->next = chain->start; \
			chain->start->prev = sub.end; \
			chain->start = sub.start; \
		} else { \
			sub.start->prev = prev; \
			sub.end->next = prev->next; \
			prev->next->prev = sub.end; \
			prev->next = sub.start; \
		} \
		return true; \
	} \
	DS_DEF T T##Chain_pop(T##Chain* chain, int64_t index) { \
		T##Node* node = T##Chain_getnode(*chain, index); \
		if (node == chain->start) chain->start = chain->start->next; \
		if (node == chain->end) chain->end = chain->end->prev; \
		if (node->prev) node->prev->next = node->next; \
		if (node->next) node->next->prev = node->prev; \
		T res = node->value; \
		free(node); \
		return res; \
	} \
	DS_DEF uint64_t T##Chain_length(T##Chain chain) { \
		uint64_t res = 0; \
		chainForeach(T, item, chain) res += 1; \
		return res; \
	} \
	DS_DEF bool T##Chain_clear(T##Chain* chain) { \
		if (!chain->end) return true; \
		T##Node* nextnode; \
		for (T##Node* node = chain->start; node != NULL; node = nextnode) { \
			nextnode = node->next; \
			free(node); \
		} \
		chain->end = NULL; \
		chain->start = NULL; \
		return true; \
	} \
	DS_DEF inline T##Chain T##Chain_slice(T##Chain chain, int64_t start, int64_t end) { \
		T##Node* first = T##Chain_getnode(chain, start); \
		chain.end = T##Chain_getnode(chain, end); \
		chain.start = first; \
		return chain; \
	} \

#endif // _DATASETS_H
