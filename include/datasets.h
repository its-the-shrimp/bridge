#ifndef _DATASETS_H
#define _DATASETS_H

#ifdef DS_NOSTDLIB
/*
if you want to use the library without a standard library, you need to define at least the following symbols from the standard library:
	types: size_t, uint32_t, int64_t, va_list, bool;
	functions: va_start, va_arg, va_end, memcpy, memmove;
	constants: true, false;
*/
#else
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#endif // DS_NOSTDLIB

#define DS_DEF static __attribute__((unused))
#define _DS_CAT(x, y) x##y
#define DS_CAT(x, y) _DS_CAT(x, y)
#define DS_TEMP DS_CAT(_ds_t, __LINE__)

static inline void nothing(void){}
 
#define arrayForeach(T, item, array) arrayForeach_as(T, T##Array, item, array)
#define arrayForeach_as(T, name, item, array) \
	nothing(); \
 	const name DS_TEMP = (array); \
 	for(T* item = DS_TEMP.data; (size_t)(item - DS_TEMP.data) < DS_TEMP.length; ++item)
#define arrayRevForeach(T, item, arrayRev) arrayRevForeach_as(T, T##Array, item, arrayRev)
#define arrayRevForeach_as(T, name, item, array) \
	nothing(); \
	const name DS_TEMP = (array); \
	if (DS_TEMP.length) \
		for (T* item = DS_TEMP.data + DS_TEMP.length - 1; item >= DS_TEMP.data; --item)
#define chainForeach(T, item, chain) chainForeach_as(T, T##Chain, item, chain)
#define chainForeach_as(T, name, item, chain) \
	nothing(); \
	const name DS_TEMP = (chain); \
	if (DS_TEMP.start) \
		for (T* item = &DS_TEMP.start->data; item != &DS_TEMP.end->next->data; item = &((name##Node*)((void**)item - 1))->next->data)
#define chainRevForeach(T, item, chainRev) chainRevForeach_as(T, T##Chain, item, chainRev)
#define chainRevForeach_as(T, name, item, chain) \
	nothing(); \
	const name DS_TEMP = (chain); \
	if (DS_TEMP.start) \
		for (T* item = &DS_TEMP.end->data; item != &DS_TEMP.start->prev->data; item = &((name##Node*)((void**)item - 1))->prev->data)

#define repeat(n) for(int64_t DS_TEMP = (n); DS_TEMP > 0; --DS_TEMP)
#define ARRAY_GROWTH(x) (((x) / 2) | 1)

typedef void* (*ds_alloc_f)(size_t n_bytes);
typedef void* (*ds_realloc_f)(void* ptr, size_t n_bytes_old, size_t n_bytes_new);
typedef void* (*ds_dealloc_f)(void* ptr, size_t n_bytes);

#define ds_defaultAlloc(n_bytes) malloc(n_bytes)
#define ds_defaultRealloc(ptr, n_bytes_old, n_bytes_new) realloc(ptr, n_bytes_new)
#define ds_defaultDealloc(ptr, n_bytes) free(ptr)

#define defArray(T) \
	declArray(T); \
	implArray(T)
#define defArray_as(T, name) \
	declArray_as(T, name); \
	implArray_as(T, name)
#define defArray_customAllocator(T, _alloc_f, _realloc_f, _dealloc_f) \
	declArray(T); \
	implArray_customAllocator(T, _alloc_f, _realloc_f, _dealloc_f) 
#define defArray_customAllocator_as(T, name, _alloc_f, _realloc_f, _dealloc_f) \
	declArray_as(T, name); \
	implArray_customAllocator_as(T, name, _alloc_f, _realloc_f, _dealloc_f)

#define arrayhead(array) ((array).data + (array).length - 1)
#define declArray(T) declArray_as(T, T##Array)
#define declArray_as(T, name) \
	typedef struct { uint32_t length; uint32_t cap; T* data; } name; \
	DS_DEF name name##_new(int64_t n, ...); \
	DS_DEF T* name##_append(name* array, T object); \
	DS_DEF T* name##_prepend(name* array, T object); \
	DS_DEF T* name##_extend(name* array, name sub); \
	DS_DEF T* name##_get(name array, int64_t index); \
	DS_DEF T* name##_set(name* array, int64_t index, T object); \
	DS_DEF inline name name##_slice(name array, int64_t start, uint32_t length); \
	DS_DEF T* name##_move(name* array, int64_t index, uint32_t n, T dst[]); \
	DS_DEF bool name##_del(name* array, int64_t index, uint32_t n); \
	DS_DEF T name##_pop(name* array, int64_t index); \
	DS_DEF bool name##_insert(name* array, name sub, int64_t index); \
	DS_DEF bool name##_clear(name* array); \
	DS_DEF T* name##_incrlen(name* array, uint32_t n); \
	DS_DEF name name##_copy(name array); \
	DS_DEF bool name##_incrcap(name* array, uint32_t new_cap); \

#define implArray(T) implArray_customAllocator_as(T, T##Array, ds_defaultAlloc, ds_defaultRealloc, ds_defaultDealloc)
#define implArray_as(T, name) implArray_customAllocator_as(T, name, ds_defaultAlloc, ds_defaultRealloc, ds_defaultDealloc)
#define implArray_customAllocator(T, _alloc_f, _realloc_f, _dealloc_f) implArray_customAllocator_as(T, T##Array, _alloc_f, _realloc_f, _dealloc_f)
#define implArray_customAllocator_as(T, name, _alloc_f, _realloc_f, _dealloc_f) \
	DS_DEF name name##_new(int64_t n, ...) { \
		va_list args; \
		va_start(args, n); \
		name res; \
		res.length = n < 0 ? 0 : n; \
		if (n < 0) n = -n; \
		res.cap = n + ARRAY_GROWTH(n); \
		res.data = _alloc_f(res.cap * sizeof(T)); \
		for (uint32_t i = 0; i < res.length; ++i) { \
			res.data[i] = va_arg(args, T); \
		} \
		va_end(args); \
		return res; \
	} \
	DS_DEF T* name##_append(name* array, T object) { \
		if (array->cap < ++array->length) \
			if (!name##_incrcap(array, ARRAY_GROWTH(array->length))) { \
				--array->length; \
				return NULL; \
			} \
		return memcpy(&array->data[array->length - 1], &object, sizeof(T)); \
	} \
	DS_DEF T* name##_prepend(name* array, T object) { \
		if (array->cap < ++array->length) \
			if (!name##_incrcap(array, ARRAY_GROWTH(array->length))) { \
				--array->length; \
				return NULL; \
			} \
		return memcpy( \
			((T*)memmove(array->data + 1, array->data, (array->length - 1) * sizeof(T))) - 1, \
			&object, \
			sizeof(T) \
		); \
	} \
	DS_DEF T* name##_extend(name* array, name sub) { \
		if (array->cap < (array->length += sub.length)) \
			if (!name##_incrcap(array, array->length - array->cap + ARRAY_GROWTH(array->length))) { \
				array->length -= sub.length; \
				return NULL; \
			} \
		return memcpy(array->data + array->length - sub.length, sub.data, sub.length * sizeof(T)); \
	} \
	DS_DEF T* name##_get(name array, int64_t index) { \
		index = index >= 0 ? index : array.length + index; \
		if ((uint64_t)index >= array.length) return NULL; \
		return array.data + index; \
	} \
	DS_DEF T* name##_set(name* array, int64_t index, T object) { \
		index = index >= 0 ? index : array->length + index; \
		if ((uint64_t)index >= array->length) return NULL; \
		array->data[index] = object; \
		return &array->data[index]; \
	} \
	DS_DEF T* name##_move(name* array, int64_t index, uint32_t n, T dst[]) { \
		index = index >= 0 ? index : array->length + index; \
		if (index + n > array->length) return NULL; \
		memmove(dst, array->data + index, n * sizeof(T)); \
		memmove(array->data + index, array->data + index + n, ((array->length -= n) - index) * sizeof(T)); \
		if (array->cap > array->length + ARRAY_GROWTH(array->length)) { \
			void* res = _realloc_f(array->data, array->cap * sizeof(T), (array->cap -= n) * sizeof(T)); \
			if (res || !array->length) { \
				array->data = res; \
				return dst; \
			} \
			array->cap += n; \
			array->length += n; \
			return NULL; \
		} \
		return dst; \
	} \
	DS_DEF bool name##_del(name* array, int64_t index, uint32_t n) { \
		T deleted[n]; \
		index = index >= 0 ? index : array->length + index; \
		if (index + n > array->length) return false; \
		if (!name##_move(array, index, n, deleted)) { \
			for (uint64_t i = index; i < (uint32_t)(index + n); ++i) { \
				array->data[i + n] = array->data[i]; \
				array->data[i] = deleted[i - index]; \
			} \
			return false; \
		} \
		return true; \
	} \
	DS_DEF T name##_pop(name* array, int64_t index) { \
		T res = (T){0}; \
		if (!name##_move(array, index, 1, &res)) return (T){0}; \
		return res; \
	} \
	DS_DEF bool name##_insert(name* array, name sub, int64_t index) { \
		index = index >= 0 ? index : array->length + index; \
		if ((uint64_t)index >= array->length) return false; \
		if (array->cap < (array->length += sub.length - 1)) \
			if (!name##_incrcap(array, array->length - array->cap + ARRAY_GROWTH(array->length))) { \
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
	DS_DEF T* name##_incrlen(name* array, uint32_t n) { \
		if ((array->length += n) > array->cap) \
			if (!name##_incrcap(array, array->length - array->cap + ARRAY_GROWTH(array->length))) return NULL; \
		return &array->data[array->length - n]; \
	} \
	DS_DEF bool name##_clear(name* array) { \
		if (!array->length) return true; \
		_dealloc_f(array->data, array->cap * sizeof(T)); \
		*array = (name){0}; \
		return true; \
	} \
	DS_DEF inline name name##_slice(name array, int64_t start, uint32_t length) { \
		if (start < 0) start = array.length - start;\
		array.data += start; \
		array.length = length; \
		array.cap = 0; \
		return array; \
	} \
	DS_DEF name name##_copy(name array) { \
		array.data = memcpy(_alloc_f(array.length * sizeof(T)), array.data, array.length * sizeof(T)); \
		array.cap = array.length; \
		return array; \
	} \
	DS_DEF bool name##_incrcap(name* array, uint32_t n) { \
		register T* new_ptr = _realloc_f(array->data, array->cap * sizeof(T), (array->cap += n) * sizeof(T)); \
		if (!new_ptr) { \
			array->cap -= n; \
			return false; \
		} \
		array->data = new_ptr; \
		return true; \
	} \

#define defChain(T) \
	declChain(T); \
	implChain(T)
#define defChain_as(T, name) \
	declChain_as(T, name); \
	implChain_as(T, name)
#define defChain_customAllocator(T, _alloc_f, _dealloc_f) \
	declChain(T); \
	implChain_customAllocator(T, _alloc_f, _dealloc_f) 
#define defChain_customAllocator_as(T, name, _alloc_f, _dealloc_f) \
	declChain_as(T, name); \
	implChain_customAllocator_as(T, name, _alloc_f, _dealloc_f)

#define declChain(T) declChain_as(T, T##Chain)
#define declChain_as(T, name) \
	typedef struct name##Node name##Node; \
	struct name##Node { name##Node* prev; T data; name##Node* next; }; \
	typedef struct { name##Node* start; name##Node* end; } name; \
	DS_DEF name name##_new(uint64_t n, ...); \
	DS_DEF T* name##_append(name* chain, T object); \
	DS_DEF T* name##_extend(name* chain, name sub); \
	DS_DEF T name##_get(name chain, int64_t index); \
	DS_DEF T* name##_getref(name chain, int64_t index); \
	DS_DEF void name##_set(name* chain, int64_t index, T object); \
	DS_DEF T name##_pop(name* chain, int64_t index); \
	DS_DEF bool name##_insert(name* chain, name sub, int64_t index); \
	DS_DEF bool name##_clear(name* chain); \
	DS_DEF inline name name##_slice(name chain, int64_t start, int64_t end) \

#define implChain(T) implChain_customAllocator_as(T, T##Chain, ds_defaultAlloc, ds_defaultDealloc)
#define implChain_as(T) implChain_customAllocator_as(T, name, ds_defaultAlloc, ds_defaultDealloc)
#define implChain_customAllocator(T, _alloc_f, _dealloc_f) implChain_customAllocator_as(T, T##Chain, _alloc_f, _dealloc_f)
#define implChain_customAllocator_as(T, name, _alloc_f, _dealloc_f) \
	DS_DEF name name##_new(uint64_t n, ...) { \
		if (n == 0) return (name){0}; \
		va_list args; \
		va_start(args, n); \
		name res; \
		name##Node* _res = _alloc_f(sizeof(name##Node)); \
		_res->prev = NULL; \
		_res->data = va_arg(args, T); \
		_res->next = NULL; \
		res.start = res.end = _res; \
		for (uint64_t i = 1; i < n; ++i) { \
			_res = _alloc_f(sizeof(name##Node));  \
			_res->prev = res.end; \
			_res->data = va_arg(args, T); \
			_res->next = NULL; \
			res.end->next = _res; \
			res.end = _res; \
		} \
		return res; \
	} \
	DS_DEF T* name##_append(name* chain, T obj) { \
		name##Node* _res = _alloc_f(sizeof(name##Node)); \
		if (!_res) return NULL; \
		_res->prev = chain->end; \
		_res->next = NULL; \
		_res->data = obj; \
		if (!chain->start) { \
			chain->start = _res; \
		} else chain->end->next = _res; \
		chain->end = _res; \
		return &chain->end->data; \
	} \
	DS_DEF name##Node* name##_getnode(name chain, int64_t index) { \
		name##Node* res; \
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
	DS_DEF T name##_get(name chain, int64_t index) { \
		name##Node* res = name##_getnode(chain, index); \
		return res ? res->data : (T){0}; \
	} \
	DS_DEF T* name##_getref(name chain, int64_t index) { \
		name##Node* res = name##_getnode(chain, index); \
		return res ? &res->data : NULL; \
	} \
	DS_DEF void name##_set(name* chain, int64_t index, T object) { \
		name##Node* res = name##_getnode(*chain, index); \
		res->data = object; \
	} \
	DS_DEF bool name##_insert(name* chain, name sub, int64_t index) { \
		name##Node* prev = name##_getnode(*chain, index); \
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
	DS_DEF T name##_pop(name* chain, int64_t index) { \
		name##Node* node = name##_getnode(*chain, index); \
		if (node == chain->start) chain->start = chain->start->next; \
		if (node == chain->end) chain->end = chain->end->prev; \
		if (node->prev) node->prev->next = node->next; \
		if (node->next) node->next->prev = node->prev; \
		T res = node->data; \
		_dealloc_f(node, sizeof(name##Node)); \
		return res; \
	} \
	DS_DEF uint64_t name##_length(name chain) { \
		uint64_t res = 0; \
		chainForeach_as(T, name, item, chain) res += 1; \
		return res; \
	} \
	DS_DEF bool name##_clear(name* chain) { \
		if (!chain->end) return true; \
		name##Node* nextnode; \
		for (name##Node* node = chain->start; node != NULL; node = nextnode) { \
			nextnode = node->next; \
			_dealloc_f(node, sizeof(name##Node)); \
		} \
		chain->end = NULL; \
		chain->start = NULL; \
		return true; \
	} \
	DS_DEF inline name name##_slice(name chain, int64_t start, int64_t end) { \
		name##Node* first = name##_getnode(chain, start); \
		chain.end = name##_getnode(chain, end); \
		chain.start = first; \
		return chain; \
	} \

#endif // _DATASETS_H
