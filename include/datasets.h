#ifndef _DATASETS_H
#define _DATASETS_H

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <unistd.h>

#define arrayForeach(T, item, array) for (T* item = (array).data; item - (array).data < (array).length; ++item)
#define arrayRevForeach(T, item, array) for (T* item = (array).data + (array).length - 1; item != (array).data; --item)
#define chainForeachFrom(T, item, chain, start) for ( \
	T* item = &(T##Chain_getnode(chain, start)->value); \
	(size_t)item != sizeof(void*); \
	item = &(((T##Node*)((void*)item - sizeof(void*)))->next->value) \
)
#define chainForeach(T, item, chain) for ( \
	T* item = &(chain).start->value; \
	(size_t)item != sizeof(void*); \
	item = &(((T##Node*)((void**)item - 1))->next->value) \
)

#define _str(t) #t

#define arrayhead(array) ((array).data + (array).length - 1)
#define arrayctx(array) chunkctx((array).data)
#define declArray(t) \
	typedef struct { int length; t* data; } t##Array; \
	static t##Array t##Array_new(int n, ...); \
	static t* t##Array_append(t##Array* array, t object); \
	static t* t##Array_extend(t##Array* array, t##Array sub); \
	static t t##Array_get(t##Array array, int index); \
	static t* t##Array_getref(t##Array array, int index); \
	static void t##Array_set(t##Array* array, int index, t object); \
	static inline t##Array t##Array_slice(t##Array array, int start, int end); \
	static bool t##Array_del(t##Array* array, int index, size_t n); \
	static t t##Array_pop(t##Array* array, int index); \
	static bool t##Array_insert(t##Array* array, t##Array sub, int index); \
	static bool t##Array_clear(t##Array* array); \
	static int t##Array_length(t##Array array); \
	static t* t##Array_resize(t##Array* array, int n); \

#define defArray(t) \
	static t##Array t##Array_new(int n, ...) { \
		va_list args; \
		va_start(args, n); \
		t##Array res = (t##Array){  \
			.length = (n < 0 ? 0 : n), \
			.data = malloc(sizeof(t) * (n < 0 ? -n : n)), \
		}; \
		for (int i = 0; i < n; i++) { \
			res.data[i] = va_arg(args, t); \
		} \
		va_end(args); \
		return res; \
	} \
	static t* t##Array_append(t##Array* array, t object) { \
		void* res = realloc(array->data, (array->length + 1) * sizeof(t)); \
		if (!res) return NULL; \
		array->data = res; \
		array->data[array->length++] = object; \
		return arrayhead(*array); \
	} \
	static t* t##Array_extend(t##Array* array, t##Array sub) { \
		void* res = realloc(array->data, (array->length + sub.length) * sizeof(t)); \
		if (!res) return NULL; \
		array->data = res; \
		memcpy(array->data + array->length, sub.data, sub.length * sizeof(t)); \
		array->length += sub.length; \
		return arrayhead(*array); \
	} \
	static t t##Array_get(t##Array array, int index) { \
		index = index >= 0 ? index : array.length + index; \
		if (index > array.length - 1) return (t){0}; \
		return array.data[index]; \
	}; \
	static t* t##Array_getref(t##Array array, int index) { \
		index = index >= 0 ? index : array.length + index; \
		if (index > array.length - 1) return NULL; \
		return array.data + index; \
	} \
	static void t##Array_set(t##Array* array, int index, t object) { \
		*(array->data + (index >= 0 ? index : array->length + index)) = object; \
	} \
	static bool t##Array_move(t##Array* array, int index, size_t n, t dst[]) { \
		index = index >= 0 ? index : array->length + index; \
		if (index + n > array->length) return false; \
		if (dst != NULL) for (int i = index; i < index + n; i++) dst[i - index] = array->data[i]; \
		for (int i = index; i + n < array->length; i++) { \
			array->data[i] = array->data[i + n]; \
		} \
		void* res = realloc(array->data, (array->length - n) * sizeof(t)); \
		if (res) { \
			array->data = res; \
			array->length -= n; \
			return true; \
		} \
		return false; \
	} \
	static bool t##Array_del(t##Array* array, int index, size_t n) { \
		t deleted[n]; \
		index = index >= 0 ? index : array->length + index; \
		if (!t##Array_move(array, index, n, deleted)) { \
			for (int i = index; i < index + n; i++) { \
				array->data[i + n] = array->data[i]; \
				array->data[i] = deleted[i - index]; \
			} \
			return false; \
		} \
		return true; \
	} \
	static t t##Array_pop(t##Array* array, int index) { \
		t res = (t){0}; \
		if (!t##Array_move(array, index, 1, &res)) return (t){0}; \
		return res; \
	} \
	static bool t##Array_insert(t##Array* array, t##Array sub, int index) { \
		index = index >= 0 ? index : array->length + index; \
		if (index > array->length) { \
			return false; \
		} \
		void* res = realloc(array->data, (array->length + sub.length - 1) * sizeof(t)); \
		if (!res) { return false; } \
		array->data = res; \
		array->length += sub.length; \
		for (int i = array->length - sub.length - 1; i >= index; i--) { \
			array->data[i + sub.length] = array->data[i]; \
		} \
		for (int i = index; i < index + sub.length; i++) { \
			array->data[i] = sub.data[i - index]; \
		} \
		return true; \
	} \
	static t* t##Array_resize(t##Array* array, int n) { \
		t* res = realloc(array->data, (array->length + n) * sizeof(t)); \
		if (!res) return NULL; \
		memset(res + array->length, 0, n * sizeof(t)); \
		array->length += n; \
		array->data = res; \
		return array->data + array->length - n; \
	} \
	static int t##Array_length(t##Array array) { \
		return array.length; \
	} \
	static bool t##Array_clear(t##Array* array) { \
		if (!array->length) return true; \
		free(array->data); \
		array->data = malloc(0); \
		array->length = 0; \
		return true; \
	} \
	static inline t##Array t##Array_slice(t##Array array, int start, int end) { \
		if (start < 0) start = array.length - start;\
		if (end < 0) end = array.length - end + 1; \
		array.data += start; \
		array.length = end - start; \
		return array; \
	} \

#define declChain(t) \
	typedef struct t##_node { struct t##_node* prev; t value; struct t##_node* next; } t##Node; \
	typedef struct t##_chain { t##Node* start; t##Node* end; } t##Chain; \
	static t##Chain t##Chain_new(int n, ...); \
	static t* t##Chain_append(t##Chain* chain, t object); \
	static t* t##Chain_extend(t##Chain* chain, t##Chain sub); \
	static t t##Chain_get(t##Chain chain, int index); \
	static t* t##Chain_getref(t##Chain chain, int index); \
	static void t##Chain_set(t##Chain* chain, int index, t object); \
	static t t##Chain_pop(t##Chain* chain, int index); \
	static bool t##Chain_insert(t##Chain* chain, t##Chain sub, int index); \
	static bool t##Chain_clear(t##Chain* chain); \

#define defChain(t) \
	static t##Chain t##Chain_new(int n, ...) { \
		if (n == 0) return (t##Chain){0}; \
		va_list args; \
		va_start(args, n); \
		t##Chain res; \
		t##Node* _res = malloc(sizeof(t##Node)); \
		_res->prev = NULL; \
		_res->value = va_arg(args, t); \
		_res->next = NULL; \
		res.start = res.end = _res; \
		for (int i = 1; i < n; i++) { \
			_res = malloc(sizeof(t##Node));  \
			_res->prev = res.end; \
			_res->value = va_arg(args, t); \
			_res->next = NULL; \
			res.end->next = _res; \
			res.end = _res; \
		} \
		return res; \
	} \
	static t* t##Chain_append(t##Chain* chain, t obj) { \
		t##Node* _res = malloc(sizeof(t##Node)); \
		if (!_res) return NULL; \
		_res->prev = chain->end; \
		_res->next = NULL; \
		_res->value = obj; \
		if (!chain->start) { chain->start = _res; } \
		else { chain->end->next = _res; } \
		chain->end = _res; \
		return &chain->end->value; \
	} \
	static t##Node* t##Chain_getnode(t##Chain chain, int index) { \
		t##Node* res; \
		if (index >= 0) { \
			res = chain.start; \
			for (int i = 0; i < index && res != NULL; i++) { res = res->next; } \
		} else { \
			res = chain.end; \
			index = index * -1 - 1; \
			for (int i = 0; i < index && res != NULL; i++) { res = res->prev; } \
		} \
		return res; \
	} \
	static t t##Chain_get(t##Chain chain, int index) { \
		t##Node* res = t##Chain_getnode(chain, index); \
		return res ? res->value : (t){0}; \
	} \
	static t* t##Chain_getref(t##Chain chain, int index) { \
		t##Node* res = t##Chain_getnode(chain, index); \
		return res ? &res->value : NULL; \
	} \
	static void t##Chain_set(t##Chain* chain, int index, t object) { \
		t##Node* res = t##Chain_getnode(*chain, index); \
		res->value = object; \
	} \
	static bool t##Chain_insert(t##Chain* chain, t##Chain sub, int index) { \
		t##Node* prev = t##Chain_getnode(*chain, index); \
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
	static t t##Chain_pop(t##Chain* chain, int index) { \
		t##Node* node = t##Chain_getnode(*chain, index); \
		if (node == chain->start) { chain->start = chain->start->next; } \
		if (node == chain->end) { chain->end = chain->end->prev; } \
		if (node->prev) { node->prev->next = node->next; } \
		if (node->next) { node->next->prev = node->prev; } \
		t res = node->value; \
		free(node); \
		return res; \
	} \
	static int t##Chain_length(t##Chain chain) { \
		int res = 0; \
		chainForeach(t, item, chain) {res++;} \
		return res; \
	} \
	static bool t##Chain_clear(t##Chain* chain) { \
		if (!chain->end) return true; \
		t##Node* nextnode; \
		for (t##Node* node = chain->start; node != NULL; node = nextnode) { \
			nextnode = node->next; \
			free(node); \
		} \
		chain->end = NULL; \
		chain->start = NULL; \
		return true; \
	} \

#endif // _DATASETS_H
