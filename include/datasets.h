#ifndef _DATASETS_
#define _DATASETS_

#include "ctxalloc.h"
#include "string.h"
#include "stdbool.h"
#include "stdarg.h"

#define array_foreach(t, item, array, body) \
	do { \
		for (int _##item = 0; _##item < (array).length; _##item++) { \
			t item = (array).data[_##item]; \
			do { body } while (0); \
		} \
	} while (0) \

#define array_rev_foreach(t, item, array, body) \
	do { \
		for (int _##item = (array).length - 1; _##item >= 0; _##item--) { \
			t item = (array).data[_##item]; \
			do { body } while (0); \
		} \
	} while (0) \

#define chain_foreach(t, item, chain, body) \
	do { \
		for (t##Node* _##item = (chain).start; _##item != NULL; _##item = _##item->next) { \
			t item = _##item->value; \
			do { body } while (0); \
		} \
	} while (0) \

#define chain_foreach_fetch(t, item, chainp, body) \
	do { \
		while ((chainp)->start) { \
			t item = t##Chain_popstart(chainp); \
			do { body } while (0); \
		} \
		(chainp)->end = NULL; \
	} while (0) \

#define _str(t) #t

#define arrayhead(array) ((array).data + (array).length - 1)
#define arrayctx(array) chunkctx((array).data)
#define declArray(t) \
	typedef struct { int length; t* data; } t##Array; \
	t##Array t##Array_new(heapctx_t ctx, int n, ...); \
	t* t##Array_append(t##Array* array, t object); \
	t t##Array_get(t##Array array, int index); \
	void t##Array_set(t##Array* array, int index, t object); \
	bool t##Array_move(t##Array* array, int index, size_t n, t dst[]); \
	t##Array t##Array_slice(t##Array array, int index, size_t n); \
	bool t##Array_del(t##Array* array, int index, size_t n); \
	t t##Array_pop(t##Array* array, int index); \
	long t##Array_index(t##Array array, t obj); \
	bool t##Array_insert(t##Array* array, t##Array sub, int index); \
	bool t##Array_clear(t##Array* array); \

#define defArray(t) \
	t##Array t##Array_new(heapctx_t ctx, int n, ...) { \
		va_list args; \
		va_start(args, n); \
		t##Array res = (t##Array){  \
			.length = (n < 0 ? 0 : n), \
			.data = ctxalloc_new(sizeof(t) * (n < 0 ? n * -1 : n), ctx), \
		}; \
		for (int i = 0; i < n; i++) \
		{ \
			res.data[i] = va_arg(args, t); \
		} \
		va_end(args); \
		return res; \
	} \
	t* t##Array_append(t##Array* array, t object) { \
		void* res = ctxalloc_resize(array->data, (array->length + 1) * sizeof(t)); \
		if (!res) return NULL; \
		array->data = res; \
		array->data[array->length++] = object; \
		return arrayhead(*array); \
	} \
	t t##Array_get(t##Array array, int index) { \
		index = index >= 0 ? index : array.length + index; \
		if (index > array.length - 1) return (t){0}; \
		return array.data[index]; \
	}; \
	void t##Array_set(t##Array* array, int index, t object) { \
		*(array->data + (index >= 0 ? index : array->length + index)) = object; \
	} \
	bool t##Array_move(t##Array* array, int index, size_t n, t dst[]) { \
		index = index >= 0 ? index : array->length + index; \
		if (index + n > array->length) return false; \
		if (dst != NULL) for (int i = index; i < index + n; i++) dst[i - index] = array->data[i]; \
		for (int i = index; i + n < array->length; i++) { \
			array->data[i] = array->data[i + n]; \
		} \
		void* res = ctxalloc_resize(array->data, (array->length - n) * sizeof(t)); \
		if (res) { \
			array->data = res; \
			array->length -= n; \
			return true; \
		} \
		return false; \
	} \
	t##Array t##Array_slice(t##Array array, int index, size_t n) { \
		index = index >= 0 ? index : array.length + index; \
		if (index + n > array.length) return (t##Array){0}; \
		return (t##Array){ \
			.length = n, \
			.data = array.data + index, \
		}; \
	} \
	bool t##Array_del(t##Array* array, int index, size_t n) { \
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
	t t##Array_pop(t##Array* array, int index) { \
		t res = (t){0}; \
		if (!t##Array_move(array, index, 1, &res)) return (t){0}; \
		return res; \
	} \
	long t##Array_index(t##Array array, t obj) { \
		for (int i = 0; i < array.length; i++) { \
			if (memcmp(array.data + i, &obj, sizeof(t))) return i; \
		} \
		return -1; \
	} \
	bool t##Array_insert(t##Array* array, t##Array sub, int index) { \
		index = index >= 0 ? index : array->length + index; \
		if (index > array->length) { \
			return false; \
		} \
		void* res = ctxalloc_resize(array->data, (array->length + sub.length - 1) * sizeof(t)); \
		if (!res) { return false; } \
		array->data = res; \
		for (int i = array->length - sub.length - 1; i > index; i--) { \
			array->data[i + sub.length - 1] = array->data[i]; \
		} \
		for (int i = index; i < index + sub.length; i++) { \
			array->data[i] = sub.data[i - index]; \
		} \
		return true; \
	} \
	bool t##Array_clear(t##Array* array) { \
		if (!array->length) return true; \
		heapctx_t ctx = chunkctx(array->data); \
		if (!ctxalloc_free(array->data)) return false; \
		array->data = (t*)ctx; \
		array->length = 0; \
		return true; \
	} \

#define chainctx(chain) chunkctx((chain).start)
#define defChain(t) \
	typedef struct t##_node { struct t##_node* prev; t value; struct t##_node* next; } t##Node; \
	typedef struct t##_chain { t##Node* start; t##Node* end; } t##Chain; \
	t##Chain t##Chain_new(heapctx_t ctx, int n, ...) { \
		if (n == 0) { return (t##Chain){ .start = ctxalloc_new(0, ctx), .end = NULL }; } \
		va_list args; \
		va_start(args, n); \
		t##Chain res; \
		t##Node* _res = ctxalloc_new(sizeof(t##Node), ctx); \
		_res->prev = NULL; \
		_res->value = va_arg(args, t); \
		_res->next = NULL; \
		res.start = res.end = _res; \
		for (int i = 1; i < n; i++) { \
			_res = ctxalloc_new(sizeof(t##Node), ctx);  \
			_res->prev = res.end; \
			_res->value = va_arg(args, t); \
			_res->next = NULL; \
			res.end->next = _res; \
			res.end = _res; \
		} \
		return res; \
	} \
	t* t##Chain_append(t##Chain* chain, t obj) { \
		t##Node* _res = ctxalloc_new(sizeof(t##Node), chainctx(*chain)); \
		if (!_res) { return NULL; } \
		_res->prev = chain->end; \
		_res->next = NULL; \
		_res->value = obj; \
		if (!isheapchunk(chain->start)) { chain->start = _res; } \
		else { chain->end->next = _res; } \
		chain->end = _res; \
		return &chain->end->value; \
	} \
	t##Node* t##Chain_getnode(t##Chain chain, int index) { \
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
	t t##Chain_get(t##Chain chain, int index) { \
		t##Node* res = t##Chain_getnode(chain, index); \
		return res ? res->value : (t){0}; \
	} \
	bool t##Chain_insert(t##Chain* chain, t##Chain sub, t##Node* prev) { \
		if (chainctx(*chain) != chainctx(sub)) { return false; } \
		if (!sub.start) { return true; } \
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
	t t##Chain_pop(t##Chain* chain, t##Node* node) { \
		if (!node) { return (t){0}; } \
		if (node == chain->start) { chain->start = chain->start->next; } \
		if (node == chain->end) { chain->end = chain->end->prev; } \
		if (node->prev) { node->prev->next = node->next; } \
		if (node->next) { node->next->prev = node->prev; } \
		t res = node->value; \
		ctxalloc_free(node); \
		return res; \
	} \
	t t##Chain_popstart(t##Chain* chain) { \
		if (!isheapchunk(chain->start)) return (t){0}; \
		t##Node res = *(chain->start); \
		if (chain->start == chain->end) { \
			chain->start = (t##Node*)chunkctx(chain->end); \
			ctxalloc_free(chain->end); \
			chain->end = NULL; \
		} else { \
			ctxalloc_free(chain->start); \
			chain->start = res.next; \
		} \
		return res.value; \
	} \
	t t##Chain_getstart(t##Chain* chain) { \
		if (!isheapchunk(chain->start)) return (t){0}; \
		return chain->start->value; \
	} \
	int t##Chain_length(t##Chain chain) { \
		int res = 0; \
		chain_foreach(t, item, chain, res++; ); \
		return res; \
	} \
	bool t##Chain_delete(t##Chain* chain) { \
		if (!chain->end) return true; \
		t##Node* prevnode; \
		chain->start = (t##Node*)chainctx(*chain); \
		for (t##Node* node = chain->start; node != NULL; node = prevnode) { \
			prevnode = node->prev; \
			if (!ctxalloc_free(node)) return false; \
		} \
		chain->end = NULL; \
		return true; \
	} \

#endif
