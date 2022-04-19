#ifndef _DATASETS_
#define _DATASETS_

#include "stdlib.h"
#include "string.h"
#include "stdbool.h"
#include "stdarg.h"
#include "unistd.h"

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
	t##Array t##Array_new(int n, ...); \
	t* t##Array_append(t##Array* array, t object); \
	t* t##Array_extend(t##Array* array, t##Array sub); \
	t t##Array_get(t##Array array, int index); \
	void t##Array_set(t##Array* array, int index, t object); \
	bool t##Array_move(t##Array* array, int index, size_t n, t dst[]); \
	t##Array t##Array_slice(t##Array array, int index, size_t n); \
	bool t##Array_del(t##Array* array, int index, size_t n); \
	t t##Array_pop(t##Array* array, int index); \
	long t##Array_index(t##Array array, t obj); \
	bool t##Array_insert(t##Array* array, t##Array sub, int index); \
	bool t##Array_clear(t##Array* array); \
	t* t##Array_resize(t##Array* array, int n); \

#define defArray(t) \
	t##Array t##Array_new(int n, ...) { \
		va_list args; \
		va_start(args, n); \
		t##Array res = (t##Array){  \
			.length = (n < 0 ? 0 : n), \
			.data = malloc(sizeof(t) * absInt(n)), \
		}; \
		for (int i = 0; i < n; i++) { \
			res.data[i] = va_arg(args, t); \
		} \
		va_end(args); \
		return res; \
	} \
	t* t##Array_append(t##Array* array, t object) { \
		void* res = realloc(array->data, (array->length + 1) * sizeof(t)); \
		if (!res) return NULL; \
		array->data = res; \
		array->data[array->length++] = object; \
		return arrayhead(*array); \
	} \
	t* t##Array_extend(t##Array* array, t##Array sub) { \
		void* res = realloc(array->data, (array->length + sub.length) * sizeof(t)); \
		if (!res) return NULL; \
		array->data = res; \
		memcpy(array->data + array->length, sub.data, sub.length * sizeof(t)); \
		array->length += sub.length; \
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
		void* res = realloc(array->data, (array->length - n) * sizeof(t)); \
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
		void* res = realloc(array->data, (array->length + sub.length - 1) * sizeof(t)); \
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
	t* t##Array_resize(t##Array* array, int n) { \
		t* res = realloc(array->data, (array->length + n) * sizeof(t)); \
		if (!res) return NULL; \
		array->length += n; \
		array->data = res; \
		return array->data + array->length - n; \
	} \
	bool t##Array_clear(t##Array* array) { \
		if (!array->length) return true; \
		free(array->data); \
		array->data = malloc(0); \
		array->length = 0; \
		return true; \
	} \

#define chainctx(chain) chunkctx((chain).start)
#define defChain(t) \
	typedef struct t##_node { struct t##_node* prev; t value; struct t##_node* next; } t##Node; \
	typedef struct t##_chain { t##Node* start; t##Node* end; } t##Chain; \
	t##Chain t##Chain_new(int n, ...) { \
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
	t* t##Chain_append(t##Chain* chain, t obj) { \
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
	t t##Chain_pop(t##Chain* chain, t##Node* node) { \
		if (node == chain->start) { chain->start = chain->start->next; } \
		if (node == chain->end) { chain->end = chain->end->prev; } \
		if (node->prev) { node->prev->next = node->next; } \
		if (node->next) { node->next->prev = node->prev; } \
		t res = node->value; \
		free(node); \
		return res; \
	} \
	t t##Chain_popstart(t##Chain* chain) { \
		if (!chain->start) return (t){0}; \
		t##Node res = *(chain->start); \
		if (chain->start == chain->end) { \
			chain->start = NULL; \
			free(chain->end); \
			chain->end = NULL; \
		} else { \
			free(chain->start); \
			chain->start = res.next; \
		} \
		return res.value; \
	} \
	t t##Chain_getstart(t##Chain* chain) { \
		if (!chain->start) return (t){0}; \
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
			free(node); \
		} \
		chain->end = NULL; \
		return true; \
	} \

#define declQueue(t) \
	typedef struct { int input_end, output_end; long length; } t##Queue; \
	t##Queue t##Queue_new(int n, ...); \
	bool t##Queue_add(t##Queue* queue, t obj); \
	bool t##Queue_fetch(t##Queue* queue, t* dst); \
	bool t##Queue_peek(t##Queue* queue, t* dst); \
	bool t##Queue_delete(t##Queue* queue) \

#define defQueue(t) \
	t##Queue t##Queue_new(int n, ...) { \
		int fds[2]; \
		pipe(fds); \
		t##Queue res = { .length = n, .input_end = fds[1], .output_end = fds[0] }; \
		if (!n) return res; \
		va_list args; \
		va_start(args, n); \
		for (int i = 0; i < n; i++) { \
			t obj = va_arg(args, t); \
			if (write(res.input_end, &obj, sizeof(t)) <= 0) { \
				va_end(args); \
				return (t##Queue){0}; \
			} \
		} \
		va_end(args); \
		return res; \
	} \
	bool t##Queue_add(t##Queue* queue, t obj) { \
		if (write(queue->input_end, &obj, sizeof(t)) <= 0) return false; \
		queue->length++; \
		return true; \
	} \
	bool t##Queue_fetch(t##Queue* queue, t* dst) { \
	 	if (!queue->length) return false; \
		if (read(queue->output_end, dst, sizeof(t)) < 0) return false; \
		queue->length--; \
		return true; \
	} \
	bool t##Queue_peek(t##Queue* queue, t* dst) { \
		if (!queue->length) return false; \
		t temp; \
		if (read(queue->output_end, &temp, sizeof(t)) < 0) return false; \
		*dst = temp; \
		if (write(queue->input_end, &temp, sizeof(t)) <= 0) return false; \
		for (int i = 0; i < queue->length - 1; i++) { \
			if (read(queue->output_end, &temp, sizeof(t)) < 0) return false; \
			if (write(queue->input_end, &temp, sizeof(t)) <= 0) return false; \
		} \
		return true; \
	} \
	bool t##Queue_delete(t##Queue* queue) { \
		if (close(queue->input_end) < 0) return false; \
		if (close(queue->output_end) < 0) return false; \
		return true; \
	} \

#endif
