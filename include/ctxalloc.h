// ctxalloc - Highly-Customizable Context-Based Memory Allocator 

#ifndef _CTXALLOC_
#define _CTXALLOC_
#include "unistd.h"
#include "string.h"
#include "assert.h"
#include "math.h"
#include "stdio.h"

struct heapspec {
	struct heapspec* prev;
	struct heapspec* next;
	struct heapctx {
		struct heapspec* first;
		struct heapspec* last;
		char options;
	}* ctx;
	long capacity;
};

typedef struct heapctx* heapctx_t;

struct ctxinfo {
	long n_chunks;
	long used;
	long allocated;
};

extern heapctx_t GLOBAL_CTX;
extern heapctx_t TEMP_CTX;
static struct heapctx FREE_CHUNKS[1] = {0};
static void* HEAP_START = NULL;

void ctxalloc_init(void);
struct ctxinfo ctxalloc_ctxinfo(heapctx_t ctx);
heapctx_t ctxalloc_newctx(char options);
void* ctxalloc_new(long size, heapctx_t ctx);
void* ctxalloc_resize(void* obj, long size);
void* ctxalloc_free(void* obj); 
void ctxalloc_clearctx(heapctx_t ctx);
void* ctxalloc_delctx(heapctx_t ctx);
heapctx_t ctxalloc_mergectx(heapctx_t src, heapctx_t dst);

#define isheapchunk(ptr) (((struct heapspec*)ptr - 1)->capacity >= 0)
#define chunkctx(ptr) ( isheapchunk(ptr) ? ((struct heapspec*)ptr - 1)->ctx : (heapctx_t)ptr )

#define enter_tempctx(ctxname, options) \
	heapctx_t prev_##ctxname = TEMP_CTX, ctxname = ctxalloc_newctx(options); \
	TEMP_CTX = ctxname;

#define exit_tempctx(ctxname) \
	ctxalloc_delctx(TEMP_CTX); \
	TEMP_CTX = prev_##ctxname;

#endif // _CTXALLOC_

#ifdef CTXALLOC_IMPLEMENTATION
#undef CTXALLOC_IMPLEMENTATION

heapctx_t GLOBAL_CTX = NULL, TEMP_CTX = NULL;

void _popchunk(struct heapspec* chunk, heapctx_t ctx) 
{
	if (chunk->prev) {
		chunk->prev->next = chunk->next;
	}
	if (chunk->next) {
		chunk->next->prev = chunk->prev;
	}
	if (chunk == ctx->last) {
		ctx->last = ctx->last->prev;
	}
	if (chunk == ctx->first) {
		ctx->first = ctx->first->next;
	}
}

void _appendchunk(struct heapspec* chunk, heapctx_t ctx)
{
	if (!ctx->first) {
		ctx->first = chunk;
		ctx->last = chunk;
		chunk->prev = NULL;
	} else {
		ctx->last->next = chunk;
		chunk->prev = ctx->last;
		ctx->last = chunk;
	}
	chunk->next = NULL;
	chunk->ctx = ctx;
}


void ctxalloc_init(void)
{
	if (GLOBAL_CTX) return;
	struct heapspec* tmp = sbrk(sizeof(struct heapspec) + sizeof(struct heapctx));
	*tmp = (struct heapspec){0};
	tmp->capacity = -1;
	GLOBAL_CTX = (heapctx_t)(tmp + 1);
	*GLOBAL_CTX = (struct heapctx){0};
	TEMP_CTX = GLOBAL_CTX;
}

struct ctxinfo ctxalloc_ctxinfo(heapctx_t ctx) {
	struct ctxinfo res = {0};
	for (struct heapspec* i = ctx->first; i != NULL; i = i->next) {
		res.n_chunks++;
		res.used += i->capacity;
		res.allocated += sizeof(struct heapspec) + i->capacity;
	}
	return res;
}

heapctx_t ctxalloc_newctx(char options)
{
	heapctx_t res = ctxalloc_new(sizeof(struct heapctx), GLOBAL_CTX);
	((struct heapspec*)res - 1)->capacity = -1;
	res->first = NULL;
	res->last = NULL;
	res->options = options;
	return res;
}

void* ctxalloc_new(long size, heapctx_t ctx)
{
	if (!size) { return ctx; }
	// iterating through free chunks to find the one with suffiecent size
	for (struct heapspec* chunk = FREE_CHUNKS->first; chunk != NULL; chunk = chunk->next) {
		if (chunk->capacity >= size) {
			_popchunk(chunk, FREE_CHUNKS);
			_appendchunk(chunk, ctx);
			return chunk + 1;
		}
	}

	// in that case a brand new chunk is allocated
	struct heapspec* res_chunk = sbrk(size + sizeof(struct heapspec));
	res_chunk->capacity = size;
	_appendchunk(res_chunk, ctx);
	return res_chunk + 1;
}

void* ctxalloc_resize(void* obj, long size)
{
	if (size <= 0) {
		heapctx_t prevctx = chunkctx(obj);
		return !ctxalloc_free(obj) ? prevctx : NULL;
	}

	struct heapspec* cur_chunk = obj - sizeof(struct heapspec);
	if (cur_chunk->capacity <= 0) return ctxalloc_new(size, obj);
	if (size < cur_chunk->capacity) return obj;

	heapctx_t ctx = chunkctx(obj);
	// checking if the last chunk ends at the heap break, in which case it can be expanded in-place
	if (obj + cur_chunk->capacity == sbrk(0)) {
		if ((long)sbrk(size - cur_chunk->capacity) == -1) {
			return NULL;
		}
		cur_chunk->capacity = size;
		return obj;
	}

	void* new = ctxalloc_new(size, ctx);
	if (!new) { return NULL; }
	memmove(new, obj, cur_chunk->capacity);
	ctxalloc_free(obj); 
	return new;
}

void* ctxalloc_free(void* ptr) 
{
	struct heapspec* res = ptr - sizeof(struct heapspec);

	if (ptr + res->capacity == sbrk(0)) {
		_popchunk(res, res->ctx);
		if ((long)sbrk(-1 * (res->capacity + sizeof(struct heapspec))) == -1) {
			_appendchunk(res, res->ctx);	
			return ptr;
		}
	} else {
		if (res->capacity < 0) res->capacity *= -1;
		_popchunk(res, res->ctx);
		_appendchunk(res, FREE_CHUNKS);
	}
	return NULL;
}

heapctx_t ctxalloc_mergectx(heapctx_t src, heapctx_t dst)
{

	for (struct heapspec* i = src->first; i != NULL; i = i->next) {
		i->ctx = dst;
	}

	if (src->last) {
		if (dst->last) {
			dst->last->next = src->first;
		} else {
			dst->first = src->first;
		}
		src->first->prev = dst->last;
		dst->last = src->last;
	}
	src->first = NULL;
	src->last = NULL;
	return dst;
}

void ctxalloc_clearctx(heapctx_t ctx) 
{ 
	ctxalloc_mergectx(ctx, FREE_CHUNKS);
}

void* ctxalloc_delctx(heapctx_t ctx)
{
	ctxalloc_mergectx(ctx, FREE_CHUNKS);
	return ctxalloc_free(ctx);
}


#endif // CTXALLOC_IMPLEMENTATION
