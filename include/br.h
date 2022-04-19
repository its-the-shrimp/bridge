#ifndef _BRIDGE_
#define _BRIDGE_

#ifdef BRIDGE_IMPLEMENTATION
#define CTXALLOC_IMPLEMENTATION
#define SBUF_IMPLEMENTATION
#define BRP_IMPLEMENTATION
#endif

#include "datasets.h"
#include "sbuf.h"
#include "assert.h"
#include "brp.h"
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"
#include "time.h"

#define UINT48_MAX 0xFFFFFFFFFFFFULL
extern bool IS_BIG_ENDIAN;
extern bool IS_LITTLE_ENDIAN;

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define class typedef struct

void initBREnv(void);
char* getAbsolutePath(char* src);
void* BRByteOrder(void* src, long length);

static struct timespec TIME;
bool startTimerAt(struct timespec* dst);
float endTimerAt(struct timespec* src);
#define startTimer() startTimerAt(&TIME)
#define endTimer() endTimerAt(&TIME)

class {
	uint8_t exitcode;
	bool exited;
	FILE* in;
	FILE* out;
	FILE* err;
} ProcessInfo;

bool execProcess(char* command, ProcessInfo* info);

FILE* fopenat(FILE* dir, const char* path, const char* mode);

#define isTempPath(path) sbufstartswith(fromstr(path), fromcstr("/tmp")) 
#define inRange(x, start, end) ( (int64_t)(x) >= (int64_t)(start) && (int64_t)(x) < (int64_t)(end) )
#define isSlice(sub_start, sub_size, start, size) \
	( (int64_t)(sub_start) >= (int64_t)(start) && ((int64_t)(sub_start) + (int64_t)(sub_size)) <= ((int64_t)(start) + (int64_t)(size)) ) 
// assumes that sub_size > 0 and size > 0
#define maxInt(a, b) ( a > b ? a : b )
#define minInt(a, b) ( a < b ? a : b )
#define absInt(x) (x<0?-x:x)
#define absNegInt(x) (x<0?x:-x)

#endif
