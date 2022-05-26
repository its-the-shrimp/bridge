#ifndef _BRIDGE_
#define _BRIDGE_

#ifdef BRIDGE_IMPLEMENTATION
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
#if defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__) && defined(__ORDER_LITTLE_ENDIAN__)
#define IS_BIG_ENDIAN (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#define IS_LITTLE_ENDIAN (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#elif defined(IS_BIG_ENDIAN) && IS_BIG_ENDIAN == 1
#define IS_LITTLE_ENDIAN 0
#elif defined(IS_LITTLE_ENDIAN) && IS_LITTLE_ENDIAN == 1
#define IS_BIG_ENDIAN 0
#else
#error "could not automatically detect system endianness; if your system is big-endian, put `#define IS_BIG_ENDIAN 1` before importing `br.h`, if your system is little-endian, put `#define IS_LITTLE_ENDIAN 1` before importing `br.h`"
#endif

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define eputc(ch) fputc(ch, stderr)
#define eputs(str) fputs(str, stderr)

#define BR_DEBUG
#ifdef BR_DEBUG
#define _s1(x) #x
#define _s2(x) _s1(x)
#define comment(msg) puts(__FILE__":"_s2(__LINE__)": "msg)
#endif

#define BRB_EXT ".brb"
#define VBRB_EXT ".vbrb"
#define BR_EXT ".br"

#if IS_LITTLE_ENDIAN
void* BRByteOrder(void* src, long length);
#else
#define BRByteOrder(src, length) (src)
#endif

static struct timespec TIME;
bool startTimerAt(struct timespec* dst);
float endTimerAt(struct timespec* src);
#define startTimer() startTimerAt(&TIME)
#define endTimer() endTimerAt(&TIME)

typedef struct {
	uint8_t exitcode;
	bool exited;
	FILE* in;
	FILE* out;
	FILE* err;
} ProcessInfo;

// executes the command in a subshell and returns the IO descriptors and the exitcode of the process
bool execProcess(char* command, ProcessInfo* info);
// same as execProcess(), but command is provided in the form of a sized buffer
bool execProcess_s(sbuf command, ProcessInfo* info);

// returns `true` if the path points to a directory, otherwise returns `false`
bool isPathDir(char* path);
// same as isPathDir(), but path is provided in the form of a sized buffer
bool isPathDir_s(sbuf path);

// string function that changes the extension of the file path `path` to `ext`
char* setFileExt(char* path, char* ext);
// same as setFileExt(), but operates on and returns a sized buffer instead of a string
sbuf setFileExt_s(sbuf path, sbuf ext);

// returns the base file name extracted from a file path, e.g. "dir/y.txt" -> "y"
char* fileBaseName(char* path);
// same as fileBaseName, but operates on and returns a sized buffer
sbuf fileBaseName_s(sbuf path);

bool fpipe(FILE** readable_end_ptr, FILE** writable_end_ptr);

#define isTempPath(path) sbufstartswith(fromstr(path), fromcstr("/tmp")) 
#define inRange(x, start, end) ( (int64_t)(x) >= (int64_t)(start) && (int64_t)(x) < (int64_t)(end) )
#define isSlice(sub_start, sub_size, start, size) \
	( (int64_t)(sub_start) >= (int64_t)(start) && ((int64_t)(sub_start) + (int64_t)(sub_size)) <= ((int64_t)(start) + (int64_t)(size)) ) 
// assumes that sub_size > 0 and size > 0
#define maxInt(a, b) ( a > b ? a : b )
#define minInt(a, b) ( a < b ? a : b )
#define absInt(x) (x<0?-x:x)
#define absNegInt(x) (x<0?x:-x)
#define swap(v1, v2, tmp_t) { \
	tmp_t __tmp = v1; \
	v1 = v2; \
	v2 = __tmp; \
}
#define byteMask(n_bytes) ((1LL << ((n_bytes) * 8 - 1)) * 2 - 1)

#endif // _BRIDGE_
