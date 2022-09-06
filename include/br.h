#ifndef _BRIDGE_
#define _BRIDGE_

#ifdef BRIDGE_IMPLEMENTATION
#define SBUF_IMPLEMENTATION
#define BRP_IMPLEMENTATION
#define BR_UTILS_IMPLEMENTATION
#endif

#include <br_utils.h>
#include <datasets.h>
#include <sbuf.h>
#include <brp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>

#define UINT48_MAX 0xFFFFFFFFFFFFULL
#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define eputc(ch) fputc(ch, stderr)
#define eputs(str) fputs(str, stderr)

#define BRB_EXT "brb"
#define VBRB_EXT "vbrb"
#define BR_EXT "br"

static struct timespec TIME;

bool startTimerAt(struct timespec* dst);
#define startTimer() startTimerAt(&TIME)

float endTimerAt(struct timespec* src);
#define endTimer() endTimerAt(&TIME)

typedef struct {
	uint8_t exitcode;
	bool exited;
	FILE* in;
	FILE* out;
	FILE* err;
} ProcessInfo;

// exEcutes the command in a subshell and returns the IO descriptors and the exitcode of the process
bool execProcess(char* command, ProcessInfo* info);
// same as execProcess(), but command is provided in the form of a sized buffer
bool execProcess_s(sbuf command, ProcessInfo* info);

// returns `true` if the path points to a directory, otherwise returns `false`
bool isPathDir(char* path);
// same as isPathDir(), but path is provided in the form of a sized buffer
bool isPathDir_s(sbuf path);

// returns a null-terminated string that is a copy of the file extension of the null-terminated `path`
char* getFileExt(const char* path);
// same as `getFileExt`, but operates on and returns a sized buffer, without additional allocations
sbuf getFileExt_s(sbuf path);

// string function that changes the extension of the file path `path` to `ext`
char* setFileExt(const char* path, const char* ext);
// same as setFileExt(), but operates on and returns a sized buffer instead of a string
sbuf setFileExt_s(sbuf path, sbuf ext);

// returns the base file name extracted from a file path, e.g. "dir/y.txt" -> "y"
char* fileBaseName(const char* path);
// same as fileBaseName, but operates on and returns a sized buffer
sbuf fileBaseName_s(sbuf path);

#define isTempPath(path) sbufstartswith(path, fromcstr("/tmp")) 
#define inRange(x, start, end) ((x) >= (start) && (x) < (end))
#define isSlice(sub_start, sub_size, start, size) \
	((sub_start) >= (start) && (sub_start) + (sub_size) <= (start) + (size)) 
// assumes that sub_size > 0 and size > 0
#define maxInt(a, b) ( a > b ? a : b )
#define minInt(a, b) ( a < b ? a : b )
#define absInt(x) (x<0?-x:x)
#define absNegInt(x) (x<0?x:-x)
#define swap(v1, v2, tmp_t) { tmp_t TEMPVAR = v1; v1 = v2; v2 = TEMPVAR; }
#define byteMask(n_bytes, offset) ((1ULL << ((n_bytes) * 8)) - 1) << ((offset) * 8)

#endif // _BRIDGE_
