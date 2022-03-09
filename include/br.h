#ifndef _BRIDGE_
#define _BRIDGE_

#include "datasets.h"
#include "sbuf.h"
#include "ctxalloc.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"

extern bool IS_BIG_ENDIAN;
extern bool IS_LITTLE_ENDIAN;

// declarations for the BRidge Parser

const sbuf NT_PATHSEP = fromcstr("\\");
const sbuf POSIX_PATHSEP = fromcstr("/");

const sbuf NT_NEWLINE = fromcstr("\r\n");
const sbuf POSIX_NEWLINE = fromcstr("\n");

#ifdef _WIN32_
#define PATHSEP NT_PATHSEP
#define NEWLINE NT_NEWLINE
#else
#define PATHSEP POSIX_PATHSEP
#define NEWLINE POSIX_NEWLINE
#endif

const sbuf SPACE = fromcstr(" ");
const sbuf TAB = fromcstr("\t");

typedef enum {
	TOKEN_NONE,
	TOKEN_WORD,
	TOKEN_KEYWORD,
	TOKEN_SYMBOL,
	TOKEN_INT,
	TOKEN_STRING,
	N_TOKEN_TYPES
} TokenType;

char* TokenTypeNames[N_TOKEN_TYPES] = {
	"nothing",
	"word",
	"keyword",
	"symbol",
	"integer",
	"string"
};


typedef struct {
	int16_t lineno;
	int16_t colno;
	int16_t src_id;
} TokenLoc;

typedef struct {
	int8_t type;
	TokenLoc loc;
	union {
		char* word; // for TOKEN_WORD or TOKEN_STRING
		int64_t value; // for TOKEN_INT
		int64_t keyword_id; // for TOKEN_KEYWORD
		int64_t symbol_id; // for TOKEN_SYMBOL
	};
} Token;
defArray(sbuf);

typedef enum {
	PREP_ERR_OK,
	PREP_ERR_UNCLOSED_STR,
	PREP_ERR_NO_NEWLINE_DELIM,
	PREP_ERR_NO_MEMORY,
	PREP_ERR_FILE_NOT_FOUND,
	N_PARSER_ERRORS
} PreprocError;

typedef struct {
	char* name;
	sbuf content;
	TokenLoc cur_loc;
} InputCtx;
defArray(InputCtx);

typedef struct {
	sbuf* keywords;
	sbuf* delims;
	PreprocError error_code;
	union {
		TokenLoc error_loc;
		char* error_filename;
	};
	InputCtxArray sources;
	int _nl_delim_id;
	Token pending;
} Preprocessor;

Preprocessor newPreprocessor(sbuf delims[], sbuf keywords[], heapctx_t ctx);
bool setInputFrom(Preprocessor* obj, char* name, sbuf input);
bool setInput(Preprocessor* obj, char* name);
Token fetchToken(Preprocessor* obj);

int fprintTokenLoc(FILE* fd, TokenLoc loc, Preprocessor* obj);
int fprintTokenStr(FILE* fd, Token token, Preprocessor* obj);
int fprintToken(FILE* fd, Token token, Preprocessor* obj);
char* getTokenWord(Preprocessor* obj, Token token);
char* getErrorStr(Preprocessor* obj);


#define setPrepError(prep, code, loc) \
	if ((prep)->error_code == PREP_ERR_OK) { \
		(prep)->error_code = code; \
		(prep)->error_loc = loc; \
	}
#define isPrepEOF(prep) ( (prep)->sources.length ? (prep)->sources.data[0].content.length == 0 : true )
#define printTokenLoc(loc, parser) fprintTokenLoc(stdout, loc, parser)
#define printTokenStr(token, parser) fprintTokenStr(stdout, token, parser)
#define printToken(token, parser) fprintToken(stdout, token, parser)
#define getTokenKeywordId(token) ( (token).type == TOKEN_KEYWORD ? (token).keyword_id : -1 )
#define getTokenSymbolId(token) ( (token).type == TOKEN_SYMBOL ? (token).symbol_id : -1 )
#define isWordToken(token) ( (token).type == TOKEN_WORD || (token).type == TOKEN_KEYWORD )

#define eprintf(...) fprintf(stderr, __VA_ARGS__)


void initBREnv(void);
char* getAbsolutePath(char* src, heapctx_t ctx);
void* BRByteOrder(void* src, long length);
bool startTimer(void);
float endTimer(void);

#define inRange(x, start, end) ( (int64_t)(x) >= (int64_t)(start) && (int64_t)(x) < (int64_t)(end) )
#define isSlice(sub_start, sub_size, start, size) \
	( (int64_t)(sub_start) >= (int64_t)(start) && ((int64_t)(sub_start) + (int64_t)(sub_size)) <= ((int64_t)(start) + (int64_t)(size)) ) 
// assumes that sub_size > 0 and size > 0
#define maxInt(a, b) ( a > b ? a : b )
#define minInt(a, b) ( a < b ? a : b )
#define absInt(x) ( x < 0 ? x * -1 : x )

#endif
