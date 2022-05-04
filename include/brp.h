// The BRidge Preprocessor
#ifndef _BRP_
#define _BRP_

#include "sbuf.h"
#include "datasets.h"
#include "errno.h"

#ifdef _WIN32_
#define PATHSEP NT_PATHSEP
#define NEWLINE NT_NEWLINE
#else
#define PATHSEP POSIX_PATHSEP
#define NEWLINE POSIX_NEWLINE
#endif

#define DQUOTE fromcstr("\"")
#define NT_PATHSEP fromcstr("\\")
#define POSIX_PATHSEP fromcstr("/")
#define NT_NEWLINE fromcstr("\r\n")
#define POSIX_NEWLINE fromcstr("\n")
#define SPACE fromcstr(" ")
#define TAB fromcstr("\t")

typedef enum token_type {
	TOKEN_NONE,
	TOKEN_WORD,
	TOKEN_KEYWORD,
	TOKEN_SYMBOL,
	TOKEN_INT,
	TOKEN_STRING,
	N_TOKEN_TYPES
} TokenType;


typedef struct token_loc {
	int16_t lineno;
	int16_t colno;
	int16_t src_id;
} TokenLoc;

typedef struct token {
	int8_t type;
	TokenLoc loc;
	union {
		char* word; // for TOKEN_WORD or TOKEN_STRING
		int64_t value; // for TOKEN_INT
		int64_t keyword_id; // for TOKEN_KEYWORD
		int64_t symbol_id; // for TOKEN_SYMBOL
	};
} Token;
declArray(sbuf);
declQueue(Token);

typedef enum {
	BRP_ERR_OK,
	BRP_ERR_UNCLOSED_STR,
	BRP_ERR_NO_MEMORY,
	BRP_ERR_FILE_NOT_FOUND,
	N_PARSER_ERRORS
} BRPError;

typedef struct {
	char* name;
	FILE* src;
	TokenLoc cur_loc;
} InputCtx;
declArray(InputCtx);


typedef struct {
	sbuf* keywords;
	sbuf* symbols;
	sbuf* hidden_symbols;
	BRPError error_code;
	union {
		TokenLoc error_loc;
		char* error_filename;
		int32_t sys_errno;
	};
	InputCtxArray sources;
	TokenQueue pending;
	sbuf buffer;
} BRP;

#define BRP_KEYWORD(spec) fromcstr(spec)
#define BRP_SYMBOL(spec) fromcstr(spec)
#define BRP_HIDDEN_SYMBOL(spec) ((sbuf){ .data = spec"\n", .length = sizeof(spec) - 1 })
// hidden symbols are delimiters that are not returned as generated tokens

BRP* initBRP(BRP* obj);
void delBRP(BRP* obj);

bool _setKeywords(BRP* obj, ...);
bool _setSymbols(BRP* obj, ...);
#define setKeywords(obj, ...) _setKeywords(obj, __VA_ARGS__, (sbuf){0})
#define setSymbols(obj, ...) _setSymbols(obj, __VA_ARGS__, (sbuf){0})
bool setInputFrom(BRP *obj, char *name, FILE* fd);
bool setInput(BRP* obj, char* name);
Token fetchToken(BRP* obj);
Token peekToken(BRP* obj);

char* getTokenSrcPath(BRP* obj, Token token);
int fprintTokenLoc(FILE* fd, TokenLoc loc, BRP* obj);
int fprintTokenStr(FILE* fd, Token token, BRP* obj);
int fprintToken(FILE* fd, Token token, BRP* obj);
char* getTokenTypeName(TokenType type);
char* getTokenWord(BRP* obj, Token token);
void printBRPErrorStr(FILE* fd, BRP* obj);
void printBRPError(FILE* fd, BRP* obj);


#define setBRPError(prep, code, loc) \
	if ((prep)->error_code == BRP_ERR_OK) { \
		(prep)->error_code = code; \
		(prep)->error_loc = loc; \
	}
#define BRPempty(prep) ( (prep)->sources.length ? feof((prep)->sources.data[0].src) || ferror((prep)->sources.data[0].src) : true )
#define printTokenLoc(loc, parser) fprintTokenLoc(stdout, loc, parser)
#define printTokenStr(token, parser) fprintTokenStr(stdout, token, parser)
#define printToken(token, parser) fprintToken(stdout, token, parser)
#define getTokenKeywordId(token) ( (token).type == TOKEN_KEYWORD ? (token).keyword_id : -1 )
#define getTokenSymbolId(token) ( (token).type == TOKEN_SYMBOL ? (token).symbol_id : -1 )
#define isWordToken(token) ( (token).type == TOKEN_WORD || (token).type == TOKEN_KEYWORD )
#define isSymbolSpecHidden(spec) (spec.data[spec.length] > 0)

#endif 


#ifdef BRP_IMPLEMENTATION
#undef BRP_IMPLEMENTATION

BRP* initBRP(BRP* obj)
{
	*obj = (BRP){0};
	obj->sources = InputCtxArray_new(-1);
	if (!obj->sources.data) return NULL;
	obj->pending = TokenQueue_new(0);
	obj->error_loc = (TokenLoc){ .src_id = 1 };

	return obj;
}

bool _setKeywords(BRP* obj, ...)
{
	va_list args;
	va_start(args, obj);
	sbuf kw;
	int n_kws = 1;
// calculating amount of keywords
	while ((kw = va_arg(args, sbuf)).data) { n_kws++; }
	va_end(args);
// copying the keywords array
	va_start(args, obj);
	obj->keywords = malloc(n_kws * sizeof(sbuf));
	if (!obj->keywords) return false;
	for (int i = 0; i < n_kws; i++) {
		obj->keywords[i] = va_arg(args, sbuf);
	}

	return true;
}

bool _setSymbols(BRP* obj, ...)
{
	va_list args;
	va_start(args, obj);
	sbuf symbol;
	int n_symbols = 1;
	int n_hidden_symbols = 1;
// calculating amount of symbols
	while ((symbol = va_arg(args, sbuf)).data) {
		n_symbols++;
		if (isSymbolSpecHidden(symbol)) n_hidden_symbols++;
	}
	va_end(args);
// copying the symbols array
	va_start(args, obj);
	obj->symbols = malloc(n_symbols * sizeof(sbuf));
	obj->hidden_symbols = malloc(n_hidden_symbols * sizeof(sbuf));
	if (!obj->symbols || !obj->hidden_symbols) return false;
	n_hidden_symbols = 0;
	for (int i = 0; i < n_symbols; i++) {
		obj->symbols[i] = va_arg(args, sbuf);
		if (obj->symbols[i].data ? isSymbolSpecHidden(obj->symbols[i]) : true) obj->hidden_symbols[n_hidden_symbols++] = obj->symbols[i];
	}
	va_end(args);

	return true;
}

bool setInputFrom(BRP *obj, char *name, FILE* fd)
{
	if (!InputCtxArray_append(
		&obj->sources,
		(InputCtx){
			.name = name,
			.src = fd,
			.cur_loc = (TokenLoc){
				.src_id = obj->sources.length,
				.colno = 1
			}
		}
	)) {
		obj->error_code = BRP_ERR_NO_MEMORY;
		return false;
	}

	return true;
}

bool setInput(BRP* obj, char* name)
{
	FILE* fd = fopen(name, "r");
	if (!fd) {
		obj->error_code = BRP_ERR_FILE_NOT_FOUND;
		obj->sys_errno = errno;
		return false;
	}
	return setInputFrom(obj, name, fd);
}

InputCtx* updateLineBuffer(BRP* obj)
{
	InputCtx* input = NULL;
	for (int i = obj->sources.length - 1; i >= 0; i--) {
		input = obj->sources.data + i;
		if (feof(input->src) || ferror(input->src)) break;
	}
	if (!input) return NULL;

	if (obj->buffer.length) input->cur_loc.colno += sbufstriplv(&obj->buffer, obj->hidden_symbols);
	while (!obj->buffer.length) {
		obj->buffer.data = fgetln(input->src, (size_t*)&obj->buffer.length);
		if (!obj->buffer.data) return NULL;
		if (obj->buffer.data[obj->buffer.length - 1] == '\n') obj->buffer.length--;
		input->cur_loc.lineno++;
		input->cur_loc.colno = sbufstriplv(&obj->buffer, obj->hidden_symbols) + 1;
	}

	return input;
}

Token fetchToken(BRP* obj)
{
	Token res = {0};
	TokenQueue_fetch(&obj->pending, &res);
	static_assert(N_TOKEN_TYPES == 6, "not all token types are handled");
	switch (res.type) {
		case TOKEN_NONE: break;
		case TOKEN_SYMBOL: if (isSymbolSpecHidden(obj->symbols[res.symbol_id])) break;
		return res;
		case TOKEN_STRING:
		case TOKEN_WORD: if (sbufspace(fromstr(res.word))) break;
		case TOKEN_KEYWORD:
		case TOKEN_INT:
		return res;
	}

	InputCtx* input = updateLineBuffer(obj);
	if (!input) return (Token){0};
	char* orig_ptr = obj->buffer.data;

	if (sbufcut(&obj->buffer, DQUOTE).data) {
		sbuf new;
		sbuf delim = sbufsplitesc(&obj->buffer, &new, DQUOTE);
		if (!sbufeq(delim, DQUOTE)) {
			obj->error_code = BRP_ERR_UNCLOSED_STR;
			obj->error_loc = input->cur_loc;
			return (Token){0};
		}

		res.type = TOKEN_STRING;
		res.loc = input->cur_loc;
		res.word = tostr(new);

		input->cur_loc.colno += sbufutf8len(new) + 2;
		return res;
	} else {
		res.type = TOKEN_WORD;
		sbuf new;
		int delim_id = sbufsplitv(&obj->buffer, &new, obj->symbols);

		if (new.length) {
			for (int i = 0; obj->keywords[i].data; i++) {
				if (sbufeq(obj->keywords[i], new)) {
					res.type = TOKEN_KEYWORD;
					res.keyword_id = i;
					break;
				}
			}
			if (res.type == TOKEN_WORD) {
				if (sbufint(new)) {
					res.type = TOKEN_INT;
					res.value = sbuftoint(new);
				}
			}
			if (res.type == TOKEN_WORD) {
				res.word = tostr(new);
			}

			res.loc = input->cur_loc;
			input->cur_loc.colno += new.length;

            if (delim_id >= 0 ? !isSymbolSpecHidden(obj->symbols[delim_id]) : false) {
                TokenQueue_add(
                    &obj->pending,
                    (Token){
                        .type = TOKEN_SYMBOL,
                        .loc = input->cur_loc,
                        .symbol_id = delim_id
                    }
                );
            } 
		} else {
			res.type = delim_id == -1 ? TOKEN_NONE : TOKEN_SYMBOL;
			res.loc = input->cur_loc;
			res.symbol_id = delim_id;
		}

		input->cur_loc.colno += obj->symbols[delim_id].length;

		return res;
	}
}

Token peekToken(BRP* obj)
{
	Token res = {0};
	if (obj->pending.length) {
		TokenQueue_peek(&obj->pending, &res);
	} else {
		res = fetchToken(obj);
		if (obj->pending.length) {
			Token swapped;
			TokenQueue_fetch(&obj->pending, &swapped);
			TokenQueue_add(&obj->pending, res);
			TokenQueue_add(&obj->pending, swapped);
		} else {
			TokenQueue_add(&obj->pending, res);
		}
	}
	return res;
}

char* getTokenSrcPath(BRP* obj, Token token)
{
	return obj->sources.data[token.loc.src_id].name;
}

int fprintTokenLoc(FILE* fd, TokenLoc loc, BRP* obj)
{
	if (loc.src_id != -1) {
		return fprintf(fd, "[ %s:%hd:%hd ] ", obj->sources.data[loc.src_id].name, loc.lineno, loc.colno);
	} else { return fprintf(fd, " "); }
}

int fprintTokenStr(FILE* fd, Token token, BRP* obj)
{
	switch (token.type) {
		case TOKEN_WORD: return fprintf(fd, "word `%s`", token.word);
		case TOKEN_INT: return fprintf(fd, "integer %lld", token.value);
		case TOKEN_STRING: 
			return fprintf(fd, "string \"") + fputsbufesc(fd, fromstr(token.word), BYTEFMT_HEX) + fputc('"', fd);
		case TOKEN_KEYWORD: return fprintf(fd, "keyword `"sbuf_format"`", unpack(obj->keywords[token.keyword_id]));
		case TOKEN_SYMBOL: return fprintf(fd, "symbol `"sbuf_format"`", unpack(obj->symbols[token.symbol_id]));
		default: return fprintf(fd, "nothing");
	}
}

int fprintToken(FILE* fd, Token token, BRP* obj)
{
	int res = fprintTokenLoc(fd, token.loc, obj);
	res += fprintTokenStr(fd, token, obj);
	res += fputc('\n', fd);
	return res;
}

char* TokenTypeNames[N_TOKEN_TYPES] = {
	"nothing",
	"word",
	"keyword",
	"symbol",
	"integer",
	"string"
};

char* getTokenTypeName(TokenType type)
{
	return TokenTypeNames[type];
}

char* getTokenWord(BRP* obj, Token token)
{
	switch (token.type) {
		case TOKEN_WORD:
		case TOKEN_STRING:
			return token.word;
		case TOKEN_KEYWORD:
			return obj->keywords[token.keyword_id].data;
		case TOKEN_SYMBOL:
			return obj->symbols[token.symbol_id].data;
		default: return NULL;
	}
}

void printBRPErrorStr(FILE* fd, BRP* obj) {
	switch (obj->error_code) {
		case BRP_ERR_OK: break;
		case BRP_ERR_UNCLOSED_STR: 
			fprintf(fd, "preprocessing error: unclosed string literal ");
		case BRP_ERR_NO_MEMORY: 
			fprintf(fd, "preprocessing error: memory allocation failure ");
		case BRP_ERR_FILE_NOT_FOUND: 
			fprintf(fd, "preprocessing error: could not open provided file (reason: %s) ", strerror(obj->sys_errno));
		default: fprintf(fd, "unreachable");
	}
}

void printBRPError(FILE* fd, BRP* obj)
{
	if (obj->error_code) {
		fprintTokenLoc(fd, obj->error_loc, obj);
		printBRPErrorStr(fd, obj);
		fputc('\n', fd);
	}
}

void delBRP(BRP* obj)
{
	free(obj->keywords);
	free(obj->symbols);
	free(obj->hidden_symbols);
	TokenQueue_delete(&obj->pending);
	InputCtxArray_clear(&obj->sources);
}

#endif // _BRP_