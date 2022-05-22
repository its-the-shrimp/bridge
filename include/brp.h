// The BRidge Preprocessor
#ifndef _BRP_
#define _BRP_

#include "sbuf.h"
#include "datasets.h"
#include "errno.h"
#include "assert.h"

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
	int32_t lineno;
	int32_t colno;
	char* src_name;
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

#define emptyToken ((Token){ .type = TOKEN_NONE })
#define wordToken(_word) ((Token){ .type = TOKEN_WORD, .word = _word })
#define keywordToken(kw_id) ((Token){ .type = TOKEN_KEYWORD, .keyword_id = kw_id })
#define symbolToken(_symbol_id) ((Token){ .type = TOKEN_SYMBOL, .symbol_id = _symbol_id })
#define intToken(_int) ((Token){ .type = TOKEN_INT, .value = _int })
#define stringToken(_word) ((Token){ .type = TOKEN_STRING, .word = _word })

typedef enum {
	BRP_ERR_OK,
	BRP_ERR_UNCLOSED_STR,
	BRP_ERR_NO_MEMORY,
	BRP_ERR_FILE_NOT_FOUND,
	BRP_ERR_SYMBOL_CONFLICT,
	N_PARSER_ERRORS
} BRPError;

typedef struct {
	char* name;
	FILE* src;
	TokenLoc cur_loc;
} InputCtx;
declArray(InputCtx);


typedef struct brp {
	sbuf* keywords;
	sbuf* symbols;
	sbuf* hidden_symbols;
	BRPError error_code;
	TokenLoc error_loc;
	union {
		sbuf error_symbol;
		char* error_filename;
		int32_t sys_errno;
	};
	void (*handler)(struct brp*);
	InputCtxArray sources;
	TokenQueue pending;
	sbuf buffer;
	TokenLoc last_loc;
} BRP;

typedef void (*BRPErrorHandler) (BRP*);

#define BRP_KEYWORD(spec) fromcstr(spec)
#define BRP_SYMBOL(spec) fromcstr(spec)
#define BRP_HIDDEN_SYMBOL(spec) ((sbuf){ .data = spec"\n", .length = sizeof(spec) - 1 })
// hidden symbols are delimiters that are not returned as generated tokens

char* getNormPath(char* src);
BRP* initBRP(BRP* obj, BRPErrorHandler handler);
void delBRP(BRP* obj);

bool _setKeywords(BRP* obj, ...);
bool _setSymbols(BRP* obj, ...);
#define setKeywords(obj, ...) _setKeywords(obj, __VA_ARGS__, (sbuf){0})
#define setSymbols(obj, ...) _setSymbols(obj, __VA_ARGS__, (sbuf){0})
bool setInputFrom(BRP *obj, char *name, FILE* fd);
bool setInput(BRP* obj, char* name);
Token fetchToken(BRP* obj);
Token peekToken(BRP* obj);
bool unfetchToken(BRP* obj, Token token);

int fprintTokenLoc(FILE* fd, TokenLoc loc);
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
#define printTokenLoc(loc) fprintTokenLoc(stdout, loc)
#define printTokenStr(token, parser) fprintTokenStr(stdout, token, parser)
#define printToken(token, parser) fprintToken(stdout, token, parser)
#define getTokenKeywordId(token) ( (token).type == TOKEN_KEYWORD ? (token).keyword_id : -1 )
#define getTokenSymbolId(token) ( (token).type == TOKEN_SYMBOL ? (token).symbol_id : -1 )
#define isWordToken(token) ( (token).type == TOKEN_WORD || (token).type == TOKEN_KEYWORD )
#define isSymbolSpecHidden(spec) (spec.data[spec.length] > 0)

#endif 


#ifdef BRP_IMPLEMENTATION
#undef BRP_IMPLEMENTATION

defArray(sbuf);
defArray(InputCtx);
defQueue(Token);

char* getNormPath(char* src)
{
	sbuf input = fromstr(src);
	sbufArray components = sbufArray_new(sbufcount(input, PATHSEP) * -1 - 1);
	sbuf new;
	
	while (input.length) {
		sbufsplit(&input, &new, PATHSEP);
		if (sbufeq(new, fromcstr(".."))) {
			sbufArray_pop(&components, -1);
		} else if (new.length && !sbufeq(new, fromcstr("."))) {
			sbufArray_append(&components, new);
		}
	}
	sbuf res = smalloc(input.length + 1);
	memset(res.data, 0, res.length);
	if (!res.data) {
		sbufArray_clear(&components);
		return NULL;
	}
	res.length = 0;
	array_foreach(sbuf, component, components,
	 	if (res.length) {
			memcpy(res.data + res.length, PATHSEP.data, PATHSEP.length);
			res.length += PATHSEP.length;
		}
		memcpy(res.data + res.length, component.data, component.length);
		res.length += component.length;
	);
	free(components.data);
	return res.data;
}


BRP* initBRP(BRP* obj, BRPErrorHandler handler)
{
	*obj = (BRP){0};
	obj->sources = InputCtxArray_new(-1);
	if (!obj->sources.data) return NULL;
	obj->pending = TokenQueue_new(0);
	obj->error_loc = (TokenLoc){0};
	obj->handler = handler;

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

	for (int i = 0; i < n_symbols; i++) {
		for (int i1 = 0; i1 < i; i1++) {
			if (sbufstartswith(obj->symbols[i], obj->symbols[i1])) {
				obj->error_code = BRP_ERR_SYMBOL_CONFLICT;
				obj->error_symbol = obj->symbols[i];
				if (obj->handler) obj->handler(obj);
				return false;
			}
		}
	}

	return true;
}

bool setInputFrom(BRP *obj, char *name, FILE* fd)
{
	char* real_path = getNormPath(name);
	if (!InputCtxArray_append(
		&obj->sources,
		(InputCtx){
			.name = real_path,
			.src = fd,
			.cur_loc = (TokenLoc){
				.src_name = real_path,
				.colno = 1
			}
		}
	)) {
		obj->error_code = BRP_ERR_NO_MEMORY;
		if (obj->handler) obj->handler(obj);
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
		if (obj->handler) obj->handler(obj);
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
		if (sbufstartswith(obj->buffer, fromcstr("#!"))) {
			obj->buffer.length = 0;
			input->cur_loc.lineno++;
			continue;
		}
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
	if (!input) return (Token){ .type = TOKEN_NONE, .loc = obj->last_loc };
	char* orig_ptr = obj->buffer.data;

	if (sbufcut(&obj->buffer, DQUOTE).data) {
		sbuf new;
		sbuf delim = sbufsplitesc(&obj->buffer, &new, DQUOTE);
		if (!sbufeq(delim, DQUOTE)) {
			obj->error_code = BRP_ERR_UNCLOSED_STR;
			obj->error_loc = input->cur_loc;
			if (obj->handler) obj->handler(obj);
			return (Token){ .type = TOKEN_NONE, .loc = obj->last_loc };
		}

		res.type = TOKEN_STRING;
		res.loc = input->cur_loc;
		res.word = tostr(new);

		input->cur_loc.colno += sbufutf8len(new) + 2;
		obj->last_loc = res.loc;
		return res;
	} else {
		res.type = TOKEN_WORD;
		sbuf new;
		int delim_id;
		if (obj->buffer.data[0] == '-' && (obj->buffer.length > 1 ? (obj->buffer.data[1] >= '0' && obj->buffer.data[1] <= '9') : false)) {
			sbufshift(obj->buffer, 1);
			delim_id = sbufsplitv(&obj->buffer, &new, obj->symbols);
			sbufshift(new, -1);
		} else {
			delim_id = sbufsplitv(&obj->buffer, &new, obj->symbols);
		}

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

		obj->last_loc = res.loc;
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

bool unfetchToken(BRP* obj, Token token)
{
	return TokenQueue_unfetch(&obj->pending, token);
}

int fprintTokenLoc(FILE* fd, TokenLoc loc)
{
	if (loc.src_name) {
		return fprintf(fd, "[%s:%d:%d] ", loc.src_name, loc.lineno, loc.colno);
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
	int res = fprintTokenLoc(fd, token.loc);
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
			fprintf(fd, "unclosed string literal ");
			break;
		case BRP_ERR_NO_MEMORY: 
			fprintf(fd, "memory allocation failure ");
			break;
		case BRP_ERR_FILE_NOT_FOUND: 
			fprintf(fd, "could not open provided file (reason: %s) ", strerror(obj->sys_errno));
			break;
		case BRP_ERR_SYMBOL_CONFLICT:
			fprintf(
				fd,
				"symbol conflict: symbol `%.*s` will be incorrectly parsed; to solve this, place it before the symbol that has a common start ",
				unpack(obj->error_symbol)
			);
			break;
		default: fprintf(fd, "unreachable");
	}
}

void printBRPError(FILE* fd, BRP* obj)
{
	if (obj->error_code) {
		fprintTokenLoc(fd, obj->error_loc);
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