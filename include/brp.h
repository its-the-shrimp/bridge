// The BRidge BRP
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
	PREP_ERR_OK,
	PREP_ERR_UNCLOSED_STR,
	PREP_ERR_NO_NEWLINE_DELIM,
	PREP_ERR_NO_MEMORY,
	PREP_ERR_FILE_NOT_FOUND,
	N_PARSER_ERRORS
} BRPError;

typedef struct {
	char* name;
	sbuf content;
	TokenLoc cur_loc;
} InputCtx;
declArray(InputCtx);

typedef struct {
	sbuf* keywords;
	sbuf* delims;
	BRPError error_code;
	union {
		TokenLoc error_loc;
		char* error_filename;
		int32_t sys_errno;
	};
	InputCtxArray sources;
	int _nl_delim_id;
	TokenQueue pending;
} BRP;

#define BRP_KEYWORD(spec) fromcstr(spec)
#define BRP_SYMBOL(spec) fromcstr(spec)
#define BRP_HIDDEN_SYMBOL(spec) ((sbuf){ .data = spec"\n", .length = sizeof(spec) - 1 })
// hidden symbols are delimiters that are not returned as geenrated tokens

BRP newBRP(sbuf delims[], sbuf keywords[]);
bool setInputFrom(BRP* obj, char* name, sbuf input);
bool setInput(BRP* obj, char* name);
Token fetchToken(BRP* obj);
Token peekToken(BRP* obj);

int fprintTokenLoc(FILE* fd, TokenLoc loc, BRP* obj);
int fprintTokenStr(FILE* fd, Token token, BRP* obj);
int fprintToken(FILE* fd, Token token, BRP* obj);
char* getTokenTypeName(TokenType type);
char* getTokenWord(BRP* obj, Token token);
int printBRPError(FILE* fd, BRP* obj);


#define setBRPError(prep, code, loc) \
	if ((prep)->error_code == PREP_ERR_OK) { \
		(prep)->error_code = code; \
		(prep)->error_loc = loc; \
	}
#define BRPempty(prep) ( (prep)->sources.length ? (prep)->sources.data[0].content.length == 0 : true )
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

BRP newBRP(sbuf delims[], sbuf keywords[])
{
	BRP res = {
		.delims = delims,
		.keywords = keywords,
		.sources = InputCtxArray_new(-1),
		.error_code = PREP_ERR_OK,
		.error_loc = (TokenLoc){ .src_id = 1 },
		._nl_delim_id = 1,
		.pending = TokenQueue_new(0)
	};

	for (int i = 0; delims[i].data; i++)
	{
		if (sbufeq(delims[i], NEWLINE)) {
			res._nl_delim_id = i;
			break;
		}
	}

	return res;
}

bool setInputFrom(BRP *obj, char *name, sbuf input)
{
	if (!InputCtxArray_append(
		&obj->sources,
		(InputCtx){
			.name = name,
			.content = input,
			.cur_loc = (TokenLoc){
				.src_id = obj->sources.length,
				.lineno = 1,
				.colno = 1
			}
		}
	)) {
		obj->error_code = PREP_ERR_NO_MEMORY;
		return false;
	}

	return true;
}

bool setInput(BRP* obj, char* name)
{
	FILE* fd = fopen(name, "r");
	if (!fd) {
		obj->error_code = PREP_ERR_FILE_NOT_FOUND;
		obj->sys_errno = errno;
		return false;
	}
	sbuf input = filecontent(fd);
	if (!input.data) {
		obj->error_code = PREP_ERR_NO_MEMORY;
		return false;
	}

	fclose(fd);
	return setInputFrom(obj, name, input);
}

Token fetchToken(BRP* obj)
{
	Token res = {0};
	TokenQueue_fetch(&obj->pending, &res);
	if (res.type && !sbufspace(fromstr(getTokenWord(obj, res)))) {
		return res;
	}

	InputCtx* input = NULL;
	for (int i = obj->sources.length - 1; i >= 0; i--) {
		input = obj->sources.data + i;
		if (input->content.length) { break; }
	}
	if (!input) return (Token){0};

	char* orig_ptr = input->content.data;
	while (input->content.length > 0) {
		if (input->content.data[0] == '\n') { input->cur_loc.lineno++; input->cur_loc.colno = 1; }
		else if (input->content.data[0] > 32) { break; }
		else { input->cur_loc.colno++; }
		sbufshift(input->content, 1);
	}

	if (sbufcut(&input->content, DQUOTE).data) {
		sbuf new;
		sbuf delim = sbufsplitesc(&input->content, &new, DQUOTE, NEWLINE);
		if (!sbufeq(delim, DQUOTE)) {
			obj->error_code = PREP_ERR_UNCLOSED_STR;
			obj->error_loc = input->cur_loc;
			return (Token){0};
		}

		res.type = TOKEN_STRING;
		res.loc = input->cur_loc;
		res.word = tostr(new);

		input->cur_loc.colno += sbufutf8len(delim) + 2;
		return res;
	} else {
		res.type = TOKEN_WORD;
		sbuf new;
		int delim_id = sbufsplitv(&input->content, &new, obj->delims);

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

            if (delim_id >= 0 ? !isSymbolSpecHidden(obj->delims[delim_id]) : false) {
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

		if (delim_id == obj->_nl_delim_id) {
			input->cur_loc.lineno++;
			input->cur_loc.colno = 1;
		} else {
			input->cur_loc.colno += obj->delims[delim_id].length;
		}

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
		case TOKEN_SYMBOL: return fprintf(fd, "symbol `"sbuf_format"`", unpack(obj->delims[token.symbol_id]));
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
			return obj->delims[token.symbol_id].data;
		default: return NULL;
	}
}

int printBRPError(FILE* fd, BRP* obj) {
	switch (obj->error_code) {
		case PREP_ERR_OK: return 0;
		fprintTokenLoc(fd, obj->error_loc, obj);
		case PREP_ERR_UNCLOSED_STR: 
			return fprintf(fd, "preprocessing error: unclosed string literal\n");
		case PREP_ERR_NO_NEWLINE_DELIM: 
			return fprintf(fd, "preprocessing error: no newline delimiter provided\n");
		case PREP_ERR_NO_MEMORY: 
			return fprintf(fd, "preprocessing error: memory allocation failure\n");
		case PREP_ERR_FILE_NOT_FOUND: 
			return fprintf(fd, "preprocessing error: could not open provided file (reason: %s)\n", strerror(obj->sys_errno));
		default: return fprintf(fd, "unreachable\n");
	}
}


#endif