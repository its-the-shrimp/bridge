#define CTXALLOC_IMPLEMENTATION
#define SBUF_IMPLEMENTATION
#include "br.h"
#include "unistd.h"
#include "sys/param.h"
#include "stdarg.h"
#include "math.h"

const sbuf DQUOTE = fromcstr("\"");

bool IS_BIG_ENDIAN, IS_LITTLE_ENDIAN;
static struct timespec TIME;

Parser newParser(sbuf delims[], sbuf keywords[], heapctx_t ctx)
{
	Parser res = {
		.delims = delims,
		.keywords = keywords,
		.sources = strArray_new(ctx, -1),
		.nl_delim_id = -1,
		.tab_delim_id = -1,
		.space_delim_id = -1,
		.error_loc = (TokenLoc){0}
	};
	res.error_loc = (TokenLoc){0};
	for (int i = 0; delims[i].data; i++) {
		if (sbufeq(delims[i], NEWLINE)) {
			res.nl_delim_id = i;
		} else if (sbufeq(delims[i], TAB)) {
			res.tab_delim_id = i;
		} else if (sbufeq(delims[i], SPACE)) {
			res.space_delim_id = i;
		}
	}
	return res;
}

ParserError parseText(Parser* parser, TokenChain* dst, char* src_name, sbuf input)
{
	TokenLoc loc = {
		.lineno = 1,
		.colno = 1,
		.src_id = parser->sources.length
	};
	if (!strArray_append(&parser->sources, src_name)) return PARSER_ERR_NO_MEMORY;

	sbuf new, delim;
	int delim_id;
	Token* last_token = NULL;
	char* tmp;
	while (input.length) {
		if (input.data[0] == '"') {
			sbufshift(input, 1);
			delim = sbufsplitesc(&input, &new, DQUOTE, NEWLINE);
			if (!sbufeq(delim, DQUOTE)) {
				parser->error_loc = loc;
				return PARSER_ERR_UNCLOSED_STR;
			}

			if (last_token ? last_token->type == TOKEN_STRING : false) {
				tmp = tostr(chainctx(*dst), fromstr(last_token->word), new);
				ctxalloc_free(last_token->word);
				last_token->word = tmp;
			} else {
				if (!(last_token = TokenChain_append(
					dst,
					(Token){
						.type = TOKEN_STRING,
						.loc = loc,
						.word = tostr(chainctx(*dst), new)
					}
				))) return PARSER_ERR_NO_MEMORY;
			}
			
			loc.colno += sbufutf8len(new) + 2;
		} else {
			delim_id = sbufsplitv(&input, &new, parser->delims);
			if (new.length) {
				if (!(last_token = TokenChain_append(
					dst,
					(Token){
						.type = TOKEN_WORD,
						.loc = loc
					}
				))) return PARSER_ERR_NO_MEMORY;

				for (int i = 0; parser->keywords[i].data; i++) {
					if (sbufeq(new, parser->keywords[i])) {
						last_token->type = TOKEN_KEYWORD;
						last_token->keyword_id = i;
						break;
					}
				}
				if (last_token->type == TOKEN_WORD) {
					if (sbufint(new)) {
						last_token->type = TOKEN_INT;
						last_token->value = sbuftoint(new);
					} else {
						last_token->word = tostr(chainctx(*dst), new);
					}
				}
			}

			if (delim_id == parser->nl_delim_id) {
				loc.colno = 1; loc.lineno++;
			} else {
				loc.colno += sbufutf8len(new);
				if (delim_id != parser->space_delim_id && delim_id != parser->tab_delim_id) {
					if (!(last_token = TokenChain_append(
						dst,
						(Token){
							.type = TOKEN_SYMBOL,
							.loc = loc,
							.symbol_id = delim_id
						}
					))) return PARSER_ERR_NO_MEMORY;
				}
				loc.colno += sbufutf8len(parser->delims[delim_id]);
			}
		}
	}
	return PARSER_ERR_OK;
}

int fprintTokenLoc(FILE* fd, TokenLoc loc, Parser* parser)
{
	return fprintf(fd, "[ %s:%hd:%hd ] ", parser->sources.data[loc.src_id], loc.lineno, loc.colno);
}

int fprintTokenStr(FILE* fd, Token token, Parser* parser)
{
	switch (token.type) {
		case TOKEN_WORD: return fprintf(fd, "word `%s`", token.word);
		case TOKEN_INT: return fprintf(fd, "integer %lld", token.value);
		case TOKEN_STRING: 
			return fprintf(fd, "string \"") + fputsbufesc(fd, fromstr(token.word), BYTEFMT_HEX) + fputc('"', fd);
		case TOKEN_KEYWORD: return fprintf(fd, "keyword `"sbuf_format"`", unpack(parser->keywords[token.keyword_id]));
		case TOKEN_SYMBOL: return fprintf(fd, "symbol `"sbuf_format"`", unpack(parser->delims[token.symbol_id]));
		default: return fprintf(fd, "nothing");
	}
}

int fprintToken(FILE* fd, Token token, Parser* parser)
{
	int res = fprintTokenLoc(fd, token.loc, parser);
	res += fprintTokenStr(fd, token, parser);
	res += fputc('\n', fd);
	return res;
}

char* getTokenWord(Parser* parser, Token token)
{
	char intlit_size;
	switch (token.type) {
		case TOKEN_NONE: 
			return "\0";
		case TOKEN_WORD:
		case TOKEN_STRING:
			return token.word;
		case TOKEN_KEYWORD:
			return parser->keywords[token.keyword_id].data;
		case TOKEN_SYMBOL:
			return parser->delims[token.symbol_id].data;
		case TOKEN_INT:
			intlit_size = floorf(logf(token.value));
			char* res = ctxalloc_new(intlit_size + 2, arrayctx(parser->sources));
			res[intlit_size + 1] = '\0';
			for (char i = 0; i <= intlit_size; i++) {
				res[intlit_size - i] = token.value % 10 + '0';
				token.value /= 10;
			}
			return res;
		default: return NULL;
	}
}

void initBREnv(void)
{
	ctxalloc_init();
	int _e = 0xDEADBEEF;
	IS_BIG_ENDIAN = *(char*)&_e == 0xDE;
	IS_LITTLE_ENDIAN = !IS_BIG_ENDIAN;
}

char* getAbsolutePath(char* src, heapctx_t ctx)
{
	enter_tempctx(funcctx, 0);
	sbuf input = fromstr(src);
	if (!sbufstartswith(input, PATHSEP)) {
		char resbuf[MAXPATHLEN];
		getwd(resbuf);
		input = sbufconcat(TEMP_CTX, fromstr(resbuf), PATHSEP, input);
	}

	sbufArray components = sbufArray_new(TEMP_CTX, sbufcount(input, PATHSEP) * -1 - 1);
	sbuf new;
	
	while (input.length) {
		sbufsplit(&input, &new, PATHSEP);
		if (sbufeq(new, fromcstr(".."))) {
			sbufArray_pop(&components, -1);
		} else if (new.length && !sbufeq(new, fromcstr("."))) {
			sbufArray_append(&components, new);
		}
	}
	sbuf res = sctxalloc_new(input.length + 1, ctx);
	memset(res.data, 0, res.length);
	if (!res.data) return NULL;
	res.length = 0;
	array_foreach(sbuf, component, components,
		memcpy(res.data + res.length, PATHSEP.data, PATHSEP.length);
		res.length += PATHSEP.length;
		memcpy(res.data + res.length, component.data, component.length);
		res.length += component.length;
	);
	exit_tempctx(funcctx);
	return res.data;
}

void* BRByteOrder(void* src, long length) {
	char* _src = src;
	if (IS_LITTLE_ENDIAN) {
		for (long i = 0; i < length / 2; i++) {
			char tmp = _src[i];
			_src[i] = _src[length - i - 1];
			_src[length - i - 1] = tmp;
		}
	}
	return src;
}

bool startTimer(void)
{
	return !clock_gettime(CLOCK_MONOTONIC, &TIME);
}

float endTimer(void)
{
	struct timespec newtime;
	clock_gettime(CLOCK_MONOTONIC, &newtime);
	return (newtime.tv_sec - TIME.tv_sec) * 1000 + (newtime.tv_nsec - TIME.tv_nsec) / (float)1e6;
}
