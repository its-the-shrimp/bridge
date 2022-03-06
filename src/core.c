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

Preprocessor newPreprocessor(sbuf delims[], sbuf keywords[], heapctx_t ctx)
{
	Preprocessor res = {
		.delims = delims,
		.keywords = keywords,
		.sources = InputCtxArray_new(ctx, -1),
		.error_code = PREP_ERR_OK,
		.error_loc = (TokenLoc){0},
		._nl_delim_id = 1
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

bool setInputFrom(Preprocessor *obj, char *name, sbuf input)
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

bool setInput(Preprocessor* obj, char* name)
{
	FILE* fd = fopen(name, "r");
	if (!fd) {
		obj->error_code = PREP_ERR_FILE_NOT_FOUND;
		return false;
	}
	sbuf input = filecontent(fd, arrayctx(obj->sources));
	if (!input.data) {
		obj->error_code = PREP_ERR_NO_MEMORY;
		return false;
	}

	fclose(fd);
	return setInputFrom(obj, name, input);
}

Token fetchToken(Preprocessor* obj)
{
	Token res;
	if (obj->pending.type && !sbufspace(fromstr(getTokenWord(obj, obj->pending)))) {
		res = obj->pending;
		obj->pending.type = TOKEN_NONE;
		return res;
	}

	InputCtx* input = NULL;
	for (int i = obj->sources.length - 1; i >= 0; i--) {
		input = obj->sources.data + i;
		if (input->content.length) { break; }
	}
	if (!input) { return (Token){0}; }

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
		res.word = tostr(arrayctx(obj->sources), new);

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
				res.word = tostr(arrayctx(obj->sources), new);
			}

			res.loc = input->cur_loc;
			input->cur_loc.colno += new.length;

			obj->pending.type = delim_id == -1 ? TOKEN_NONE : TOKEN_SYMBOL;
			obj->pending.loc = input->cur_loc;
			obj->pending.symbol_id = delim_id;
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

int fprintTokenLoc(FILE* fd, TokenLoc loc, Preprocessor* obj)
{
	return fprintf(fd, "[ %s:%hd:%hd ] ", obj->sources.data[loc.src_id].name, loc.lineno, loc.colno);
}

int fprintTokenStr(FILE* fd, Token token, Preprocessor* obj)
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

int fprintToken(FILE* fd, Token token, Preprocessor* obj)
{
	int res = fprintTokenLoc(fd, token.loc, obj);
	res += fprintTokenStr(fd, token, obj);
	res += fputc('\n', fd);
	return res;
}

char* getTokenWord(Preprocessor* obj, Token token)
{
	char intlit_size;
	switch (token.type) {
		case TOKEN_NONE: 
			return "\0";
		case TOKEN_WORD:
		case TOKEN_STRING:
			return token.word;
		case TOKEN_KEYWORD:
			return obj->keywords[token.keyword_id].data;
		case TOKEN_SYMBOL:
			return obj->delims[token.symbol_id].data;
		case TOKEN_INT:
			intlit_size = floorf(logf(token.value));
			char* res = ctxalloc_new(intlit_size + 2, arrayctx(obj->sources));
			res[intlit_size + 1] = '\0';
			for (char i = 0; i <= intlit_size; i++) {
				res[intlit_size - i] = token.value % 10 + '0';
				token.value /= 10;
			}
			return res;
		default: return NULL;
	}
}

char* getErrorStr(Preprocessor* obj) {
	switch (obj->error_code) {
		case PREP_ERR_UNCLOSED_STR: return "unclosed string literal";
		case PREP_ERR_NO_NEWLINE_DELIM: return "no newline delimiter provided";
		case PREP_ERR_NO_MEMORY: return "memory allocation failure";
		case PREP_ERR_FILE_NOT_FOUND: return "could not open provided file";
		default: return "unreachable";
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
