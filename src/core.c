#define CTXALLOC_IMPLEMENTATION
#define SBUF_IMPLEMENTATION
#include "br.h"
#include "unistd.h"
#include "sys/param.h"
#include "math.h"
#include "time.h"
#include "errno.h"
#include "spawn.h"
#include "sys/wait.h"
extern char** environ;

defArray(sbuf);
defArray(InputCtx);

sbuf DQUOTE = fromcstr("\"");
sbuf NT_NEWLINE = fromcstr("\r\n");
sbuf POSIX_NEWLINE = fromcstr("\n");
sbuf NT_PATHSEP = fromcstr("\\");
sbuf POSIX_PATHSEP = fromcstr("/");

bool IS_BIG_ENDIAN, IS_LITTLE_ENDIAN;

Preprocessor newPreprocessor(sbuf delims[], sbuf keywords[], heapctx_t ctx)
{
	Preprocessor res = {
		.delims = delims,
		.keywords = keywords,
		.sources = InputCtxArray_new(ctx, -1),
		.error_code = PREP_ERR_OK,
		.error_loc = (TokenLoc){ .src_id = 1 },
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
		obj->sys_errno = errno;
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
	if (loc.src_id != -1) {
		return fprintf(fd, "[ %s:%hd:%hd ] ", obj->sources.data[loc.src_id].name, loc.lineno, loc.colno);
	} else { return fprintf(fd, " "); }
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

int printPrepError(FILE* fd, Preprocessor* obj) {
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
	if (!res.data) {
		exit_tempctx(funcctx);
		return NULL;
	}
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

bool startTimerAt(struct timespec* dst)
{
	return !clock_gettime(CLOCK_MONOTONIC, dst);
}

float endTimerAt(struct timespec* src)
{
	struct timespec newtime;
	clock_gettime(CLOCK_MONOTONIC, &newtime);
	return (newtime.tv_sec - src->tv_sec) * 1000 + (newtime.tv_nsec - src->tv_nsec) / (float)1e6;
}

bool execProcess(char* command, ProcessInfo* info)
{
	pid_t pid;
	int out_pipe[2], err_pipe[2], local_errno, exit_status;
	posix_spawn_file_actions_t file_actions;

	if ((local_errno = posix_spawn_file_actions_init(&file_actions))) {
		errno = local_errno;
		return false;
	}

	if (info->in) {
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, fileno(info->in), STDIN_FILENO))) {
			errno = local_errno;
			return false;
		}
	}
	if (info->out != stdout) {
		if (pipe(out_pipe) < 0) return false;
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, out_pipe[1], STDOUT_FILENO))) {
			errno = local_errno;
			return false;
		}
	}
	if (info->err != stderr) {
		if (pipe(err_pipe) < 0) return false;
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, err_pipe[1], STDERR_FILENO))) {
			errno = local_errno;
			return false;
		}
	}

	char* argv[] = { "sh", "-c", command, NULL };

	if ((local_errno = posix_spawn(&pid, "/bin/sh", &file_actions, NULL, argv, environ))) {
		errno = local_errno;
		return false;
	}
	
	if (waitpid(pid, &exit_status, 0) != pid) return false;

	if (info->out != stdout) {
		close(out_pipe[1]);
		info->out = fdopen(out_pipe[0], "r");
	} else { 
		info->out = NULL; 
	}
	if (info->err != stderr) {
		close(err_pipe[1]);
		info->err = fdopen(err_pipe[0], "r");
	} else {
		info->err = NULL;
	}
	info->exited = WIFEXITED(exit_status);
	info->exitcode = info->exited ? WEXITSTATUS(exit_status) : WTERMSIG(exit_status);

	return true;
}
