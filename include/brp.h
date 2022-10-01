// The BRidge Preprocessor
#ifndef _BRP_
#define _BRP_

#include <br_utils.h>
#include <sbuf.h>
#include <datasets.h>
#include <errno.h>
#include <sys/cdefs.h>
#include <external/arena.h>

typedef enum {
	TOKEN_NONE,
	TOKEN_WORD,
	TOKEN_KEYWORD,
	TOKEN_SYMBOL,
	TOKEN_INT,
	TOKEN_STRING,
	N_TOKEN_TYPES
} BRP_TokenType;

typedef struct BRP_TokenLoc BRP_TokenLoc;
struct BRP_TokenLoc {
	uint32_t lineno;
	uint32_t colno;
	char* src_name;
	BRP_TokenLoc* included_from;
};
declArray(BRP_TokenLoc);

typedef struct {
	int8_t type;
	BRP_TokenLoc loc;
	union {
		char* word; // for TOKEN_WORD
		sbuf string; // for TOKEN_STRING
		int64_t value; // for TOKEN_INT
		uint32_t keyword_id; // for TOKEN_KEYWORD
		uint32_t symbol_id; // for TOKEN_SYMBOL
	};
} BRP_Token;
#define BRP_emptyToken ((BRP_Token){ .type = TOKEN_NONE })
#define BRP_wordToken(_word) ((BRP_Token){ .type = TOKEN_WORD, .word = _word })
#define BRP_keywordToken(kw_id) ((BRP_Token){ .type = TOKEN_KEYWORD, .keyword_id = kw_id })
#define BRP_symbolToken(_symbol_id) ((BRP_Token){ .type = TOKEN_SYMBOL, .symbol_id = _symbol_id })
#define BRP_intToken(_int) ((BRP_Token){ .type = TOKEN_INT, .value = _int })
#define BRP_stringToken(_word) ((BRP_Token){ .type = TOKEN_STRING, .word = _word })

typedef enum {
	BRP_ERR_OK,
	BRP_ERR_UNCLOSED_STR,
	BRP_ERR_NO_MEMORY,
	BRP_ERR_FILE_NOT_FOUND,
	BRP_ERR_SYMBOL_CONFLICT,
	BRP_ERR_INVALID_CMD_SYNTAX,
	BRP_ERR_UNKNOWN_CMD,
	BRP_ERR_UNCLOSED_MACRO,
	BRP_ERR_EXCESS_ENDIF,
	BRP_ERR_EXCESS_ENDMACRO,
	BRP_ERR_UNCLOSED_CONDITION,
	BRP_ERR_EXCESS_ELSE,
	BRP_ERR_UNCLOSED_CHAR,
	BRP_ERR_MACRO_ARG_COUNT_MISMATCH,
	N_BRP_ERRORS
} BRP_Error;

typedef struct macro_arg {
	char* name;
	BRP_TokenLoc def_loc;
} BRP_MacroArg;

declArray(BRP_MacroArg);
typedef struct macro {
	char* name;
	sbuf def;
	BRP_TokenLoc def_loc;
	BRP_MacroArgArray args;
} BRP_Macro;
declArray(BRP_Macro);

typedef struct input_ctx {
	struct input_ctx* prev;
	sbuf buffer;
	char* orig_data;
	BRP_TokenLoc cur_loc;
	BRP_MacroArray locals;
} BRP_InputCtx;

declArray(BRP_Token);
typedef struct BRP BRP;
typedef void (*BRP_ErrorHandler) (BRP*);
struct BRP {
	sbuf* keywords;
	sbuf* symbols;
	sbuf* hidden_symbols;
	short _dquote_symbol_id;
	short _quote_symbol_id;
	char flags;
	BRP_Error error_code;
	BRP_TokenLoc error_loc;
	union {
		sbuf error_symbol;
		struct {
			char* error_macro_name;
			int error_macro_n_args;
			int error_n_args;
			BRP_TokenLoc error_macro_def_loc;
		};
		char* error_filename;
		int32_t sys_errno;
	};
	BRP_ErrorHandler handler;
	BRP_InputCtx cur_input;
	BRP_TokenArray pending;
	BRP_MacroArray macros;
	BRP_TokenLocArray conditional_blocks;
	Arena arena;
};
#define BRP_KEYWORD(spec) fromcstr(spec)
#define BRP_SYMBOL(spec) fromcstr(spec)
// hidden symbols are delimiters that are not returned as generated tokens
#define BRP_HIDDEN_SYMBOL(spec) ((sbuf){ .data = spec"\n", .length = sizeof(spec) - 1 })

#define BRPempty(prep) ((prep)->cur_input.buffer.length + (size_t)(prep)->cur_input.prev == 0)
#define BRP_isWordToken(token) ( (token).type == TOKEN_WORD || (token).type == TOKEN_KEYWORD )
#define BRP_isSymbolSpecHidden(spec) ((spec).data[(spec).length] > 0)

typedef void (*BRPErrorHandler) (BRP*);

BRP* BRP_initBRP(BRP* obj, BRPErrorHandler handler, char flags);
// flags for BRP_initBRP function
#define BRP_ESC_STR_LITERALS 0x1
void BRP_delBRP(BRP* obj);

bool BRP_setKeywords(BRP* obj, sbuf kws[]);

bool BRP_setSymbols(BRP* obj, sbuf symbols[]);

bool BRP_appendInput(BRP* obj, BRP_InputCtx* input, FILE* input_fd, BRP_TokenLoc initial_loc, BRP_TokenLoc include_loc);
#define BRP_setInput(obj, path, input_fd) BRP_appendInput(obj, &(obj)->cur_input, input_fd, (BRP_TokenLoc){ .src_name = path, .colno = 1, .lineno = 1, .included_from = NULL }, (BRP_TokenLoc){0})

BRP_Token BRP__fetchToken(BRP* obj, BRP_InputCtx* input, BRP_TokenArray* queue);
#define BRP_fetchToken(obj) BRP__fetchToken(obj, &(obj)->cur_input, &(obj)->pending)

BRP_Token BRP_peekToken(BRP* obj);
bool BRP_unfetchToken(BRP* obj, BRP_Token token);

void BRP_fprintTokenLoc(FILE* fd, BRP_TokenLoc loc);
#define BRP_printTokenLoc(loc) BRP_fprintTokenLoc(stdout, loc)

void BRP_fprintTokenStr(FILE* fd, BRP_Token token, BRP* obj);
#define BRP_printTokenStr(token, parser) BRP_fprintTokenStr(stdout, token, parser)

void BRP_fprintToken(FILE* fd, BRP_Token token, BRP* obj);
#define BRP_printToken(token, parser) BRP_fprintToken(stdout, token, parser)

void BRP_fprintTokenText(FILE* fd, BRP_Token token, BRP* obj);
#define BRP_printTokenText(token, parser) BRP_fprintTokenText(stdout, token, parser)

int BRP_getTokenSymbolId(BRP_Token token);
int BRP_getTokenKeywordId(BRP_Token token);
char* BRP_getTokenTypeName(BRP_TokenType type);
char* BRP_getTokenWord(BRP* obj, BRP_Token token);
void BRP_printBRPErrorStr(FILE* fd, BRP* obj);
void BRP_printBRPError(BRP* obj);

#endif // _BRP_

#if defined(BRP_IMPLEMENTATION) && !defined(_BRP_IMPL_LOCK)
#define _BRP_IMPL_LOCK

#define BR_UTILS_IMPLEMENTATION
#include <br_utils.h>
#define SBUF_IMPLEMENTATION
#include <sbuf.h>

defArray(BRP_TokenLoc);
defArray(BRP_Macro);
defArray(sbuf);
defArray(BRP_Token);
defArray(BRP_MacroArg);

static bool pathEquals(const char path1[], const char path2[])
{
	char path1_r[256];
	char path2_r[256];
	realpath(path1, path1_r);
	realpath(path2, path2_r);
	return streq(path1_r, path2_r);
}

bool BRP_setKeywords(BRP* const obj, sbuf* const kws)
{
	long n_kws = 1;
	for (sbuf* kw = kws; kw->data; ++kw) ++n_kws;
	obj->keywords = memcpy(arena_alloc(&obj->arena, n_kws * sizeof(sbuf)), kws, n_kws * sizeof(sbuf));
	return obj->keywords != NULL;
}

bool BRP_setSymbols(BRP* obj, sbuf symbols[])
{
	int n_symbols = 1;
	int n_hidden_symbols = 1;
// calculating amount of symbols
	for (sbuf* symbol = symbols; symbol->data; ++symbol) {
		n_symbols++;
		if (BRP_isSymbolSpecHidden(*symbol)) n_hidden_symbols++;
	}
// copying the symbols array
	obj->symbols = arena_alloc(&obj->arena, (n_symbols + 2) * sizeof(sbuf));
	obj->hidden_symbols = arena_alloc(&obj->arena, n_hidden_symbols * sizeof(sbuf));
	if (!obj->symbols || !obj->hidden_symbols) return false;

	n_hidden_symbols = 0;
	for (int i = 0; i < n_symbols - 1; i++) {
		obj->symbols[i] = symbols[i];
		if (BRP_isSymbolSpecHidden(obj->symbols[i])) obj->hidden_symbols[n_hidden_symbols++] = obj->symbols[i];
	}

	obj->_dquote_symbol_id = n_symbols - 1;
	obj->symbols[n_symbols - 1] = DQUOTE;
	obj->_quote_symbol_id = n_symbols;
	obj->symbols[n_symbols] = QUOTE;
	obj->symbols[n_symbols + 1] = obj->hidden_symbols[n_hidden_symbols] = (sbuf){0};

	for (int i = 0; i < n_symbols; i++) {
		for (int i1 = 0; i1 < i; i1++) {
			if (sbufstartswith(obj->symbols[i], obj->symbols[i1])) {
				obj->error_code = BRP_ERR_SYMBOL_CONFLICT;
				obj->error_symbol = obj->symbols[i];
				obj->handler(obj);
				return false;
			}
		}
	}

	return true;
}

static sbuf removeComments(sbuf buffer)
{
	char* to_free = buffer.data;
	static sbuf comment_delims[5] = {
		fromcstr("\""),
		fromcstr("'"),
		fromcstr("//"),
		fromcstr("/*"),
		(sbuf){0}
	};
	sbuf res = fromcstr("");
	
	while (buffer.length) {
		sbuf part, other_part;
		switch ((sbufsplitv(&buffer, &part, comment_delims))) {
			case 0: // double quote
				sbufsplitesc(&buffer, &other_part, comment_delims[0]);
				res = sbufconcat(res, part, comment_delims[0], other_part, comment_delims[0]);
				break;
			case 1: // single quote
				sbufsplitesc(&buffer, &other_part, comment_delims[1]);
				res = sbufconcat(res, part, comment_delims[1], other_part, comment_delims[1]);
				break;
			case 2: // line-terminated comment (like the one this text is in)
				sbufsplit(&buffer, &other_part, NEWLINE);
				res = sbufconcat(res, part);
				break;
			case 3: /* free-form comment (like the one this text is in) */
				sbufsplit(&buffer, &other_part, fromcstr("*/"));
				res = sbufconcat(res, part);
				break;
			default:
				res = sbufconcat(res, part);
				break;
		}
	}
	free(to_free);
	return res;
}

bool BRP_appendInput(BRP *obj, BRP_InputCtx* const input, FILE* input_fd, BRP_TokenLoc initial_loc, BRP_TokenLoc include_loc)
{
	if (!input_fd) {
		obj->error_code = BRP_ERR_FILE_NOT_FOUND;
		obj->error_loc = include_loc;
		obj->error_symbol = fromstr((char*)initial_loc.src_name);
		obj->handler(obj);
		return false;
	}

	BRP_InputCtx* prev_ctx = arena_alloc(&obj->arena, sizeof(BRP_InputCtx));
	*prev_ctx = *input;
	input->cur_loc = initial_loc;
	input->buffer = removeComments(filecontent(input_fd));
	input->prev = prev_ctx;

	if (!input->buffer.data) {
		obj->error_code = BRP_ERR_NO_MEMORY;
		obj->error_loc = include_loc;
		obj->handler(obj);
		fclose(input_fd);
		return false;
	}
	

	input->orig_data = input->buffer.data;
	if (include_loc.src_name) {
		input->cur_loc.included_from = arena_alloc(&obj->arena, sizeof(BRP_TokenLoc));
		*input->cur_loc.included_from = include_loc;
	}

	sbufstriprv(&input->buffer, obj->hidden_symbols);
	fclose(input_fd);
	return true;
}

static void delInput(BRP* obj, BRP_InputCtx* const input)
{
	BRP_InputCtx* to_free = input->prev;
	if (to_free) {
		*input = *input->prev;
	} else *input = (BRP_InputCtx){0};

	if (!input && obj->conditional_blocks.length) {
		obj->error_code = BRP_ERR_UNCLOSED_CONDITION;
		obj->error_loc = BRP_TokenLocArray_pop(&obj->conditional_blocks, -1);
		obj->handler(obj);
	}
}

static sbuf nl_arg[2] = { NEWLINE };

static void cleanInput(BRP* obj, BRP_InputCtx* const input)
{
	while (!input->buffer.length) {
		if (!input->prev) return;
		delInput(obj, input);
	}

	sbuf stripped = sbufstriplv(&input->buffer, obj->hidden_symbols);
	int nl_count = sbufcount_v(stripped, nl_arg);
	if (nl_count) {
		input->cur_loc.lineno += nl_count;
		sbuf last_stripped_line;
		sbufsplitrv(&stripped, &last_stripped_line, nl_arg);
		input->cur_loc.colno = stripped.length + 1;
	} else input->cur_loc.colno += stripped.length;

	while (!input->buffer.length) {
		if (!input->prev) return;
		delInput(obj, input);
	}
}

typedef enum {
	BRP_INCLUDE,
	BRP_DEFINE,
	BRP_MACRO,
	BRP_COMMENT,
	BRP_IFDEF,
	BRP_ENDIF,
	BRP_ENDMACRO,
	BRP_IFNDEF,
	BRP_ELSE,
	N_BRP_CMDS
} BRPDirectiveCode;

static const sbuf brp_directives[N_BRP_CMDS + 1] = {
	[BRP_INCLUDE] = fromcstr("include"),
	[BRP_DEFINE] = fromcstr("define"),
	[BRP_MACRO] = fromcstr("macro"),
	[BRP_COMMENT] = fromcstr("!"),
	[BRP_IFDEF] = fromcstr("ifdef"),
	[BRP_ENDIF] = fromcstr("endif"),
	[BRP_ENDMACRO] = fromcstr("endmacro"),
	[BRP_IFNDEF] = fromcstr("ifndef"),
	[BRP_ELSE] = fromcstr("else")
};

static uint8_t oppositeBrackets[UINT8_MAX] = {
	['('] = ')', [')'] = '(',
	['['] = ']', [']'] = '[',
	['{'] = '}', ['}'] = '{'
};

static inline BRP_Macro fetchMacroArg(BRP* const obj, BRP_InputCtx* const input, const BRP_MacroArg arg)
{
	BRP_Macro res = { .name = arg.name, .def_loc = arg.def_loc };
	bool isnt_last_arg = true;
	while (true) {
		sbuf delim = sbufsplit(&input->buffer, &res.def, fromcstr(","), fromcstr("("), fromcstr("["), fromcstr("{"), fromcstr(")"));
		if (delim.length ? sbufeq(delim, NEWLINE) : true) {
			obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
			obj->error_loc = input->cur_loc;
			obj->error_loc.colno += res.def.length;
			obj->handler(obj);
			return (BRP_Macro){0};
		}

		if (delim.data[0] == ',') break;
		if (delim.data[0] == ')') {
			sbufshift(input->buffer, -1);
			isnt_last_arg = false;
			break;
		}
		sbuf also_def;
		delim = sbufsplit(&input->buffer, &also_def, fromchar(oppositeBrackets[(unsigned)delim.data[0]]), NEWLINE);
		if (!delim.data ? sbufeq(delim, NEWLINE) : false) {
			obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
			obj->error_loc = input->cur_loc;
			obj->error_loc.colno += res.def.length;
			obj->handler(obj);
			return (BRP_Macro){0};
		}
		res.def.length += also_def.length + 2;
	}

	input->cur_loc.colno += res.def.length + isnt_last_arg;
	return res;
}

static void preprocessInput(BRP* obj, BRP_InputCtx* const input);

static inline BRP_InputCtx* expandMacro(BRP* const obj, BRP_InputCtx* const input, const BRP_Macro macro)
{
	BRP_InputCtx* prev_ctx = arena_alloc(&obj->arena, sizeof(BRP_InputCtx));
	*prev_ctx = *input;
	input->buffer = macro.def;
	input->orig_data = NULL;
	input->cur_loc = macro.def_loc;
	input->prev = prev_ctx;
	input->locals = BRP_MacroArray_new(-macro.args.length);
	input->locals.length = macro.args.length;
	input->cur_loc.included_from = arena_alloc(&obj->arena, sizeof(BRP_TokenLoc));
	*input->cur_loc.included_from = prev_ctx->cur_loc;
	int name_length = strlen(macro.name);
	sbufshift(prev_ctx->buffer, name_length);
	prev_ctx->cur_loc.colno += name_length;

	if (sbufcutc(&prev_ctx->buffer, fromcstr("("))) {
		prev_ctx->cur_loc.colno += 1;
		arrayForeach (BRP_MacroArg, arg, macro.args) {
			if (!prev_ctx->buffer.length) {
				obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
				obj->error_loc = prev_ctx->cur_loc;
				obj->handler(obj);
				return NULL;
			}
			if (prev_ctx->buffer.data[0] == ')') {
				obj->error_code = BRP_ERR_MACRO_ARG_COUNT_MISMATCH;
				obj->error_loc = prev_ctx->cur_loc;
				obj->error_n_args = input->locals.length;
				obj->error_macro_def_loc = macro.def_loc;
				obj->error_macro_name = macro.name;
				obj->error_macro_n_args = macro.args.length;
				obj->handler(obj);
				return NULL;
			}

			input->locals.data[arg - macro.args.data] = fetchMacroArg(obj, prev_ctx, *arg);
		}
		if (prev_ctx->buffer.length ? prev_ctx->buffer.data[0] != ')' : true) {
			obj->error_code = BRP_ERR_MACRO_ARG_COUNT_MISMATCH;
			obj->error_loc = prev_ctx->cur_loc;
			obj->error_n_args = -1;
			obj->error_macro_def_loc = macro.def_loc;
			obj->error_macro_name = macro.name;
			obj->error_macro_n_args = macro.args.length;
			obj->handler(obj);
			return NULL;
		}
		sbufshift(prev_ctx->buffer, 1);
		++prev_ctx->cur_loc.colno;
	} else if (macro.args.length > 0) {
		obj->error_code = BRP_ERR_MACRO_ARG_COUNT_MISMATCH;
		obj->error_loc = prev_ctx->cur_loc;
		obj->error_n_args = 0;
		obj->error_macro_def_loc = macro.def_loc;
		obj->error_macro_name = macro.name;
		obj->error_macro_n_args = macro.args.length;
		obj->handler(obj);
		return NULL;
	}

	preprocessInput(obj, input);
	return prev_ctx;
}

static void preprocessInput(BRP* obj, BRP_InputCtx* const input)
{
// stripping unwanted symbols from the start of the buffer
	cleanInput(obj, input);
// processing directives
	while (sbufcutc(&input->buffer, fromcstr("#"))) {
		input->cur_loc.colno += sbufstripl(&input->buffer, SPACE, TAB).length + 1;
		BRPDirectiveCode directive_id = sbufcutv(&input->buffer, brp_directives);

		static_assert(N_BRP_CMDS == 9, "not all BRP directives are handled in preprocessInput");
		switch (directive_id) {
			case BRP_COMMENT: {
				sbuf stub;
				sbufsplitv(&input->buffer, &stub, nl_arg);
				input->cur_loc.lineno++;
				input->cur_loc.colno = 1;
				break;
			}
			case BRP_INCLUDE: {
				input->cur_loc.colno += brp_directives[BRP_INCLUDE].length + sbufstripl(&input->buffer, SPACE, TAB).length;
// TODO: make BRP distinguish between `#include "..."` and `#include <...>`
				char path_start = sbufcutc(&input->buffer, fromcstr("<\""));
				if (!path_start) {
					obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return;
				}
				sbuf path_spec;
				sbufsplit(&input->buffer, &path_spec, path_start == '"' ? DQUOTE : fromcstr(">"));
				input->cur_loc.colno += path_spec.length + 2;
				char path_spec_c[path_spec.length + 1];
				memcpy(path_spec_c, path_spec.data, path_spec.length);
				path_spec_c[path_spec.length] = '\0';

				for (BRP_InputCtx* ctx = input; ctx; ctx = ctx->prev) {
					if (pathEquals(path_spec_c, ctx->cur_loc.src_name)) path_spec.length = 0;
				}

				if (path_spec.length) {
					BRP_appendInput(
						obj,
						input,
						fopen(path_spec_c, "r"),
						(BRP_TokenLoc){ .src_name = tostr(path_spec), .lineno = 1, .colno = 1, .included_from = NULL },
						input->cur_loc
					);
					cleanInput(obj, input->prev);
					preprocessInput(obj, input);
				}
				break;
			}
			case BRP_MACRO:
			case BRP_DEFINE: {
				input->cur_loc.colno += brp_directives[directive_id].length + sbufstriplc(&input->buffer, fromcstr(" \t")).length;
				BRP_Macro macro = {0};
				sbuf macro_name;
				sbuf delim = sbufsplit(&input->buffer, &macro_name, SPACE, TAB, NEWLINE, fromcstr("("));
				
				input->cur_loc.colno += delim.length + macro_name.length;
				macro.name = tostr(macro_name);
// parsing macro arguments declaration
				if (sbufeq(delim, fromcstr("("))) {
					while (!sbufcutc(&input->buffer, fromcstr(")"))) {
						input->cur_loc.colno += sbufstriplc(&input->buffer, fromcstr(" \t")).length;
						sbuf arg_name;
						BRP_MacroArg arg = { .def_loc = input->cur_loc };

						sbuf arg_name_delim = sbufsplit(&input->buffer, &arg_name, SPACE, TAB, NEWLINE, fromcstr(","), fromcstr(")"));
						input->cur_loc.colno += arg_name.length + arg_name_delim.length;
						arg.name = tostr(arg_name);
						if (sbufeq(arg_name_delim, NEWLINE)) {
							obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
							obj->error_loc = input->cur_loc;
							obj->handler(obj);
						} else if (sbufeq(arg_name_delim, fromcstr(")"))) {
							sbufshift(input->buffer, -1);
						} else if (!sbufeq(arg_name_delim, fromcstr(","))) {
							sbuf leftovers;
							sbuf comma_or_bracket = sbufsplit(&input->buffer, &leftovers, NEWLINE, fromcstr(","), fromcstr(")"));
							if (!sbufspace(leftovers)) {
								obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
								obj->error_loc = input->cur_loc;
								obj->handler(obj);
								return;
							}

							input->cur_loc.colno += leftovers.length + comma_or_bracket.length;
							if (sbufeq(comma_or_bracket, fromcstr(")"))) {
								sbufshift(input->buffer, -1);
							} else if (!sbufeq(comma_or_bracket, fromcstr(","))) {
								obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
								obj->error_loc = input->cur_loc;
								obj->handler(obj);
								return;
							}
						}
						BRP_MacroArgArray_append(&macro.args, arg);
					}
				}
				macro.def_loc = input->cur_loc;
// parsing macro definition
				if (directive_id == BRP_DEFINE) {
					if (!sbufeq(delim, NEWLINE)) {
						if (sbufsplitescv(&input->buffer, &macro.def, nl_arg) >= 0) input->cur_loc.lineno++;
						macro.def_loc.colno += sbufstriplv(&macro.def, obj->hidden_symbols).length;
						sbufstriprv(&macro.def, obj->hidden_symbols);

						sbuf macro_def = macro.def;
						sbufsub(macro_def, &macro.def, fromcstr("\\\n"), NEWLINE);
					}
					input->cur_loc.colno = 1;
					input->cur_loc.lineno += sbufcount_v(macro.def, nl_arg);
				} else {
					if (!sbufsplit(&input->buffer, &macro.def, fromcstr("#endmacro")).data) {
						obj->error_code = BRP_ERR_UNCLOSED_MACRO;
						obj->error_loc = input->cur_loc;
						obj->handler(obj);
						free(macro.name);
						return;
					}
					macro.def = sbufcopy(macro.def);
					input->cur_loc.colno = brp_directives[BRP_ENDMACRO].length + 2;
					input->cur_loc.lineno += sbufcount_v(macro.def, nl_arg);
				}

				arrayForeach(BRP_Macro, prev_macro, obj->macros) {
					if (sbufeq(fromstr(macro.name), fromstr(prev_macro->name))) {
						free(macro.name);
						macro.name = NULL;
						free(prev_macro->def.data);
						prev_macro->def = macro.def;
					}
				}
				if (macro.name) BRP_MacroArray_append(&obj->macros, macro);
				break;
			}
			case BRP_IFNDEF:
			case BRP_IFDEF: {
				input->cur_loc.colno += brp_directives[directive_id].length + sbufstripl(&input->buffer, SPACE, TAB).length;
				sbuf macro_name;
				sbufsplitv(&input->buffer, &macro_name, nl_arg);
				sbufstripl(&macro_name, SPACE, TAB);
				if (!macro_name.length) {
					obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return;
				}

				bool defined = false;
				arrayForeach (BRP_Macro, macro, obj->macros) {
					if (sbufeq(macro_name, fromstr(macro->name))) {
						defined = true;
						break;
					}
				}
				if (directive_id == BRP_IFNDEF) defined = !defined;

				if (defined) {
					BRP_TokenLocArray_append(&obj->conditional_blocks, input->cur_loc);
					input->cur_loc.lineno++;
					input->cur_loc.colno = 1;
				} else {
					BRP_TokenLoc cond_loc = input->cur_loc;
					input->cur_loc.lineno++;
					input->cur_loc.colno = 1;
					int block_level = 0;
					while (true) {
						sbuf line;
						if (sbufsplitv(&input->buffer, &line, nl_arg) < 0) {
							obj->error_code = BRP_ERR_UNCLOSED_CONDITION;
							obj->error_loc = cond_loc;
							obj->handler(obj);
							return;
						}

						input->cur_loc.lineno++;
						sbufshift(line, 1);
						if (sbufstartswith(line, brp_directives[BRP_IFDEF])) {
							block_level++;
						} else if (sbufstartswith(line, brp_directives[BRP_ENDIF])) {
							if (block_level == 0) break;
							block_level--;
						} else if (!block_level ? sbufstartswith(line, brp_directives[BRP_ELSE]) : false) {
							BRP_TokenLocArray_append(&obj->conditional_blocks, input->cur_loc);
							break;
						}
					}
				}
				break;
			}
			case BRP_ENDIF: {
				if (obj->conditional_blocks.length == 0) {
					obj->error_code = BRP_ERR_EXCESS_ENDIF;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return;
				}
				BRP_TokenLocArray_pop(&obj->conditional_blocks, -1);
				sbuf stub;
				sbufsplitv(&input->buffer, &stub, nl_arg);
				input->cur_loc.lineno++;
				break;
			}
			case BRP_ENDMACRO:
				obj->error_code = BRP_ERR_EXCESS_ENDMACRO;
				obj->error_loc = input->cur_loc;
				obj->handler(obj);
				return;
			case BRP_ELSE:
				if (obj->conditional_blocks.length == 0) {
					obj->error_code = BRP_ERR_EXCESS_ELSE;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return;
				}
				BRP_TokenLocArray_pop(&obj->conditional_blocks, -1);
				BRP_TokenLoc cond_loc = input->cur_loc;
				input->cur_loc.colno = 1;
				int block_level = 0;
				while (true) {
					sbuf line;
					if (sbufsplitv(&input->buffer, &line, nl_arg) < 0) {
						obj->error_code = BRP_ERR_UNCLOSED_CONDITION;
						obj->error_loc = cond_loc;
						obj->handler(obj);
						return;
					}

					input->cur_loc.lineno++;
					sbufshift(line, 1);
					if (sbufstartswith(line, brp_directives[BRP_IFDEF])) {
						block_level++;
					} else if (sbufstartswith(line, brp_directives[BRP_ENDIF])) {
						if (block_level == 0) break;
						block_level--;
					} else if (!block_level ? sbufstartswith(line, brp_directives[BRP_ELSE]) : false) {
						obj->error_code = BRP_ERR_EXCESS_ELSE;
						obj->error_loc = input->cur_loc;
						obj->handler(obj);
						return;
					}
				}
				break;
			case N_BRP_CMDS:
			default:
				obj->error_code = BRP_ERR_UNKNOWN_CMD;
				obj->error_loc = input->cur_loc;
				sbufsplitv(&input->buffer, &obj->error_symbol, nl_arg);
				obj->handler(obj);
				return;
		}
		cleanInput(obj, input);
	}
// replacing a macro name with its definition if one is found
	sbuf macro_name, buffer_view = input->buffer;
	sbufsplitv(&buffer_view, &macro_name, obj->symbols);
	
	BRP_InputCtx* prev_ctx = NULL;
	arrayForeach (BRP_Macro, macro, input->locals) {
		if (sbufeq(macro_name, fromstr(macro->name))) {
			prev_ctx = expandMacro(obj, input, *macro);
			break;
		}
	}
	if (!prev_ctx) {
		arrayForeach(BRP_Macro, macro, obj->macros) {
			if (sbufeq(macro_name, fromstr(macro->name))) {
				prev_ctx = expandMacro(obj, input, *macro);
				break;
			}
		}
	}
	if (!input->buffer.length) delInput(obj, input);
}

BRP_Token BRP__fetchToken(BRP* const obj, BRP_InputCtx* const input, BRP_TokenArray* const queue)
{
	BRP_Token res = BRP_TokenArray_pop(queue, 0);
	static_assert(N_TOKEN_TYPES == 6, "not all token types are handled");
	switch (res.type) {
		case TOKEN_NONE: break;
		case TOKEN_SYMBOL: if (BRP_isSymbolSpecHidden(obj->symbols[res.symbol_id])) break;
			return res;
		case TOKEN_WORD: if (sbufspace(fromstr(res.word))) break;
		case TOKEN_KEYWORD:
		case TOKEN_STRING:
		case TOKEN_INT:
			return res;
	}

	preprocessInput(obj, input);
	if (!input->buffer.length) return (BRP_Token){ .type = TOKEN_NONE };

	res.type = TOKEN_WORD;
	sbuf new;
	int delim_id;
	delim_id = sbufsplitv(&input->buffer, &new, obj->symbols);

	if (new.length) {
		for (uint32_t i = 0; obj->keywords[i].data; i++) {
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
		if (res.type == TOKEN_WORD) res.word = tostr(new);

		res.loc = input->cur_loc;
		input->cur_loc.colno += new.length;

		if (delim_id >= 0) {
			if (delim_id == obj->_dquote_symbol_id) {
				sbuf str_literal;
				sbuf delim = sbufsplitesc(&input->buffer, &str_literal, DQUOTE, NEWLINE);
				if (sbufeq(delim, NEWLINE)) {
					obj->error_code = BRP_ERR_UNCLOSED_STR;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return (BRP_Token){0};
				}

				sbuf str_literal_res = str_literal;
				if (obj->flags & BRP_ESC_STR_LITERALS) str_literal_res = sbufunesc(str_literal, NULL);

				BRP_TokenArray_append(queue, (BRP_Token){
					.type = TOKEN_STRING,
					.loc = input->cur_loc,
					.string = str_literal_res
				});
				input->cur_loc.colno += sbufutf8len(str_literal) + DQUOTE.length;
			} else if (delim_id == obj->_quote_symbol_id) {
				sbuf char_literal;
				sbuf delim = sbufsplitesc(&input->buffer, &char_literal, QUOTE, NEWLINE);
				if (sbufeq(delim, NEWLINE)) {
					obj->error_code = BRP_ERR_UNCLOSED_CHAR;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return (BRP_Token){0};
				}
				char unesc_buffer[sizeof(res.value)] = {0};
				sbuf unesc = { .data = unesc_buffer, .length = sizeof(unesc_buffer) };
				sbufunesc(char_literal, &unesc);
				if (IS_BIG_ENDIAN) reverseByteOrder(unesc.data, unesc.length);

				BRP_TokenArray_append(queue, (BRP_Token){
					.type = TOKEN_INT,
					.loc = input->cur_loc,
					.value = *(int64_t*)unesc_buffer
				});
				input->cur_loc.colno += sbufutf8len(char_literal) + QUOTE.length;
			} else if (!BRP_isSymbolSpecHidden(obj->symbols[delim_id])) {
				BRP_TokenArray_append(queue, (BRP_Token){
					.type = TOKEN_SYMBOL,
					.loc = input->cur_loc,
					.symbol_id = delim_id
				});
			} 
		}
	} else if (delim_id >= 0) {
		if (delim_id == obj->_dquote_symbol_id) {
			sbuf str_literal;
			sbuf delim = sbufsplitesc(&input->buffer, &str_literal, DQUOTE, NEWLINE);
			if (sbufeq(delim, NEWLINE)) {
				obj->error_code = BRP_ERR_UNCLOSED_STR;
				obj->error_loc = input->cur_loc;
				obj->handler(obj);
				return (BRP_Token){0};
			}

			sbuf str_literal_res = str_literal;
			if (obj->flags & BRP_ESC_STR_LITERALS) str_literal_res = sbufunesc(str_literal, NULL);


			res.type = TOKEN_STRING;
			res.loc = input->cur_loc;
			res.string = str_literal_res;
			input->cur_loc.colno += sbufutf8len(str_literal) + DQUOTE.length;
		} else if (delim_id == obj->_quote_symbol_id) {
			sbuf char_literal;
			sbuf delim = sbufsplitesc(&input->buffer, &char_literal, QUOTE, NEWLINE);
			if (sbufeq(delim, NEWLINE)) {
				obj->error_code = BRP_ERR_UNCLOSED_CHAR;
				obj->error_loc = input->cur_loc;
				obj->handler(obj);
				return (BRP_Token){0};
			}
			char unesc_buffer[sizeof(res.value)] = {0};
			sbuf unesc = { .data = unesc_buffer, .length = sizeof(unesc_buffer) };
			sbufunesc(char_literal, &unesc);
			if (IS_BIG_ENDIAN) reverseByteOrder(unesc.data, unesc.length);

			res.type = TOKEN_INT;
			res.loc = input->cur_loc;
			res.value = *(int64_t*)unesc_buffer;
			input->cur_loc.colno += sbufutf8len(char_literal) + QUOTE.length;
		} else {
			res.type = TOKEN_SYMBOL;
			res.symbol_id = delim_id;
			res.loc = input->cur_loc;
		}
	} else return (BRP_Token){ .type = TOKEN_NONE, .loc = input->cur_loc };

	if (delim_id >= 0) {
		if (obj->symbols[delim_id].data[0] == '\n') {
			input->cur_loc.lineno++;
			input->cur_loc.colno = 1;
		} else {
			input->cur_loc.colno += obj->symbols[delim_id].length;
		}
	}

	return res;
}

BRP_Token BRP_peekToken(BRP* obj)
{
	if (obj->pending.length) {
		return obj->pending.data[0];
	} else {
		BRP_Token res = BRP_fetchToken(obj);
		return *BRP_TokenArray_prepend(&obj->pending, res);
	}
}

bool BRP_unfetchToken(BRP* obj, BRP_Token token)
{
	return BRP_TokenArray_prepend(&obj->pending, token) != NULL;
}

void BRP_fprintTokenLoc(FILE* fd, BRP_TokenLoc loc)
{
	if (loc.src_name) {
		if (loc.included_from) {
			BRP_fprintTokenLoc(fd, *loc.included_from);
			fputs(":\n", fd);
		}
		fprintf(fd, "%s:%d:%d", loc.src_name, loc.lineno, loc.colno);
	}
}

void BRP_fprintTokenStr(FILE* fd, BRP_Token token, BRP* obj)
{
	switch (token.type) {
		case TOKEN_WORD: fprintf(fd, "word `%s`", token.word); break;
		case TOKEN_INT: fprintf(fd, "integer %lld", token.value); break;
		case TOKEN_STRING:
			fprintf(fd, "string \"");
			fputsbufesc(fd, token.string, BYTEFMT_HEX);
			fputc('"', fd);
			break;
		case TOKEN_KEYWORD: fprintf(fd, "keyword `"sbuf_format"`", unpack(obj->keywords[token.keyword_id])); break;
		case TOKEN_SYMBOL: fprintf(fd, "symbol `"sbuf_format"`", unpack(obj->symbols[token.symbol_id])); break;
		default: fprintf(fd, "nothing");
	}
}

void BRP_fprintToken(FILE* fd, BRP_Token token, BRP* obj)
{
	BRP_fprintTokenLoc(fd, token.loc);
	fputs(": ", fd);
	BRP_fprintTokenStr(fd, token, obj);
	fputc('\n', fd);
}

void BRP_fprintTokenText(FILE* fd, BRP_Token token, BRP* obj)
{
	switch (token.type) {
		case TOKEN_NONE:
			fputc('\0', fd);
			break;
		case TOKEN_WORD:
			fputs(token.word, fd);
			break;
		case TOKEN_STRING:
			fputc('"', fd);
			fputsbufesc(fd, token.string, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE);
			fputc('"', fd);
			break;
		case TOKEN_KEYWORD:
			fputsbuf(fd, obj->keywords[token.keyword_id]);
			break;
		case TOKEN_SYMBOL:
			fputsbuf(fd, obj->symbols[token.symbol_id]);
			break;
		case TOKEN_INT:
			fprintf(fd, "%lld", token.value);
			break;
	}
}

int BRP_getTokenSymbolId(BRP_Token token)
{
	return token.type == TOKEN_SYMBOL ? token.symbol_id : -1;
}

int BRP_getTokenKeywordId(BRP_Token token)
{
	return token.type == TOKEN_KEYWORD ? token.keyword_id : -1;
}

char* TokenTypeNames[N_TOKEN_TYPES] = {
	"nothing",
	"word",
	"keyword",
	"symbol",
	"integer",
	"string"
};

char* BRP_getTokenTypeName(BRP_TokenType type)
{
	return TokenTypeNames[type];
}

char* BRP_getTokenWord(BRP* obj, BRP_Token token)
{
	switch (token.type) {
		case TOKEN_WORD:
			return token.word;
		case TOKEN_KEYWORD:
			return obj->keywords[token.keyword_id].data;
		case TOKEN_SYMBOL:
			return obj->symbols[token.symbol_id].data;
		default: return NULL;
	}
}

void BRP_printBRPErrorStr(FILE* fd, BRP* obj) {
	static_assert(N_BRP_ERRORS == 14, "not all BRP errors are handled in BRP_printBRPErrorStr");
	switch (obj->error_code) {
		case BRP_ERR_OK: break;
		case BRP_ERR_UNCLOSED_STR: 
			fprintf(fd, "unclosed string literal ");
			break;
		case BRP_ERR_NO_MEMORY: 
			fprintf(fd, "memory allocation failure (reason: %s) ", strerror(errno));
			break;
		case BRP_ERR_FILE_NOT_FOUND:
			fprintf(fd, "could not open file `%.*s` (reason: %s) ", unpack(obj->error_symbol), strerror(errno));
			break;
		case BRP_ERR_SYMBOL_CONFLICT:
			fprintf(
				fd,
				"symbol conflict: symbol `%.*s` will be incorrectly parsed; to solve this, place it before the symbol that has a common start ",
				unpack(obj->error_symbol)
			);
			break;
		case BRP_ERR_INVALID_CMD_SYNTAX:
			fprintf(fd, "invalid BRP directive syntax ");
			break;
		case BRP_ERR_UNKNOWN_CMD:
			fprintf(fd, "unknown BRP directive `%.*s` ", unpack(obj->error_symbol));
			break;
		case BRP_ERR_UNCLOSED_MACRO:
			fprintf(fd, "macros defined with `#macro` must be closed with the `#endmacro` directive ");
			break;
		case BRP_ERR_EXCESS_ENDIF:
			fprintf(fd, "excess `#endif` directive ");
			break;
		case BRP_ERR_EXCESS_ENDMACRO:
			fprintf(fd, "excess `#endmacro` directive ");
			break;
		case BRP_ERR_UNCLOSED_CONDITION:
			fprintf(fd, "unclosed conditional block; to close it, use the `#endif` dirrective ");
			break;
		case BRP_ERR_EXCESS_ELSE:
			fprintf(fd, "excess `#else` directive ");
			break;
		case BRP_ERR_UNCLOSED_CHAR:
			fprintf(fd, "unclosed character literal ");
			break;
		case BRP_ERR_MACRO_ARG_COUNT_MISMATCH:
			fprintf(fd, "macro `%s`, defined at ", obj->error_macro_name);
			BRP_fprintTokenLoc(fd, obj->error_macro_def_loc);
			fprintf(fd, ", expects %d arguments, instead got ", obj->error_macro_n_args);
			if (obj->error_n_args < 0) {
				fputs("more ", fd);
			} else {
				fprintf(fd, "%d ", obj->error_n_args);
			}
			break;
		case N_BRP_ERRORS:
		default:
			fprintf(fd, "unreachable");
	}
}

void BRP_printBRPError(BRP* obj)
{
	if (obj->error_code) {
		BRP_fprintTokenLoc(stderr, obj->error_loc);
		fputs(": preprocessing error: ", stderr);
		BRP_printBRPErrorStr(stderr, obj);
		fputc('\n', stderr);
		exit(1);
	}
}

BRP* BRP_initBRP(BRP* obj, BRP_ErrorHandler handler, char flags)
{
	*obj = (BRP){0};
	obj->handler = handler ? handler : BRP_printBRPError;
	obj->flags = flags;
	obj->macros = BRP_MacroArray_new(
		3,
		(BRP_Macro){.name = "__BRC__"},
		(BRP_Macro){0}, // OS indicator macro placeholder
		(BRP_Macro){0} // platform (POSIX | NT) indicator macro placeholder
	);

#if defined(__APPLE__) || defined(__MACH__)
	obj->macros.data[1] = (BRP_Macro){.name = "__APPLE__"};
	obj->macros.data[2] = (BRP_Macro){.name = "__POSIX__"};
#elif defined(_WIN32)
	obj->macros.data[1] = (BRP_Macro){.name = "_WIN32"};
	obj->macros.data[2] = (BRP_Macro){.name = "__NT__"};
#elif defined(__linux__)
	obj->macros.data[1] = (BRP_Macro){.name = "__linux__"};
	obj->macros.data[2] = (BRP_Macro){.name = "__POSIX__"};
#endif

	return obj;
}

void BRP_delBRP(BRP* obj)
{
	BRP_TokenArray_clear(&obj->pending);
	BRP_MacroArray_clear(&obj->macros);
	BRP_TokenLocArray_clear(&obj->conditional_blocks);
	arena_free(&obj->arena);
	while (obj->cur_input.prev) delInput(obj, &obj->cur_input);
}

#endif // _BRP_
