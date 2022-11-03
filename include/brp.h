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
	BRP_TOKEN_NONE,
	BRP_TOKEN_WORD,
	BRP_TOKEN_KEYWORD,
	BRP_TOKEN_SYMBOL,
	BRP_TOKEN_INT,
	BRP_TOKEN_STRING,
	BRP_N_TOKEN_TYPES
} BRP_TokenType;

typedef struct BRP_TokenLoc BRP_TokenLoc;
struct BRP_TokenLoc {
	uint32_t lineno;
	uint32_t colno;
	const char* src_name;
	BRP_TokenLoc* included_from;
};
declArray(BRP_TokenLoc);

typedef struct {
	int8_t type;
	BRP_TokenLoc loc;
	union {
		char* word; // for BRP_TOKEN_WORD
		sbuf string; // for BRP_TOKEN_STRING
		int64_t value; // for BRP_TOKEN_INT
		uint32_t keyword_id; // for BRP_TOKEN_KEYWORD
		uint32_t symbol_id; // for BRP_TOKEN_SYMBOL
	};
} BRP_Token;
#define BRP_emptyToken ((BRP_Token){ .type = BRP_TOKEN_NONE })
#define BRP_wordToken(_word) ((BRP_Token){ .type = BRP_TOKEN_WORD, .word = _word })
#define BRP_keywordToken(kw_id) ((BRP_Token){ .type = BRP_TOKEN_KEYWORD, .keyword_id = kw_id })
#define BRP_symbolToken(_symbol_id) ((BRP_Token){ .type = BRP_TOKEN_SYMBOL, .symbol_id = _symbol_id })
#define BRP_intToken(_int) ((BRP_Token){ .type = BRP_TOKEN_INT, .value = _int })
#define BRP_stringToken(_word) ((BRP_Token){ .type = BRP_TOKEN_STRING, .word = _word })

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
#define BRP_KEYWORD(spec) sbuf_fromcstr(spec)
#define BRP_SYMBOL(spec) sbuf_fromcstr(spec)
// hidden symbols are delimiters that are not returned as generated tokens
#define BRP_HIDDEN_SYMBOL(spec) ((sbuf){ .data = spec"\n", .length = sizeof(spec) - 1 })

#define BRP_isEmpty(prep) ((prep)->cur_input.buffer.length + (size_t)(prep)->cur_input.prev == 0)
#define BRP_isWordToken(token) ( (token).type == BRP_TOKEN_WORD || (token).type == BRP_TOKEN_KEYWORD )
#define BRP_isSymbolSpecHidden(spec) ((spec).data[(spec).length] > 0)

typedef void (*BRPErrorHandler) (BRP*);

// flags for BRP_initBRP function
#define BRP_ESC_STR_LITERALS 0x1
BRP*      BRP_initBRP(BRP* obj, BRPErrorHandler handler, char flags);
void      BRP_delBRP(BRP* obj);
bool      BRP_setKeywords(BRP* obj, sbuf kws[]);
bool      BRP_setSymbols(BRP* obj, sbuf symbols[]);
bool      BRP_appendInput(BRP* obj, BRP_InputCtx* input, FILE* input_fd, BRP_TokenLoc initial_loc, BRP_TokenLoc include_loc);
#define   BRP_setInput(obj, path, input_fd) BRP_appendInput(obj, &(obj)->cur_input, input_fd, (BRP_TokenLoc){ .src_name = path, .colno = 1, .lineno = 1, .included_from = NULL }, (BRP_TokenLoc){0})
BRP_Token BRP__fetchToken(BRP* obj, BRP_InputCtx* input, BRP_TokenArray* queue);
#define   BRP_fetchToken(obj) BRP__fetchToken(obj, &(obj)->cur_input, &(obj)->pending)
BRP_Token BRP_peekToken(BRP* obj);
bool      BRP_unfetchToken(BRP* obj, BRP_Token token);
void      BRP_fprintTokenLoc(FILE* fd, BRP_TokenLoc loc);
#define   BRP_printTokenLoc(loc) BRP_fprintTokenLoc(stdout, loc)
void      BRP_fprintTokenStr(FILE* fd, BRP_Token token, BRP* obj);
#define   BRP_printTokenStr(token, parser) BRP_fprintTokenStr(stdout, token, parser)
void      BRP_fprintToken(FILE* fd, BRP_Token token, BRP* obj);
#define   BRP_printToken(token, parser) BRP_fprintToken(stdout, token, parser)
void      BRP_fprintTokenText(FILE* fd, BRP_Token token, BRP* obj);
#define   BRP_printTokenText(token, parser) BRP_fprintTokenText(stdout, token, parser)
int       BRP_getTokenSymbolId(BRP_Token token);
int       BRP_getTokenKeywordId(BRP_Token token);
char*     BRP_getTokenTypeName(BRP_TokenType type);
char*     BRP_getTokenWord(BRP* obj, BRP_Token token);
void      BRP_printErrorMsg(FILE* fd, BRP* obj);
void      BRP_defaultErrorHandler(BRP* obj);

#endif // _BRP_

#if defined(BRP_IMPLEMENTATION) && !defined(_BRP_IMPL_LOCK)
#define _BRP_IMPL_LOCK

#include <sys/param.h>
#define BR_UTILS_IMPLEMENTATION
#include <br_utils.h>
#define SBUF_IMPLEMENTATION
#include <sbuf.h>

implArray(BRP_TokenLoc);
implArray(BRP_Macro);
implArray(sbuf);
implArray(BRP_Token);
implArray(BRP_MacroArg);

static bool pathEquals(const char path1[], const char path2[])
{
	char path1_r[MAXPATHLEN];
	char path2_r[MAXPATHLEN];
	realpath(path1, path1_r);
	realpath(path2, path2_r);
	return str_eq(path1_r, path2_r);
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
	obj->symbols[n_symbols - 1] = SBUF_DQUOTE;
	obj->_quote_symbol_id = n_symbols;
	obj->symbols[n_symbols] = SBUF_QUOTE;
	obj->symbols[n_symbols + 1] = obj->hidden_symbols[n_hidden_symbols] = (sbuf){0};

	for (int i = 0; i < n_symbols; i++) {
		for (int i1 = 0; i1 < i; i1++) {
			if (sbuf_startswith(obj->symbols[i], obj->symbols[i1])) {
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
		sbuf_fromcstr("\""),
		sbuf_fromcstr("'"),
		sbuf_fromcstr("//"),
		sbuf_fromcstr("/*"),
		(sbuf){0}
	};
	sbuf res = sbuf_fromcstr("");
// TODO: make the preprocessor aware of comments for them not to screw up the tokens' source locations
	while (buffer.length) {
		sbuf part, other_part;
		switch ((sbuf_split_v(&buffer, &part, comment_delims))) {
			case 0: // double quote
				sbuf_splitesc(&buffer, &other_part, comment_delims[0]);
				res = sbuf_concat(res, part, comment_delims[0], other_part, comment_delims[0]);
				break;
			case 1: // single quote
				sbuf_splitesc(&buffer, &other_part, comment_delims[1]);
				res = sbuf_concat(res, part, comment_delims[1], other_part, comment_delims[1]);
				break;
			case 2: // line-terminated comment (like the one this text is in)
				sbuf_split(&buffer, &other_part, SBUF_NEWLINE);
				res = sbuf_concat(res, part);
				break;
			case 3: /* free-form comment (like the one this text is in) */
				sbuf_split(&buffer, &other_part, sbuf_fromcstr("*/"));
				res = sbuf_concat(res, part);
				break;
			default:
				res = sbuf_concat(res, part);
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
		obj->error_symbol = sbuf_fromstr((char*)initial_loc.src_name);
		obj->handler(obj);
		return false;
	}

	BRP_InputCtx* prev_ctx = arena_alloc(&obj->arena, sizeof(BRP_InputCtx));
	*prev_ctx = *input;
	input->cur_loc = initial_loc;
	input->buffer = removeComments(sbuf_fromfile(input_fd));
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

	sbuf_rstrip_v(&input->buffer, obj->hidden_symbols);
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

static sbuf nl_arg[2] = { SBUF_NEWLINE };

static void cleanInput(BRP* obj, BRP_InputCtx* const input)
{
	while (!input->buffer.length) {
		if (!input->prev) return;
		delInput(obj, input);
	}

	sbuf stripped = sbuf_lstrip_v(&input->buffer, obj->hidden_symbols);
	int nl_count = sbuf_count_v(stripped, nl_arg);
	if (nl_count) {
		input->cur_loc.lineno += nl_count;
		sbuf last_stripped_line;
		sbuf_rsplit_v(&stripped, &last_stripped_line, nl_arg);
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
	BRP_N_DIRS
} BRP_DirectiveType;

static const sbuf brp_directives[BRP_N_DIRS + 1] = {
	[BRP_INCLUDE] = sbuf_fromcstr("include"),
	[BRP_DEFINE] = sbuf_fromcstr("define"),
	[BRP_MACRO] = sbuf_fromcstr("macro"),
	[BRP_COMMENT] = sbuf_fromcstr("!"),
	[BRP_IFDEF] = sbuf_fromcstr("ifdef"),
	[BRP_ENDIF] = sbuf_fromcstr("endif"),
	[BRP_ENDMACRO] = sbuf_fromcstr("endmacro"),
	[BRP_IFNDEF] = sbuf_fromcstr("ifndef"),
	[BRP_ELSE] = sbuf_fromcstr("else")
};

static char* oppositeBrackets[UINT8_MAX] = {
	['('] = ")", [')'] = "(",
	['['] = "]", [']'] = "[",
	['{'] = "}", ['}'] = "{"
};

static inline BRP_Macro fetchMacroArg(BRP* const obj, BRP_InputCtx* const input, const BRP_MacroArg arg)
{
	BRP_Macro res = { .name = arg.name, .def_loc = arg.def_loc };
	bool isnt_last_arg = true;
	while (true) {
		sbuf delim = sbuf_split(&input->buffer, &res.def, sbuf_fromcstr(","), sbuf_fromcstr("("), sbuf_fromcstr("["), sbuf_fromcstr("{"), sbuf_fromcstr(")"));
		if (delim.length ? sbuf_eq(delim, SBUF_NEWLINE) : true) {
			obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
			obj->error_loc = input->cur_loc;
			obj->error_loc.colno += res.def.length;
			obj->handler(obj);
			return (BRP_Macro){0};
		}

		if (delim.data[0] == ',') break;
		if (delim.data[0] == ')') {
			sbuf_shift(input->buffer, -1);
			isnt_last_arg = false;
			break;
		}
		sbuf also_def;
		delim = sbuf_split(&input->buffer, &also_def, sbuf_fromstr(oppositeBrackets[(unsigned)delim.data[0]]), SBUF_NEWLINE);
		if (!delim.data ? sbuf_eq(delim, SBUF_NEWLINE) : false) {
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
	sbuf_shift(prev_ctx->buffer, name_length);
	prev_ctx->cur_loc.colno += name_length;

	if (sbuf_cut_c(&prev_ctx->buffer, sbuf_fromcstr("("))) {
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
		sbuf_shift(prev_ctx->buffer, 1);
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
	while (sbuf_cut_c(&input->buffer, sbuf_fromcstr("#"))) {
		input->cur_loc.colno += sbuf_lstrip(&input->buffer, SBUF_SPACE, SBUF_TAB).length + 1;
		BRP_DirectiveType directive_id = sbuf_cut_v(&input->buffer, brp_directives);

		static_assert(BRP_N_DIRS == 9, "not all BRP directives are handled in preprocessInput");
		switch (directive_id) {
			case BRP_COMMENT: {
				sbuf stub;
				sbuf_split_v(&input->buffer, &stub, nl_arg);
				input->cur_loc.lineno++;
				input->cur_loc.colno = 1;
				break;
			}
			case BRP_INCLUDE: {
				input->cur_loc.colno += brp_directives[BRP_INCLUDE].length + sbuf_lstrip(&input->buffer, SBUF_SPACE, SBUF_TAB).length;
// TODO: make BRP distinguish between `#include "..."` and `#include <...>`
				char path_start = sbuf_cut_c(&input->buffer, sbuf_fromcstr("<\""));
				if (!path_start) {
					obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return;
				}
				sbuf path_spec;
				sbuf_split(&input->buffer, &path_spec, path_start == '"' ? SBUF_DQUOTE : sbuf_fromcstr(">"));
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
						(BRP_TokenLoc){ .src_name = sbuf_tostr(path_spec), .lineno = 1, .colno = 1, .included_from = NULL },
						input->cur_loc
					);
					cleanInput(obj, input->prev);
					preprocessInput(obj, input);
				}
				break;
			}
			case BRP_MACRO:
			case BRP_DEFINE: {
				input->cur_loc.colno += brp_directives[directive_id].length + sbuf_lstrip_c(&input->buffer, sbuf_fromcstr(" \t")).length;
				BRP_Macro macro = {0};
				sbuf macro_name;
				sbuf delim = sbuf_split(&input->buffer, &macro_name, SBUF_SPACE, SBUF_TAB, SBUF_NEWLINE, sbuf_fromcstr("("));
				
				input->cur_loc.colno += delim.length + macro_name.length;
				macro.name = sbuf_tostr(macro_name);
// parsing macro arguments declaration
				if (sbuf_eq(delim, sbuf_fromcstr("("))) {
					while (!sbuf_cut_c(&input->buffer, sbuf_fromcstr(")"))) {
						input->cur_loc.colno += sbuf_lstrip_c(&input->buffer, sbuf_fromcstr(" \t")).length;
						sbuf arg_name;
						BRP_MacroArg arg = { .def_loc = input->cur_loc };

						sbuf arg_name_delim = sbuf_split(&input->buffer, &arg_name, SBUF_SPACE, SBUF_TAB, SBUF_NEWLINE, sbuf_fromcstr(","), sbuf_fromcstr(")"));
						input->cur_loc.colno += arg_name.length + arg_name_delim.length;
						arg.name = sbuf_tostr(arg_name);
						if (sbuf_eq(arg_name_delim, SBUF_NEWLINE)) {
							obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
							obj->error_loc = input->cur_loc;
							obj->handler(obj);
						} else if (sbuf_eq(arg_name_delim, sbuf_fromcstr(")"))) {
							sbuf_shift(input->buffer, -1);
						} else if (!sbuf_eq(arg_name_delim, sbuf_fromcstr(","))) {
							sbuf leftovers;
							sbuf comma_or_bracket = sbuf_split(&input->buffer, &leftovers, SBUF_NEWLINE, sbuf_fromcstr(","), sbuf_fromcstr(")"));
							if (!sbuf_space(leftovers)) {
								obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
								obj->error_loc = input->cur_loc;
								obj->handler(obj);
								return;
							}

							input->cur_loc.colno += leftovers.length + comma_or_bracket.length;
							if (sbuf_eq(comma_or_bracket, sbuf_fromcstr(")"))) {
								sbuf_shift(input->buffer, -1);
							} else if (!sbuf_eq(comma_or_bracket, sbuf_fromcstr(","))) {
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
// TODO: make `#macro`s terminate by `#.*endmacro`, not just `#endmacro`
// TODO: allow directives for conditional blocks be placed at any place in the file, not just right after a newline
// parsing macro definition
				if (directive_id == BRP_DEFINE) {
					if (!sbuf_eq(delim, SBUF_NEWLINE)) {
						if (sbuf_splitesc_v(&input->buffer, &macro.def, nl_arg) >= 0) ++input->cur_loc.lineno;
						macro.def_loc.colno += sbuf_lstrip_v(&macro.def, obj->hidden_symbols).length;
						sbuf_rstrip_v(&macro.def, obj->hidden_symbols);

						sbuf macro_def = macro.def;
						sbuf_sub(macro_def, &macro.def, sbuf_fromcstr("\\\n"), SBUF_NEWLINE);
					}
					input->cur_loc.colno = 1;
					input->cur_loc.lineno += sbuf_count_v(macro.def, nl_arg);
				} else {
					if (!sbuf_split(&input->buffer, &macro.def, sbuf_fromcstr("#endmacro")).data) {
						obj->error_code = BRP_ERR_UNCLOSED_MACRO;
						obj->error_loc = input->cur_loc;
						obj->handler(obj);
						free(macro.name);
						return;
					}
					macro.def = sbuf_copy(macro.def);
					input->cur_loc.colno = brp_directives[BRP_ENDMACRO].length + 2;
					input->cur_loc.lineno += sbuf_count_v(macro.def, nl_arg);
				}

				arrayForeach(BRP_Macro, prev_macro, obj->macros)
					if (sbuf_eq(sbuf_fromstr(macro.name), sbuf_fromstr(prev_macro->name))) {
						free(macro.name);
						macro.name = NULL;
						free(prev_macro->def.data);
						prev_macro->def = macro.def;
					}
				if (macro.name) BRP_MacroArray_append(&obj->macros, macro);
				break;
			}
			case BRP_IFNDEF:
			case BRP_IFDEF: {
				input->cur_loc.colno += brp_directives[directive_id].length + sbuf_lstrip(&input->buffer, SBUF_SPACE, SBUF_TAB).length;
				sbuf macro_name;
				sbuf_split_v(&input->buffer, &macro_name, nl_arg);
				sbuf_lstrip(&macro_name, SBUF_SPACE, SBUF_TAB);
				if (!macro_name.length) {
					obj->error_code = BRP_ERR_INVALID_CMD_SYNTAX;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return;
				}

				bool defined = false;
				arrayForeach (BRP_Macro, macro, obj->macros) {
					if (sbuf_eq(macro_name, sbuf_fromstr(macro->name))) {
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
						if (sbuf_split_v(&input->buffer, &line, nl_arg) < 0) {
							obj->error_code = BRP_ERR_UNCLOSED_CONDITION;
							obj->error_loc = cond_loc;
							obj->handler(obj);
							return;
						}

						input->cur_loc.lineno++;
						sbuf_shift(line, 1);
						if (sbuf_startswith(line, brp_directives[BRP_IFDEF])) {
							block_level++;
						} else if (sbuf_startswith(line, brp_directives[BRP_ENDIF])) {
							if (block_level == 0) break;
							block_level--;
						} else if (!block_level ? sbuf_startswith(line, brp_directives[BRP_ELSE]) : false) {
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
				sbuf_split_v(&input->buffer, &stub, nl_arg);
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
					if (sbuf_split_v(&input->buffer, &line, nl_arg) < 0) {
						obj->error_code = BRP_ERR_UNCLOSED_CONDITION;
						obj->error_loc = cond_loc;
						obj->handler(obj);
						return;
					}

					input->cur_loc.lineno++;
					sbuf_shift(line, 1);
					if (sbuf_startswith(line, brp_directives[BRP_IFDEF])) {
						block_level++;
					} else if (sbuf_startswith(line, brp_directives[BRP_ENDIF])) {
						if (block_level == 0) break;
						block_level--;
					} else if (!block_level ? sbuf_startswith(line, brp_directives[BRP_ELSE]) : false) {
						obj->error_code = BRP_ERR_EXCESS_ELSE;
						obj->error_loc = input->cur_loc;
						obj->handler(obj);
						return;
					}
				}
				break;
			case BRP_N_DIRS:
			default:
				obj->error_code = BRP_ERR_UNKNOWN_CMD;
				obj->error_loc = input->cur_loc;
				sbuf_split_v(&input->buffer, &obj->error_symbol, nl_arg);
				obj->handler(obj);
				return;
		}
		cleanInput(obj, input);
	}
// replacing a macro name with its definition if one is found
	sbuf macro_name, buffer_view = input->buffer;
	sbuf_split_v(&buffer_view, &macro_name, obj->symbols);
	
	BRP_InputCtx* prev_ctx = NULL;
	arrayForeach (BRP_Macro, macro, input->locals) {
		if (sbuf_eq(macro_name, sbuf_fromstr(macro->name))) {
			prev_ctx = expandMacro(obj, input, *macro);
			break;
		}
	}
	if (!prev_ctx) {
		arrayForeach(BRP_Macro, macro, obj->macros) {
			if (sbuf_eq(macro_name, sbuf_fromstr(macro->name))) {
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
	static_assert(BRP_N_TOKEN_TYPES == 6, "not all token types are handled");
	switch (res.type) {
		case BRP_TOKEN_NONE: break;
		case BRP_TOKEN_SYMBOL: if (BRP_isSymbolSpecHidden(obj->symbols[res.symbol_id])) break;
			return res;
		case BRP_TOKEN_WORD: if (sbuf_space(sbuf_fromstr(res.word))) break;
		case BRP_TOKEN_KEYWORD:
		case BRP_TOKEN_STRING:
		case BRP_TOKEN_INT:
			return res;
	}

	preprocessInput(obj, input);
	if (!input->buffer.length) return (BRP_Token){ .type = BRP_TOKEN_NONE };

	res.type = BRP_TOKEN_WORD;
	sbuf new;
	int delim_id;
	delim_id = sbuf_split_v(&input->buffer, &new, obj->symbols);

	if (new.length) {
		for (uint32_t i = 0; obj->keywords[i].data; i++) {
			if (sbuf_eq(obj->keywords[i], new)) {
				res.type = BRP_TOKEN_KEYWORD;
				res.keyword_id = i;
				break;
			}
		}
		if (res.type == BRP_TOKEN_WORD) {
			if (sbuf_int(new)) {
				res.type = BRP_TOKEN_INT;
				res.value = sbuf_toint(new);
			}
		}
		if (res.type == BRP_TOKEN_WORD) res.word = sbuf_tostr(new);

		res.loc = input->cur_loc;
		input->cur_loc.colno += new.length;

		if (delim_id >= 0) {
			if (delim_id == obj->_dquote_symbol_id) {
				sbuf str_literal;
				sbuf delim = sbuf_splitesc(&input->buffer, &str_literal, SBUF_DQUOTE, SBUF_NEWLINE);
				if (sbuf_eq(delim, SBUF_NEWLINE)) {
					obj->error_code = BRP_ERR_UNCLOSED_STR;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return (BRP_Token){0};
				}

				sbuf str_literal_res = str_literal;
				if (obj->flags & BRP_ESC_STR_LITERALS) str_literal_res = sbuf_unesc(str_literal, NULL);

				BRP_TokenArray_append(queue, (BRP_Token){
					.type = BRP_TOKEN_STRING,
					.loc = input->cur_loc,
					.string = str_literal_res
				});
				input->cur_loc.colno += sbuf_utf8len(str_literal) + SBUF_DQUOTE.length;
			} else if (delim_id == obj->_quote_symbol_id) {
				sbuf char_literal;
				sbuf delim = sbuf_splitesc(&input->buffer, &char_literal, SBUF_QUOTE, SBUF_NEWLINE);
				if (sbuf_eq(delim, SBUF_NEWLINE)) {
					obj->error_code = BRP_ERR_UNCLOSED_CHAR;
					obj->error_loc = input->cur_loc;
					obj->handler(obj);
					return (BRP_Token){0};
				}
				char unesc_buffer[sizeof(res.value)] = {0};
				sbuf unesc = { .data = unesc_buffer, .length = sizeof(unesc_buffer) };
				sbuf_unesc(char_literal, &unesc);
				if (IS_BIG_ENDIAN) reverseByteOrder(unesc.data, unesc.length);

				BRP_TokenArray_append(queue, (BRP_Token){
					.type = BRP_TOKEN_INT,
					.loc = input->cur_loc,
					.value = *(int64_t*)unesc_buffer
				});
				input->cur_loc.colno += sbuf_utf8len(char_literal) + SBUF_QUOTE.length;
			} else if (!BRP_isSymbolSpecHidden(obj->symbols[delim_id])) {
				BRP_TokenArray_append(queue, (BRP_Token){
					.type = BRP_TOKEN_SYMBOL,
					.loc = input->cur_loc,
					.symbol_id = delim_id
				});
			} 
		}
	} else if (delim_id >= 0) {
		if (delim_id == obj->_dquote_symbol_id) {
			sbuf str_literal;
			sbuf delim = sbuf_splitesc(&input->buffer, &str_literal, SBUF_DQUOTE, SBUF_NEWLINE);
			if (sbuf_eq(delim, SBUF_NEWLINE)) {
				obj->error_code = BRP_ERR_UNCLOSED_STR;
				obj->error_loc = input->cur_loc;
				obj->handler(obj);
				return (BRP_Token){0};
			}

			sbuf str_literal_res = str_literal;
			if (obj->flags & BRP_ESC_STR_LITERALS) str_literal_res = sbuf_unesc(str_literal, NULL);


			res.type = BRP_TOKEN_STRING;
			res.loc = input->cur_loc;
			res.string = str_literal_res;
			input->cur_loc.colno += sbuf_utf8len(str_literal) + SBUF_DQUOTE.length;
		} else if (delim_id == obj->_quote_symbol_id) {
			sbuf char_literal;
			sbuf delim = sbuf_splitesc(&input->buffer, &char_literal, SBUF_QUOTE, SBUF_NEWLINE);
			if (sbuf_eq(delim, SBUF_NEWLINE)) {
				obj->error_code = BRP_ERR_UNCLOSED_CHAR;
				obj->error_loc = input->cur_loc;
				obj->handler(obj);
				return (BRP_Token){0};
			}
			char unesc_buffer[sizeof(res.value)] = {0};
			sbuf unesc = {.data = unesc_buffer, .length = sizeof(unesc_buffer)};
			sbuf_unesc(char_literal, &unesc);
			if (IS_BIG_ENDIAN) reverseByteOrder(unesc.data, unesc.length);

			res.type = BRP_TOKEN_INT;
			res.loc = input->cur_loc;
			res.value = *(int64_t*)unesc_buffer;
			input->cur_loc.colno += sbuf_utf8len(char_literal) + SBUF_QUOTE.length;
		} else {
			res.type = BRP_TOKEN_SYMBOL;
			res.symbol_id = delim_id;
			res.loc = input->cur_loc;
		}
	} else return (BRP_Token){ .type = BRP_TOKEN_NONE, .loc = input->cur_loc };

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
		case BRP_TOKEN_WORD: fprintf(fd, "word `%s`", token.word); break;
		case BRP_TOKEN_INT: fprintf(fd, "integer %lld", token.value); break;
		case BRP_TOKEN_STRING:
			fprintf(fd, "string \"");
			sbuf_fputesc(fd, token.string, SBUF_BFMT_HEX);
			fputc('"', fd);
			break;
		case BRP_TOKEN_KEYWORD: fprintf(fd, "keyword `"sbuf_fmt"`", sbuf_unpack(obj->keywords[token.keyword_id])); break;
		case BRP_TOKEN_SYMBOL: fprintf(fd, "symbol `"sbuf_fmt"`", sbuf_unpack(obj->symbols[token.symbol_id])); break;
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
		case BRP_TOKEN_NONE:
			fputc('\0', fd);
			break;
		case BRP_TOKEN_WORD:
			fputs(token.word, fd);
			break;
		case BRP_TOKEN_STRING:
			fputc('"', fd);
			sbuf_fputesc(fd, token.string, SBUF_BFMT_HEX | SBUF_BFMT_ESC_DQUOTE);
			fputc('"', fd);
			break;
		case BRP_TOKEN_KEYWORD:
			sbuf_fput(fd, obj->keywords[token.keyword_id]);
			break;
		case BRP_TOKEN_SYMBOL:
			sbuf_fput(fd, obj->symbols[token.symbol_id]);
			break;
		case BRP_TOKEN_INT:
			fprintf(fd, "%lld", token.value);
			break;
	}
}

int BRP_getTokenSymbolId(BRP_Token token)
{
	return token.type == BRP_TOKEN_SYMBOL ? token.symbol_id : -1;
}

int BRP_getTokenKeywordId(BRP_Token token)
{
	return token.type == BRP_TOKEN_KEYWORD ? token.keyword_id : -1;
}

char* TokenTypeNames[BRP_N_TOKEN_TYPES] = {
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
		case BRP_TOKEN_WORD:
			return token.word;
		case BRP_TOKEN_KEYWORD:
			return obj->keywords[token.keyword_id].data;
		case BRP_TOKEN_SYMBOL:
			return obj->symbols[token.symbol_id].data;
		default: return NULL;
	}
}

void BRP_printErrorMsg(FILE* fd, BRP* obj)
{
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
			fprintf(fd, "could not open file `%.*s` (reason: %s) ", sbuf_unpack(obj->error_symbol), strerror(errno));
			break;
		case BRP_ERR_SYMBOL_CONFLICT:
			fprintf(
				fd,
				"symbol conflict: symbol `%.*s` will be incorrectly parsed; to solve this, place it before the symbol that has a common start ",
				sbuf_unpack(obj->error_symbol)
			);
			break;
		case BRP_ERR_INVALID_CMD_SYNTAX:
			fprintf(fd, "invalid BRP directive syntax ");
			break;
		case BRP_ERR_UNKNOWN_CMD:
			fprintf(fd, "unknown BRP directive `%.*s` ", sbuf_unpack(obj->error_symbol));
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
			unreachable();
	}
}

void BRP_defaultErrorHandler(BRP* obj)
{
	if (obj->error_code) {
		BRP_fprintTokenLoc(stderr, obj->error_loc);
		fputs(": preprocessing error: ", stderr);
		BRP_printErrorMsg(stderr, obj);
		fputc('\n', stderr);
		exit(1);
	}
}

BRP* BRP_initBRP(BRP* obj, BRP_ErrorHandler handler, char flags)
{
	*obj = (BRP){0};
	obj->handler = handler ? handler : BRP_defaultErrorHandler;
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
}

#endif // _BRP_
