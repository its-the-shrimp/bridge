#define _BRB_INTERNAL
#include <brb.h>

declArray(Var);
defArray(Var);
defArray(Op);
defArray(DataPiece);
defArray(DataBlock);
defArray(Submodule);

typedef enum {
	SYMBOL_SEGMENT_START,
	SYMBOL_SEGMENT_END,
	SYMBOL_COLON,
	N_VBRB_SYMBOLS
} VBRBSymbol;

typedef enum {
	KW_NOP,
	KW_END,
	KW_MARK,
	KW_SET,
	KW_SETR,
	KW_SETD,
	KW_SETB,
	KW_ADD,
	KW_ADDR,
	KW_SUB,
	KW_SUBR,
	KW_SYSCALL,
	KW_GOTO,
	KW_CMP,
	KW_CMPR,
	KW_AND,
	KW_ANDR,
	KW_OR,
	KW_ORR,
	KW_NOT,
	KW_XOR,
	KW_XORR,
	KW_SHL,
	KW_SHLR,
	KW_SHR,
	KW_SHRR,
	KW_SHRS,
	KW_SHRSR,
	KW_PROC,
	KW_CALL,
	KW_RET,
	KW_ENDPROC,
	KW_LD64,
	KW_STR64,
	KW_LD32,
	KW_STR32,
	KW_LD16,
	KW_STR16,
	KW_LD8,
	KW_STR8,
	KW_VAR,
	KW_SETV,
	KW_MUL,
	KW_MULR,
	KW_DIV,
	KW_DIVR,
	KW_DIVS,
	KW_DIVSR,
	KW_EXTPROC,
	KW_LDV,
	KW_STRV,
	KW_POPV,
	KW_PUSHV,
	KW_ATF,
	KW_ATL,
	KW_SETC,
	KW_DELNV,
	KW_LD64S,
	KW_LD32S,
	KW_LD16S,
	KW_LD8S,
	KW_LDVS,
	KW_SX32,
	KW_SX16,
	KW_SX8,
	KW_MOD,
	KW_MODS,
	KW_MODR,
	KW_MODSR,
	KW_ARG,
	KW_SYS_NONE,
	KW_SYS_EXIT,
	KW_SYS_WRITE,
	KW_SYS_ARGC,
	KW_SYS_ARGV,
	KW_SYS_READ,
	KW_SYS_GET_ERRNO,
	KW_SYS_SET_ERRNO,	
	KW_COND_NON,
	KW_COND_EQU,
	KW_COND_NEQ,
	KW_COND_LTU,
	KW_COND_GTU,
	KW_COND_LEU,
	KW_COND_GEU,
	KW_COND_LTS,
	KW_COND_GTS,
	KW_COND_LES,
	KW_COND_GES,
	KW_DATA,
	KW_LOAD,
	KW_INT16,
	KW_INT32,
	KW_INT64,
	KW_DB_ADDR,
	KW_MUT,
	KW_ZERO,
	N_VBRB_KWS
} VBRBKeyword;
static_assert(N_OPS == 70, "Some BRB operations have unmatched keywords");
static_assert(N_SYS_OPS == 8, "there might be system ops with unmatched keywords");

typedef struct {
	Token name;
	int32_t id;
} ExecMark;
declArray(ExecMark);
defArray(ExecMark);

char* getVBRBTokenTypeName(TokenType type)
{
	if (type == TOKEN_COND) return "condition";
	if (type == TOKEN_REG_ID) return "register";
	return getTokenTypeName(type);
}

void printVBRBError(FILE* dst, VBRBError err) {
	static_assert(N_VBRB_ERRORS == 27, "not all VBRB errors are handled");
	if (err.code != VBRB_ERR_OK) {
		fprintTokenLoc(dst, err.loc.loc);
		fprintf(dst, "error %04x: ", err.code);
		switch (err.code) {
			case N_VBRB_ERRORS: break;
			case VBRB_ERR_OK: break;
			case VBRB_ERR_BLOCK_NAME_EXPECTED:
				fprintf(dst, "expected a word as the block name, instead got ");
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_NO_MEMORY:
				fprintf(dst, "memory allocation failure\n");
				break;
			case VBRB_ERR_BLOCK_SPEC_EXPECTED:
				fprintf(dst, "expected a string as the data block specifier, instead got ");
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_BLOCK_SIZE_EXPECTED:
				fprintf(dst, "expected an integer as the memory block size, instead got ");
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_INVALID_ARG:
				fprintf(dst, 
					"`%.*s` operation expects %s as argument %hhd, instead got ",
					unpack(err.prep->keywords[err.op_type]),
					getVBRBTokenTypeName(err.expected_token_type),
					err.arg_id
				);
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_INVALID_OP:
				fprintf(dst, "expected operation name, `.load` directive or `.data` directive, instead got ");
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_UNKNOWN_SYSCALL:
				fprintf(dst, 
					"expected%s a syscall identifier, instead got ",
					( isWordToken(err.loc) ? "" : "a word as" )
				);
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_EXEC_MARK_NOT_FOUND:
				fprintf(dst, "execution mark `%s` not found\n", err.loc.string.data);
				break;
			case VBRB_ERR_DATA_BLOCK_NOT_FOUND:
				fprintf(dst, "data block `%s` not found\n", err.loc.string.data);
				break;
			case VBRB_ERR_INVALID_REG_ID:
				fprintf(dst, "invalid register index %d\n", err.loc.word[1] - '0');
				break;
			case VBRB_ERR_UNKNOWN_BUILTIN:
				fprintf(dst, "unknown built-in name `%s`\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_UNCONDITIONAL_OP:
			 	fprintf(dst, "operation `%.*s` cannot be conditional\n", unpack(opNames[err.op_type]));
				break;
			case VBRB_ERR_UNKNOWN_VAR_NAME:
				fprintf(dst, "unknown variable name %.*s\n", unpack(err.loc.string));
				break;
			case VBRB_ERR_UNCLOSED_PROC:
				fprintf(dst, "procedure must start in the global scope, i.e. not inside another procedure\n");
				break;
			case VBRB_ERR_UNKNOWN_CONDITION:
				fprintf(dst, "unknown condition specifier `%s`\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_INVALID_MODULE_NAME:
				fprintf(dst, "expected a word as the module name, instead got ");
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_MODULE_NOT_FOUND:
				fprintf(dst, "module `%s` not found\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_MODULE_NOT_LOADED:
				fprintf(dst, "module `%s` not loaded:\n", err.loc.word);
				printLoadError(dst, err.load_error);
				break;
			case VBRB_ERR_INVALID_NAME:
			 	fputs("invalid name `", dst);
				fputsbufesc(dst, err.loc.string, BYTEFMT_HEX);
				fputs("`\n", dst);
				break;
			case VBRB_ERR_NO_VAR:
				fprintf(dst, "cannot use `popv` operation; the stack is empty\n");
				break;
			case VBRB_ERR_DELNV_TOO_FEW_VARS:
				fprintf(dst, "cannot use `delnv` operation; the amount of variables on the stack is less than %d\n", err.var_count);
				break;
			case VBRB_ERR_VAR_TOO_LARGE:
				fprintf(
					dst,
					"cannot use `ldv`, `strv` and `pushv` operations; variable `%s` is larger than a BRB register (%lld > 8)\n",
					err.var.name,
					err.var.size
				);
				break;
			case VBRB_ERR_INVALID_DATA_BLOCK_FMT:
				fprintf(
					dst,
					"`%.*s` data piece expects %s as argument %hhd, instead got ",
					unpack(dataPieceNames[err.data_piece_type]),
					getVBRBTokenTypeName(err.expected_token_type),
					err.arg_id
				);
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_OP_OUTSIDE_OF_PROC:
				fprintf(dst, "operation `%.*s` can only be used inside a procedure\n", unpack(opNames[err.loc.keyword_id]));
				break;
			case VBRB_ERR_INVALID_VAR_SIZE:
				fputs("expected a positive integer as the variable size specifier, instead got ", dst);
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
			case VBRB_ERR_INVALID_VAR_OFFSET:
				fputs("expected an unsigned integer as the variable offset specifier, instead got ", dst);
				fprintTokenStr(dst, err.loc, err.prep);
				fputc('\n', dst);
				break;
				
		}
	}
}

int64_t getBRBuiltinValue(char* name)
{
	for (int64_t i = 0; i < N_BUILTINS; i++) {
		if (sbufeq(builtins[i].name, fromstr(name))) return i;
	}
	return -1;
}

typedef struct {
	ExecMarkArray proc_gotos;
	ExecMarkArray proc_marks;
	VarArray vars;
	int frame_start;
	ExecMarkArray global_gotos;
	ExecMarkArray global_marks;
	VarArray global_vars;
	bool in_proc;
	Token op_token;
	BRP* prep;
} CompilerCtx;
typedef VBRBError (*OpCompiler) (CompilerCtx*, Module*);

VBRBError getIntArg(CompilerCtx* ctx, Token src, uint64_t* dst, char op_type, char arg_id, bool for_data_block)
{
	if (src.type != TOKEN_INT) return (VBRBError){
		.prep = ctx->prep,
		.code = for_data_block ? VBRB_ERR_INVALID_DATA_BLOCK_FMT : VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_INT
	};
	*dst = src.value;
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError getIdent_s(CompilerCtx* ctx, Token src, sbuf* ident_p, char op_type, char arg_id, bool for_data_block)
{
	if (src.type != TOKEN_STRING) return (VBRBError){
		.prep = ctx->prep,
		.code = for_data_block ? VBRB_ERR_INVALID_DATA_BLOCK_FMT : VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_WORD
	};
	if (memchr(src.string.data, '\0', src.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = src
		};
	
	*ident_p = src.string;
	return (VBRBError){.prep = ctx->prep};
}

VBRBError getIdent(CompilerCtx* ctx, Token src, char** ident_p, char op_type, char arg_id, bool for_data_block)
{
	sbuf res;
	VBRBError err = getIdent_s(ctx, src, &res, op_type, arg_id, for_data_block);
	if (err.code) {
		sfree(&res);
		return err;
	}
	*ident_p = tostr(res);
	sfree(&res);
	return (VBRBError){.prep = ctx->prep};
}

VBRBError getSubmodule(CompilerCtx* ctx, Token src, int32_t* submodule_id_p, char op_type, char arg_id, bool for_data_block); // TODO

VBRBError getDataPiece(CompilerCtx* ctx, Module* module, DataPiece* piece, bool is_mutable)
{
	static_assert(N_PIECE_TYPES == 8, "not all data piece types are handled in `getDataPiece`");

	VBRBError err;
	Token spec = fetchToken(ctx->prep);
	if (spec.type == TOKEN_STRING) { // TODO: implement the syntax to explicitly create a nul-terminated string
		piece->data = spec.string;
		piece->type = PIECE_BYTES;
		return (VBRBError){ .prep = ctx->prep };
	} else if (spec.type == TOKEN_INT) {
		piece->type = PIECE_ZERO;
		piece->n_bytes = spec.value;
		return (VBRBError){ .prep = ctx->prep };
	}

	Token arg;
	switch (getTokenKeywordId(spec)) {
		case KW_INT16:
		case KW_INT32:
		case KW_INT64:
			piece->type = spec.keyword_id - KW_INT16 + PIECE_INT16;
			err = getIntArg(ctx, fetchToken(ctx->prep), &piece->integer, piece->type, 0, true);
			if (err.code) return err;
			break;
		case KW_DB_ADDR:
			arg = fetchToken(ctx->prep);
			piece->type = spec.keyword_id - KW_DB_ADDR + PIECE_DB_ADDR;
			if (!isWordToken(arg)) return (VBRBError){
				.prep = ctx->prep,
				.code = VBRB_ERR_INVALID_DATA_BLOCK_FMT,
				.loc = arg,
				.data_piece_type = piece->type,
				.arg_id = 0,
				.expected_token_type = TOKEN_WORD
			};
			const sbuf module_name = fromstr(getTokenWord(ctx->prep, arg));
			if (sbufeq(module_name, fromcstr("."))) {
				piece->module_id = module->submodules.length;
			} else {
				arrayForeach (Submodule, submodule, module->submodules) {
					if (sbufeq(module_name, fromstr((char*)submodule->name))) {
						piece->module_id = submodule - module->submodules.data;
						arg.type = TOKEN_NONE;
						break;
					}
				}
				if (arg.type) return (VBRBError){
					.prep = ctx->prep,
					.code = VBRB_ERR_MODULE_NOT_FOUND,
					.loc = arg
				};
			}

			err = getIdent(ctx, fetchToken(ctx->prep), &piece->mark_name, piece->type, 1, true);
			if (err.code) return err;
			break;
		case KW_ZERO:
			piece->type = PIECE_ZERO;
			err = getIntArg(ctx, fetchToken(ctx->prep), &piece->n_bytes, piece->type, 0, true);
			if (err.code) return err;
			break;
		default:
			return (VBRBError){
				.prep = ctx->prep,
				.code = VBRB_ERR_BLOCK_SPEC_EXPECTED,
				.loc = spec
			};
	}
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError getRegIdArg(CompilerCtx* ctx, Token src, uint8_t* dst, char op_type, char arg_id)
{
	if (src.type != TOKEN_WORD) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_REG_ID
	};
	if (strlen(src.word) != 2 || src.word[0] != 'r') return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_REG_ID
	};
	if (src.word[1] == 'Z') {
		*dst = ZEROREG_ID;
	} else {
		*dst = src.word[1] - '0';
		if (!inRange(*dst, 0, N_USER_REGS)) return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_REG_ID,
			.loc = src
		};
	}
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError getCondArg(CompilerCtx* ctx, Token src, uint8_t* dst, char op_type, char arg_id)
{
	*dst = getTokenKeywordId(src) - KW_COND_NON;
	if (*dst >= N_CONDS) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_COND
	};
	return (VBRBError){0};
}

VBRBError getVarArg(CompilerCtx* ctx, Token src, int32_t* offset_p, uint8_t* var_size_p, char op_type, char arg_id)
{
	int32_t stub_offset;
	uint8_t stub_var_size;
	if (!offset_p) offset_p = &stub_offset;
	if (!var_size_p) var_size_p = &stub_var_size;

	VBRBError err = getIdent_s(ctx, src, &src.string, op_type, arg_id, false);
	if (err.code) return err;
	
	*offset_p = ctx->frame_start;
	arrayForeach(Var, var, ctx->in_proc ? ctx->vars : ctx->global_vars) {
		if (sbufeq(fromstr(var->name), src.string)) { 
			src.string.data = NULL;
			*var_size_p = var->size;
			break;
		}
		*offset_p += var->size;
	}
	if (src.string.data) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_UNKNOWN_VAR_NAME,
		.loc = src
	};
	*offset_p += *var_size_p;

	src = peekToken(ctx->prep);
	if (getTokenSymbolId(src) == SYMBOL_COLON) {
		fetchToken(ctx->prep);
		src = fetchToken(ctx->prep);
		if (src.type != TOKEN_INT || src.value <= 0) return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_VAR_SIZE,
			.loc = src
		};
		*var_size_p = src.value;
		
		src = peekToken(ctx->prep);
		if (getTokenSymbolId(src) == SYMBOL_COLON) {
			fetchToken(ctx->prep);
			src = fetchToken(ctx->prep);
			if (src.type != TOKEN_INT || src.value < 0) return (VBRBError){
				.prep = ctx->prep,
				.code = VBRB_ERR_INVALID_VAR_OFFSET,
				.loc = src
			};

			*offset_p -= src.value;
		}
	}

	return (VBRBError){0};
}

void delCompilerCtx(CompilerCtx* ctx)
{
	ExecMarkArray_clear(&ctx->proc_gotos);
	ExecMarkArray_clear(&ctx->proc_marks);
	VarArray_clear(&ctx->vars);
	ExecMarkArray_clear(&ctx->global_gotos);
	ExecMarkArray_clear(&ctx->global_marks);
	VarArray_clear(&ctx->global_vars);
}

VBRBError compileNoArgOp(CompilerCtx* ctx, Module* dst)
{
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpMark(CompilerCtx* ctx, Module* dst)
{
	Token name_spec = fetchToken(ctx->prep);
	VBRBError err = getIdent_s(ctx, name_spec, &name_spec.string, OP_MARK, 0, false);
	if (err.code) return err;

	ExecMarkArray_append(
		ctx->in_proc ? &ctx->proc_marks : &ctx->global_marks,
		(ExecMark){ .name = name_spec, .id = dst->seg_exec.length - 1 }
	);

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileRegImmOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	err = getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 1, false);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile2RegOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);
	
	uint8_t dst_reg, src_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;
	op->src_reg = src_reg;
	
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetd(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	Token module_name_spec = fetchToken(ctx->prep);
	if (!isWordToken(module_name_spec)) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = module_name_spec,
		.op_type = OP_SETD,
		.arg_id = 1,
		.expected_token_type = TOKEN_WORD
	};
	const sbuf module_name = fromstr(getTokenWord(ctx->prep, module_name_spec));
	if (sbufeq(module_name, fromcstr("."))) {
		op->module_id = dst->submodules.length;
	} else {
		arrayForeach (Submodule, submodule, dst->submodules) {
			if (sbufeq(fromstr((char*)submodule->name), module_name)) {
				op->module_id = submodule - dst->submodules.data;
				module_name_spec.type = TOKEN_NONE;
				break;
			}
		}
		if (module_name_spec.type) return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_MODULE_NOT_FOUND,
			.loc  = module_name_spec
		};
	}

	return getIdent(ctx, fetchToken(ctx->prep), &op->mark_name, OP_SETD, 2, false);
}

VBRBError compileOpSetb(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	Token arg = fetchToken(ctx->prep);
	if (!isWordToken(arg)) {
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = op->type,
			.expected_token_type = TOKEN_WORD
		};
	}
	op->symbol_id = getBRBuiltinValue(getTokenWord(ctx->prep, arg));
	if ((int)op->symbol_id == -1) {
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_UNKNOWN_BUILTIN,
			.loc = arg
		};
	}

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSyscall(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);
	Token arg = fetchToken(ctx->prep);
	if (
		arg.type != TOKEN_KEYWORD || 
		!inRange(arg.keyword_id, N_OPS, N_OPS + N_SYS_OPS)
	) {
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_UNKNOWN_SYSCALL,
			.loc = arg
		};
	}
	op->syscall_id = arg.keyword_id - N_OPS;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpGoto(CompilerCtx* ctx, Module* dst)
{
	Token name_spec = fetchToken(ctx->prep);
	VBRBError err = getIdent_s(ctx, name_spec, &name_spec.string, OP_GOTO, 0, false);
	if (err.code) return err;
	ExecMarkArray_append(
		ctx->in_proc ? &ctx->proc_gotos : &ctx->global_gotos,
		(ExecMark){ .name = name_spec, .id = dst->seg_exec.length - 1 }
	);

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile2RegImmOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t dst_reg, src_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;
	op->src_reg = src_reg;

	err = getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 2, false);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile3RegOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);
	
	uint8_t dst_reg, src_reg, src2_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;
	op->src_reg = src_reg;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src2_reg, op->type, 2);
	if (err.code != VBRB_ERR_OK) return err;
	op->src2_reg = src2_reg;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpCall(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	Token module_name_spec = fetchToken(ctx->prep);
	if (!isWordToken(module_name_spec)) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = module_name_spec,
		.op_type = OP_CALL,
		.arg_id = 0,
		.expected_token_type = TOKEN_WORD
	};
	const char* module_name = getTokenWord(ctx->prep, module_name_spec);
	if (strcmp(module_name, ".") == 0) {
		op->module_id = dst->submodules.length;
	} else {
		arrayForeach (Submodule, submodule, dst->submodules) {
			if (strcmp(submodule->name, module_name) == 0) {
				op->module_id = submodule - dst->submodules.data;
				module_name_spec.type = TOKEN_NONE;
				break;
			}
		}
		if (module_name_spec.type) return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_MODULE_NOT_FOUND,
			.loc = module_name_spec
		};
	}

	return getIdent(ctx, fetchToken(ctx->prep), &op->mark_name, OP_CALL, 1, false);
}

VBRBError compileOpCmp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t src_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->src_reg = src_reg;

	return getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 1, false);
}

VBRBError compileOpCmpr(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t src_reg, src2_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->src_reg = src_reg;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src2_reg, op->type, 1);
	if (!err.code) op->src2_reg = src2_reg;

	return err;
}

VBRBError compileOpProc(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);
	if (ctx->in_proc) {
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_UNCLOSED_PROC,
			.loc = ctx->op_token
		};
	}
	ctx->in_proc = true;

	return getIdent(ctx, fetchToken(ctx->prep), &op->mark_name, op->type, 0, false);
}

VBRBError compileOpEndproc(CompilerCtx* ctx, Module* dst)
{
	if (!ctx->in_proc) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_OP_OUTSIDE_OF_PROC,
		.loc = ctx->op_token
	};
// resolving local marks and `goto`s
	arrayForeach (ExecMark, dst_mark, ctx->proc_gotos) {
		bool resolved = false;
		arrayForeach (ExecMark, src_mark, ctx->proc_marks) {
			if (sbufeq(src_mark->name.string, dst_mark->name.string)) {
				dst->seg_exec.data[dst_mark->id].op_offset = src_mark->id - dst_mark->id;
				resolved = true;
				break;
			}
		}
		if (!resolved) {
			return (VBRBError){
				.prep = ctx->prep,
				.code = VBRB_ERR_EXEC_MARK_NOT_FOUND,
				.loc = dst_mark->name
			};
		}
	}

	ExecMarkArray_clear(&ctx->proc_gotos);
	ExecMarkArray_clear(&ctx->proc_marks);
	VarArray_clear(&ctx->vars);
	ctx->in_proc = false;
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpVar(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	Var* new_var;
	if (!(new_var = VarArray_append(ctx->in_proc ? &ctx->vars : &ctx->global_vars, (Var){0}))) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_NO_MEMORY,
	};

	VBRBError err = getIdent(ctx, fetchToken(ctx->prep), &new_var->name, op->type, 0, false);
	if (err.code) return err;

	err = getIntArg(ctx, fetchToken(ctx->prep), (uint64_t*)&new_var->size, op->type, 1, false);
	if (!err.code) op->new_var_size = new_var->size;

	return err;
}

VBRBError compileOpSetv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code) return err;
	op->dst_reg = dst_reg;

	return getVarArg(ctx, fetchToken(ctx->prep), &op->symbol_id, NULL, op->type, 1);
}

VBRBError compileOpLdv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t dst_reg, var_size;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code) return err;
	op->dst_reg = dst_reg;

	Token var_name = fetchToken(ctx->prep);
	err = getVarArg(ctx, var_name, &op->symbol_id, &var_size, op->type, 1);
	if (err.code) return err;

	if (var_size > 8) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_VAR_TOO_LARGE,
		.var = (Var){
			.name = var_name.word,
			.size = var_size
		},
		.loc = ctx->op_token
	};
	op->var_size = var_size;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpStrv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	Token var_name = fetchToken(ctx->prep);
	uint8_t var_size, src_reg;
	VBRBError err = getVarArg(ctx, var_name, &op->symbol_id, &var_size, op->type, 1);
	if (err.code) return err;

	if (var_size > 8) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_VAR_TOO_LARGE,
		.var = (Var){
			.name = var_name.word,
			.size = var_size
		},
		.loc = ctx->op_token
	};
	op->var_size = var_size;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 0);
	if (err.code) return err;
	op->src_reg = src_reg;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpPopv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	VarArray* vars = ctx->in_proc ? &ctx->vars : &ctx->global_vars;
	if (vars->length == 0) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_NO_VAR,
		.loc = ctx->op_token
	};
	Var popped = VarArray_pop(vars, -1);
	op->var_size = popped.size;
	if (popped.size > 8) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_VAR_TOO_LARGE,
		.loc = ctx->op_token,
		.var = popped
	};

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (!err.code) op->dst_reg = dst_reg;

	return err;
}

VBRBError compileOpPushv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	Var* new_var;
	if (!(new_var = VarArray_append(ctx->in_proc ? &ctx->vars : &ctx->global_vars, (Var){0}))) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_NO_MEMORY,
	};

	VBRBError err = getIdent(ctx, fetchToken(ctx->prep), &new_var->name, op->type, 0, false);
	if (err.code) return err;

	err = getIntArg(ctx, fetchToken(ctx->prep), (uint64_t*)&new_var->size, op->type, 1, false);
	if (err.code) return err;

	if (new_var->size > 8) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_VAR_TOO_LARGE,
		.loc = ctx->op_token,
		.var = *new_var
	};
	op->var_size = new_var->size;

	uint8_t src_reg;
	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 2);
	if (!err.code) op->src_reg = src_reg;

	return err;
}

VBRBError compileOpAtf(CompilerCtx* ctx, Module* dst)
{
	register Op* op = arrayhead(dst->seg_exec);
	return getIdent(ctx, fetchToken(ctx->prep), &op->mark_name, op->type, 0, false);
}

VBRBError compileOpAtl(CompilerCtx* ctx, Module* dst)
{
	register Op* op = arrayhead(dst->seg_exec);
	return getIntArg(ctx, fetchToken(ctx->prep), (uint64_t*)&op->symbol_id, op->type, 0, false);
}

VBRBError compileOpSetc(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint8_t dst_reg, cond_arg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code) return err;
	op->dst_reg = dst_reg;

	err = getCondArg(ctx, fetchToken(ctx->prep), &cond_arg, op->type, 1);
	if (!err.code) op->cond_arg = cond_arg;

	return err;
}

VBRBError compileOpDelnv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->seg_exec);

	uint64_t n_vars;
	Token token = fetchToken(ctx->prep);
	VBRBError err = getIntArg(ctx, token, &n_vars, op->type, 0, false);
	if (err.code) return err;

	VarArray* vars = ctx->in_proc ? &ctx->vars : &ctx->global_vars;
	if (n_vars > vars->length) return (VBRBError){
		.code = VBRB_ERR_DELNV_TOO_FEW_VARS,
		.prep = ctx->prep,
		.loc = token,
		.var_count = n_vars
	};

	op->symbol_id = 0;
	for (int64_t i = vars->length - 1; i >= (int64_t)(vars->length - n_vars); --i) {
		op->symbol_id += vars->data[i].size;
	}
	vars->length -= n_vars;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpRet(CompilerCtx* ctx, Module* dst)
{
	if (!ctx->in_proc) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_OP_OUTSIDE_OF_PROC,
		.loc = ctx->op_token
	};
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpArg(CompilerCtx* ctx, Module* dst)
{
	if (!ctx->in_proc) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_OP_OUTSIDE_OF_PROC,
		.loc = ctx->op_token
	};
	if (ctx->frame_start == 0) {
		VarArray_prepend(&ctx->vars, (Var){ .name = "", .size = STACKFRAME_SIZE });
		ctx->frame_start = -STACKFRAME_SIZE;
	}

	Op* op = arrayhead(dst->seg_exec);
	Var* new_var;
	if (!(new_var = VarArray_prepend(&ctx->vars, (Var){0}))) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_NO_MEMORY,
	};

	VBRBError err = getIdent(ctx, fetchToken(ctx->prep), &new_var->name, op->type, 0, false);
	if (err.code) return err;

	err = getIntArg(ctx, fetchToken(ctx->prep), (uint64_t*)&new_var->size, op->type, 1, false);
	if (!err.code) ctx->frame_start -= op->new_var_size = new_var->size;

	return err;
}

OpCompiler op_compilers[] = {
	[OP_NONE] = compileNoArgOp,
	[OP_END] = compileNoArgOp,
	[OP_MARK] = compileOpMark,
	[OP_SET] = compileRegImmOp,
	[OP_SETR] = compile2RegOp,
	[OP_SETD] = compileOpSetd,
	[OP_SETB] = compileOpSetb,
	[OP_ADD] = compile2RegImmOp,
	[OP_ADDR] = compile3RegOp,
	[OP_SUB] = compile2RegImmOp,
	[OP_SUBR] = compile3RegOp,
	[OP_SYS] = compileOpSyscall,
	[OP_GOTO] = compileOpGoto,
	[OP_CMP] = compileOpCmp,
	[OP_CMPR] = compileOpCmpr,
	[OP_AND] = compile2RegImmOp,
	[OP_ANDR] = compile3RegOp,
	[OP_OR] = compile2RegImmOp,
	[OP_ORR] = compile3RegOp,
	[OP_NOT] = compile2RegOp,
	[OP_XOR] = compile2RegImmOp,
	[OP_XORR] = compile3RegOp,
	[OP_SHL] = compile2RegImmOp,
	[OP_SHLR] = compile3RegOp,
	[OP_SHR] = compile2RegImmOp,
	[OP_SHRR] = compile3RegOp,
	[OP_SHRS] = compile2RegImmOp,
	[OP_SHRSR] = compile3RegOp,
	[OP_PROC] = compileOpProc,
	[OP_CALL] = compileOpCall,
	[OP_RET] = compileOpRet,
	[OP_ENDPROC] = compileOpEndproc,
	[OP_LD64] = compile2RegOp,
	[OP_STR64] = compile2RegOp,
	[OP_LD32] = compile2RegOp,
	[OP_STR32] = compile2RegOp,
	[OP_LD16] = compile2RegOp,
	[OP_STR16] = compile2RegOp,
	[OP_LD8] = compile2RegOp,
	[OP_STR8] = compile2RegOp,
	[OP_VAR] = compileOpVar,
	[OP_SETV] = compileOpSetv,
	[OP_MUL] = compile2RegImmOp,
	[OP_MULR] = compile3RegOp,
	[OP_DIV] = compile2RegImmOp,
	[OP_DIVR] = compile3RegOp,
	[OP_DIVS] = compile2RegImmOp,
	[OP_DIVSR] = compile3RegOp,
	[OP_EXTPROC] = compileOpProc,
	[OP_LDV] = compileOpLdv,
	[OP_STRV] = compileOpStrv,
	[OP_POPV] = compileOpPopv,
	[OP_PUSHV] = compileOpPushv,
	[OP_ATF] = compileOpAtf,
	[OP_ATL] = compileOpAtl,
	[OP_SETC] = compileOpSetc,
	[OP_DELNV] = compileOpDelnv,
	[OP_LD64S] = compile2RegOp,
	[OP_LD32S] = compile2RegOp,
	[OP_LD16S] = compile2RegOp,
	[OP_LD8S] = compile2RegOp,
	[OP_LDVS] = compileOpLdv,
	[OP_SX32] = compile2RegOp,
	[OP_SX16] = compile2RegOp,
	[OP_SX8] = compile2RegOp,
	[OP_MOD] = compile2RegImmOp,
	[OP_MODS] = compile2RegImmOp,
	[OP_MODR] = compile3RegOp,
	[OP_MODSR] = compile3RegOp,
	[OP_ARG] = compileOpArg
};
static_assert(N_OPS == sizeof(op_compilers) / sizeof(op_compilers[0]), "Some BRB operations have unmatched compilers");

VBRBError compileVBRB(FILE* src, const char* src_name, Module* dst, const char* search_paths[])
{
	BRP* obj = malloc(sizeof(BRP));
	initBRP(obj, NULL, BRP_ESC_STR_LITERALS);
	setSymbols(obj, vbrb_symbols);
	setKeywords(obj, vbrb_keywords);
	setInput(obj, (char*)src_name, src);
	
	dst->seg_exec = OpArray_new(0);
	dst->seg_data = DataBlockArray_new(0);
	dst->submodules = SubmoduleArray_new(0);
	dst->stack_size = DEFAULT_STACK_SIZE;

	CompilerCtx compctx = {.prep = obj};
	VBRBError err;

	while (true) {
		Token op_name = fetchToken(obj);
		if (!op_name.type) break; 

		char kw_id = getTokenKeywordId(op_name);
		if (inRange(kw_id, 0, N_OPS)) {
			Op* new_op;
			if (!(new_op = OpArray_append(&dst->seg_exec, (Op){0}))) {
				delCompilerCtx(&compctx);
				return (VBRBError){
					.prep = obj,
					.code = VBRB_ERR_NO_MEMORY,
					.loc = op_name
				};
			}

			new_op->type = kw_id;
			compctx.op_token = op_name;
			Token cond_spec = peekToken(obj);

			if (getTokenSymbolId(cond_spec) == SYMBOL_COLON) {
				if (op_flags[new_op->type] & OPF_UNCONDITIONAL) return (VBRBError){
					.prep = obj,
					.code = VBRB_ERR_UNCONDITIONAL_OP,
					.loc = cond_spec,
					.op_type = new_op->type
				};

				fetchToken(obj); // to remove the peeked ':' symbol
				cond_spec = fetchToken(obj);
				if (!inRange(getTokenKeywordId(cond_spec), KW_COND_NON, KW_COND_NON + N_CONDS)) {
					delCompilerCtx(&compctx);
					return (VBRBError){
						.prep = obj,
						.code = VBRB_ERR_UNKNOWN_CONDITION,
						.loc = cond_spec
					};
				}
				new_op->cond_id = cond_spec.keyword_id - KW_COND_NON;
			}

			err = op_compilers[(unsigned)kw_id](&compctx, dst);
			if (err.code) {
				delCompilerCtx(&compctx);
				return err;
			}
		} else if (kw_id == KW_DATA) {
			Token token = fetchToken(compctx.prep);
			DataBlock* block;
			if (!(block = DataBlockArray_append(&dst->seg_data, (DataBlock){0}))) {
				delCompilerCtx(&compctx);
				return (VBRBError){
					.prep = obj,
					.code = VBRB_ERR_NO_MEMORY,
					.loc = token
				};
			}

			if (getTokenKeywordId(token) == KW_MUT) {
				block->is_mutable = true;
				token = fetchToken(compctx.prep);
			}

			err = getIdent(&compctx, token, &block->name, 0, 0, true);
			if (err.code) {
				if (err.code == VBRB_ERR_INVALID_DATA_BLOCK_FMT) err.code = VBRB_ERR_BLOCK_NAME_EXPECTED;
				return err;
			}

			token = peekToken(compctx.prep);
			if (getTokenSymbolId(token) == SYMBOL_SEGMENT_START) {
				fetchToken(compctx.prep);
				while (getTokenSymbolId(peekToken(compctx.prep)) != SYMBOL_SEGMENT_END) {
					DataPiece* piece;
					if (!(piece = DataPieceArray_append(&block->pieces, (DataPiece){0}))) return (VBRBError){
						.prep = compctx.prep,
						.code = VBRB_ERR_NO_MEMORY,
						.loc = peekToken(compctx.prep)
					};

					VBRBError err = getDataPiece(&compctx, dst, piece, block->is_mutable);
					if (err.code) return err;
				}
				fetchToken(compctx.prep);
			} else {
				DataPiece* piece;
				if (!(piece = DataPieceArray_append(&block->pieces, (DataPiece){0}))) return (VBRBError){
					.prep = compctx.prep,
					.code = VBRB_ERR_NO_MEMORY,
					.loc = peekToken(compctx.prep)
				};

				VBRBError err = getDataPiece(&compctx, dst, piece, block->is_mutable);
				if (err.code) return err;
			}
		} else if (kw_id == KW_LOAD) {
			Token token = fetchToken(compctx.prep);
			if (!isWordToken(token)) return (VBRBError){
				.prep = compctx.prep,
				.code = VBRB_ERR_INVALID_MODULE_NAME,
				.loc = token
			};
			
			char* module_name = getTokenWord(compctx.prep, token);
			FILE* module_fd = findModule(module_name, search_paths);
			if (!module_fd) return (VBRBError){
				.prep = compctx.prep,
				.code = VBRB_ERR_INVALID_MODULE_NAME,
				.loc = token
			};

			Module submodule;
			BRBLoadError err = preloadModule(module_fd, &submodule, search_paths);
			if (err.code) return (VBRBError){
				.prep = compctx.prep,
				.code = VBRB_ERR_MODULE_NOT_LOADED,
				.loc = token,
				.load_error = err
			};
			mergeModule(&submodule, dst, module_name);
		} else return (VBRBError){
			.prep = compctx.prep,
			.code = VBRB_ERR_INVALID_OP,
			.loc = op_name
		};
	}

// resolving global marks and `goto`s
	arrayForeach (ExecMark, dst_mark, compctx.global_gotos) {
		bool resolved = false;
		arrayForeach (ExecMark, src_mark, compctx.global_marks) {
			if (sbufeq(src_mark->name.string, dst_mark->name.string)) {
				dst->seg_exec.data[dst_mark->id].op_offset = src_mark->id - dst_mark->id;
				resolved = true;
				break;
			}
		}
		if (!resolved) return (VBRBError){
			.prep = compctx.prep,
			.code = VBRB_ERR_EXEC_MARK_NOT_FOUND,
			.loc = dst_mark->name
		};
	}

	OpArray_append(&dst->seg_exec, (Op){.type = OP_END});

	BRBLoadError resolution_err = resolveModule(dst);
	if (resolution_err.code) return (VBRBError){
		.code = VBRB_ERR_MODULE_NOT_LOADED,
		.loc = (Token){ .type = TOKEN_WORD, .word = "." },
		.load_error = resolution_err
	};

	delCompilerCtx(&compctx);
	return (VBRBError){ .prep = obj };
}

void cleanupVBRBCompiler(VBRBError status)
{
	delBRP(status.prep);
}
