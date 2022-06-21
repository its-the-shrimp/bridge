#include <brb.h>

declArray(Var);
defArray(Var);

typedef enum {
	SYMBOL_SEGMENT_START,
	SYMBOL_SEGMENT_END,
	SYMBOL_CONDITION_SPEC,
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
	KW_SETM,
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
	KW_ENTRY,
	KW_STACKSIZE,
	KW_EXEC,
	KW_DATA,
	KW_MEMORY,
	KW_LOAD,
	KW_INT16,
	KW_INT32,
	KW_INT64,
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
	static_assert(N_VBRB_ERRORS == 28, "not all VBRB errors are handled");
	if (err.code != VBRB_ERR_OK) {
		fprintTokenLoc(stderr, err.loc.loc);
		fprintf(dst, "error: ");
		switch (err.code) {
			case N_VBRB_ERRORS: break;
			case VBRB_ERR_OK: break;
			case VBRB_ERR_BLOCK_NAME_EXPECTED:
				fprintf(dst, "expected a word as the block name, instead got ");
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_STACK_SIZE_EXPECTED:
				fprintf(dst, "expected integer as the stack size specifier, instead got ");
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_NO_MEMORY:
				fprintf(dst, "memory allocation failure\n");
				break;
			case VBRB_ERR_SEGMENT_START_EXPECTED:
				fprintf(dst, "expected ");
				fprintTokenStr(
					stderr, 
					(Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_SEGMENT_START }, 
					err.prep
				);
				fprintf(dst, " as the segment start, instead got ");
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_BLOCK_SPEC_EXPECTED:
				fprintf(dst, "expected a string as the data block specifier, instead got ");
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_BLOCK_SIZE_EXPECTED:
				fprintf(dst, "expected an integer as the memory block size, instead got ");
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_UNKNOWN_SEGMENT_SPEC:
				fprintf(dst, "expected a segment specifier, instead got ");
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_UNCLOSED_SEGMENT:
				fprintf(dst, "expected ");
				fprintTokenStr(
					stderr, 
					(Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_SEGMENT_END }, 
					err.prep
				);
				fprintf(dst, ", instead got ");
				fprintTokenStr(stderr, (Token){ .type = TOKEN_NONE }, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_INVALID_ARG:
				fprintf(dst, 
					sbuf_format" operation expects %s as argument %hhd, instead got ",
					unpack(err.prep->keywords[err.op_type]),
					getVBRBTokenTypeName(err.expected_token_type),
					err.arg_id
				);
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_INVALID_OP:
				fprintf(dst, "expected operation name, instead got ");
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_UNKNOWN_SYSCALL:
				fprintf(dst, 
					"expected%s a syscall identifier, instead got ",
					( isWordToken(err.loc) ? "" : "a word as" )
				);
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_EXEC_MARK_NOT_FOUND:
				fprintf(dst, "execution mark `%s` not found\n", err.loc.string.data);
				break;
			case VBRB_ERR_DATA_BLOCK_NOT_FOUND:
				fprintf(dst, "data block `%s` not found\n", err.loc.string.data);
				break;
			case VBRB_ERR_MEM_BLOCK_NOT_FOUND:
				fprintf(dst, "memory block `%s` not found\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_INVALID_REG_ID:
				fprintf(dst, "invalid register index %d\n", err.loc.word[1] - '0');
				break;
			case VBRB_ERR_UNKNOWN_BUILTIN:
				fprintf(dst, "unknown built-in name `%s`\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_UNCONDITIONAL_OP:
			 	fprintf(stderr, "operation `%.*s` cannot be conditional\n", unpack(opNames[err.op_type]));
				break;
			case VBRB_ERR_UNKNOWN_VAR_NAME:
				fprintf(dst, "unknown variable name %s\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_UNCLOSED_PROC:
				fprintf(dst, "procedure must start in the global scope, i.e. not inside another procedure\n");
				break;
			case VBRB_ERR_UNKNOWN_CONDITION:
				fprintf(dst, "unknown condition specifier `%s`\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_INVALID_MODULE_NAME:
				fprintf(dst, "expected a word as the module name, instead got ");
				fprintTokenStr(stderr, err.loc, err.prep);
				fputc('\n', stderr);
				break;
			case VBRB_ERR_MODULE_NOT_FOUND:
				fprintf(dst, "module `%s` not found\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_MODULE_NOT_LOADED:
				printLoadError(err.load_error);
				break;
            case VBRB_ERR_INVALID_NAME:
			 	fputs("invalid name `", stderr);
				fputsbufesc(stderr, err.loc.string, BYTEFMT_HEX);
				fputs("`\n", stderr);
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
		}
	}
}

int64_t getBRBuiltinValue(char* name)
{
	for (int64_t i = 0; i < N_BUILTINS; i++) {
		if (sbufeq(builtins[i].name, name)) return i;
	}
	return -1;
} 

typedef struct {
	ExecMarkArray proc_gotos;
	ExecMarkArray proc_marks;
	VarArray vars;
	bool in_proc;
	Token op_token;
	BRP* prep;
} CompilerCtx;
typedef VBRBError (*OpCompiler) (CompilerCtx*, Module*);

VBRBError getDataPiece(CompilerCtx* ctx, DataPiece* piece)
{
	Token spec = fetchToken(ctx->prep);
	if (spec.type == TOKEN_STRING) {
		piece->data = spec.string;
		int nul_index = sbufindex(spec.string, CSTRTERM);
		if (nul_index < 0) {
			piece->type = PIECE_TEXT;
			++piece->data.length;
		} else {
			piece->type = PIECE_BYTES;
		}
		return (VBRBError){ .prep = ctx->prep };
	}

	Token arg;
	switch (getTokenKeywordId(spec)) {
		case KW_INT16:
		case KW_INT32:
		case KW_INT64:
			arg = fetchToken(ctx->prep);
			if (arg.type != TOKEN_INT) return (VBRBError){
				.prep = ctx->prep,
				.code = VBRB_ERR_BLOCK_SPEC_EXPECTED,
				.loc = arg
			};
			piece->type = spec.keyword_id - KW_INT16 + PIECE_INT16;
			piece->integer = arg.value;
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

VBRBError getIntArg(CompilerCtx* ctx, Token src, uint64_t* dst, char op_type, char arg_id)
{
	if (src.type != TOKEN_INT) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_INT
	};
	*dst = src.value;
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError getVarArg(CompilerCtx* ctx, Token src, int32_t* offset_p, uint8_t* var_size_p, char op_type, char arg_id)
{
	int32_t stub_offset;
	uint8_t stub_var_size;
	if (!offset_p) offset_p = &stub_offset;
	if (!var_size_p) var_size_p = &stub_var_size;

	if (src.type != TOKEN_STRING) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
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
	
	*offset_p = 0;
	for (int i = 0; i < ctx->vars.length; ++i) {
		if (sbufeq(ctx->vars.data[i].name, src.string)) { 
			src.string.data = NULL;
			*var_size_p = ctx->vars.data[i].size;
			break;
		}
		*offset_p += ctx->vars.data[i].size;
	}
	if (src.string.data) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_UNKNOWN_VAR_NAME,
		.loc = src
	};
	*offset_p += *var_size_p;

	return (VBRBError){0};
}

void delCompilerCtx(CompilerCtx* ctx)
{
	ExecMarkArray_clear(&ctx->proc_gotos);
	ExecMarkArray_clear(&ctx->proc_marks);
	VarArray_clear(&ctx->vars);
}

VBRBError compileNoArgOp(CompilerCtx* ctx, Module* dst)
{
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpMark(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	Token name_spec = fetchToken(ctx->prep);
	if (name_spec.type != TOKEN_STRING) {
		return (VBRBError){
            .prep = ctx->prep,
			.code = VBRB_ERR_INVALID_ARG,
			.loc = name_spec,
			.op_type = OP_MARK,
			.arg_id = 0,
			.expected_token_type = TOKEN_STRING
		};
	}
	ExecMarkArray_append(&ctx->proc_marks, (ExecMark){ .name = name_spec, .id = dst->execblock.length - 1 });

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileRegImmOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	err = getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile2RegOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);
	
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
	Op* op = arrayhead(dst->execblock);

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
	const sbuf module_name = SBUF(getTokenWord(ctx->prep, module_name_spec));
	if (sbufeq(module_name, CSBUF("."))) {
		op->module_id = dst->submodules.length;
	} else {
		for (Submodule* submodule = dst->submodules.data; submodule - dst->submodules.data < dst->submodules.length; ++submodule) {
			if (sbufeq(submodule->name, module_name)) {
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

	Token datablock_name_spec = fetchToken(ctx->prep);
	if (datablock_name_spec.type != TOKEN_STRING) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.op_type = OP_SETD,
		.arg_id = 2,
		.expected_token_type = TOKEN_STRING
	};
	if (memchr(datablock_name_spec.string.data, '\0', datablock_name_spec.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = datablock_name_spec
		};
	op->mark_name = datablock_name_spec.string.data;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetm(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	Token module_name_spec = fetchToken(ctx->prep);
	if (!isWordToken(module_name_spec)) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = module_name_spec,
		.op_type = OP_SETM,
		.arg_id = 1,
		.expected_token_type = TOKEN_WORD
	};
	const sbuf module_name = SBUF(getTokenWord(ctx->prep, module_name_spec));
	if (sbufeq(module_name, CSBUF("."))) {
		op->module_id = dst->submodules.length;
	} else {
		for (Submodule* submodule = dst->submodules.data; submodule - dst->submodules.data < dst->submodules.length; ++submodule) {
			if (sbufeq(submodule->name, module_name)) {
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

	Token name_spec = fetchToken(ctx->prep);
	if (name_spec.type != TOKEN_STRING) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = name_spec,
		.op_type = OP_SETM,
		.arg_id = 2,
		.expected_token_type = TOKEN_STRING
	};
	if (memchr(name_spec.string.data, '\0', name_spec.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = name_spec
		};
	op->mark_name = name_spec.string.data;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetb(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

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
	if (op->symbol_id == -1) {
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
	Op* op = arrayhead(dst->execblock);
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
	Op* op = arrayhead(dst->execblock);

	Token name_spec = fetchToken(ctx->prep);
	if (name_spec.type != TOKEN_STRING) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = name_spec,
		.op_type = op->type,
		.arg_id = 0,
		.expected_token_type = TOKEN_STRING
	};
	if (memchr(name_spec.string.data, '\0', name_spec.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = name_spec
		};
	ExecMarkArray_append(&ctx->proc_gotos, (ExecMark){ .name = name_spec, .id = dst->execblock.length - 1 });

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile2RegImmOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint8_t dst_reg, src_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->dst_reg = dst_reg;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;
	op->src_reg = src_reg;

	err = getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 2);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile3RegOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);
	
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
	Op* op = arrayhead(dst->execblock);

	Token module_name_spec = fetchToken(ctx->prep);
	if (!isWordToken(module_name_spec)) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = module_name_spec,
		.op_type = OP_CALL,
		.arg_id = 0,
		.expected_token_type = TOKEN_WORD
	};
	const sbuf module_name = SBUF(getTokenWord(ctx->prep, module_name_spec));
	if (sbufeq(module_name, CSBUF("."))) {
		op->module_id = dst->submodules.length;
	} else {
		for (Submodule* submodule = dst->submodules.data; submodule - dst->submodules.data < dst->submodules.length; ++submodule) {
			if (sbufeq(submodule->name, module_name)) {
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

	Token name_spec = fetchToken(ctx->prep);
	if (name_spec.type != TOKEN_STRING) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = name_spec,
		.op_type = OP_CALL,
		.arg_id = 1,
		.expected_token_type = TOKEN_WORD
	};
	if (memchr(name_spec.string.data, '\0', name_spec.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = name_spec
		};
	op->mark_name = name_spec.string.data;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpCmp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint8_t src_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->src_reg = src_reg;

	err = getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpCmpr(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint8_t src_reg, src2_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->src_reg = src_reg;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src2_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;
	op->src2_reg = src2_reg;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpProc(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);
	if (ctx->in_proc) {
		return (VBRBError){
            .prep = ctx->prep,
			.code = VBRB_ERR_UNCLOSED_PROC,
			.loc = ctx->op_token
		};
	}
	ctx->in_proc = true;

	Token name_spec = fetchToken(ctx->prep);
	if (name_spec.type != TOKEN_STRING) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = name_spec,
		.op_type = op->type,
		.arg_id = 0,
		.expected_token_type = TOKEN_STRING
	};
	if (memchr(name_spec.string.data, '\0', name_spec.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = name_spec
		};
	op->mark_name = name_spec.string.data;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpEndproc(CompilerCtx* ctx, Module* dst)
{
// resolving local marks and `goto`s
	for (int goto_index = 0; goto_index < ctx->proc_gotos.length; goto_index++) {
		ExecMark* dst_mark = ctx->proc_gotos.data + goto_index;
		for (int mark_index = 0; mark_index < ctx->proc_marks.length; mark_index++) {
			ExecMark* src_mark = ctx->proc_marks.data + mark_index;
			if (sbufeq(src_mark->name.string, dst_mark->name.string)) {
				dst->execblock.data[dst_mark->id].op_offset = src_mark->id - dst_mark->id;
				dst_mark = NULL;
				break;
			}
		}
		if (dst_mark) {
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
	Op* op = arrayhead(dst->execblock);

	Token token = fetchToken(ctx->prep);
	Var* new_var;
	if (!(new_var = VarArray_append(&ctx->vars, (Var){0}))) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_NO_MEMORY,
		.loc = token
	};

	if (token.type != TOKEN_STRING) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = token,
		.op_type = op->type,
		.arg_id = 0,
		.expected_token_type = TOKEN_STRING
	};
	if (memchr(token.string.data, '\0', token.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = token
		};
	new_var->name = token.string.data;

	token = fetchToken(ctx->prep);
	if (token.type != TOKEN_INT) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = token,
		.op_type = op->type,
		.arg_id = 1,
		.expected_token_type = TOKEN_INT
	};
	op->new_var_size = new_var->size = token.value;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code) return err;
	op->dst_reg = dst_reg;

	err = getVarArg(ctx, fetchToken(ctx->prep), &op->symbol_id, NULL, op->type, 1);
	if (err.code) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpLdv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint8_t dst_reg, var_size;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code) return err;
	op->dst_reg = dst_reg;

	Token var_name = fetchToken(ctx->prep);
	err = getVarArg(ctx, var_name, &op->symbol_id, &var_size, op->type, 1);
	if (err.code) return err;
	op->var_size = var_size;

	if (op->var_size > 8) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_VAR_TOO_LARGE,
		.var = (Var){
			.name = var_name.word,
			.size = op->var_size
		},
		.loc = ctx->op_token
	};

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpStrv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	Token var_name = fetchToken(ctx->prep);
	uint8_t var_size, src_reg;
	VBRBError err = getVarArg(ctx, var_name, &op->symbol_id, &var_size, op->type, 1);
	if (err.code) return err;
	op->var_size = var_size;

	if (op->var_size > 8) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_VAR_TOO_LARGE,
		.var = (Var){
			.name = var_name.word,
			.size = op->var_size
		},
		.loc = ctx->op_token
	};

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 0);
	if (err.code) return err;
	op->src_reg = src_reg;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpPopv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	if (ctx->vars.length == 0) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_NO_VAR,
		.loc = ctx->op_token
	};
	Var popped = VarArray_pop(&ctx->vars, -1);
	op->var_size = popped.size;
	if (popped.size > 8) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_VAR_TOO_LARGE,
		.loc = ctx->op_token,
		.var = popped
	};

	uint8_t dst_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code) return err;
	op->dst_reg = dst_reg;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpPushv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	Token token = fetchToken(ctx->prep);
	Var* new_var;
	if (!(new_var = VarArray_append(&ctx->vars, (Var){0}))) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_NO_MEMORY,
		.loc = token
	};

	if (token.type != TOKEN_STRING) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = token,
		.op_type = op->type,
		.arg_id = 0,
		.expected_token_type = TOKEN_WORD
	};
	if (memchr(token.string.data, '\0', token.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = token
		};
	new_var->name = token.string.data;

	token = fetchToken(ctx->prep);
	if (token.type != TOKEN_INT) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = token,
		.op_type = op->type,
		.arg_id = 1,
		.expected_token_type = TOKEN_INT
	};
	op->var_size = new_var->size = (int8_t)token.value;

	if (token.value > 8) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_VAR_TOO_LARGE,
		.loc = ctx->op_token,
		.var = *new_var
	};

	uint8_t src_reg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &src_reg, op->type, 2);
	if (err.code) return err;
	op->src_reg = src_reg;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpAtf(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	Token src_path = fetchToken(ctx->prep);
	if (src_path.type != TOKEN_STRING) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src_path,
		.op_type = op->type,
		.arg_id = 0,
		.expected_token_type = TOKEN_STRING
	};
	if (memchr(src_path.string.data, '\0', src_path.string.length))
		return (VBRBError){
			.prep = ctx->prep,
			.code = VBRB_ERR_INVALID_NAME,
			.loc = src_path
		};
	op->mark_name = src_path.string.data;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpAtl(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getIntArg(ctx, fetchToken(ctx->prep), (uint64_t*)&op->symbol_id, op->type, 0);
	if (err.code) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetc(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint8_t dst_reg, cond_arg;
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &dst_reg, op->type, 0);
	if (err.code) return err;
	op->dst_reg = dst_reg;

	err = getCondArg(ctx, fetchToken(ctx->prep), &cond_arg, op->type, 1);
	if (err.code) return err;
	op->cond_arg = cond_arg;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpDelnv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	uint64_t n_vars;
	Token token = fetchToken(ctx->prep);
	VBRBError err = getIntArg(ctx, token, &n_vars, op->type, 0);
	if (err.code) return err;

	if (n_vars > ctx->vars.length) return (VBRBError){
		.code = VBRB_ERR_DELNV_TOO_FEW_VARS,
		.prep = ctx->prep,
		.loc = token,
		.var_count = n_vars
	};

	op->symbol_id = 0;
	for (int64_t i = ctx->vars.length - 1; i >= (int64_t)(ctx->vars.length - n_vars); i--) {
		op->symbol_id += ctx->vars.data[i].size;
	}
	ctx->vars.length -= n_vars;

	return (VBRBError){ .prep = ctx->prep };
}

OpCompiler op_compilers[] = {
	[OP_NONE] = &compileNoArgOp,
	[OP_END] = &compileNoArgOp,
	[OP_MARK] = &compileOpMark,
	[OP_SET] = &compileRegImmOp,
	[OP_SETR] = &compile2RegOp,
	[OP_SETD] = &compileOpSetd,
	[OP_SETB] = &compileOpSetb,
	[OP_SETM] = &compileOpSetm,
	[OP_ADD] = &compile2RegImmOp,
	[OP_ADDR] = &compile3RegOp,
	[OP_SUB] = &compile2RegImmOp,
	[OP_SUBR] = &compile3RegOp,
	[OP_SYS] = &compileOpSyscall,
	[OP_GOTO] = &compileOpGoto,
	[OP_CMP] = &compileOpCmp,
	[OP_CMPR] = &compileOpCmpr,
	[OP_AND] = &compile2RegImmOp,
	[OP_ANDR] = &compile3RegOp,
	[OP_OR] = &compile2RegImmOp,
	[OP_ORR] = &compile3RegOp,
	[OP_NOT] = &compile2RegOp,
	[OP_XOR] = &compile2RegImmOp,
	[OP_XORR] = &compile3RegOp,
	[OP_SHL] = &compile2RegImmOp,
	[OP_SHLR] = &compile3RegOp,
	[OP_SHR] = &compile2RegImmOp,
	[OP_SHRR] = &compile3RegOp,
	[OP_SHRS] = &compile2RegImmOp,
	[OP_SHRSR] = &compile3RegOp,
	[OP_PROC] = &compileOpProc,
	[OP_CALL] = &compileOpCall,
	[OP_RET] = &compileNoArgOp,
	[OP_ENDPROC] = &compileOpEndproc,
	[OP_LD64] = &compile2RegOp,
	[OP_STR64] = &compile2RegOp,
	[OP_LD32] = &compile2RegOp,
	[OP_STR32] = &compile2RegOp,
	[OP_LD16] = &compile2RegOp,
	[OP_STR16] = &compile2RegOp,
	[OP_LD8] = &compile2RegOp,
	[OP_STR8] = &compile2RegOp,
	[OP_VAR] = &compileOpVar,
	[OP_SETV] = &compileOpSetv,
	[OP_MUL] = &compile2RegImmOp,
	[OP_MULR] = &compile3RegOp,
	[OP_DIV] = &compile2RegImmOp,
	[OP_DIVR] = &compile3RegOp,
	[OP_DIVS] = &compile2RegImmOp,
	[OP_DIVSR] = &compile3RegOp,
	[OP_EXTPROC] = &compileOpProc,
	[OP_LDV] = &compileOpLdv,
	[OP_STRV] = &compileOpStrv,
	[OP_POPV] = &compileOpPopv,
	[OP_PUSHV] = &compileOpPushv,
	[OP_ATF] = &compileOpAtf,
	[OP_ATL] = &compileOpAtl,
	[OP_SETC] = &compileOpSetc,
	[OP_DELNV] = &compileOpDelnv,
	[OP_LD64S] = &compile2RegOp,
	[OP_LD32S] = &compile2RegOp,
	[OP_LD16S] = &compile2RegOp,
	[OP_LD8S] = &compile2RegOp,
	[OP_LDVS] = &compileOpLdv,
	[OP_SX32] = &compile2RegOp,
	[OP_SX16] = &compile2RegOp,
	[OP_SX8] = &compile2RegOp,
	[OP_MOD] = &compile2RegImmOp,
	[OP_MODS] = &compile2RegImmOp,
	[OP_MODR] = &compile3RegOp,
	[OP_MODSR] = &compile3RegOp
};
static_assert(N_OPS == sizeof(op_compilers) / sizeof(op_compilers[0]), "Some BRB operations have unmatched compilers");

VBRBError compileModule(FILE* src, char* src_name, Module* dst, char* search_paths[], int flags)
{
    BRP* obj = malloc(sizeof(BRP));
    initBRP(obj, NULL, BRP_ESC_STR_LITERALS);
	setSymbols(
		obj,
		BRP_SYMBOL("{"),
		BRP_SYMBOL("}"),
		BRP_SYMBOL(":"),
		BRP_HIDDEN_SYMBOL(" "),
		BRP_HIDDEN_SYMBOL("\t"),
		BRP_HIDDEN_SYMBOL("\n")
	);
	setKeywords(
		obj,
		_opNames,
		_syscallNames,
		_conditionNames,
		BRP_KEYWORD("entry"), 
		BRP_KEYWORD("stacksize"),
		BRP_KEYWORD("exec"),
		BRP_KEYWORD("data"),
		BRP_KEYWORD("memory"),
		BRP_KEYWORD("load"),
		BRP_KEYWORD(".int16"),
		BRP_KEYWORD(".int32"),
		BRP_KEYWORD(".int64")
	);
	setInput(obj, src_name, src);
	
	dst->entry_opid = -1;
	dst->execblock = OpArray_new(0);
	dst->memblocks = MemBlockArray_new(0);
	dst->datablocks = DataBlockArray_new(0);
	dst->submodules = SubmoduleArray_new(0);
	dst->stack_size = DEFAULT_STACK_SIZE;

	CompilerCtx compctx = {
		.proc_gotos = ExecMarkArray_new(0),
		.proc_marks = ExecMarkArray_new(0),
		.vars = VarArray_new(0),
		.prep = obj
	};
	Token entry_name = {0};

	while (!BRPempty(obj)) {
		Token segment_spec = fetchToken(obj);
		if (segment_spec.type == TOKEN_NONE) { break; }
		switch (getTokenKeywordId(segment_spec)) {
			case KW_STACKSIZE: {
				Token size_spec = fetchToken(obj);
				if (size_spec.type != TOKEN_INT) {
					delCompilerCtx(&compctx);
					return (VBRBError){
                        .prep = obj,
						.code = VBRB_ERR_STACK_SIZE_EXPECTED,
						.loc = size_spec
					};
				}

				dst->stack_size = size_spec.value * 1024;
				break;
			} case KW_DATA: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					delCompilerCtx(&compctx);
					return (VBRBError){
                        .prep = obj,
						.code = VBRB_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Token block_name, block_spec;
				DataBlock* block;
				while (true) {
					block_name = fetchToken(obj);
					if (getTokenSymbolId(block_name) == SYMBOL_SEGMENT_END) {
						break;
					} else if (block_name.type != TOKEN_STRING) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					if (memchr(block_name.string.data, '\0', block_name.string.length)) return (VBRBError){
						.prep = obj,
						.code = VBRB_ERR_INVALID_NAME,
						.loc = block_name
					};

					if (!(block = DataBlockArray_append(&dst->datablocks, (DataBlock){0}))) {
						delCompilerCtx(&compctx);
						return (VBRBError){
							.prep = obj,
							.code = VBRB_ERR_NO_MEMORY
						};
					}

					block->name = block_name.string.data;

					block_spec = peekToken(obj);
					if (getTokenSymbolId(block_spec) == SYMBOL_SEGMENT_START) {
						fetchToken(obj);
						while (getTokenSymbolId(peekToken(obj)) != SYMBOL_SEGMENT_END) {
							DataPiece* piece = DataPieceArray_resize(&block->pieces, 1);
							if (!piece) {
								delCompilerCtx(&compctx);
								return (VBRBError){
									.prep = obj,
									.code = VBRB_ERR_NO_MEMORY
								};
							}
							VBRBError err = getDataPiece(&compctx, piece);
							if (err.code) return err;
						}
						fetchToken(obj);
					} else {
						DataPiece* piece = DataPieceArray_resize(&block->pieces, 1);
						if (!piece) {
							delCompilerCtx(&compctx);
							return (VBRBError){
								.prep = obj,
								.code = VBRB_ERR_NO_MEMORY
							};
						}
						VBRBError err = getDataPiece(&compctx, piece);
						if (err.code) return err;
					}
				}

				break;
			} case KW_MEMORY: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					delCompilerCtx(&compctx);
					return (VBRBError){
                        .prep = obj,
						.code = VBRB_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Token block_name, block_spec;
				while (true) {
					block_name = fetchToken(obj);
					if (getTokenSymbolId(block_name) == SYMBOL_SEGMENT_END) {
						break;
					} else if (block_name.type != TOKEN_STRING) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					if (memchr(block_name.string.data, '\0', block_name.string.length)) return (VBRBError){
						.prep = obj,
						.code = VBRB_ERR_INVALID_NAME,
						.loc = block_name
					};

					block_spec = fetchToken(obj);
					if (block_spec.type != TOKEN_INT) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_BLOCK_SIZE_EXPECTED,
							.loc = block_spec
						};
					}

					if (!MemBlockArray_append(
						&dst->memblocks,
						(MemBlock){
							.name = getTokenWord(obj, block_name),
							.size = block_spec.value
						}
					)) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_NO_MEMORY,
							.loc = block_spec
						};
					}
				}
				break;
			} case KW_EXEC: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					delCompilerCtx(&compctx);
					return (VBRBError){
                        .prep = obj,
						.code = VBRB_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Op* new_op;
				while (!BRPempty(obj)) {
					Token op_name = fetchToken(obj);
					if (getTokenSymbolId(op_name) == SYMBOL_SEGMENT_END) break; 

					if (!(new_op = OpArray_append(&dst->execblock, (Op){0}))) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_NO_MEMORY,
							.loc = segment_spec
						};
					}

					char kw_id = getTokenKeywordId(op_name);
					if (!inRange(kw_id, 0, N_OPS)) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_INVALID_OP,
							.loc = op_name
						};
					}

					new_op->type = kw_id;
					compctx.op_token = op_name;
					Token cond_spec = peekToken(obj);
					if (getTokenSymbolId(cond_spec) == SYMBOL_CONDITION_SPEC) {
						if (op_flags[new_op->type] & OPF_UNCONDITIONAL) return (VBRBError){
							.prep = compctx.prep,
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

					VBRBError err = op_compilers[kw_id](&compctx, dst);
					if (err.code != VBRB_ERR_OK) {
						delCompilerCtx(&compctx);
						return err;
					}
				}
				break;
			} case KW_LOAD: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					delCompilerCtx(&compctx);
					return (VBRBError){
                        .prep = obj,
						.code = VBRB_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				while (!BRPempty(obj)) {
					Token module_name_spec = fetchToken(obj);
					if (getTokenSymbolId(module_name_spec) == SYMBOL_SEGMENT_END) break;
					if (!isWordToken(module_name_spec)) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_INVALID_MODULE_NAME,
							.loc = module_name_spec
						};
					}

					char* module_name = getTokenWord(obj, module_name_spec);
					FILE* module_fd = findModule(module_name, search_paths);
					if (!module_fd) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_MODULE_NOT_FOUND,
							.loc = module_name_spec
						};
					}

					Module submodule;
					BRBLoadError err = preloadModule(module_fd, &submodule, search_paths);
					if (err.code) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_MODULE_NOT_LOADED,
							.loc = module_name_spec,
							.load_error = err
						};
					}
					mergeModule(&submodule, dst, module_name);
				}
				break;
			} default: {
				delCompilerCtx(&compctx);
				return (VBRBError){
                    .prep = obj,
					.code = VBRB_ERR_UNKNOWN_SEGMENT_SPEC,
					.loc = segment_spec
				};
			}
		}
	}
	OpArray_append(&dst->execblock, (Op){ .type = OP_END });
	
	resolveModule(dst, flags & BRB_EXECUTABLE);
	delCompilerCtx(&compctx);
	return (VBRBError){ .prep = obj };
}

void cleanupVBRBCompiler(VBRBError status)
{
    delBRP(status.prep);
}
