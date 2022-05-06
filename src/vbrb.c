#include "brb.h"

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
	N_VBRB_KWS
} VBRBKeyword;
static_assert(N_OPS == 56, "Some BRB operations have unmatched keywords");
static_assert(N_SYS_OPS == 8, "there might be system ops with unmatched keywords");

typedef struct {
	Token name;
	int32_t id;
} ExecMark;
declArray(ExecMark);
defArray(ExecMark);

void printVBRBError(FILE* dst, VBRBError err) {
	static_assert(N_VBRB_ERRORS == 27, "not all VBRB errors are handled");
	if (err.code != VBRB_ERR_OK) {
		if (err.code != VBRB_ERR_PREPROCESSOR_FAILURE) fprintTokenLoc(stderr, err.loc.loc, err.prep);
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
					getTokenTypeName(err.expected_token_type),
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
				fprintf(dst, "execution mark `%s` not found\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_DATA_BLOCK_NOT_FOUND:
				fprintf(dst, "data block `%s` not found\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_MEM_BLOCK_NOT_FOUND:
				fprintf(dst, "memory block `%s` not found\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_INVALID_REG_ID:
				fprintf(dst, "invalid register index %d\n", err.loc.word[1] - '0');
				break;
			case VBRB_ERR_UNKNOWN_CONST:
				fprintf(dst, "unknown constant `%s`\n", getTokenWord(err.prep, err.loc));
				break;
			case VBRB_ERR_INVALID_VAR_SIZE:
				fprintf(dst, "variable size can only be either 1, 2, 4 or 8, instead got %lld\n", err.loc.value);
				break;
			case VBRB_ERR_NON_PROC_CALL:
				fprintf(dst, "mark `%s` must point to the start of a procedure to be able to be called\n", err.mark_name);
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
            case VBRB_ERR_PREPROCESSOR_FAILURE:
                fprintf(dst, "preprocessor failed (reason: ");
                printBRPErrorStr(dst, err.prep);
                fprintf(dst, ")\n");
				break;
			case VBRB_ERR_NO_VAR:
				fprintf(dst, "cannot use `popv` operation; the stack is empty\n");
				break;
		}
	}
}

int64_t getBRBuiltinValue(char* name)
{
	for (int64_t i = 0; i < N_BUILTINS; i++) {
		if (streq(builtins[i].name, name)) return i;
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

VBRBError getRegIdArg(CompilerCtx* ctx, Token src, int8_t* dst, char op_type, char arg_id)
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
	*dst = src.word[1] - '0';
	if (!inRange(*dst, 0, N_USER_REGS)) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_REG_ID,
		.loc = src
	};
	return (VBRBError){ .prep = ctx->prep };
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

VBRBError getVarArg(CompilerCtx* ctx, Token src, int64_t* offset_p, uint8_t* var_size_p, char op_type, char arg_id)
{
	int64_t stub_offset;
	uint8_t stub_var_size;
	if (!offset_p) offset_p = &stub_offset;
	if (!var_size_p) var_size_p = &stub_var_size;

	if (!isWordToken(src)) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_WORD
	};
	
	*offset_p = 0;
	sbuf var_name = fromstr(getTokenWord(ctx->prep, src));
	for (int i = ctx->vars.length - 1; i >= 0; i--) {
		if (sbufeq(fromstr(ctx->vars.data[i].name), var_name)) { 
			var_name.data = NULL;
			*var_size_p = ctx->vars.data[i].size;
			break;
		}
		*offset_p += ctx->vars.data[i].size;
	}
	if (var_name.data) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_UNKNOWN_VAR_NAME,
		.loc = src
	};

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
	if (!isWordToken(name_spec)) {
		return (VBRBError){
            .prep = ctx->prep,
			.code = VBRB_ERR_INVALID_ARG,
			.loc = name_spec,
			.op_type = OP_MARK,
			.arg_id = 0,
			.expected_token_type = TOKEN_WORD
		};
	}
	ExecMarkArray_append(&ctx->proc_marks, (ExecMark){ .name = name_spec, .id = dst->execblock.length - 1 });

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileRegImmOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile2RegOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);
	
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;
	
	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetd(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	Token datablock_name_spec = fetchToken(ctx->prep);
	if (!isWordToken(datablock_name_spec)) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.op_type = OP_SETD,
		.arg_id = 1,
		.expected_token_type = TOKEN_WORD
	};
	op->mark_name = getTokenWord(ctx->prep, datablock_name_spec);

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetm(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	Token name_spec = fetchToken(ctx->prep);
	if (!isWordToken(name_spec)) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = name_spec,
		.op_type = OP_SETM,
		.arg_id = 1,
		.expected_token_type = TOKEN_WORD
	};
	op->mark_name = getTokenWord(ctx->prep, name_spec);

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetb(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

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
			.code = VBRB_ERR_UNKNOWN_CONST,
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
	if (!isWordToken(name_spec)) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = name_spec,
		.op_type = OP_GOTO,
		.arg_id = 0,
		.expected_token_type = TOKEN_WORD
	};
	ExecMarkArray_append(&ctx->proc_gotos, (ExecMark){ .name = name_spec, .id = dst->execblock.length - 1 });

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile2RegImmOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	err = getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 2);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compile3RegOp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);
	
	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src2_reg, op->type, 2);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpCall(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	Token name_spec = fetchToken(ctx->prep);
	if (!isWordToken(name_spec)) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = name_spec,
		.op_type = OP_CALL,
		.arg_id = 0,
		.expected_token_type = TOKEN_WORD
	};
	op->mark_name = getTokenWord(ctx->prep, name_spec);

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpCmp(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getIntArg(ctx, fetchToken(ctx->prep), &op->value, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpCmpr(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src2_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

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
	if (!isWordToken(name_spec)) return (VBRBError){
        .prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = name_spec,
		.op_type = op->type,
		.arg_id = 0,
		.expected_token_type = TOKEN_WORD
	};
	op->mark_name = getTokenWord(ctx->prep, name_spec);

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpEndproc(CompilerCtx* ctx, Module* dst)
{
// resolving local marks and `goto`s
	for (int goto_index = 0; goto_index < ctx->proc_gotos.length; goto_index++) {
		ExecMark* dst_mark = ctx->proc_gotos.data + goto_index;
		for (int mark_index = 0; mark_index < ctx->proc_marks.length; mark_index++) {
			ExecMark* src_mark = ctx->proc_marks.data + mark_index;
			if (streq(getTokenWord(ctx->prep, src_mark->name), getTokenWord(ctx->prep, dst_mark->name))) {
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

	if (!isWordToken(token)) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = token,
		.op_type = op->type,
		.arg_id = 0,
		.expected_token_type = TOKEN_WORD
	};
	new_var->name = getTokenWord(ctx->prep, token);

	token = fetchToken(ctx->prep);
	if (token.type != TOKEN_INT) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = token,
		.op_type = op->type,
		.arg_id = 1,
		.expected_token_type = TOKEN_INT
	};
	if (token.value != 1 && token.value != 2 && token.value != 4 && token.value != 8) {
		return (VBRBError){
            .prep = ctx->prep,
			.code = VBRB_ERR_INVALID_VAR_SIZE,
			.loc = token
		};
	}
	op->var_size = new_var->size = (int8_t)token.value;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpSetv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code) return err;

	err = getVarArg(ctx, fetchToken(ctx->prep), &op->symbol_id, NULL, op->type, 1);
	if (err.code) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpLdv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code) return err;

	err = getVarArg(ctx, fetchToken(ctx->prep), &op->symbol_id, &op->var_size, op->type, 1);
	if (err.code) return err;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpStrv(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getVarArg(ctx, fetchToken(ctx->prep), &op->symbol_id, &op->var_size, op->type, 1);
	if (err.code) return err;

	err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src_reg, op->type, 0);
	if (err.code) return err;

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
	op->var_size = VarArray_get(ctx->vars, -1).size;
	ctx->vars.length--;

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->dst_reg, op->type, 0);
	if (err.code) return err;

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

	if (!isWordToken(token)) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = token,
		.op_type = op->type,
		.arg_id = 0,
		.expected_token_type = TOKEN_WORD
	};
	new_var->name = getTokenWord(ctx->prep, token);

	token = fetchToken(ctx->prep);
	if (token.type != TOKEN_INT) return (VBRBError){
		.prep = ctx->prep,
		.code = VBRB_ERR_INVALID_ARG,
		.loc = token,
		.op_type = op->type,
		.arg_id = 1,
		.expected_token_type = TOKEN_INT
	};
	if (token.value != 1 && token.value != 2 && token.value != 4 && token.value != 8) {
		return (VBRBError){
            .prep = ctx->prep,
			.code = VBRB_ERR_INVALID_VAR_SIZE,
			.loc = token
		};
	}
	op->var_size = new_var->size = (int8_t)token.value;

	VBRBError err = getRegIdArg(ctx, fetchToken(ctx->prep), &op->src_reg, op->type, 2);
	if (err.code) return err;

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
	op->mark_name = src_path.word;

	return (VBRBError){ .prep = ctx->prep };
}

VBRBError compileOpAtl(CompilerCtx* ctx, Module* dst)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getIntArg(ctx, fetchToken(ctx->prep), (uint64_t*)&op->symbol_id, op->type, 0);
	if (err.code) return err;

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
	[OP_ATL] = &compileOpAtl
};
static_assert(N_OPS == sizeof(op_compilers) / sizeof(op_compilers[0]), "Some BRB operations have unmatched compilers");

VBRBError compileModule(FILE* src, char* src_name, Module* dst, char* search_paths[], int flags)
{
    BRP* obj = malloc(sizeof(BRP));
    initBRP(obj);
	setSymbols(
		obj,
		BRP_SYMBOL("{"),
		BRP_SYMBOL("}"),
		BRP_SYMBOL(":"),
		BRP_HIDDEN_SYMBOL(" "),
		BRP_HIDDEN_SYMBOL("\t")
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
		BRP_KEYWORD("load")
	);
	
	if (!setInputFrom(obj, src_name, src)) return (VBRBError){
        .prep = obj,
        .code = VBRB_ERR_PREPROCESSOR_FAILURE,
    };

	dst->entry_opid = -1;
	dst->execblock = OpArray_new(0);
	dst->memblocks = MemBlockArray_new(0);
	dst->datablocks = DataBlockArray_new(0);
	dst->submodules = strArray_new(0);
	dst->stack_size = DEFAULT_STACK_SIZE;
	dst->_root_db_start = 0;
	dst->_root_eb_start = 0;
	dst->_root_mb_start = 0;

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
				while (!BRPempty(obj)) {
					block_name = fetchToken(obj);
					if (getTokenSymbolId(block_name) == SYMBOL_SEGMENT_END) {
						break;
					} else if (!isWordToken(block_name)) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					block_spec = fetchToken(obj);
					if (block_spec.type != TOKEN_STRING) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_BLOCK_SPEC_EXPECTED,
							.loc = block_spec
						};
					}

					if (!DataBlockArray_append(
						&dst->datablocks,
						(DataBlock){
							.name = getTokenWord(obj, block_name),
							.spec = sbufunesc(fromstr(block_spec.word))
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
				while (!BRPempty(obj)) {
					block_name = fetchToken(obj);
					if (getTokenSymbolId(block_name) == SYMBOL_SEGMENT_END) {
						break;
					} else if (!isWordToken(block_name)) {
						delCompilerCtx(&compctx);
						return (VBRBError){
                            .prep = obj,
							.code = VBRB_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

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

					FILE* module_fd = findModule(getTokenWord(obj, module_name_spec), search_paths);
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
					mergeModule(&submodule, dst);
					strArray_append(&dst->submodules, getTokenWord(obj, module_name_spec));
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
