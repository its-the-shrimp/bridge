#include "brb.h"
#include "errno.h"

defArray(Op);
defArray(DataBlock);
defArray(MemBlock);
declArray(Var);
defArray(Var);

typedef enum {
	SYMBOL_SEGMENT_START,
	SYMBOL_SEGMENT_END,
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
	KW_CGOTO,
	KW_EQ,
	KW_EQR,	
	KW_NEQ,
	KW_NEQR,
	KW_LT,
	KW_LTR,
	KW_GT,
	KW_GTR,
	KW_LE,
	KW_LER,
	KW_GE,
	KW_GER,
	KW_LTS,
	KW_LTSR,
	KW_GTS,
	KW_GTSR,
	KW_LES,
	KW_LESR,
	KW_GES,
	KW_GESR,
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
	KW_SYS_NONE,
	KW_SYS_EXIT,
	KW_SYS_WRITE,
	KW_SYS_ARGC,
	KW_SYS_ARGV,
	KW_SYS_READ,
	KW_SYS_GET_ERRNO,
	KW_SYS_SET_ERRNO,
	KW_ENTRY,
	KW_STACKSIZE,
	KW_EXEC,
	KW_DATA,
	KW_MEMORY,
	N_VBRB_KWS
} VBRBKeyword;
static_assert(N_OPS == 68, "Some BRB operations have unmatched keywords");
static_assert(N_SYS_OPS == 8, "there might be system ops with unmatched keywords");

bool minimal = false;

// special value for error reporting
#define TOKEN_REG_ID 125

void writeInt(FILE* fd, int64_t x)
{
	if (x) {
		if (inRange(x, INT8_MIN, INT8_MAX)) {
			fputc(1, fd);
			int8_t x8 = (int8_t)x;
			fputc(x8, fd);
		} else if (inRange(x, INT16_MIN, INT16_MAX)) {
			fputc(2, fd);
			int16_t x16 = (int16_t)x;
			fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
		} else if (inRange(x, INT32_MIN, INT32_MAX)) {
			fputc(4, fd);
			int32_t x32 = (int32_t)x;
			fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
		} else {
			fputc(8, fd);
			fwrite(BRByteOrder(&x, 8), 8, 1, fd);
		}
	} else {
		fputc(0, fd);
	}
}

bool startDataSegment(FILE* fd)
{
	return fputsbuf(fd, DATA_SEGMENT_START) > 0;
}

bool writeDataBlock(FILE* fd, char* name, sbuf obj)
{
	sbuf input_name = fromstr(name);
	fputsbuf(fd, minimal ? CSTRTERM : input_name);
	fputsbuf(fd, SEP);
	writeInt(fd, obj.length + 1);
	fputsbuf(fd, obj);
	fputc('\0', fd);
	return true;
}

bool endMemorySegment(FILE* fd)
{
	return fputsbuf(fd, SEP) > 0;
}

bool endDataSegment(FILE* fd)
{
	return fputsbuf(fd, SEP) > 0;
}

bool endExecSegment(FILE* fd)
{
	return fputc(OP_END, fd) > 0;
}

bool startMemorySegment(FILE* fd)
{
	return fputsbuf(fd, MEMBLOCK_SEGMENT_START) > 0;
}

bool writeMemoryBlock(FILE* fd, char* name, int32_t size)
{
	sbuf input_name = fromstr(name);
	fputsbuf(fd, minimal ? CSTRTERM : input_name);
	fputsbuf(fd, SEP);
	writeInt(fd, size);
	return true;
}

bool startExecSegment(FILE* fd)
{
	return fputsbuf(fd, EXEC_SEGMENT_START) > 0;
}

typedef void (*OpWriter) (FILE*, Op);

void writeNoArgOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
}

void writeMarkOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputsbuf(fd, fromstr(op.mark_name));
	fputsbuf(fd, SEP);
}

void writeRegImmOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	writeInt(fd, op.value);
}

void write2RegOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
}

void writeRegSymbolIdOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	writeInt(fd, op.symbol_id);
}

void writeOpSyscall(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.syscall_id, fd);
}

void writeJumpOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	writeInt(fd, op.symbol_id);
}

void writeOpCgoto(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.src_reg, fd);
	writeInt(fd, op.symbol_id);
}

void write2RegImmOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	writeInt(fd, op.value);
}

void write3RegOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writeOpVar(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.var_size, fd);
}

OpWriter op_writers[] = {
	&writeNoArgOp, // OP_NONE
	&writeNoArgOp, // OP_END
	&writeMarkOp, // OP_MARK
	&writeRegImmOp, // OP_SET
	&write2RegOp, // OP_SETR
	&writeRegSymbolIdOp, // OP_SETD
	&writeRegSymbolIdOp, // OP_SETB
	&writeRegSymbolIdOp, // OP_SETM
	&write2RegImmOp, // OP_ADD
	&write3RegOp, // OP_ADDR
	&write2RegImmOp, // OP_SUB
	&write3RegOp, // OP_SUBR
	&writeOpSyscall,
	&writeJumpOp, // OP_GOTO
	&writeOpCgoto,
	&write2RegImmOp, // OP_EQ
	&write3RegOp, // OP_EQR
	&write2RegImmOp, // OP_NEQ
	&write3RegOp, // OP_NEQR
	&write2RegImmOp, // OP_LT
	&write3RegOp, // OP_LTR
	&write2RegImmOp, // OP_GT
	&write3RegOp, // OP_GTR
	&write2RegImmOp, // OP_LE
	&write3RegOp, // OP_LER
	&write2RegImmOp, // OP_GE
	&write3RegOp, // OP_GER
	&write2RegImmOp, // OP_LTS
	&write3RegOp, // OP_LTSR
	&write2RegImmOp, // OP_GTS
	&write3RegOp, // OP_GTSR
	&write2RegImmOp, // OP_LES
	&write3RegOp, // OP_LESR
	&write2RegImmOp, // OP_GES
	&write3RegOp, // OP_GESR
	&write2RegImmOp, // OP_AND
	&write3RegOp, // OP_ANDR
	&write2RegImmOp, // OP_OR
	&write3RegOp, // OP_ORR
	&write2RegOp, // OP_NOT
	&write2RegImmOp, // OP_XOR
	&write3RegOp, // OP_XORR
	&write2RegImmOp, // OP_SHL
	&write3RegOp, // OP_SHLR
	&write2RegImmOp, // OP_SHR
	&write3RegOp, // OP_SHRR
	&write2RegImmOp, // OP_SHRS
	&write3RegOp, // OP_SHRSR
	&writeMarkOp, // OP_PROC
	&writeJumpOp, // OP_CALL
	&writeNoArgOp, // OP_RET
	&writeNoArgOp, // OP_ENDPROC
	&write2RegOp, // OP_LD64
	&write2RegOp, // OP_STR64
	&write2RegOp, // OP_LD32
	&write2RegOp, // OP_STR32
	&write2RegOp, // OP_LD16
	&write2RegOp, // OP_STR16
	&write2RegOp, // OP_LD8
	&write2RegOp, // OP_STR8
	&writeOpVar,
	&writeRegSymbolIdOp, // OP_SETV
	&write2RegImmOp, // OP_MUL
	&write3RegOp, // OP_MULR
	&write2RegImmOp, // OP_DIV
	&write3RegOp, // OP_DIVR
	&write2RegImmOp, // OP_DIVS
	&write3RegOp // OP_DIVSR
};
static_assert(N_OPS == sizeof(op_writers) / sizeof(op_writers[0]), "Some BRB operations have unmatched writers");

bool setEntryPoint(FILE* fd, int64_t mark_id)
{
	fputsbuf(fd, ENTRYSPEC_SEGMENT_START);
	writeInt(fd, mark_id);
	return true;
}

bool setStackSize(FILE* fd, int64_t stack_size)
{
	fputsbuf(fd, STACKSIZE_SEGMENT_START);
    writeInt(fd, stack_size);
	return true;
}

typedef enum {
	VBRB_ERR_OK,
	VBRB_ERR_BLOCK_NAME_EXPECTED,
	VBRB_ERR_ENTRY_NAME_EXPECTED,
	VBRB_ERR_STACK_SIZE_EXPECTED,
	VBRB_ERR_NO_MEMORY,
	VBRB_ERR_SEGMENT_START_EXPECTED,
	VBRB_ERR_BLOCK_SPEC_EXPECTED,
	VBRB_ERR_BLOCK_SIZE_EXPECTED,
	VBRB_ERR_UNKNOWN_SEGMENT_SPEC,
	VBRB_ERR_UNCLOSED_SEGMENT,
	VBRB_ERR_INVALID_ARG,
	VBRB_ERR_INVALID_OP,
	VBRB_ERR_UNKNOWN_SYSCALL,
	VBRB_ERR_EXEC_MARK_NOT_FOUND,
	VBRB_ERR_DATA_BLOCK_NOT_FOUND,
	VBRB_ERR_MEM_BLOCK_NOT_FOUND,
	VBRB_ERR_INVALID_REG_ID,
	VBRB_ERR_INVALID_VAR_SIZE,
	VBRB_ERR_UNKNOWN_CONST,
	VBRB_ERR_NON_PROC_ENTRY,
	VBRB_ERR_NON_PROC_CALL,
	VBRB_ERR_UNKNOWN_VAR_ID,
	VBRB_ERR_UNCLOSED_PROC,
	N_VBRB_ERRORS
} VBRBErrorCode;

typedef struct vbrb_error {
	VBRBErrorCode code;
	Token loc;
	union {
		struct { // for VBRB_ERR_INVALID_ARG
			int8_t arg_id;
			uint8_t op_type;
			uint8_t expected_token_type;
		};
		int64_t item_size;
		char* mark_name;
	};
} VBRBError;

typedef struct {
	Token name;
	int32_t id;
} ExecMark;
declArray(ExecMark);
defArray(ExecMark);

int64_t getBRBuiltinValue(char* name)
{
	for (int64_t i = 0; i < sizeof(consts); i++) {
		if (streq(consts[i].name, name)) return i;
	}
	return -1;
} 

VBRBError getRegIdArg(Token src, int8_t* dst, char op_type, char arg_id)
{
	if (src.type != TOKEN_WORD) return (VBRBError){
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_REG_ID
	};
	if (strlen(src.word) != 2 || src.word[0] != 'r') return (VBRBError){
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_REG_ID
	};
	*dst = src.word[1] - '0';
	if (!inRange(*dst, 0, N_REGISTERS)) return (VBRBError){
		.code = VBRB_ERR_INVALID_REG_ID,
		.loc = src
	};
	return (VBRBError){0};
}

VBRBError getIntArg(Token src, int64_t* dst, char op_type, char arg_id)
{
	if (src.type != TOKEN_INT) return (VBRBError){
		.code = VBRB_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_INT
	};
	*dst = src.value;
	return (VBRBError){0};
}

VBRBError getExecMarkArg(Token src, int id, ExecMarkArray* dst, char op_type, char arg_id)
{
	if (!isWordToken(src)) {
		return (VBRBError){
			.code = VBRB_ERR_INVALID_ARG,
			.loc = src,
			.arg_id = arg_id,
			.op_type = op_type,
			.expected_token_type = TOKEN_WORD
		};
	}
	if (!ExecMarkArray_append(dst, (ExecMark){ .name = src, .id = id })) {
		return (VBRBError){
			.code = VBRB_ERR_NO_MEMORY,
			.loc = src
		};
	}
	return (VBRBError){0};
}

typedef struct {
	ExecMarkArray data_unresolved;
	ExecMarkArray mem_unresolved;
	ExecMarkArray exec_unresolved;
	ExecMarkArray marks;
	VarArray vars;
	bool in_proc;
	Token op_token;
} CompilerCtx;
typedef VBRBError (*OpCompiler) (Preprocessor*, Program*, CompilerCtx*);

VBRBError compileNoArgOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	return (VBRBError){0};
}

VBRBError compileMarkOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->marks, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	op->mark_name = getTokenWord(obj, arrayhead(ctx->marks)->name);

	return (VBRBError){0};
}

VBRBError compileRegImmOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getIntArg(fetchToken(obj), &op->value, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){0};
}

VBRBError compile2RegOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	
	VBRBError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;
	
	return (VBRBError){0};
}

VBRBError compileOpSetd(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->data_unresolved, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){0};
}

VBRBError compileOpSetm(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->mem_unresolved, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){0};
}

VBRBError compileOpSetb(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	Token arg = fetchToken(obj);
	if (!isWordToken(arg)) {
		return (VBRBError){
			.code = VBRB_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = op->type,
			.expected_token_type = TOKEN_WORD
		};
	}
	op->symbol_id = getBRBuiltinValue(getTokenWord(obj, arg));
	if (op->symbol_id == -1) {
		return (VBRBError){
			.code = VBRB_ERR_UNKNOWN_CONST,
			.loc = arg
		};
	}

	return (VBRBError){0};
}

VBRBError compileOpSyscall(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	Token arg = fetchToken(obj);
	if (
		arg.type != TOKEN_KEYWORD || 
		!inRange(arg.keyword_id, N_OPS, N_OPS + N_SYS_OPS)
	) {
		return (VBRBError){
			.code = VBRB_ERR_UNKNOWN_SYSCALL,
			.loc = arg
		};
	}
	op->syscall_id = arg.keyword_id - N_OPS;
							
	return (VBRBError){0};
}

VBRBError compile2RegImmOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	err = getIntArg(fetchToken(obj), &op->value, op->type, 2);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){0};
}

VBRBError compile3RegOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	
	VBRBError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	err = getRegIdArg(fetchToken(obj), &op->src2_reg, op->type, 2);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){0};
}

VBRBError compileJumpOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->exec_unresolved, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	return (VBRBError){0};
}

VBRBError compileOpCgoto(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 0);
	if (err.code != VBRB_ERR_OK) return err;
	
	err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->exec_unresolved, op->type, 1);
	if (err.code != VBRB_ERR_OK) return err;

	return (VBRBError){0};
}

VBRBError compileOpProc(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	if (ctx->in_proc) {
		return (VBRBError){
			.code = VBRB_ERR_UNCLOSED_PROC,
			.loc = ctx->op_token
		};
	}
	ctx->in_proc = true;
	return compileMarkOp(obj, dst, ctx);
}

VBRBError compileOpEndproc(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	VarArray_clear(&ctx->vars);
	ctx->in_proc = false;
	return (VBRBError){0};
}

VBRBError compileOpVar(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	Token token = fetchToken(obj);
	if (!VarArray_append(&ctx->vars, (Var){0})) {
		return (VBRBError){
			.code = VBRB_ERR_NO_MEMORY,
			.loc = token
		};
	}
	if (token.type != TOKEN_INT) {
		return (VBRBError){
			.code = VBRB_ERR_INVALID_ARG,
			.loc = token,
			.op_type = op->type,
			.arg_id = 1
		};
	}
	if (token.value != 1 && token.value != 2 && token.value != 4 && token.value != 8) {
		return (VBRBError){
			.code = VBRB_ERR_INVALID_VAR_SIZE,
			.loc = token
		};
	}
	op->var_size = arrayhead(ctx->vars)->size = (int8_t)token.value;

	return (VBRBError){0};
}

VBRBError compileOpSetv(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRBError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code) { return err; }

	Token var_id_spec = fetchToken(obj);
	if (var_id_spec.type != TOKEN_INT) {
		return (VBRBError){
			.code = VBRB_ERR_INVALID_ARG,
			.loc = var_id_spec,
			.op_type = op->type,
			.arg_id = 0
		};
	}
	if (var_id_spec.value >= ctx->vars.length) {
		return (VBRBError){
			.code = VBRB_ERR_UNKNOWN_VAR_ID,
			.loc = var_id_spec
		};
	}

	op->symbol_id = 0;
	for (int i = ctx->vars.length - 1; i >= 0; i--) {
		if (i == var_id_spec.value) { break; }
		op->symbol_id += ctx->vars.data[i].size;
	}

	return (VBRBError){0};
}

OpCompiler op_compilers[] = {
	&compileNoArgOp, // OP_NONE
	&compileNoArgOp, // OP_END
	&compileMarkOp, //OP_MARK
	&compileRegImmOp, // OP_SET
	&compile2RegOp, // OP_SETR
	&compileOpSetd,
	&compileOpSetb,
	&compileOpSetm,
	&compile2RegImmOp, // OP_ADD
	&compile3RegOp, // OP_ADDR
	&compile2RegImmOp, // OP_SUB
	&compile3RegOp, // OP_SUBR
	&compileOpSyscall, // OP_SYSCALL
	&compileJumpOp, // OP_GOTO
	&compileOpCgoto,
	&compile2RegImmOp, // OP_EQ
	&compile3RegOp, // OP_EQR
	&compile2RegImmOp, // OP_NEQ
	&compile3RegOp, // OP_NEQR
	&compile2RegImmOp, // OP_LT
	&compile3RegOp, // OP_LTR
	&compile2RegImmOp, // OP_GT
	&compile3RegOp, // OP_GTR
	&compile2RegImmOp, // OP_LE
	&compile3RegOp, // OP_LER
	&compile2RegImmOp, // OP_GE
	&compile3RegOp, // OP_GER
	&compile2RegImmOp, // OP_LTS
	&compile3RegOp, // OP_LTSR
	&compile2RegImmOp, // OP_GTS
	&compile3RegOp, // OP_GTSR
	&compile2RegImmOp, // OP_LES
	&compile3RegOp, // OP_LESR
	&compile2RegImmOp, // OP_GES
	&compile3RegOp, // OP_GESR
	&compile2RegImmOp, // OP_AND
	&compile3RegOp, // OP_ANDR
	&compile2RegImmOp, // OP_OR
	&compile3RegOp, // OP_ORR
	&compile2RegOp, // OP_NOT
	&compile2RegImmOp, // OP_XOR
	&compile3RegOp, // OP_XORR
	&compile2RegImmOp, // OP_SHL
	&compile3RegOp, // OP_SHLR
	&compile2RegImmOp, // OP_SHR
	&compile3RegOp, // OP_SHRR
	&compile2RegImmOp, // OP_SHRS
	&compile3RegOp, // OP_SHRSR
	&compileOpProc,
	&compileJumpOp, // OP_CALL
	&compileNoArgOp, // OP_RET
	&compileOpEndproc, // OP_ENDPROC
	&compile2RegOp, // OP_LD64
	&compile2RegOp, // OP_STR64
	&compile2RegOp, // OP_LD32
	&compile2RegOp, // OP_STR32
	&compile2RegOp, // OP_LD16
	&compile2RegOp, // OP_STR16
	&compile2RegOp, // OP_LD8
	&compile2RegOp, // OP_STR8
	&compileOpVar,
	&compileOpSetv,
	&compile2RegImmOp, // OP_MUL
	&compile3RegOp, // OP_MULR
	&compile2RegImmOp, // OP_DIV
	&compile3RegOp, // OP_DIVR
	&compile2RegImmOp, // OP_DIVS
	&compile3RegOp // OP_DIVSR
};
static_assert(N_OPS == sizeof(op_compilers) / sizeof(op_compilers[0]), "Some BRB operations have unmatched compilers");

VBRBError compileSourceCode(Preprocessor* obj, Program* dst, heapctx_t ctx)
{
	enter_tempctx(funcctx, 0);
	dst->entry_opid = 0;
	dst->execblock = OpArray_new(ctx, -1);
	dst->memblocks = MemBlockArray_new(ctx, 0);
	dst->datablocks = DataBlockArray_new(ctx, 0);
	dst->stack_size = DEFAULT_STACK_SIZE;

	CompilerCtx compctx = {
		.exec_unresolved = ExecMarkArray_new(TEMP_CTX, 0),
		.mem_unresolved = ExecMarkArray_new(TEMP_CTX, 0),
		.data_unresolved = ExecMarkArray_new(TEMP_CTX, 0),
		.marks = ExecMarkArray_new(TEMP_CTX, -1),
		.vars = VarArray_new(TEMP_CTX, 0)
	};
	Token entry_name = {0};

	while (!isPrepEOF(obj)) {
		Token segment_spec = fetchToken(obj);
		if (segment_spec.type == TOKEN_NONE) { break; }
		switch (getTokenKeywordId(segment_spec)) {
			case KW_ENTRY: {
				Token entry_symbol = fetchToken(obj);
				if (entry_symbol.type == TOKEN_INT) {
					dst->entry_opid = entry_symbol.value;
				} else if (isWordToken(entry_symbol)) {
					entry_name = entry_symbol;
				} else {
					exit_tempctx(funcctx);
					return (VBRBError){
						.code = VBRB_ERR_ENTRY_NAME_EXPECTED,
						.loc = entry_symbol
					};
				}
				break;
			} case KW_STACKSIZE: {
				Token size_spec = fetchToken(obj);
				if (size_spec.type != TOKEN_INT) {
					exit_tempctx(funcctx);
					return (VBRBError){
						.code = VBRB_ERR_STACK_SIZE_EXPECTED,
						.loc = size_spec
					};
				}

				dst->stack_size = size_spec.value * 1024;
				break;
			} case KW_DATA: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRBError){
						.code = VBRB_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Token block_name, block_spec;
				while (!isPrepEOF(obj)) {
					block_name = fetchToken(obj);
					if (getTokenSymbolId(block_name) == SYMBOL_SEGMENT_END) {
						break;
					} else if (!isWordToken(block_name)) {
						exit_tempctx(funcctx);
						return (VBRBError){
							.code = VBRB_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					block_spec = fetchToken(obj);
					if (block_spec.type != TOKEN_STRING) {
						exit_tempctx(funcctx);
						return (VBRBError){
							.code = VBRB_ERR_BLOCK_SPEC_EXPECTED,
							.loc = block_spec
						};
					}

					if (!DataBlockArray_append(
						&dst->datablocks,
						(DataBlock){
							.name = getTokenWord(obj, block_name),
							.spec = sbufunesc(fromstr(block_spec.word), ctx)
						}
					)) {
						exit_tempctx(funcctx);
						return (VBRBError){
							.code = VBRB_ERR_NO_MEMORY,
							.loc = block_spec
						};
					}
				}

				break;
			} case KW_MEMORY: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRBError){
						.code = VBRB_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Token block_name, block_spec;
				while (!isPrepEOF(obj)) {
					block_name = fetchToken(obj);
					if (getTokenSymbolId(block_name) == SYMBOL_SEGMENT_END) {
						break;
					} else if (!isWordToken(block_name)) {
						exit_tempctx(funcctx);
						return (VBRBError){
							.code = VBRB_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					block_spec = fetchToken(obj);
					if (block_spec.type != TOKEN_INT) {
						exit_tempctx(funcctx);
						return (VBRBError){
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
						exit_tempctx(funcctx);
						return (VBRBError){
							.code = VBRB_ERR_NO_MEMORY,
							.loc = block_spec
						};
					}
				}
				break;
			} case KW_EXEC: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRBError){
						.code = VBRB_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Op* new_op;
				while (!isPrepEOF(obj)) {
					Token op_name = fetchToken(obj);
					if (getTokenSymbolId(op_name) == SYMBOL_SEGMENT_END) break; 

					if (!(new_op = OpArray_append(&dst->execblock, (Op){0}))) {
						exit_tempctx(funcctx);
						return (VBRBError){
							.code = VBRB_ERR_NO_MEMORY,
							.loc = segment_spec
						};
					}

					char kw_id = getTokenKeywordId(op_name);
					if (!inRange(kw_id, 0, N_OPS)) {
						exit_tempctx(funcctx);
						return (VBRBError){
							.code = VBRB_ERR_INVALID_OP,
							.loc = op_name
						};
					}

					new_op->type = kw_id;
					compctx.op_token = op_name;
					VBRBError err = op_compilers[kw_id](obj, dst, &compctx);
					if (err.code != VBRB_ERR_OK) {
						exit_tempctx(funcctx);
						return err;
					}
				}
				break;
			} default: {
				exit_tempctx(funcctx);
				return (VBRBError){
					.code = VBRB_ERR_UNKNOWN_SEGMENT_SPEC,
					.loc = segment_spec
				};
			}
		}
	}
	
	// resolving entry mark
	if (entry_name.type != TOKEN_NONE) {
		bool resolved = false;
		array_foreach(ExecMark, mark, compctx.marks,
			if (streq(getTokenWord(obj, mark.name), entry_name.word)) {
				dst->entry_opid = mark.id; 
				if (dst->execblock.data[mark.id].type != OP_PROC) {
					exit_tempctx(funcctx);
					return ((VBRBError){
						.code = VBRB_ERR_NON_PROC_ENTRY,
						.loc = entry_name,
						.mark_name = getTokenWord(obj, mark.name)
					});
				}
				resolved = true;
				break;
			}
		);
		if (!resolved) {
			exit_tempctx(funcctx);
			return (VBRBError){
				.code = VBRB_ERR_EXEC_MARK_NOT_FOUND,
				.loc = entry_name
			};
		}
	}
	// resolving references to data blocks
	bool resolved = false;
	array_foreach(ExecMark, unresolved, compctx.data_unresolved, {
		array_foreach(DataBlock, block, dst->datablocks,
			if (streq(block.name, getTokenWord(obj, unresolved.name))) {
				dst->execblock.data[unresolved.id].symbol_id = _block;
				resolved = true;
				break;
			}
		);
		if (!resolved) {
			exit_tempctx(funcctx);
			return ((VBRBError){
				.code = VBRB_ERR_DATA_BLOCK_NOT_FOUND,
				.loc = unresolved.name
			});
		}
		resolved = false;
	});
	// resolving references to memory blocks
	array_foreach(ExecMark, unresolved, compctx.mem_unresolved,
		bool resolved = false;
		array_foreach(MemBlock, block, dst->memblocks,
			if (streq(block.name, getTokenWord(obj, unresolved.name))) {
				dst->execblock.data[unresolved.id].symbol_id = _block;
				resolved = true;
				break;
			}
		);
		if (!resolved) {
			exit_tempctx(funcctx);
			return ((VBRBError){
				.code = VBRB_ERR_MEM_BLOCK_NOT_FOUND,
				.loc = unresolved.name
			});
		}
	);
	// resolving code jumps
	array_foreach(ExecMark, unresolved, compctx.exec_unresolved, {
		array_foreach(ExecMark, mark, compctx.marks,
			if (streq(getTokenWord(obj, mark.name), getTokenWord(obj, unresolved.name))) {
				dst->execblock.data[unresolved.id].symbol_id = mark.id;
				if (dst->execblock.data[mark.id].type != OP_PROC && dst->execblock.data[unresolved.id].type == OP_CALL) {
					exit_tempctx(funcctx);
					return ((VBRBError){
						.code = VBRB_ERR_NON_PROC_CALL,
						.loc = unresolved.name,
						.mark_name = getTokenWord(obj, mark.name)
					});
				}
				resolved = true;
				break;
			}
		);
		if (!resolved) {
			exit_tempctx(funcctx);
			return ((VBRBError){
				.code = VBRB_ERR_EXEC_MARK_NOT_FOUND,
				.loc = unresolved.name
			});
		}
		resolved = false;
	});
	
	exit_tempctx(funcctx);
	return (VBRBError){ .code = VBRB_ERR_OK };
}

char* getTokenTypeName(TokenType token_type)
{
	if (token_type == TOKEN_REG_ID) return "register";
	return getTokenTypeName(token_type);
}

void printUsageMsg(FILE* fd, char* execname)
{
	fprintf(fd, "brs - Compiles `.vbrb` (Visual BRidge Bytecode) to interpetable `.brb` files (BRidge Bytecode)");
	fprintf(fd, "usage: %s [options] <file>\n", execname);
	fprintf(fd, "options:\n");
	fprintf(fd, "\t-h           Output this message and exit\n");
	fprintf(fd, "\t-o <file>    The output will be saved to <file>\n");
	fprintf(fd, "\t-m           Minimize the size of compiled program by removing all names from it. Not recommended when debugging\n");
	fprintf(fd, "\t-n <file>    Compile source directly to a native executable, which will be saved to <file>\n");
	fprintf(fd, "\t-N           The same as `-n` option, but the resulting executable will have the same name as the input file\n");
}

int main(int argc, char* argv[])
{
	initBREnv();
	startTimer();

	bool go_on = false;
	char *input_path = NULL, *output_path = NULL, *exec_output_path = NULL;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			for (argv[i]++; *argv[i]; argv[i]++) {
				if (go_on) { go_on = false; break; }
				switch (*argv[i]) {
					case 'h': printUsageMsg(stdout, argv[0]); return 0;
					case 'o':
						if (!argv[++i]) {
							eprintf("error: `-o` option specified but no output file path provided\n");
							return 1;
						}
						output_path = argv[i];
						go_on = true;
						break;
					case 'm': minimal = true; break;
					case 'n':
						if (!argv[++i]) {
							eprintf("error: `-n` option specified but no executable output file path provided\n");
							return 1;
						}
						exec_output_path = argv[i];
						go_on = true;
						break;
					case 'N': exec_output_path = (void*)1; break;
					default: eprintf("error: unknown option `-%c`\n", *argv[i]); return 1;
				}
			}
		} else {
			if (input_path) {
				eprintf("error: got more than one input paths\n");
			}
			input_path = argv[i];
		}
	}

	if (!input_path) {
		fprintf(stderr, "error: no input path provided\n");
		return 1;
	}

	sbuf input_path_sbuf = fromstr(input_path), basename = {0};
	if (!output_path) {
		sbufsplit(&input_path_sbuf, &basename, fromcstr("."));
		output_path = tostr(GLOBAL_CTX, basename, fromcstr(".brb"));
	}
	if (exec_output_path == (void*)1) {
		exec_output_path = tostr(GLOBAL_CTX, basename);
	}
	
	sbuf delims[] = {
		fromcstr("{"),
		fromcstr("}"),
		SPACE,
		NEWLINE,
		TAB,
		(sbuf){0}
	};
	sbuf kws[] = { 
		_opNames,
		_syscallNames,
		fromcstr("entry"), 
		fromcstr("stacksize"),
		fromcstr("exec"),
		fromcstr("data"),
		fromcstr("memory"),
		(sbuf){0}
	};

	Preprocessor prep = newPreprocessor(delims, kws, GLOBAL_CTX);
	if (!setInput(&prep, input_path)) {
		printPrepError(stderr, &prep);
		return 1;
	}

	Program res;
	VBRBError err = compileSourceCode(&prep, &res, ctxalloc_newctx(0));

	static_assert(N_VBRB_ERRORS == 23, "not all VBRB errors are handled");
	if (err.code != VBRB_ERR_OK) {
		fprintTokenLoc(stderr, err.loc.loc, &prep);
		eprintf("error: ");
		switch (err.code) {
			case N_VBRB_ERRORS: break;
			case VBRB_ERR_OK: break;
			case VBRB_ERR_ENTRY_NAME_EXPECTED:
				eprintf("expected a word or integer as the entry mark, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_BLOCK_NAME_EXPECTED:
				eprintf("expected a word as the block name, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_STACK_SIZE_EXPECTED:
				eprintf("expected integer as the stack size specifier, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_NO_MEMORY:
				eprintf("memory allocation failure\n");
				return 1;
			case VBRB_ERR_SEGMENT_START_EXPECTED:
				eprintf("expected ");
				fprintTokenStr(
					stderr, 
					(Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_SEGMENT_START }, 
					&prep
				);
				eprintf(" as the segment start, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_BLOCK_SPEC_EXPECTED:
				eprintf("expected a string as the data block specifier, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				return 1;
			case VBRB_ERR_BLOCK_SIZE_EXPECTED:
				eprintf("expected an integer as the memory block size, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				return 1;
			case VBRB_ERR_UNKNOWN_SEGMENT_SPEC:
				eprintf("expected a segment specifier, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_UNCLOSED_SEGMENT:
				eprintf("expected ");
				fprintTokenStr(
					stderr, 
					(Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_SEGMENT_END }, 
					&prep
				);
				eprintf(", instead got ");
				fprintTokenStr(stderr, (Token){ .type = TOKEN_NONE }, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_INVALID_ARG:
				eprintf(
					sbuf_format" operation expects %s as argument %hhd, instead got ",
					unpack(prep.keywords[err.op_type]),
					getTokenTypeName(err.expected_token_type),
					err.arg_id
				);
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_INVALID_OP:
				eprintf("expected operation name, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_UNKNOWN_SYSCALL:
				eprintf(
					"expected%s a syscall identifier, instead got ",
					( isWordToken(err.loc) ? "" : "a word as" )
				);
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRB_ERR_EXEC_MARK_NOT_FOUND:
				eprintf("execution mark `%s` not found\n", getTokenWord(&prep, err.loc));
				return 1;
			case VBRB_ERR_DATA_BLOCK_NOT_FOUND:
				eprintf("data block `%s` not found\n", getTokenWord(&prep, err.loc));
				return 1;
			case VBRB_ERR_MEM_BLOCK_NOT_FOUND:
				eprintf("memory block `%s` not found\n", getTokenWord(&prep, err.loc));
				return 1;
			case VBRB_ERR_INVALID_REG_ID:
				eprintf("invalid register index %lld\n", err.loc.value);
				return 1;
			case VBRB_ERR_UNKNOWN_CONST:
				eprintf("unknown constant `%s`\n", getTokenWord(&prep, err.loc));
				return 1;
			case VBRB_ERR_INVALID_VAR_SIZE:
				eprintf("variable size can only be either 1, 2, 4 or 8, instead got %lld\n", err.loc.value);
				return 1;
			case VBRB_ERR_NON_PROC_ENTRY:
				eprintf("entry mark `%s` must point to the start of a procedure\n", err.mark_name);
				return 1;
			case VBRB_ERR_NON_PROC_CALL:
				eprintf("mark `%s` must point to the start of a procedure to be able to be called\n", err.mark_name);
				return 1;
			case VBRB_ERR_UNKNOWN_VAR_ID:
				eprintf("unknown variable index %lld\n", err.loc.value);
				return 1;
			case VBRB_ERR_UNCLOSED_PROC:
				eprintf("procedure must start in the global scope, i.e. not inside another procedure\n");
				return 1;
		}
	}

	FILE* output_fd = fopen(output_path, "wb");
	if (!output_fd) {
		eprintf("error: could not open/create file `%s` (reason: %s)\n", output_path, strerror(errno)); 
		return 1;
	}

	if (res.entry_opid) {
		setEntryPoint(output_fd, res.entry_opid);
	}
	if (res.stack_size != DEFAULT_STACK_SIZE) {
		setStackSize(output_fd, res.stack_size);
	}

	if (res.datablocks.length) {
		startDataSegment(output_fd);
		array_foreach(DataBlock, block, res.datablocks,
			writeDataBlock(output_fd, block.name, block.spec);
		);
		endDataSegment(output_fd);
	}
	
	if (res.memblocks.length) {
		startMemorySegment(output_fd);
		array_foreach(MemBlock, block, res.memblocks,
			writeMemoryBlock(output_fd, block.name, block.size);
		);
		endMemorySegment(output_fd);
	}
	
	startExecSegment(output_fd);
	array_foreach(Op, op, res.execblock,
		op_writers[op.type](output_fd, op);
	);
	endExecSegment(output_fd);	
	fclose(output_fd);

	printf("%s -> %s in %.3f ms\n", input_path, output_path, endTimer());
	if (!exec_output_path) return 0;

	char cmd[1024];
	ProcessInfo proc_res = { .out = stdout };
	snprintf(cmd, sizeof(cmd), "brbc -o %s %s", exec_output_path, output_path);

	if (!execProcess(cmd, &proc_res)) {
		eprintf("error: could start the bytecode compiler (reason: %s)\n", strerror(errno));
		return 1;
	} else if (proc_res.exitcode) {
		eprintf("error: bytecode compiler exited with code %hhu\n", proc_res.exitcode);
		sbuf err_output = filecontent(proc_res.err, GLOBAL_CTX);
		err_output.length--; // removing the newline from the output
		eprintf("bytecode compiler output:\n\t\""sbuf_format"\"\n", unpack(err_output));
		return 1;
	}
}
