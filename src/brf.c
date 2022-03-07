#include "brf.h"
#include "errno.h"

typedef enum {
	SYMBOL_SEGMENT_START,
	SYMBOL_SEGMENT_END,
	N_VBRF_SYMBOLS
} VBRFSymbol;

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
	KW_PUSH64,
	KW_POP64,
	KW_PUSH32,
	KW_POP32,
	KW_PUSH16,
	KW_POP16,
	KW_PUSH8,
	KW_POP8,
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
	KW_CALL,
	KW_RET,
	KW_SYS_NONE,
	KW_SYS_EXIT,
	KW_SYS_WRITE,
	KW_ENTRY,
	KW_STACKSIZE,
	KW_EXEC,
	KW_DATA,
	KW_MEMORY,
	N_VBRF_KWS
} VBRFKeyword;
static_assert(N_OPS == 56, "Some BRF operations have unmatched keywords");
static_assert(N_SYS_OPS == 3, "there might be system ops with unmatched keywords");

// special value for error reporting
#define TOKEN_REG_ID 125

bool startDataSegment(FILE* fd)
{
	return fputsbuf(fd, DATA_SEGMENT_START) > 0;
}

bool writeDataBlock(FILE* fd, char* name, sbuf obj)
{
	sbuf input_name = fromstr(name);
	fputsbuf(fd, input_name);
	fputsbuf(fd, SEP);
	int32_t data_length = obj.length;
	fwrite(BRByteOrder(&data_length, sizeof(data_length)), 1, sizeof(data_length), fd);
	fputsbuf(fd, obj);
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
	fputsbuf(fd, input_name);
	fputsbuf(fd, SEP);
	fwrite(BRByteOrder(&size, sizeof(size)), 1, sizeof(size), fd);
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

void writeOpMark(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputsbuf(fd, fromstr(op.mark_name));
	fputsbuf(fd, SEP);
}

void writeOpSet(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void write2RegOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
}

void writeOpSetd(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fwrite(BRByteOrder(&op.symbol_id, sizeof(op.symbol_id)), sizeof(op.symbol_id), 1, fd);
}

void writeOpSetm(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fwrite(BRByteOrder(&op.symbol_id, sizeof(op.symbol_id)), sizeof(op.symbol_id), 1, fd);
}

void writeOpSetb(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fwrite(BRByteOrder(&op.symbol_id, sizeof(op.symbol_id)), sizeof(op.symbol_id), 1, fd);
}

void writeOpSyscall(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fwrite(BRByteOrder(&op.syscall_id, sizeof(op.syscall_id)), sizeof(op.syscall_id), 1, fd);
}

void writeJumpOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fwrite(BRByteOrder(&op.symbol_id, sizeof(op.symbol_id)), sizeof(op.symbol_id), 1, fd);
}

void writeOpCgoto(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.symbol_id, sizeof(op.symbol_id)), sizeof(op.symbol_id), 1, fd);
}

void write2RegImmOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void write3RegOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writePushOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.src_reg, fd);
}

void writePopOp(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
}

OpWriter op_writers[] = {
	&writeNoArgOp, // OP_NONE
	&writeNoArgOp, // OP_END
	&writeOpMark,
	&writeOpSet,
	&write2RegOp, // OP_SETR
	&writeOpSetd,
	&writeOpSetb,
	&writeOpSetm,
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
	&writePushOp, // OP_PUSH64
	&writePopOp, // OP_POP64
	&writePushOp, // OP_PUSH32
	&writePopOp, // OP_POP32
	&writePushOp, // OP_PUSH16
	&writePopOp, // OP_POP16
	&writePushOp, // OP_PUSH8
	&writePopOp, // OP_POP8
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
	&writeJumpOp, // OP_CALL
	&writeNoArgOp // OP_RET
};
static_assert(N_OPS == sizeof(op_writers) / sizeof(op_writers[0]), "Some BRF operations have unmatched writers");

bool setEntryPoint(FILE* fd, int64_t mark_id)
{
	fputsbuf(fd, ENTRYSPEC_SEGMENT_START);
	fwrite(BRByteOrder(&mark_id, sizeof(mark_id)), sizeof(mark_id), 1, fd);
	return true;
}

bool setStackSize(FILE* fd, int64_t stack_size)
{
	fputsbuf(fd, STACKSIZE_SEGMENT_START);
	fwrite(BRByteOrder(&stack_size, sizeof(stack_size)), sizeof(stack_size), 1, fd);
	return true;
}

typedef enum {
	VBRF_ERR_OK,
	VBRF_ERR_BLOCK_NAME_EXPECTED,
	VBRF_ERR_ENTRY_NAME_EXPECTED,
	VBRF_ERR_STACK_SIZE_EXPECTED,
	VBRF_ERR_NO_MEMORY,
	VBRF_ERR_SEGMENT_START_EXPECTED,
	VBRF_ERR_BLOCK_SPEC_EXPECTED,
	VBRF_ERR_BLOCK_SIZE_EXPECTED,
	VBRF_ERR_UNKNOWN_SEGMENT_SPEC,
	VBRF_ERR_UNCLOSED_SEGMENT,
	VBRF_ERR_INVALID_ARG,
	VBRF_ERR_INVALID_OP,
	VBRF_ERR_UNKNOWN_SYSCALL,
	VBRF_ERR_EXEC_MARK_NOT_FOUND,
	VBRF_ERR_DATA_BLOCK_NOT_FOUND,
	VBRF_ERR_MEM_BLOCK_NOT_FOUND,
	VBRF_ERR_INVALID_REG_ID,
	VBRF_ERR_UNKNOWN_CONST
} VBRFErrorCode;

typedef struct {
	VBRFErrorCode code;
	Token loc;
	union {
		struct { // for VBRF_ERR_INVALID_ARG
			int8_t arg_id;
			uint8_t op_type;
			uint8_t expected_token_type;
		};
	};
} VBRFError;

typedef struct {
	Token name;
	int32_t id;
} ExecMark;
defArray(ExecMark);

int64_t getBRBuiltinValue(char* name)
{
	for (int64_t i = 0; i < sizeof(consts); i++) {
		if (streq(consts[i].name, name)) return i;
	}
	return -1;
} 

VBRFError getRegIdArg(Token src, int8_t* dst, char op_type, char arg_id)
{
	if (src.type != TOKEN_WORD) return (VBRFError){
		.code = VBRF_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_REG_ID
	};
	if (strlen(src.word) != 2 || src.word[0] != 'r') return (VBRFError){
		.code = VBRF_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_REG_ID
	};
	*dst = src.word[1] - '0';
	if (!inRange(*dst, 0, N_REGISTERS)) return (VBRFError){
		.code = VBRF_ERR_INVALID_REG_ID,
		.loc = src
	};
	return (VBRFError){0};
}

VBRFError getIntArg(Token src, int64_t* dst, char op_type, char arg_id)
{
	if (src.type != TOKEN_INT) return (VBRFError){
		.code = VBRF_ERR_INVALID_ARG,
		.loc = src,
		.op_type = op_type,
		.arg_id = arg_id,
		.expected_token_type = TOKEN_INT
	};
	*dst = src.value;
	return (VBRFError){0};
}

VBRFError getExecMarkArg(Token src, int id, ExecMarkArray* dst, char op_type, char arg_id)
{
	if (!isWordToken(src)) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = src,
			.arg_id = arg_id,
			.op_type = op_type,
			.expected_token_type = TOKEN_WORD
		};
	}
	if (!ExecMarkArray_append(dst, (ExecMark){ .name = src, .id = id })) {
		return (VBRFError){
			.code = VBRF_ERR_NO_MEMORY,
			.loc = src
		};
	}
	return (VBRFError){0};
}

typedef struct {
	ExecMarkArray data_unresolved;
	ExecMarkArray mem_unresolved;
	ExecMarkArray exec_unresolved;
	ExecMarkArray marks;
} CompilerCtx;
typedef VBRFError (*OpCompiler) (Preprocessor*, Program*, CompilerCtx*);

VBRFError compileNoArgOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	return (VBRFError){0};
}

VBRFError compileOpMark(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRFError err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->marks, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;
	op->mark_name = getTokenWord(obj, arrayhead(ctx->marks)->name);

	return (VBRFError){0};
}

VBRFError compileOpSet(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SET;

	VBRFError err = getRegIdArg(fetchToken(obj), &op->dst_reg, OP_SET, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(fetchToken(obj), &op->value, OP_SET, 1);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compile2RegOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	
	VBRFError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 1);
	if (err.code != VBRF_ERR_OK) return err;
	
	return (VBRFError){0};
}

VBRFError compileOpSetd(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRFError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->data_unresolved, op->type, 1);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpSetm(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRFError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->mem_unresolved, op->type, 1);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpSetb(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRFError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;

	Token arg = fetchToken(obj);
	if (!isWordToken(arg)) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = op->type,
			.expected_token_type = TOKEN_WORD
		};
	}
	op->symbol_id = getBRBuiltinValue(getTokenWord(obj, arg));
	if (op->symbol_id == -1) {
		return (VBRFError){
			.code = VBRF_ERR_UNKNOWN_CONST,
			.loc = arg
		};
	}

	return (VBRFError){0};
}

VBRFError compileOpSyscall(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	Token arg = fetchToken(obj);
	if (
		arg.type != TOKEN_KEYWORD || 
		!inRange(arg.keyword_id, N_OPS, N_OPS + N_SYS_OPS)
	) {
		return (VBRFError){
			.code = VBRF_ERR_UNKNOWN_SYSCALL,
			.loc = arg
		};
	}
	op->syscall_id = arg.keyword_id - N_OPS;
							
	return (VBRFError){0};
}

VBRFError compile2RegImmOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRFError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(fetchToken(obj), &op->value, op->type, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compile3RegOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	
	VBRFError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(fetchToken(obj), &op->src2_reg, op->type, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileJumpOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRFError err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->exec_unresolved, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;
	return (VBRFError){0};
}

VBRFError compileOpCgoto(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRFError err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;
	
	err = getExecMarkArg(fetchToken(obj), dst->execblock.length - 1, &ctx->exec_unresolved, op->type, 1);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compilePushOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);

	VBRFError err = getRegIdArg(fetchToken(obj), &op->src_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compilePopOp(Preprocessor* obj, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	
	VBRFError err = getRegIdArg(fetchToken(obj), &op->dst_reg, op->type, 0);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

OpCompiler op_compilers[] = {
	&compileNoArgOp, // OP_NONE
	&compileNoArgOp, // OP_END
	&compileOpMark,
	&compileOpSet,
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
	&compilePushOp, // OP_PUSH64
	&compilePopOp, // OP_POP64
	&compilePushOp, // OP_PUSH32
	&compilePopOp, // OP_POP32
	&compilePushOp, // OP_PUSH16
	&compilePopOp, // OP_POP16
	&compilePushOp, // OP_PUSH8
	&compilePopOp, // OP_POP8
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
	&compileJumpOp, // OP_CALL
	&compileNoArgOp // OP_RET
};
static_assert(N_OPS == sizeof(op_compilers) / sizeof(op_compilers[0]), "Some BRF operations have unmatched compilers");

VBRFError compileSourceCode(Preprocessor* obj, Program* dst, heapctx_t ctx)
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
		.marks = ExecMarkArray_new(TEMP_CTX, -1)
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
					return (VBRFError){
						.code = VBRF_ERR_ENTRY_NAME_EXPECTED,
						.loc = entry_symbol
					};
				}
				break;
			} case KW_STACKSIZE: {
				Token size_spec = fetchToken(obj);
				if (size_spec.type != TOKEN_INT) {
					exit_tempctx(funcctx);
					return (VBRFError){
						.code = VBRF_ERR_STACK_SIZE_EXPECTED,
						.loc = size_spec
					};
				}

				dst->stack_size = size_spec.value * 1024;
				break;
			} case KW_DATA: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRFError){
						.code = VBRF_ERR_SEGMENT_START_EXPECTED,
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
						return (VBRFError){
							.code = VBRF_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					block_spec = fetchToken(obj);
					if (block_spec.type != TOKEN_STRING) {
						exit_tempctx(funcctx);
						return (VBRFError){
							.code = VBRF_ERR_BLOCK_SPEC_EXPECTED,
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
						return (VBRFError){
							.code = VBRF_ERR_NO_MEMORY,
							.loc = block_spec
						};
					}
				}

				break;
			} case KW_MEMORY: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRFError){
						.code = VBRF_ERR_SEGMENT_START_EXPECTED,
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
						return (VBRFError){
							.code = VBRF_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					block_spec = fetchToken(obj);
					if (block_spec.type != TOKEN_INT) {
						exit_tempctx(funcctx);
						return (VBRFError){
							.code = VBRF_ERR_BLOCK_SIZE_EXPECTED,
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
						return (VBRFError){
							.code = VBRF_ERR_NO_MEMORY,
							.loc = block_spec
						};
					}
				}
				break;
			} case KW_EXEC: {
				Token block_start = fetchToken(obj);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRFError){
						.code = VBRF_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Op* new_op;
				while (!isPrepEOF(obj)) {
					Token op_name = fetchToken(obj);
					if (getTokenSymbolId(op_name) == SYMBOL_SEGMENT_END) break; 

					if (!(new_op = OpArray_append(&dst->execblock, (Op){0}))) {
						exit_tempctx(funcctx);
						return (VBRFError){
							.code = VBRF_ERR_NO_MEMORY,
							.loc = segment_spec
						};
					}

					char kw_id = getTokenKeywordId(op_name);
					if (!inRange(kw_id, 0, N_OPS)) {
						exit_tempctx(funcctx);
						return (VBRFError){
							.code = VBRF_ERR_INVALID_OP,
							.loc = op_name
						};
					}

					new_op->type = kw_id;
					VBRFError err = op_compilers[kw_id](obj, dst, &compctx);
					if (err.code != VBRF_ERR_OK) {
						exit_tempctx(funcctx);
						return err;
					}
				}
				break;
			} default: {
				exit_tempctx(funcctx);
				return (VBRFError){
					.code = VBRF_ERR_UNKNOWN_SEGMENT_SPEC,
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
				resolved = true;
				break;
			}
		);
		if (!resolved) {
			exit_tempctx(funcctx);
			return (VBRFError){
				.code = VBRF_ERR_EXEC_MARK_NOT_FOUND,
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
			return ((VBRFError){
				.code = VBRF_ERR_DATA_BLOCK_NOT_FOUND,
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
			return ((VBRFError){
				.code = VBRF_ERR_MEM_BLOCK_NOT_FOUND,
				.loc = unresolved.name
			});
		}
	);
	// resolving code jumps
	array_foreach(ExecMark, unresolved, compctx.exec_unresolved, {
		array_foreach(ExecMark, mark, compctx.marks,
			if (streq(getTokenWord(obj, mark.name), getTokenWord(obj, unresolved.name))) {
				dst->execblock.data[unresolved.id].symbol_id = mark.id;
				resolved = true;
				break;
			}
		);
		if (!resolved) {
			exit_tempctx(funcctx);
			return ((VBRFError){
				.code = VBRF_ERR_EXEC_MARK_NOT_FOUND,
				.loc = unresolved.name
			});
		}
		resolved = false;
	});
	
	exit_tempctx(funcctx);
	return (VBRFError){ .code = VBRF_ERR_OK };
}

char* getTokenTypeName(char token_type)
{
	if (token_type == TOKEN_REG_ID) return "register";
	return TokenTypeNames[token_type];
}

void printUsageMsg(FILE* fd, char* execname)
{
	fprintf(fd, "brf - Compiles `.vbrf` (BRidge Assembler) to interpetable `.brf` files (BRidge Executable)");
	fprintf(fd, "usage: %s [options] <file>\n", execname);
	fprintf(fd, "options:\n");
	fprintf(fd, "\t-h           Output this message and exit\n");
	fprintf(fd, "\t-o <file>    The output will be saved to <file>\n");
}

int main(int argc, char* argv[])
{
	initBREnv();
	char *input_path = NULL, *output_path = NULL;
	for (int i = 1; i < argc; i++) {
		if (streq(argv[i], "-h")) {
			printUsageMsg(stdout, argv[0]);
			return 0;
		} else if (streq(argv[i], "-o")) {
			if (!argv[++i]) {
				fprintf(stderr, "error: `-o` flag specified but no output path provided\n");
				return 1;
			}
			output_path = argv[i];
		} else if (argv[i][0] != '-') {
			input_path = argv[i];
		} else {
			fprintf(stderr, "error: unknown option `%s`\n", argv[i]);
			return 1;
		}
	}

	if (!input_path) {
		fprintf(stderr, "error: no input path provided\n");
		return 1;
	}

	if (!output_path) {
		sbuf ext = fromstr(input_path), noext = {0};
		sbufsplit(&ext, &noext, fromcstr("."));
		output_path = tostr(GLOBAL_CTX, noext, fromcstr(".brf"));
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

	startTimer();
	Preprocessor prep = newPreprocessor(delims, kws, GLOBAL_CTX);
	if (!setInput(&prep, input_path)) {
		eprintf("preprocessing error: %s\n", getErrorStr(&prep));
		return 1;
	}

	Program res;
	VBRFError err = compileSourceCode(&prep, &res, ctxalloc_newctx(0));

	if (err.code != VBRF_ERR_OK) {
		fprintTokenLoc(stderr, err.loc.loc, &prep);
		eprintf("error: ");
		switch (err.code) {
			case VBRF_ERR_OK: break;
			case VBRF_ERR_ENTRY_NAME_EXPECTED:
				eprintf("expected a word or integer as the entry mark, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_BLOCK_NAME_EXPECTED:
				eprintf("expected a word as the block name, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_STACK_SIZE_EXPECTED:
				eprintf("expected integer as the stack size specifier, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				break;
			case VBRF_ERR_NO_MEMORY:
				eprintf("memory allocation failure\n");
				return 1;
			case VBRF_ERR_SEGMENT_START_EXPECTED:
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
			case VBRF_ERR_BLOCK_SPEC_EXPECTED:
				eprintf("expected a string as the data block specifier, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				return 1;
			case VBRF_ERR_BLOCK_SIZE_EXPECTED:
				eprintf("expected an integer as the memory block size, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				return 1;
			case VBRF_ERR_UNKNOWN_SEGMENT_SPEC:
				eprintf("expected a segment specifier, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_UNCLOSED_SEGMENT:
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
			case VBRF_ERR_INVALID_ARG:
				eprintf(
					sbuf_format" operation expects %s as argument %hhd, instead got ",
					unpack(prep.keywords[err.op_type]),
					getTokenTypeName(err.expected_token_type),
					err.arg_id
				);
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_INVALID_OP:
				eprintf("expected operation name, instead got ");
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_UNKNOWN_SYSCALL:
				eprintf(
					"expected%s a syscall identifier, instead got ",
					( isWordToken(err.loc) ? "" : "a word as" )
				);
				fprintTokenStr(stderr, err.loc, &prep);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_EXEC_MARK_NOT_FOUND:
				eprintf("execution mark `%s` not found\n", getTokenWord(&prep, err.loc));
				return 1;
			case VBRF_ERR_DATA_BLOCK_NOT_FOUND:
				eprintf("data block `%s` not found\n", getTokenWord(&prep, err.loc));
				return 1;
			case VBRF_ERR_MEM_BLOCK_NOT_FOUND:
				eprintf("memory block `%s` not found\n", getTokenWord(&prep, err.loc));
				return 1;
			case VBRF_ERR_INVALID_REG_ID:
				eprintf("invalid register index %lld\n", err.loc.value);
				return 1;
			case VBRF_ERR_UNKNOWN_CONST:
				eprintf("unknown constant `%s`\n", getTokenWord(&prep, err.loc));
				return 1;
		}
	}

	FILE* output_fd = fopen(output_path, "wb");
	if (!output_fd) {
		eprintf("error: could not open/create file `%s` (reason: %s)\n", output_path, strerror(errno)); 
		return 1;
	}

	setEntryPoint(output_fd, res.entry_opid);
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

	printf("%s -> %s in %f ms\n", input_path, output_path, endTimer());
}
