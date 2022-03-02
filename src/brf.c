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
	KW_SETC,
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
	KW_SYS_NONE,
	KW_SYS_EXIT,
	KW_SYS_WRITE,
	KW_ENTRY,
	KW_EXEC,
	KW_DATA,
	KW_MEMORY,
	N_VBRF_KWS
} VBRFKeyword;
static_assert(N_OPS == 27, "Some BRF operations have unmatched keywords");
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

void writeNop(FILE* fd, Op op)
{
	fputc(op.type, fd);
}

void writeOpEnd(FILE* fd, Op op)
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

void writeOpSetr(FILE* fd, Op op)
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

void writeOpSetc(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fwrite(BRByteOrder(&op.symbol_id, sizeof(op.symbol_id)), sizeof(op.symbol_id), 1, fd);
}

void writeOpAdd(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void writeOpAddr(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writeOpSub(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void writeOpSubr(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writeOpSyscall(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fwrite(BRByteOrder(&op.syscall_id, sizeof(op.syscall_id)), sizeof(op.syscall_id), 1, fd);
}

void writeOpGoto(FILE* fd, Op op)
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

void writeOpEq(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void writeOpEqr(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writeOpNeq(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void writeOpNeqr(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writeOpLt(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void writeOpLtr(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writeOpGt(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void writeOpGtr(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writeOpLe(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void writeOpLer(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

void writeOpGe(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fwrite(BRByteOrder(&op.value, sizeof(op.value)), sizeof(op.value), 1, fd);
}

void writeOpGer(FILE* fd, Op op)
{
	fputc(op.type, fd);
	fputc(op.dst_reg, fd);
	fputc(op.src_reg, fd);
	fputc(op.src2_reg, fd);
}

OpWriter op_writers[] = {
	&writeNop,
	&writeOpEnd,
	&writeOpMark,
	&writeOpSet,
	&writeOpSetr,
	&writeOpSetd,
	&writeOpSetc,
	&writeOpSetm,
	&writeOpAdd,
	&writeOpAddr,
	&writeOpSub,
	&writeOpSubr,
	&writeOpSyscall,
	&writeOpGoto,
	&writeOpCgoto,
	&writeOpEq,
	&writeOpEqr,
	&writeOpNeq,
	&writeOpNeqr,
	&writeOpLt,
	&writeOpLtr,
	&writeOpGt,
	&writeOpGtr,
	&writeOpLe,
	&writeOpLer,
	&writeOpGe,
	&writeOpGer
};
static_assert(N_OPS == sizeof(op_writers) / sizeof(op_writers[0]), "Some BRF operations have unmatched writers");

bool setEntryPoint(FILE* fd, int32_t mark_id)
{
	fputsbuf(fd, ENTRYSPEC_SEGMENT_START);
	fwrite(BRByteOrder(&mark_id, sizeof(mark_id)), sizeof(mark_id), 1, fd);
	return true;
}

typedef enum {
	VBRF_ERR_OK,
	VBRF_ERR_BLOCK_NAME_EXPECTED,
	VBRF_ERR_ENTRY_NAME_EXPECTED,
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

int getBRConstValue(char* name)
{
	for (int i = 0; i < sizeof(consts); i++) {
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
typedef VBRFError (*OpCompiler) (Parser*, TokenChain*, Program*, CompilerCtx*);

VBRFError compileNop(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_NONE;
	return (VBRFError){0};
}

VBRFError compileOpEnd(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_END;
	Token new = {0};
	while (src->end && getTokenSymbolId(new) != SYMBOL_SEGMENT_END) {
		new = TokenChain_popstart(src);
	}

	if (getTokenSymbolId(new) != SYMBOL_SEGMENT_END) {
		return (VBRFError){
			.code = VBRF_ERR_UNCLOSED_SEGMENT,
			.loc = new
		};
	}
	return (VBRFError){0};
}

VBRFError compileOpMark(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_MARK;

	VBRFError err = getExecMarkArg(TokenChain_popstart(src), dst->execblock.length - 1, &ctx->marks, OP_MARK, 0);
	if (err.code != VBRF_ERR_OK) return err;
	op->mark_name = getTokenWord(parser, arrayhead(ctx->marks)->name);

	return (VBRFError){0};
}

VBRFError compileOpSet(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SET;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_SET, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_SET, 1);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpSetr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SETR;
	
	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_SETR, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_SETR, 1);
	if (err.code != VBRF_ERR_OK) return err;
	
	return (VBRFError){0};
}

VBRFError compileOpSetd(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SETD;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_SETD, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getExecMarkArg(TokenChain_popstart(src), dst->execblock.length - 1, &ctx->data_unresolved, OP_SETD, 1);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpSetc(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SETC;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_SETC, 0);
	if (err.code != VBRF_ERR_OK) return err;

	Token arg = TokenChain_popstart(src);
	if (!isWordToken(arg)) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_SETC,
			.expected_token_type = TOKEN_WORD
		};
	}
	op->symbol_id = getBRConstValue(getTokenWord(parser, arg));
	if (op->symbol_id == -1) {
		return (VBRFError){
			.code = VBRF_ERR_UNKNOWN_CONST,
			.loc = arg
		};
	}

	return (VBRFError){0};
}

VBRFError compileOpSetm(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SETM;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_SETM, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getExecMarkArg(TokenChain_popstart(src), dst->execblock.length - 1, &ctx->mem_unresolved, OP_SETM, 1);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpAddr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_ADDR;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_ADDR, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_ADDR, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src2_reg, OP_ADDR, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpAdd(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_ADD;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_ADD, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_ADD, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_ADD, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpSyscall(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SYSCALL;
	Token arg = TokenChain_popstart(src);
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

VBRFError compileOpSub(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SUB;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_SUB, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_SUB, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_SUB, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpSubr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_SUBR;
	
	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_SUBR, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_SUBR, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src2_reg, OP_SUBR, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpGoto(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_GOTO;

	VBRFError err = getExecMarkArg(TokenChain_popstart(src), dst->execblock.length - 1, &ctx->exec_unresolved, OP_GOTO, 0);
	if (err.code != VBRF_ERR_OK) return err;
	return (VBRFError){0};
}

VBRFError compileOpCgoto(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_CGOTO;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_CGOTO, 0);
	if (err.code != VBRF_ERR_OK) return err;
	
	err = getExecMarkArg(TokenChain_popstart(src), dst->execblock.length - 1, &ctx->exec_unresolved, OP_CGOTO, 1);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpEq(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_EQ;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_EQ, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_EQ, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_EQ, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpEqr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_EQR;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_EQR, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_EQR, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src2_reg, OP_EQR, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpNeq(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_NEQ;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_NEQ, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_NEQ, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_NEQ, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpNeqr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_NEQR;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_NEQR, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_NEQR, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src2_reg, OP_NEQR, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpLt(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_LT;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_LT, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_LT, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_LT, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpLtr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_LTR;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_LTR, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_LTR, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src2_reg, OP_LTR, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpGt(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_GT;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_GT, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_GT, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_GT, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpGtr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_GTR;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_GTR, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_GTR, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src2_reg, OP_GTR, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpLe(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_LE;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_LE, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_LE, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_LE, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpLer(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_LER;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_LER, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_LER, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src2_reg, OP_LER, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpGe(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_GE;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_GE, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_GE, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getIntArg(TokenChain_popstart(src), &op->value, OP_GE, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

VBRFError compileOpGer(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->type = OP_GER;

	VBRFError err = getRegIdArg(TokenChain_popstart(src), &op->dst_reg, OP_GER, 0);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src_reg, OP_GER, 1);
	if (err.code != VBRF_ERR_OK) return err;

	err = getRegIdArg(TokenChain_popstart(src), &op->src2_reg, OP_GER, 2);
	if (err.code != VBRF_ERR_OK) return err;

	return (VBRFError){0};
}

OpCompiler op_compilers[] = {
	&compileNop,
	&compileOpEnd,
	&compileOpMark,
	&compileOpSet,
	&compileOpSetr,
	&compileOpSetd,
	&compileOpSetc,
	&compileOpSetm,
	&compileOpAdd,
	&compileOpAddr,
	&compileOpSub,
	&compileOpSubr,
	&compileOpSyscall,
	&compileOpGoto,
	&compileOpCgoto,
	&compileOpEq,
	&compileOpEqr,
	&compileOpNeq,
	&compileOpNeqr,
	&compileOpLt,
	&compileOpLtr,
	&compileOpGt,
	&compileOpGtr,
	&compileOpLe,
	&compileOpLer,
	&compileOpGe,
	&compileOpGer,
};
static_assert(N_OPS == sizeof(op_compilers) / sizeof(op_compilers[0]), "Some BRF operations have unmatched compilers");

VBRFError compileSourceCode(Parser* parser, TokenChain* src, Program* dst, heapctx_t ctx)
{
	enter_tempctx(funcctx, 0);
	dst->entry_opid = 0;
	dst->execblock = OpArray_new(ctx, -1);
	dst->memblocks = MemBlockArray_new(ctx, 0);
	dst->datablocks = DataBlockArray_new(ctx, 0);

	CompilerCtx compctx = {
		.exec_unresolved = ExecMarkArray_new(TEMP_CTX, 0),
		.mem_unresolved = ExecMarkArray_new(TEMP_CTX, 0),
		.data_unresolved = ExecMarkArray_new(TEMP_CTX, 0),
		.marks = ExecMarkArray_new(TEMP_CTX, -1)
	};
	Token entry_name = {0};

	while (src->end) {
		Token segment_spec = TokenChain_popstart(src);
		switch (getTokenKeywordId(segment_spec)) {
			case KW_ENTRY: {
				Token entry_symbol = TokenChain_popstart(src);
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
			} case KW_DATA: {
				Token block_start = TokenChain_popstart(src);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRFError){
						.code = VBRF_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Token block_name, block_spec;
				while (src->end) {
					block_name = TokenChain_popstart(src);
					if (getTokenSymbolId(block_name) == SYMBOL_SEGMENT_END) {
						break;
					} else if (!isWordToken(block_name)) {
						exit_tempctx(funcctx);
						return (VBRFError){
							.code = VBRF_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					block_spec = TokenChain_popstart(src);
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
							.name = getTokenWord(parser, block_name),
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
				Token block_start = TokenChain_popstart(src);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRFError){
						.code = VBRF_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Token block_name, block_spec;
				while (src->end) {
					block_name = TokenChain_popstart(src);
					if (getTokenSymbolId(block_name) == SYMBOL_SEGMENT_END) {
						break;
					} else if (!isWordToken(block_name)) {
						exit_tempctx(funcctx);
						return (VBRFError){
							.code = VBRF_ERR_BLOCK_NAME_EXPECTED,
							.loc = block_name
						};
					}

					block_spec = TokenChain_popstart(src);
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
							.name = getTokenWord(parser, block_name),
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
				Token block_start = TokenChain_popstart(src);
				if (getTokenSymbolId(block_start) != SYMBOL_SEGMENT_START) {
					exit_tempctx(funcctx);
					return (VBRFError){
						.code = VBRF_ERR_SEGMENT_START_EXPECTED,
						.loc = block_start
					};
				}

				Op* new_op;
				while (src->end) {
					Token op_name = TokenChain_popstart(src);
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
					VBRFError err = op_compilers[kw_id](parser, src, dst, &compctx);
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
			if (streq(getTokenWord(parser, mark.name), entry_name.word)) {
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
			if (streq(block.name, getTokenWord(parser, unresolved.name))) {
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
			if (streq(block.name, getTokenWord(parser, unresolved.name))) {
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
			if (streq(getTokenWord(parser, mark.name), getTokenWord(parser, unresolved.name))) {
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
		fromcstr("exec"),
		fromcstr("data"),
		fromcstr("memory"),
		(sbuf){0}
	};

	startTimer();
	Parser parser = newParser(delims, kws, GLOBAL_CTX);
	TokenChain tokens = TokenChain_new(GLOBAL_CTX, 0);

	FILE* input_fd = fopen(input_path, "r");
	if (!input_fd) {
		fprintf(stderr, "error: could not open file `%s` (reason: %s)\n", input_path, strerror(errno));
		return 1;
	}
	sbuf input = filecontent(input_fd, GLOBAL_CTX);
	if (!input.data) {
		fprintf(stderr, "error: could not fetch the content of file `%s`\n", input_path);
		return 1;
	}
	fclose(input_fd);

	switch (parseText(&parser, &tokens, input_path, input)) {
		case PARSER_ERR_NO_MEMORY:
			fprintf(stderr,"error: could not parse contents of file `%s`, memory failure\n", input_path);
			return 1;
		case PARSER_ERR_UNCLOSED_STR:
			fprintTokenLoc(stderr, parser.error_loc, &parser);
			fprintf(stderr, "error: unclosed string literal in file `%s`\n", input_path);
			return 1;
		default: break;
	}
	sctxalloc_free(&input);

	Program res;
	VBRFError err = compileSourceCode(&parser, &tokens, &res, ctxalloc_newctx(0));

	if (err.code != VBRF_ERR_OK) {
		fprintTokenLoc(stderr, err.loc.loc, &parser);
		eprintf("error: ");
		switch (err.code) {
			case VBRF_ERR_OK: break;
			case VBRF_ERR_ENTRY_NAME_EXPECTED:
				eprintf("expected a word or integer as the entry mark, instead got ");
				fprintTokenStr(stderr, err.loc, &parser);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_BLOCK_NAME_EXPECTED:
				eprintf("expected a word as the block name, instead got ");
				fprintTokenStr(stderr, err.loc, &parser);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_NO_MEMORY:
				eprintf("memory allocation failure\n");
				return 1;
			case VBRF_ERR_SEGMENT_START_EXPECTED:
				eprintf("expected ");
				fprintTokenStr(
					stderr, 
					(Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_SEGMENT_START }, 
					&parser
				);
				eprintf(" as the segment start, instead got ");
				fprintTokenStr(stderr, err.loc, &parser);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_BLOCK_SPEC_EXPECTED:
				eprintf("expected a string as the data block specifier, instead got ");
				fprintTokenStr(stderr, err.loc, &parser);
				return 1;
			case VBRF_ERR_BLOCK_SIZE_EXPECTED:
				eprintf("expected an integer as the memory block size, instead got ");
				fprintTokenStr(stderr, err.loc, &parser);
				return 1;
			case VBRF_ERR_UNKNOWN_SEGMENT_SPEC:
				eprintf("expected a segment specifier, instead got ");
				fprintTokenStr(stderr, err.loc, &parser);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_UNCLOSED_SEGMENT:
				eprintf("expected ");
				fprintTokenStr(
					stderr, 
					(Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_SEGMENT_END }, 
					&parser
				);
				eprintf(", instead got ");
				fprintTokenStr(stderr, (Token){ .type = TOKEN_NONE }, &parser);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_INVALID_ARG:
				eprintf(
					sbuf_format" operation expects %s as argument %hhd, instead got ",
					unpack(parser.keywords[err.op_type]),
					getTokenTypeName(err.expected_token_type),
					err.arg_id
				);
				fprintTokenStr(stderr, err.loc, &parser);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_INVALID_OP:
				eprintf("expected operation name, instead got ");
				fprintTokenStr(stderr, err.loc, &parser);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_UNKNOWN_SYSCALL:
				eprintf(
					"expected%s a syscall identifier, instead got ",
					( isWordToken(err.loc) ? "" : "a word as" )
				);
				fprintTokenStr(stderr, err.loc, &parser);
				fputc('\n', stderr);
				return 1;
			case VBRF_ERR_EXEC_MARK_NOT_FOUND:
				eprintf("execution mark `%s` not found\n", getTokenWord(&parser, err.loc));
				return 1;
			case VBRF_ERR_DATA_BLOCK_NOT_FOUND:
				eprintf("data block `%s` not found\n", getTokenWord(&parser, err.loc));
				return 1;
			case VBRF_ERR_MEM_BLOCK_NOT_FOUND:
				eprintf("memory block `%s` not found\n", getTokenWord(&parser, err.loc));
				return 1;
			case VBRF_ERR_INVALID_REG_ID:
				eprintf("invalid register index %lld\n", err.loc.value);
				return 1;
			case VBRF_ERR_UNKNOWN_CONST:
				eprintf("unknown constant `%s`\n", getTokenWord(&parser, err.loc));
				return 1;
		}
	}

	FILE* output_fd = fopen(output_path, "wb");
	if (!output_fd) {
		eprintf("error: could not open/create file `%s` (reason: %s)\n", output_path, strerror(errno)); 
		return 1;
	}

	setEntryPoint(output_fd, res.entry_opid);

	startDataSegment(output_fd);
	array_foreach(DataBlock, block, res.datablocks,
		writeDataBlock(output_fd, block.name, block.spec);
	);
	endDataSegment(output_fd);

	startMemorySegment(output_fd);
	array_foreach(MemBlock, block, res.memblocks,
		writeMemoryBlock(output_fd, block.name, block.size);
	);
	endMemorySegment(output_fd);

	startExecSegment(output_fd);
	array_foreach(Op, op, res.execblock,
		op_writers[op.type](output_fd, op);
	);
	endExecSegment(output_fd);	
	fclose(output_fd);

	printf("%s -> %s in %f ms\n", input_path, output_path, endTimer());
}
