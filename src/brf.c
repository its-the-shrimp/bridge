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
	KW_SYS_NONE,
	KW_SYS_EXIT,
	KW_SYS_WRITE,
	KW_ENTRY,
	KW_EXEC,
	KW_DATA,
	KW_MEMORY,
	N_VBRF_KWS
} VBRFKeyword;
static_assert(N_OPS == 13, "there might be operations with unmatched keywords");
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
	&writeOpSyscall
};
static_assert(N_OPS == 13, "Handling all defined BRF operations");

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

int8_t getRegId(Token token)
{
	if (token.type != TOKEN_WORD) return -1;
	if (strlen(token.word) != 2) return -1;
	if (token.word[0] != 'r') return -1;
	return token.word[1] - '0';
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
	arrayhead(dst->execblock)->type = OP_MARK;
	Token mark_name = TokenChain_popstart(src);
	if (!isWordToken(mark_name)) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = mark_name,
			.arg_id = 0,
			.op_type = OP_MARK,
			.expected_token_type = TOKEN_WORD
		};
	}
	arrayhead(dst->execblock)->mark_name = getTokenWord(parser, mark_name);

	if (!ExecMarkArray_append(
		&ctx->marks, 
		(ExecMark){
			.name = mark_name,
			.id = dst->execblock.length - 1
		}
	)) {
		return (VBRFError){
			.code = VBRF_ERR_NO_MEMORY,
			.loc = mark_name
		};
	}
	return (VBRFError){0};
}

VBRFError compileOpSet(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_SET;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_SET,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	if (arg.type != TOKEN_INT) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_SET,
			.expected_token_type = TOKEN_INT
		};
	}
	arrayhead(dst->execblock)->value = arg.value;
	return (VBRFError){0};
}

VBRFError compileOpSetr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_SETR;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_SETR,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->src_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->src_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_SETR,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->src_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}
	
	return (VBRFError){0};
}

VBRFError compileOpSetd(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_SETD;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_SETD,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	if (!isWordToken(arg)) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_SETD,
			.expected_token_type = TOKEN_WORD
		};
	}

	if (!ExecMarkArray_append(
		&ctx->data_unresolved,
		(ExecMark){
			.name = arg,
			.id = dst->execblock.length - 1
		}
	)) {
		return (VBRFError){
			.code = VBRF_ERR_NO_MEMORY,
			.loc = arg
		};
	}

	return (VBRFError){0};
}

VBRFError compileOpSetc(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_SETC;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_SETC,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	if (!isWordToken(arg)) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_SETC,
			.expected_token_type = TOKEN_WORD
		};
	}
	arrayhead(dst->execblock)->symbol_id = getBRConstValue(getTokenWord(parser, arg));
	if (arrayhead(dst->execblock)->symbol_id == -1) {
		return (VBRFError){
			.code = VBRF_ERR_UNKNOWN_CONST,
			.loc = arg
		};
	}

	return (VBRFError){0};
}

VBRFError compileOpSetm(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_SETM;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_SETM,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	if (!isWordToken(arg)) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_SETM,
			.expected_token_type = TOKEN_WORD
		};
	}

	if (!ExecMarkArray_append(
		&ctx->mem_unresolved,
		(ExecMark){
			.name = arg,
			.id = dst->execblock.length - 1
		}
	)) {
		return (VBRFError){
			.code = VBRF_ERR_NO_MEMORY,
			.loc = arg
		};
	}

	return (VBRFError){0};
}

VBRFError compileOpAddr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_ADDR;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_ADDR,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->src_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->src_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_ADDR,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->src_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->src2_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->src2_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 2,
			.op_type = OP_ADDR,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->src2_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	return (VBRFError){0};
}

VBRFError compileOpAdd(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_ADD;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_ADD,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->src_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->src_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_ADD,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->src_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	if (arg.type != TOKEN_INT) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 2,
			.op_type = OP_ADD,
			.expected_token_type = TOKEN_INT
		};
	}
	arrayhead(dst->execblock)->value = arg.value;

	return (VBRFError){0};
}

VBRFError compileOpSyscall(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_SYSCALL;
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
	arrayhead(dst->execblock)->syscall_id = arg.keyword_id - N_OPS;
							
	return (VBRFError){0};
}

VBRFError compileOpSub(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_SUB;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_SUB,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->src_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->src_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_SUB,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->src_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	if (arg.type != TOKEN_INT) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 2,
			.op_type = OP_SUB,
			.expected_token_type = TOKEN_INT
		};
	}
	arrayhead(dst->execblock)->value = arg.value;

	return (VBRFError){0};
}

VBRFError compileOpSubr(Parser* parser, TokenChain* src, Program* dst, CompilerCtx* ctx)
{
	arrayhead(dst->execblock)->type = OP_SUBR;
	Token arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->dst_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->dst_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 0,
			.op_type = OP_SUBR,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->dst_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->src_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->src_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 1,
			.op_type = OP_SUBR,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->src_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

	arg = TokenChain_popstart(src);
	arrayhead(dst->execblock)->src2_reg = getRegId(arg);
	if (arrayhead(dst->execblock)->src2_reg == -1) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_ARG,
			.loc = arg,
			.arg_id = 2,
			.op_type = OP_SUBR,
			.expected_token_type = TOKEN_REG_ID
		};
	}
	if (arrayhead(dst->execblock)->src2_reg >= N_REGISTERS) {
		return (VBRFError){
			.code = VBRF_ERR_INVALID_REG_ID,
			.loc = arg,
		};
	}

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
};
static_assert(N_OPS == 13, "handling all defined BRF operations");

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
	// TODO: code jumps 
#if 0
	array_foreach(ExecMark, unresolved, exec_unresolved,
		array_foreach(ExecMark, mark, marks,
			if (streq(unresolved.name, mark.name)) 
		);
	);
#endif
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
