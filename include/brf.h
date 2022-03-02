#ifndef _BRF_
#define _BRF_

#include "br.h"

typedef enum {
	OP_NONE,
	OP_END,
	OP_MARK, // uses Op::mark_name
	OP_SET, // uses Op::dst_reg and Op::value
	OP_SETR, // uses Op::dst_reg and Op::src_reg
	OP_SETD, // uses Op::dst_reg and Op::symbol_id
	OP_SETC, // uses Op::dst_reg and Op::symbol_id
	OP_SETM, // uses Op::dst_reg and Op::symbol_id
	OP_ADD, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_ADDR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SUB, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_SUBR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SYSCALL, // uses Op::syscall_id
	OP_GOTO, // uses Op::symbol_id
	OP_CGOTO, // uses Op::src_reg and Op::symbol_id
	OP_EQ, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_EQR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_NEQ, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_NEQR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_LT, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_LTR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_GT, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_GTR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_LE, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_LER, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_GE, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_GER, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	N_OPS
} OpType;

#define _opNames \
	fromcstr("nop"), \
	fromcstr("end"), \
	fromcstr("mark"), \
	fromcstr("set"), \
	fromcstr("setr"), \
	fromcstr("setd"), \
	fromcstr("setc"), \
	fromcstr("setm"), \
	fromcstr("add"), \
	fromcstr("addr"), \
	fromcstr("sub"), \
	fromcstr("subr"), \
	fromcstr("sys"), \
	fromcstr("goto"), \
	fromcstr("cgoto"), \
	fromcstr("eq"), \
	fromcstr("eqr"), \
	fromcstr("neq"), \
	fromcstr("neqr"), \
	fromcstr("lt"), \
	fromcstr("ltr"), \
	fromcstr("gt"), \
	fromcstr("gtr"), \
	fromcstr("le"), \
	fromcstr("ler"), \
	fromcstr("ge"), \
	fromcstr("ger") \

sbuf opNames[] = { _opNames };

#define _syscallNames \
	fromcstr("\x01"), \
	fromcstr("exit"), \
	fromcstr("write") \

sbuf syscallNames[] = { _syscallNames };

typedef enum {
	SYS_OP_INVALID,
	SYS_OP_EXIT,
	SYS_OP_WRITE,
	N_SYS_OPS
} SysOpCode;

typedef enum {
	EC_OK,
	EC_FAILURE,
	EC_UNKNOWN_OP,
	EC_STACK_UNDERFLOW,
	EC_UNKNOWN_MARK,
	EC_UNKNOWN_SYS_OP
} ExitCode;

const sbuf DATA_SEGMENT_START = fromcstr("D\n");
const sbuf MEMBLOCK_SEGMENT_START = fromcstr("M\n");
const sbuf EXEC_SEGMENT_START = fromcstr("X\n");
const sbuf ENTRYSPEC_SEGMENT_START = fromcstr("E:");
const sbuf EXECMARK_SEGMENT_START = fromcstr("S\n");
const sbuf SEP = fromcstr(":");

typedef struct op {
	int8_t type;
	int8_t dst_reg;
	int8_t src_reg;
	union {
		int64_t value;
		int32_t symbol_id;
		char* mark_name;
		uint8_t syscall_id; 
		int8_t src2_reg;
	};
} Op;
defArray(Op);
static_assert(sizeof(Op) == 16, "checking compactness of operations' storage");

typedef struct {
	char* name;
	int64_t value;
} BRConst;

BRConst consts[] = {
	(BRConst){
		.name = "stdin",
		.value = 0
	},
	(BRConst){
		.name = "stdout",
		.value = 1
	},
	(BRConst){
		.name = "stderr",
		.value = 2
	}
};

typedef struct {
	char* name;
	int32_t size;
} MemBlock;
defArray(MemBlock);

typedef struct {
	char* name;
	sbuf spec;
} DataBlock;
defArray(DataBlock);

typedef struct {
	OpArray execblock;
	MemBlockArray memblocks;
	DataBlockArray datablocks;
	int32_t entry_opid;
} Program;

#define N_REGISTERS 8

typedef struct {
	heapctx_t ctx;
	sbuf heap;
	sbuf stack;
	sbufArray memblocks;
	uint8_t exitcode;
	int32_t op_id;
	int64_t* registers;
} ExecEnv;

typedef bool (*BRFFunc) (ExecEnv*, Program*);

#endif
