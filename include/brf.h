#ifndef _BRF_
#define _BRF_

#include "br.h"

typedef enum {
	OP_NONE,
	OP_END,
	OP_MARK,
	OP_SET,
	OP_SETR,
	OP_SETD,
	OP_SETC,
	OP_SETM,
	OP_ADD,
	OP_ADDR,
	OP_SUB,
	OP_SUBR,
	OP_SYSCALL,
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
	fromcstr("sys") \

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
	char type;
	char dst_reg;
	int8_t src_reg; // for OP_*R or OP_ADD*
	union {
		int64_t value; // for OP_SET and OP_ADD
		int32_t symbol_id; // for OP_*D or OP_*M or OP_*C
		char* mark_name; // for OP_MARK
		uint8_t syscall_id; // for OP_SYSCALL
		int8_t src2_reg; // for OP_ADDR
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
