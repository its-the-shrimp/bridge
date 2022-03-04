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
	OP_PUSH64, // uses Op::src_reg
	OP_POP64, // uses Op::dst_reg
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
	fromcstr("ger"), \
	fromcstr("push64"), \
	fromcstr("pop64") \

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
	EC_STACK_OVERFLOW = -7,
	EC_NEGATIVE_SIZE_ACCESS,
	EC_ACCESS_FAILURE,
	EC_STACK_UNDERFLOW, 
	EC_UNKNOWN_SYSCALL,
	EC_STACK_MISALIGNMENT,
	EC_ACCESS_MISALIGNMENT,
	EC_OK
} ExitCode;
static_assert(EC_OK == 0, "all special exit codes must have a value below 0");

const sbuf DATA_SEGMENT_START = fromcstr("D\n");
const sbuf MEMBLOCK_SEGMENT_START = fromcstr("M\n");
const sbuf EXEC_SEGMENT_START = fromcstr("X\n");
const sbuf ENTRYSPEC_SEGMENT_START = fromcstr("e:");
const sbuf STACKSIZE_SEGMENT_START = fromcstr("s:");
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
	int32_t value;
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

typedef struct program {
	OpArray execblock;
	MemBlockArray memblocks;
	DataBlockArray datablocks;
	int32_t entry_opid;
	int64_t stack_size;
} Program;

#define N_REGISTERS 8
#define DEFAULT_STACK_SIZE 512 * 1024 // 512 Kb, just like in JVM

#define BREX_TRACE_REGS  0x00000001
#define BREX_TRACE_STACK 0x00000010

typedef enum {
	TRACER_VOID,
	TRACER_BOOL,
	TRACER_INT64,
	TRACER_INT32,
	TRACER_INT16,
	TRACER_INT8,
	TRACER_DATAPTR,
	TRACER_MEMPTR,
	TRACER_CONST,
	N_TRACER_TYPES
} TracerType;

char TracerTypeSizes[N_TRACER_TYPES] = { 0, 1, 8, 4, 2, 1, 8, 8, 4 };
#define isIntTracer(tracer) \
	( (tracer).type == TRACER_INT8 || (tracer).type == TRACER_INT16 || (tracer).type == TRACER_INT32 || (tracer).type == TRACER_INT64 )

typedef struct {
	int8_t type;
	int32_t symbol_id; // for SI_CONST, SI_MEMPTR, SI_DATAPTR
} Tracer;
defArray(Tracer);

typedef struct {
	heapctx_t ctx;
	sbuf heap;
	void* stack_brk;
	void* stack_head;
	sbufArray memblocks;
	int8_t exitcode;
	int32_t op_id;
	uint64_t* registers;
	union {
		int8_t err_pop_size; // for OP_POP*
		int8_t err_push_size; // for OP_PUSH*
		struct {
			int64_t err_access_length;
			DataBlock err_buf;
			void* err_ptr;
		};
	};
	int8_t flags;
	Tracer* regs_trace; // initialized only if BREX_TRACE_REGS flag is set
	TracerArray stack_trace; // initialized only if BREX_TRACE_STACK flag is set
} ExecEnv;

typedef bool (*BRFFunc) (ExecEnv*, Program*);

#endif
