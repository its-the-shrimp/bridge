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
	OP_SETB, // uses Op::dst_reg and Op::symbol_id
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
	OP_LTS, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_LTSR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_GTS, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_GTSR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_LES, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_LESR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_GES, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_GESR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_PUSH64, // uses Op::src_reg
	OP_POP64, // uses Op::dst_reg
	OP_PUSH32, // uses Op::src_reg
	OP_POP32, // uses Op::dst_reg
	OP_PUSH16, // uses Op::src_reg
	OP_POP16, // uses Op::dst_reg
	OP_PUSH8, // uses Op::src_reg
	OP_POP8, // uses Op::dst_reg
	OP_AND, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_ANDR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_OR, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_ORR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_NOT, // uses Op::dst_reg and Op::src_reg
	OP_XOR, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_XORR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SHL, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_SHLR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SHR, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_SHRR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SHRS, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_SHRSR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_CALL, // uses Op::symbol_id,
	OP_RET,
	OP_LD64, // uses Op::dst_reg and Op::src_reg
	OP_STR64, // uses Op::dst_reg and Op::src_reg
	OP_LD32, // uses Op::dst_reg and Op::src_reg
	OP_STR32, // uses Op::dst_reg and Op::src_reg
	OP_LD16, // uses Op::dst_reg and Op::src_reg
	OP_STR16, // uses Op::dst_reg and Op::src_reg
	OP_LD8, // uses Op::dst_reg and Op::src_reg
	OP_STR8, // uses Op::dst_reg and Op::src_reg
	OP_SETS, // uses Op::dst_reg and Op::value
	OP_SETSR, // uses Op::dst_reg and Op::src_reg
	OP_ALLOC, // uses Op::item_size and Op::value
	OP_ALLOCR, // uses Op::item_size and Op::src_reg
	N_OPS
} OpType;

#define _opNames \
	fromcstr("nop"), \
	fromcstr("end"), \
	fromcstr("mark"), \
	fromcstr("set"), \
	fromcstr("setr"), \
	fromcstr("setd"), \
	fromcstr("setb"), \
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
	fromcstr("lts"), \
	fromcstr("ltsr"), \
	fromcstr("gts"), \
	fromcstr("gtsr"), \
	fromcstr("les"), \
	fromcstr("lesr"), \
	fromcstr("ges"), \
	fromcstr("gesr"), \
	fromcstr("push64"), \
	fromcstr("pop64"), \
	fromcstr("push32"), \
	fromcstr("pop32"), \
	fromcstr("push16"), \
	fromcstr("pop16"), \
	fromcstr("push8"), \
	fromcstr("pop8"), \
	fromcstr("and"), \
	fromcstr("andr"), \
	fromcstr("or"), \
	fromcstr("orr"), \
	fromcstr("not"), \
	fromcstr("xor"), \
	fromcstr("xorr"), \
	fromcstr("shl"), \
	fromcstr("shlr"), \
	fromcstr("shr"), \
	fromcstr("shrr"), \
	fromcstr("shrs"), \
	fromcstr("shrsr"), \
	fromcstr("call"), \
	fromcstr("ret"), \
	fromcstr("ld64"), \
	fromcstr("str64"), \
	fromcstr("ld32"), \
	fromcstr("str32"), \
	fromcstr("ld16"), \
	fromcstr("str16"), \
	fromcstr("ld8"), \
	fromcstr("str8"), \
	fromcstr("sets"), \
	fromcstr("setsr"), \
	fromcstr("alloc"), \
	fromcstr("allocr") \

sbuf opNames[] = { _opNames };

#define _syscallNames \
	fromcstr("\x01"), \
	fromcstr("exit"), \
	fromcstr("write"), \
	fromcstr("argc"), \
	fromcstr("argv") \

sbuf syscallNames[] = { _syscallNames };

typedef enum {
	SYS_OP_INVALID,
	SYS_OP_EXIT,
	SYS_OP_WRITE,
	SYS_OP_ARGC,
	SYS_OP_ARGV,
	N_SYS_OPS
} SysOpCode;

typedef enum {
	EC_STACK_OVERFLOW = -9,
	EC_INVALID_ARG_ID,
	EC_NO_STACKFRAME,
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
	int8_t item_type;
	union {
		int64_t value;
		int64_t symbol_id;
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
} BRBuiltin;

BRBuiltin consts[] = {
	(BRBuiltin){
		.name = "stdin",
		.value = 0
	},
	(BRBuiltin){
		.name = "stdout",
		.value = 1
	},
	(BRBuiltin){
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
	int64_t entry_opid;
	int64_t stack_size;
} Program;

#define N_REGISTERS 8
#define DEFAULT_STACK_SIZE 512 * 1024 // 512 Kb, just like in JVM

#define BREX_TRACE_REGS     0b00000001
#define BREX_TRACE_STACK    0b00000010
#define BREX_CHECK_SYSCALLS 0b00000100

typedef enum {
	TRACER_VOID,
	TRACER_BOOL,
	TRACER_INT8,
	TRACER_INT16,
	TRACER_INT32,
	TRACER_INT64,
	TRACER_PTR,
	TRACER_CONST,
	N_TRACER_TYPES
} TracerType;

char TracerTypeSizes[] = { 
	0, // TRACER_VOID
	1, // TRACER_BOOL
	1, // TRACER_INT8
	2, // TRACER_INT16
	4, // TRACER_INT32
	8, // TRACER_INT64
	8, // TRACER_PTR
	8, // TRACER_CONST
};
static_assert(N_TRACER_TYPES == sizeof(TracerTypeSizes), "not all tracers have their sizes set");

#define isIntTracer(tracer) inRange((tracer).type, TRACER_BOOL, TRACER_PTR)


typedef enum {
	BUF_UNKNOWN,
	BUF_DATA,
	BUF_MEMORY,
	BUF_STACK,
	BUF_ARGV,
	N_BUF_TYPES
} BufferRefType;

typedef struct {
	BufferRefType type;
	int id;
} BufferRef;

typedef struct {
	int8_t type;
	union {
		BufferRef ref;
		int64_t symbol_id;
	};
	bool is_stackframe;
} Tracer;
defArray(Tracer);

typedef struct {
	sbuf heap;
	void* stack_brk;
	void* stack_head;
	void* prev_stack_head;
	sbufArray memblocks;
	int8_t exitcode;
	int64_t op_id;
	uint64_t* registers;
	union {
		int8_t err_pop_size; // for OP_POP*
		int8_t err_push_size; // for OP_PUSH*
		struct {
			int64_t err_access_length;
			BufferRef err_buf_ref;
			void* err_ptr;
		};
	};
	int8_t flags;
	Tracer* regs_trace; // initialized only if BREX_TRACE_REGS flag is set
	TracerArray stack_trace; // initialized only if BREX_TRACE_STACK flag is set
	int exec_argc;
	sbuf* exec_argv;
} ExecEnv;

typedef bool (*BRFFunc) (ExecEnv*, Program*);

#endif
