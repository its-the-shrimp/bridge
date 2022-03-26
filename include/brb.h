#ifndef _BRB_
#define _BRB_

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
	OP_PROC, // uses Op::mark_name
	OP_CALL, // uses Op::symbol_id
	OP_RET,
	OP_ENDPROC,
	OP_LD64, // uses Op::dst_reg and Op::src_reg
	OP_STR64, // uses Op::dst_reg and Op::src_reg
	OP_LD32, // uses Op::dst_reg and Op::src_reg
	OP_STR32, // uses Op::dst_reg and Op::src_reg
	OP_LD16, // uses Op::dst_reg and Op::src_reg
	OP_STR16, // uses Op::dst_reg and Op::src_reg
	OP_LD8, // uses Op::dst_reg and Op::src_reg
	OP_STR8, // uses Op::dst_reg and Op::src_reg
	OP_VAR, // uses Op::var_size
	OP_SETV, // uses Op::dst_reg and Op::symbol_id
	OP_MUL, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_MULR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_DIV, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_DIVR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg 
	OP_DIVS, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_DIVSR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg 
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
	fromcstr("proc"), \
	fromcstr("call"), \
	fromcstr("ret"), \
	fromcstr("endproc"), \
	fromcstr("ld64"), \
	fromcstr("str64"), \
	fromcstr("ld32"), \
	fromcstr("str32"), \
	fromcstr("ld16"), \
	fromcstr("str16"), \
	fromcstr("ld8"), \
	fromcstr("str8"), \
	fromcstr("var"), \
	fromcstr("setv"), \
	fromcstr("mul"), \
	fromcstr("mulr"), \
	fromcstr("div"), \
	fromcstr("divr"), \
	fromcstr("divs"), \
	fromcstr("divsr") \

sbuf opNames[] = { _opNames };


typedef enum {
	SYS_OP_INVALID,
	SYS_OP_EXIT,
	SYS_OP_WRITE,
	SYS_OP_ARGC,
	SYS_OP_ARGV,
	SYS_OP_READ,
	SYS_OP_GET_ERRNO, // TODO: replace this dinosaur with proper exceptions like in Python or Java
	SYS_OP_SET_ERRNO,
	N_SYS_OPS
} SysOpCode;

#define _syscallNames \
	fromcstr("\x01"), \
	fromcstr("exit"), \
	fromcstr("write"), \
	fromcstr("argc"), \
	fromcstr("argv"), \
	fromcstr("read"), \
	fromcstr("get_errno"), \
	fromcstr("set_errno")

sbuf syscallNames[] = { _syscallNames };


typedef enum {
	BRB_ERR_OK,
	BRB_ERR_NO_MEMORY,
	BRB_ERR_NO_ENTRY_SPEC,
	BRB_ERR_NO_BLOCK_NAME,
	BRB_ERR_NO_BLOCK_SIZE,
	BRB_ERR_NO_BLOCK_SPEC,
	BRB_ERR_NO_OPCODE,
	BRB_ERR_NO_MARK_NAME,
	BRB_ERR_NO_OP_ARG,
	BRB_ERR_INVALID_OPCODE,
	BRB_ERR_UNKNOWN_SEGMENT_SPEC,
	BRB_ERR_NO_STACK_SIZE
} BRBLoadErrorCode;

typedef struct {
	BRBLoadErrorCode code;
	union {
		sbuf segment_spec; // for BRB_ERR_UNKNOWN_SEGMENT_SPEC
		int32_t opcode; // for BRB_ERR_INVALID_OPCODE and BRB_ERR_NO_OP_ARG
	};
} BRBLoadError;

typedef enum {
	EC_STACK_OVERFLOW = -11,
	EC_ZERO_DIVISION,
	EC_OUTDATED_LOCALPTR,
	EC_UNDEFINED_STACK_LOAD,
	EC_NON_PROC_CALL,
	EC_NO_STACKFRAME,
	EC_NEGATIVE_SIZE_ACCESS,
	EC_ACCESS_FAILURE,
	EC_STACK_UNDERFLOW,
	EC_UNKNOWN_SYSCALL,
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
	int8_t var_size;
	union {
		int64_t value;
		int64_t symbol_id;
		char* mark_name;
		uint8_t syscall_id; 
		int8_t src2_reg;
	};
} Op;
declArray(Op);
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
	int64_t size;
} MemBlock;
declArray(MemBlock);

typedef struct {
	char* name;
	sbuf spec;
} DataBlock;
declArray(DataBlock);

typedef struct program {
	OpArray execblock;
	MemBlockArray memblocks;
	DataBlockArray datablocks;
	int64_t entry_opid;
	int64_t stack_size;
} Program;

typedef enum {
	DS_INT8,
	DS_INT16,
	DS_VOID,
	DS_INT32,
	DS_BOOL,
	DS_CONST,
	DS_PTR,
	DS_INT64,
	N_DS_TYPES
} DataType;

static const char DataTypeSizes[] = {
	1, // DS_INT8
	2, // DS_INT16
	0, // DS_VOID
	4, // DS_INT32
	1, // DS_BOOL
	8, // DS_CONST
	8, // DS_PTR
	8, // DS_INT64
};
static_assert(N_DS_TYPES == sizeof(DataTypeSizes), "not all tracers have their sizes set");
#define dataSpecSize(spec) ( (spec).type != DS_VOID ? DataTypeSizes[(spec).type] : (spec).size )
#define intSpecFromSize(size) ((DataSpec){.type = (size) - 1})
#define isIntSpec(spec) ( spec.type != DS_VOID && spec.type != DS_CONST && spec.type != DS_PTR )

typedef enum {
	BUF_UNKNOWN,
	BUF_DATA,
	BUF_MEMORY,
	BUF_VAR,
	BUF_ARGV,
	N_BUF_TYPES
} BufferRefType;

typedef struct {
	int8_t type;
	int id;
} BufferRef;

typedef struct {
	int8_t type;
	union { // type-specific parameters
		BufferRef ref; // for DS_PTR
		int64_t symbol_id; // for DS_CONST
		int64_t size; // for DS_VOID
		char* mark_name; // for DS_PROCFRAME of DS_FRAME
	};
} DataSpec;
declArray(DataSpec);

typedef struct {
	int8_t size;
	int32_t n_elements;
} Var;

typedef struct {
	DataSpecArray vars;
	int64_t prev_opid;
	int64_t call_id;
} ProcFrame;
declArray(ProcFrame);

#define N_REGISTERS 8
#define DEFAULT_STACK_SIZE (512 * 1024) // 512 Kb, just like in JVM

#define BRBX_TRACING         0b00000001
#define BRBX_CHECK_SYSCALLS  0b00000100
#define BRBX_PRINT_MEMBLOCKS 0b00001000

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
			union {
				BufferRef err_buf_ref;
				int err_spec_id;
				int8_t err_var_size;
			};
			void* err_ptr;
		};
	};
	int8_t flags;
	DataSpec* regs_trace; // initialized only if BRBX_TRACING flag is set
	ProcFrameArray vars; // initialized only if BRBX_TRACING flag is set
	int exec_argc;
	sbuf* exec_argv;
	int64_t call_count;
} ExecEnv;
#define envctx(envp) chunkctx(envp->stack_brk)

BRBLoadError loadProgram(FILE* fd, Program* dst, heapctx_t ctx);
void printLoadError(BRBLoadError err);
ExecEnv execProgram(Program* program, int8_t flags, char** args, volatile bool* interruptor);
void printExecState(FILE* fd, ExecEnv* env, Program* program);
void printRuntimeError(FILE* fd, ExecEnv* env, Program* program);

#endif // _BRB_
