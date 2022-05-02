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
	OP_SYS, // uses Op::syscall_id
	OP_GOTO, // uses Op::op_offset
	OP_CMP, // uses Op::src_reg and Op::value
	OP_CMPR, // uses Op::src_reg and Op::src2_reg
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
	OP_EXTPROC, // uses Op::mark_name
	OP_LDV, // uses Op::src_reg, Op::symbol_id and Op::var_size
	OP_STRV, // uses Op::dst_reg, Op::symbol_id and Op::var_size
	OP_POPV, // uses Op::dst_reg, Op::var_size
	OP_PUSHV, // uses Op::src_reg and Op::var_size
	OP_ATF, // uses Op::mark_name
	OP_ATL, // uses Op::symbol_id
	N_OPS
} OpType;

#define _opNames \
	BRP_KEYWORD("nop"), \
	BRP_KEYWORD("end"), \
	BRP_KEYWORD("mark"), \
	BRP_KEYWORD("set"), \
	BRP_KEYWORD("setr"), \
	BRP_KEYWORD("setd"), \
	BRP_KEYWORD("setb"), \
	BRP_KEYWORD("setm"), \
	BRP_KEYWORD("add"), \
	BRP_KEYWORD("addr"), \
	BRP_KEYWORD("sub"), \
	BRP_KEYWORD("subr"), \
	BRP_KEYWORD("sys"), \
	BRP_KEYWORD("goto"), \
	BRP_KEYWORD("cmp"), \
	BRP_KEYWORD("cmpr"), \
	BRP_KEYWORD("and"), \
	BRP_KEYWORD("andr"), \
	BRP_KEYWORD("or"), \
	BRP_KEYWORD("orr"), \
	BRP_KEYWORD("not"), \
	BRP_KEYWORD("xor"), \
	BRP_KEYWORD("xorr"), \
	BRP_KEYWORD("shl"), \
	BRP_KEYWORD("shlr"), \
	BRP_KEYWORD("shr"), \
	BRP_KEYWORD("shrr"), \
	BRP_KEYWORD("shrs"), \
	BRP_KEYWORD("shrsr"), \
	BRP_KEYWORD("proc"), \
	BRP_KEYWORD("call"), \
	BRP_KEYWORD("ret"), \
	BRP_KEYWORD("endproc"), \
	BRP_KEYWORD("ld64"), \
	BRP_KEYWORD("str64"), \
	BRP_KEYWORD("ld32"), \
	BRP_KEYWORD("str32"), \
	BRP_KEYWORD("ld16"), \
	BRP_KEYWORD("str16"), \
	BRP_KEYWORD("ld8"), \
	BRP_KEYWORD("str8"), \
	BRP_KEYWORD("var"), \
	BRP_KEYWORD("setv"), \
	BRP_KEYWORD("mul"), \
	BRP_KEYWORD("mulr"), \
	BRP_KEYWORD("div"), \
	BRP_KEYWORD("divr"), \
	BRP_KEYWORD("divs"), \
	BRP_KEYWORD("divsr"), \
	BRP_KEYWORD("extproc"), \
	BRP_KEYWORD("ldv"), \
	BRP_KEYWORD("strv"), \
	BRP_KEYWORD("popv"), \
	BRP_KEYWORD("pushv"), \
	BRP_KEYWORD("@f"), \
	BRP_KEYWORD("@l") \

static sbuf opNames[] = { _opNames };


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
	BRP_KEYWORD("\x01"), \
	BRP_KEYWORD("exit"), \
	BRP_KEYWORD("write"), \
	BRP_KEYWORD("argc"), \
	BRP_KEYWORD("argv"), \
	BRP_KEYWORD("read"), \
	BRP_KEYWORD("get_errno"), \
	BRP_KEYWORD("set_errno")

static sbuf syscallNames[] = { _syscallNames };

typedef enum {
	BRB_ERR_OK,
	BRB_ERR_NO_MEMORY,
	BRB_ERR_NO_BLOCK_NAME,
	BRB_ERR_NO_BLOCK_SIZE,
	BRB_ERR_NO_BLOCK_SPEC,
	BRB_ERR_NO_OPCODE,
	BRB_ERR_NO_MARK_NAME,
	BRB_ERR_NO_OP_ARG,
	BRB_ERR_INVALID_OPCODE,
	BRB_ERR_NO_DATA_SEGMENT,
	BRB_ERR_NO_MEMORY_SEGMENT,
	BRB_ERR_NO_EXEC_SEGMENT,
	BRB_ERR_NO_NAME_SEGMENT,
	BRB_ERR_NO_NAME_SPEC,
	BRB_ERR_INVALID_COND_ID,
	BRB_ERR_NO_COND_ID,
	BRB_ERR_NO_STACK_SIZE,
	BRB_ERR_NO_ENTRY,
	BRB_ERR_NO_LOAD_SEGMENT,
	BRB_ERR_MODULE_NOT_FOUND,
	N_BRB_ERRORS
} BRBLoadErrorCode;

typedef struct {
	BRBLoadErrorCode code;
	union {
		char* module_name; // for BRB_ERR_MODULE_NOT_FOUND
		int32_t opcode; // for BRB_ERR_INVALID_OPCODE and BRB_ERR_NO_OP_ARG
		uint8_t cond_id; // for BRB_ERR_INVALID_COND_ID
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

typedef enum {
	COND_NON,
	COND_EQU,
	COND_NEQ,
	COND_LTU,
	COND_GTU,
	COND_LEU,
	COND_GEU,
	COND_LTS,
	COND_GTS,
	COND_LES,
	COND_GES,
	N_CONDS
} ConditionCode;

static ConditionCode opposite_conditions[N_CONDS] = {
	[COND_NON] = COND_NON,
	[COND_EQU] = COND_NEQ,
	[COND_NEQ] = COND_EQU,
	[COND_LTU] = COND_GEU,
	[COND_GTU] = COND_LEU,
	[COND_LEU] = COND_GTU,
	[COND_GEU] = COND_LTU,
	[COND_LTS] = COND_GES,
	[COND_GTS] = COND_LES,
	[COND_LES] = COND_GTS,
	[COND_GES] = COND_LTS
};

#define _conditionNames \
	BRP_KEYWORD("non"), \
	BRP_KEYWORD("equ"), \
	BRP_KEYWORD("neq"), \
	BRP_KEYWORD("ltu"), \
	BRP_KEYWORD("gtu"), \
	BRP_KEYWORD("leu"), \
	BRP_KEYWORD("geu"), \
	BRP_KEYWORD("lts"), \
	BRP_KEYWORD("gts"), \
	BRP_KEYWORD("les"), \
	BRP_KEYWORD("ges") \

typedef struct {
	int8_t type;
	int8_t dst_reg;
	int8_t src_reg;
	uint8_t var_size;
	uint8_t cond_id;
	union {
		int64_t value;
		int64_t symbol_id;
		int64_t op_offset;
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

#define N_BUILTINS 3
static const BRBuiltin builtins[N_BUILTINS] = {
	(BRBuiltin){
		.name = "STDIN",
		.value = STDIN_FILENO
	},
	(BRBuiltin){
		.name = "STDOUT",
		.value = STDOUT_FILENO
	},
	(BRBuiltin){
		.name = "STDERR",
		.value = STDERR_FILENO
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

typedef char* str;
declArray(str);

typedef struct {
	OpArray execblock;
	MemBlockArray memblocks;
	DataBlockArray datablocks;
	int64_t entry_opid;
	int64_t stack_size;
	strArray submodules;
	int _root_db_start;
	int _root_mb_start;
	int _root_eb_start;
} Module;

// special value for error reporting
#define TOKEN_REG_ID 125

typedef enum {
	VBRB_ERR_OK,
	VBRB_ERR_BLOCK_NAME_EXPECTED,
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
	VBRB_ERR_NON_PROC_CALL,
	VBRB_ERR_UNKNOWN_VAR_NAME,
	VBRB_ERR_UNCLOSED_PROC,
	VBRB_ERR_UNKNOWN_CONDITION,
	VBRB_ERR_INVALID_MODULE_NAME,
	VBRB_ERR_MODULE_NOT_FOUND,
	VBRB_ERR_MODULE_NOT_LOADED,
	VBRB_ERR_PREPROCESSOR_FAILURE,
	VBRB_ERR_NO_VAR,
	N_VBRB_ERRORS
} VBRBErrorCode;

typedef struct vbrb_error {
	VBRBErrorCode code;
	BRP* prep;
	Token loc;
	union {
		struct { // for VBRB_ERR_INVALID_ARG
			int8_t arg_id;
			uint8_t op_type;
			uint8_t expected_token_type;
		};
		int64_t item_size;
		char* mark_name;
		char* module_name;
		int var_count;
		BRBLoadError load_error;
	};
} VBRBError;

VBRBError compileModule(FILE* src, char* src_name, Module* dst, char* search_paths[], int flags);
void printVBRBError(FILE* dst, VBRBError err);
void cleanupVBRBCompiler(VBRBError status);

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
	[DS_INT8] = 1,
	[DS_INT16] = 2,
	[DS_VOID] = 0,
	[DS_INT32] = 4,
	[DS_BOOL] = 1,
	[DS_CONST] = 8,
	[DS_PTR] = 8,
	[DS_INT64] = 8,
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
	char* name;
	int8_t size;
	int32_t n_elements;
} Var;

typedef struct {
	DataSpecArray vars;
	int64_t prev_opid;
	int64_t call_id;
} ProcFrame;
declArray(ProcFrame);

#define N_REGS 10
#define N_USER_REGS 8
#define CONDREG1_ID 8
#define CONDREG2_ID 9
#define DEFAULT_STACK_SIZE 512 // 512 Kb, just like in JVM

#define BRBX_TRACING         0b00000001 // used in execModule function to enable value tracing, it helps in debugging memory access errors, but reduces execution speed
#define BRB_EXECUTABLE       0b00000010 // used in loadModule function to make the loaded module executable with execModule
#define BRBX_PRINT_MEMBLOCKS 0b00001000

typedef struct {
	sbuf heap;
	void* stack_brk;
	void* stack_head;
	void* prev_stack_head;
	sbufArray memblocks;
	int8_t exitcode;
	int op_id;
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
	char* src_path;
	int src_line;
} ExecEnv;

#define getCurStackSize(execenv_p, module_p) (int64_t)((execenv_p)->stack_brk + (module_p)->stack_size - (execenv_p)->stack_head)

void writeModule(Module* src, FILE* dst);
FILE* findModule(char* name, char* search_paths[]);
Module* mergeModule(Module* src, Module* dst);
void resolveModule(Module* dst, bool for_exec);
BRBLoadError preloadModule(FILE* src, Module* dst, char* search_paths[]);
BRBLoadError loadModule(FILE* src, Module* dst, char* search_paths[], int flags);
void printLoadError(BRBLoadError err);
ExecEnv execModule(Module* module, int8_t flags, char** args, volatile bool* interruptor);
void printExecState(FILE* fd, ExecEnv* env, Module* module);
void printRuntimeError(FILE* fd, ExecEnv* env, Module* module);

#endif // _BRB_
