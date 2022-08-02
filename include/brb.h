#ifndef _BRB_
#define _BRB_

#include <br.h>

typedef enum {
	OP_NONE,
	OP_END,
	OP_MARK,
	OP_SET, // uses Op::dst_reg and Op::value
	OP_SETR, // uses Op::dst_reg and Op::src_reg
	OP_SETD, // uses Op::dst_reg and Op::symbol_id
	OP_SETB, // uses Op::dst_reg and Op::symbol_id
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
	OP_VAR, // uses Op::new_var_size
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
	OP_SETC, // uses Op::cond_arg and Op::dst_reg
	OP_DELNV, // uses Op::symbol_id
	OP_LD64S, // uses Op::dst_reg and Op::src_reg
	OP_LD32S, // uses Op::dst_reg and Op::src_reg
	OP_LD16S, // uses Op::dst_reg and Op::src_reg
	OP_LD8S, // uses Op::dst_reg and Op::src_reg
	OP_LDVS, // uses Op::src_reg, Op::symbol_id and Op::var_size
	OP_SX32, // uses Op::dst_reg and Op::src_reg
	OP_SX16, // uses Op::dst_reg and Op::src_reg
	OP_SX8, // uses Op::dst_reg and Op::src_reg
	OP_MOD, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_MODS, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_MODR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_MODSR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	N_OPS
} OpType;

#define OPF_USES_DST_REG 1
#define OPF_USES_SRC_REG 2
#define OPF_USES_SRC2_REG 4
#define OPF_USES_VALUE 8
#define OPF_UNCONDITIONAL 16
#define OPF_USES_SYMBOL_ID 32
#define OPF_USES_SYSCALL_ID 64
#define OPF_USES_OP_OFFSET 128
#define OPF_USES_MARK_NAME 256
#define OPF_USES_NEW_VAR_SIZE 512
#define OPF_USES_VAR_SIZE 1024
#define OPF_USES_COND_ARG 2048
#define OPF_USES_MODULE_ID 4096
#define OPF_REQ_NAME_RESOLUTION 8192

#define OPF_IS_2REG_IMM (OPF_USES_DST_REG | OPF_USES_SRC_REG | OPF_USES_VALUE)
#define OPF_IS_3REG (OPF_USES_DST_REG | OPF_USES_SRC_REG | OPF_USES_SRC2_REG)
#define OPF_IS_2REG (OPF_USES_DST_REG | OPF_USES_SRC_REG)

// information on the operations, i.e. what fields do they use, can they be conditional etc.
const static unsigned short op_flags[N_OPS] = {
	[OP_NONE] = 0,
	[OP_END] = OPF_UNCONDITIONAL,
	[OP_MARK] = OPF_UNCONDITIONAL,
	[OP_SET] = OPF_USES_DST_REG | OPF_USES_VALUE,
	[OP_SETR] = OPF_IS_2REG,
	[OP_SETD] = OPF_USES_DST_REG | OPF_USES_SYMBOL_ID | OPF_USES_MODULE_ID | OPF_REQ_NAME_RESOLUTION,
	[OP_SETB] = OPF_USES_DST_REG | OPF_USES_SYMBOL_ID,
	[OP_ADD] = OPF_IS_2REG_IMM, 
	[OP_ADDR] = OPF_IS_3REG,
	[OP_SUB] = OPF_IS_2REG_IMM,
	[OP_SUBR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_SYS] = OPF_USES_SYSCALL_ID, // uses Op::syscall_id
	[OP_GOTO] = OPF_USES_OP_OFFSET, // uses Op::op_offset
	[OP_CMP] = OPF_USES_SRC_REG | OPF_USES_VALUE, // uses Op::src_reg and Op::value
	[OP_CMPR] = OPF_USES_SRC_REG | OPF_USES_SRC2_REG, // uses Op::src_reg and Op::src2_reg
	[OP_AND] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_ANDR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_OR] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_ORR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_NOT] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_XOR] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_XORR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_SHL] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_SHLR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_SHR] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_SHRR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_SHRS] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_SHRSR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_PROC] = OPF_USES_MARK_NAME | OPF_REQ_NAME_RESOLUTION, // uses Op::mark_name
	[OP_CALL] = OPF_USES_SYMBOL_ID | OPF_USES_MODULE_ID | OPF_REQ_NAME_RESOLUTION, // uses Op::symbol_id
	[OP_RET] = 0,
	[OP_ENDPROC] = OPF_UNCONDITIONAL,
	[OP_LD64] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_STR64] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_LD32] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_STR32] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_LD16] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_STR16] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_LD8] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_STR8] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_VAR] = OPF_USES_NEW_VAR_SIZE | OPF_UNCONDITIONAL, // uses Op::new_var_size
	[OP_SETV] = OPF_USES_DST_REG | OPF_USES_SYMBOL_ID, // uses Op::dst_reg and Op::symbol_id
	[OP_MUL] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_MULR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_DIV] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_DIVR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg 
	[OP_DIVS] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_DIVSR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg 
	[OP_EXTPROC] = OPF_USES_MARK_NAME | OPF_UNCONDITIONAL | OPF_REQ_NAME_RESOLUTION, // uses Op::mark_name
	[OP_LDV] = OPF_USES_DST_REG | OPF_USES_SYMBOL_ID | OPF_USES_VAR_SIZE, // uses Op::src_reg, Op::symbol_id and Op::var_size
	[OP_STRV] = OPF_USES_SRC_REG | OPF_USES_SYMBOL_ID | OPF_USES_VAR_SIZE, // uses Op::dst_reg, Op::symbol_id and Op::var_size
	[OP_POPV] = OPF_USES_DST_REG | OPF_USES_VAR_SIZE | OPF_UNCONDITIONAL, // uses Op::dst_reg, Op::var_size
	[OP_PUSHV] = OPF_USES_SRC_REG | OPF_USES_VAR_SIZE | OPF_UNCONDITIONAL, // uses Op::src_reg and Op::var_size
	[OP_ATF] = OPF_USES_MARK_NAME | OPF_UNCONDITIONAL | OPF_REQ_NAME_RESOLUTION, // uses Op::mark_name
	[OP_ATL] = OPF_USES_SYMBOL_ID | OPF_UNCONDITIONAL, // uses Op::symbol_id
	[OP_SETC] = OPF_USES_DST_REG | OPF_USES_COND_ARG | OPF_USES_DST_REG, // uses Op::cond_arg and Op::dst_reg
	[OP_DELNV] = OPF_USES_SYMBOL_ID | OPF_UNCONDITIONAL, // uses Op::symbol_id
	[OP_LD64S] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_LD32S] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_LD16S] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_LD8S] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_LDVS] = OPF_USES_DST_REG | OPF_USES_SYMBOL_ID | OPF_USES_VAR_SIZE, // uses Op::src_reg, Op::symbol_id and Op::var_size
	[OP_SX32] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_SX16] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_SX8] = OPF_IS_2REG, // uses Op::dst_reg and Op::src_reg
	[OP_MOD] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_MODS] = OPF_IS_2REG_IMM, // uses Op::dst_reg, Op::src_reg and Op::value
	[OP_MODR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	[OP_MODSR] = OPF_IS_3REG, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
};

#define _opNames \
	BRP_KEYWORD("nop"), \
	BRP_KEYWORD("end"), \
	BRP_KEYWORD("mark"), \
	BRP_KEYWORD("set"), \
	BRP_KEYWORD("setr"), \
	BRP_KEYWORD("setd"), \
	BRP_KEYWORD("setb"), \
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
	BRP_KEYWORD("@l"), \
	BRP_KEYWORD("setc"), \
	BRP_KEYWORD("delnv"), \
	BRP_KEYWORD("ld64s"), \
	BRP_KEYWORD("ld32s"), \
	BRP_KEYWORD("ld16s"), \
	BRP_KEYWORD("ld8s"), \
	BRP_KEYWORD("ldvs"), \
	BRP_KEYWORD("sx32"), \
	BRP_KEYWORD("sx16"), \
	BRP_KEYWORD("sx8"), \
	BRP_KEYWORD("mod"), \
	BRP_KEYWORD("mods"), \
	BRP_KEYWORD("modr"), \
	BRP_KEYWORD("modsr") \

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
	BRB_ERR_NO_OPCODE,
	BRB_ERR_UNRESOLVED_NAMES,
	BRB_ERR_NO_OP_ARG,
	BRB_ERR_INVALID_OPCODE,
	BRB_ERR_NO_DATA_SEGMENT,
	BRB_ERR_NO_EXEC_SEGMENT,
	BRB_ERR_NO_NAME_SEGMENT,
	BRB_ERR_NO_NAME_SPEC,
	BRB_ERR_INVALID_COND_ID,
	BRB_ERR_NO_COND_ID,
	BRB_ERR_NO_STACK_SIZE,
	BRB_ERR_NO_ENTRY,
	BRB_ERR_NO_LOAD_SEGMENT,
	BRB_ERR_MODULE_NOT_FOUND,
	BRB_ERR_INVALID_BLOCK,
	BRB_ERR_UNRESOLVED_DB_REF,
	BRB_ERR_UNRESOLVED_PROC_REF,
	N_BRB_ERRORS
} BRBLoadErrorCode;

typedef struct {
	BRBLoadErrorCode code;
	union {
		struct { // for BRB_ERR_UNRESOLVED_*_REF
			const char* module_name; // also for BRB_ERR_MODULE_NOT_FOUND
			const char* mark_name;
			const char* in_module_name;
			const char* in_proc_name;
			const char* in_db_name;
		};
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

static sbuf conditionNames[N_CONDS] = { _conditionNames };

typedef struct {
	int8_t type;
	uint8_t dst_reg:4;
	uint8_t src_reg:4;
	uint8_t var_size:4;
	uint8_t cond_id:4;
	uint8_t cond_arg:4;
	uint8_t src2_reg:4;
	uint32_t module_id;
	union {
		uint64_t value;
		int32_t symbol_id;
		int64_t new_var_size;
		int64_t op_offset;
		char* mark_name;
		uint8_t syscall_id; 
	};
} Op;
declArray(Op);
static_assert(sizeof(Op) <= 16, "checking compactness of operations' storage");

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

typedef enum {
	PIECE_NONE,
	PIECE_BYTES,
	PIECE_INT16,
	PIECE_INT32,
	PIECE_INT64,
	PIECE_TEXT,
	PIECE_DB_ADDR,
	PIECE_ZERO,
	N_PIECE_TYPES
} DataPieceType;

static const sbuf dataPieceNames[N_PIECE_TYPES] = {
	[PIECE_INT16] = CSBUF(".int16"),
	[PIECE_INT32] = CSBUF(".int32"),
	[PIECE_INT64] = CSBUF(".int64"),
	[PIECE_DB_ADDR] = CSBUF(".db_addr"),
	[PIECE_ZERO] = CSBUF(".zero")
};

typedef struct {
	DataPieceType type;
	union {
		sbuf data; // for PIECE_BYTES or PIECE_TEXT
		int64_t integer; // for PIECE_INT*
		struct { // for PIECE_*_ADDR
			uint32_t module_id;
			union {
				char* mark_name; // if not resolved
				int64_t symbol_id; // after it's resolved
			};
		};
		int64_t n_bytes; // for PIECE_ZERO
	};
} DataPiece;
declArray(DataPiece);

typedef struct {
	char* name;
	DataPieceArray pieces;
	bool is_mutable;
} DataBlock;
declArray(DataBlock);

typedef char* str;
declArray(str);

typedef struct {
	char* name;
	int64_t size;
} Var;

typedef struct {
	int es_offset;
	int es_length;
	int ds_offset;
	int ds_length;
	const char* name;
	bool direct;
} Submodule;
declArray(Submodule);

typedef struct {
	OpArray seg_exec;
	DataBlockArray seg_data;
	int64_t stack_size;
	SubmoduleArray submodules;
} Module;

// special value for error reporting
#define TOKEN_REG_ID 125
#define TOKEN_COND 126

typedef enum {
	VBRB_ERR_OK,
	VBRB_ERR_BLOCK_NAME_EXPECTED,
	VBRB_ERR_NO_MEMORY,
	VBRB_ERR_BLOCK_SPEC_EXPECTED,
	VBRB_ERR_BLOCK_SIZE_EXPECTED,
	VBRB_ERR_INVALID_ARG,
	VBRB_ERR_INVALID_OP,
	VBRB_ERR_UNKNOWN_SYSCALL,
	VBRB_ERR_EXEC_MARK_NOT_FOUND,
	VBRB_ERR_DATA_BLOCK_NOT_FOUND,
	VBRB_ERR_INVALID_REG_ID,
	VBRB_ERR_UNKNOWN_BUILTIN,
	VBRB_ERR_UNCONDITIONAL_OP,
	VBRB_ERR_UNKNOWN_VAR_NAME,
	VBRB_ERR_UNCLOSED_PROC,
	VBRB_ERR_UNKNOWN_CONDITION,
	VBRB_ERR_INVALID_MODULE_NAME,
	VBRB_ERR_MODULE_NOT_FOUND,
	VBRB_ERR_MODULE_NOT_LOADED,
	VBRB_ERR_INVALID_NAME,
	VBRB_ERR_NO_VAR,
	VBRB_ERR_DELNV_TOO_FEW_VARS,
	VBRB_ERR_VAR_TOO_LARGE,
	VBRB_ERR_INVALID_DATA_BLOCK_FMT,
	VBRB_ERR_OP_OUTSIDE_OF_PROC,
	VBRB_ERR_INVALID_VAR_SIZE,
	VBRB_ERR_INVALID_VAR_OFFSET,
	N_VBRB_ERRORS
} VBRBErrorCode;

typedef struct vbrb_error {
	VBRBErrorCode code;
	BRP* prep;
	Token loc;
	union {
		struct { // for VBRB_ERR_INVALID_ARG or VBRB_ERR_INVALID_DATA_BLOCK_FMT
			int8_t arg_id;
			uint8_t op_type;
#			define data_piece_type op_type
			uint8_t expected_token_type;
		};
		int64_t item_size;
		char* mark_name;
		int var_count;
		BRBLoadError load_error;
		Var var;
	};
} VBRBError;

static sbuf vbrb_symbols[] = {
	BRP_SYMBOL("{"),
	BRP_SYMBOL("}"),
	BRP_SYMBOL(":"),
	BRP_HIDDEN_SYMBOL(" "),
	BRP_HIDDEN_SYMBOL("\t"),
	BRP_HIDDEN_SYMBOL("\n"),
	(sbuf){0}
};

static sbuf vbrb_keywords[] = {
	_opNames,
	_syscallNames,
	_conditionNames,
	BRP_KEYWORD(".data"),
	BRP_KEYWORD(".load"),
	BRP_KEYWORD("int16"),
	BRP_KEYWORD("int32"),
	BRP_KEYWORD("int64"),
	BRP_KEYWORD("db_addr"),
	BRP_KEYWORD("mut"),
	BRP_KEYWORD("zero"),
	(sbuf){0}
};

VBRBError compileVBRB(FILE* src, const char* src_name, Module* dst, const char* search_paths[]);
void printVBRBError(FILE* dst, VBRBError err);
void cleanupVBRBCompiler(VBRBError status);

#define N_REGS 11
#define ZEROREG_ID  8
#define N_USER_REGS 9
#define CONDREG1_ID 9
#define CONDREG2_ID 10
#define DEFAULT_STACK_SIZE 512 // 512 Kb, just like in JVM
#define STACKFRAME_SIZE 16

typedef struct execEnv {
	void* stack_brk;
	void* stack_head;
	void* prev_stack_head;
	sbufArray seg_data;
	uint8_t exitcode;
	int op_id;
	uint64_t* registers;
	int exec_argc;
	sbuf* exec_argv;
	char* src_path;
	int src_line;
	bool calling_proc;
	bool (**exec_callbacks) (struct execEnv*, Module*, const Op*);
} ExecEnv;

typedef bool (*ExecCallback) (ExecEnv*, Module*, const Op*);

#define getCurStackSize(execenv_p, module_p) (int64_t)((execenv_p)->stack_brk + (module_p)->stack_size - (execenv_p)->stack_head)

// implemented in `src/brb_write.c`
void writeModule(Module* src, FILE* dst);
// implemented in `src/brb_core.c`
FILE* findModule(const char* name, const char* search_paths[]);
Submodule* getOpSubmodule(Module* module, Op* op);
Submodule* getDataBlockSubmodule(Module* module, DataBlock* block);
Module* mergeModule(Module* restrict src, Module* dst, char* src_name);
Submodule getRootSubmodule(Module* module, const char* name);
// implemented in `src/brb_load.c`
BRBLoadError loadModule(FILE* src, Module* dst, const char* search_paths[]);
void printLoadError(FILE* dst, BRBLoadError err);
#ifdef _BRB_INTERNAL
BRBLoadError resolveModule(Module* dst);
BRBLoadError preloadModule(FILE* src, Module* dst, const char* search_paths[]);
#endif
// implemented in `src/brb_optimize.c`
void optimizeModule(Module* module, const char* search_paths[], FILE* output, unsigned int level);
// implemented in `src/brb_exec.c`
void initExecEnv(ExecEnv* env, Module* module, const char** args);
bool addDefaultCallback(ExecEnv* env, ExecCallback callback);
bool addCallBack(ExecEnv* env, uint8_t op_id, ExecCallback callback);
void execOp(ExecEnv* env, Module* module);
void execModule(ExecEnv* env, Module* module, volatile bool* interruptor);
// implemented in `src/brb_compile.c`
void compileModule(Module* src, FILE* dst);

#endif // _BRB_
