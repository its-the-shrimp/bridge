#ifndef _BRB_
#define _BRB_

#include <br.h>

typedef enum {
/* 
Data Types:
 	i8 - a byte
	i16 - a 2-byte value
	i32 - a 4-byte value
	ptr - a pointer-sized value
	i64 - an 8-byte value
	int - any of the above

Notes:
 	[1] - the item must be either pointer-sized or of the size 1, 2, 4 or 8 bytes
	[2] - range of valid values for the operand is [0, 64)
	[3] - range of valid values for the operand is (-inf, 0) | (0, +inf)
	[4] - control flow operation: the structure of the stack before and after the block, formed by the operation and it's corresponding terminator, must be identical;
		every such operation must have a terminator present
*/
	BRB_OP_NOP,     // [] -> nop -> []
	// does nothing
	BRB_OP_END,     // [] -> end -> []
	// if inside a control flow block, closes the block; in other cases, stops execution of the program
	BRB_OP_INT8,    // [] -> int8 <x> -> [i8]
	// pushes 1 byte-sized integer literal <x> onto the stack
	BRB_OP_INT16,   // [] -> int16 <x> -> [i16]
	// pushes 2 byte-sized integer literal <x> onto the stack
	BRB_OP_INT32,   // [] -> int32 <x> -> [i32]
	// pushes 4 byte-sized integer literal <x> onto the stack
	BRB_OP_INT64,   // [] -> int64 <x> -> [i64]
	// pushes 8 byte-sized integer literal <x> onto the stack
	BRB_OP_INTPTR,  // [] -> intptr <x> -> [ptr]
	// pushes pointer-sized integer literal <x> onto the stack
	BRB_OP_ADDR,    // [A, *] -> addr <i> -> [A, *, ptr]
	// pushes address of the stack item at index <i> onto the stack; <i> refers to the index on the stack after pushing the address
	BRB_OP_DBADDR,  // [] -> dbaddr <i> -> [ptr]
	// pushes address of data block at index <i> onto the stack
	BRB_OP_LOAD,    // [A:ptr] -> load <n> -> [<n>]
	// replace address A with <n> bytes loaded from it
	BRB_OP_STR,     // [A:ptr, B] -> str -> []
	// store B at the address A; same as *A = B;
	BRB_OP_SYS,
	// executes system procedure <f> with arguments from the stack
	/* System calls:
		[A:ptr] -> sys exit // exits the program with exit code A
		[A:ptr, B:ptr, C:ptr] -> sys write -> [ptr] // write C bytes from address B to file descriptor A
		[A:ptr, B:ptr, C:ptr] -> sys read -> [ptr] // read C bytes from file descriptor A to address B
	*/
	BRB_OP_BUILTIN, // [] -> builtin <id> -> ptr
	// places a pointer-sized built-in constant on top of the stack
/* TODO
	BRB_OP_INC,     // [A:int] -> inc <n> -> [A:int]
	// increments A by <n> in-place
	BRB_OP_DEC,     // [A:int] -> dec <n> -> [A:int]
	// decrements A by <n> in-place
	BRB_OP_MUL,     // [A:int] -> mul <n> -> [A:int]
	// multiplies A by <n> in-place
	BRB_OP_DIV,     // [A:int] -> div <n> -> [A:int]
	// divides A by <n> in-place; <n> may not be zero
	BRB_OP_REM, 	// [A:int] -> rem <n> -> [A:int]
	// replaces A with the remainder of dividing A by <n>; <n> may not be 0
	BRB_OP_AND,	// [A:int] -> and <n> -> [A:int]
	// performs bitwise AND operation on A and <n>, stores the result in A
	BRB_OP_OR,      // [A:int] -> or <n> -> [A:int]
	// performs bitwise OR operation on A and <n>, stores the result in A
	BRB_OP_XOR,     // [A:int] -> xor <n> -> [A:int]
	// performs bitwise XOR operation on A and <n>, stores the result in A
	BRB_OP_SHL,     // [A:int] -> shl <n> -> [A:int]
	// shifts the value of A to the left by <n> bits; <n> must be in range [0, 64)
	BRB_OP_SHR,     // [A:int] -> shr <n> -> [A:int]
	// shifts the value of A to the right by <n> bits, shifting in zeros; <n> must be in range [0, 64)
	BRB_OP_SHRS,    // [A:int] -> shr:s <n> -> [A:int]
	// shifts the value of A to the right by <n> bits, shifting in copies of the sign bit; <n> must be in range [0, 64)
	BRB_OP_NOT,     // [A:int] -> inv -> [A:int]
	// inverts the bits of A
	BRB_OP_DUP,     // [A:x...] -> dup <i> -> [A:x...A:x]
	// duplicates stack item at index <i> on top of the stack
	BRB_OP_SWAP,    // [A:x...B:x] -> swap <i> -> [B:x...A:x]
	// swaps the contents of A and B, A being the stack head and B being at index <i>
	BRB_OP_EQU,     // [A:x, B:x] -> equ -> i8
	// compares A and B and replace them with a byte, the value of which will be 1 if they are equal, and 0 otherwise
	BRB_OP_NEQ,     // [A:x, B:x] -> neq -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if they are equal, and 0 otherwise
	BRB_OP_LTU,     // [A:int, B:int] -> ltu -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if A < B, and 0 otherwise; comparison is unsigned
	BRB_OP_LTS,     // [A:int, B:int] -> lts -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if A < B, and 0 otherwise; comparison is signed
	BRB_OP_GTU,     // [A:int, B:int] -> gtu -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if A > B, and 0 otherwise; comparison is unsigned
	BRB_OP_GTS,     // [A:int, B:int] -> gts -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if A > B, and 0 otherwise; comparison is signed
	BRB_OP_LEU,     // [A:int, B:int] -> leu -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if A <= B, and 0 otherwise; comparison is unsigned
	BRB_OP_LES,     // [A:int, B:int] -> les -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if A <= B, and 0 otherwise; comparison is signed
	BRB_OP_GEU,     // [A:int, B:int] -> geu -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if A >= B, and 0 otherwise; comparison is unsigned
	BRB_OP_GES,     // [A:int, B:int] -> ges -> i8
	// compares A and B and replaces them with a byte, the value of which will be 1 if A >= B, and 0 otherwise; comparison is signed
	BRB_OP_GOTO,    // [] -> goto <i> -> []
	// jumps to operation at index <i>
	BRB_OP_GOTOIF,  // [A:int] -> gotoif <i> -> []
	// jumps to operation at index <i> if A is not zero
	BRB_OP_GOTOIFN, // [A:int] -> gotoifn <i> -> []
	// jumps to operation at index <i> if A is zero
*/
	BRB_N_OPS
} BRB_OpType;

#define _BRB_opNames \
	fromcstr("nop"), \
	fromcstr("end"), \
	fromcstr("int8"), \
	fromcstr("int16"), \
	fromcstr("int32"), \
	fromcstr("intptr"), \
	fromcstr("int64"), \
	fromcstr("addr"), \
	fromcstr("dbaddr"), \
	fromcstr("load"), \
	fromcstr("str"), \
	fromcstr("sys"), \
	fromcstr("builtin")

static const sbuf BRB_opNames[] = { _BRB_opNames };
static_assert(sizeof(BRB_opNames) / sizeof(BRB_opNames[0]) == BRB_N_OPS, "not all BRB operations have their names defined in `BRB_opNames`");

#define BRB_OPF_HAS_OPERAND 1
static const uint8_t BRB_opFlags[] = {
	[BRB_OP_NOP] = 0,
	[BRB_OP_END] = 0,
	[BRB_OP_INT8] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_INT16] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_INT32] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_INT64] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_INTPTR] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_ADDR] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_DBADDR] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_LOAD] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_STR] = 0,
	[BRB_OP_SYS] = BRB_OPF_HAS_OPERAND,
	[BRB_OP_BUILTIN] = BRB_OPF_HAS_OPERAND
};
static_assert(sizeof(BRB_opFlags) / sizeof(BRB_opFlags[0]) == BRB_N_OPS, "not all BRB operations have their flags set defined in `BRB_opFlags`"); 

typedef struct {
	BRB_OpType type:8;
	union {
		uint64_t operand_u;	
		int64_t operand_s;
	};
} BRB_Op;
static_assert(sizeof(BRB_Op) <= 16, "`sizeof(BRB_Op) > 16`, that's no good, gotta save up on that precious memory");
declArray(BRB_Op);

// System Calls: crossplatform way to use system-dependent functionality
typedef enum {
	BRB_SYS_EXIT,
	BRB_SYS_WRITE,
	BRB_SYS_READ,
	BRB_N_SYSCALLS
} BRB_Syscall;

#define _BRB_syscallNames \
	fromcstr("exit"), \
	fromcstr("write"), \
	fromcstr("read") \

extern const sbuf BRB_syscallNames[];

extern const size_t BRB_syscallNArgs[];

// Built-ins: crossplatform way to get system-dependent constants
// all built-ins are of pointer size
typedef enum {
	BRB_BUILTIN_NULL,
	BRB_BUILTIN_STDIN,
	BRB_BUILTIN_STDOUT,
	BRB_BUILTIN_STDERR,
	BRB_N_BUILTINS
} BRB_Builtin;

extern const uintptr_t BRB_builtinValues[];
extern const sbuf BRB_builtinNames[];


typedef enum {
	BRB_DP_NONE,
	BRB_DP_BYTES,
	BRB_DP_INT16,
	BRB_DP_INT32,
	BRB_DP_INTPTR,
	BRB_DP_INT64,
	BRB_DP_TEXT,
	BRB_DP_DBADDR,
	BRB_DP_ZERO,
	BRB_DP_BUILTIN,
	BRB_N_DP_TYPES
} BRB_DataPieceType;
// TODO: add pre-evaluation data pieces that would pre-evaluate a block of code and embed it's output as a data piece

static const sbuf BRB_dataPieceNames[BRB_N_DP_TYPES] = {
	[BRB_DP_BYTES] =   fromcstr("bytes"),
	[BRB_DP_INT16] =   fromcstr("int16"),
	[BRB_DP_INT32] =   fromcstr("int32"),
	[BRB_DP_INTPTR] =  fromcstr("intptr"),
	[BRB_DP_INT64] =   fromcstr("int64"),
	[BRB_DP_TEXT]  =   fromcstr("text"),
	[BRB_DP_DBADDR] =  fromcstr("dbaddr"),
	[BRB_DP_ZERO] =    fromcstr("zero"),
	[BRB_DP_BUILTIN] = fromcstr("builtin")
};

typedef struct {
	BRB_DataPieceType type;
	union {
		sbuf data; // for PIECE_BYTES or PIECE_TEXT
		uint64_t operand_u;
		int64_t operand_s;
		BRB_Builtin builtin_id;
	};
} BRB_DataPiece;
declArray(BRB_DataPiece);

typedef struct {
	const char* name;
	union {
		BRB_DataPieceArray pieces;
		sbuf data;
	};
	bool is_mutable;
} BRB_DataBlock;
declArray(BRB_DataBlock);

typedef struct {
	BRB_OpArray seg_exec;
	BRB_DataBlockArray seg_data;
} BRB_Module;

#define BRB_HEADER_SIZE 8
#define BRB_V1_HEADER fromcstr("BRBv1\0\0\0")

typedef enum {
	BRB_ERR_OK,
	BRB_ERR_INVALID_DB,
	BRB_ERR_INVALID_HEADER,
	BRB_ERR_NO_HEADER,
	BRB_ERR_NO_DATA_SEG,
	BRB_ERR_NO_MEMORY,
	BRB_ERR_NO_EXEC_SEG,
	BRB_ERR_NO_OPCODE,
	BRB_ERR_INVALID_OPCODE,
	BRB_ERR_NO_OPERAND,
	BRB_ERR_INVALID_NAME,
	BRB_ERR_NAMES_NOT_RESOLVED,
	BRB_ERR_INVALID_BUILTIN,
	BRB_ERR_INVALID_SYSCALL,
	BRB_ERR_STACK_UNDERFLOW,
	BRB_ERR_OPERAND_TOO_LARGE,
	BRB_ERR_OPERAND_OUT_OF_RANGE,
	BRB_N_ERROR_TYPES
} BRB_ErrorType;

typedef struct {
	BRB_ErrorType type;
	uint8_t opcode;
	union {
		char header[BRB_HEADER_SIZE];
		BRB_Builtin builtin_id;
		BRB_Syscall syscall_id;
		struct {
			uint32_t expected_stack_length;
			uint32_t actual_stack_length;
		};
		uint64_t operand;
	};
} BRB_Error;

typedef struct BRB_stacknode_t* BRB_StackNode;
#define BRB_TYPE_PTR SIZE_MAX
struct BRB_stacknode_t {
	BRB_StackNode prev;
	const char* name;
	size_t size;
};
declArray(BRB_StackNode);

typedef struct {
	BRB_Module module;
	BRB_StackNodeArray stack_traces;
	BRB_Error error;
} BRB_ModuleBuilder;

typedef enum {
	BRB_EXC_CONTINUE,
	BRB_EXC_EXIT,
	BRB_EXC_END,
	BRB_EXC_INTERRUPT,
	BRB_EXC_UNKNOWN_OP,
	BRB_EXC_STACK_OVERFLOW,
	N_BRB_EXCS
} BRB_ExecStatusType;

typedef struct {
	BRB_ExecStatusType type;
	uint8_t exit_code; // for BRB_EXC_EXIT
} BRB_ExecStatus;

#define BRB_DEFAULT_STACK_SIZE (512 * 1024) /* 512 KBs, just like in JVM */
typedef struct {
	sbufArray seg_data;
	BRB_OpArray seg_exec;
	sbuf stack;
	char* stack_head;
	uint32_t exec_index;
	BRB_ExecStatus exec_status;
	sbuf* exec_argv;
	uint32_t exec_argc;
} BRB_ExecEnv;

typedef const char** field;
declArray(field);
defArray(field);
// implemented in `src/brb_core.c`
void BRB_printErrorMsg(FILE* dst, BRB_Error err);
char* BRB_getErrorMsg(FILE* dst, BRB_Error err);

fieldArray BRB_getNameFields(BRB_Module* module);
FILE* BRB_findModule(const char* module_name, const char* search_paths[]);
size_t BRB_getDataBlockId(BRB_Module* module, const char* name);

BRB_Error BRB_initModuleBuilder(BRB_ModuleBuilder* builder);
BRB_Error BRB_extractModule(BRB_ModuleBuilder builder, BRB_Module* dst);

BRB_Error BRB_addOp(BRB_ModuleBuilder* builder, BRB_Op op);
BRB_Error BRB_labelStackHead(BRB_ModuleBuilder* builder, const char* name);

BRB_Error BRB_addDataBlock(BRB_ModuleBuilder* builder, uint32_t* db_id_p, const char* name, bool is_mutable);
BRB_Error BRB_addDataPiece(BRB_ModuleBuilder* builder, uint32_t db_id, BRB_DataPiece piece);
size_t BRB_getStackItemSize(BRB_ModuleBuilder* builder, uint32_t op_id, uint32_t var_id);
size_t BRB_getStackItemRTOffset(BRB_ModuleBuilder* builder, uint32_t op_id, uint32_t var_id);
size_t BRB_getStackItemRTSize(BRB_ModuleBuilder* builder, uint32_t op_id, uint32_t var_id);

// implemented in `src/brb_write.c`
void BRB_writeModule(BRB_Module src, FILE* dst);

// implemented in `src/brb_load.c`
BRB_Error BRB_loadModule(FILE* src, BRB_Module* dst);

// implemented in `src/brb_exec.c`
BRB_Error BRB_initExecEnv(BRB_ExecEnv* env, BRB_Module module, size_t stack_size);
void BRB_execModule(BRB_ExecEnv* env, char* args[], volatile bool* interruptor);

#endif // _BRB_
