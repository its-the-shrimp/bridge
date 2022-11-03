#define BRIDGE_IMPLEMENTATION
#define _BR_INTERNAL
#include <br.h>
#include <unistd.h>
#include <sys/param.h>
#include <math.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <spawn.h>
#include <sys/stat.h>
#include <sys/wait.h>
#define ARENA_IMPLEMENTATION
#include <external/arena.h>
extern char** environ;

implArray(BR_Struct);
implArray(BR_Proc);
implArray(BR_Type);
implArray(BR_Op);
implArray(BR_DataBlock);
implArray(BR_StackNode);
implArray(BR_StackNodeArray);

#define BR_SNF_STACKFRAME 0x1

const sbuf BR_opNames[]   = {
	[BR_OP_NOP]       = sbuf_fromcstr("nop"),
	[BR_OP_END]       = sbuf_fromcstr("end"),
	[BR_OP_I8]        = sbuf_fromcstr("i8"),
	[BR_OP_I16]       = sbuf_fromcstr("i16"),
	[BR_OP_I32]       = sbuf_fromcstr("i32"),
	[BR_OP_PTR]       = sbuf_fromcstr("ptr"),
	[BR_OP_I64]       = sbuf_fromcstr("i64"),
	[BR_OP_ADDR]      = sbuf_fromcstr("addr"),
	[BR_OP_DBADDR]    = sbuf_fromcstr("dbaddr"),
	[BR_OP_SYS]       = sbuf_fromcstr("sys"),
	[BR_OP_BUILTIN]   = sbuf_fromcstr("builtin"),
	[BR_OP_ADD]       = sbuf_fromcstr("add"),
	[BR_OP_ADDI]      = sbuf_fromcstr("add-i"),
	[BR_OP_ADDIAT8]   = sbuf_fromcstr("add-i@8"),
	[BR_OP_ADDIAT16]  = sbuf_fromcstr("add-i@16"),
	[BR_OP_ADDIAT32]  = sbuf_fromcstr("add-i@32"),
	[BR_OP_ADDIATP]   = sbuf_fromcstr("add-i@p"),
	[BR_OP_ADDIAT64]  = sbuf_fromcstr("add-i@64"),
	[BR_OP_SUB]       = sbuf_fromcstr("sub"),
	[BR_OP_SUBI]      = sbuf_fromcstr("sub-i"),
	[BR_OP_SUBIAT8]   = sbuf_fromcstr("sub-i@8"),
	[BR_OP_SUBIAT16]  = sbuf_fromcstr("sub-i@16"),
	[BR_OP_SUBIAT32]  = sbuf_fromcstr("sub-i@32"),
	[BR_OP_SUBIATP]   = sbuf_fromcstr("sub-i@p"),
	[BR_OP_SUBIAT64]  = sbuf_fromcstr("sub-i@64"),
	[BR_OP_MUL]       = sbuf_fromcstr("mul"),
	[BR_OP_MULI]      = sbuf_fromcstr("mul-i"),
	[BR_OP_MULIAT8]   = sbuf_fromcstr("mul-i@8"),
	[BR_OP_MULIAT16]  = sbuf_fromcstr("mul-i@16"),
	[BR_OP_MULIAT32]  = sbuf_fromcstr("mul-i@32"),
	[BR_OP_MULIATP]   = sbuf_fromcstr("mul-i@p"),
	[BR_OP_MULIAT64]  = sbuf_fromcstr("mul-i@64"),
	[BR_OP_DIV]       = sbuf_fromcstr("div"),
	[BR_OP_DIVI]      = sbuf_fromcstr("div-i"),
	[BR_OP_DIVIAT8]   = sbuf_fromcstr("div-i@8"),
	[BR_OP_DIVIAT16]  = sbuf_fromcstr("div-i@16"),
	[BR_OP_DIVIAT32]  = sbuf_fromcstr("div-i@32"),
	[BR_OP_DIVIATP]   = sbuf_fromcstr("div-i@p"),
	[BR_OP_DIVIAT64]  = sbuf_fromcstr("div-i@64"),
	[BR_OP_DIVS]      = sbuf_fromcstr("divs"),
	[BR_OP_DIVSI]     = sbuf_fromcstr("divs-i"),
	[BR_OP_DIVSIAT8]  = sbuf_fromcstr("divs-i@8"),
	[BR_OP_DIVSIAT16] = sbuf_fromcstr("divs-i@16"),
	[BR_OP_DIVSIAT32] = sbuf_fromcstr("divs-i@32"),
	[BR_OP_DIVSIATP]  = sbuf_fromcstr("divs-i@p"),
	[BR_OP_DIVSIAT64] = sbuf_fromcstr("divs-i@64"),
	[BR_OP_MOD]       = sbuf_fromcstr("mod"),
	[BR_OP_MODI]      = sbuf_fromcstr("mod-i"),
	[BR_OP_MODIAT8]   = sbuf_fromcstr("mod-i@8"),
	[BR_OP_MODIAT16]  = sbuf_fromcstr("mod-i@16"),
	[BR_OP_MODIAT32]  = sbuf_fromcstr("mod-i@32"),
	[BR_OP_MODIATP]   = sbuf_fromcstr("mod-i@p"),
	[BR_OP_MODIAT64]  = sbuf_fromcstr("mod-i@64"),
	[BR_OP_MODS]      = sbuf_fromcstr("mods"),
	[BR_OP_MODSI]     = sbuf_fromcstr("mods-i"),
	[BR_OP_MODSIAT8]  = sbuf_fromcstr("mods-i@8"),
	[BR_OP_MODSIAT16] = sbuf_fromcstr("mods-i@16"),
	[BR_OP_MODSIAT32] = sbuf_fromcstr("mods-i@32"),
	[BR_OP_MODSIATP]  = sbuf_fromcstr("mods-i@p"),
	[BR_OP_MODSIAT64] = sbuf_fromcstr("mods-i@64"),
	[BR_OP_AND]       = sbuf_fromcstr("and"),
	[BR_OP_ANDI]      = sbuf_fromcstr("and-i"),
	[BR_OP_ANDIAT8]   = sbuf_fromcstr("and-i@8"),
	[BR_OP_ANDIAT16]  = sbuf_fromcstr("and-i@16"),
	[BR_OP_ANDIAT32]  = sbuf_fromcstr("and-i@32"),
	[BR_OP_ANDIATP]   = sbuf_fromcstr("and-i@p"),
	[BR_OP_ANDIAT64]  = sbuf_fromcstr("and-i@64"),
	[BR_OP_OR]        = sbuf_fromcstr("or"),
	[BR_OP_ORI]       = sbuf_fromcstr("or-i"),
	[BR_OP_ORIAT8]    = sbuf_fromcstr("or-i@8"),
	[BR_OP_ORIAT16]   = sbuf_fromcstr("or-i@16"),
	[BR_OP_ORIAT32]   = sbuf_fromcstr("or-i@32"),
	[BR_OP_ORIATP]    = sbuf_fromcstr("or-i@p"),
	[BR_OP_ORIAT64]   = sbuf_fromcstr("or-i@64"),
	[BR_OP_XOR]       = sbuf_fromcstr("xor"),
	[BR_OP_XORI]      = sbuf_fromcstr("xor-i"),
	[BR_OP_XORIAT8]   = sbuf_fromcstr("xor-i@8"),
	[BR_OP_XORIAT16]  = sbuf_fromcstr("xor-i@16"),
	[BR_OP_XORIAT32]  = sbuf_fromcstr("xor-i@32"),
	[BR_OP_XORIATP]   = sbuf_fromcstr("xor-i@p"),
	[BR_OP_XORIAT64]  = sbuf_fromcstr("xor-i@64"),
	[BR_OP_SHL]       = sbuf_fromcstr("shl"),
	[BR_OP_SHLI]      = sbuf_fromcstr("shl-i"),
	[BR_OP_SHLIAT8]   = sbuf_fromcstr("shl-i@8"),
	[BR_OP_SHLIAT16]  = sbuf_fromcstr("shl-i@16"),
	[BR_OP_SHLIAT32]  = sbuf_fromcstr("shl-i@32"),
	[BR_OP_SHLIATP]   = sbuf_fromcstr("shl-i@p"),
	[BR_OP_SHLIAT64]  = sbuf_fromcstr("shl-i@64"),
	[BR_OP_SHR]       = sbuf_fromcstr("shr"),
	[BR_OP_SHRI]      = sbuf_fromcstr("shr-i"),
	[BR_OP_SHRIAT8]   = sbuf_fromcstr("shr-i@8"),
	[BR_OP_SHRIAT16]  = sbuf_fromcstr("shr-i@16"),
	[BR_OP_SHRIAT32]  = sbuf_fromcstr("shr-i@32"),
	[BR_OP_SHRIATP]   = sbuf_fromcstr("shr-i@p"),
	[BR_OP_SHRIAT64]  = sbuf_fromcstr("shr-i@64"),
	[BR_OP_SHRS]      = sbuf_fromcstr("shrs"),
	[BR_OP_SHRSI]     = sbuf_fromcstr("shrs-i"),
	[BR_OP_SHRSIAT8]  = sbuf_fromcstr("shrs-i@8"),
	[BR_OP_SHRSIAT16] = sbuf_fromcstr("shrs-i@16"),
	[BR_OP_SHRSIAT32] = sbuf_fromcstr("shrs-i@32"),
	[BR_OP_SHRSIATP]  = sbuf_fromcstr("shrs-i@p"),
	[BR_OP_SHRSIAT64] = sbuf_fromcstr("shrs-i@64"),
	[BR_OP_NOT]       = sbuf_fromcstr("not"),
	[BR_OP_NOTAT8]    = sbuf_fromcstr("not-@8"),
	[BR_OP_NOTAT16]   = sbuf_fromcstr("not-@16"),
	[BR_OP_NOTAT32]   = sbuf_fromcstr("not-@32"),
	[BR_OP_NOTATP]    = sbuf_fromcstr("not-@p"),
	[BR_OP_NOTAT64]   = sbuf_fromcstr("not-@64"),
	[BR_OP_DROP]      = sbuf_fromcstr("drop"),
	[BR_OP_NEW]       = sbuf_fromcstr("new"),
	[BR_OP_ZERO]      = sbuf_fromcstr("zero"),
	[BR_OP_GET]       = sbuf_fromcstr("get"),
	[BR_OP_SETAT]     = sbuf_fromcstr("set-at"),
	[BR_OP_GETFROM]   = sbuf_fromcstr("get-from"),
	[BR_OP_COPY]      = sbuf_fromcstr("copy"),
};
static_assert(sizeof(BR_opNames) / sizeof(BR_opNames[0]) == BR_N_OPS, "not all BRB operations have their names defined");

#define SET_OPERAND_TYPE(type) (BR_OPERAND_##type << 0)
#define SET_ADDR_OP_TYPE(type) (BR_ADDR_##type    << 3)
const uint64_t BR_opFlags[] = {
	[BR_OP_NOP]       = 0,
	[BR_OP_END]       = 0,
	[BR_OP_I8]        = SET_OPERAND_TYPE(INT8),
	[BR_OP_I16]       = SET_OPERAND_TYPE(INT),
	[BR_OP_I32]       = SET_OPERAND_TYPE(INT),
	[BR_OP_PTR]       = SET_OPERAND_TYPE(INT),
	[BR_OP_I64]       = SET_OPERAND_TYPE(INT),
	[BR_OP_ADDR]      = SET_OPERAND_TYPE(VAR_NAME),
	[BR_OP_DBADDR]    = SET_OPERAND_TYPE(DB_NAME),
	[BR_OP_SYS]       = SET_OPERAND_TYPE(SYSCALL_NAME),
	[BR_OP_BUILTIN]   = SET_OPERAND_TYPE(BUILTIN),
	[BR_OP_ADD]       = 0,
	[BR_OP_ADDI]      = SET_OPERAND_TYPE(INT),
	[BR_OP_ADDIAT8]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_ADDIAT16]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_ADDIAT32]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_ADDIATP]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_ADDIAT64]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_SUB]       = 0,
	[BR_OP_SUBI]      = SET_OPERAND_TYPE(INT),
	[BR_OP_SUBIAT8]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_SUBIAT16]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_SUBIAT32]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_SUBIATP]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_SUBIAT64]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_MUL]       = 0,
	[BR_OP_MULI]      = SET_OPERAND_TYPE(INT),
	[BR_OP_MULIAT8]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_MULIAT16]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_MULIAT32]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_MULIATP]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_MULIAT64]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_DIV]       = 0,
	[BR_OP_DIVI]      = SET_OPERAND_TYPE(INT),
	[BR_OP_DIVIAT8]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_DIVIAT16]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_DIVIAT32]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_DIVIATP]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_DIVIAT64]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_DIVS]      = 0,
	[BR_OP_DIVSI]     = SET_OPERAND_TYPE(INT),
	[BR_OP_DIVSIAT8]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_DIVSIAT16] = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_DIVSIAT32] = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_DIVSIATP]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_DIVSIAT64] = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_MOD]       = 0,
	[BR_OP_MODI]      = SET_OPERAND_TYPE(INT),
	[BR_OP_MODIAT8]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_MODIAT16]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_MODIAT32]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_MODIATP]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_MODIAT64]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_MODS]      = 0,
	[BR_OP_MODSI]     = SET_OPERAND_TYPE(INT),
	[BR_OP_MODSIAT8]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_MODSIAT16] = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_MODSIAT32] = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_MODSIATP]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_MODSIAT64] = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_AND]       = 0,
	[BR_OP_ANDI]      = SET_OPERAND_TYPE(INT),
	[BR_OP_ANDIAT8]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_ANDIAT16]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_ANDIAT32]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_ANDIATP]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_ANDIAT64]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_OR]        = 0,
	[BR_OP_ORI]       = SET_OPERAND_TYPE(INT),
	[BR_OP_ORIAT8]    = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_ORIAT16]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_ORIAT32]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_ORIATP]    = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_ORIAT64]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_XOR]       = 0,
	[BR_OP_XORI]      = SET_OPERAND_TYPE(INT),
	[BR_OP_XORIAT8]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_XORIAT16]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_XORIAT32]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_XORIATP]   = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_XORIAT64]  = SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_SHL]       = 0,
	[BR_OP_SHLI]      = SET_OPERAND_TYPE(INT8),
	[BR_OP_SHLIAT8]   = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_SHLIAT16]  = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_SHLIAT32]  = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_SHLIATP]   = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_SHLIAT64]  = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_SHR]       = 0,
	[BR_OP_SHRI]      = SET_OPERAND_TYPE(INT8),
	[BR_OP_SHRIAT8]   = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_SHRIAT16]  = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_SHRIAT32]  = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_SHRIATP]   = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_SHRIAT64]  = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_SHRS]      = 0,
	[BR_OP_SHRSI]     = SET_OPERAND_TYPE(INT8),
	[BR_OP_SHRSIAT8]  = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I8),
	[BR_OP_SHRSIAT16] = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I16),
	[BR_OP_SHRSIAT32] = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I32),
	[BR_OP_SHRSIATP]  = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(PTR),
	[BR_OP_SHRSIAT64] = SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I64),
	[BR_OP_NOT]       = 0,
	[BR_OP_NOTAT8]    = SET_ADDR_OP_TYPE(I8),
	[BR_OP_NOTAT16]   = SET_ADDR_OP_TYPE(I16),
	[BR_OP_NOTAT32]   = SET_ADDR_OP_TYPE(I32),
	[BR_OP_NOTATP]    = SET_ADDR_OP_TYPE(PTR),
	[BR_OP_NOTAT64]   = SET_ADDR_OP_TYPE(I64),
	[BR_OP_DROP]      = 0,
	[BR_OP_NEW]       = SET_OPERAND_TYPE(TYPE),
	[BR_OP_ZERO]      = SET_OPERAND_TYPE(TYPE),
	[BR_OP_GET]       = SET_OPERAND_TYPE(VAR_NAME),
	[BR_OP_SETAT]     = 0,
	[BR_OP_GETFROM]   = SET_OPERAND_TYPE(TYPE),
	[BR_OP_COPY]      = SET_OPERAND_TYPE(TYPE)
};
static_assert(sizeof(BR_opFlags) / sizeof(BR_opFlags[0]) == BR_N_OPS, "not all BRB operations have their flags defined");

const sbuf BR_syscallNames[] = {
	[BR_SYS_EXIT]  = sbuf_fromcstr("exit"),
	[BR_SYS_WRITE] = sbuf_fromcstr("write"),
	[BR_SYS_READ]  = sbuf_fromcstr("read")
};
static_assert(sizeof(BR_syscallNames) / sizeof(BR_syscallNames[0]) == BR_N_SYSCALLS, "not all BRB syscalls have their names defined");

const uintptr_t BR_builtinValues[] = {
	[BR_BUILTIN_NULL] = (uintptr_t)NULL,
	[BR_BUILTIN_STDIN] = STDIN_FILENO,
	[BR_BUILTIN_STDOUT] = STDOUT_FILENO,
	[BR_BUILTIN_STDERR] = STDERR_FILENO,
};
static_assert(sizeof(BR_builtinValues) / sizeof(BR_builtinValues[0]) == BR_N_BUILTINS, "not all BRB built-ins have their runtime values defined");

const sbuf BR_builtinNames[] = {
	[BR_BUILTIN_NULL] = sbuf_fromcstr("NULL"),
	[BR_BUILTIN_STDIN] = sbuf_fromcstr("STDIN"),
	[BR_BUILTIN_STDOUT] = sbuf_fromcstr("STDOUT"),
	[BR_BUILTIN_STDERR] = sbuf_fromcstr("STDERR"),
};
static_assert(sizeof(BR_builtinNames) / sizeof(BR_builtinNames[0]) == BR_N_BUILTINS, "not all BRB built-ins have their names defined");

const size_t BR_syscallNArgs[] = {
	[BR_SYS_EXIT] = 1, // [code]
	[BR_SYS_WRITE] = 3, // [fd, addr, n_bytes]
	[BR_SYS_READ] = 3, // [fd, addr, n_bytes]
};
static_assert(sizeof(BR_syscallNArgs) / sizeof(BR_syscallNArgs[0]) == BR_N_SYSCALLS, "not all BRB syscalls have their prototype defined");

const sbuf BR_typeNames[] = {
	[BR_TYPE_DYNAMIC] = sbuf_fromcstr("__dynamic"),
	[BR_TYPE_I8]      = sbuf_fromcstr("i8"),
	[BR_TYPE_I16]     = sbuf_fromcstr("i16"),
	[BR_TYPE_I32]     = sbuf_fromcstr("i32"),
	[BR_TYPE_PTR]     = sbuf_fromcstr("ptr"),
	[BR_TYPE_I64]     = sbuf_fromcstr("i64"),
	[BR_TYPE_VOID]    = sbuf_fromcstr("void"),
	[BR_TYPE_STRUCT]  = sbuf_fromcstr("struct"),
	[BR_TYPE_INT]     = sbuf_fromcstr("__int"),
	[BR_TYPE_ANY]     = sbuf_fromcstr("__any")
};
static_assert(sizeof(BR_typeNames) / sizeof(BR_typeNames[0]) == BR_N_TYPE_KINDS, "not all BRB types have their names defined");

bool BR_startTimerAt(struct timespec* dst)
{
	return !clock_gettime(CLOCK_MONOTONIC, dst);
}

float BR_endTimerAt(struct timespec* src)
{
	struct timespec newtime;
	clock_gettime(CLOCK_MONOTONIC, &newtime);
	return (newtime.tv_sec - src->tv_sec) * 1000 + (newtime.tv_nsec - src->tv_nsec) / (float)1e6;
}

bool BR_execProcess(char* argv[], BR_ProcessInfo* info)
{
	pid_t pid;
	int out_pipe[2], err_pipe[2], local_errno, exit_status;
	posix_spawn_file_actions_t file_actions;

	if ((local_errno = posix_spawn_file_actions_init(&file_actions))) {
		errno = local_errno;
		return false;
	}

	if (info->in) {
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, fileno(info->in), STDIN_FILENO))) {
			errno = local_errno;
			return false;
		}
	}
	if (info->out != stdout) {
		if (pipe(out_pipe) < 0) return false;
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, out_pipe[1], STDOUT_FILENO))) {
			errno = local_errno;
			return false;
		}
	}
	if (info->err != stderr) {
		if (pipe(err_pipe) < 0) return false;
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, err_pipe[1], STDERR_FILENO))) {
			errno = local_errno;
			return false;
		}
	}

	if ((local_errno = posix_spawn(&pid, "/bin/sh", &file_actions, NULL, argv, environ))) {
		errno = local_errno;
		return false;
	}

	if (waitpid(pid, &exit_status, 0) != pid) return false;

	if (info->out != stdout) {
		close(out_pipe[1]);
		info->out = fdopen(out_pipe[0], "r");
	} else {
		info->out = NULL;
	}
	if (info->err != stderr) {
		close(err_pipe[1]);
		info->err = fdopen(err_pipe[0], "r");
	} else {
		info->err = NULL;
	}
	info->exited = WIFEXITED(exit_status);
	info->exitcode = info->exited ? WEXITSTATUS(exit_status) : WTERMSIG(exit_status);

	return true;
}

bool BR_isPathDir(char* path)
{
	struct stat info;
	if (lstat(path, &info)) return false;
	return S_ISDIR(info.st_mode);
}

bool BR_isPathDir_s(sbuf path)
{
	char temp[path.length + 1];
	memcpy(temp, path.data, path.length);
	temp[path.length] = '\0';
	return BR_isPathDir(temp);
}

char* BR_getFileExt(const char* path)
{
	char* dot = strrchr(path, '.');
	if (!dot || dot == path) return "";
	return strdup(dot + 1);
}

sbuf BR_getFileExt_s(sbuf path)
{
	sbuf noext;
	if (!sbuf_rsplit(&path, &noext, sbuf_fromcstr(".")).data) return sbuf_fromcstr("");
	return path;
}

char* BR_setFileExt(const char* path, const char* ext)
{
	sbuf src = sbuf_fromstr((char*)path);
	sbuf noext;
	sbuf_rsplit(&src, &noext, sbuf_fromcstr("."));
	return *ext ? sbuf_tostr(noext, sbuf_fromcstr("."), sbuf_fromstr((char*)ext)) : sbuf_tostr(noext);
}

sbuf BR_setFileExt_s(sbuf path, sbuf ext)
{
	sbuf noext;
	sbuf_rsplit(&path, &noext, sbuf_fromcstr("."));
	return ext.length ? sbuf_concat(noext, sbuf_fromcstr("."), ext) : sbuf_copy(noext);
}

char* BR_fileBaseName(const char* path)
{
	sbuf src = sbuf_fromstr((char*)path);
	sbuf res;
	if (sbuf_eq(sbuf_rsplit(&src, &res, sbuf_fromcstr("."), SBUF_PATHSEP), sbuf_fromcstr("."))) {
		return sbuf_tostr(sbuf_rsplit(&res, &src, SBUF_PATHSEP).length ? res : src);
	} else return sbuf_tostr(src.length ? src : res);
}

sbuf BR_fileBaseName_s(sbuf path)
{
	sbuf res;
	if (sbuf_eq(sbuf_rsplit(&path, &res, sbuf_fromcstr("."), SBUF_PATHSEP), sbuf_fromcstr("."))) {
		return sbuf_rsplit(&res, &path, SBUF_PATHSEP).length ? res : path;
	} else return path.length ? path : res;
}

void BR_printErrorMsg(FILE* dst, BR_Error err, const char* prefix)
{
	if (err.type == BR_ERR_OK) return;
	if (err.loc) {
		BRP_fprintTokenLoc(dst, *err.loc);
		fputs(": ", dst);
	}
	if (prefix) fprintf(dst, "%s: ", prefix);
	switch (err.type) {
		case BR_ERR_INVALID_HEADER:
			fputs("invalid module header: \"", dst);
			sbuf_fputesc(dst, (sbuf){ .data = err.header, .length = BR_HEADER_SIZE }, SBUF_BFMT_HEX | SBUF_BFMT_ESC_DQUOTE);
			fputs("\"\n", dst);
			break;
		case BR_ERR_NO_HEADER:
			fputs("no module header found\n", dst);
			break;
		case BR_ERR_NO_MEMORY:
			fputs("memory allocation failed\n", dst);
			break;
		case BR_ERR_NO_SEG_SIZES:
			fputs("unexpected end of input while loading segment sizes\n", dst);
			break;
		case BR_ERR_NO_OPCODE:
			fputs("unexpected end of input while loading type of an operation\n", dst);
			break;
		case BR_ERR_INVALID_OPCODE:
			fprintf(dst, "invalid operation type: %u\n", err.opcode);
			break;
		case BR_ERR_NO_OPERAND:
			fprintf(dst, "unexpected end of input while loading operand of an operation `%.*s`\n", sbuf_unpack(BR_opNames[err.opcode]));
			break;
		case BR_ERR_INVALID_NAME:
			fputs("invalid name found\n", dst);
			break;
		case BR_ERR_NAMES_NOT_RESOLVED:
			fputs("not all symbol names were resolved from the `name` segment\n", dst);
			break;
		case BR_ERR_STACK_UNDERFLOW:
			if (err.opcode == BR_N_OPS) {
				fprintf(dst, "attempted to label head of an empty stack\n");
			} else fprintf(dst,
				"stack underflow for operation `%.*s`: expected %u items, instead got %u\n",
				sbuf_unpack(BR_opNames[err.opcode]),
				err.expected_stack_length,
				err.actual_stack_length
			);
			break;
		case BR_ERR_OPERAND_OUT_OF_RANGE:
			fprintf(dst, "operand %llu is out of range for operation `%.*s`\n", err.operand, sbuf_unpack(BR_opNames[err.opcode]));
			break;
		case BR_ERR_NO_PROC_RET_TYPE:
			fprintf(dst, "unexpected end of input while loading return type of a procedure\n");
			break;
		case BR_ERR_NO_PROC_NAME:
			fprintf(dst, "unexpected end of input while loading name of a procedure\n");
			break;
		case BR_ERR_NO_PROC_ARG:
			fprintf(dst, "unexpected end of input while loading arguments of a procedure\n");
			break;
		case BR_ERR_NO_PROC_BODY_SIZE:
			fprintf(dst, "unexpected end of input while loading body size of a procedure\n");
			break;
		case BR_ERR_NO_DB_NAME:
			fprintf(dst, "unexpected end of input while loading name of a data block\n");
			break;
		case BR_ERR_NO_DB_BODY_SIZE:
			fprintf(dst, "unexpected end of input whole loading body size of a data block\n");
			break;
		case BR_ERR_NO_ENTRY:
			fprintf(dst, "unexpected end of input while loading the entry point\n");
			break;
		case BR_ERR_INVALID_ENTRY:
			fprintf(dst, "invalid entry point ID provided\n");
			break;
		case BR_ERR_INVALID_ENTRY_PROTOTYPE:
			fprintf(dst, "the entry point procedure must not accept any arguments or return anything\n");
			break;
		case BR_ERR_TYPE_EXPECTED:
			fprintf(dst, "expected a type specification\n");
			break;
		case BR_ERR_OP_NAME_EXPECTED:
			fprintf(dst, "expected an operation name\n");
			break;
		case BR_ERR_INT_OPERAND_EXPECTED:
			fprintf(dst, "expected an integer as an operand\n");
			break;
		case BR_ERR_INT_OR_DB_NAME_EXPECTED:
			fprintf(dst, "expected an integer or a data block name as an operand\n");
			break;
		case BR_ERR_BUILTIN_NAME_EXPECTED:
			fprintf(dst, "expected name of a built-in constant as an operand\n");
			break;
		case BR_ERR_TEXT_OPERAND_EXPECTED:
			fprintf(dst, "expected a string literal as an operand\n");
			break;
		case BR_ERR_INVALID_DECL:
			fprintf(dst, "top-level statements in the assembly code must start with either a return type specification, or with keyword `data`\n");
			break;
		case BR_ERR_ARGS_EXPECTED:
			fprintf(dst, "symbol `(` expected after name of the declared procedure\n");
			break;
		case BR_ERR_PROTOTYPE_MISMATCH:
			fprintf(dst, "procedure `%s` was already declared, but with another prototype\n", err.name);
			break;
		case BR_ERR_SYSCALL_NAME_EXPECTED:
			fprintf(dst, "expected a syscall name as an operand\n");
			break;
		case BR_ERR_INVALID_ARRAY_SIZE_SPEC:
			fprintf(dst, "array types must be denoted the following way: T[n]\n");
			break;
		case BR_ERR_TYPE_MISMATCH:
			fprintf(dst, "argument #%u for a `%.*s` operation is expected to be of type `", err.arg_id, sbuf_unpack(BR_opNames[err.opcode]));
			BR_printType(err.expected_type, dst);
			fputs("` instead got an argument of type `", dst);
			BR_printType(err.actual_type, dst);
			fputs("`\n", dst);
			break;
		case BR_ERR_DEL_ARGS:
			fputs("attempted to delete procedure arguments from the stack\n", dst);
			break;
		case BR_ERR_MODULE_LOAD_INTERRUPT:
			fputs("loading the module was interrupted by user\n", dst);
			break;
		case BR_ERR_UNKNOWN_DB:
			fprintf(dst, "unknown data block \"%s\"\n", err.name);
			break;
		case BR_ERR_UNKNOWN_BUILTIN:
			fprintf(dst, "unknown built-in constant \"%s\"\n", err.name);
			break;
		case BR_ERR_UNKNOWN_SYSCALL:
			fprintf(dst, "unknown syscall \"%s\"\n", err.name);
			break;
		case BR_ERR_TOO_MANY_STRUCTS:
			fprintf(dst, "amount of declared structs exceeded the limit of %zu\n", MAX_N_STRUCTS);
			break;
		case BR_ERR_STRUCT_NAME_EXPECTED:
			fputs("expected a word or a string as the struct name\n", dst);
			break;
		case BR_ERR_UNKNOWN_STRUCT:
			fprintf(dst, "unknown struct \"%s\"\n", err.name);
			break;
		case BR_ERR_RECURSIVE_TYPE:
			if (err.structs[err.struct_id].name) {
				fprintf(dst, "struct \"%s\" contains fields of the same type as itself\n", err.structs[err.struct_id].name);
			} else fprintf(dst, "struct $%u contains fields of the same type as itself\n", err.struct_id);
			break;
		case BR_ERR_NO_STRUCT_DECL:
			fputs("unexpected end of input while loading struct declarations\n", dst);
			break;
		case BR_ERR_NO_STRUCT_FIELD:
			fputs("unexpected end of input while loading structs' fields\n", dst);
			break;
		case BR_ERR_STRUCT_ID_EXPECTED:
			fputs("an integer expected as the struct index after the `$` symbol\n", dst);
			break;
		case BR_ERR_INVALID_STRUCT_ID:
			fprintf(dst, "number %llu is an invalid struct ID\n", err.operand);
			break;
		case BR_ERR_INVALID_TYPE_KIND:
			fprintf(dst, "invalid type kind: %llu\n", err.operand);
			break;
		case BR_ERR_OK:
		case BR_N_ERROR_TYPES:
		default:
			fputs("undefined\n", dst);
	}
}

BR_Error BR_initModuleBuilder(BR_ModuleBuilder* builder)
{
	*builder = (BR_ModuleBuilder){0};
	builder->module.exec_entry_point = SIZE_MAX;
	return (BR_Error){0};
}

BR_Error BR_analyzeModule(const BR_Module* module, BR_ModuleBuilder* dst)
{
	if (BR_initModuleBuilder(dst).type) return dst->error;
	if (BR_preallocStructs(dst, module->seg_typeinfo.length).type) return dst->error;
	if (BR_preallocDataBlocks(dst, module->seg_data.length).type) return dst->error;
	if (BR_preallocProcs(dst, module->seg_exec.length).type) return dst->error;
	arrayForeach (BR_Struct, _struct, module->seg_typeinfo) {
		BR_id _;
		if (BR_addStruct(dst, &_, _struct->name, _struct->fields.length, _struct->fields.data).type) return dst->error;
	}
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		BR_id _;
		if (BR_addDataBlock(dst, &_, block->name, block->is_mutable, block->body.length).type) return dst->error;
	}
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		BR_id _;
		if (BR_addProc(dst, &_, proc->name, proc->args.length, proc->args.data, proc->ret_type, proc->body.length).type) return dst->error;
	}
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		arrayForeach (BR_Op, op, block->body) {
			if (BR_addOp(dst, ~(block - module->seg_data.data), *op).type) return dst->error;
		}
	}
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		arrayForeach (BR_Op, op, proc->body) {
			if (BR_addOp(dst, proc - module->seg_exec.data, *op).type) return dst->error;
		}
	}
	return BR_setEntryPoint(dst, module->exec_entry_point);
}

void BR_delModuleBuilder(BR_ModuleBuilder builder)
{
	BR_Module stub;
	BR_extractModule(builder, &stub);
	BR_delModule(stub);
}

void BR_delModule(BR_Module module)
{
	BR_deallocProcs(&module);
	BR_deallocDataBlocks(&module);
	BR_deallocStructs(&module);
}

BR_Error BR_extractModule(BR_ModuleBuilder builder, BR_Module* dst)
{
	arrayForeach (BR_StackNodeArray, proc_info, builder.procs) {
		BR_StackNodeArray_clear(proc_info);
	}
	BR_StackNodeArrayArray_clear(&builder.procs);
	arrayForeach (BR_StackNodeArray, data_block_info, builder.data_blocks) {
		BR_StackNodeArray_clear(data_block_info);
	}
	BR_StackNodeArrayArray_clear(&builder.data_blocks);
	arena_free(&builder.arena);
	*dst = builder.module;
	return builder.error;
}

BR_Error BR_setEntryPoint(BR_ModuleBuilder* builder, size_t proc_id)
{
	if (builder->error.type) return builder->error;
	builder->module.exec_entry_point = proc_id;
	return (BR_Error){0};
}

FILE* BR_findModule(const char* name, const char* search_paths[])
{
	for (uint64_t i = 0; search_paths[i]; i += 1) {
		char path[MAXPATHLEN];
		snprintf(path, sizeof(path), "%s/%s.brb", search_paths[i], name);

		FILE* module_fd = fopen(path, "r");
		if (module_fd) return module_fd;
		errno = 0;
	}

	return NULL;
}

static BR_StackNode getNthStackNode(BR_StackNode head, size_t n)
{
	while (head && n--) head = head->prev;
	return head;
}

static BR_StackNode getNthStackNode_nonInternal(BR_StackNode head, size_t n)
{
	if (head && !n ? head->flags & BR_SNF_STACKFRAME : false) {
		return head->prev;
	}
	while (head && n--) {
		if (head->flags & BR_SNF_STACKFRAME) ++n;
		head = head->prev;
	}
	return head;
}

static size_t getStackLength(BR_StackNode head)
{
	size_t res = 0;
	while (head) {
		head = head->prev;
		++res;
	}
	return res;
}

static bool compareTypes(BR_Type field, BR_Type entry)
{
	return entry.kind & field.kind
		&& (field.kind != BR_TYPE_STRUCT || entry.struct_id == field.struct_id) && entry.n_items == field.n_items;
}

static BR_Error validateType(BR_Module* module, BR_Type* type)
{
	if ((type->kind & (type->kind - 1)) != 0) // if `type->kind` is not a power of 2
		return (BR_Error){.type = BR_ERR_INVALID_TYPE_KIND, .operand = type->kind};
	if (type->kind == BR_TYPE_STRUCT && type->struct_id >= module->seg_typeinfo.length)
		return (BR_Error){.type = BR_ERR_INVALID_STRUCT_ID, .operand = type->struct_id};
	return (BR_Error){0};
}

static void __unused printStack(BR_StackNode head)
{
	putchar('[');
	while (head) {
		BR_printType(head->type, stdout);
		if (head->prev) str_put(", ");
		head = head->prev;
	}
	putchar(']');
}

static BR_Error changeStack(BR_StackNodeArray* stack, BR_Op op, size_t n_in, BR_Type* in_types, size_t n_out, BR_Type* out_types, Arena* allocator)
{
	BR_StackNode iter = *arrayhead(*stack);
	if (getStackLength(iter) < n_in)
		return (BR_Error){
			.type = BR_ERR_STACK_UNDERFLOW,
			.opcode = op.type,
			.expected_stack_length = n_in,
			.actual_stack_length = getStackLength(iter)
		};
	arrayForeach (BR_Type, type, ((BR_TypeArray){.data = in_types, .length = n_in})) {
		if (iter->flags & BR_SNF_STACKFRAME)
			return (BR_Error){.type = BR_ERR_DEL_ARGS};
		if (!compareTypes(*type, iter->type))
			return (BR_Error){
				.type = BR_ERR_TYPE_MISMATCH,
				.opcode = op.type,
				.expected_type = *type,
				.actual_type = iter->type,
				.arg_id = type - in_types
			};
		iter = iter->prev;
	}

	BR_StackNode node = getNthStackNode(*arrayhead(*stack), n_in), prev;
	arrayRevForeach (BR_Type, type, ((BR_TypeArray){.data = out_types, .length = n_out})) {
		prev = node;
		if (!(node = arena_alloc(allocator, sizeof(struct BR_stacknode_t)))) return (BR_Error){.type = BR_ERR_NO_MEMORY};
		*node = (struct BR_stacknode_t){0};
		node->prev = prev;
		node->type = type->kind ? *type : type->ctor(op, *arrayhead(*stack));
	}
	return (BR_Error){.type = BR_StackNodeArray_append(stack, node) ? 0 : BR_ERR_NO_MEMORY};
}

BR_Error BR_addProc(BR_ModuleBuilder* builder, BR_id* proc_id_p, const char* name, size_t n_args, const BR_Type* args, BR_Type ret_type, uint32_t n_ops_hint)
{
	if (builder->error.type) return builder->error;
	BR_Error err;
// validating the return type
	if ((err = validateType(&builder->module, &ret_type)).type) return err;
	if (!BR_ProcArray_append(&builder->module.seg_exec, (BR_Proc){.name = name, .ret_type = ret_type})
		|| !BR_StackNodeArrayArray_append(&builder->procs, (BR_StackNodeArray){0}))
		return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
// copying the argumants
	if (!(arrayhead(builder->module.seg_exec)->args = BR_TypeArray_copy((BR_TypeArray){ .data = (BR_Type*)args, .length = n_args })).data)
		return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
// validating the arguments
	arrayForeach (BR_Type, arg, arrayhead(builder->module.seg_exec)->args) {
		if ((err = validateType(&builder->module, arg)).type) return err;
	}
// pre-allocating the stack trace and the procedure body
	if (n_ops_hint)
		if (!(arrayhead(builder->module.seg_exec)->body = BR_OpArray_new(-(int32_t)n_ops_hint)).data
			|| !(*arrayhead(builder->procs) = BR_StackNodeArray_new(-(int32_t)n_ops_hint)).data)
			return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
// initializing the stack trace
	BR_StackNode input = NULL, prev;
 	while (n_args--) {
		prev = input;
		if (!(input = arena_alloc(&builder->arena, sizeof(struct BR_stacknode_t))))
			return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
		input->prev = prev;
		input->type = args[n_args];
		input->name = NULL;
		input->flags = BR_SNF_STACKFRAME;
	}
	prev = input;
	if (!(input = arena_alloc(&builder->arena, sizeof(struct BR_stacknode_t))))
		return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
	input->prev = prev;
	input->type = BR_PTR_TYPE(2);
	input->name = NULL;
	input->flags = BR_SNF_STACKFRAME;
	*proc_id_p = builder->module.seg_exec.length - 1;
	return builder->error = (BR_Error){.type = BR_StackNodeArray_append(arrayhead(builder->procs), input) ? 0 : BR_ERR_NO_MEMORY};
}

static BR_Type _tctor_inputType(BR_Op op, BR_StackNode stack) {
	return op.operand_type;
}

static BR_Type _tctor_stackHeadType(BR_Op op, BR_StackNode stack) {
	return stack->type;
}

static BR_Type _tctor_typeOfInputStackItem(BR_Op op, BR_StackNode stack) {
	return getNthStackNode_nonInternal(stack, op.operand_u)->type;
}

static BR_Type _tctor_2ndStackItemType(BR_Op op, BR_StackNode stack) {
	return stack->prev->type;
}

BR_Error BR_addOp(BR_ModuleBuilder* builder, BR_id proc_id, BR_Op op)
{
	if (builder->error.type) return builder->error;
	static uint8_t n_in[] = {
		[BR_OP_NOP]       = 0,
		[BR_OP_END]       = 0,
		[BR_OP_I8]        = 0,
		[BR_OP_I16]       = 0,
		[BR_OP_I32]       = 0,
		[BR_OP_PTR]       = 0,
		[BR_OP_I64]       = 0,
		[BR_OP_ADDR]      = 0,
		[BR_OP_DBADDR]    = 0,
		[BR_OP_SYS]       = 0, // needs to be set manually every time, depending on the system function in question
		[BR_OP_BUILTIN]   = 0,
		[BR_OP_ADD]       = 2,
		[BR_OP_ADDI]      = 1,
		[BR_OP_ADDIAT8]   = 1,
		[BR_OP_ADDIAT16]  = 1,
		[BR_OP_ADDIAT32]  = 1,
		[BR_OP_ADDIATP]   = 1,
		[BR_OP_ADDIAT64]  = 1,
		[BR_OP_SUB]       = 2,
		[BR_OP_SUBI]      = 1,
		[BR_OP_SUBIAT8]   = 1,
		[BR_OP_SUBIAT16]  = 1,
		[BR_OP_SUBIAT32]  = 1,
		[BR_OP_SUBIATP]   = 1,
		[BR_OP_SUBIAT64]  = 1,
		[BR_OP_MUL]       = 2,
		[BR_OP_MULI]      = 1,
		[BR_OP_MULIAT8]   = 1,
		[BR_OP_MULIAT16]  = 1,
		[BR_OP_MULIAT32]  = 1,
		[BR_OP_MULIATP]   = 1,
		[BR_OP_MULIAT64]  = 1,
		[BR_OP_DIV]       = 2,
		[BR_OP_DIVI]      = 1,
		[BR_OP_DIVIAT8]   = 1,
		[BR_OP_DIVIAT16]  = 1,
		[BR_OP_DIVIAT32]  = 1,
		[BR_OP_DIVIATP]   = 1,
		[BR_OP_DIVIAT64]  = 1,
		[BR_OP_DIVS]      = 2,
		[BR_OP_DIVSI]     = 1,
		[BR_OP_DIVSIAT8]  = 1,
		[BR_OP_DIVSIAT16] = 1,
		[BR_OP_DIVSIAT32] = 1,
		[BR_OP_DIVSIATP]  = 1,
		[BR_OP_DIVSIAT64] = 1,
		[BR_OP_MOD]       = 2,
		[BR_OP_MODI]      = 1,
		[BR_OP_MODIAT8]   = 1,
		[BR_OP_MODIAT16]  = 1,
		[BR_OP_MODIAT32]  = 1,
		[BR_OP_MODIATP]   = 1,
		[BR_OP_MODIAT64]  = 1,
		[BR_OP_MODS]      = 2,
		[BR_OP_MODSI]     = 1,
		[BR_OP_MODSIAT8]  = 1,
		[BR_OP_MODSIAT16] = 1,
		[BR_OP_MODSIAT32] = 1,
		[BR_OP_MODSIATP]  = 1,
		[BR_OP_MODSIAT64] = 1,
		[BR_OP_AND]       = 2,
		[BR_OP_ANDI]      = 1,
		[BR_OP_ANDIAT8]   = 1,
		[BR_OP_ANDIAT16]  = 1,
		[BR_OP_ANDIAT32]  = 1,
		[BR_OP_ANDIATP]   = 1,
		[BR_OP_ANDIAT64]  = 1,
		[BR_OP_OR]        = 2,
		[BR_OP_ORI]       = 1,
		[BR_OP_ORIAT8]    = 1,
		[BR_OP_ORIAT16]   = 1,
		[BR_OP_ORIAT32]   = 1,
		[BR_OP_ORIATP]    = 1,
		[BR_OP_ORIAT64]   = 1,
		[BR_OP_XOR]       = 2,
		[BR_OP_XORI]      = 1,
		[BR_OP_XORIAT8]   = 1,
		[BR_OP_XORIAT16]  = 1,
		[BR_OP_XORIAT32]  = 1,
		[BR_OP_XORIATP]   = 1,
		[BR_OP_XORIAT64]  = 1,
		[BR_OP_SHL]       = 2,
		[BR_OP_SHLI]      = 1,
		[BR_OP_SHLIAT8]   = 1,
		[BR_OP_SHLIAT16]  = 1,
		[BR_OP_SHLIAT32]  = 1,
		[BR_OP_SHLIATP]   = 1,
		[BR_OP_SHLIAT64]  = 1,
		[BR_OP_SHR]       = 2,
		[BR_OP_SHRI]      = 1,
		[BR_OP_SHRIAT8]   = 1,
		[BR_OP_SHRIAT16]  = 1,
		[BR_OP_SHRIAT32]  = 1,
		[BR_OP_SHRIATP]   = 1,
		[BR_OP_SHRIAT64]  = 1,
		[BR_OP_SHRS]      = 2,
		[BR_OP_SHRSI]     = 1,
		[BR_OP_SHRSIAT8]  = 1,
		[BR_OP_SHRSIAT16] = 1,
		[BR_OP_SHRSIAT32] = 1,
		[BR_OP_SHRSIATP]  = 1,
		[BR_OP_SHRSIAT64] = 1,
		[BR_OP_NOT]       = 1,
		[BR_OP_NOTAT8]    = 1,
		[BR_OP_NOTAT16]   = 1,
		[BR_OP_NOTAT32]   = 1,
		[BR_OP_NOTATP]    = 1,
		[BR_OP_NOTAT64]   = 1,
		[BR_OP_DROP]      = 1,
		[BR_OP_NEW]       = 0,
		[BR_OP_ZERO]      = 0,
		[BR_OP_GET]       = 0,
		[BR_OP_SETAT]     = 2,
		[BR_OP_GETFROM]   = 1,
		[BR_OP_COPY]      = 2
	};
	static BR_Type in_types[][3] = {
		[BR_OP_NOP]       = { BR_VOID_TYPE },
		[BR_OP_END]       = { BR_VOID_TYPE },
		[BR_OP_I8]        = { BR_VOID_TYPE },
		[BR_OP_I16]       = { BR_VOID_TYPE },
		[BR_OP_I32]       = { BR_VOID_TYPE },
		[BR_OP_PTR]       = { BR_VOID_TYPE },
		[BR_OP_I64]       = { BR_VOID_TYPE },
		[BR_OP_ADDR]      = { BR_VOID_TYPE },
		[BR_OP_DBADDR]    = { BR_VOID_TYPE },
		[BR_OP_SYS]       = { BR_VOID_TYPE }, // needs to be set manually every time, depending on the system function in question
		[BR_OP_BUILTIN]   = { BR_VOID_TYPE },
		[BR_OP_ADD]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_ADDI]      = { BR_INT_TYPE(1) },
		[BR_OP_ADDIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_ADDIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_ADDIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_ADDIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_ADDIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_SUB]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_SUBI]      = { BR_INT_TYPE(1) },
		[BR_OP_SUBIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_SUBIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_SUBIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_SUBIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_SUBIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_MUL]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_MULI]      = { BR_INT_TYPE(1) },
		[BR_OP_MULIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_MULIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_MULIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_MULIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_MULIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_DIV]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_DIVI]      = { BR_INT_TYPE(1) },
		[BR_OP_DIVIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_DIVIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_DIVIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_DIVIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_DIVIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_DIVS]      = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_DIVSI]     = { BR_INT_TYPE(1) },
		[BR_OP_DIVSIAT8]  = { BR_PTR_TYPE(1) },
		[BR_OP_DIVSIAT16] = { BR_PTR_TYPE(1) },
		[BR_OP_DIVSIAT32] = { BR_PTR_TYPE(1) },
		[BR_OP_DIVSIATP]  = { BR_PTR_TYPE(1) },
		[BR_OP_DIVSIAT64] = { BR_PTR_TYPE(1) },
		[BR_OP_MOD]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_MODI]      = { BR_INT_TYPE(1) },
		[BR_OP_MODIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_MODIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_MODIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_MODIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_MODIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_MODS]      = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_MODSI]     = { BR_INT_TYPE(1) },
		[BR_OP_MODSIAT8]  = { BR_PTR_TYPE(1) },
		[BR_OP_MODSIAT16] = { BR_PTR_TYPE(1) },
		[BR_OP_MODSIAT32] = { BR_PTR_TYPE(1) },
		[BR_OP_MODSIATP]  = { BR_PTR_TYPE(1) },
		[BR_OP_MODSIAT64] = { BR_PTR_TYPE(1) },
		[BR_OP_AND]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_ANDI]      = { BR_INT_TYPE(1) },
		[BR_OP_ANDIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_ANDIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_ANDIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_ANDIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_ANDIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_OR]        = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_ORI]       = { BR_INT_TYPE(1) },
		[BR_OP_ORIAT8]    = { BR_PTR_TYPE(1) },
		[BR_OP_ORIAT16]   = { BR_PTR_TYPE(1) },
		[BR_OP_ORIAT32]   = { BR_PTR_TYPE(1) },
		[BR_OP_ORIATP]    = { BR_PTR_TYPE(1) },
		[BR_OP_ORIAT64]   = { BR_PTR_TYPE(1) },
		[BR_OP_XOR]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_XORI]      = { BR_INT_TYPE(1) },
		[BR_OP_XORIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_XORIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_XORIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_XORIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_XORIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHL]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_SHLI]      = { BR_INT_TYPE(1) },
		[BR_OP_SHLIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_SHLIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHLIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHLIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_SHLIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHR]       = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_SHRI]      = { BR_INT_TYPE(1) },
		[BR_OP_SHRIAT8]   = { BR_PTR_TYPE(1) },
		[BR_OP_SHRIAT16]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHRIAT32]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHRIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_SHRIAT64]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHRS]      = { BR_INT_TYPE(1), BR_INT_TYPE(1) },
		[BR_OP_SHRSI]     = { BR_INT_TYPE(1) },
		[BR_OP_SHRSIAT8]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHRSIAT16] = { BR_PTR_TYPE(1) },
		[BR_OP_SHRSIAT32] = { BR_PTR_TYPE(1) },
		[BR_OP_SHRSIATP]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHRSIAT64] = { BR_PTR_TYPE(1) },
		[BR_OP_NOT]       = { BR_INT_TYPE(1) },
		[BR_OP_NOTAT8]    = { BR_PTR_TYPE(1) },
		[BR_OP_NOTAT16]   = { BR_PTR_TYPE(1) },
		[BR_OP_NOTAT32]   = { BR_PTR_TYPE(1) },
		[BR_OP_NOTATP]    = { BR_PTR_TYPE(1) },
		[BR_OP_NOTAT64]   = { BR_PTR_TYPE(1) },
		[BR_OP_DROP]      = { BR_ANY_TYPE(1) },
		[BR_OP_NEW]       = { BR_VOID_TYPE },
		[BR_OP_ZERO]      = { BR_VOID_TYPE },
		[BR_OP_GET]       = { BR_VOID_TYPE },
		[BR_OP_SETAT]     = { BR_PTR_TYPE(1), BR_ANY_TYPE(1) },
		[BR_OP_GETFROM]   = { BR_PTR_TYPE(1) },
		[BR_OP_COPY]      = { BR_PTR_TYPE(1), BR_PTR_TYPE(1) }
	};
	static_assert(BR_N_OPS == 115, "not all BRB operations have their input types defined");
	static uint8_t n_out[] = {
		[BR_OP_NOP]       = 0,
		[BR_OP_END]       = 0,
		[BR_OP_I8]        = 1,
		[BR_OP_I16]       = 1,
		[BR_OP_I32]       = 1,
		[BR_OP_PTR]       = 1,
		[BR_OP_I64]       = 1,
		[BR_OP_ADDR]      = 1,
		[BR_OP_DBADDR]    = 1,
		[BR_OP_SYS]       = 1,
		[BR_OP_BUILTIN]   = 1,
		[BR_OP_ADD]       = 1,
		[BR_OP_ADDI]      = 1,
		[BR_OP_ADDIAT8]   = 1,
		[BR_OP_ADDIAT16]  = 1,
		[BR_OP_ADDIAT32]  = 1,
		[BR_OP_ADDIATP]   = 1,
		[BR_OP_ADDIAT64]  = 1,
		[BR_OP_SUB]       = 1,
		[BR_OP_SUBI]      = 1,
		[BR_OP_SUBIAT8]   = 1,
		[BR_OP_SUBIAT16]  = 1,
		[BR_OP_SUBIAT32]  = 1,
		[BR_OP_SUBIATP]   = 1,
		[BR_OP_SUBIAT64]  = 1,
		[BR_OP_MUL]       = 1,
		[BR_OP_MULI]      = 1,
		[BR_OP_MULIAT8]   = 1,
		[BR_OP_MULIAT16]  = 1,
		[BR_OP_MULIAT32]  = 1,
		[BR_OP_MULIATP]   = 1,
		[BR_OP_MULIAT64]  = 1,
		[BR_OP_DIV]       = 1,
		[BR_OP_DIVI]      = 1,
		[BR_OP_DIVIAT8]   = 1,
		[BR_OP_DIVIAT16]  = 1,
		[BR_OP_DIVIAT32]  = 1,
		[BR_OP_DIVIATP]   = 1,
		[BR_OP_DIVIAT64]  = 1,
		[BR_OP_DIVS]      = 1,
		[BR_OP_DIVSI]     = 1,
		[BR_OP_DIVSIAT8]  = 1,
		[BR_OP_DIVSIAT16] = 1,
		[BR_OP_DIVSIAT32] = 1,
		[BR_OP_DIVSIATP]  = 1,
		[BR_OP_DIVSIAT64] = 1,
		[BR_OP_MOD]       = 1,
		[BR_OP_MODI]      = 1,
		[BR_OP_MODIAT8]   = 1,
		[BR_OP_MODIAT16]  = 1,
		[BR_OP_MODIAT32]  = 1,
		[BR_OP_MODIATP]   = 1,
		[BR_OP_MODIAT64]  = 1,
		[BR_OP_MODS]      = 1,
		[BR_OP_MODSI]     = 1,
		[BR_OP_MODSIAT8]  = 1,
		[BR_OP_MODSIAT16] = 1,
		[BR_OP_MODSIAT32] = 1,
		[BR_OP_MODSIATP]  = 1,
		[BR_OP_MODSIAT64] = 1,
		[BR_OP_AND]       = 1,
		[BR_OP_ANDI]      = 1,
		[BR_OP_ANDIAT8]   = 1,
		[BR_OP_ANDIAT16]  = 1,
		[BR_OP_ANDIAT32]  = 1,
		[BR_OP_ANDIATP]   = 1,
		[BR_OP_ANDIAT64]  = 1,
		[BR_OP_OR]        = 1,
		[BR_OP_ORI]       = 1,
		[BR_OP_ORIAT8]    = 1,
		[BR_OP_ORIAT16]   = 1,
		[BR_OP_ORIAT32]   = 1,
		[BR_OP_ORIATP]    = 1,
		[BR_OP_ORIAT64]   = 1,
		[BR_OP_XOR]       = 1,
		[BR_OP_XORI]      = 1,
		[BR_OP_XORIAT8]   = 1,
		[BR_OP_XORIAT16]  = 1,
		[BR_OP_XORIAT32]  = 1,
		[BR_OP_XORIATP]   = 1,
		[BR_OP_XORIAT64]  = 1,
		[BR_OP_SHL]       = 1,
		[BR_OP_SHLI]      = 1,
		[BR_OP_SHLIAT8]   = 1,
		[BR_OP_SHLIAT16]  = 1,
		[BR_OP_SHLIAT32]  = 1,
		[BR_OP_SHLIATP]   = 1,
		[BR_OP_SHLIAT64]  = 1,
		[BR_OP_SHR]       = 1,
		[BR_OP_SHRI]      = 1,
		[BR_OP_SHRIAT8]   = 1,
		[BR_OP_SHRIAT16]  = 1,
		[BR_OP_SHRIAT32]  = 1,
		[BR_OP_SHRIATP]   = 1,
		[BR_OP_SHRIAT64]  = 1,
		[BR_OP_SHRS]      = 1,
		[BR_OP_SHRSI]     = 1,
		[BR_OP_SHRSIAT8]  = 1,
		[BR_OP_SHRSIAT16] = 1,
		[BR_OP_SHRSIAT32] = 1,
		[BR_OP_SHRSIATP]  = 1,
		[BR_OP_SHRSIAT64] = 1,
		[BR_OP_NOT]       = 1,
		[BR_OP_NOTAT8]    = 1,
		[BR_OP_NOTAT16]   = 1,
		[BR_OP_NOTAT32]   = 1,
		[BR_OP_NOTATP]    = 1,
		[BR_OP_NOTAT64]   = 1,
		[BR_OP_DROP]      = 0,
		[BR_OP_NEW]       = 1,
		[BR_OP_ZERO]      = 1,
		[BR_OP_GET]       = 1,
		[BR_OP_SETAT]     = 1,
		[BR_OP_GETFROM]   = 1,
		[BR_OP_COPY]      = 1
	};
	static BR_Type out_types[][1] = {
		[BR_OP_NOP]       = { BR_VOID_TYPE },
		[BR_OP_END]       = { BR_VOID_TYPE },
		[BR_OP_I8]        = { BR_I8_TYPE(1) },
		[BR_OP_I16]       = { BR_I16_TYPE(1) },
		[BR_OP_I32]       = { BR_I32_TYPE(1) },
		[BR_OP_PTR]       = { BR_PTR_TYPE(1) },
		[BR_OP_I64]       = { BR_I64_TYPE(1) },
		[BR_OP_ADDR]      = { BR_PTR_TYPE(1) },
		[BR_OP_DBADDR]    = { BR_PTR_TYPE(1) },
		[BR_OP_SYS]       = { BR_PTR_TYPE(1) },
		[BR_OP_BUILTIN]   = { BR_PTR_TYPE(1) },
		[BR_OP_ADD]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_ADDI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_ADDIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_ADDIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_ADDIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_ADDIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_ADDIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_SUB]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_SUBI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_SUBIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_SUBIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_SUBIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_SUBIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_SUBIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_MUL]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_MULI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_MULIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_MULIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_MULIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_MULIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_MULIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_DIV]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_DIVI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_DIVIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_DIVIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_DIVIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_DIVIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_DIVIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_DIVS]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_DIVSI]     = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_DIVSIAT8]  = { BR_I8_TYPE(1) },
		[BR_OP_DIVSIAT16] = { BR_I16_TYPE(1) },
		[BR_OP_DIVSIAT32] = { BR_I32_TYPE(1) },
		[BR_OP_DIVSIATP]  = { BR_PTR_TYPE(1) },
		[BR_OP_DIVSIAT64] = { BR_I64_TYPE(1) },
		[BR_OP_MOD]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_MODI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_MODIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_MODIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_MODIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_MODIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_MODIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_MODS]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_MODSI]     = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_MODSIAT8]  = { BR_I8_TYPE(1) },
		[BR_OP_MODSIAT16] = { BR_I16_TYPE(1) },
		[BR_OP_MODSIAT32] = { BR_I32_TYPE(1) },
		[BR_OP_MODSIATP]  = { BR_PTR_TYPE(1) },
		[BR_OP_MODSIAT64] = { BR_I64_TYPE(1) },
		[BR_OP_AND]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_ANDI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_ANDIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_ANDIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_ANDIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_ANDIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_ANDIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_OR]        = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_ORI]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_ORIAT8]    = { BR_I8_TYPE(1) },
		[BR_OP_ORIAT16]   = { BR_I16_TYPE(1) },
		[BR_OP_ORIAT32]   = { BR_I32_TYPE(1) },
		[BR_OP_ORIATP]    = { BR_PTR_TYPE(1) },
		[BR_OP_ORIAT64]   = { BR_I64_TYPE(1) },
		[BR_OP_XOR]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_XORI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_XORIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_XORIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_XORIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_XORIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_XORIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_SHL]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_SHLI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_SHLIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_SHLIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_SHLIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_SHLIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_SHLIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_SHR]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_SHRI]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_SHRIAT8]   = { BR_I8_TYPE(1) },
		[BR_OP_SHRIAT16]  = { BR_I16_TYPE(1) },
		[BR_OP_SHRIAT32]  = { BR_I32_TYPE(1) },
		[BR_OP_SHRIATP]   = { BR_PTR_TYPE(1) },
		[BR_OP_SHRIAT64]  = { BR_I64_TYPE(1) },
		[BR_OP_SHRS]      = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_SHRSI]     = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_SHRSIAT8]  = { BR_I8_TYPE(1) },
		[BR_OP_SHRSIAT16] = { BR_I16_TYPE(1) },
		[BR_OP_SHRSIAT32] = { BR_I32_TYPE(1) },
		[BR_OP_SHRSIATP]  = { BR_PTR_TYPE(1) },
		[BR_OP_SHRSIAT64] = { BR_I64_TYPE(1) },
		[BR_OP_NOT]       = { BR_DYN_TYPE(_tctor_stackHeadType) },
		[BR_OP_NOTAT8]    = { BR_I8_TYPE(1) },
		[BR_OP_NOTAT16]   = { BR_I16_TYPE(1) },
		[BR_OP_NOTAT32]   = { BR_I32_TYPE(1) },
		[BR_OP_NOTATP]    = { BR_PTR_TYPE(1) },
		[BR_OP_NOTAT64]   = { BR_I64_TYPE(1) },
		[BR_OP_DROP]      = { BR_VOID_TYPE },
		[BR_OP_NEW]       = { BR_DYN_TYPE(_tctor_inputType) },
		[BR_OP_ZERO]      = { BR_DYN_TYPE(_tctor_inputType) },
		[BR_OP_GET]       = { BR_DYN_TYPE(_tctor_typeOfInputStackItem) },
		[BR_OP_SETAT]     = { BR_DYN_TYPE(_tctor_2ndStackItemType) },
		[BR_OP_GETFROM]   = { BR_DYN_TYPE(_tctor_inputType) },
		[BR_OP_COPY]      = { BR_PTR_TYPE(1) }
	};
	static_assert(BR_N_OPS == 115, "not all BRB operations have their output types defined");
	static uint8_t sys_n_in[] = {
		[BR_SYS_EXIT]  = 1,
		[BR_SYS_WRITE] = 3,
		[BR_SYS_READ]  = 3
	};
	static BR_Type sys_in_types[][3] = {
		[BR_SYS_EXIT]  = { BR_PTR_TYPE(1) },
		[BR_SYS_WRITE] = { BR_PTR_TYPE(1), BR_PTR_TYPE(1), BR_PTR_TYPE(1) },
		[BR_SYS_READ]  = { BR_PTR_TYPE(1), BR_PTR_TYPE(1), BR_PTR_TYPE(1) }
	};
	static_assert(BR_N_SYSCALLS == 3, "not all BRB syscalls have their input types defined");
	static const bool may_overflow_div[BR_N_OPS] = {
		[BR_OP_DIVI]      = true,
		[BR_OP_DIVIAT8]   = true,
		[BR_OP_DIVIAT16]  = true,
		[BR_OP_DIVIAT32]  = true,
		[BR_OP_DIVIATP]   = true,
		[BR_OP_DIVIAT64]  = true,
		[BR_OP_DIVSI]     = true,
		[BR_OP_DIVSIAT8]  = true,
		[BR_OP_DIVSIAT16] = true,
		[BR_OP_DIVSIAT32] = true,
		[BR_OP_DIVSIATP]  = true,
		[BR_OP_DIVSIAT64] = true,
		[BR_OP_MODI]      = true,
		[BR_OP_MODIAT8]   = true,
		[BR_OP_MODIAT16]  = true,
		[BR_OP_MODIAT32]  = true,
		[BR_OP_MODIATP]   = true,
		[BR_OP_MODIAT64]  = true,
		[BR_OP_MODSI]     = true,
		[BR_OP_MODSIAT8]  = true,
		[BR_OP_MODSIAT16] = true,
		[BR_OP_MODSIAT32] = true,
		[BR_OP_MODSIATP]  = true,
		[BR_OP_MODSIAT64] = true
	};
	static const bool may_overflow_shift[BR_N_OPS] = {
		[BR_OP_SHLI]      = true,
		[BR_OP_SHLIAT8]   = true,
		[BR_OP_SHLIAT16]  = true,
		[BR_OP_SHLIAT32]  = true,
		[BR_OP_SHLIATP]   = true,
		[BR_OP_SHLIAT64]  = true,
		[BR_OP_SHRI]      = true,
		[BR_OP_SHRIAT8]   = true,
		[BR_OP_SHRIAT16]  = true,
		[BR_OP_SHRIAT32]  = true,
		[BR_OP_SHRIATP]   = true,
		[BR_OP_SHRIAT64]  = true,
		[BR_OP_SHRSI]     = true,
		[BR_OP_SHRSIAT8]  = true,
		[BR_OP_SHRSIAT16] = true,
		[BR_OP_SHRSIAT32] = true,
		[BR_OP_SHRSIATP]  = true,
		[BR_OP_SHRSIAT64] = true
	};
// resolving where to add the operation
	if (builder->error.type) return builder->error;
	BR_OpArray* body;
	BR_StackNodeArray* vframe;
	if (proc_id < 0) {
		body = &builder->module.seg_data.data[~proc_id].body;
		vframe = &builder->data_blocks.data[~proc_id];
	} else {
		body = &builder->module.seg_exec.data[proc_id].body;
		vframe = &builder->procs.data[proc_id];
	}
// validating the type operand, if there is one
	if (BR_GET_OPERAND_TYPE(op.type) == BR_OPERAND_TYPE)
		if ((builder->error = validateType(&builder->module, &op.operand_type)).type) return builder->error;
// adding the operation
	if (!BR_OpArray_append(body, op))
		return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
// checking the edge cases
	if (BR_GET_OPERAND_TYPE(op.type) == BR_OPERAND_VAR_NAME) {
		if (op.operand_u >= UINT32_MAX) {
			--body->length;
			return builder->error = (BR_Error){.type = BR_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u};
		}
		if (op.operand_u ? !getNthStackNode_nonInternal(*arrayhead(*vframe), op.operand_u) : false) {
			--body->length;
			return (builder->error = (BR_Error){.type = BR_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u});
		}
	} else if (BR_GET_OPERAND_TYPE(op.type) == BR_OPERAND_DB_NAME) {
		if (~op.operand_s >= UINT32_MAX) {
			--body->length;
			return (builder->error = (BR_Error){.type = BR_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u});
		}
		if (~op.operand_s >= builder->module.seg_data.length) {
			--body->length;
			return (builder->error = (BR_Error){.type = BR_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u});
		}
	} else if (op.type == BR_OP_SYS) {
		if (op.operand_u >= BR_N_SYSCALLS) {
			--body->length;
			return (builder->error = (BR_Error){ .type = BR_ERR_OPERAND_OUT_OF_RANGE, .opcode = BR_OP_SYS, .operand = op.operand_u });
		}
		n_in[BR_OP_SYS] = sys_n_in[op.operand_u];
		memcpy(in_types[BR_OP_SYS], sys_in_types[op.operand_u], sizeof(in_types[BR_OP_SYS]));
	} else if (op.type == BR_OP_BUILTIN && op.operand_u >= BR_N_BUILTINS) {
		--body->length;
		return builder->error = (BR_Error){ .type = BR_ERR_OPERAND_OUT_OF_RANGE, .opcode = BR_OP_BUILTIN, .operand = op.operand_u };
	} else if (may_overflow_div[op.type] && op.operand_u == 0) {
		--body->length;
		return builder->error = (BR_Error){ .type = BR_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = 0 };
	} else if (may_overflow_shift[op.type] && op.operand_u >= 64) {
		--body->length;
		return builder->error = (BR_Error){ .type = BR_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u };
	} else if (op.type >= BR_N_OPS) {
		--body->length;
		return builder->error = (BR_Error){.type = BR_ERR_INVALID_OPCODE, .opcode = op.type};
	}
// registering the changes in the stack
	if ((builder->error = changeStack(
		vframe,
		op,
		n_in[op.type], in_types[op.type],
		n_out[op.type], out_types[op.type],
		&builder->arena
	)).type) {
		--body->length;
		return builder->error;
	}
	return (BR_Error){0};
}

BR_Op* BR_getOp(BR_Module* module, BR_id proc_id, uint32_t op_id) {
	return &(proc_id < 0 ? module->seg_data.data[~proc_id].body : module->seg_exec.data[proc_id].body).data[op_id];
}

BR_Error BR_labelStackItem(BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id, uint32_t item_id, const char* name)
{
	if (builder->error.type) return builder->error;
	BR_StackNode target = getNthStackNode(proc_id < 0 ? builder->data_blocks.data[~proc_id].data[op_id] : builder->procs.data[proc_id].data[op_id], item_id);
	if (!target) return (builder->error = (BR_Error){
		.type = BR_ERR_STACK_UNDERFLOW,
		.opcode = BR_N_OPS,
		.expected_stack_length = 1,
		.actual_stack_length = 0
	});
	target->name = name;
	return (BR_Error){0};
}

BR_Error BR_addDataBlock(BR_ModuleBuilder* builder, BR_id* db_id_p, const char* name, bool is_mutable, uint32_t n_ops_hint)
{
	if (builder->error.type) return builder->error;
	if (!BR_DataBlockArray_append(&builder->module.seg_data, (BR_DataBlock){ .name = name, .is_mutable = is_mutable })
		|| !BR_StackNodeArrayArray_append(&builder->data_blocks, (BR_StackNodeArray){0}))
		return (builder->error = (BR_Error){ .type = BR_ERR_NO_MEMORY });

	if (!(arrayhead(builder->module.seg_data)->body = BR_OpArray_new(-(int32_t)n_ops_hint)).data)
		return (builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY});

	if (!BR_StackNodeArray_append(arrayhead(builder->data_blocks), NULL))
		return (builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY});

	*db_id_p = ~((size_t)builder->module.seg_data.length - 1);
	return (BR_Error){0};
}

BR_id BR_getDataBlockIdByName(const BR_Module* module, const char* name)
{
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		if (str_eq(block->name, name)) return ~(block - module->seg_data.data);
	}
	return BR_INVALID_ID;
}

static size_t getTypeAlignment(BR_Module* module, BR_Type type)
{
	if (type.kind == BR_TYPE_STRUCT) {
		if (type.struct_id >= module->seg_typeinfo.length) return SIZE_MAX;
		BR_Struct* obj = &module->seg_typeinfo.data[type.struct_id];
		return obj->alignment ? obj->alignment : SIZE_MAX;
	}
	static size_t values[] = {
		[BR_TYPE_I8] = 1,
		[BR_TYPE_I16] = 2,
		[BR_TYPE_I32] = 4,
		[BR_TYPE_PTR] = sizeof(void*),
		[BR_TYPE_I64] = 8,
		[BR_TYPE_VOID] = 0,
	};
	return values[type.kind];
}

size_t BR_getTypeRTSize(const BR_Module* module, BR_Type type)
{
	if (type.kind == BR_TYPE_STRUCT)
		return type.struct_id >= module->seg_typeinfo.length
			? SIZE_MAX
			: module->seg_typeinfo.data[type.struct_id].size;
	assert(!(type.kind & (type.kind - 1)), "invalid type kind %i", type.kind);
	static size_t coeffs[] = {
		[BR_TYPE_I8] = 1,
		[BR_TYPE_I16] = 2,
		[BR_TYPE_I32] = 4,
		[BR_TYPE_PTR] = sizeof(void*),
		[BR_TYPE_I64] = 8,
		[BR_TYPE_VOID] = 0,
	};
	return type.n_items * coeffs[type.kind];
}

size_t BR_getStackItemRTOffset(const BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id, size_t item_id)
{
	const bool strict = item_id != SIZE_MAX;
	BR_StackNodeArray* vframe;
	if (proc_id < 0) {
		if (~proc_id >= builder->data_blocks.length) return SIZE_MAX;
		vframe = &builder->data_blocks.data[~proc_id];
	} else {
		if (proc_id >= builder->procs.length) return SIZE_MAX;
		vframe = &builder->procs.data[proc_id];
	}
	if (++op_id >= vframe->length) return SIZE_MAX;
	// `++op_id` because the first element in the state stack is always the initial state, determined by proc args
	size_t res = 0;
	BR_StackNode node = vframe->data[op_id];
	while (node ? node->flags & BR_SNF_STACKFRAME : false) node = node->prev; // setting up the iterator
	while (node && item_id) {
		size_t node_size = BR_getTypeRTSize(&builder->module, node->type);
		if (node_size == SIZE_MAX) return SIZE_MAX;
		res += node_size;
		if (!(node->flags & BR_SNF_STACKFRAME)) --item_id;
		node = node->prev;
	}
	return item_id && strict ? SIZE_MAX : res;
}

size_t BR_getStackRTSize(const BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id)
{
	return BR_getStackItemRTOffset(builder, proc_id, op_id, SIZE_MAX);
}

size_t BR_getMaxStackRTSize(const BR_ModuleBuilder* builder, BR_id proc_id)
{
	size_t res = 0, i = -1;
	while (true) {
		size_t cur_size = BR_getStackRTSize(builder, proc_id, i++);
		if (cur_size == SIZE_MAX) break;
		if (cur_size > res) res = cur_size;
	}
	return res;
}

bool BR_getStackItemType(const BR_ModuleBuilder* builder, BR_Type* dst, BR_id proc_id, uint32_t op_id, uint32_t item_id)
{
	BR_StackNodeArray* vframe;
	if (proc_id < 0) {
		if (~proc_id >= builder->data_blocks.length) return false;
		vframe = &builder->data_blocks.data[~proc_id];
	} else {
		if (proc_id >= builder->procs.length) return false;
		vframe = &builder->procs.data[proc_id];
	}
	if (++op_id >= vframe->length) return false;
	// `++op_id` because the first element in the state stack is always the initial state, determined by proc args
	BR_StackNode node = vframe->data[op_id];
	while (node && item_id--) node = node->prev;
	if (!node) return false;
	*dst = node->type;
	return true;
}

size_t BR_getStackItemRTSize(const BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id, uint32_t item_id)
{
	BR_Type res;
	if (!BR_getStackItemType(builder, &res, proc_id, op_id, item_id)) return SIZE_MAX;
	return BR_getTypeRTSize(&builder->module, res);
}

BR_id BR_getProcIdByName(const BR_Module* module, const char* name)
{
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		if (str_eq(name, proc->name)) return proc - module->seg_exec.data;
	}
	return BR_INVALID_ID;
}

size_t BR_getStackItemIdByName(const BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id, const char* name)
{
	BR_StackNodeArray* vframe;
	if (proc_id < 0) {
		if (~proc_id >= builder->data_blocks.length) return SIZE_MAX;
		vframe = &builder->data_blocks.data[~proc_id];
	} else {
		if (proc_id >= builder->procs.length) return SIZE_MAX;
		vframe = &builder->procs.data[proc_id];
	}

	if (++op_id >= vframe->length) return SIZE_MAX;
	size_t res = 0;
	for (BR_StackNode node = vframe->data[op_id]; node; node = node->prev) {
		if (node->name ? str_eq(name, node->name) : false) return res;
		++res;
	}
	return SIZE_MAX;
}

BR_Error BR_preallocProcs(BR_ModuleBuilder* builder, uint32_t n_procs_hint)
{
	if (builder->error.type) return builder->error;
	if (!(builder->procs = BR_StackNodeArrayArray_new(-(int64_t)n_procs_hint)).data) return (builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY});
	if (!(builder->module.seg_exec = BR_ProcArray_new(-(int64_t)n_procs_hint)).data) return (builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY});
	return (BR_Error){0};
}

BR_Error BR_preallocDataBlocks(BR_ModuleBuilder* builder, uint32_t n_dbs_hint)
{
	if (builder->error.type) return builder->error;
	if (!(builder->data_blocks = BR_StackNodeArrayArray_new(-(int64_t)n_dbs_hint)).data) return (builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY});
	if (!(builder->module.seg_data = BR_DataBlockArray_new(-(int64_t)n_dbs_hint)).data) return (builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY});
	return (BR_Error){0};
}

void BR_deallocProcs(BR_Module* module)
{
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		arrayForeach (BR_Op, op, proc->body) {
			if (BR_opFlags[op->type] & BR_OPERAND_ALLOCATED)
				free(op->operand_ptr);
		}
		BR_TypeArray_clear(&proc->args);
		BR_OpArray_clear(&proc->body);
	}
	BR_ProcArray_clear(&module->seg_exec);
}

void BR_deallocDataBlocks(BR_Module* module)
{
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		arrayForeach (BR_Op, op, block->body) {
			if (BR_opFlags[op->type] & BR_OPERAND_ALLOCATED)
				free(op->operand_ptr);
		}
		BR_OpArray_clear(&block->body);
	}
	BR_DataBlockArray_clear(&module->seg_data);
}

BR_Error BR_addStruct(BR_ModuleBuilder* builder, BR_id* struct_id_p, const char* name, uint32_t n_fields, BR_Type* fields)
{
	if (builder->error.type) return builder->error;
// because the `struct_id` field of the `BR_Type` type is limited to 25 bits, a module cannot have more than 33554432 structs
	if (builder->module.seg_typeinfo.length == MAX_N_STRUCTS)
		return builder->error = (BR_Error){.type = BR_ERR_TOO_MANY_STRUCTS};
	*struct_id_p = builder->module.seg_typeinfo.length;
	if (!BR_StructArray_append(&builder->module.seg_typeinfo, (BR_Struct){.name = name}))
		return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
	BR_Struct* obj = &builder->module.seg_typeinfo.data[*struct_id_p];
// copying the fields
	if (!(obj->fields = BR_TypeArray_copy((BR_TypeArray){.length = n_fields, .data = fields})).data)
		return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
// validating that the type isn't recursive, as a struct that has a field of the same type as the struct itself has undefined size
// example in C: struct x { struct x field; };
	arrayForeach (BR_Type, field, obj->fields) {
		builder->error = validateType(&builder->module, field);
		if (builder->error.type) return builder->error;
		if (field->kind == BR_TYPE_STRUCT && field->struct_id == *struct_id_p)
			return (BR_Error){.type = BR_ERR_RECURSIVE_TYPE,
				.structs = builder->module.seg_typeinfo.data,
				.struct_id = *struct_id_p};
	}
	arrayForeach (BR_Type, field, obj->fields) {
		size_t field_align = getTypeAlignment(&builder->module, *field),
			field_size = BR_getTypeRTSize(&builder->module, *field);
		obj->size += field_align - (obj->size - 1) % field_align - 1 + field_size; // adding the field size along with the padding for it
		obj->alignment = maxInt(obj->alignment, field_align);
	}
	obj->size += obj->alignment - (obj->size - 1) % obj->alignment - 1;
	return (BR_Error){0};
}

BR_Error BR_preallocStructs(BR_ModuleBuilder* builder, uint32_t n_structs_hint)
{
	if (builder->error.type) return builder->error;
	if (!(builder->module.seg_typeinfo = BR_StructArray_new(-(int64_t)n_structs_hint)).data)
		return builder->error = (BR_Error){.type = BR_ERR_NO_MEMORY};
	return (BR_Error){0};
}

void BR_deallocStructs(BR_Module* module)
{
	arrayForeach (BR_Struct, obj, module->seg_typeinfo) {
		BR_TypeArray_clear(&obj->fields);
	}
	BR_StructArray_clear(&module->seg_typeinfo);
}

BR_id BR_getStructIdByName(const BR_Module* module, const char* name)
{
	arrayForeach (BR_Struct, obj, module->seg_typeinfo) {
		if (str_eq(obj->name, name)) return obj - module->seg_typeinfo.data;
	}
	return BR_INVALID_ID;
}
