#define BRIDGE_IMPLEMENTATION
#define _BRB_INTERNAL
#include <brb.h>
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

defArray(BRB_Proc);
defArray(BRB_Type);
defArray(BRB_Op);
defArray(BRB_DataBlock);
defArray(BRB_DataPiece);
defArray(BRB_StackNode);
defArray(BRB_StackNodeArray);

#define BRB_SNF_STACKFRAME 0x1

const sbuf BRB_opNames[]   = {
	[BRB_OP_NOP]       = fromcstr("nop"),
	[BRB_OP_END]       = fromcstr("end"),
	[BRB_OP_I8]        = fromcstr("i8"),
	[BRB_OP_I16]       = fromcstr("i16"),
	[BRB_OP_I32]       = fromcstr("i32"),
	[BRB_OP_PTR]       = fromcstr("ptr"),
	[BRB_OP_I64]       = fromcstr("i64"),
	[BRB_OP_ADDR]      = fromcstr("addr"),
	[BRB_OP_DBADDR]    = fromcstr("dbaddr"),
	[BRB_OP_LD]        = fromcstr("ld"),
	[BRB_OP_STR]       = fromcstr("str"),
	[BRB_OP_SYS]       = fromcstr("sys"),
	[BRB_OP_BUILTIN]   = fromcstr("builtin"),
	[BRB_OP_ADD]       = fromcstr("add"),
	[BRB_OP_ADDI]      = fromcstr("add-i"),
	[BRB_OP_ADDIAT8]   = fromcstr("add-i@8"),
	[BRB_OP_ADDIAT16]  = fromcstr("add-i@16"),
	[BRB_OP_ADDIAT32]  = fromcstr("add-i@32"),
	[BRB_OP_ADDIATP]   = fromcstr("add-i@p"),
	[BRB_OP_ADDIAT64]  = fromcstr("add-i@64"),
	[BRB_OP_SUB]       = fromcstr("sub"),
	[BRB_OP_SUBI]      = fromcstr("sub-i"),
	[BRB_OP_SUBIAT8]   = fromcstr("sub-i@8"),
	[BRB_OP_SUBIAT16]  = fromcstr("sub-i@16"),
	[BRB_OP_SUBIAT32]  = fromcstr("sub-i@32"),
	[BRB_OP_SUBIATP]   = fromcstr("sub-i@p"),
	[BRB_OP_SUBIAT64]  = fromcstr("sub-i@64"),
	[BRB_OP_MUL]       = fromcstr("mul"),
	[BRB_OP_MULI]      = fromcstr("mul-i"),
	[BRB_OP_MULIAT8]   = fromcstr("mul-i@8"),
	[BRB_OP_MULIAT16]  = fromcstr("mul-i@16"),
	[BRB_OP_MULIAT32]  = fromcstr("mul-i@32"),
	[BRB_OP_MULIATP]   = fromcstr("mul-i@p"),
	[BRB_OP_MULIAT64]  = fromcstr("mul-i@64"),
	[BRB_OP_DIV]       = fromcstr("div"),
	[BRB_OP_DIVI]      = fromcstr("div-i"),
	[BRB_OP_DIVIAT8]   = fromcstr("div-i@8"),
	[BRB_OP_DIVIAT16]  = fromcstr("div-i@16"),
	[BRB_OP_DIVIAT32]  = fromcstr("div-i@32"),
	[BRB_OP_DIVIATP]   = fromcstr("div-i@p"),
	[BRB_OP_DIVIAT64]  = fromcstr("div-i@64"),
	[BRB_OP_DIVS]      = fromcstr("divs"),
	[BRB_OP_DIVSI]     = fromcstr("divs-i"),
	[BRB_OP_DIVSIAT8]  = fromcstr("divs-i@8"),
	[BRB_OP_DIVSIAT16] = fromcstr("divs-i@16"),
	[BRB_OP_DIVSIAT32] = fromcstr("divs-i@32"),
	[BRB_OP_DIVSIATP]  = fromcstr("divs-i@p"),
	[BRB_OP_DIVSIAT64] = fromcstr("divs-i@64"),
	[BRB_OP_MOD]       = fromcstr("mod"),
	[BRB_OP_MODI]      = fromcstr("mod-i"),
	[BRB_OP_MODIAT8]   = fromcstr("mod-i@8"),
	[BRB_OP_MODIAT16]  = fromcstr("mod-i@16"),
	[BRB_OP_MODIAT32]  = fromcstr("mod-i@32"),
	[BRB_OP_MODIATP]   = fromcstr("mod-i@p"),
	[BRB_OP_MODIAT64]  = fromcstr("mod-i@64"),
	[BRB_OP_MODS]      = fromcstr("mods"),
	[BRB_OP_MODSI]     = fromcstr("mods-i"),
	[BRB_OP_MODSIAT8]  = fromcstr("mods-i@8"),
	[BRB_OP_MODSIAT16] = fromcstr("mods-i@16"),
	[BRB_OP_MODSIAT32] = fromcstr("mods-i@32"),
	[BRB_OP_MODSIATP]  = fromcstr("mods-i@p"),
	[BRB_OP_MODSIAT64] = fromcstr("mods-i@64"),
	[BRB_OP_DROP]      = fromcstr("drop")
};
static_assert(sizeof(BRB_opNames) / sizeof(BRB_opNames[0]) == BRB_N_OPS, "not all BRB operations have their names defined");

#define SET_OPERAND_TYPE(type) (BRB_OPERAND_##type << 0)
#define SET_ADDR_OP_TYPE(type) (BRB_ADDR_##type    << 3)
#define SET_BASE_OP(type)      (BRB_OP_##type      << 6)
const uint64_t BRB_opFlags[] = {
	[BRB_OP_NOP]       = SET_BASE_OP(NOP),
	[BRB_OP_END]       = SET_BASE_OP(END),
	[BRB_OP_I8]        = SET_BASE_OP(I8)      | SET_OPERAND_TYPE(INT8),
	[BRB_OP_I16]       = SET_BASE_OP(I16)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_I32]       = SET_BASE_OP(I32)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_PTR]       = SET_BASE_OP(PTR)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_I64]       = SET_BASE_OP(I64)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_ADDR]      = SET_BASE_OP(ADDR)    | SET_OPERAND_TYPE(VAR_NAME),
	[BRB_OP_DBADDR]    = SET_BASE_OP(DBADDR)  | SET_OPERAND_TYPE(DB_NAME),
	[BRB_OP_LD]        = SET_BASE_OP(LD)      | SET_OPERAND_TYPE(TYPE),
	[BRB_OP_STR]       = SET_BASE_OP(STR),
	[BRB_OP_SYS]       = SET_BASE_OP(SYS)     | SET_OPERAND_TYPE(SYSCALL_NAME),
	[BRB_OP_BUILTIN]   = SET_BASE_OP(BUILTIN) | SET_OPERAND_TYPE(BUILTIN),
	[BRB_OP_ADD]       = SET_BASE_OP(ADD),
	[BRB_OP_ADDI]      = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_ADDIAT8]   = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_ADDIAT16]  = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_ADDIAT32]  = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_ADDIATP]   = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_ADDIAT64]  = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_SUB]       = SET_BASE_OP(SUB),
	[BRB_OP_SUBI]      = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_SUBIAT8]   = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_SUBIAT16]  = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_SUBIAT32]  = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_SUBIATP]   = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_SUBIAT64]  = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_MUL]       = SET_BASE_OP(MUL),
	[BRB_OP_MULI]      = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_MULIAT8]   = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_MULIAT16]  = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_MULIAT32]  = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_MULIATP]   = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_MULIAT64]  = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_DIV]       = SET_BASE_OP(DIV),
	[BRB_OP_DIVI]      = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_DIVIAT8]   = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_DIVIAT16]  = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_DIVIAT32]  = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_DIVIATP]   = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_DIVIAT64]  = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_DIVS]      = SET_BASE_OP(DIVS),
	[BRB_OP_DIVSI]     = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT),
	[BRB_OP_DIVSIAT8]  = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_DIVSIAT16] = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_DIVSIAT32] = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_DIVSIATP]  = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_DIVSIAT64] = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_MOD]       = SET_BASE_OP(MOD),
	[BRB_OP_MODI]      = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_MODIAT8]   = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_MODIAT16]  = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_MODIAT32]  = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_MODIATP]   = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_MODIAT64]  = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_MODS]      = SET_BASE_OP(MODS),
	[BRB_OP_MODSI]     = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT),
	[BRB_OP_MODSIAT8]  = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_MODSIAT16] = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_MODSIAT32] = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_MODSIATP]  = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_MODSIAT64] = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_DROP]      = SET_BASE_OP(DROP)
};
static_assert(sizeof(BRB_opFlags) / sizeof(BRB_opFlags[0]) == BRB_N_OPS, "not all BRB operations have their flags defined");

const sbuf BRB_syscallNames[] = {
	[BRB_SYS_EXIT]  = fromcstr("exit"),
	[BRB_SYS_WRITE] = fromcstr("write"),
	[BRB_SYS_READ]  = fromcstr("read")
};
static_assert(sizeof(BRB_syscallNames) / sizeof(BRB_syscallNames[0]) == BRB_N_SYSCALLS, "not all BRB syscalls have their names defined");

const uintptr_t BRB_builtinValues[] = {
	[BRB_BUILTIN_NULL] = (uintptr_t)NULL,
	[BRB_BUILTIN_STDIN] = STDIN_FILENO,
	[BRB_BUILTIN_STDOUT] = STDOUT_FILENO,
	[BRB_BUILTIN_STDERR] = STDERR_FILENO,
};
static_assert(sizeof(BRB_builtinValues) / sizeof(BRB_builtinValues[0]) == BRB_N_BUILTINS, "not all BRB built-ins have their runtime values defined");

const sbuf BRB_builtinNames[] = {
	[BRB_BUILTIN_NULL] = fromcstr("NULL"),
	[BRB_BUILTIN_STDIN] = fromcstr("STDIN"),
	[BRB_BUILTIN_STDOUT] = fromcstr("STDOUT"),
	[BRB_BUILTIN_STDERR] = fromcstr("STDERR"),
};
static_assert(sizeof(BRB_builtinNames) / sizeof(BRB_builtinNames[0]) == BRB_N_BUILTINS, "not all BRB built-ins have their names defined");

const size_t BRB_syscallNArgs[] = {
	[BRB_SYS_EXIT] = 1, // [code]
	[BRB_SYS_WRITE] = 3, // [fd, addr, n_bytes]
	[BRB_SYS_READ] = 3, // [fd, addr, n_bytes]
};
static_assert(sizeof(BRB_syscallNArgs) / sizeof(BRB_syscallNArgs[0]) == BRB_N_SYSCALLS, "not all BRB syscalls have their prototype defined");

const sbuf BRB_typeNames[] = {
	[BRB_TYPE_I8] = fromcstr("i8"),
	[BRB_TYPE_I16] = fromcstr("i16"),
	[BRB_TYPE_I32] = fromcstr("i32"),
	[BRB_TYPE_PTR] = fromcstr("ptr"),
	[BRB_TYPE_I64] = fromcstr("i64"),
	[BRB_TYPE_VOID] = fromcstr("void")
};
static_assert(sizeof(BRB_typeNames) / sizeof(BRB_typeNames[0]) == BRB_N_TYPE_KINDS, "not all BRB types have their names defined");

const sbuf BRB_dataPieceNames[] = {
	[BRB_DP_BYTES] =   fromcstr("bytes"),
	[BRB_DP_I16] =     fromcstr("i16"),
	[BRB_DP_I32] =     fromcstr("i32"),
	[BRB_DP_PTR] =     fromcstr("ptr"),
	[BRB_DP_I64] =     fromcstr("i64"),
	[BRB_DP_TEXT]  =   fromcstr("text"),
	[BRB_DP_DBADDR] =  fromcstr("dbaddr"),
	[BRB_DP_ZERO] =    fromcstr("zero"),
	[BRB_DP_BUILTIN] = fromcstr("builtin")
};
static_assert(sizeof(BRB_dataPieceNames) / sizeof(BRB_dataPieceNames[0]) == BRB_N_DP_TYPES, "not all BRB data pieces have their names defined");

bool startTimerAt(struct timespec* dst)
{
	return !clock_gettime(CLOCK_MONOTONIC, dst);
}

float endTimerAt(struct timespec* src)
{
	struct timespec newtime;
	clock_gettime(CLOCK_MONOTONIC, &newtime);
	return (newtime.tv_sec - src->tv_sec) * 1000 + (newtime.tv_nsec - src->tv_nsec) / (float)1e6;
}

bool execProcess(char* command, ProcessInfo* info)
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

	char* argv[] = { "sh", "-c", command, NULL };

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

bool execProcess_s(sbuf command, ProcessInfo* info)
{
	char temp[command.length + 1];
	memcpy(temp, command.data, command.length);
	temp[command.length] = '\0';
	return execProcess(temp, info);
}

bool isPathDir(char* path)
{
	struct stat info;
	if (lstat(path, &info)) return false;
	return S_ISDIR(info.st_mode);
}

bool isPathDir_s(sbuf path)
{
	char temp[path.length + 1];
	memcpy(temp, path.data, path.length);
	temp[path.length] = '\0';
	return isPathDir(temp);
}

char* getFileExt(const char* path)
{
	char* dot = strrchr(path, '.');
	if (!dot || dot == path) return "";
	return strdup(dot + 1);
}

sbuf getFileExt_s(sbuf path)
{
	sbuf noext;
	if (!sbufsplitr(&path, &noext, fromcstr(".")).data) return fromcstr("");
	return path;
}

char* setFileExt(const char* path, const char* ext)
{
	sbuf src = fromstr((char*)path);
	sbuf noext;
	sbufsplitr(&src, &noext, fromcstr("."));
	return *ext ? tostr(noext, fromcstr("."), fromstr((char*)ext)) : tostr(noext);
}

sbuf setFileExt_s(sbuf path, sbuf ext)
{
	sbuf noext;
	sbufsplitr(&path, &noext, fromcstr("."));
	return ext.length ? sbufconcat(noext, fromcstr("."), ext) : sbufcopy(noext);
}

char* fileBaseName(const char* path)
{
	sbuf src = fromstr((char*)path);
	sbuf res;
	if (sbufeq(sbufsplitr(&src, &res, fromcstr("."), PATHSEP), fromcstr("."))) {
		return tostr(sbufsplitr(&res, &src, PATHSEP).length ? res : src);
	} else return tostr(src.length ? src : res);
}

sbuf fileBaseName_s(sbuf path)
{
	sbuf res;
	if (sbufeq(sbufsplitr(&path, &res, fromcstr("."), PATHSEP), fromcstr("."))) {
		return sbufsplitr(&res, &path, PATHSEP).length ? res : path;
	} else return path.length ? path : res;
}

void BRB_printErrorMsg(FILE* dst, BRB_Error err, const char* prefix)
{
	if (err.type == BRB_ERR_OK) return;
	if (err.loc) {
		BRP_fprintTokenLoc(dst, *err.loc);
		fputs(": ", dst);
	}
	if (prefix) fprintf(dst, "%s: ", prefix);
	switch (err.type) {
		case BRB_ERR_INVALID_HEADER:
			fputs("invalid module header: \"", dst);
			fputsbufesc(dst, (sbuf){ .data = err.header, .length = BRB_HEADER_SIZE }, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE);
			fputs("\"\n", dst);
			break;
		case BRB_ERR_NO_HEADER:
			fputs("no module header found\n", dst);
			break;
		case BRB_ERR_NO_DATA_SEG:
			fputs("no `data` segment found\n", dst);
			break;
		case BRB_ERR_NO_MEMORY:
			fputs("memory allocation failed\n", dst);
			break;
		case BRB_ERR_NO_EXEC_SEG:
			fputs("no `exec` segment found\n", dst);
			break;
		case BRB_ERR_NO_OPCODE:
			fputs("unexpected end-of-input while loading type of an operation\n", dst);
			break;
		case BRB_ERR_INVALID_OPCODE:
			fprintf(dst, "invalid operation type: %u\n", err.opcode);
			break;
		case BRB_ERR_NO_OPERAND:
			fprintf(dst, "unexpected end-of-input while loading operand of an operation `%.*s`\n", unpack(BRB_opNames[err.opcode]));
			break;
		case BRB_ERR_INVALID_NAME:
			fputs("invalid name found\n", dst);
			break;
		case BRB_ERR_NAMES_NOT_RESOLVED:
			fputs("not all symbol names were resolved from the `name` segment\n", dst);
			break;
		case BRB_ERR_INVALID_BUILTIN:
			fprintf(dst, "invalid built-in constant #%u\n", err.builtin_id);
			break;
		case BRB_ERR_INVALID_SYSCALL:
			fprintf(dst, "invalid syscall #%u\n", err.syscall_id);
			break;
		case BRB_ERR_STACK_UNDERFLOW:
			if (err.opcode == BRB_N_OPS) {
				fprintf(dst, "attempted to label head of an empty stack\n");
			} else fprintf(dst,
				"stack underflow for operation `%.*s`: expected %u items, instead got %u\n",
				unpack(BRB_opNames[err.opcode]),
				err.expected_stack_length,
				err.actual_stack_length
			);
			break;
		case BRB_ERR_OPERAND_TOO_LARGE:
			fprintf(dst, "operand for operation `%.*s` is too large\n", unpack(BRB_opNames[err.opcode]));
			break;
		case BRB_ERR_OPERAND_OUT_OF_RANGE:
			fprintf(dst, "operand %llu is out of range for ", err.operand);
			if (err.opcode < 0) {
				fprintf(dst, "data piece `%.*s`\n", unpack(BRB_dataPieceNames[err.opcode]));
			} else fprintf(dst, "operation `%.*s`\n", unpack(BRB_opNames[err.opcode]));
			break;
		case BRB_ERR_NO_PROC_RET_TYPE:
			fprintf(dst, "unexpected end-of-input while loading return type of a procedure\n");
			break;
		case BRB_ERR_NO_PROC_NAME:
			fprintf(dst, "unexpected end-of-input while loading name of a procedure\n");
			break;
		case BRB_ERR_NO_PROC_ARG:
			fprintf(dst, "unexpected end-of-input while loading arguments of a procedure\n");
			break;
		case BRB_ERR_NO_PROC_BODY_SIZE:
			fprintf(dst, "unexpected end-of-input while loading body size of a procedure\n");
			break;
		case BRB_ERR_NO_DP_TYPE:
			fprintf(dst, "unexpected end-of-input while loading type of a data piece\n");
			break;
		case BRB_ERR_INVALID_DP_TYPE:
			fprintf(dst, "invalid data piece type: %u\n", err.opcode);
			break;
		case BRB_ERR_NO_DP_CONTENT:
			fprintf(dst, "unexpected end-of-input while loading data piece content\n");
			break;
		case BRB_ERR_NO_DB_NAME:
			fprintf(dst, "unexpected end-of-input while loading name of a data block\n");
		case BRB_ERR_NO_DB_BODY_SIZE:
			fprintf(dst, "unexpected end-of-input whole loading body size of a data block\n");
			break;
		case BRB_ERR_NO_ENTRY:
			fprintf(dst, "unexpected end-of-input while loading the entry point\n");
			break;
		case BRB_ERR_INVALID_ENTRY:
			fprintf(dst, "invalid entry point ID provided\n");
			break;
		case BRB_ERR_INVALID_ENTRY_PROTOTYPE:
			fprintf(dst, "the entry point procedure must not accept any arguments or return anything\n");
			break;
		case BRB_ERR_TYPE_EXPECTED:
			fprintf(dst, "expected a type specification\n");
			break;
		case BRB_ERR_OP_NAME_EXPECTED:
			fprintf(dst, "expected an operation name\n");
			break;
		case BRB_ERR_INT_OPERAND_EXPECTED:
			fprintf(dst, "expected an integer as an operand\n");
			break;
		case BRB_ERR_INT_OR_NAME_OPERAND_EXPECTED:
			fprintf(dst, "expected an integer or a name as an operand\n");
			break;
		case BRB_ERR_BUILTIN_OPERAND_EXPECTED:
			fprintf(dst, "expected name of a built-in constant as an operand\n");
			break;
		case BRB_ERR_DP_NAME_EXPECTED:
			fprintf(dst, "expected a data piece name\n");
			break;
		case BRB_ERR_TEXT_OPERAND_EXPECTED:
			fprintf(dst, "expected a string literal as an operand\n");
			break;
		case BRB_ERR_INVALID_TEXT_OPERAND:
			fprintf(dst, "a `text` data piece cannot contain null bytes\n");
			break;
		case BRB_ERR_INVALID_DECL:
			fprintf(dst, "top-level statements in the assembly code must start with either a return type specification, or with keyword `data`\n");
			break;
		case BRB_ERR_ARGS_EXPECTED:
			fprintf(dst, "symbol `(` expected after name of the declared procedure\n");
			break;
		case BRB_ERR_PROTOTYPE_MISMATCH:
			fprintf(dst, "procedure `%s` was already declared, but with another prototype\n", err.name);
			break;
		case BRB_ERR_SYSCALL_NAME_EXPECTED:
			fprintf(dst, "expected a syscall name as an operand\n");
			break;
		case BRB_ERR_INVALID_ARRAY_SIZE_SPEC:
			fprintf(dst, "array types must be denoted the following way: T[n]\n");
			break;
		case BRB_ERR_TYPE_MISMATCH:
			fprintf(dst, "argument #%u for a `%.*s` operation is expected to be of type `", err.arg_id, unpack(BRB_opNames[err.opcode]));
			BRB_printType(err.expected_type, dst);
			fputs("` instead got an argument of type `", dst);
			BRB_printType(err.actual_type, dst);
			fputs("`\n", dst);
			break;
		case BRB_ERR_DEL_ARGS:
			fputs("attempted to delete procedure arguments from the stack\n", dst);
			break;
		case BRB_ERR_OK:
		case BRB_N_ERROR_TYPES:
		default:
			fputs("undefined\n", dst);
	}
}

char* BRB_getErrorMsg(BRB_Error err, const char* prefix) {
	sbuf res = {0};
	FILE* stream = open_memstream(&res.data, &res.length);
	BRB_printErrorMsg(stream, err, prefix);
	fclose(stream);
	return res.data;
}

BRB_Error BRB_initModuleBuilder(BRB_ModuleBuilder* builder)
{
	*builder = (BRB_ModuleBuilder){0};
	builder->module.exec_entry_point = SIZE_MAX;
	return (BRB_Error){0};
}

BRB_Error BRB_analyzeModule(const BRB_Module* module, BRB_ModuleBuilder* dst)
{
	BRB_Error err;
	if ((err = BRB_initModuleBuilder(dst)).type) return err;
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		uint32_t _;
		if ((err = BRB_addDataBlock(dst, &_, block->name, block->is_mutable, block->pieces.length)).type) return err;
	}
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		uint32_t _;
		if ((err = BRB_addProc(dst, &_, proc->name, proc->args.length, proc->args.data, proc->ret_type, proc->body.length)).type) return err;
	}
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		arrayForeach (BRB_DataPiece, piece, block->pieces) {
			if ((err = BRB_addDataPiece(dst, block - module->seg_data.data, *piece)).type) return err;
		}
	}
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		arrayForeach (BRB_Op, op, proc->body) {
			if ((err = BRB_addOp(dst, proc - module->seg_exec.data, *op)).type) return err;
		}
	}
	return BRB_setEntryPoint(dst, module->exec_entry_point);
}

BRB_Error BRB_extractModule(BRB_ModuleBuilder builder, BRB_Module* dst)
{
	if (builder.error.type) return builder.error;
	arrayForeach (BRB_StackNodeArray, proc_info, builder.procs) {
		BRB_StackNodeArray_clear(proc_info);
	}
	arena_free(&builder.arena);
	BRB_StackNodeArrayArray_clear(&builder.procs);
	*dst = builder.module;
	return (BRB_Error){0};
}

BRB_Error BRB_setEntryPoint(BRB_ModuleBuilder* builder, size_t proc_id)
{
	builder->module.exec_entry_point = proc_id;
	return (BRB_Error){0};
}

FILE* BRB_findModule(const char* name, const char* search_paths[])
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

static BRB_StackNode getNthStackNode(BRB_StackNode head, size_t n)
{
	while (head && n--) head = head->prev;
	return head;
}

static BRB_StackNode getNthStackNode_nonInternal(BRB_StackNode head, size_t n)
{
	if (head && !n ? head->flags & BRB_SNF_STACKFRAME : false) {
		return head->prev;
	}
	while (head && n--) {
		if (head->flags & BRB_SNF_STACKFRAME) ++n;
		head = head->prev;
	}
	return head;
}

static size_t getStackLength(BRB_StackNode head)
{
	size_t res = 0;
	while (head) {
		head = head->prev;
		++res;
	}
	return res;
}

static bool compareTypes(BRB_Type field, BRB_Type entry)
{
	switch (field.internal_kind) {
		case BRB_TYPE_ANY: return entry.kind != BRB_TYPE_VOID;
		case BRB_TYPE_INT: return entry.kind != BRB_TYPE_VOID && entry.n_items == 1;
		default: return entry.kind == field.kind && entry.n_items == field.n_items;
	}
}

static void __unused printStack(BRB_StackNode head)
{
	putchar('[');
	while (head) {
		BRB_printType(head->type, stdout);
		if (head->prev) putstr(", ");
		head = head->prev;
	}
	putchar(']');
}

static BRB_Error changeStack(BRB_StackNodeArray* stack, BRB_Op op, size_t n_in, BRB_Type* in_types, size_t n_out, BRB_Type* out_types, Arena* allocator)
{
	BRB_StackNode iter = *arrayhead(*stack);
	if (getStackLength(iter) < n_in)
		return (BRB_Error){
			.type = BRB_ERR_STACK_UNDERFLOW,
			.opcode = op.type,
			.expected_stack_length = n_in,
			.actual_stack_length = getStackLength(iter)
		};
	arrayForeach (BRB_Type, type, ((BRB_TypeArray){.data = in_types, .length = n_in})) {
		if (iter->flags & BRB_SNF_STACKFRAME)
			return (BRB_Error){.type = BRB_ERR_DEL_ARGS};
		if (!compareTypes(*type, iter->type))
			return (BRB_Error){
				.type = BRB_ERR_TYPE_MISMATCH,
				.opcode = op.type,
				.expected_type = *type,
				.actual_type = iter->type,
				.arg_id = type - in_types
			};
		iter = iter->prev;
	}

	BRB_StackNode node = getNthStackNode(*arrayhead(*stack), n_in), prev;
	arrayRevForeach (BRB_Type, type, ((BRB_TypeArray){.data = out_types, .length = n_out})) {
		prev = node;
		if (!(node = arena_alloc(allocator, sizeof(struct BRB_stacknode_t)))) return (BRB_Error){.type = BRB_ERR_NO_MEMORY};
		*node = (struct BRB_stacknode_t){0};
		node->prev = prev;
		switch (type->internal_kind) {
			case BRB_TYPE_OF:
				node->type = getNthStackNode(*arrayhead(*stack), type->n_items)->type; break;
			case BRB_TYPE_INPUT:
				node->type = op.operand_type; break;
			default:
				node->type = *type;
		}
		node->type.internal_kind = 0;
	}
	return (BRB_Error){.type = BRB_StackNodeArray_append(stack, node) ? 0 : BRB_ERR_NO_MEMORY};
}

fieldArray BRB_getNameFields(BRB_Module* module)
{
	fieldArray res = fieldArray_new(-(int64_t)module->seg_data.length - module->seg_exec.length);
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		fieldArray_append(&res, &block->name);
	}

	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		fieldArray_append(&res, &proc->name);
	}

	return res;
}

BRB_Error BRB_addProc(BRB_ModuleBuilder* builder, uint32_t* proc_id_p, const char* name, size_t n_args, BRB_Type* args, BRB_Type ret_type, uint32_t n_ops_hint)
{
	if (builder->error.type) return builder->error;

	ret_type.internal_kind = 0;
	if (!BRB_ProcArray_append(&builder->module.seg_exec, (BRB_Proc){.name = name, .ret_type = ret_type})
		|| !BRB_StackNodeArrayArray_append(&builder->procs, (BRB_StackNodeArray){0}))
		return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});

	if (!(arrayhead(builder->module.seg_exec)->args = BRB_TypeArray_copy((BRB_TypeArray){
		.data = args,
		.length = n_args
	})).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	arrayForeach (BRB_Type, arg, arrayhead(builder->module.seg_exec)->args) {
		arg->internal_kind = 0;
	}

	if (n_ops_hint)
		if (!(arrayhead(builder->module.seg_exec)->body = BRB_OpArray_new(-(int32_t)n_ops_hint)).data
			|| !(*arrayhead(builder->procs) = BRB_StackNodeArray_new(-(int32_t)n_ops_hint)).data)
			return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});

	BRB_StackNode input = NULL, prev;
 	while (n_args--) {
		prev = input;
		if (!(input = arena_alloc(&builder->arena, sizeof(struct BRB_stacknode_t)))) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
		input->prev = prev;
		input->type = args[n_args];
		input->name = NULL;
		input->flags = BRB_SNF_STACKFRAME;
	}

	prev = input;
	if (!(input = arena_alloc(&builder->arena, sizeof(struct BRB_stacknode_t)))) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	input->prev = prev;
	input->type = BRB_PTR_TYPE(2);
	input->name = NULL;
	input->flags = BRB_SNF_STACKFRAME;

	*proc_id_p = builder->module.seg_exec.length - 1;
	return (builder->error = (BRB_Error){.type = BRB_StackNodeArray_append(arrayhead(builder->procs), input) ? 0 : BRB_ERR_NO_MEMORY});
}

BRB_Error BRB_addOp(BRB_ModuleBuilder* builder, uint32_t proc_id, BRB_Op op)
{
	static uint8_t n_in[] = {
		[BRB_OP_NOP]       = 0,
		[BRB_OP_END]       = 0,
		[BRB_OP_I8]        = 0,
		[BRB_OP_I16]       = 0,
		[BRB_OP_I32]       = 0,
		[BRB_OP_PTR]       = 0,
		[BRB_OP_I64]       = 0,
		[BRB_OP_ADDR]      = 0,
		[BRB_OP_DBADDR]    = 0,
		[BRB_OP_LD]        = 1,
		[BRB_OP_STR]       = 2,
		[BRB_OP_SYS]       = 0, // needs to be set manually every time, depending on the system function in question
		[BRB_OP_BUILTIN]   = 0,
		[BRB_OP_ADD]       = 2,
		[BRB_OP_ADDI]      = 1,
		[BRB_OP_ADDIAT8]   = 1,
		[BRB_OP_ADDIAT16]  = 1,
		[BRB_OP_ADDIAT32]  = 1,
		[BRB_OP_ADDIATP]   = 1,
		[BRB_OP_ADDIAT64]  = 1,
		[BRB_OP_SUB]       = 2,
		[BRB_OP_SUBI]      = 1,
		[BRB_OP_SUBIAT8]   = 1,
		[BRB_OP_SUBIAT16]  = 1,
		[BRB_OP_SUBIAT32]  = 1,
		[BRB_OP_SUBIATP]   = 1,
		[BRB_OP_SUBIAT64]  = 1,
		[BRB_OP_MUL]       = 2,
		[BRB_OP_MULI]      = 1,
		[BRB_OP_MULIAT8]   = 1,
		[BRB_OP_MULIAT16]  = 1,
		[BRB_OP_MULIAT32]  = 1,
		[BRB_OP_MULIATP]   = 1,
		[BRB_OP_MULIAT64]  = 1,
		[BRB_OP_DIV]       = 2,
		[BRB_OP_DIVI]      = 1,
		[BRB_OP_DIVIAT8]   = 1,
		[BRB_OP_DIVIAT16]  = 1,
		[BRB_OP_DIVIAT32]  = 1,
		[BRB_OP_DIVIATP]   = 1,
		[BRB_OP_DIVIAT64]  = 1,
		[BRB_OP_DIVS]      = 2,
		[BRB_OP_DIVSI]     = 1,
		[BRB_OP_DIVSIAT8]  = 1,
		[BRB_OP_DIVSIAT16] = 1,
		[BRB_OP_DIVSIAT32] = 1,
		[BRB_OP_DIVSIATP]  = 1,
		[BRB_OP_DIVSIAT64] = 1,
		[BRB_OP_MOD]       = 2,
		[BRB_OP_MODI]      = 1,
		[BRB_OP_MODIAT8]   = 1,
		[BRB_OP_MODIAT16]  = 1,
		[BRB_OP_MODIAT32]  = 1,
		[BRB_OP_MODIATP]   = 1,
		[BRB_OP_MODIAT64]  = 1,
		[BRB_OP_MODS]       = 2,
		[BRB_OP_MODSI]      = 1,
		[BRB_OP_MODSIAT8]   = 1,
		[BRB_OP_MODSIAT16]  = 1,
		[BRB_OP_MODSIAT32]  = 1,
		[BRB_OP_MODSIATP]   = 1,
		[BRB_OP_MODSIAT64]  = 1,
		[BRB_OP_DROP]      = 1
	};
	static BRB_Type in_types[][3] = {
		[BRB_OP_NOP]       = { BRB_VOID_TYPE },
		[BRB_OP_END]       = { BRB_VOID_TYPE },
		[BRB_OP_I8]        = { BRB_VOID_TYPE },
		[BRB_OP_I16]       = { BRB_VOID_TYPE },
		[BRB_OP_I32]       = { BRB_VOID_TYPE },
		[BRB_OP_PTR]       = { BRB_VOID_TYPE },
		[BRB_OP_I64]       = { BRB_VOID_TYPE },
		[BRB_OP_ADDR]      = { BRB_VOID_TYPE },
		[BRB_OP_DBADDR]    = { BRB_VOID_TYPE },
		[BRB_OP_LD]        = { BRB_PTR_TYPE(1) },
		[BRB_OP_STR]       = { BRB_PTR_TYPE(1), BRB_ANY_TYPE },
		[BRB_OP_SYS]       = { BRB_VOID_TYPE }, // needs to be set manually every time, depending on the system function in question
		[BRB_OP_BUILTIN]   = { BRB_VOID_TYPE },
		[BRB_OP_ADD]       = { BRB_INT_TYPE, BRB_INT_TYPE },
		[BRB_OP_ADDI]      = { BRB_INT_TYPE },
		[BRB_OP_ADDIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUB]       = { BRB_INT_TYPE, BRB_INT_TYPE },
		[BRB_OP_SUBI]      = { BRB_INT_TYPE },
		[BRB_OP_SUBIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MUL]       = { BRB_INT_TYPE, BRB_INT_TYPE },
		[BRB_OP_MULI]      = { BRB_INT_TYPE },
		[BRB_OP_MULIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIV]       = { BRB_INT_TYPE, BRB_INT_TYPE },
		[BRB_OP_DIVI]      = { BRB_INT_TYPE },
		[BRB_OP_DIVIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVS]      = { BRB_INT_TYPE, BRB_INT_TYPE },
		[BRB_OP_DIVSI]     = { BRB_INT_TYPE },
		[BRB_OP_DIVSIAT8]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIAT16] = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIAT32] = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIAT64] = { BRB_PTR_TYPE(1) },
		[BRB_OP_MOD]       = { BRB_INT_TYPE, BRB_INT_TYPE },
		[BRB_OP_MODI]      = { BRB_INT_TYPE },
		[BRB_OP_MODIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODS]      = { BRB_INT_TYPE, BRB_INT_TYPE },
		[BRB_OP_MODSI]     = { BRB_INT_TYPE },
		[BRB_OP_MODSIAT8]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIAT16] = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIAT32] = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIAT64] = { BRB_PTR_TYPE(1) },
		[BRB_OP_DROP]      = { BRB_ANY_TYPE }
	};
	static_assert(BRB_N_OPS == 63, "not all BRB operations have their input types defined");
	static uint8_t n_out[] = {
		[BRB_OP_NOP]       = 0,
		[BRB_OP_END]       = 0,
		[BRB_OP_I8]        = 1,
		[BRB_OP_I16]       = 1,
		[BRB_OP_I32]       = 1,
		[BRB_OP_PTR]       = 1,
		[BRB_OP_I64]       = 1,
		[BRB_OP_ADDR]      = 1,
		[BRB_OP_DBADDR]    = 1,
		[BRB_OP_LD]        = 1,
		[BRB_OP_STR]       = 0,
		[BRB_OP_SYS]       = 1,
		[BRB_OP_BUILTIN]   = 1,
		[BRB_OP_ADD]       = 1,
		[BRB_OP_ADDI]      = 1,
		[BRB_OP_ADDIAT8]   = 1,
		[BRB_OP_ADDIAT16]  = 1,
		[BRB_OP_ADDIAT32]  = 1,
		[BRB_OP_ADDIATP]   = 1,
		[BRB_OP_ADDIAT64]  = 1,
		[BRB_OP_SUB]       = 1,
		[BRB_OP_SUBI]      = 1,
		[BRB_OP_SUBIAT8]   = 1,
		[BRB_OP_SUBIAT16]  = 1,
		[BRB_OP_SUBIAT32]  = 1,
		[BRB_OP_SUBIATP]   = 1,
		[BRB_OP_SUBIAT64]  = 1,
		[BRB_OP_MUL]       = 1,
		[BRB_OP_MULI]      = 1,
		[BRB_OP_MULIAT8]   = 1,
		[BRB_OP_MULIAT16]  = 1,
		[BRB_OP_MULIAT32]  = 1,
		[BRB_OP_MULIATP]   = 1,
		[BRB_OP_MULIAT64]  = 1,
		[BRB_OP_DIV]       = 1,
		[BRB_OP_DIVI]      = 1,
		[BRB_OP_DIVIAT8]   = 1,
		[BRB_OP_DIVIAT16]  = 1,
		[BRB_OP_DIVIAT32]  = 1,
		[BRB_OP_DIVIATP]   = 1,
		[BRB_OP_DIVIAT64]  = 1,
		[BRB_OP_DIVS]      = 1,
		[BRB_OP_DIVSI]     = 1,
		[BRB_OP_DIVSIAT8]  = 1,
		[BRB_OP_DIVSIAT16] = 1,
		[BRB_OP_DIVSIAT32] = 1,
		[BRB_OP_DIVSIATP]  = 1,
		[BRB_OP_DIVSIAT64] = 1,
		[BRB_OP_MOD]       = 1,
		[BRB_OP_MODI]      = 1,
		[BRB_OP_MODIAT8]   = 1,
		[BRB_OP_MODIAT16]  = 1,
		[BRB_OP_MODIAT32]  = 1,
		[BRB_OP_MODIATP]   = 1,
		[BRB_OP_MODIAT64]  = 1,
		[BRB_OP_MODS]       = 1,
		[BRB_OP_MODSI]      = 1,
		[BRB_OP_MODSIAT8]   = 1,
		[BRB_OP_MODSIAT16]  = 1,
		[BRB_OP_MODSIAT32]  = 1,
		[BRB_OP_MODSIATP]   = 1,
		[BRB_OP_MODSIAT64]  = 1,
		[BRB_OP_DROP]      = 0
	};
	static BRB_Type out_types[][1] = {
		[BRB_OP_NOP]       = { BRB_VOID_TYPE },
		[BRB_OP_END]       = { BRB_VOID_TYPE },
		[BRB_OP_I8]        = { BRB_I8_TYPE(1) },
		[BRB_OP_I16]       = { BRB_I16_TYPE(1) },
		[BRB_OP_I32]       = { BRB_I32_TYPE(1) },
		[BRB_OP_PTR]       = { BRB_PTR_TYPE(1) },
		[BRB_OP_I64]       = { BRB_I64_TYPE(1) },
		[BRB_OP_ADDR]      = { BRB_PTR_TYPE(1) },
		[BRB_OP_DBADDR]    = { BRB_PTR_TYPE(1) },
		[BRB_OP_LD]        = { BRB_INPUT_TYPE },
		[BRB_OP_STR]       = { BRB_VOID_TYPE },
		[BRB_OP_SYS]       = { BRB_PTR_TYPE(1) },
		[BRB_OP_BUILTIN]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADD]       = { BRB_TYPEOF(0) },
		[BRB_OP_ADDI]      = { BRB_TYPEOF(0) },
		[BRB_OP_ADDIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_ADDIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_ADDIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_ADDIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_SUB]       = { BRB_TYPEOF(0) },
		[BRB_OP_SUBI]      = { BRB_TYPEOF(0) },
		[BRB_OP_SUBIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_SUBIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_SUBIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_SUBIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_MUL]       = { BRB_TYPEOF(0) },
		[BRB_OP_MULI]      = { BRB_TYPEOF(0) },
		[BRB_OP_MULIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_MULIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_MULIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_MULIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_DIV]       = { BRB_TYPEOF(0) },
		[BRB_OP_DIVI]      = { BRB_TYPEOF(0) },
		[BRB_OP_DIVIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_DIVIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_DIVIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_DIVIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_DIVS]      = { BRB_TYPEOF(0) },
		[BRB_OP_DIVSI]     = { BRB_TYPEOF(0) },
		[BRB_OP_DIVSIAT8]  = { BRB_I8_TYPE(1) },
		[BRB_OP_DIVSIAT16] = { BRB_I16_TYPE(1) },
		[BRB_OP_DIVSIAT32] = { BRB_I32_TYPE(1) },
		[BRB_OP_DIVSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIAT64] = { BRB_I64_TYPE(1) },
		[BRB_OP_MOD]       = { BRB_TYPEOF(0) },
		[BRB_OP_MODI]      = { BRB_TYPEOF(0) },
		[BRB_OP_MODIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_MODIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_MODIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_MODIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_MODS]      = { BRB_TYPEOF(0) },
		[BRB_OP_MODSI]     = { BRB_TYPEOF(0) },
		[BRB_OP_MODSIAT8]  = { BRB_I8_TYPE(1) },
		[BRB_OP_MODSIAT16] = { BRB_I16_TYPE(1) },
		[BRB_OP_MODSIAT32] = { BRB_I32_TYPE(1) },
		[BRB_OP_MODSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIAT64] = { BRB_I64_TYPE(1) },
		[BRB_OP_DROP]      = { BRB_VOID_TYPE }
	};
	static_assert(BRB_N_OPS == 63, "not all BRB operations have their output types defined");
	static uint8_t sys_n_in[] = {
		[BRB_SYS_EXIT] = 1,
		[BRB_SYS_WRITE] = 3,
		[BRB_SYS_READ] = 3
	};
	static BRB_Type sys_in_types[][3] = {
		[BRB_SYS_EXIT] = { BRB_PTR_TYPE(1) },
		[BRB_SYS_WRITE] = { BRB_PTR_TYPE(1), BRB_PTR_TYPE(1), BRB_PTR_TYPE(1) },
		[BRB_SYS_READ] = { BRB_PTR_TYPE(1), BRB_PTR_TYPE(1), BRB_PTR_TYPE(1) }
	};
	static_assert(BRB_N_SYSCALLS == 3, "not all BRB syscalls have their input types defined");

	if (builder->error.type) return builder->error;
	if (!BRB_OpArray_append(&builder->module.seg_exec.data[proc_id].body, op))
		return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});

	if (op.type == BRB_OP_ADDR) {
			if (op.operand_u >= UINT32_MAX) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_TOO_LARGE, .opcode = BRB_OP_ADDR});
			}
			if (op.operand_u ? !getNthStackNode_nonInternal(*arrayhead(builder->procs.data[proc_id]), op.operand_u) : false) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = BRB_OP_ADDR, .operand = op.operand_u});
			}
	} else if (op.type == BRB_OP_DBADDR) {
			if (op.operand_u >= UINT32_MAX) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_TOO_LARGE, .opcode = BRB_OP_DBADDR});
			}
			if (op.operand_u >= builder->module.seg_data.length) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = BRB_OP_DBADDR, .operand = op.operand_u});
			}
	} else if (op.type == BRB_OP_SYS) {
			if (op.operand_u >= BRB_N_SYSCALLS) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){ .type = BRB_ERR_INVALID_SYSCALL, .syscall_id = op.operand_u });
			}
			n_in[BRB_OP_SYS] = sys_n_in[op.operand_u];
			memcpy(in_types[BRB_OP_SYS], sys_in_types[op.operand_u], sizeof(in_types[BRB_OP_SYS]));
	} else if (op.type == BRB_OP_BUILTIN && op.operand_u >= BRB_N_BUILTINS) {
		--builder->module.seg_exec.data[proc_id].body.length;
		return (builder->error = (BRB_Error){ .type = BRB_ERR_INVALID_BUILTIN, .builtin_id = op.operand_u });
	} else if (
		(BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_DIV
			|| BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_DIVS
			|| BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_MOD
			|| BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_MODS)
		&& BRB_GET_OPERAND_TYPE(op.type) == BRB_OPERAND_INT
		&& op.operand_u == 0
	) {
		--builder->module.seg_exec.data[proc_id].body.length;
		return (builder->error = (BRB_Error){ .type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = 0 });
	} else if (op.type >= BRB_N_OPS) {
		--builder->module.seg_exec.data[proc_id].body.length;
		return (builder->error = (BRB_Error){.type = BRB_ERR_INVALID_OPCODE, .opcode = op.type});
	}

	if ((builder->error = changeStack(
		&builder->procs.data[proc_id],
		op,
		n_in[op.type], in_types[op.type],
		n_out[op.type], out_types[op.type],
		&builder->arena
	)).type) {
		--builder->module.seg_exec.data[proc_id].body.length;
		return builder->error;
	}
	return (BRB_Error){0};
}

BRB_Error BRB_labelStackItem(BRB_ModuleBuilder* builder, uint32_t proc_id, uint32_t op_id, uint32_t item_id, const char* name)
{
	if (builder->error.type) return builder->error;
	BRB_StackNode target = getNthStackNode(builder->procs.data[proc_id].data[op_id], item_id);
	if (!target) return (builder->error = (BRB_Error){
		.type = BRB_ERR_STACK_UNDERFLOW,
		.opcode = BRB_N_OPS,
		.expected_stack_length = 1,
		.actual_stack_length = 0
	});
	target->name = name;
	return (BRB_Error){0};
}

BRB_Error BRB_addDataBlock(BRB_ModuleBuilder* builder, uint32_t* db_id_p, const char* name, bool is_mutable, uint32_t n_pieces_hint)
{
	if (builder->error.type) return builder->error;
	if (!BRB_DataBlockArray_append(&builder->module.seg_data, (BRB_DataBlock){
		.name = name,
		.is_mutable = is_mutable
	})) return (builder->error = (BRB_Error){ .type = BRB_ERR_NO_MEMORY });
	if (!(arrayhead(builder->module.seg_data)->pieces = BRB_DataPieceArray_new(-(int32_t)n_pieces_hint)).data)
		return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	*db_id_p = builder->module.seg_data.length - 1;
	return (BRB_Error){0};
}

BRB_Error BRB_addDataPiece(BRB_ModuleBuilder* builder, uint32_t db_id, BRB_DataPiece piece)
{
	if (builder->error.type) return builder->error;
	switch (piece.type) {
		case BRB_DP_BUILTIN:
			if (piece.content_u >= BRB_N_BUILTINS)
				return (builder->error = (BRB_Error){.type = BRB_ERR_INVALID_BUILTIN});
			break;
		case BRB_DP_DBADDR:
			if (piece.content_u >= builder->module.seg_data.length)
				return (builder->error = (BRB_Error){
					.type = BRB_ERR_OPERAND_OUT_OF_RANGE,
					.opcode = -BRB_DP_DBADDR,
					.operand = piece.content_u
				});
			break;
		case BRB_DP_BYTES:
		case BRB_DP_I16:
		case BRB_DP_I32:
		case BRB_DP_PTR:
		case BRB_DP_I64:
		case BRB_DP_TEXT:
		case BRB_DP_ZERO:
			break;
		case BRB_DP_NONE:
		case BRB_N_DP_TYPES:
			return (builder->error = (BRB_Error){.type = BRB_ERR_INVALID_DP_TYPE, .opcode = piece.type});
	}

	return (builder->error = (BRB_Error){
		.type = BRB_DataPieceArray_append(&builder->module.seg_data.data[db_id].pieces, piece)
			? 0 : BRB_ERR_NO_MEMORY
	});
}

size_t BRB_getDataBlockIdByName(BRB_Module* module, const char* name)
{
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		if (streq(block->name, name)) return block - module->seg_data.data;
	}
	return SIZE_MAX;
}

size_t BRB_getTypeRTSize(BRB_Type type)
{
	static size_t coeffs[] = {
		[BRB_TYPE_I8] = 1,
		[BRB_TYPE_I16] = 2,
		[BRB_TYPE_I32] = 4,
		[BRB_TYPE_PTR] = sizeof(void*),
		[BRB_TYPE_I64] = 8,
		[BRB_TYPE_VOID] = 0
	};
	return type.n_items * coeffs[type.kind];
}

size_t BRB_getStackItemRTOffset(BRB_ModuleBuilder* builder, uint32_t proc_id, uint32_t op_id, size_t item_id)
{
	const bool strict = item_id != SIZE_MAX;
	if (proc_id >= builder->procs.length) return SIZE_MAX;
	if (++op_id >= builder->procs.data[proc_id].length) return SIZE_MAX;
	// `++op_id` because the first element in the state stack is always the initial state, determined by proc args
	size_t res = 0;
	BRB_StackNode node = builder->procs.data[proc_id].data[op_id];
	while (node ? node->flags & BRB_SNF_STACKFRAME : false) node = node->prev; // setting up the iterator
	while (node && item_id) {
		res += BRB_getTypeRTSize(node->type);
		if (!(node->flags & BRB_SNF_STACKFRAME)) --item_id;
		node = node->prev;
	}
	return item_id && strict ? SIZE_MAX : res;
}

size_t BRB_getStackRTSize(BRB_ModuleBuilder* builder, uint32_t proc_id, uint32_t op_id)
{
	return BRB_getStackItemRTOffset(builder, proc_id, op_id, SIZE_MAX);
}

bool BRB_getStackItemType(BRB_ModuleBuilder* builder, BRB_Type* dst, uint32_t proc_id, uint32_t op_id, uint32_t item_id)
{
	if (proc_id >= builder->procs.length) return false;
	if (++op_id >= builder->procs.data[proc_id].length) return false;
	// `++op_id` because the first element in the state stack is always the initial state, determined by proc args
	BRB_StackNode node = builder->procs.data[proc_id].data[op_id];
	while (node && item_id--) node = node->prev;
	if (!node) return false;
	*dst = node->type;
	return true;
}

size_t BRB_getStackItemRTSize(BRB_ModuleBuilder* builder, uint32_t proc_id, uint32_t op_id, uint32_t item_id)
{
	if (proc_id >= builder->procs.length) return SIZE_MAX;
	BRB_Type res;
	if (!BRB_getStackItemType(builder, &res, proc_id, op_id, item_id)) return SIZE_MAX;
	return BRB_getTypeRTSize(res);
}

ssize_t BRB_getStackRTSizeDiff(BRB_ModuleBuilder* builder, uint32_t proc_id, uint32_t op_id)
{
	if (proc_id >= builder->procs.length) return SSIZE_MAX;
	if (op_id + 1 >= builder->procs.data[proc_id].length) return SSIZE_MAX;
	// `op_id + 1` because the first element in the state stack is always the initial state, determined by proc args
	return (ssize_t)(BRB_getStackItemRTOffset(builder, proc_id, op_id, SIZE_MAX) - BRB_getStackItemRTOffset(builder, proc_id, op_id - 1, SIZE_MAX));
}

size_t BRB_getProcIdByName(BRB_Module* module, const char* name)
{
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		if (streq(name, proc->name)) return proc - module->seg_exec.data;
	}
	return SIZE_MAX;
}

size_t BRB_getStackItemIdByName(BRB_ModuleBuilder* builder, uint32_t proc_id, uint32_t op_id, const char* name)
{
	if (proc_id >= builder->procs.length) return SIZE_MAX;
	if (op_id >= builder->procs.data[proc_id].length) return SIZE_MAX;
	size_t res = 0;
	for (BRB_StackNode node = builder->procs.data[proc_id].data[op_id]; node; node = node->prev) {
		if (node->name ? streq(name, node->name) : false) return res;
		++res;
	}
	return SIZE_MAX;
}

BRB_Error BRB_preallocExecSegment(BRB_ModuleBuilder* builder, uint32_t n_procs_hint)
{
	if (!(builder->procs = BRB_StackNodeArrayArray_new(-(int64_t)n_procs_hint)).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	if (!(builder->module.seg_exec = BRB_ProcArray_new(-(int64_t)n_procs_hint)).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	return (BRB_Error){0};
}

BRB_Error BRB_preallocDataSegment(BRB_ModuleBuilder* builder, uint32_t n_dbs_hint)
{
	if (!(builder->module.seg_data = BRB_DataBlockArray_new(-(int64_t)n_dbs_hint)).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	return (BRB_Error){0};
}
