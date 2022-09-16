#define BRIDGE_IMPLEMENTATION
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

const sbuf BRB_opNames[] = {
	[BRB_OP_NOP] = fromcstr("nop"),
	[BRB_OP_END] = fromcstr("end"),
	[BRB_OP_I8] = fromcstr("i8"),
	[BRB_OP_I16] = fromcstr("i16"),
	[BRB_OP_I32] = fromcstr("i32"),
	[BRB_OP_PTR] = fromcstr("ptr"),
	[BRB_OP_I64] = fromcstr("i64"),
	[BRB_OP_ADDR] = fromcstr("addr"),
	[BRB_OP_DBADDR] = fromcstr("dbaddr"),
	[BRB_OP_LD] = fromcstr("ld"),
	[BRB_OP_STR] = fromcstr("str"),
	[BRB_OP_SYS] = fromcstr("sys"),
	[BRB_OP_BUILTIN] = fromcstr("builtin")
};
static_assert(sizeof(BRB_opNames) / sizeof(BRB_opNames[0]) == BRB_N_OPS, "not all BRB operations have their names defined");

const sbuf BRB_syscallNames[] = {
	[BRB_SYS_EXIT] = fromcstr("exit"),
	[BRB_SYS_WRITE] = fromcstr("write"),
	[BRB_SYS_READ] = fromcstr("read")
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
	if (field.is_any) return entry.kind != BRB_TYPE_VOID;
	if (field.is_any_int) return entry.kind != BRB_TYPE_VOID && entry.n_items == 1;
	return entry.kind == field.kind && entry.n_items == field.n_items;
}

#define BRB_ANY_TYPE ((BRB_Type){.is_any = true})
#define BRB_INT_TYPE ((BRB_Type){.is_any_int = true})
static BRB_Error changeStack(BRB_StackNodeArray* stack, BRB_OpType op_type, size_t n_in, BRB_Type* in_types, size_t n_out, BRB_Type* out_types, Arena* allocator)
{
	BRB_StackNode iter = *arrayhead(*stack);
	if (getStackLength(iter) < n_in)
		return (BRB_Error){
			.type = BRB_ERR_STACK_UNDERFLOW,
			.opcode = op_type,
			.expected_stack_length = n_in,
			.actual_stack_length = getStackLength(iter)
		};
	arrayForeach (BRB_Type, type, ((BRB_TypeArray){.data = in_types, .length = n_in})) {
		if (iter->flags & BRB_SNF_STACKFRAME)
			return (BRB_Error){.type = BRB_ERR_DEL_ARGS};
		if (!compareTypes(*type, iter->type))
			return (BRB_Error){
				.type = BRB_ERR_TYPE_MISMATCH,
				.opcode = op_type,
				.expected_type = *type,
				.actual_type = iter->type,
				.arg_id = type - in_types
			};
		iter = iter->prev;
	}

	BRB_StackNode new = getNthStackNode(*arrayhead(*stack), n_in), prev;
	arrayRevForeach (BRB_Type, type, ((BRB_TypeArray){.data = out_types, .length = n_out})) {
		prev = new;
		if (!(new = arena_alloc(allocator, sizeof(struct BRB_stacknode_t)))) return (BRB_Error){.type = BRB_ERR_NO_MEMORY};
		*new = (struct BRB_stacknode_t){0};
		new->prev = prev;
		new->type = *type;
	}
	return (BRB_Error){.type = BRB_StackNodeArray_append(stack, new) ? 0 : BRB_ERR_NO_MEMORY};
}

static inline BRB_Error addStackItem(BRB_StackNodeArray* stack, BRB_OpType op_type, BRB_Type item_type, Arena* allocator)
{
	return changeStack(stack, op_type, 0, NULL, 1, &item_type, allocator);
}

static inline BRB_Error preserveStack(BRB_StackNodeArray* stack, BRB_OpType op_type, Arena* allocator)
{
	return changeStack(stack, op_type, 0, NULL, 0, NULL, allocator);
}

static inline BRB_Error replaceStackHead(BRB_StackNodeArray* stack, BRB_OpType op_type, BRB_Type new_head_type, Arena* allocator)
{
	static BRB_Type arg = BRB_ANY_TYPE;
	return changeStack(stack, op_type, 1, &arg, 1, &new_head_type, allocator);
}

static BRB_Error clearStack(BRB_StackNodeArray* stack, BRB_OpType op_type)
{
	return (BRB_Error){.type = BRB_StackNodeArray_append(stack, NULL) ? 0 : BRB_ERR_NO_MEMORY};
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
	if (!BRB_ProcArray_append(&builder->module.seg_exec, (BRB_Proc){.name = name, .ret_type = ret_type})
		|| !BRB_StackNodeArrayArray_append(&builder->procs, (BRB_StackNodeArray){0}))
		return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});

	if (!(arrayhead(builder->module.seg_exec)->args = BRB_TypeArray_copy((BRB_TypeArray){
		.data = args,
		.length = n_args
	})).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});

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
	if (builder->error.type) return builder->error;
	if (!BRB_OpArray_append(&builder->module.seg_exec.data[proc_id].body, op))
		return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	switch (op.type) {
		case BRB_OP_NOP:
			if ((builder->error = preserveStack(&builder->procs.data[proc_id], BRB_OP_NOP, &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
		case BRB_OP_END:
			if ((builder->error = clearStack(&builder->procs.data[proc_id], BRB_OP_END)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_I8:
			if ((builder->error = addStackItem(&builder->procs.data[proc_id], BRB_OP_I8, BRB_I8_TYPE(1), &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_I16:
			if ((builder->error = addStackItem(&builder->procs.data[proc_id], BRB_OP_I16, BRB_I16_TYPE(1), &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_I32:
			if ((builder->error = addStackItem(&builder->procs.data[proc_id], BRB_OP_I32, BRB_I32_TYPE(1), &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_PTR:
			if ((builder->error = addStackItem(&builder->procs.data[proc_id], BRB_OP_PTR, BRB_PTR_TYPE(1), &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_I64:
			if ((builder->error = addStackItem(&builder->procs.data[proc_id], BRB_OP_I64, BRB_I64_TYPE(1), &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_ADDR:
			if (op.operand_u >= UINT32_MAX) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_TOO_LARGE, .opcode = BRB_OP_ADDR});
			}
			if (op.operand_u ? !getNthStackNode_nonInternal(*arrayhead(builder->procs.data[proc_id]), op.operand_u) : false) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = BRB_OP_ADDR, .operand = op.operand_u});
			}
			if ((builder->error = addStackItem(&builder->procs.data[proc_id], BRB_OP_ADDR, BRB_PTR_TYPE(1), &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_DBADDR:
			if (op.operand_u >= UINT32_MAX) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_TOO_LARGE, .opcode = BRB_OP_DBADDR});
			}
			if (op.operand_u >= builder->module.seg_data.length) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = BRB_OP_DBADDR, .operand = op.operand_u});
			}
			if ((builder->error = addStackItem(&builder->procs.data[proc_id], BRB_OP_DBADDR, BRB_PTR_TYPE(1), &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_LD:
			if ((builder->error = replaceStackHead(&builder->procs.data[proc_id], BRB_OP_LD, op.operand_type, &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_OP_STR: {
			static BRB_Type op_args[2] = { BRB_PTR_TYPE(1), BRB_ANY_TYPE };
			if ((builder->error = changeStack(&builder->procs.data[proc_id], BRB_OP_STR, 2, op_args, 0, NULL, &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
		} break;
// TODO: make the algorithm distinguish between empty and unreachable stack states, to allow for unreachable code removal
		case BRB_OP_SYS: {
			if (op.operand_u >= BRB_N_SYSCALLS) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){ .type = BRB_ERR_INVALID_SYSCALL, .syscall_id = op.operand_u });
			}
			BRB_Type op_args[BRB_syscallNArgs[op.operand_u]];
			arrayForeach (BRB_Type, arg, ((BRB_TypeArray){.data = op_args, .length = BRB_syscallNArgs[op.operand_u]})) {
				*arg = BRB_PTR_TYPE(1);
			}
			static BRB_Type op_retval = BRB_PTR_TYPE(1);
			if ((builder->error = (op.operand_u == BRB_SYS_EXIT
				? clearStack(&builder->procs.data[proc_id], BRB_OP_SYS)
				: changeStack(&builder->procs.data[proc_id], BRB_OP_SYS, BRB_syscallNArgs[op.operand_u], op_args, 1, &op_retval, &builder->arena))
			).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
		} break;
		case BRB_OP_BUILTIN:
			if (op.operand_u >= BRB_N_BUILTINS) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return (builder->error = (BRB_Error){ .type = BRB_ERR_INVALID_BUILTIN, .builtin_id = op.operand_u });
			}
			if ((builder->error = addStackItem(&builder->procs.data[proc_id], BRB_OP_BUILTIN, BRB_PTR_TYPE(1), &builder->arena)).type) {
				--builder->module.seg_exec.data[proc_id].body.length;
				return builder->error;
			}
			break;
		case BRB_N_OPS:
		default:
			return (builder->error = (BRB_Error){.type = BRB_ERR_INVALID_OPCODE, .opcode = op.type});
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
	for (BRB_StackNode node = builder->procs.data[proc_id].data[op_id]; node && item_id--; node = node->prev) {
		res += BRB_getTypeRTSize(node->type);
		if (node->flags & BRB_SNF_STACKFRAME) ++item_id;
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
