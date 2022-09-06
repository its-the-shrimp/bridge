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
extern char** environ;

defArray(BRB_Op);
defArray(BRB_DataBlock);
defArray(BRB_DataPiece);
defArray(BRB_StackNode);

const sbuf BRB_syscallNames[] = { _BRB_syscallNames };
const uintptr_t BRB_builtinValues[] = {
	[BRB_BUILTIN_NULL] = (uintptr_t)NULL,
	[BRB_BUILTIN_STDIN] = STDIN_FILENO,
	[BRB_BUILTIN_STDOUT] = STDOUT_FILENO,
	[BRB_BUILTIN_STDERR] = STDERR_FILENO,
};
const sbuf BRB_builtinNames[] = {
	[BRB_BUILTIN_NULL] = fromcstr("NULL"),
	[BRB_BUILTIN_STDIN] = fromcstr("STDIN"),
	[BRB_BUILTIN_STDOUT] = fromcstr("STDOUT"),
	[BRB_BUILTIN_STDERR] = fromcstr("STDERR"),
};
const size_t BRB_syscallNArgs[] = {
	[BRB_SYS_EXIT] = 1, // [code]
	[BRB_SYS_WRITE] = 3, // [fd, addr, n_bytes]
	[BRB_SYS_READ] = 3, // [fd, addr, n_bytes]
};
	

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

void BRB_printErrorMsg(FILE* dst, BRB_Error err)
{
	if (err.type == BRB_ERR_OK) return;
	switch (err.type) {
		case BRB_ERR_INVALID_DB:
			fputs("invalid data block structure\n", dst);
			break;
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
			fputs("unexpected end-of-input while loading exec segment; no opcode\n", dst);
			break;
		case BRB_ERR_INVALID_OPCODE:
			fprintf(dst, "invalid opcode: %u\n", err.opcode);
			break;
		case BRB_ERR_NO_OPERAND:
			fprintf(dst, "unexpected end-of-input while loading exec segment; no operand for operation `%.*s`\n", unpack(BRB_opNames[err.opcode]));
			break;
		case BRB_ERR_INVALID_NAME:
			fputs("invalid symbol name found in the `name` segment\n", dst);
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
			fprintf(dst, "operand %llu is out of range for operation `%.*s`\n", err.operand, unpack(BRB_opNames[err.opcode]));
			break;
		case BRB_ERR_OK:
		case BRB_N_ERROR_TYPES:
		default:
			fputs("undefined\n", dst);
	}
}

char* BRB_getErrorMsg(FILE* dst, BRB_Error err) {
	sbuf res = {0};
	FILE* stream = open_memstream(&res.data, &res.length);
	BRB_printErrorMsg(stream, err);
	fclose(stream);
	return res.data;
}

BRB_Error BRB_initModuleBuilder(BRB_ModuleBuilder* builder)
{
	*builder = (BRB_ModuleBuilder){0};
	BRB_StackNodeArray_append(&builder->stack_traces, NULL);
	return (BRB_Error){0};
}

BRB_Error BRB_extractModule(BRB_ModuleBuilder builder, BRB_Module* dst)
{
	if (builder.error.type) return builder.error;
	// TODO: improve memory reusability by adding arena allocators in various places
	BRB_StackNodeArray_clear(&builder.stack_traces);
	*dst = builder.module;
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

static size_t getStackLength(BRB_StackNode head)
{
	size_t res = 0;
	while (head) {
		head = head->prev;
		++res;
	}
	return res;
}

static bool addStackItem(BRB_StackNodeArray* stack, size_t size)
{
	BRB_StackNode new = malloc(sizeof(struct BRB_stacknode_t));
	if (!new) return false;
	new->prev = *arrayhead(*stack);
	new->size = size;
	new->name = NULL;
	return BRB_StackNodeArray_append(stack, new) != NULL;
}

static bool replaceStackHead(BRB_StackNodeArray* stack, size_t size)
{
	BRB_StackNode new = malloc(sizeof(struct BRB_stacknode_t));
	if (!new) return false;
	new->prev = (*arrayhead(*stack))->prev;
	new->size = size;
	new->name = NULL;
	return BRB_StackNodeArray_append(stack, new) != NULL;
}

static bool preserveStack(BRB_StackNodeArray* stack)
{
	return BRB_StackNodeArray_append(stack, *arrayhead(*stack)) != NULL;
}

static bool removeStackItems(BRB_StackNodeArray* stack, size_t n_items)
{
	return BRB_StackNodeArray_append(stack, getNthStackNode(*arrayhead(*stack), n_items)) != NULL;
}

static bool clearStack(BRB_StackNodeArray* stack)
{
	return BRB_StackNodeArray_append(stack, NULL);
}

static bool exchangeStackItems(BRB_StackNodeArray* stack, size_t n_items_in, size_t out_size) // to simulate a syscall or a procedure call
{
	BRB_StackNode new = malloc(sizeof(struct BRB_stacknode_t));
	if (!new) return false;
	new->prev = getNthStackNode(*arrayhead(*stack), n_items_in);
	new->size = out_size;
	new->name = NULL;
	return BRB_StackNodeArray_append(stack, new) != NULL;
}

fieldArray BRB_getNameFields(BRB_Module* module)
{
	fieldArray res = {0};
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		fieldArray_append(&res, &block->name);
	}

	return res;
}

BRB_Error BRB_addOp(BRB_ModuleBuilder* builder, BRB_Op op)
{
	if (builder->error.type) return builder->error;
	switch (op.type) {
		case BRB_OP_NOP:
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& preserveStack(&builder->stack_traces)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_END:
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& clearStack(&builder->stack_traces)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_INT8:
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& addStackItem(&builder->stack_traces, 1)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_INT16:
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& addStackItem(&builder->stack_traces, 2)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_INT32:
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& addStackItem(&builder->stack_traces, 4)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_INTPTR:
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& addStackItem(&builder->stack_traces, BRB_TYPE_PTR)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_INT64:
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& addStackItem(&builder->stack_traces, 8)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_ADDR:
			if (op.operand_u >= UINT32_MAX) return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_TOO_LARGE, .opcode = BRB_OP_ADDR});
			if (op.operand_u ? !getNthStackNode(*arrayhead(builder->stack_traces), op.operand_u) : false)
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = BRB_OP_ADDR, .operand = op.operand_u});
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& addStackItem(&builder->stack_traces, BRB_TYPE_PTR)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_DBADDR:
			if (op.operand_u >= UINT32_MAX) return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_TOO_LARGE, .opcode = BRB_OP_DBADDR});
			if (op.operand_u >= builder->module.seg_data.length)
				return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = BRB_OP_DBADDR, .operand = op.operand_u});
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& addStackItem(&builder->stack_traces, BRB_TYPE_PTR)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_LOAD:
			if (getStackLength(*arrayhead(builder->stack_traces)) < 1) return (builder->error = (BRB_Error){
				.type = BRB_ERR_STACK_UNDERFLOW,
				.opcode = BRB_OP_LOAD,
				.expected_stack_length = 1,
				.actual_stack_length = getStackLength(*arrayhead(builder->stack_traces))
			});
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& replaceStackHead(&builder->stack_traces, op.operand_u)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_STR:
			if (getStackLength(*arrayhead(builder->stack_traces)) < 2) return (builder->error = (BRB_Error){
				.type = BRB_ERR_STACK_UNDERFLOW,
				.opcode = BRB_OP_STR,
				.expected_stack_length = 2,
				.actual_stack_length = getStackLength(*arrayhead(builder->stack_traces))
			});
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
					&& removeStackItems(&builder->stack_traces, 2)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_SYS:
			if (op.operand_u >= BRB_N_SYSCALLS) return (builder->error = (BRB_Error){ .type = BRB_ERR_INVALID_SYSCALL, .syscall_id = op.operand_u });
			if (getStackLength(*arrayhead(builder->stack_traces)) < BRB_syscallNArgs[op.operand_u]) return (builder->error = (BRB_Error){
				.type = BRB_ERR_STACK_UNDERFLOW,
				.opcode = BRB_OP_SYS,
				.expected_stack_length = BRB_syscallNArgs[op.operand_u],
				.actual_stack_length = getStackLength(*arrayhead(builder->stack_traces))  
			});
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, op)
// TODO: make the algorithm distinguish between empty and unreachable stack states, to allow for unreachable code removal
					&& (op.operand_u == BRB_SYS_EXIT
						? clearStack(&builder->stack_traces)
						: exchangeStackItems(&builder->stack_traces, BRB_syscallNArgs[op.operand_u], BRB_TYPE_PTR))
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_OP_BUILTIN:
			if (op.operand_u >= BRB_N_BUILTINS) return (builder->error = (BRB_Error){ .type = BRB_ERR_INVALID_BUILTIN, .builtin_id = op.operand_u });
			return (builder->error = (BRB_Error){
				.type = BRB_OpArray_append(&builder->module.seg_exec, (BRB_Op){.type = BRB_OP_BUILTIN, .operand_u = op.operand_u})
					&& addStackItem(&builder->stack_traces, BRB_TYPE_PTR)
					? 0 : BRB_ERR_NO_MEMORY
			});
		case BRB_N_OPS:
		default:
			return (builder->error = (BRB_Error){.type = BRB_ERR_INVALID_OPCODE, .opcode = op.type});
	}
}

BRB_Error BRB_labelStackHead(BRB_ModuleBuilder* builder, const char* name)
{
	if (builder->error.type) return builder->error;
	if (!*arrayhead(builder->stack_traces)) return (builder->error = (BRB_Error){
		.type = BRB_ERR_STACK_UNDERFLOW,	
		.opcode = BRB_N_OPS,
		.expected_stack_length = 1,
		.actual_stack_length = 0
	});
	(*arrayhead(builder->stack_traces))->name = name;
	return (BRB_Error){0};
}

BRB_Error BRB_addDataBlock(BRB_ModuleBuilder* builder, uint32_t* db_id_p, const char* name, bool is_mutable)
{
	if (builder->error.type) return builder->error;
	if (!BRB_DataBlockArray_append(&builder->module.seg_data, (BRB_DataBlock){
		.name = name,
		.is_mutable = is_mutable
	})) return (builder->error = (BRB_Error){ .type = BRB_ERR_NO_MEMORY });
	*db_id_p = builder->module.seg_data.length - 1;
	return (BRB_Error){0};
}

BRB_Error BRB_addDataPiece(BRB_ModuleBuilder* builder, uint32_t db_id, BRB_DataPiece piece)
{
	if (builder->error.type) return builder->error;
	if (piece.type == BRB_DP_BUILTIN && piece.builtin_id >= BRB_N_BUILTINS)
		return (builder->error = (BRB_Error){.type = BRB_ERR_INVALID_BUILTIN}); 
	return (builder->error = (BRB_Error){
		.type = BRB_DataPieceArray_append(&builder->module.seg_data.data[db_id].pieces, piece)
			? 0 : BRB_ERR_NO_MEMORY
	});
}

size_t BRB_getDataBlockId(BRB_Module* module, const char* name)
{
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		if (streq(block->name, name)) return block - module->seg_data.data;
	}
	return SIZE_MAX;
} 

size_t BRB_getStackItemRTOffset(BRB_ModuleBuilder* builder, uint32_t op_id, uint32_t var_id)
{
	if (++op_id >= builder->stack_traces.length) return SIZE_MAX; // `++op_id` because `builder.stack_traces` always has an additional empty stack as the first state
	size_t res = 0;
	for (BRB_StackNode node = builder->stack_traces.data[op_id]; node && var_id--; node = node->prev) {
		res += node->size == BRB_TYPE_PTR ? sizeof(void*) : node->size;
	}
	return var_id ? SIZE_MAX : res;
}

size_t BRB_getStackItemSize(BRB_ModuleBuilder* builder, uint32_t op_id, uint32_t var_id)
{
	if (++op_id >= builder->stack_traces.length) return SIZE_MAX; // because the first element in `builder.stack_traces` is always an additional empty stack
	BRB_StackNode node = builder->stack_traces.data[op_id];
	while (node && var_id--) node = node->prev;
	return node ? node->size : SIZE_MAX;
}

size_t BRB_getStackItemRTSize(BRB_ModuleBuilder* builder, uint32_t op_id, uint32_t var_id)
{
	size_t res = BRB_getStackItemSize(builder, op_id, var_id);
	return res == BRB_TYPE_PTR ? sizeof(void*) : res;
}
