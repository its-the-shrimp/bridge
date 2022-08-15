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

defArray(Op);
defArray(Submodule);
defArray(DataBlock);

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
	} else {
		return tostr(src.length ? src : res);
	}
}

sbuf fileBaseName_s(sbuf path)
{
	sbuf res;
	if (sbufeq(sbufsplitr(&path, &res, fromcstr("."), PATHSEP), fromcstr("."))) {
		return sbufsplitr(&res, &path, PATHSEP).length ? res : path;
	} else {
		return path.length ? path : res;
	}
}

FILE* findModule(const char* name, const char* search_paths[])
{
	for (uint64_t i = 0; search_paths[i]; i += 1) {
		char path[256];
		snprintf(path, sizeof(path), "%s/%s.brb", search_paths[i], name);

		FILE* module_fd = fopen(path, "r");
		if (module_fd) return module_fd;
		errno = 0;
	}
	
	return NULL;
}

Submodule* getOpSubmodule(Module* module, Op* op)
{
	uint64_t index = op - module->seg_exec.data;
	
	arrayForeach (Submodule, submodule, module->submodules) {
		if (inRange(index, submodule->es_offset, submodule->es_offset + submodule->es_length)) 
			return submodule;
	}
	assert(false, "`op` does not belong to `module`");
	return NULL;
}

Submodule* getDataBlockSubmodule(Module* module, DataBlock* block)
{
	uint64_t index = block - module->seg_data.data;
	
	arrayForeach (Submodule, submodule, module->submodules) {
		if (inRange(index, submodule->ds_offset, submodule->ds_offset + submodule->ds_length)) 
			return submodule;
	}
	assert(false, "`block` does not belong to `module`");
	return NULL;
}

Submodule getRootSubmodule(Module* module, const char* name)
{
	if (module->submodules.length) {
		Submodule* last = arrayhead(module->submodules);
		if (strcmp(last->name, ".") == 0) {
			Submodule res = *last;
			res.name = name;
			return res;
		};
		return (Submodule){
			.ds_offset = last->ds_offset + last->ds_length,
			.ds_length = module->seg_data.length - last->ds_offset - last->ds_length,
			.es_offset = last->es_offset + last->es_length,
			.es_length = module->seg_exec.length - last->es_offset - last->es_length,
			.name = name,
			.direct = true
		};
	} else return (Submodule){
		.ds_offset = 0,
		.ds_length = module->seg_data.length,
		.es_offset = 0,
		.es_length = module->seg_exec.length,
		.name = name,
		.direct = true
	};
}

Module* mergeModule(Module* restrict src, Module* dst, char* src_name)
{
	SubmoduleArray_append(&src->submodules, getRootSubmodule(src, src_name));
	arrayForeach (Submodule, submodule, src->submodules) {
		submodule->ds_offset += dst->seg_data.length;
		submodule->es_offset += dst->seg_exec.length;
		if ((uint64_t)(submodule - src->submodules.data) != src->submodules.length - 1)
			submodule->direct = false;
	}

	arrayForeach (Op, op, src->seg_exec) {
		if (op_flags[op->type] & OPF_USES_MODULE_ID)
			op->module_id += dst->submodules.length;
	}

	DataBlockArray_extend(&dst->seg_data, src->seg_data);
	OpArray_extend(&dst->seg_exec, src->seg_exec);
	SubmoduleArray_extend(&dst->submodules, src->submodules);

	DataBlockArray_clear(&src->seg_data);
	OpArray_clear(&src->seg_exec);
	SubmoduleArray_clear(&src->submodules);

	return dst;
}
