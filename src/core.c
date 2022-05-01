#define BRIDGE_IMPLEMENTATION
#include "br.h"
#include "unistd.h"
#include "sys/param.h"
#include "math.h"
#include "time.h"
#include "errno.h"
#include "fcntl.h"
#include "spawn.h"
#include "sys/stat.h"
#include "sys/wait.h"
extern char** environ;

defArray(sbuf);
defArray(InputCtx);
defQueue(Token);

bool IS_BIG_ENDIAN, IS_LITTLE_ENDIAN;

void initBREnv(void)
{
	int _e = 0xDEADBEEF;
	IS_BIG_ENDIAN = *(char*)&_e == 0xDE;
	IS_LITTLE_ENDIAN = !IS_BIG_ENDIAN;
}

char* getAbsolutePath(char* src)
{
	sbuf input = fromstr(src);
	if (!sbufstartswith(input, PATHSEP)) {
		char resbuf[MAXPATHLEN];
		getwd(resbuf);
		input = sbufconcat(fromstr(resbuf), PATHSEP, input);
	}

	sbufArray components = sbufArray_new(sbufcount(input, PATHSEP) * -1 - 1);
	sbuf new;
	
	while (input.length) {
		sbufsplit(&input, &new, PATHSEP);
		if (sbufeq(new, fromcstr(".."))) {
			sbufArray_pop(&components, -1);
		} else if (new.length && !sbufeq(new, fromcstr("."))) {
			sbufArray_append(&components, new);
		}
	}
	sbuf res = smalloc(input.length + 1);
	memset(res.data, 0, res.length);
	if (!res.data) {
		sbufArray_clear(&components);
		return NULL;
	}
	res.length = 0;
	array_foreach(sbuf, component, components,
		memcpy(res.data + res.length, PATHSEP.data, PATHSEP.length);
		res.length += PATHSEP.length;
		memcpy(res.data + res.length, component.data, component.length);
		res.length += component.length;
	);
	free(components.data);
	return res.data;
}

void* BRByteOrder(void* src, long length) {
	char* _src = src;
	if (IS_LITTLE_ENDIAN) {
		for (long i = 0; i < length / 2; i++) {
			char tmp = _src[i];
			_src[i] = _src[length - i - 1];
			_src[length - i - 1] = tmp;
		}
	}
	return src;
}

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

char* setFileExt(char* path, char* ext)
{
	sbuf src = fromstr(path);
	sbuf noext;
	sbufsplit(&src, &noext, fromcstr("."));
	return tostr(noext, fromstr(ext));
}

sbuf setFileExt_s(sbuf path, sbuf ext)
{
	sbuf noext;
	sbufsplit(&path, &noext, fromcstr("."));
	return sbufconcat(noext, ext);
}

char* fileBaseName(char* path)
{
	sbuf src = fromstr(path);
	sbuf res;
	sbufsplit(&src, &res, fromcstr("."));
	for (char* ptr = res.data + res.length - 1; ptr >= res.data; ptr--) {
		if (*ptr == '/') {
			res.data = ptr;
			break;
		}
	}
	return tostr(res);
}

sbuf fileBaseName_s(sbuf path)
{
	sbuf res;
	sbufsplit(&path, &res, fromcstr("."));
	for (char* ptr = res.data + res.length - 1; ptr >= res.data; ptr--) {
		if (*ptr == '/') {
			res.data = ptr;
			break;
		}
	}
	return res;
}

bool fpipe(FILE** readable_end_ptr, FILE** writable_end_ptr)
{
	int fds[2];
	if (pipe(fds) != 0) return false;

	*readable_end_ptr = fdopen(fds[0], "r");
	*writable_end_ptr = fdopen(fds[1], "w");
	
	return true;
}