#include <brb.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>

bool interrupt = false;

void handleExecInt(int sig)
{
	interrupt = true;
}

void printUsageMsg(FILE* fd, char* exec_name)
{
	fprintf(fd, "brbx - Execute and debug .brb (BRidge Executable) files\n");
	fprintf(fd, "usage: %s [-chmrs] <file> [module-args...]\n", exec_name);
	fprintf(fd, "options:\n");
	fprintf(fd, "\t-h     Output this message and exit\n");
}

int main(int argc, char* argv[]) {
	char *input_name = NULL, **module_argv = NULL;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			for (argv[i]++; *argv[i]; argv[i]++) {
				switch (*argv[i]) {
					case 'h': printUsageMsg(stdout, argv[0]); return 0;
					default: eprintf("error: unknown option `-%c`\n", *argv[i]); return 1;
				}
			}
		} else {
			input_name = argv[i];
			module_argv = argv + i;
			break;
		}
	}
	if (!input_name) {
		fprintf(stderr, "error: no input file provided\n");
		return 1;
	}

	FILE* input_fd = fopen(input_name, "rb");
	if (!input_fd) {
		fprintf(stderr, "error: could not open file `%s` (reason: %s)\n", input_name, strerror(errno));
		return 1;
	}

	Module module;
	char* module_search_paths[] = { ".", NULL };
	BRBLoadError err = loadModule(input_fd, &module, module_search_paths);
	if (err.code) {
		printLoadError(stderr, err);
		return 1;
	}

	signal(SIGINT, &handleExecInt);
	ExecEnv env;
	initExecEnv(&env, &module, module_argv);
	startTimer();
	execModule(&env, &module, &interrupt);
	printf("executed in %.3f ms\n", endTimer());
	signal(SIGINT, SIG_DFL);

	return env.exitcode;
}
