#include "brb.h"
#include "stdio.h"
#include "errno.h"
#include "signal.h"

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
	fprintf(fd, "\t-t     Trace data alignment and validate arguments to the operations. Provides precise error tracking and verbose reporting\n");
	fprintf(fd, "\t-m     Output contents of memory blocks after execution of the module\n");
}

int main(int argc, char* argv[]) {
	initBREnv();
	char *input_name = NULL, **module_argv = NULL;
	uint8_t exec_flags = 0;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			for (argv[i]++; *argv[i]; argv[i]++) {
				switch (*argv[i]) {
					case 'h': printUsageMsg(stdout, argv[0]); return 0;
					case 't': exec_flags |= BRBX_TRACING; break;
					case 'm': exec_flags |= BRBX_PRINT_MEMBLOCKS; break;
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
	strArray search_paths = strArray_new(1, ".");
	BRBLoadError err = loadModule(input_fd, &module, search_paths);
	if (err.code) {
		printLoadError(err);
		return 1;
	}

	signal(SIGINT, &handleExecInt);
	ExecEnv res = execModule(&module, exec_flags, module_argv, &interrupt);
	signal(SIGINT, SIG_DFL);

	printExecState(stdout, &res, &module);
	printRuntimeError(stderr, &res, &module);

	return res.exitcode;
}