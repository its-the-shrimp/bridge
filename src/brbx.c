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
	fprintf(fd, "usage: %s [-chmrs] <file> [program-args...]\n", exec_name);
	fprintf(fd, "options:\n");
	fprintf(fd, "\t-h     Output this message and exit\n");
	fprintf(fd, "\t-t     Trace data alignment and validate arguments to the operations. Provides precise error tracking and verbose reporting\n");
	fprintf(fd, "\t-m     Output contents of memory blocks after execution of the program\n");
}

int main(int argc, char* argv[]) {
	initBREnv();
	char *input_name = NULL, **program_argv = NULL;
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
			program_argv = argv + i;
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

	Program program;
	BRBLoadError err = loadProgram(input_fd, &program, GLOBAL_CTX);
	if (err.code) {
		printLoadError(err);
		return 1;
	}

	signal(SIGINT, &handleExecInt);
	ExecEnv res = execProgram(&program, exec_flags, program_argv, &interrupt);
	signal(SIGINT, SIG_DFL);

	printExecState(stdout, &res, &program);
	printRuntimeError(stderr, &res, &program);

	return res.exitcode;
}