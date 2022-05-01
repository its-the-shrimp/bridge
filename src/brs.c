#include "brb.h"
#include "errno.h"

void printUsageMsg(FILE* fd, char* execname)
{
	fprintf(fd, "brs - Compiles `.vbrb` (Visual BRidge Bytecode) to interpetable `.brb` files (BRidge Bytecode)");
	fprintf(fd, "usage: %s [options] <file>\n", execname);
	fprintf(fd, "options:\n");
	fprintf(fd, "\t-h           Output this message and exit\n");
	fprintf(fd, "\t-o <file>    The output will be saved to <file>\n");
	fprintf(fd, "\t-n <file>    Compile source directly to a native executable, which will be saved to <file>\n");
	fprintf(fd, "\t-N           The same as `-n` option, but the resulting executable will have the same name as the input file\n");
}

int main(int argc, char* argv[])
{
	initBREnv();
	startTimer();

	bool go_on = false;
	char *input_path = NULL, *output_path = NULL, *exec_output_path = NULL;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			for (argv[i]++; *argv[i]; argv[i]++) {
				if (go_on) { go_on = false; break; }
				switch (*argv[i]) {
					case 'h': printUsageMsg(stdout, argv[0]); return 0;
					case 'o':
						if (!argv[++i]) {
							eprintf("error: `-o` option specified but no output file path provided\n");
							return 1;
						}
						output_path = argv[i];
						go_on = true;
						break;
					case 'n':
						if (!argv[++i]) {
							eprintf("error: `-n` option specified but no executable output file path provided\n");
							return 1;
						}
						exec_output_path = argv[i];
						go_on = true;
						break;
					case 'N': exec_output_path = (void*)1; break;
					default: eprintf("error: unknown option `-%c`\n", *argv[i]); return 1;
				}
			}
		} else {
			if (input_path) {
				eprintf("error: got more than one input paths\n");
			}
			input_path = argv[i];
		}
	}

	if (!input_path) {
		fprintf(stderr, "error: no input path provided\n");
		return 1;
	}

	sbuf input_path_sbuf = fromstr(input_path), basename = {0};
	if (!output_path) {
		sbufsplit(&input_path_sbuf, &basename, fromcstr("."));
		output_path = tostr(basename, fromcstr(".brb"));
	}
	if (exec_output_path == (void*)1) {
		exec_output_path = tostr(basename);
	}

	FILE* input_fd = fopen(input_path, "r");
	if (!input_fd) {
		eprintf("error: could not open file `%s` (reason: %s)\n", input_path, strerror(errno));
		return 1;
	}
	
	Module res;
	char* module_search_paths[] = { ".", NULL };
	VBRBError err = compileModule(input_fd, input_path, &res, module_search_paths, 0);
	if (err.code) {
		printVBRBError(stderr, err);
		return 1;
	}
	cleanupVBRBCompiler(err);

	FILE* output_fd = fopen(output_path, "wb");
	if (!output_fd) {
		eprintf("error: could not open/create file `%s` (reason: %s)\n", output_path, strerror(errno)); 
		return 1;
	}
	writeModule(&res, output_fd);
	fclose(output_fd);

	printf("%s -> %s in %.3f ms\n", input_path, output_path, endTimer());
	if (!exec_output_path) return 0;

	char cmd[1024];
	ProcessInfo proc_res = { .out = stdout };
	snprintf(cmd, sizeof(cmd), "brbc -o %s %s", exec_output_path, output_path);

	if (!execProcess(cmd, &proc_res)) {
		eprintf("error: could start the bytecode compiler (reason: %s)\n", strerror(errno));
		return 1;
	} else if (proc_res.exitcode) {
		eprintf("error: bytecode compiler exited with code %hhu\n", proc_res.exitcode);
		sbuf err_output = filecontent(proc_res.err);
		err_output.length--; // removing the newline from the output
		eprintf("bytecode compiler output:\n\t\""sbuf_format"\"\n", unpack(err_output));
		return 1;
	}
}
