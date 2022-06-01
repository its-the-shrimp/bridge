#include <brb.h>
#include <errno.h>

void printUsageMsg(FILE* fd, char* execname)
{
	fprintf(
		fd, 
		"brs - Compiles `.vbrb` (Visual BRidge Bytecode) to interpetable `.brb` files (BRidge Bytecode)\n"
		"usage: %s [options] <source path>\n"
		"options:\n"
		"\t-h           Output this message and exit\n"
		"\t-o <path>    The output will be saved to <path>;\n"
		"\t\tif <path> is a directory, the output will be at <path>/<source name>.brb;\n"
		"\t\tthe default output path is <source dir>/<source name>.brb\n",
		execname
	);
}

int main(int argc, char* argv[])
{
	startTimer();

	char *input_path = NULL, *output_path = NULL;
	for (int i = 1; i < argc; i++) {
		bool go_on = false;
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

	if (output_path) {
		if (isPathDir(output_path)) {
			output_path = tostr(SBUF(output_path), PATHSEP, fileBaseName_s(SBUF(input_path)), SBUF(BRB_EXT));
		}
	} else {
		output_path = setFileExt(input_path, BRB_EXT);
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
		eprintf("error: could not open file `%s` for writing BRB output (reason: %s)\n", output_path, strerror(errno)); 
		return 1;
	}
	writeModule(&res, output_fd);
	fclose(output_fd);

	printf("%s -> %s in %.3f ms\n", input_path, output_path, endTimer());
}
