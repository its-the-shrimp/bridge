#include <brb.h>

sbuf ASM_EXT = CSBUF(".S");
sbuf EXEC_EXT = CSBUF("");
sbuf OBJ_EXT = CSBUF(".o");

#define DEFAULT_ENTRY_NAME ".entry"

void printUsageMsg(FILE* fd, char* execname)
{
	fprintf(
		fd,
		"brbc - compile BRidge bytecode `.brb` files to native executable files.\n"
		"usage: %s [options] <source path>\n"
		"options:\n"
		"\t-h                     Output this message and exit\n"
		"\t--asm-output <path>    Output native assembly code to <path>;\n"
		"\t\tif <path> is a directory, output will be at <path>/<source name>.S;\n"
		"\t\tby default, native assembly is stored in a temporary file and deleted after compilation\n"
		"\t--obj-output <path>    Output native code object to <path>;\n"
		"\t\tif <path> is a directory, output will be at <path>/<source name>.o;\n"
		"\t\tby default, native code objects are stored in a temporary file and deleted after compilation\n"
		"\t-o <path>              Output native executable to <path>;\n"
		"\t\tif <path> is a directory, output will be at <path>/<source name>;\n"
		"\t\tthe default output path is <source dir>/<source name>\n",
		execname
	);
}

int main(int argc, char* argv[])
{
	startTimer();

	char *input_path = NULL, *exec_output_path = NULL, *asm_output_path = NULL, *obj_output_path = NULL;
	for (int i = 1; i < argc; i++) {
		bool go_on = false;
		if (argv[i][0] == '-') {
			for (argv[i]++; *argv[i]; argv[i]++) {
				if (go_on) { go_on = false; break; }
				switch (*argv[i]) {
					case 'h': printUsageMsg(stdout, argv[0]); return 0;
					case 'o':
						if (!argv[++i]) {
							eprintf("error: `-o` option specified but no executable output file path provided\n");
							return 1;
						}
						exec_output_path = argv[i];
						go_on = true;
						break;
					case '-':
						argv[i]++;
						if (sbufeq(argv[i], "asm-output")) {
							if (!argv[++i]) {
								eprintf("error: `--asm-output` option specified but no path is provided\n");
								return 1;
							}
							asm_output_path = argv[i];
							go_on = true;
						} else if (sbufeq(argv[i], "obj-output")) {
							if (!argv[++i]) {
								eprintf("error: `--obj-output` option specified but no path is provided\n");
								return 1;
							}
							obj_output_path = argv[i];
							go_on = true;
						} else {
							eprintf("error: unknown option `--%s`\n", argv[i]);
							return 1;
						}
						break;
					default: eprintf("error: unknown option `-%c`\n", *argv[i]); return 1;
				}
			}
		} else {
			if (input_path) {
				eprintf("error: got more than one input paths (`%s` and `%s`)\n", argv[i], input_path);
			}
			input_path = argv[i];
		}
	}

	if (!input_path) {
		eprintf("error: no input file provided\n");
		return 1;
	}

	sbuf input_path_sbuf = SBUF(input_path), basepath = {0};
	sbufsplitr(&input_path_sbuf, &basepath, CSBUF("."));
	sbuf basename = fileBaseName_s(basepath);

	if (exec_output_path) {
		if (isPathDir(exec_output_path)) {
			exec_output_path = tostr(SBUF(exec_output_path), PATHSEP, basename);
		}
	} else {
		exec_output_path = tostr(basepath);
	}

	if (asm_output_path) {
		if (isPathDir(asm_output_path)) {
			asm_output_path = tostr(SBUF(asm_output_path), PATHSEP, basename, ASM_EXT);
		}
	} else {
		asm_output_path = mktemp(tostr(SBUF("/tmp/asmXXXXXX"))); // string is copied to dynamic memory
	}

	if (obj_output_path) {
		if (isPathDir(obj_output_path)) {
			obj_output_path = tostr(SBUF(obj_output_path), PATHSEP, basename, OBJ_EXT);
		}
	} else {
		obj_output_path = mktemp(tostr(SBUF("/tmp/objXXXXXX")));
	}

	char* asm_visual_output_path = isTempPath(asm_output_path) ? tostr(CSBUF("~"), basepath, ASM_EXT) : asm_output_path;
	char* obj_visual_output_path = isTempPath(obj_output_path) ? tostr(CSBUF("~"), basepath, OBJ_EXT) : obj_output_path;



	FILE* input_fd = fopen(input_path, "rb");
	if (!input_fd) {
		eprintf("error: could not open input file `%s` (reason: %s)\n", input_path, strerror(errno));
		return 1;
	}

	Module module;
	char* search_paths[] = { ".", NULL };
	BRBLoadError err = loadModule(input_fd, &module, search_paths, BRB_EXECUTABLE);
	if (err.code) {
		printLoadError(stderr, err);
		return 1;
	}

	FILE *asm_output_fd = fopen(asm_output_path, "w");
	if (!asm_output_fd) {
		if (!isTempPath(asm_output_path)) {
			eprintf("error: could not open file `%s` for writing assembly output (reason: %s)\n", asm_output_path, strerror(errno));
		} else {
			eprintf("error: could not open temporary file for writing assembly output (reason: %s)\n", strerror(errno));
		}
		return 1;
	}

	compileModule(&module, asm_output_fd);
	fclose(asm_output_fd);

	printf(
		"%s -> %s in %.3f ms\n", 
		input_path,
		asm_visual_output_path,
		endTimer()
	);
	startTimer();

	char cmd[1024];
	ProcessInfo proc_res = {0};
	snprintf(cmd, sizeof(cmd), "as -arch arm64 -o %s %s", obj_output_path, asm_output_path);
	if (!execProcess(cmd, &proc_res)) {
		eprintf("error: could not start the assembler (reason: %s)\n", strerror(errno));
		return 1;
	} else if (proc_res.exitcode) {
		eprintf("error: native assembler exited with code %hhu\n", proc_res.exitcode);
		sbuf err_output = filecontent(proc_res.err);
		eprintf("assembler output: \"\n"sbuf_format"\"\n", unpack(err_output));
		// unlink(asm_output_path);
		return 1;
	}

	printf(
		"%s -> %s in %.3f ms\n",
		asm_visual_output_path,
		obj_visual_output_path,
		endTimer()
	);
	startTimer();

	snprintf(
		cmd, sizeof(cmd),
		"ld -arch arm64 -e "DEFAULT_ENTRY_NAME" -syslibroot `xcrun --show-sdk-path` -lSystem -o %s %s",
		exec_output_path,
		obj_output_path
	);
	if (!execProcess(cmd, &proc_res)) {
		eprintf("error: could not start the linker (reason: %s)\n", strerror(errno));
		return 1;
	} else if (proc_res.exitcode) {
		eprintf("error: linker exited with code %hhu\n", proc_res.exitcode);
		sbuf err_output = filecontent(proc_res.err);
		eprintf("linker output:\n\t\""sbuf_format"\"\n", unpack(err_output));
		unlink(asm_output_path);
		unlink(obj_output_path);
		return 1;
	}

	printf(
		"%s -> %s in %.3f ms\n",
		obj_visual_output_path,
		exec_output_path,
		endTimer()
	);
	if (isTempPath(asm_output_path)) unlink(asm_output_path);
	if (isTempPath(obj_output_path)) unlink(obj_output_path);
}
