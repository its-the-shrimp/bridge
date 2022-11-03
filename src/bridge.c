#include <br.h>
#include <errno.h>

static char* shiftArgs(int* argc_p, char*** argv_p)
{
	--*argc_p;
	register char* res = **argv_p;
	return ++*argv_p, res;
}

static char shiftStr(char** str_p)
{
	register char res = **str_p;
	return ++*str_p, res;
}

defArray_as(char*, strArray);
static strArray inputs = {0};
static sbuf as_format; 
static char* output;

static const char* help_msg = 	"bridge - all-in-one tool for working with BRidge\n"
				"usage: %s [options] <inputs>\n"
				"options:\n"
				"\t-h, --help\tPrint this message and quit\n"
				"\t-o <path>\tSave output to <path>\n"
				"\t-x<format>\tForce <inputs> to be interpreted as <format> input\n"
				"\t\t<format> coresponds to the file endings of supported input formats:\n"
				"\t\t\tbr\tBRidge source code\n"
				"\t\t\tvbrb\tBRidge assembly code\n"
				"\t\t\tbrb\tBRidge bytecode\n";

static const sbuf formats[] = {sbuf_fromcstr("br"), sbuf_fromcstr("brb"), sbuf_fromcstr("vbrb")};


static int parseArgs(int argc, char** argv)
{
	bool x_flag_set = false;
	char* program_name = *argv;
	while (shiftArgs(&argc, &argv), *argv) {
		if (**(int16_t**)argv == *(int16_t*)"--") {
			*argv += 2;
			if (str_eq(*argv, "help")) {
				printf(help_msg, program_name);
				return 0;
			} else if (**argv == '\0') {
				assert(strArray_extend(&inputs, (strArray){.length = argc, .data = argv}),
					"memory allocation failure during initialization");
				return -1;
			} else return eprintf("error: unknown option `--%s`\n", *argv), true;
		} else if (**argv == '-') {
			while (shiftStr(argv)) switch (**argv) {
				case 'h':
					printf(help_msg, program_name);
					return 0;
				case 'o':
					shiftArgs(&argc, &argv);
					output = *argv;
					break;
				case 'x':
					shiftStr(argv);
					sbuf format_s = sbuf_fromstr(*argv);
					for (uint8_t i = 0; i < (sizeof(formats) / sizeof(formats[0])); ++i)
						if (sbuf_eq(formats[i], format_s)) {
							as_format = formats[i];
							x_flag_set = true;
							break;
						}
					if (!x_flag_set)
						return eprintf("error: unknown input format `%.*s`\n"
								"\tsupported formats: br, vbrb, brb\n", sbuf_unpack(format_s)), 1;
					break;
				default:
					return eprintf("error: unknown option `-%c`\n", **argv), 1;
			}
		} else {
			sbuf format_s = BR_getFileExt_s(sbuf_fromstr(*argv));
			if (!x_flag_set) {
				if (as_format.data) {
					if (!sbuf_eq(as_format, format_s))
						return eprintf("error: inconsistent input formats: `%.*s` and `%.*s`\n",
							sbuf_unpack(as_format), sbuf_unpack(format_s)), 1;
				} else as_format = BR_getFileExt_s(sbuf_fromstr(*argv));
			}
			strArray_append(&inputs, *argv);
		}
	}
	if (!inputs.length) return eprintf("error: no input provided\n"), 1;
	if (!output) output = *arrayhead(inputs);
	return -1;
}

int main(int argc, char* argv[])
{
	int exitcode;
	if ((exitcode = parseArgs(argc, argv)) > 0) return exitcode;
// initializing the builder
	BR_ModuleBuilder builder;
	BR_initModuleBuilder(&builder);
// processing the input
	if (sbuf_eq(as_format, sbuf_fromcstr("br"))) {
		arrayForeach_as (char*, strArray, input, inputs) {
			FILE* input_fd = fopen(*input, "r");
			if (!input_fd)
				return eprintf("error: could not open `%s` (reason: %s)\n", *input, strerror(errno)), 1;
			BR_CompilationError err = BR_loadFromSource(input_fd, *input, &builder);
			if (err.type)
				return BR_printCompilationErrorMsg(stderr, err, "compilation error"), 1;
		}
	} else if (sbuf_eq(as_format, sbuf_fromcstr("vbrb"))) {
		arrayForeach_as (char*, strArray, input, inputs) {
			FILE* input_fd = fopen(*input, "r");
			if (!input_fd)
				return eprintf("error: could not open `%s` (reason: %s)\n", *input, strerror(errno)), 1;
			BR_Error err = BR_loadFromAssembly(input_fd, *input, &builder);
			if (err.type)
				return BR_printErrorMsg(stderr, err, "assembly error"), 1;
		}
	} else if (sbuf_eq(as_format, sbuf_fromcstr("brb"))) {
		arrayForeach_as (char*, strArray, input, inputs) {
			FILE* input_fd = fopen(*input, "rb");
			if (!input_fd)
				return eprintf("error: could not open `%s` (reason: %s)\n", *input, strerror(errno)), 1;
			BR_Error err = BR_loadFromBytecode(input_fd, &builder);
			if (err.type)
				return BR_printErrorMsg(stderr, err, "loading error"), 1;
		}
	} else return eprintf("error: unknown input format `%.*s`\n"
			      "\tsupported formats: br, vbrb, brb\n", sbuf_unpack(as_format)), 1;
	return 0;
// compiling the output
	BR_Module module;
	BR_Error err = BR_extractModule(builder, &module);
	if (err.type) return BR_printErrorMsg(stderr, err, "loading error"), 1;
	FILE* output_fd = fopen(output, "w");
	if (!output_fd)
		return eprintf("error: could not open `%s` (reason: %s)\n", output, strerror(errno)), 1;
	char* native_entry_point;
	BR_compileModule_darwin_arm64(&module, output_fd, &native_entry_point);
// calling native assembler
	BR_ProcessInfo proc = {.out = stdout, .err = stderr};
	assert(BR_execProcess((char*[]){"as", "-arch", "arm64", output, "-o", output, NULL}, &proc),
		"could not call the native assembler (reason: %s)", strerror(errno));
	if (proc.exitcode)
		return eprintf("native assembler error: subprocess exited with code %i\n", proc.exitcode), 1;
// calling native linker
	proc = (BR_ProcessInfo){.out = stdout, .err = stderr};
	FILE* syslibroot_fd = popen("xcrun --show-sdk-path", "r");
	if (!syslibroot_fd) return eprintf("native linker error\n"), 1;
	sbuf syslibroot = sbuf_fromfile(syslibroot_fd);
	if (!syslibroot.data) return eprintf("native linker error\n"), 1;
	pclose(syslibroot_fd);
	syslibroot.data[syslibroot.length - 1] = '\0';
	assert(BR_execProcess((char*[]){"ld", "-syslibroot", syslibroot.data, "-lSystem", "-e", native_entry_point, "-arch", "arm64", "-o", output, output}, &proc),
		"could not call the native linker (reason: %s)", strerror(errno));
	if (proc.exitcode)
		return eprintf("native linker error: subprocess exited with code %i\n", proc.exitcode), 1;
}
