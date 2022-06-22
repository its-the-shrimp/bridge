#include <brb.h>
#include <errno.h>

typedef void (*OpPrinter) (Module*, Op*, FILE*);

#define regchar(reg_id) (reg_id < N_USER_REGS ? reg_id + '0' : 'Z')

void printNoArgOp(Module* module, Op* op, FILE* dst) {}

void printRegImmOp(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c %llu", regchar(op->dst_reg), op->value);
}

void print2RegOp(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c r%c", regchar(op->dst_reg), regchar(op->src_reg));
}

void printOpSetd(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c %s", regchar(op->dst_reg), module->seg_data.data[op->symbol_id].name);
}

void printOpSetb(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c %s", regchar(op->dst_reg), builtins[op->symbol_id].name);
}

void printOpSetm(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c %s", regchar(op->dst_reg), module->seg_memory.data[op->symbol_id].name);
}

void print2RegImmOp(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c r%c %llu", regchar(op->dst_reg), regchar(op->src_reg), op->value);
}

void print3RegOp(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c r%c r%c", regchar(op->dst_reg), regchar(op->src_reg), regchar(op->src2_reg));
}

void printOpSys(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "%s", syscallNames[op->syscall_id].data);
}

void printOpGoto(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "%%%lld", op->op_offset);
}

void printOpCmp(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c %llu", regchar(op->src_reg), op->value);
}

void printOpCmpr(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c r%c", regchar(op->src_reg), regchar(op->src2_reg));
}

void printOpMark(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, ".m%ld", op - module->seg_exec.data);
}

void printMarkOp(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "%s", op->mark_name);
}

void printOpCall(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "%s", module->seg_exec.data[op->symbol_id].mark_name);
}

void printOpVar(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "0%%%lld %lld", op->new_var_size, op->new_var_size);
}

void printRegSymbolIdOp(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c %%%d", regchar(op->dst_reg), op->symbol_id);
}

void printSymbolIdOp(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "%d", op->symbol_id);
}

void printOpLdv(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c %d%%%hhu", regchar(op->dst_reg), op->symbol_id, op->var_size);
}

void printOpStrv(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "%d%%%hhu r%c", op->symbol_id, op->var_size, regchar(op->dst_reg));
}

void printOpPopv(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c 0%%%hhu", regchar(op->dst_reg), op->var_size);
}

void printOpPushv(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "0%%%hhu %hhu r%c", op->var_size, op->var_size, regchar(op->src_reg));
}

void printOpSetc(Module* module, Op* op, FILE* dst)
{
	fprintf(dst, "r%c %s", regchar(op->dst_reg), conditionNames[op->cond_arg].data);
}

const OpPrinter op_printers[] = {
	[OP_NONE] = &printNoArgOp,
	[OP_END] = &printNoArgOp,
	[OP_MARK] = &printOpMark,
	[OP_SET] = &printRegImmOp,
	[OP_SETR] = &print2RegOp,
	[OP_SETD] = &printOpSetd,
	[OP_SETB] = &printOpSetb,
	[OP_SETM] = &printOpSetm,
	[OP_ADD] = &print2RegImmOp,
	[OP_ADDR] = &print3RegOp,
	[OP_SUB] = &print2RegImmOp,
	[OP_SUBR] = &print3RegOp,
	[OP_SYS] = &printOpSys,
	[OP_GOTO] = &printOpGoto,
	[OP_CMP] = &printOpCmp,
	[OP_CMPR] = &printOpCmpr,
	[OP_AND] = &print2RegImmOp,
	[OP_ANDR] = &print3RegOp,
	[OP_OR] = &print2RegImmOp,
	[OP_ORR] = &print3RegOp,
	[OP_NOT] = &print2RegOp,
	[OP_XOR] = &print2RegImmOp,
	[OP_XORR] = &print3RegOp,
	[OP_SHL] = &print2RegImmOp,
	[OP_SHLR] = &print3RegOp,
	[OP_SHR] = &print2RegImmOp,
	[OP_SHRR] = &print3RegOp,
	[OP_SHRS] = &print2RegImmOp,
	[OP_SHRSR] = &print3RegOp,
	[OP_PROC] = &printMarkOp,
	[OP_CALL] = &printOpCall,
	[OP_RET] = &printNoArgOp,
	[OP_ENDPROC] = &printNoArgOp,
	[OP_LD64] = &print2RegOp,
	[OP_STR64] = &print2RegOp,
	[OP_LD32] = &print2RegOp,
	[OP_STR32] = &print2RegOp,
	[OP_LD16] = &print2RegOp,
	[OP_STR16] = &print2RegOp,
	[OP_LD8] = &print2RegOp,
	[OP_STR8] = &print2RegOp,
	[OP_VAR] = &printOpVar,
	[OP_SETV] = &printRegSymbolIdOp,
	[OP_MUL] = &print2RegImmOp,
	[OP_MULR] = &print3RegOp,
	[OP_DIV] = &print2RegImmOp,
	[OP_DIVR] = &print3RegOp,
	[OP_DIVS] = &print2RegImmOp,
	[OP_DIVSR] = &print3RegOp,
	[OP_EXTPROC] = &printMarkOp,
	[OP_LDV] = &printOpLdv,
	[OP_STRV] = &printOpStrv,
	[OP_POPV] = &printOpPopv,
	[OP_PUSHV] = &printOpPushv,
	[OP_ATF] = &printMarkOp,
	[OP_ATL] = &printSymbolIdOp,
	[OP_SETC] = &printOpSetc,
	[OP_DELNV] = &printSymbolIdOp,
	[OP_LD64S] = &print2RegOp,
	[OP_LD32S] = &print2RegOp,
	[OP_LD16S] = &print2RegOp,
	[OP_LD8S] = &print2RegOp,
	[OP_LDVS] = &printOpLdv,
	[OP_SX32] = &print2RegOp,
	[OP_SX16] = &print2RegOp,
	[OP_SX8] = &print2RegOp,
	[OP_MOD] = &print2RegImmOp,
	[OP_MODS] = &print2RegImmOp,
	[OP_MODR] = &print3RegOp,
	[OP_MODSR] = &print3RegOp
};

void printOp(ExecEnv* env, Module* module, Op* op, FILE* dst)
{
	int op_index = op - module->seg_exec.data;
	fprintf(dst, "%c 0x%08x\t%s", op_index == env->op_id ? '>' : ' ', op_index, opNames[op->type].data);
	if (op->cond_id != COND_NON)
		fprintf(dst, ":%s ", conditionNames[op->cond_id].data);
	else fputc(' ', dst);

	op_printers[op->type](module, op, dst);
	fputc('\n', dst);
}

void printExecCtx(ExecEnv* env, Module* module)
{
	if (env->op_id >= 2)
		printOp(env, module, module->seg_exec.data + env->op_id - 2, stdout);
	if (env->op_id >= 1)
		printOp(env, module, module->seg_exec.data + env->op_id - 1, stdout);
	printOp(env, module, module->seg_exec.data + env->op_id, stdout);
	if (env->op_id < module->seg_exec.length - 1)
		printOp(env, module, module->seg_exec.data + env->op_id + 1, stdout);
	if (env->op_id < module->seg_exec.length - 2)
		printOp(env, module, module->seg_exec.data + env->op_id + 2, stdout);
}


bool interrupt = false;
void handleExecInt(int sig)
{
	interrupt = true;
}

static int breakpoints[64];
static char n_breakpoints;
static int last_breakpoint = -1;
static bool program_ended;
static bool program_exited;

bool handler(ExecEnv* env, Module* module, const Op* op)
{
    for (int i = 0; i < n_breakpoints; i++) {
        if (breakpoints[i] == env->op_id) {
            if (last_breakpoint == breakpoints[i]) last_breakpoint = -1;
            else {
				last_breakpoint = breakpoints[i];
				return true;
			}
        }
    }
    return false;
}

bool opEndCallback(ExecEnv* env, Module* module, const Op* op)
{
	program_ended = true;
	return false;
}

bool opSysCallback(ExecEnv* env, Module* module, const Op* op)
{
	program_exited = true;
	return false;
}

void printUsageMsg(FILE* dst, char* exec_name)
{
    fprintf(
        dst,
        "brbd - BRB debugger\n"
        "usage: %s [options] <file> [module args...]\n"
        "options:\n"
        "\t-h    Output this message and exit\n",
        exec_name
    );
}


int main(int argc, char* argv[])
{
    char **module_argv, *input_name;
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
	BRBLoadError err = loadModule(input_fd, &module, module_search_paths, BRB_EXECUTABLE);
	if (err.code) {
		printLoadError(stderr, err);
		return 1;
	}
	if (module.entry_opid < 0) {
		eputs("error: no entry point, i.e. the \"main\" procedure found\n");
		return 1;
	}

	ExecEnv env;
	initExecEnv(&env, &module, module_argv);
	addDefaultCallback(&env, handler);
	addCallBack(&env, OP_END, opEndCallback);
	addCallBack(&env, OP_SYS, opSysCallback);
 
    	sbuf cmd = smalloc(256);
	sbuf cmd_original = cmd;
	while (true) {
		cmd = cmd_original;
		cmd.length = getline(&cmd.data, (size_t*)&cmd.length, stdin);
		cmd.length--;
		sbufstripr(&cmd, CSBUF(" "), CSBUF("\t"));
		sbufstripl(&cmd, CSBUF(" "), CSBUF("\t"));

		if (sbufeq(cmd, "q")) return 0;

			if (sbufeq(cmd, "r")) {
				execModule(&env, &module, &interrupt);
				if (last_breakpoint >= 0) {
					printf("interrupt: breakpoint on index 0x%08x\n", env.op_id);
				} else if (program_ended) {
					printf("interrupt: program finished\n");
				} else {
					printf("interrupt: program exited\n");
				}
				printExecCtx(&env, &module);
				continue;
			}

			if (sbufcut(&cmd, CSBUF("b")).data) {
				sbufstripl(&cmd, CSBUF(" "), CSBUF("\t"));
				if (cmd.length == 0) {
					printf("error: breakpoint is not provided\n");
					continue;
				}

				if (sbufint(cmd)) {
					int breakp = sbuftoint(cmd);
					if (!inRange(breakp, 0, module.seg_exec.length)) {
						printf("index 0x%08x is out of range for a breakpoint\n", breakp);
						continue;
					}

					breakpoints[n_breakpoints++] = sbuftoint(cmd);
					printf("breakpoint set at index 0x%08x\n", breakpoints[n_breakpoints - 1]);
				} else if (cmd.data[0] == '[') {
					sbufshift(cmd, 1);

					sbuf filename, lineno_spec;
					if (!sbufsplit(&cmd, &filename, CSBUF(":")).data) {
						printf("error: line number not provided\n");
						continue;
					}
					if (!sbufsplit(&cmd, &lineno_spec, CSBUF("]")).data) {
						printf("error: symbol `]` expected after a source code breakpoint specifier\n");
						continue;
					}
					if (!sbuftoint(lineno_spec)) {
						printf("error: invalid line number specifier `%.*s`\n", unpack(lineno_spec));
						continue;
					}
					int lineno = sbuftoint(lineno_spec);

					sbuf cur_src_path = (sbuf){0};
					int cur_lineno = -1;
					for (int i = 0; i < module.seg_exec.length; i++) {
						Op* op = module.seg_exec.data + i;
						if (op->type == OP_ATF) cur_src_path = SBUF(op->mark_name);
						else if (op->type == OP_ATL) cur_lineno = op->symbol_id; 

						if (sbufeq(filename, cur_src_path) && cur_lineno == lineno) {
							breakpoints[n_breakpoints++] = i;
							printf("breakpoint set at file `%.*s`, line %d, index 0x%08x\n", unpack(cur_src_path), cur_lineno, i);
							filename.data = NULL;
							break;
						}
					}

					if (filename.data)
						printf("error: reference to source code location [%.*s:%d] could not be found\n", unpack(filename), lineno);
				} else {
					sbuf proc_name;
					sbufsplit(&cmd, &proc_name, CSBUF(" "), CSBUF("\t"));

					for (int i = 0; i < module.seg_exec.length; i++) {
						Op* op = module.seg_exec.data + i;
						if (op->type == OP_PROC || op->type == OP_EXTPROC) {
							if (sbufeq(proc_name, op->mark_name)) {
								breakpoints[n_breakpoints++] = i;
								printf("breakpoint set at procedure `%s`, index 0x%08x\n", op->mark_name, i);
								proc_name.data = NULL;
								break;
							}
						}
					}

					if (proc_name.data)
						printf("error: procedure `%.*s` could not be found\n", unpack(proc_name));
				}
				continue;
			}

			if (sbufeq(cmd, "ei")) {
				printf("0x%08x\n", env.op_id);
				continue;
			}

			if (sbufeq(cmd, "n")) {
				execOp(&env, &module);
				printf("interrupt: single operation executed\n");
				printExecCtx(&env, &module);
				continue;
			}

			if (sbufeq(cmd, "ctx")) {
				printExecCtx(&env, &module);
				continue;
			}

			if (sbufcut(&cmd, CSBUF("rr")).data) {
				sbufstripl(&cmd, CSBUF(" "), CSBUF("\t"));
				if (cmd.length > 0) {
					if (cmd.length == 2) {
						if (cmd.data[0] == 'r' && cmd.data[1] >= '0' && cmd.data[1] <= '7') {
							char reg_id = cmd.data[1] - '0';
							printf("  r%hhd:\t0x%016llx\n", reg_id, env.registers[reg_id]);
						} else printf("invalid register specifier `%.*s`\n", unpack(cmd));
					} else printf("invalid register specifier `%.*s`\n", unpack(cmd));
				} else {
					for (char i = 0; i < N_USER_REGS; i++) {
						printf("  r%hhd:\t0x%016llx\n", i, env.registers[i]);
					}
					printf("  .sp:\t0x%p\n", env.stack_head);
					printf("  .ei:\t0x%08x\n", env.op_id);
				}
				continue;
			}

			if (sbufcut(&cmd, CSBUF("rm")).data) {
				sbufstripl(&cmd, CSBUF(" "), CSBUF("\t"));
				if (cmd.length == 0) {
					printf("memory address to read not provided\n");
					continue;
				}

				sbuf address, n_bytes;
				sbufsplit(&cmd, &address, CSBUF(" "), CSBUF("\t"));
				sbufstripl(&cmd, CSBUF(" "), CSBUF("\t"));
				if (cmd.length == 0) {
					printf("amount of bytes to be read not provided\n");
					continue;
				}
				sbufsplit(&cmd, &n_bytes, CSBUF(" "), CSBUF("\t"));

				if (!sbufint(address)) {
					printf("invalid memory address specifier `%.*s`\n", unpack(address));
					continue;
				}
				if (!sbufint(n_bytes)) {
					printf("invalid memory span specifier `%.*s`\n", unpack(n_bytes));
					continue;
				}
			
				sbuf span = (sbuf){ .data = (char*)sbuftoint(address), .length = sbuftoint(n_bytes) };
				printf("  %p: ", span.data);
				putsbuflnesc(span, BYTEFMT_HEX);
				continue;
			}

			if (cmd.length > 0)
				printf("invalid command `%.*s`\n", unpack(cmd));
    }

    return env.exitcode;
}
