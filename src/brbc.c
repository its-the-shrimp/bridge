#include <brb.h>
#include <errno.h>
#include <math.h>

#define STACK_ALIGNMENT 16
#define DEFAULT_ENTRY_NAME ".entry"

defArray(str);

sbuf ASM_EXT = CSBUF(".S");
sbuf EXEC_EXT = CSBUF("");
sbuf OBJ_EXT = CSBUF(".o");

char* conditionNames_arm64[] = {
	[COND_NON] = "",
	[COND_EQU] = "eq",
	[COND_NEQ] = "ne",
	[COND_LTU] = "lo",
	[COND_GTU] = "hs",
	[COND_LEU] = "ls",
	[COND_GEU] = "hi",
	[COND_LTS] = "lt",
	[COND_GTS] = "gt",
	[COND_LES] = "le",
	[COND_GES] = "ge"
};
#define oppositeConditionName(cond_id) conditionNames_arm64[opposite_conditions[cond_id]]

static const char* regNames64[N_REGS] = {
	[0] = "x0",
	[1] = "x1",
	[2] = "x2",
	[3] = "x3",
	[4] = "x4",
	[5] = "x5",
	[6] = "x6",
	[7] = "x7",
	[ZEROREG_ID] = "xzr"
};

static const char* regNames32[N_REGS] = {
	[0] = "w0",
	[1] = "w1",
	[2] = "w2",
	[3] = "w3",
	[4] = "w4",
	[5] = "w5",
	[6] = "w6",
	[7] = "w7",
	[ZEROREG_ID] = "wzr"
};

static int cond_counter = 0;
void compileCondition(FILE* dst, uint8_t cond_id, uint8_t n_ops)
{
	if (cond_id == COND_NON) return;
	assert(cond_id < N_CONDS);
	fprintf(dst, "\tb%s . + %d\n", oppositeConditionName(cond_id), (n_ops + 1) * 4);
}

int startConditionalOp(FILE* dst, uint8_t cond_id)
{
	if (!cond_id) return -1;
	fprintf(dst, "\tb%s .co_%d\n", oppositeConditionName(cond_id), cond_counter++);
	return cond_counter;
}

void endConditionalOp(FILE* dst, int cond_ctx)
{
	if (cond_ctx < 0) return;
	fprintf(dst, ".co_%d:\n", cond_ctx);
}

static char literal_buf[32];

static int bitOffset(int64_t value) {
	for (int i = 0; i < 64; ++i) {
		if (value & (1 << i)) return i;
	}
	return 64;
}

static int64_t powerOf2(int64_t value)
{
    asm (
        "tbz %0, 63, . + 8\n"
        "\tneg %0, %0\n"
        "\tclz x8, %0\n"
        "\trbit %0, %0\n"
        "\tclz %0, %0\n"
        "\tadd x8, x8, %0\n"
        "\tcmp x8, 63\n"
        "\tbeq . + 8\n"
        "\tmvn %0, xzr\n"
        : "=r"(value) : "r"(value)
    );
    return value;
}

#define INTL_REG_ONLY 0x1
#define INTL_ADDR_OFFSET 0x2

const char* intLiteral(FILE* dst, int8_t reg_id, int64_t value, int flag)
{
	if (flag == INTL_ADDR_OFFSET && inRange(value, -256, 256)) {
		snprintf(literal_buf, 32, "%lld", value);
		return literal_buf;
	}

	bool inverted = false;
	if (value < 0) {
		value *= -1;
		inverted = true;
	}

	if ((value >> bitOffset(value)) < 4096 && bitOffset(value) <= 12 && flag == 0) {
		snprintf(literal_buf, 32, "%lld", inverted ? value *= -1 : value);
		return literal_buf;
	} else {
		if (value == 0) return "xzr";

		if (value >= 65536 ? (value >> bitOffset(value)) < 65536 && (bitOffset(value) == 0 || !inverted && bitOffset(value) % 16 == 0) : true) {
			fprintf(dst, "\tmov x%hhd, %lld\n", reg_id, inverted ? value *= -1 : value);
		} else if (!(value >> 32)) {
			fprintf(
				dst,
				"\tmov x%1$hhd, %2$lld\n"
				"\tmovk x%1$hhd, %3$lld, lsl 16\n",
				reg_id, value & 0xFFFF, value >> 16
			);
		} else if (!(value >> 48)) {
			fprintf(
				dst,
				"\tmov x%1$hhd, %2$lld\n"
				"\tmovk x%1$hhd, %3$lld, lsl 16\n"
				"\tmovk x%1$hhd, %4$lld, lsl 32\n",
				reg_id, value & 0xFFFF, (value >> 16) & 0xFFFF, value >> 32
			);
		} else {
			fprintf(
				dst,
				"\tmov x%1$hhd, %2$lld\n"
				"\tmovk x%1$hhd, %3$lld, lsl 16\n"
				"\tmovk x%1$hhd, %4$lld, lsl 32\n"
				"\tmovk x%1$hhd, %5$lld, lsl 48\n",
				reg_id, value & 0xFFFF, (value >> 16) & 0xFFFF, (value >> 32) & 0xFFFF, value >> 48
			);
		}
		if (inverted) fprintf(dst, "\tmvn x%1$hhd, x%1$hhd\n", reg_id);
		snprintf(literal_buf, 32, "x%hhd", reg_id);
		return literal_buf;
	}
}

typedef struct comp_ctx {
	FILE* dst;
	int cur_frame_size;
	bool is_cur_proc_complex;
	char* src_path;
	int src_line;
} CompCtx;

uint64_t nativeStackOffset(CompCtx* ctx, uint64_t offset)
{
	return alignby(ctx->cur_frame_size, STACK_ALIGNMENT) - ctx->cur_frame_size + offset;
}

typedef void (*OpNativeCompiler) (Module*, int, CompCtx*);

void compileSysNoneNative(Module* module, int index, CompCtx* ctx)
{
	return;
}

void compileSysExitNative(Module* module, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 2);
	fprintf(ctx->dst, "\tmov x16, 1\n\tsvc 0\n");
}

void compileSysWriteNative(Module* module, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 5);
	fprintf(ctx->dst,
		"\tmov x16, 4\n"
		"\tsvc 0\n"
		"\tbcc . + 12\n"
		"\tmov x26, x0\n"
		"\tmvn x0, xzr\n"
	);
}

void compileSysArgcNative(Module* module, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 1);
	fprintf(ctx->dst, "\tmov x0, x28\n");
}

void compileSysArgvNative(Module* module, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 3);
	fprintf(ctx->dst,
		"\tmadd x0, x0, 8, x27\n"
		"\tldr x0, x0\n"
	);
}

void compileSysReadNative(Module* module, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 5);
	fprintf(ctx->dst,
		"\tmov x16, 3\n"
		"\tsvc 0\n"
		"\tbcc . + 12\n"
		"\tmov x26, x0\n"
		"\tmvn x0, xzr\n"
	);
}

void compileSysGetErrnoNative(Module* module, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 1);
	fprintf(ctx->dst, "\tmov x0, x26\n");
}

void compileSysSetErrnoNative(Module* module, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 1);
	fprintf(ctx->dst, "\tmov x26, x0\n");
}

OpNativeCompiler native_syscall_compilers[] = {
	[SYS_OP_INVALID] = &compileSysNoneNative,
	[SYS_OP_EXIT] = &compileSysExitNative,
	[SYS_OP_WRITE] = &compileSysWriteNative,
	[SYS_OP_ARGC] = &compileSysArgcNative,
	[SYS_OP_ARGV] = &compileSysArgvNative,
	[SYS_OP_READ] = &compileSysReadNative,
	[SYS_OP_GET_ERRNO] = &compileSysGetErrnoNative,
	[SYS_OP_SET_ERRNO] = &compileSysSetErrnoNative
};
static_assert(
	N_SYS_OPS == sizeof(native_syscall_compilers) / sizeof(native_syscall_compilers[0]),
	"not all syscalls have matching native compilers"
);

void compileNopNative(Module* module, int index, CompCtx* ctx)
{
	fprintf(ctx->dst, "\tnop\n");
}

void compileOpEndNative(Module* module, int index, CompCtx* ctx)
{
	if (index == module->execblock.length - 1) return;
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 3);
	fprintf(
		ctx->dst,
		"\tmov x16, 1\n"
		"\tmov x0, 0\n"
		"\tsvc 0\n"
	);
}

void compileOpSetNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	intLiteral(ctx->dst, op.dst_reg, op.value, INTL_REG_ONLY);
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpSetrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmov %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpSetdNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 2);
	fprintf(
		ctx->dst,
		"\tadr %s, %s\n",
		regNames64[op.dst_reg], module->datablocks.data[op.symbol_id].name
	);
}

void compileOpSetbNative(Module* module, int index, CompCtx* ctx)
{
    Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	intLiteral(
		ctx->dst,
		op.dst_reg,
		builtins[op.symbol_id].value,
		INTL_REG_ONLY
	);
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpSetmNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 2);
	fprintf(
		ctx->dst,
		"\tadrp %1$s, %2$s@PAGE\n"
		"\tadd %1$s, %1$s, %2$s@PAGEOFF",
		regNames64[op.dst_reg], module->memblocks.data[op.symbol_id].name
	);
}

void compileOpAddNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	fprintf(ctx->dst, "\tadd %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, 0));
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpAddrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tadd %s, %s, %s\n",
		regNames64[op.dst_reg],
		regNames64[op.src_reg],
		regNames64[op.src2_reg]
	);
}

void compileOpSubNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	fprintf(ctx->dst, "\tsub %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, 0));
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpSubrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tsub %s, %s, %s\n",
		regNames64[op.dst_reg],
		regNames64[op.src_reg],
		regNames64[op.src2_reg]
	);
}

void compileOpSyscallNative(Module* module, int index, CompCtx* ctx)
{
	native_syscall_compilers[module->execblock.data[index].symbol_id](module, index, ctx);
}

void compileOpGotoNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(ctx->dst, "\tb%s . %+lld\n", conditionNames_arm64[op.cond_id], op.op_offset * 4);
}

void compileOpCmpNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if (op.cond_id) {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		fprintf(
			ctx->dst,
			"\tccmp %s, %s, %s\n",
			regNames64[op.src_reg],
			intLiteral(ctx->dst, 8, op.value, 0),
			conditionNames_arm64[op.cond_id]
		);
		endConditionalOp(ctx->dst, cond_ctx);
	} else {
		fprintf(ctx->dst, "\tcmp %s, %s\n", regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, 0));
	}
}

void compileOpCmprNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	if (op.cond_id) {
		fprintf(
			ctx->dst,
			"\tccmp %s, %s, %s\n",
			regNames64[op.src_reg],
			regNames64[op.src2_reg],
			conditionNames_arm64[op.cond_id]
		);
	} else {
		fprintf(ctx->dst, "\tcmp %s, %s\n", regNames64[op.src_reg], regNames64[op.src2_reg]);
	}
}

void compileOpAndNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	fprintf(ctx->dst, "\tand %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, 0));
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpAndrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tand %s, %s, %s\n",
		regNames64[op.dst_reg],
		regNames64[op.src_reg],
		regNames64[op.src2_reg]
	);
}

void compileOpOrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	fprintf(ctx->dst, "\torr %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, 0));
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpOrrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\torr %s, %s, %s\n",
		regNames64[op.dst_reg],
		regNames64[op.src_reg],
		regNames64[op.src2_reg]
	);
}

void compileOpNotNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmvn %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpXorNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	fprintf(ctx->dst, "\teor %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, 0));
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpXorrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\teor %s, %s, %s\n",
		regNames64[op.dst_reg],
		regNames64[op.src_reg],
		regNames64[op.src2_reg]
	);
}

void compileOpShlNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst,
		"\tlsl %s, %s, %llu\n",
		regNames64[op.dst_reg],
		regNames64[op.src_reg],
		op.value
	);
}

void compileOpShlrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tlsl %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], regNames64[op.src2_reg]);
}

void compileOpShrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst,
		"\tlsr %s, %s, %llu\n",
		regNames64[op.dst_reg],
		regNames64[op.src_reg],
		op.value
	);
}

void compileOpShrrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tlsr %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], regNames64[op.src2_reg]);
}

void compileOpShrsNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst,
		"\tasr %s, %s, %llu\n",
		regNames64[op.dst_reg],
		regNames64[op.src_reg],
		op.value
	);
}

void compileOpShrsrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tasr %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], regNames64[op.src2_reg]);
}

void compileOpProcNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(ctx->dst, "%s:\n", op.mark_name);
	for (Op* next_op = module->execblock.data + index + 1; next_op->type != OP_ENDPROC; next_op++) {
		if (next_op->type == OP_CALL) {
			ctx->is_cur_proc_complex = true;
			break;
		}
	}
	if (ctx->is_cur_proc_complex) {
		fprintf(
			ctx->dst,
			"\tstp fp, lr, [sp, -16]!\n"
			"\tmov fp, sp\n"
		);
	}
}

void compileOpCallNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tbl %s\n", module->execblock.data[op.symbol_id].mark_name);
}

void compileOpRetNative(Module* module, int index, CompCtx* ctx)
{
	if (ctx->is_cur_proc_complex) {
		compileCondition(ctx->dst, module->execblock.data[index].cond_id, 3);
		fprintf(ctx->dst,
			"\tmov sp, fp\n"
			"\tldp fp, lr, [sp], 16\n"
			"\tret\n"
		);
	} else if (ctx->cur_frame_size) {
		int cond_ctx = startConditionalOp(ctx->dst, module->execblock.data[index].cond_id);
		fprintf(
			ctx->dst,
			"\tadd sp, sp, %s\n"
			"\tret\n",
			intLiteral(ctx->dst, 8, alignby(ctx->cur_frame_size, STACK_ALIGNMENT), 0)
		);
		endConditionalOp(ctx->dst, cond_ctx);
	} else {
		compileCondition(ctx->dst, module->execblock.data[index].cond_id, 1);
		fprintf(ctx->dst, "\tret\n");
	}
}

void compileOpEndprocNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if (module->execblock.data[index - 1].type == OP_RET) {
		ctx->cur_frame_size = 0;
		return;
	}
	if (ctx->is_cur_proc_complex) {
		ctx->is_cur_proc_complex = false;
		fprintf(
			ctx->dst,
			"\tmov sp, fp\n"
			"\tldp fp, lr, [sp], 16\n"
		);
	} else if (ctx->cur_frame_size) {
		fprintf(ctx->dst, "\tadd sp, sp, %s\n", intLiteral(ctx->dst, 8, alignby(ctx->cur_frame_size, STACK_ALIGNMENT), 0));
	}
	fprintf(ctx->dst, "\tret\n");
	ctx->cur_frame_size = 0;
}

void compileOpLd64Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldr %s, [%s]\n", regNames64[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpStr64Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstr %s, [%s]\n", regNames64[op.src_reg], regNames64[op.dst_reg]);
}

void compileOpLd32Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrw %s, [%s]\n", regNames32[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpStr32Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstr %s, [%s]\n", regNames32[op.src_reg], regNames64[op.dst_reg]);
}

void compileOpLd16Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrh %s, [%s]\n", regNames32[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpStr16Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstrh %s, [%s]\n", regNames32[op.src_reg], regNames64[op.dst_reg]);
}

void compileOpLd8Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrb %s, [%s]\n", regNames32[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpStr8Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstrb %s, [%s]\n", regNames32[op.src_reg], regNames64[op.dst_reg]);
}

void compileOpVarNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	uint64_t offset = alignby(ctx->cur_frame_size, STACK_ALIGNMENT);
	ctx->cur_frame_size += op.new_var_size;
	offset = alignby(ctx->cur_frame_size, STACK_ALIGNMENT) - offset;

	if (offset) fprintf(ctx->dst, "\tsub sp, sp, %s\n", intLiteral(ctx->dst, 8, offset, 0));
}

void compileOpSetvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tadd %s, sp, %llu\n", regNames64[op.dst_reg], nativeStackOffset(ctx, op.symbol_id));
}

void compileOpMulNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	fprintf(ctx->dst, "\tmul %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, INTL_REG_ONLY));
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpMulrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmul %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], regNames64[op.src2_reg]);
}

void compileOpDivNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	fprintf(ctx->dst, "\tudiv %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, INTL_REG_ONLY));
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpDivrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tudiv %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], regNames64[op.src2_reg]);
}

void compileOpDivsNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	fprintf(ctx->dst, "\tsdiv %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, INTL_REG_ONLY));
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpDivsrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tsdiv %s, %s, %s\n", regNames64[op.dst_reg], regNames64[op.src_reg], regNames64[op.src2_reg]);
}

void compileOpExtprocNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(
		ctx->dst, 
		".global %1$s\n"
		"%1$s:\n",
		op.mark_name
	);
}

void compileOpLdvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	uint64_t stack_offset = nativeStackOffset(ctx, op.symbol_id);

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	const char* literal = intLiteral(ctx->dst, 8, stack_offset, INTL_ADDR_OFFSET);
	switch (op.var_size) {
		case 1:
			fprintf(ctx->dst, "\tldrb %s, [sp, %s]\n", regNames64[op.dst_reg], literal);
			break;
		case 2:
			fprintf(ctx->dst, "\tldrh %s, [sp, %s]\n", regNames64[op.dst_reg], literal);
			break;
		case 4:
			fprintf(ctx->dst, "\tldrw %s, [sp, %s]\n", regNames64[op.dst_reg], literal);
			break;
		case 8:
			fprintf(ctx->dst, "\tldr %s, [sp, %s]\n", regNames64[op.dst_reg], literal);
			break;
		default:
			eprintf("internal compiler bug in compileOpLdvNative: unexpected variable size %hhd\n", op.var_size);
			abort();
	}
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpStrvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	uint64_t stack_offset = nativeStackOffset(ctx, op.symbol_id);

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	const char* literal = intLiteral(ctx->dst, 8, stack_offset, INTL_ADDR_OFFSET);
	switch (op.var_size) {
		case 1:
			fprintf(ctx->dst, "\tstrb %s, [sp, %s]\n", regNames32[op.src_reg], literal);
			break;
		case 2:
			fprintf(ctx->dst, "\tstrh %s, [sp, %s]\n", regNames32[op.src_reg], literal);
			break;
		case 4:
			fprintf(ctx->dst, "\tstr %s, [sp, %s]\n", regNames32[op.src_reg], literal);
			break;
		case 8:
			fprintf(ctx->dst, "\tstr %s, [sp, %s]\n", regNames64[op.src_reg], literal);
			break;
		default:
			eprintf("internal compiler bug in compileOpStrvNative: unexpected variable size %hhd\n", op.var_size);
			abort();
	endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpPopvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	uint64_t offset = nativeStackOffset(ctx, 0);

	switch (op.var_size) {
		case 1:
			fprintf(ctx->dst, "\tldrsb %s, [sp, %llu]\n", regNames64[op.dst_reg], offset);
			break;
		case 2:
			fprintf(ctx->dst, "\tldrsh %s, [sp, %llu]\n", regNames64[op.dst_reg], offset);
			break;
		case 4:
			fprintf(ctx->dst, "\tldrsw %s, [sp, %llu]\n", regNames64[op.dst_reg], offset);
			break;
		case 8:
			fprintf(ctx->dst, "\tldr %s, [sp, %llu]\n", regNames64[op.dst_reg], offset);
			break;
		default:
			eprintf("internal compiler bug in compileOpPopvNative: unexpected variable size %hhd\n", op.var_size);
	}

	if (offset + op.var_size >= STACK_ALIGNMENT) {
		fprintf(ctx->dst, "\tadd sp, sp, "_s2(STACK_ALIGNMENT)"\n");
	}
	ctx->cur_frame_size -= op.var_size;
}

void compileOpPushvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	printf("%d\t%d\t%d\t%d\n", alignby(ctx->cur_frame_size, STACK_ALIGNMENT), alignby(ctx->cur_frame_size + op.var_size, STACK_ALIGNMENT), ctx->cur_frame_size, ctx->cur_frame_size + op.var_size);
	if (alignby(ctx->cur_frame_size, STACK_ALIGNMENT) < alignby(ctx->cur_frame_size + op.var_size, STACK_ALIGNMENT)) {
		fprintf(ctx->dst, "\tsub sp, sp, "_s2(STACK_ALIGNMENT)"\n");
	}
	ctx->cur_frame_size += op.var_size;
	uint64_t offset = nativeStackOffset(ctx, 0);

	switch (op.var_size) {
		case 1:
			fprintf(ctx->dst, "\tstrb %s, [sp, %llu]\n", regNames32[op.src_reg], offset);
			break;
		case 2:
			fprintf(ctx->dst, "\tstrh %s, [sp, %llu]\n", regNames32[op.src_reg], offset);
			break;
		case 4:
			fprintf(ctx->dst, "\tstr %s, [sp, %llu]\n", regNames32[op.src_reg], offset);
			break;
		case 8:
			fprintf(ctx->dst, "\tstr %s, [sp, %llu]\n", regNames64[op.src_reg], offset);
			break;
		default:
			eprintf("internal compiler bug in compileOpPushvNative: unexpected variable size %hhd\n", op.var_size);
			abort();
	}
}

void compileOpAtfNative(Module* module, int index, CompCtx* ctx)
{
	ctx->src_path = module->execblock.data[index].mark_name;
	ctx->src_line = 0;
	fprintf(ctx->dst, "// %s:%d\n", ctx->src_path, ctx->src_line);
}

void compileOpAtlNative(Module* module, int index, CompCtx* ctx)
{
	ctx->src_line = module->execblock.data[index].symbol_id;
	fprintf(ctx->dst, "// %s:%d\n", ctx->src_path, ctx->src_line);
}

void compileOpSetcNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tcset %s, %s\n", regNames64[op.dst_reg], conditionNames_arm64[op.cond_arg]);
}

void compileOpDelnvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int64_t aligned_offset = alignby(ctx->cur_frame_size, STACK_ALIGNMENT);
	ctx->cur_frame_size -= op.symbol_id;
	aligned_offset -= alignby(ctx->cur_frame_size, STACK_ALIGNMENT);

	if (aligned_offset) {
		int cond_id = startConditionalOp(ctx->dst, op.cond_id);
		fprintf(ctx->dst, "\tadd sp, sp, %s\n", intLiteral(ctx->dst, 8, aligned_offset, 0));
		endConditionalOp(ctx->dst, cond_id);
	}
}

void compileOpLd64SNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldr %s, [%s]\n", regNames64[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpLd32SNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsw %s, [%s]\n", regNames64[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpLd16SNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsh %s, [%s]\n", regNames64[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpLd8SNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsb %s, [%s]\n", regNames64[op.dst_reg], regNames64[op.src_reg]);
}

void compileOpLdvsNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	uint64_t stack_offset = nativeStackOffset(ctx, op.symbol_id);

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	const char* literal = intLiteral(ctx->dst, 8, stack_offset, INTL_ADDR_OFFSET);
	switch (op.var_size) {
		case 1:
			fprintf(ctx->dst, "\tldrsb %s, [sp, %s]\n", regNames64[op.dst_reg], literal);
			break;
		case 2:
			fprintf(ctx->dst, "\tldrsh %s, [sp, %s]\n", regNames64[op.dst_reg], literal);
			break;
		case 4:
			fprintf(ctx->dst, "\tldrsw %s, [sp, %s]\n", regNames64[op.dst_reg], literal);
			break;
		case 8:
			fprintf(ctx->dst, "\tldr %s, [sp, %s]\n", regNames64[op.dst_reg], literal);
			break;
		default:
			eprintf("internal compiler bug in compileOpLdvNative: unexpected variable size %hhd\n", op.var_size);
			abort();
	}
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpSx32Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(ctx->dst, "\tsxtw %s, %s\n", regNames64[op.dst_reg], regNames32[op.src_reg]);
}

void compileOpSx16Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(ctx->dst, "\tsxth %s, %s\n", regNames64[op.dst_reg], regNames32[op.src_reg]);
}

void compileOpSx8Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(ctx->dst, "\tsxtb %s, %s\n", regNames64[op.dst_reg], regNames32[op.src_reg]);
}

void compileOpModNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if (op.dst_reg == op.src_reg) {
		fprintf(
			ctx->dst,
			"\tmov x9, %1$s\n"
			"\tudiv %1$s, %1$s, %2$s\n"
			"\tmul %1$s, %1$s, %2$s\n"
			"\tsub %1$s, x9, %1$s\n",
			regNames64[op.dst_reg], intLiteral(ctx->dst, 8, op.value, INTL_REG_ONLY)
		);
	} else {
		fprintf(
			ctx->dst,
			"\tudiv %1$s, %1$s, %3$s\n"
			"\tmul %1$s, %1$s, %3$s\n"
			"\tsub %1$s, %2$s, %1$s\n",
			regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, INTL_REG_ONLY)
		);
	}
}

void compileOpModsNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if (op.dst_reg == op.src_reg) {
		fprintf(
			ctx->dst,
			"\tmov x9, %1$s\n"
			"\tsdiv %1$s, %1$s, %2$s\n"
			"\tmul %1$s, %1$s, %2$s\n"
			"\tsub %1$s, x9, %1$s\n",
			regNames64[op.dst_reg], intLiteral(ctx->dst, 8, op.value, INTL_REG_ONLY)
		);
	} else {
		fprintf(
			ctx->dst,
			"\tsdiv %1$s, %1$s, %3$s\n"
			"\tmul %1$s, %1$s, %3$s\n"
			"\tsub %1$s, %2$s, %1$s\n",
			regNames64[op.dst_reg], regNames64[op.src_reg], intLiteral(ctx->dst, 8, op.value, INTL_REG_ONLY)
		);
	}
}

void compileOpModrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if (op.dst_reg == op.src_reg) {
		fprintf(
			ctx->dst,
			"\tmov x8, %1$s\n"
			"\tudiv %1$s, %1$s, %2$s\n"
			"\tmul %1$s, %1$s, %2$s\n"
			"\tsub %1$s, x8, %1$s\n",
			regNames64[op.dst_reg], regNames64[op.src2_reg]
		);
	} else if (op.dst_reg == op.src2_reg) {
		fprintf(
			ctx->dst,
			"\tmov x8, %1$s\n"
			"\tudiv %1$s, %2$s, x8\n"
			"\tmul %1$s, %1$s, x8\n"
			"\tsub %1$s, %2$s, %1$s\n",
			regNames64[op.dst_reg], regNames64[op.src_reg]
		);
	} else {
		fprintf(
			ctx->dst,
			"\tudiv %1$s, %2$s, %3$s\n"
			"\tmul %1$s, %1$s, %3$s\n"
			"\tsub %1$s, %2$s, %1$s\n",
			regNames64[op.dst_reg], regNames64[op.src_reg], regNames64[op.src2_reg]
		);
	}
}

void compileOpModsrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if (op.dst_reg == op.src_reg) {
		fprintf(
			ctx->dst,
			"\tmov x8, %1$s\n"
			"\tsdiv %1$s, %1$s, %2$s\n"
			"\tmul %1$s, %1$s, %2$s\n"
			"\tsub %1$s, x8, %1$s\n",
			regNames64[op.dst_reg], regNames64[op.src2_reg]
		);
	} else if (op.dst_reg == op.src2_reg) {
		fprintf(
			ctx->dst,
			"\tmov x8, %1$s\n"
			"\tsdiv %1$s, %2$s, x8\n"
			"\tmul %1$s, %1$s, x8\n"
			"\tsub %1$s, %2$s, %1$s\n",
			regNames64[op.dst_reg], regNames64[op.src_reg]
		);
	} else {
		fprintf(
			ctx->dst,
			"\tsdiv %1$s, %2$s, %3$s\n"
			"\tmul %1$s, %1$s, %3$s\n"
			"\tsub %1$s, %2$s, %1$s\n",
			regNames64[op.dst_reg], regNames64[op.src_reg], regNames64[op.src2_reg]
		);
	}
}

OpNativeCompiler native_op_compilers[] = {
	[OP_NONE] = &compileNopNative,
	[OP_END] = &compileOpEndNative,
	[OP_MARK] = &compileNopNative,
	[OP_SET] = &compileOpSetNative,
	[OP_SETR] = &compileOpSetrNative,
	[OP_SETD] = &compileOpSetdNative,
	[OP_SETB] = &compileOpSetbNative,
	[OP_SETM] = &compileOpSetmNative,
	[OP_ADD] = &compileOpAddNative,
	[OP_ADDR] = &compileOpAddrNative,
	[OP_SUB] = &compileOpSubNative,
	[OP_SUBR] = &compileOpSubrNative,
	[OP_SYS] = &compileOpSyscallNative,
	[OP_GOTO] = &compileOpGotoNative,
	[OP_CMP] = &compileOpCmpNative,
	[OP_CMPR] = &compileOpCmprNative,
	[OP_AND] = &compileOpAndNative,
	[OP_ANDR] = &compileOpAndrNative,
	[OP_OR] = &compileOpOrNative,
	[OP_ORR] = &compileOpOrrNative,
	[OP_NOT] = &compileOpNotNative,
	[OP_XOR] = &compileOpXorNative,
	[OP_XORR] = &compileOpXorrNative,
	[OP_SHL] = &compileOpShlNative,
	[OP_SHLR] = &compileOpShlrNative,
	[OP_SHR] = &compileOpShrNative,
	[OP_SHRR] = &compileOpShrrNative,
	[OP_SHRS] = &compileOpShrsNative,
	[OP_SHRSR] = &compileOpShrsrNative,
	[OP_PROC] = &compileOpProcNative,
	[OP_CALL] = &compileOpCallNative,
	[OP_RET] = &compileOpRetNative,
	[OP_ENDPROC] = &compileOpEndprocNative,
	[OP_LD64] = &compileOpLd64Native,
	[OP_STR64] = &compileOpStr64Native,
	[OP_LD32] = &compileOpLd32Native,
	[OP_STR32] = &compileOpStr32Native,
	[OP_LD16] = &compileOpLd16Native,
	[OP_STR16] = &compileOpStr16Native,
	[OP_LD8] = &compileOpLd8Native,
	[OP_STR8] = &compileOpStr8Native,
	[OP_VAR] = &compileOpVarNative,
	[OP_SETV] = &compileOpSetvNative,
	[OP_MUL] = &compileOpMulNative,
	[OP_MULR] = &compileOpMulrNative,
	[OP_DIV] = &compileOpDivNative,
	[OP_DIVR] = &compileOpDivrNative,
	[OP_DIVS] = &compileOpDivsNative,
	[OP_DIVSR] = &compileOpDivsrNative,
	[OP_EXTPROC] = &compileOpExtprocNative,
	[OP_LDV] = &compileOpLdvNative,
	[OP_STRV] = &compileOpStrvNative,
	[OP_POPV] = &compileOpPopvNative,
	[OP_PUSHV] = &compileOpPushvNative,
	[OP_ATF] = &compileOpAtfNative,
	[OP_ATL] = &compileOpAtlNative,
	[OP_SETC] = &compileOpSetcNative,
	[OP_DELNV] = &compileOpDelnvNative,
	[OP_LD64S] = &compileOpLd64SNative,
	[OP_LD32S] = &compileOpLd32SNative,
	[OP_LD16S] = &compileOpLd16SNative,
	[OP_LD8S] = &compileOpLd8SNative,
	[OP_LDVS] = &compileOpLdvsNative,
	[OP_SX32] = &compileOpSx32Native,
	[OP_SX16] = &compileOpSx16Native,
	[OP_SX8] = &compileOpSx8Native,
	[OP_MOD] = &compileOpModNative,
	[OP_MODS] = &compileOpModsNative,
	[OP_MODR] = &compileOpModrNative,
	[OP_MODSR] = &compileOpModsrNative
};
static_assert(
	N_OPS == sizeof(native_op_compilers) / sizeof(native_op_compilers[0]),
	"not all operations have matching native compilers"
);

void compileByteCode(Module* src, FILE* dst)
// notes: 
// 		x28 - argc
// 		x27 - argv
// 		x26 - errno
{
	CompCtx ctx = {.dst = dst};

	fprintf(dst, ".bss\n");
	array_foreach(MemBlock, block, src->memblocks, 
		fprintf(dst, "\t%s: .zero %lld\n", block.name, block.size);
	);

	fprintf(dst, ".text\n");
	if (src->datablocks.length) {
		array_foreach(DataBlock, block, src->datablocks,
			fprintf(dst, "\t%s: .ascii \"", block.name);
			array_foreach(DataPiece, piece, block.pieces,
				switch (piece.type) {
					case PIECE_LITERAL:
						fputsbufesc(dst, piece.data, BYTEFMT_ESC_DQUOTE | BYTEFMT_HEX);
						break;
					case PIECE_INT16: {
						int16_t x = (int16_t)piece.integer;
						fputsbufesc(dst, (sbuf){ .data = (char*)&x, .length = sizeof(x) }, BYTEFMT_ESC_DQUOTE | BYTEFMT_HEX);
						break;
					} case PIECE_INT32: {
						int32_t x = (int32_t)piece.integer;
						fputsbufesc(dst, (sbuf){ .data = (char*)&x, .length = sizeof(x) }, BYTEFMT_ESC_DQUOTE | BYTEFMT_HEX);
						break;
					} case PIECE_INT64:
						fputsbufesc(dst, (sbuf){ .data = (char*)&piece.integer, .length = sizeof(piece.integer) }, BYTEFMT_ESC_DQUOTE | BYTEFMT_HEX);
						break;
					case PIECE_NONE:
					case N_PIECE_TYPES:
					default:
						eprintf("internal compiler bug: invalid block piece type %d\n", piece.type);
						abort();
				}
			);
			fprintf(dst, "\"\n");
		);
	}


	fprintf(dst, ".align 4\n");
	if (src->entry_opid >= 0) {
		fprintf(
			dst,
			".global "DEFAULT_ENTRY_NAME"\n"
			DEFAULT_ENTRY_NAME":\n"
			"\tmov x28, x0\n"
			"\tmov x27, x1\n"
			"\tmov x26, 0\n"
			"\tmov x0, 0\n"
			"\tmov x1, 0\n"
			"\tmov x2, 0\n"
			"\tmov x3, 0\n"
			"\tmov x4, 0\n"
			"\tmov x5, 0\n"
			"\tmov x6, 0\n"
			"\tmov x7, 0\n"
			"\tbl main\n"
			"\tmov x0, 0\n"
			"\tmov x16, 1\n"
			"\tsvc 0\n"
		);
	}
	for (int i = 0; i < src->execblock.length; i++) {
		native_op_compilers[src->execblock.data[i].type](src, i, &ctx);
	}

}

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
		printLoadError(err);
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

	compileByteCode(&module, asm_output_fd);
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