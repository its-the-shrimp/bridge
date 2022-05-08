#include "brb.h"
#include "errno.h"
#include "math.h"

#define ARM64_STACK_ALIGNMENT 16
#define X86_64_STACK_ALIGNMENT 8
#define DEFAULT_ENTRY_NAME ".entry"

defArray(str);

sbuf ASM_EXT = fromcstr(".S");
sbuf EXEC_EXT = fromcstr("");
sbuf OBJ_EXT = fromcstr(".o");

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

static int cond_counter = 0;
void compileCondition(FILE* dst, uint8_t cond_id, uint8_t n_ops)
{
	if (!cond_id) return;
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

void compileNativeImmSet(FILE* dst, int8_t reg_id, uint64_t value)
{
	int block_size = value > 1 ? ceil(log((double)value - 1) / log(65536.0)) : 1;
	switch (block_size) {
		case 4:
			fprintf(dst, "\tmovk x%hhd, %llu, lsl 48\n", reg_id, (value >> 48) & 0xFFFF);
		case 3:
			fprintf(dst, "\tmovk x%hhd, %llu, lsl 32\n", reg_id, (value >> 32) & 0xFFFFF);
		case 2:
			fprintf(dst, "\tmovk x%hhd, %llu, lsl 16\n", reg_id, (value >> 16) & 0xFFFF);
		case 1:
			fprintf(dst, "\tmov x%hhd, %llu\n", reg_id, value & 0xFFFF);
	}
}

typedef struct comp_ctx {
	FILE* dst;
	int cur_frame_size;
	char* src_path;
	int src_line;
} CompCtx;

uint64_t getNativeStackOffset(CompCtx* ctx, uint64_t offset)
{
	return ARM64_STACK_ALIGNMENT - ctx->cur_frame_size % ARM64_STACK_ALIGNMENT + offset;
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
		"\tmul x8, x0, 8\n"
		"\tadd x0, x8, x27\n"
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
	compileCondition(ctx->dst, module->execblock.data[index].cond_id, 3);
	fprintf(
		ctx->dst,
		"\tmov x16, 1\n"
		"\tmov x0, 0\n"
		"\tsvc 0\n"
	);
}

void compileOpMarkNative(Module* module, int index, CompCtx* ctx)
{
	fprintf(ctx->dst, "%s:\n", module->execblock.data[index].mark_name);
}

void compileOpSetNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	compileNativeImmSet(ctx->dst, op.dst_reg, op.value);
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpSetrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmov x%hhd, x%hhd\n", op.dst_reg, op.src_reg);
}

void compileOpSetdNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 2);
	fprintf(ctx->dst, "\tadrp x%hhd, %s@PAGE\n", op.dst_reg, module->datablocks.data[op.symbol_id].name);
	fprintf(ctx->dst, "\tadd x%hhd, x%hhd, %s@PAGEOFF\n", op.dst_reg, op.dst_reg, module->datablocks.data[op.symbol_id].name);
}

void compileOpSetbNative(Module* module, int index, CompCtx* ctx)
{
    Op op = module->execblock.data[index];

	int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
	compileNativeImmSet(
		ctx->dst,
		op.dst_reg,
		builtins[op.symbol_id].value
	);
	endConditionalOp(ctx->dst, cond_ctx);
}

void compileOpSetmNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 2);
	fprintf(ctx->dst, "\tadrp x%hhd, %s@PAGE\n", op.dst_reg, module->memblocks.data[op.symbol_id].name);
	fprintf(ctx->dst, "\tadd x%hhd, x%hhd, %s@PAGEOFF\n", op.dst_reg, op.dst_reg, module->memblocks.data[op.symbol_id].name);
}

void compileOpAddNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(
			ctx->dst,
			"\tadd x%hhd, x%hhd, %llu",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tadd x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpAddrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tadd x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpSubNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(
			ctx->dst, 
			"\tsub x%hhd, x%hhd, %llu",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tsub x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpSubrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tsub x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpSyscallNative(Module* module, int index, CompCtx* ctx)
{
	native_syscall_compilers[module->execblock.data[index].symbol_id](module, index, ctx);
}

void compileOpGotoNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(
		ctx->dst,
		"\tb%s %s\n",
		conditionNames_arm64[op.cond_id],
		module->execblock.data[op.symbol_id].mark_name
	);
}

void compileOpCmpNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if (op.cond_id) {
		if ((uint64_t)op.value < 4096) {
			fprintf(
				ctx->dst, 
				"\tccmp x%hhd, %llu, %s\n", 
				op.src_reg,
				op.value,
				conditionNames_arm64[op.cond_id]
			);
		} else {
			int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
			fprintf(
				ctx->dst, 
				"\tccmp x%hhd, x8, %s\n", 
				op.src_reg,
				conditionNames_arm64[op.cond_id]
			);
			endConditionalOp(ctx->dst, cond_ctx);
		}
	} else {
		if ((uint64_t)op.value < 4096) {
			fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
		} else {
			compileNativeImmSet(ctx->dst, 8, op.value);
			fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
		}
	}
}

void compileOpCmprNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	if (op.cond_id) {
		fprintf(
			ctx->dst,
			"\tccmp x%hhd, x%hhd, %s\n",
			op.src_reg,
			op.src2_reg,
			conditionNames_arm64[op.cond_id]
		);
	} else {
		fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	}
}

void compileOpAndNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\tand %hhd, %hhd, %llu\n", op.dst_reg, op.src_reg, op.value);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tand x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpAndrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tand x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpOrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\torr %hhd, %hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\torr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpOrrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\torr x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpNotNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmvn x%hhd, x%hhd\n", op.dst_reg, op.src_reg);
}

void compileOpXorNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\teor %hhd, %hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\teor x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpXorrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\teor x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpShlNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst,
			"\tlsl x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tlsl x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpShlrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tlsl x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpShrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst,
			"\tlsr x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tlsr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpShrrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tlsr x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpShrsNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst,
			"\tasr x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tasr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpShrsrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tasr x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpProcNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(ctx->dst, "%s:\n", op.mark_name);
	fprintf(
		ctx->dst, 
		"\tstp fp, lr, [sp, -16]!\n"
		"\tmov fp, sp\n"
	);
}

void compileOpCallNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tbl %s\n", op.mark_name);
}

void compileOpRetNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 3);
	fprintf(
		ctx->dst,
		"\tmov sp, fp\n"
		"\tldp fp, lr, [sp], 16\n"
		"\tret\n"
	);
}

void compileOpEndprocNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(ctx->dst, "\tret\n");
	ctx->cur_frame_size = 0;
}

void compileOpLd64Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldr x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr64Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstr x%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd32Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsw x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr32Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstr w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd16Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsh x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr16Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstrh w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd8Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsb x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr8Native(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstrb w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpVarNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	if (ceilf(ctx->cur_frame_size / ARM64_STACK_ALIGNMENT) < ceilf((ctx->cur_frame_size + op.var_size) / ARM64_STACK_ALIGNMENT)) {
		fprintf(ctx->dst, "\tsub sp, sp, 16\n");
	}
	ctx->cur_frame_size += op.var_size;
}

void compileOpSetvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tadd x%hhd, sp, %llu\n", op.dst_reg, getNativeStackOffset(ctx, op.symbol_id));
}

void compileOpMulNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\tmul x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tmul x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpMulrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmul x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpDivNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpDivrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpDivsNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpDivsrNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpExtprocNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	fprintf(
		ctx->dst, 
		".global %s\n"
		"%s:\n",
		op.mark_name,
		op.mark_name
	);
}

void compileOpLdvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	uint64_t stack_offset = getNativeStackOffset(ctx, op.symbol_id);
	if (stack_offset < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		switch (op.var_size) {
			case 1:
				fprintf(ctx->dst, "\tldrsb w%hhd, [sp, %lld]\n", op.dst_reg, stack_offset);
				break;
			case 2:
				fprintf(ctx->dst, "\tldrsh w%hhd, [sp, %lld]\n", op.dst_reg, stack_offset);
				break;
			case 4:
				fprintf(ctx->dst, "\tldrsw w%hhd, [sp, %lld]\n", op.dst_reg, stack_offset);
				break;
			case 8:
				fprintf(ctx->dst, "\tldr x%hhd, [sp, %lld]\n", op.dst_reg, stack_offset);
				break;
			default:
				eprintf("internal compiler bug in compileOpLdvNative: unexpected variable size %hhd\n", op.var_size);
				abort();
		}
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, stack_offset);
		switch (op.var_size) {
			case 1:
				fprintf(ctx->dst, "\tldrsb x%hhd, [sp, x8]\n", op.dst_reg);
				break;
			case 2:
				fprintf(ctx->dst, "\tldrsh x%hhd, [sp, x8]\n", op.dst_reg);
				break;
			case 4:
				fprintf(ctx->dst, "\tldrsw x%hhd, [sp, x8]\n", op.dst_reg);
				break;
			case 8:
				fprintf(ctx->dst, "\tldr x%hhd, [sp, x8]\n", op.dst_reg);
				break;
			default:
				eprintf("internal compiler bug in compileOpLdvNative: unexpected variable size %hhd\n", op.var_size);
				abort();
		}
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpStrvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	uint64_t stack_offset = getNativeStackOffset(ctx, op.symbol_id);
	if (stack_offset < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		switch (op.var_size) {
			case 1:
				fprintf(ctx->dst, "\tstrb w%hhd, [sp, %lld]\n", op.src_reg, stack_offset);
				break;
			case 2:
				fprintf(ctx->dst, "\tstrh w%hhd, [sp, %lld]\n", op.src_reg, stack_offset);
				break;
			case 4:
				fprintf(ctx->dst, "\tstr w%hhd, [sp, %lld]\n", op.src_reg, stack_offset);
				break;
			case 8:
				fprintf(ctx->dst, "\tstr x%hhd, [sp, %lld]\n", op.src_reg, stack_offset);
				break;
			default:
				eprintf("internal compiler bug in compileOpStrvNative: unexpected variable size %hhd\n", op.var_size);
				abort();
		}
	} else {
		int cond_ctx = startConditionalOp(ctx->dst, op.cond_id);
		compileNativeImmSet(ctx->dst, 8, stack_offset);
		switch (op.var_size) {
			case 1:
				fprintf(ctx->dst, "\tstrb w%hhd, [sp, x8]\n", op.src_reg);
				break;
			case 2:
				fprintf(ctx->dst, "\tstrh w%hhd, [sp, x8]\n", op.src_reg);
				break;
			case 4:
				fprintf(ctx->dst, "\tstr w%hhd, [sp, x8]\n", op.src_reg);
				break;
			case 8:
				fprintf(ctx->dst, "\tstr x%hhd, [sp, x8]\n", op.src_reg);
				break;
			default:
				eprintf("internal compiler bug in compileOpStrvNative: unexpected variable size %hhd\n", op.var_size);
				abort();
		}
		endConditionalOp(ctx->dst, cond_ctx);
	}
}

void compileOpPopvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];
	uint64_t offset = getNativeStackOffset(ctx, 0);

	switch (op.var_size) {
		case 1:
			fprintf(ctx->dst, "\tldrsb x%hhd, [sp, %llu]\n", op.dst_reg, offset);
			break;
		case 2:
			fprintf(ctx->dst, "\tldrsh x%hhd, [sp, %llu]\n", op.dst_reg, offset);
			break;
		case 4:
			fprintf(ctx->dst, "\tldrsw x%hhd, [sp, %llu]\n", op.dst_reg, offset);
			break;
		case 8:
			fprintf(ctx->dst, "\tldr x%hhd, [sp, %llu]\n", op.dst_reg, offset);
			break;
		default:
			eprintf("internal compiler bug in compileOpPopvNative: unexpected variable size %hhd\n", op.var_size);
	}

	if (offset + op.var_size >= ARM64_STACK_ALIGNMENT) {
		fprintf(ctx->dst, "\tadd sp, sp, 16\n");
	}
	ctx->cur_frame_size -= op.var_size;
}

void compileOpPushvNative(Module* module, int index, CompCtx* ctx)
{
	Op op = module->execblock.data[index];

	if (ceilf(ctx->cur_frame_size / ARM64_STACK_ALIGNMENT) < ceilf((ctx->cur_frame_size + op.var_size) / ARM64_STACK_ALIGNMENT)) {
		fprintf(ctx->dst, "\tsub sp, sp, 16\n");
	}
	ctx->cur_frame_size += op.var_size;

	uint64_t offset = getNativeStackOffset(ctx, 0);
	switch (op.var_size) {
		case 1:
			fprintf(ctx->dst, "\tstrb w%hhd, [sp, %llu]\n", op.src_reg, offset);
			break;
		case 2:
			fprintf(ctx->dst, "\tstrh w%hhd, [sp, %llu]\n", op.src_reg, offset);
			break;
		case 4:
			fprintf(ctx->dst, "\tstr w%hhd, [sp, %llu]\n", op.src_reg, offset);
			break;
		case 8:
			fprintf(ctx->dst, "\tstr x%hhd, [sp, %llu]\n", op.src_reg, offset);
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
	fprintf(ctx->dst, "\tcset x%hhd, %s\n", op.dst_reg, conditionNames_arm64[op.cond_arg]);
}

OpNativeCompiler native_op_compilers[] = {
	[OP_NONE] = &compileNopNative,
	[OP_END] = &compileOpEndNative,
	[OP_MARK] = &compileOpMarkNative,
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
	[OP_SETC] = &compileOpSetcNative
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

	if (src->datablocks.length) {
		fprintf(dst, ".data\n");
		array_foreach(DataBlock, block, src->datablocks, 
			fprintf(dst, "\t%s: .ascii \"", block.name);
			fputsbufesc(dst, block.spec, BYTEFMT_ESC_DQUOTE | BYTEFMT_HEX);
			fprintf(dst, "\"\n");
		);
	}

	fprintf(dst, ".bss\n");
	array_foreach(MemBlock, block, src->memblocks, 
		fprintf(dst, "\t%s: .zero %lld\n", block.name, block.size);
	);

	fprintf(dst, ".text\n.align 4\n");
	if (src->entry_opid >= 0) {
		fprintf(
			dst,
			".global "DEFAULT_ENTRY_NAME"\n"
			DEFAULT_ENTRY_NAME":\n"
			"\tmov x28, x0\n"
			"\tmov x27, x1\n%s",
			"\tb main\n"
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
    initBREnv();
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
                        if (streq(argv[i], "asm-output")) {
                            if (!argv[++i]) {
                                eprintf("error: `--asm-output` option specified but no path is provided\n");
                                return 1;
                            }
                            asm_output_path = argv[i];
                            go_on = true;
                        } else if (streq(argv[i], "obj-output")) {
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

	sbuf input_path_sbuf = fromstr(input_path), basepath = {0};
	sbufsplitr(&input_path_sbuf, &basepath, fromcstr("."));
	sbuf basename = fileBaseName_s(basepath);

	if (exec_output_path) {
		if (isPathDir(exec_output_path)) {
			exec_output_path = tostr(fromstr(exec_output_path), PATHSEP, basename);
		}
	} else {
		exec_output_path = tostr(basepath);
	}

	if (asm_output_path) {
		if (isPathDir(asm_output_path)) {
			asm_output_path = tostr(fromstr(asm_output_path), PATHSEP, basename, ASM_EXT);
		}
	} else {
		asm_output_path = mktemp(tostr(fromstr("/tmp/asmXXXXXX"))); // string is copied to dynamic memory
	}

	if (obj_output_path) {
		if (isPathDir(obj_output_path)) {
			obj_output_path = tostr(fromstr(obj_output_path), PATHSEP, basename, OBJ_EXT);
		}
	} else {
		obj_output_path = mktemp(tostr(fromstr("/tmp/objXXXXXX")));
	}

	char* asm_visual_output_path = isTempPath(asm_output_path) ? tostr(fromcstr("~"), basepath, ASM_EXT) : asm_output_path;
	char* obj_visual_output_path = isTempPath(obj_output_path) ? tostr(fromcstr("~"), basepath, OBJ_EXT) : obj_output_path;



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
		eprintf("assembler output:\n\t\""sbuf_format"\"\n", unpack(err_output));
		unlink(asm_output_path);
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