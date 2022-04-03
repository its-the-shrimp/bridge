#include "brb.h"
#include "errno.h"

#define ARM64_STACK_ALIGNMENT 16
#define X86_64_STACK_ALIGNMENT 8
#define DEFAULT_ENTRY_NAME ".entry"

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

void compileCondition(FILE* dst, uint8_t cond_id, uint8_t n_ops)
{
	if (!cond_id) return;
	fprintf(dst, "\tb%s . + %d\n", oppositeConditionName(cond_id), (n_ops + 1) * 4);
}

void compileNativeImmSet(FILE* dst, int8_t reg_id, uint64_t value, uint8_t cond_id, uint8_t op_offset)
{
	int block_size = value > 1 ? ceil(log((double)value - 1) / log(65536.0)) : 1;
	compileCondition(dst, cond_id, block_size + op_offset);
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

declArray(Var);
defArray(Var);
typedef struct comp_ctx {
	FILE* dst;
	VarArray cur_frame;
} CompCtx;

int frameSize(VarArray frame)
{
	int res = 0;
	array_foreach(Var, var, frame, 
		res += var.size * var.n_elements;
	);
	return res;
}

typedef void (*OpNativeCompiler) (Program*, int, CompCtx*);

void compileSysNoneNative(Program* program, int index, CompCtx* ctx)
{
	return;
}

void compileSysExitNative(Program* program, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, program->execblock.data[index].cond_id, 2);
	fprintf(ctx->dst, "\tmov x16, 1\n\tsvc 0\n");
}

void compileSysWriteNative(Program* program, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, program->execblock.data[index].cond_id, 5);
	fprintf(ctx->dst,
		"\tmov x16, 4\n"
		"\tsvc 0\n"
		"\tbcc . + 12\n"
		"\tmov x26, x0\n"
		"\tmvn x0, xzr\n"
	);
}

void compileSysArgcNative(Program* program, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, program->execblock.data[index].cond_id, 1);
	fprintf(ctx->dst, "\tmov x0, x28\n");
}

void compileSysArgvNative(Program* program, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, program->execblock.data[index].cond_id, 3);
	fprintf(ctx->dst,
		"\tmul x8, x0, 8\n"
		"\tadd x0, x8, x27\n"
		"\tldr x0, x0\n"
	);
}

void compileSysReadNative(Program* program, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, program->execblock.data[index].cond_id, 5);
	fprintf(ctx->dst,
		"\tmov x16, 3\n"
		"\tsvc 0\n"
		"\tbcc . + 12\n"
		"\tmov x26, x0\n"
		"\tmvn x0, xzr\n"
	);
}

void compileSysGetErrnoNative(Program* program, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, program->execblock.data[index].cond_id, 1);
	fprintf(ctx->dst, "\tmov x0, x26\n");
}

void compileSysSetErrnoNative(Program* program, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, program->execblock.data[index].cond_id, 1);
	fprintf(ctx->dst, "\tmov x26, x0\n");
}

OpNativeCompiler native_syscall_compilers[] = {
	&compileSysNoneNative,
	&compileSysExitNative,
	&compileSysWriteNative,
	&compileSysArgcNative,
	&compileSysArgvNative,
	&compileSysReadNative,
	&compileSysGetErrnoNative,
	&compileSysSetErrnoNative
};
static_assert(
	N_SYS_OPS == sizeof(native_syscall_compilers) / sizeof(native_syscall_compilers[0]),
	"not all syscalls have matching native compilers"
);

void compileNopNative(Program* program, int index, CompCtx* ctx)
{
	fprintf(ctx->dst, "\tnop\n");
}

void compileOpEndNative(Program* program, int index, CompCtx* ctx)
{
	compileCondition(ctx->dst, program->execblock.data[index].cond_id, 3);
	fprintf(
		ctx->dst,
		"\tmov x16, 1\n"
		"\tmov x0, 0\n"
		"\tsvc 0\n"
	);
}

void compileOpMarkNative(Program* program, int index, CompCtx* ctx)
{
	fprintf(ctx->dst, "%s:\n", program->execblock.data[index].mark_name);
}

void compileOpSetNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileNativeImmSet(ctx->dst, op.dst_reg, op.value, op.cond_id, 0);
}

void compileOpSetrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmov x%hhd, x%hhd\n", op.dst_reg, op.src_reg);
}

void compileOpSetdNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 2);
	fprintf(ctx->dst, "\tadrp x%hhd, %s@PAGE\n", op.dst_reg, program->datablocks.data[op.symbol_id].name);
	fprintf(ctx->dst, "\tadd x%hhd, x%hhd, %s@PAGEOFF\n", op.dst_reg, op.dst_reg, program->datablocks.data[op.symbol_id].name);
}

void compileOpSetbNative(Program* program, int index, CompCtx* ctx)
{
    Op op = program->execblock.data[index];
	compileNativeImmSet(
		ctx->dst,
		op.dst_reg,
		builtins[op.symbol_id].value,
		op.cond_id,
		0
	);
}

void compileOpSetmNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 2);
	fprintf(ctx->dst, "\tadrp x%hhd, %s@PAGE\n", op.dst_reg, program->memblocks.data[op.symbol_id].name);
	fprintf(ctx->dst, "\tadd x%hhd, x%hhd, %s@PAGEOFF\n", op.dst_reg, op.dst_reg, program->memblocks.data[op.symbol_id].name);
}

void compileOpAddNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
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
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tadd x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpAddrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tadd x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpSubNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
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
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tsub x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpSubrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tsub x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpSyscallNative(Program* program, int index, CompCtx* ctx)
{
	native_syscall_compilers[program->execblock.data[index].symbol_id](program, index, ctx);
}

void compileOpGotoNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(
		ctx->dst,
		"\tb%s %s\n",
		conditionNames_arm64[op.cond_id],
		program->execblock.data[op.symbol_id].mark_name
	);
}

void compileOpCmpNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
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
			compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
			fprintf(
				ctx->dst, 
				"\tccmp x%hhd, x8, %s\n", 
				op.src_reg,
				conditionNames_arm64[op.cond_id]
			);
		}
	} else {
		if ((uint64_t)op.value < 4096) {
			fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
		} else {
			compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
			fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
		}
	}
}

void compileOpCmprNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

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

void compileOpAndNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\tand %hhd, %hhd, %llu\n", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tand x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpAndrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\tand x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpOrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\torr %hhd, %hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\torr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpOrrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\torr x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpNotNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmvn x%hhd, x%hhd\n", op.dst_reg, op.src_reg);
}

void compileOpXorNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\teor %hhd, %hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\teor x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpXorrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(
		ctx->dst,
		"\teor x%hhd, x%hhd, x%hhd\n",
		op.dst_reg,
		op.src_reg,
		op.src2_reg
	);
}

void compileOpShlNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst,
			"\tlsl x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tlsl x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpShlrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tlsl x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpShrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst,
			"\tlsr x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tlsr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpShrrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tlsr x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpShrsNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst,
			"\tasr x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tasr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpShrsrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tasr x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpProcNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "%s:\n", op.mark_name);
	fprintf(
		ctx->dst, 
		"\tstp fp, lr, [sp, -16]!\n"
		"\tmov fp, sp\n"
	);
}

void compileOpCallNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tbl %s\n", op.mark_name);
}

void compileOpRetNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 3);
	fprintf(
		ctx->dst,
		"\tmov sp, fp\n"
		"\tldp fp, lr, [sp], 16\n"
		"\tret\n"
	);
}

void compileOpEndprocNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tret\n");
	VarArray_clear(&ctx->cur_frame);
}

void compileOpLd64Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldr x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr64Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstr x%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd32Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsw x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr32Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstr w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd16Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsh x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr16Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstrh w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd8Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tldrsb x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr8Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tstrb w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpVarNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	float frame_size = (float)frameSize(ctx->cur_frame);

	if (ceilf(frame_size / ARM64_STACK_ALIGNMENT) < ceilf((frame_size + op.var_size) / ARM64_STACK_ALIGNMENT)) {
		fprintf(ctx->dst, "\tsub sp, sp, 16\n");
	}
	VarArray_append(&ctx->cur_frame, (Var){ .size = op.var_size, .n_elements = 1 });
}

void compileOpSetvNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	int frame_size = frameSize(ctx->cur_frame);
	uint16_t offset = ceilf((float)frame_size / (float)ARM64_STACK_ALIGNMENT) * ARM64_STACK_ALIGNMENT - frame_size;

	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tadd x%hhd, sp, %llu\n", op.dst_reg, offset + op.symbol_id);
}

void compileOpMulNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\tmul x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tmul x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpMulrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tmul x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpDivNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpDivrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpDivsNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		compileCondition(ctx->dst, op.cond_id, 1);
		fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value, op.cond_id, 1);
		fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpDivsrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileCondition(ctx->dst, op.cond_id, 1);
	fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpExtprocNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(
		ctx->dst, 
		".global %s\n"
		"%s:\n",
		op.mark_name,
		op.mark_name
	);
}


OpNativeCompiler native_op_compilers[] = {
	&compileNopNative,
	&compileOpEndNative,
	&compileOpMarkNative,
	&compileOpSetNative,
	&compileOpSetrNative,
	&compileOpSetdNative,
	&compileOpSetbNative,
	&compileOpSetmNative,
	&compileOpAddNative,
	&compileOpAddrNative,
	&compileOpSubNative,
	&compileOpSubrNative,
	&compileOpSyscallNative,
	&compileOpGotoNative,
	&compileOpCmpNative,
	&compileOpCmprNative,
	&compileOpAndNative,
	&compileOpAndrNative,
	&compileOpOrNative,
	&compileOpOrrNative,
	&compileOpNotNative,
	&compileOpXorNative,
	&compileOpXorrNative,
	&compileOpShlNative,
	&compileOpShlrNative,
	&compileOpShrNative,
	&compileOpShrrNative,
	&compileOpShrsNative,
	&compileOpShrsrNative,
	&compileOpProcNative,
	&compileOpCallNative,
	&compileOpRetNative,
	&compileOpEndprocNative,
	&compileOpLd64Native,
	&compileOpStr64Native,
	&compileOpLd32Native,
	&compileOpStr32Native,
	&compileOpLd16Native,
	&compileOpStr16Native,
	&compileOpLd8Native,
	&compileOpStr8Native,
	&compileOpVarNative,
	&compileOpSetvNative,
	&compileOpMulNative,
	&compileOpMulrNative,
	&compileOpDivNative,
	&compileOpDivrNative,
	&compileOpDivsNative,
	&compileOpDivsrNative,
	&compileOpExtprocNative
};
static_assert(
	N_OPS == sizeof(native_op_compilers) / sizeof(native_op_compilers[0]),
	"not all operations have matching native compilers"
);

void compileByteCode(Program* src, FILE* dst)
// notes: 
// 		x28 - argc
// 		x27 - argv
// 		x26 - errno
{
	enter_tempctx(funcctx, 0);
	CompCtx ctx = { .dst = dst, .cur_frame = VarArray_new(TEMP_CTX, 0) };

	if (src->datablocks.length) {
		fprintf(dst, ".data\n");
		array_foreach(DataBlock, block, src->datablocks, 
			fprintf(dst, "\t%s: .asciz \"", block.name);
			fputsbufesc(dst, block.spec, BYTEFMT_ESC_DQUOTE | BYTEFMT_HEX);
			fprintf(dst, "\"\n");
		);
	}

	fprintf(dst, ".bss\n");
	array_foreach(MemBlock, block, src->memblocks, 
		fprintf(dst, "\t%s: .zero %lld\n", block.name, block.size);
	);
	fprintf(dst, "\t.errno: .word\n");

	fprintf(dst, ".text\n.align 4\n");
	fprintf(
		dst,
		".global "DEFAULT_ENTRY_NAME"\n"
		DEFAULT_ENTRY_NAME":\n"
		"\tmov x28, x0\n"
		"\tmov x27, x1\n"
	);
	if (src->entry_opid) fprintf(dst, "\tb %s\n", src->execblock.data[src->entry_opid].mark_name);
	for (int i = 0; i < src->execblock.length; i++) {
		native_op_compilers[src->execblock.data[i].type](src, i, &ctx);
	}

	exit_tempctx(funcctx);
}

void printUsageMsg(FILE* fd, char* execname)
{
    fprintf(fd, "brbc - compile BRidge bytecode `.brb` files to native executable files.\n");
    fprintf(fd, "usage: %s [options] <file>\n", execname);
    fprintf(fd, "options:\n");
    fprintf(fd, "\t-h        Output this message and exit\n");
    fprintf(fd, "\t-a <file> Output native assembly code to file <file>. By default, compiled assembly code is not saved.\n");
    fprintf(fd, "\t-o <file> Output native executable to file <file>. Default output path is the input path without `.brb` extension.\n");
	fprintf(fd, "\t-O <file> Output native code object to file <file>. By default, compiled code object is not saved.\n");
}

int main(int argc, char* argv[])
{
    initBREnv();
	startTimer();

    char *input_path = NULL, *exec_output_path = NULL, *asm_output_path = NULL, *obj_output_path = NULL;
    bool go_on = false; 
	for (int i = 1; i < argc; i++) {
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
					case 'a':
						if (!argv[++i]) {
							eprintf("error: `-a` option specified but no assembly code file path provided\n");
							return 1;
						}
						asm_output_path = argv[i];
						go_on = true;
						break;
					case 'O':
						if (!argv[++i]) {
							eprintf("error: `-O` option specified but no object file path provided\n");
							return 1;
						}
						obj_output_path = argv[i];
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
		eprintf("error: no input file provided\n");
		return 1;
	}

	sbuf input_path_sbuf = fromstr(input_path), basename = {0};
	sbufsplit(&input_path_sbuf, &basename, fromcstr("."));
	if (!exec_output_path) {
		exec_output_path = tostr(GLOBAL_CTX, basename);
	}
	if (!asm_output_path) {
		asm_output_path = mktemp(tostr(GLOBAL_CTX, fromstr("/tmp/asmXXXXXX")));
	}
	if (!obj_output_path) {
		obj_output_path = mktemp(tostr(GLOBAL_CTX, fromstr("/tmp/objXXXXXX")));
	}
	char* asm_visual_output_path = isTempPath(asm_output_path) ? tostr(TEMP_CTX, fromcstr("~"), basename, ASM_EXT) : asm_output_path;
	char* obj_visual_output_path = isTempPath(obj_output_path) ? tostr(TEMP_CTX, fromcstr("~"), basename, OBJ_EXT) : obj_output_path;

	FILE* input_fd = fopen(input_path, "rb");
	if (!input_fd) {
		eprintf("error: could not open input file `%s` (reason: %s)\n", input_path, strerror(errno));
		return 1;
	}

	Program program;
	BRBLoadError err = loadProgram(input_fd, &program, GLOBAL_CTX);
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

	compileByteCode(&program, asm_output_fd);
	fclose(asm_output_fd);

	printf(
		"%s -> %s in %.3f ms\n", 
		input_path,
		asm_visual_output_path,
		endTimer()
	);
	startTimer();

	char cmd[1024];
	ProcessInfo proc_res;
	snprintf(cmd, sizeof(cmd), "as -arch arm64 -o %s %s", obj_output_path, asm_output_path);
	if (!execProcess(cmd, &proc_res)) {
		eprintf("error: could not start the assembler (reason: %s)\n", strerror(errno));
		return 1;
	} else if (proc_res.exitcode) {
		eprintf("error: native assembler exited with code %hhu\n", proc_res.exitcode);
		sbuf err_output = filecontent(proc_res.err, GLOBAL_CTX);
		eprintf("assembler output:\n"sbuf_format"\n", unpack(err_output));
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
		"ld -arch arm64 -e %s -syslibroot `xcrun --show-sdk-path` -lSystem -o %s %s",
		program.entry_opid ? program.execblock.data[program.entry_opid].mark_name : DEFAULT_ENTRY_NAME,
		exec_output_path,
		obj_output_path
	);
	if (!execProcess(cmd, &proc_res)) {
		eprintf("error: could not start the linker (reason: %s)\n", strerror(errno));
		return 1;
	} else if (proc_res.exitcode) {
		eprintf("error: linker exited with code %hhu\n", proc_res.exitcode);
		sbuf err_output = filecontent(proc_res.err, GLOBAL_CTX);
		eprintf("linker output:\n"sbuf_format"\n", unpack(err_output));
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