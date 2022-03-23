#include "brb.h"
#include "unistd.h"
#include "errno.h"
#include "fcntl.h"

#define ARM64_STACK_ALIGNMENT 16
#define X86_64_STACK_ALIGNMENT 8
#define DEFAULT_ENTRY_NAME ".entry"

sbuf ASM_EXT = fromcstr(".S");
sbuf EXEC_EXT = fromcstr("");
sbuf OBJ_EXT = fromcstr(".o");

void compileNativeImmSet(FILE* dst, int8_t reg_id, uint64_t value)
{
	fprintf(dst, "\tmov x%hhd, %llu\n", reg_id, value & 0xFFFF);
	if (value >= (1U << 16)) {
		fprintf(dst, "\tmovk x%hhd, %llu, lsl 16\n", reg_id, (value >> 16) & 0xFFFF);
	}
	if (value >= (1UL << 32)) {
		fprintf(dst, "\tmovk x%hhd, %llu, lsl 32\n", reg_id, (value >> 32) & 0xFFFF);
	}
	if (value >= (1UL << 48)) {
		fprintf(dst, "\tmovk x%hhd, %llu, lsl 48\n", reg_id, (value >> 48) & 0xFFFF);
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
	fprintf(ctx->dst, "\tmov x16, 1\nsvc 0\n");
}

void compileSysWriteNative(Program* program, int index, CompCtx* ctx)
{
	fprintf(ctx->dst, 
		"\tmov x16, 4\n"
		"\tsvc 0\n"
		"\tbcc . + 16\n"
		"\tadrp x8, .errno@PAGE\n"
		"\tadd x8, x8, .errno@PAGEOFF\n"
		"\tldr x0, [x8]\n"	
	);
}

void compileSysArgcNative(Program* program, int index, CompCtx* ctx)
{
	fprintf(ctx->dst, "\tmov x0, x28\n");
}

void compileSysArgvNative(Program* program, int index, CompCtx* ctx)
{
	fprintf(ctx->dst, "\tmul x8, x0, 8\n");
	fprintf(ctx->dst, "\tadd x0, x8, x27\n");
	fprintf(ctx->dst, "\tldr x0, x0\n");
}

OpNativeCompiler native_syscall_compilers[] = {
	&compileSysNoneNative,
	&compileSysExitNative,
	&compileSysWriteNative,
	&compileSysArgcNative,
	&compileSysArgvNative
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
	fprintf(ctx->dst, "\tmov x16, 1\n\tmov x0, 0\n\tsvc 0\n");
}

void compileOpMarkNative(Program* program, int index, CompCtx* ctx)
{
	fprintf(ctx->dst, "\t%s:\n", program->execblock.data[index].mark_name);
}

void compileOpSetNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	compileNativeImmSet(ctx->dst, op.dst_reg, op.value);
}

void compileOpSetrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tmov x%hhd, x%hhd\n", op.dst_reg, op.src_reg);
}

void compileOpSetdNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tadrp x%hhd, %s@PAGE\n", op.dst_reg, program->datablocks.data[op.symbol_id].name);
	fprintf(ctx->dst, "\tadd x%hhd, x%hhd, %s@PAGEOFF\n", op.dst_reg, op.dst_reg, program->datablocks.data[op.symbol_id].name);
}

void compileOpSetbNative(Program* program, int index, CompCtx* ctx)
{
    Op op = program->execblock.data[index];
	compileNativeImmSet(ctx->dst, op.dst_reg, consts[op.symbol_id].value);
}

void compileOpSetmNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tadrp x%hhd, %s@PAGE\n", op.dst_reg, program->memblocks.data[op.symbol_id].name);
	fprintf(ctx->dst, "\tadd x%hhd, x%hhd, %s@PAGEOFF\n", op.dst_reg, op.dst_reg, program->memblocks.data[op.symbol_id].name);
}

void compileOpAddNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst, "\tadd x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tadd x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpAddrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tadd x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpSubNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst, "\tsub x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tsub x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpSubrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tsub x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpSyscallNative(Program* program, int index, CompCtx* ctx)
{
	native_syscall_compilers[program->execblock.data[index].symbol_id](program, index, ctx);
}

void compileOpGotoNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tb %s\n", program->execblock.data[op.symbol_id].mark_name);
}

void compileOpCgotoNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(
		ctx->dst, 
		"\tcbnz x%hhd, %s\n",
		op.src_reg,
		program->execblock.data[op.symbol_id].mark_name
	);
}

void compileOpEqNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, eq\n", op.dst_reg);
}

void compileOpEqrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, eq\n", op.dst_reg);
}

void compileOpNeqNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, ne\n", op.dst_reg);
}

void compileOpNeqrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, ne\n", op.dst_reg);
}

void compileOpLtNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, lo\n", op.dst_reg);
}

void compileOpLtrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, lo\n", op.dst_reg);
}

void compileOpGtNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, hi\n", op.dst_reg);
}

void compileOpGtrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, hi\n", op.dst_reg);
}

void compileOpLeNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, ls\n", op.dst_reg);
}

void compileOpLerNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, ls\n", op.dst_reg);
}

void compileOpGeNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, hs\n", op.dst_reg);
}

void compileOpGerNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, hs\n", op.dst_reg);
}

void compileOpLtsNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, lt\n", op.dst_reg);
}

void compileOpLtsrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, lt\n", op.dst_reg);
}

void compileOpGtsNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, gt\n", op.dst_reg);
}

void compileOpGtsrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, gt\n", op.dst_reg);
}

void compileOpLesNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, le\n", op.dst_reg);
}

void compileOpLesrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, le\n", op.dst_reg);
}

void compileOpGesNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < (1 << 16)) {
		fprintf(ctx->dst, "\tcmp x%hhd, %llu\n", op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tcmp x%hhd, x8\n", op.src_reg);
	}
	fprintf(ctx->dst, "\tcset x%hhd, ge\n", op.dst_reg);
}

void compileOpGesrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tcmp x%hhd, x%hhd\n", op.src_reg, op.src2_reg);
	fprintf(ctx->dst, "\tcset x%hhd, ge\n", op.dst_reg);
}

void compileOpAndNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst, "\tand %hhd, %hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tand x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpAndrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

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
		fprintf(ctx->dst, "\torr %hhd, %hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\torr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpOrrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

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
	fprintf(ctx->dst, "\tmvn x%hhd, x%hhd\n", op.dst_reg, op.src_reg);
}

void compileOpXorNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst, "\teor %hhd, %hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\teor x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpXorrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];

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
		fprintf(ctx->dst,
			"\tlsl x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tlsl x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpShlrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tlsl x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpShrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst,
			"\tlsr x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tlsr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpShrrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tlsr x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpShrsNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst,
			"\tasr x%hhd, x%hhd, %llu\n",
			op.dst_reg,
			op.src_reg,
			op.value
		);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tasr x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpShrsrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tlsr x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
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
	fprintf(ctx->dst, "\tbl %s\n", op.mark_name);
}

void compileOpRetNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tldp fp, lr, [sp], 16\n");
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
	fprintf(ctx->dst, "\tldr x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr64Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tstr x%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd32Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tldrsw x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr32Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tstr w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd16Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tldrsh x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr16Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tstrh w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpLd8Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tldrsb x%hhd, [x%hhd]\n", op.dst_reg, op.src_reg);
}

void compileOpStr8Native(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tstrb w%hhd, [x%hhd]\n", op.src_reg, op.dst_reg);
}

void compileOpVarNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	float frame_size = (float)frameSize(ctx->cur_frame);

	if (floorf(frame_size / ARM64_STACK_ALIGNMENT) < floorf((frame_size + op.var_size) / ARM64_STACK_ALIGNMENT)) {
		fprintf(ctx->dst, "\tsub sp, sp, 16\n");
	}
	VarArray_append(&ctx->cur_frame, (Var){ .size = op.var_size, .n_elements = 1 });
}

void compileOpSetvNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	int frame_size = frameSize(ctx->cur_frame);
	uint16_t offset = ceilf((float)frame_size / (float)ARM64_STACK_ALIGNMENT) * ARM64_STACK_ALIGNMENT - frame_size;

	fprintf(ctx->dst, "\tadd x%hhd, sp, %llu\n", op.dst_reg, offset + op.symbol_id);
}

void compileOpMulNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst, "\tmul x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tmul x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpMulrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tmul x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpDivNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpDivrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tudiv x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
}

void compileOpDivsNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	if ((uint64_t)op.value < 4096) {
		fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, %llu", op.dst_reg, op.src_reg, op.value);
	} else {
		compileNativeImmSet(ctx->dst, 8, op.value);
		fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, x8\n", op.dst_reg, op.src_reg);
	}
}

void compileOpDivsrNative(Program* program, int index, CompCtx* ctx)
{
	Op op = program->execblock.data[index];
	fprintf(ctx->dst, "\tsdiv x%hhd, x%hhd, x%hhd\n", op.dst_reg, op.src_reg, op.src2_reg);
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
	&compileOpCgotoNative,
	&compileOpEqNative,
	&compileOpEqrNative,
	&compileOpNeqNative,
	&compileOpNeqrNative,
	&compileOpLtNative,
	&compileOpLtrNative,
	&compileOpGtNative,
	&compileOpGtrNative,
	&compileOpLeNative,
	&compileOpLerNative,
	&compileOpGeNative,
	&compileOpGerNative,
	&compileOpLtsNative,
	&compileOpLtsrNative,
	&compileOpGtsNative,
	&compileOpGtsrNative,
	&compileOpLesNative,
	&compileOpLesrNative,
	&compileOpGesNative,
	&compileOpGesrNative,
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
	&compileOpDivsrNative
};
static_assert(
	N_OPS == sizeof(native_op_compilers) / sizeof(native_op_compilers[0]),
	"not all operations have matching native compilers"
);

void compileByteCode(Program* src, FILE* dst)
{
	enter_tempctx(funcctx, 0);
	CompCtx ctx = { .dst = dst, .cur_frame = VarArray_new(TEMP_CTX, 0) };

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
	fprintf(dst, "\t.errno: .word\n");

	fprintf(dst, ".text\n.align 4\n");
	if (src->entry_opid) {
		fprintf(dst, ".global %s\n", src->execblock.data[src->entry_opid].mark_name);
	} else {
		fprintf(dst, ".global "DEFAULT_ENTRY_NAME"\n"DEFAULT_ENTRY_NAME":\n");
	}
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

	char* cmd;
	uint8_t exitcode;
	FILE* err_output;
	asprintf(&cmd, "as -arch arm64 -o %s %s", obj_output_path, asm_output_path);
	if ((exitcode = execProcess(cmd, NULL, NULL, &err_output))) {
		eprintf("error: native assembler exited with code %hhu\n", exitcode);
		eprintf("assembler output: "sbuf_format"\n", unpack(filecontent(err_output, GLOBAL_CTX)));
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

	asprintf(
		&cmd, 
		"ld -arch arm64 -e %s -syslibroot `xcrun --show-sdk-path` -lSystem -o %s %s",
		program.entry_opid ? program.execblock.data[program.entry_opid].mark_name : DEFAULT_ENTRY_NAME,
		exec_output_path,
		obj_output_path
	);
	if ((exitcode = execProcess(cmd, NULL, NULL, &err_output))) {
		eprintf("error: linker exited with code %hhu\n", exitcode);
		eprintf("linker output:\n"sbuf_format"\n", unpack(filecontent(err_output, GLOBAL_CTX)));
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