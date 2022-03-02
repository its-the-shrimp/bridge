#include "brf.h"
#include "stdio.h"
#include "errno.h"
#include "unistd.h"
#include "signal.h"

bool interrupt = false;

void handleExecInt(int sig)
{
	interrupt = true;
}

int8_t loadInt8(sbuf* input)
{
	int8_t res = input->data[0];
	sbufpshift(input, 1);
	return res;
}

int16_t loadInt16(sbuf* input)
{
	int16_t res = *(int16_t*)BRByteOrder(input->data, 2);
	sbufpshift(input, 2);
	return res; 
}

int32_t loadInt32(sbuf* input)
{
	int32_t res = *(int32_t*)BRByteOrder(input->data, 4);
	sbufpshift(input, 4);
	return res;
}

int64_t loadInt64(sbuf* input)
{
	int64_t res = *(int64_t*)BRByteOrder(input->data, 8);
	sbufpshift(input, 8);
	return res;
}

typedef enum {
	BRF_ERR_OK,
	BRF_ERR_NO_MEMORY,
	BRF_ERR_NO_ENTRY_SPEC,
	BRF_ERR_NO_BLOCK_NAME,
	BRF_ERR_NO_BLOCK_SIZE,
	BRF_ERR_NO_BLOCK_SPEC,
	BRF_ERR_NO_OPCODE,
	BRF_ERR_NO_MARK_NAME,
	BRF_ERR_NO_OP_ARG,
	BRF_ERR_INVALID_OPCODE,
	BRF_ERR_UNKNOWN_SEGMENT_SPEC
} BRFErrorCode;

typedef struct {
	BRFErrorCode code;
	union {
		sbuf segment_spec; // for BRF_ERR_UNKNOWN_SEGMENT_SPEC
		int32_t opcode; // for BRF_ERR_INVALID_OPCODE and BRF_ERR_NO_OP_ARG
	};
} BRFError;

typedef BRFError (*OpLoader) (sbuf*, Program*, heapctx_t);

BRFError loadNop(sbuf* input, Program* dst, heapctx_t ctx)
{
	return (BRFError){0};
}

BRFError loadOpEnd(sbuf* input, Program* dst, heapctx_t ctx)
{
	return (BRFError){0};
}

BRFError loadOpMark(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	sbuf new;
	if (!sbufsplit(input, &new, SEP).data) {
		return (BRFError){.code = BRF_ERR_NO_MARK_NAME};
	}
	op->mark_name = tostr(ctx, new);
	return (BRFError){0};
}

BRFError loadOpSet(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 10) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_SET
		};
	}
	op->dst_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError loadOpSetr(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 3) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_SETR
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	return (BRFError){0};
}

BRFError loadOpSetd(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG, 
			.opcode = OP_SETD
		};
	}
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt32(input);
	return (BRFError){0};
}

BRFError loadOpSetm(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG, 
			.opcode = OP_SETM
		};
	}
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt32(input);
	return (BRFError){0};
}

BRFError loadOpSetc(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG, 
			.opcode = OP_SETC
		};
	}
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt32(input);
	return (BRFError){0};
}

BRFError loadOpAdd(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_ADD
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);
	return (BRFError){0};
}

BRFError loadOpAddr(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_ADDR
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);
	return (BRFError){0};
}

BRFError loadOpSub(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_SUB
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);
	return (BRFError){0};
}

BRFError loadOpSubr(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_SUBR
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);
	return (BRFError){0};
}

BRFError loadOpSyscall(sbuf* input, Program* dst, heapctx_t ctx)
{	
	Op* op = arrayhead(dst->execblock);
	if (input->length < 2) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG, 
			.opcode = OP_SYSCALL
		};
	}
	op->syscall_id = loadInt8(input);
	return (BRFError){0};
}

BRFError loadOpGoto(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 5) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG, 
			.opcode = OP_GOTO
		};
	}
	op->symbol_id = loadInt32(input);
	return (BRFError){0};
}

BRFError loadOpCgoto(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 6) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG, 
			.opcode = OP_CGOTO
		};
	}
	op->src_reg = loadInt8(input);
	op->symbol_id = loadInt32(input);
	return (BRFError){0};
}

BRFError loadOpEq(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_EQ
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError loadOpEqr(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_EQR
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);	
	return (BRFError){0};
}

BRFError loadOpNeq(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_NEQ
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError loadOpNeqr(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_NEQR
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);	
	return (BRFError){0};
}

BRFError loadOpLt(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_LT
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError loadOpLtr(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_LTR
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);	
	return (BRFError){0};
}

BRFError loadOpGt(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_GT
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError loadOpGtr(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_GTR
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);	
	return (BRFError){0};
}

BRFError loadOpLe(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_LE
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError loadOpLer(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_LER
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);	
	return (BRFError){0};
}

BRFError loadOpGe(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_GE
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError loadOpGer(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) {
		return (BRFError){
			.code = BRF_ERR_NO_OP_ARG,
			.opcode = OP_GER
		};
	}
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);	
	return (BRFError){0};
}

OpLoader op_loaders[] = {
	&loadNop,
	&loadOpEnd,
	&loadOpMark,
	&loadOpSet,
	&loadOpSetr,
	&loadOpSetd,
	&loadOpSetc,
	&loadOpSetm,
	&loadOpAdd,
	&loadOpAddr,
	&loadOpSub,
	&loadOpSubr,
	&loadOpSyscall,
	&loadOpGoto,
	&loadOpCgoto,
	&loadOpEq,
	&loadOpEqr,
	&loadOpNeq,
	&loadOpNeqr,
	&loadOpLt,
	&loadOpLtr,
	&loadOpGt,
	&loadOpGtr,
	&loadOpLe,
	&loadOpLer,
	&loadOpGe,
	&loadOpGer
};
static_assert(N_OPS == sizeof(op_loaders) / sizeof(op_loaders[0]), "Some BRF operations have unmatched loaders");

BRFError loadProgram(sbuf input, Program* dst, heapctx_t ctx)
{
	dst->execblock = OpArray_new(ctx, -1),
	dst->memblocks = MemBlockArray_new(ctx, 0),
	dst->datablocks = DataBlockArray_new(ctx, 0),
	dst->entry_opid = 0;

	DataBlock* datablock;
	MemBlock* memblock;
	Op* op;
	sbuf new;

	while (input.length) {
		if (sbufcut(&input, ENTRYSPEC_SEGMENT_START).data) {
			if (input.length < 2) return (BRFError){.code = BRF_ERR_NO_ENTRY_SPEC};
			dst->entry_opid = loadInt32(&input);
		} else if (sbufcut(&input, DATA_SEGMENT_START).data) {
			while (true) {
				if (!sbufsplit(&input, &new, SEP).data) return (BRFError){.code = BRF_ERR_NO_BLOCK_NAME};
				if (!new.length) break;

				if (!(datablock = DataBlockArray_append(&dst->datablocks, (DataBlock){0}))) {
					return (BRFError){.code = BRF_ERR_NO_MEMORY};
				}
				datablock->name = tostr(ctx, new);

				if (input.length < 5) return (BRFError){.code = BRF_ERR_NO_BLOCK_SIZE};
				datablock->spec.length = loadInt32(&input);

				if (input.length < datablock->spec.length + 1) {
					return (BRFError){.code = BRF_ERR_NO_BLOCK_SPEC};
				}
				datablock->spec.data = input.data;
				sbufshift(input, datablock->spec.length);
			}
		} else if (sbufcut(&input, MEMBLOCK_SEGMENT_START).data) {
			while (true) {
				if (!sbufsplit(&input, &new, SEP).data) return (BRFError){.code = BRF_ERR_NO_BLOCK_NAME};
				if (!new.length) break;

				if (!(memblock = MemBlockArray_append(&dst->memblocks, (MemBlock){0}))) {
					return (BRFError){.code = BRF_ERR_NO_MEMORY};
				}
				memblock->name = tostr(ctx, new);

				if (input.length < 5) return (BRFError){.code = BRF_ERR_NO_BLOCK_SIZE};
				memblock->size = loadInt32(&input);
			}
		} else if (sbufcut(&input, EXEC_SEGMENT_START).data) {
			do {
				if (!(op = OpArray_append(&dst->execblock, (Op){0}))) {
					return (BRFError){.code = BRF_ERR_NO_MEMORY};
				}
				if (input.length < 1) return (BRFError){.code = BRF_ERR_NO_OPCODE};

				op->type = loadInt8(&input);
				if (!inRange(op->type, 0, N_OPS)) {
					return (BRFError){.code = BRF_ERR_INVALID_OPCODE, .opcode = op->type};
				}

				BRFError err = op_loaders[op->type](&input, dst, ctx);
				if (err.code != BRF_ERR_OK) return err;
			} while (op->type != OP_END);
		} else {
			return (BRFError){
				.code = BRF_ERR_UNKNOWN_SEGMENT_SPEC, 
				.segment_spec = sbufslice(input, 0, input.length < 2 ? input.length : 2)
			};
		}
	}

	return (BRFError){.code = BRF_ERR_OK};
} 

ExecEnv initExecEnv(Program* program)
{
	heapctx_t memctx = ctxalloc_newctx(CTXALLOC_OPT_CLEAR);
	ExecEnv res = {
		.heap = sctxalloc_new(0, ctxalloc_newctx(0)),
		.stack = sctxalloc_new(0, ctxalloc_newctx(CTXALLOC_OPT_EXPANDDOWN)),
		.exitcode = 0,
		.memblocks = sbufArray_new(memctx, program->memblocks.length * -1),
		.op_id = program->entry_opid,
		.registers = ctxalloc_new(sizeof(int64_t) * N_REGISTERS, memctx)
	};

	array_foreach(MemBlock, block, program->memblocks,
		sbufArray_append(&res.memblocks, sctxalloc_new(block.size, arrayctx(res.memblocks)));
	);

	return res;
}

bool handleInvalidSyscall(ExecEnv* env, Program* program)
{
	env->exitcode = EC_UNKNOWN_SYS_OP;
	return true;
}

bool handleExitSyscall(ExecEnv* env, Program* program)
{
	env->exitcode = env->registers[0];
	return true;
}

bool handleWriteSyscall(ExecEnv* env, Program* program)
{
	env->registers[0] = write(env->registers[0], (char*)env->registers[1], env->registers[2]);
	return false;
}

BRFFunc syscall_handlers[N_SYS_OPS] = {
	&handleInvalidSyscall,
	&handleExitSyscall,
	&handleWriteSyscall
};

bool handleNop(ExecEnv* env, Program* program)
{
	env->op_id++;
	return false;
}

bool handleOpEnd(ExecEnv* env, Program* program)
{
	env->exitcode = EC_OK;
	return true;
}

bool handleOpMark(ExecEnv* env, Program* program)
{
	env->op_id++;
	return false;
}

bool handleOpSet(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = op.value;
	env->op_id++;
	return false;
}

bool handleOpSetr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpSetd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)program->datablocks.data[op.symbol_id].spec.data;
	env->op_id++;
	return false;
}

bool handleOpSetc(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = consts[op.symbol_id].value;
	env->op_id++;
	return false;
}

bool handleOpSetm(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->memblocks.data[op.symbol_id].data;
	env->op_id++;
	return false;
}

bool handleOpAdd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + op.value;
	env->op_id++;
	return false;
}

bool handleOpAddr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpSub(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] - op.value;
	env->op_id++;
	return false;
}

bool handleOpSubr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] - env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpSyscall(ExecEnv* env, Program* program)
{
	// NOTE: shifting the operation index before calling the function might cause a problem, its ok for now
	return syscall_handlers[program->execblock.data[env->op_id++].syscall_id](env, program);
}

bool handleOpGoto(ExecEnv* env, Program* program)
{
	env->op_id = program->execblock.data[env->op_id].symbol_id;
	return false;
}

bool handleOpCgoto(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	if (env->registers[op.src_reg]) {
		env->op_id = op.symbol_id;
	} else {
		env->op_id++;
	}
	return false;
}

bool handleOpEq(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] == op.value;
	env->op_id++;
	return false;
}

bool handleOpEqr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] == env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpNeq(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] != op.value;
	env->op_id++;
	return false;
}

bool handleOpNeqr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] != env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpLt(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] < op.value;
	env->op_id++;
	return false;
}

bool handleOpLtr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] < env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpGt(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] > op.value;
	env->op_id++;
	return false;
}

bool handleOpGtr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] > env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpLe(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] <= op.value;
	env->op_id++;
	return false;
}

bool handleOpLer(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] <= env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpGe(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >= op.value;
	env->op_id++;
	return false;
}

bool handleOpGer(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >= env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

BRFFunc op_handlers[] = {
	&handleNop,
	&handleOpEnd,
	&handleOpMark,
	&handleOpSet,
	&handleOpSetr,
	&handleOpSetd,
	&handleOpSetc,
	&handleOpSetm,
	&handleOpAdd,
	&handleOpAddr,
	&handleOpSub,
	&handleOpSubr,
	&handleOpSyscall,
	&handleOpGoto,
	&handleOpCgoto,
	&handleOpEq,
	&handleOpEqr,
	&handleOpNeq,
	&handleOpNeqr,
	&handleOpLt,
	&handleOpLtr,
	&handleOpGt,
	&handleOpGtr,
	&handleOpLe,
	&handleOpLer,
	&handleOpGe,
	&handleOpGer
};
static_assert(N_OPS == sizeof(op_handlers) / sizeof(op_handlers[0]), "Some BRF operations have unmatched execution handlers");

ExecEnv execProgram(Program* program)
{
	ExecEnv env = initExecEnv(program);
	while (!op_handlers[program->execblock.data[env.op_id].type](&env, program)) { if (interrupt) break; }
	return env;
}

void printUsageMsg(FILE* fd, char* exec_name)
{
	fprintf(fd, "brex - Execute .brf (BRidge Executable) files\n");
	fprintf(fd, "usage: %s [options] <file>\n", exec_name);
	fprintf(fd, "options:\n");
	fprintf(fd, "\t-h     Output this message and exit\n");
	fprintf(fd, "\t-r     Dump registers' values after execution of the program\n");
}

int main(int argc, char* argv[]) {
	initBREnv();
	char* input_name = NULL;
	bool dump_registers = false;
	for (int i = 1; i < argc; i++) {
		if (streq(argv[i], "-h")) {
			printUsageMsg(stdout, argv[0]);
			return 0;
		} else if (streq(argv[i], "-r")) {
			dump_registers = true;
		} else if (argv[i][0] != '-') {
			input_name = argv[i];
		} else {
			fprintf(stderr, "error: unknown option `%s`\n", argv[i]);
			return 1;
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
	sbuf input = filecontent(input_fd, GLOBAL_CTX);
	if (!input.data) {
		fprintf(stderr, "error: could not fetch contents of file `%s`\n", input_name);
		return 1;
	}
	fclose(input_fd);

	Program program;
	BRFError err = loadProgram(input, &program, GLOBAL_CTX);
	switch (err.code) {
		case BRF_ERR_OK: break;
		case BRF_ERR_NO_MEMORY:
			eprintf("BRF error: memory allocation failure\n");
			return 1;
		case BRF_ERR_NO_ENTRY_SPEC:
			eprintf("BRF error: no entry mark specifier found\n");
			return 1;
		case BRF_ERR_NO_BLOCK_NAME:
			eprintf("BRF error: block name not found\n");
			return 1;
		case BRF_ERR_NO_BLOCK_SIZE:
			eprintf("BRF error: block size not found\n");
			return 1;
		case BRF_ERR_NO_BLOCK_SPEC:
			eprintf("BRF error: data block specifier not found\n");
			return 1;
		case BRF_ERR_NO_OPCODE:
			eprintf("BRF error: operation code not found\n");
			return 1;
		case BRF_ERR_NO_MARK_NAME:
			eprintf("BRF error: mark name not found\n");
			return 1;
		case BRF_ERR_NO_OP_ARG:
			eprintf(
				"BRF error: argument for operation `"sbuf_format"` not found\n", 
				unpack(opNames[err.opcode])
			);
			return 1;
		case BRF_ERR_INVALID_OPCODE:
			eprintf("BRF error: invalid operation code %d found\n", err.opcode);
			return 1;
		case BRF_ERR_UNKNOWN_SEGMENT_SPEC:
			eprintf("BRF error: unknown segment specifier \"");
			fputsbufesc(stderr, err.segment_spec, BYTEFMT_HEX);
			eprintf("\"\n");
			return 1;
	}

	signal(SIGINT, &handleExecInt);
	ExecEnv res = execProgram(&program);
	signal(SIGINT, SIG_DFL);

	if (dump_registers) {
		printf("registers:\n");
		for (int i = 0; i < 8; i++) {
			printf("\t%d: %lld\n", i, res.registers[i]);
		}
	}

	return res.exitcode;
}
