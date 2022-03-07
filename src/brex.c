#include "brf.h"
#include "stdio.h"
#include "errno.h"
#include "unistd.h"
#include "signal.h"
#include "stdlib.h"

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
	BRF_ERR_UNKNOWN_SEGMENT_SPEC,
	BRF_ERR_NO_STACK_SIZE
} BRFErrorCode;

typedef struct {
	BRFErrorCode code;
	union {
		sbuf segment_spec; // for BRF_ERR_UNKNOWN_SEGMENT_SPEC
		int32_t opcode; // for BRF_ERR_INVALID_OPCODE and BRF_ERR_NO_OP_ARG
	};
} BRFError;

typedef BRFError (*OpLoader) (sbuf*, Program*, heapctx_t);

BRFError loadNoArgOp(sbuf* input, Program* dst, heapctx_t ctx)
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
	if (input->length < 10) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG,
		.opcode = op->type
	};
	op->dst_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError load2RegOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 3) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG,
		.opcode = op->type
	};
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	return (BRFError){0};
}

BRFError loadOpSetd(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG, 
		.opcode = op->type
	};
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt64(input);
	return (BRFError){0};
}

BRFError loadOpSetm(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG, 
		.opcode = op->type
	};
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt64(input);
	return (BRFError){0};
}

BRFError loadOpSetb(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG, 
		.opcode = op->type
	};
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt64(input);
	return (BRFError){0};
}

BRFError loadOpSyscall(sbuf* input, Program* dst, heapctx_t ctx)
{	
	Op* op = arrayhead(dst->execblock);
	if (input->length < 2) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG, 
		.opcode = op->type
	};
	op->syscall_id = loadInt8(input);
	return (BRFError){0};
}

BRFError loadJumpOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 5) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG, 
		.opcode = op->type
	};
	op->symbol_id = loadInt64(input);
	return (BRFError){0};
}

BRFError loadOpCgoto(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 6) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG, 
		.opcode = op->type
	};
	op->src_reg = loadInt8(input);
	op->symbol_id = loadInt64(input);
	return (BRFError){0};
}

BRFError load2RegImmOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 11) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG,
		.opcode = op->type
	};
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt64(input);	
	return (BRFError){0};
}

BRFError load3RegOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	if (input->length < 4) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG,
		.opcode = op->type
	};
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);	
	return (BRFError){0};
}

BRFError loadPushOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	if (input->length < 2) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG,
		.opcode = arrayhead(dst->execblock)->type
	};
	arrayhead(dst->execblock)->src_reg = loadInt8(input);
	return (BRFError){0};
}

BRFError loadPopOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	if (input->length < 2) return (BRFError){
		.code = BRF_ERR_NO_OP_ARG,
		.opcode = arrayhead(dst->execblock)->type
	};
	arrayhead(dst->execblock)->dst_reg = loadInt8(input);
	return (BRFError){0};
}

OpLoader op_loaders[] = {
	&loadNoArgOp, // OP_NONE
	&loadNoArgOp, // OP_END
	&loadOpMark,
	&loadOpSet,
	&load2RegOp, // OP_SETR
	&loadOpSetd,
	&loadOpSetb,
	&loadOpSetm,
	&load2RegImmOp, // OP_ADD
	&load3RegOp, // OP_ADDR
	&load2RegImmOp, // OP_SUB
	&load3RegOp, // OP_SUBR
	&loadOpSyscall,
	&loadJumpOp, // OP_GOTO
	&loadOpCgoto,
	&load2RegImmOp, // OP_EQ
	&load3RegOp, // OP_EQR
	&load2RegImmOp, // OP_NEQ
	&load3RegOp, // OP_NEQR
	&load2RegImmOp, // OP_LT
	&load3RegOp, // OP_LTR
	&load2RegImmOp, // OP_GT
	&load3RegOp, // OP_GTR
	&load2RegImmOp, // OP_LE
	&load3RegOp, // OP_LER
	&load2RegImmOp, // OP_GE
	&load3RegOp, // OP_GER
	&load2RegImmOp, // OP_LTS
	&load3RegOp, // OP_LTSR
	&load2RegImmOp, // OP_GTS
	&load3RegOp, // OP_GTSR
	&load2RegImmOp, // OP_LES
	&load3RegOp, // OP_LESR
	&load2RegImmOp, // OP_GES
	&load3RegOp, // OP_GESR
	&loadPushOp, // OP_PUSH64
	&loadPopOp, // OP_POP64
	&loadPushOp, // OP_PUSH32
	&loadPopOp, // OP_POP32
	&loadPushOp, // OP_PUSH16
	&loadPopOp, // OP_POP16
	&loadPushOp, // OP_PUSH8
	&loadPopOp, // OP_POP8
	&load2RegImmOp, // OP_AND
	&load3RegOp, // OP_ANDR
	&load2RegImmOp, // OP_OR
	&load3RegOp, // OP_ORR
	&load2RegOp, // OP_NOT
	&load2RegImmOp, // OP_XOR
	&load3RegOp, // OP_XORR
	&load2RegImmOp, // OP_SHL
	&load3RegOp, // OP_SHLR
	&load2RegImmOp, // OP_SHR
	&load3RegOp, // OP_SHRR
	&loadJumpOp, // OP_CALL
	&loadNoArgOp // OP_RET
};
static_assert(N_OPS == sizeof(op_loaders) / sizeof(op_loaders[0]), "Some BRF operations have unmatched loaders");

BRFError loadProgram(sbuf input, Program* dst, heapctx_t ctx)
{
	dst->execblock = OpArray_new(ctx, -1),
	dst->memblocks = MemBlockArray_new(ctx, 0),
	dst->datablocks = DataBlockArray_new(ctx, 0),
	dst->entry_opid = 0;
	dst->stack_size = DEFAULT_STACK_SIZE;

	DataBlock* datablock;
	MemBlock* memblock;
	Op* op;
	sbuf new;

	while (input.length) {
		if (sbufcut(&input, ENTRYSPEC_SEGMENT_START).data) {
			if (input.length < 8) return (BRFError){.code = BRF_ERR_NO_ENTRY_SPEC};
			dst->entry_opid = loadInt64(&input);
		} else if (sbufcut(&input, STACKSIZE_SEGMENT_START).data) {
			if (input.length < 8) return (BRFError){.code = BRF_ERR_NO_STACK_SIZE};
			dst->stack_size = loadInt64(&input);
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
				if (!input.length) return (BRFError){.code = BRF_ERR_NO_OPCODE};

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

ExecEnv initExecEnv(Program* program, int8_t flags)
{
	heapctx_t memctx = ctxalloc_newctx(0);
	ExecEnv res = {
		.heap = sctxalloc_new(0, memctx),
		.stack_brk = ctxalloc_new(program->stack_size, memctx),
		.exitcode = 0,
		.memblocks = sbufArray_new(memctx, program->memblocks.length * -1),
		.op_id = program->entry_opid,
		.registers = ctxalloc_new(sizeof(int64_t) * N_REGISTERS, memctx),
		.flags = flags,
		.prev_stack_head = NULL
	};
	res.stack_head = res.stack_brk + program->stack_size;
	memset(res.registers, 0, sizeof(int64_t) * N_REGISTERS);

	sbuf* newblock;
	array_foreach(MemBlock, block, program->memblocks,
		newblock = sbufArray_append(&res.memblocks, sctxalloc_new(block.size, memctx));
		memset(newblock->data, 0, newblock->length);
	);

	if (flags & BREX_TRACE_REGS || flags & BREX_TRACE_STACK) {
		res.regs_trace = ctxalloc_new(sizeof(Tracer) * N_REGISTERS, memctx);
		memset(res.regs_trace, 0, sizeof(Tracer) * N_REGISTERS);
		res.stack_trace = TracerArray_new(memctx, 0);
	}

	return res;
}

void printTracer(FILE* fd, Program* program, ExecEnv* env, Tracer tracer, int64_t value)
{
	static_assert(N_TRACER_TYPES == 10, "not all tracer types are handled");
	switch (tracer.type) {
			case TRACER_VOID:
				fprintf(fd, "(void)\n");
				return;
			case TRACER_BOOL:
				fprintf(fd, "(bool)%s\n", value ? "true" : "false");
				break;
			case TRACER_INT64:
				fprintf(fd, "(int64_t)%lld\n", value);
				break;
			case TRACER_INT32:
				fprintf(fd, "(int32_t)%lld\n", value);
				break;
			case TRACER_INT16:
				fprintf(fd, "(int16_t)%lld\n", value);
				break;
			case TRACER_INT8:
				fprintf(fd, "(char)'");
				fputcesc(fd, value, BYTEFMT_HEX);
				fprintf(fd, "' // %lld\n", value);
				break;
			case TRACER_DATAPTR:
				fprintf(fd, "(void*)%s ", program->datablocks.data[tracer.symbol_id].name);
				if ((char*)value < program->datablocks.data[tracer.symbol_id].spec.data) {
					fprintf(fd, "- %lld ", (int64_t)(program->datablocks.data[tracer.symbol_id].spec.data - value));
				} else if ((char*)value > program->datablocks.data[tracer.symbol_id].spec.data) {
					fprintf(fd, "+ %lld ", (int64_t)(value - (int64_t)program->datablocks.data[tracer.symbol_id].spec.data));
				}
				fprintf(fd, "// %p\n", (void*)value);
				break;
			case TRACER_MEMPTR:
				fprintf(fd, "(void*)%s ", program->memblocks.data[tracer.symbol_id].name);
				if ((char*)value < env->memblocks.data[tracer.symbol_id].data) {
					fprintf(fd, "- %lld ", (int64_t)(env->memblocks.data[tracer.symbol_id].data - value));
				} else if ((char*)value > env->memblocks.data[tracer.symbol_id].data) {
					fprintf(fd, "+ %lld ", (int64_t)(value - (int64_t)env->memblocks.data[tracer.symbol_id].data));
				}
				fprintf(fd, "// %p\n", (void*)value);
				break;
			case TRACER_STACKPTR:
				fprintf(fd, "(stackptr_t)sp + %lld // %p\n", value - (int64_t)env->stack_head, (void*)value);
				break;
			case TRACER_CONST:
				fprintf(fd, "(const int)%s // %d\n", consts[tracer.symbol_id].name, (int32_t)value);
				break;
	}
}

void printExecState(FILE* fd, ExecEnv* env, Program* program)
{
	if (env->flags & BREX_TRACE_STACK) {
		fprintf(fd, "stack:\n");
		void* cur_stack_pos = env->stack_brk + program->stack_size;
		static_assert(N_TRACER_TYPES == 10, "not all tracer types are handled");

		array_foreach(Tracer, tracer, env->stack_trace,
			fprintf(fd, "\t[%d]\t", _tracer);
			int64_t input = 0;
			switch (tracer.type) {
				case TRACER_INT64:
				case TRACER_DATAPTR:
				case TRACER_MEMPTR:
				case TRACER_STACKPTR:
					input = *(int64_t*)(cur_stack_pos -= 8);
					break;
				case TRACER_INT32:
				case TRACER_CONST:
					input = (int64_t)*(int32_t*)(cur_stack_pos -= 4);
					break;
				case TRACER_INT16:
					cur_stack_pos -= 2;
					input = (int64_t)*(int16_t*)(cur_stack_pos -= 2);
					break;
				case TRACER_INT8:
					input = (int64_t)*(int8_t*)(--cur_stack_pos);
					break;
			}
			printTracer(fd, program, env, tracer, input);
		);
		fprintf(fd, "total stack usage: %.3f Kb\n", (float)(env->stack_brk + program->stack_size - env->stack_head) / 1024.0f);
	}

	if (env->flags & BREX_TRACE_REGS) {
		fprintf(fd, "registers:\n");
		for (char i = 0; i < N_REGISTERS; i++) {
			fprintf(fd, "\t[%hhd]\t", i);
			printTracer(fd, program, env, env->regs_trace[i], env->registers[i]);
		}
	}
}

DataBlock getBufferByPtr(ExecEnv* env, Program* program, void* ptr)
{
	array_foreach(DataBlock, block, program->datablocks,
		if (inRange(ptr, block.spec.data, block.spec.data + block.spec.length)) {
			return block;
		}
	);

	array_foreach(sbuf, block, env->memblocks, 
		if (inRange(ptr, block.data, block.data + block.length)) {
			return ((DataBlock){ .name = program->memblocks.data[_block].name, .spec = block });
		}
	);

	return (DataBlock){0};
}

bool validateMemoryAccess(ExecEnv* env, Program* program, Tracer tracer, void* ptr, int64_t size)
{
	if (size < 0) {
		env->exitcode = EC_NEGATIVE_SIZE_ACCESS;
		env->err_ptr = ptr,
		env->err_access_length = size;
		return true;
	}
	if (tracer.type == TRACER_DATAPTR) {
		DataBlock block = program->datablocks.data[tracer.symbol_id];
		if (!isSlice(ptr, size, block.spec.data, block.spec.length)) {
			env->exitcode = EC_ACCESS_MISALIGNMENT;
			env->err_buf = block;
			env->err_access_length = size;
			env->err_ptr = ptr;
			return true;
		}
	} else if (tracer.type == TRACER_MEMPTR) {
		sbuf block = env->memblocks.data[tracer.symbol_id];
		if (!isSlice(ptr, size, block.data, block.length)) {
			env->exitcode = EC_ACCESS_MISALIGNMENT;
			env->err_buf = (DataBlock){ 
				.name = program->memblocks.data[tracer.symbol_id].name,
				.spec = block
			};
			env->err_access_length = size;
			env->err_ptr = ptr;
			return true;
		}
	} else {
		DataBlock block = getBufferByPtr(env, program, (char*)ptr);
		if (!block.name) {
			env->exitcode = EC_ACCESS_FAILURE;
			env->err_ptr = ptr;
			env->err_access_length = size;
			return true;
		}
		if (!isSlice(ptr, size, block.spec.data, block.spec.length)) {
			env->exitcode = EC_ACCESS_MISALIGNMENT;
			env->err_buf = block;
			env->err_access_length = size;
			env->err_ptr = ptr;
			return true;
		}
	}

	return false;
}

bool handleInvalidSyscall(ExecEnv* env, Program* program)
{
	env->exitcode = EC_UNKNOWN_SYSCALL;
	return true;
}

bool handleExitSyscall(ExecEnv* env, Program* program)
{
	env->exitcode = env->registers[0];
	return true;
}

bool handleWriteSyscall(ExecEnv* env, Program* program)
{
	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		if (validateMemoryAccess(env, program, env->regs_trace[1], (char*)env->registers[1], env->registers[2])) { return true; }
	}

	env->registers[0] = write(env->registers[0], (char*)env->registers[1], env->registers[2]);

	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		env->regs_trace[0].type = TRACER_INT64;
	}

	env->op_id++;
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

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_INT64;
	}

	env->op_id++;
	return false;
}

bool handleOpSetr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
	}

	env->op_id++;
	return false;
}

bool handleOpSetd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)program->datablocks.data[op.symbol_id].spec.data;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg] = (Tracer){ .type = TRACER_DATAPTR, .symbol_id = op.symbol_id };
	}

	env->op_id++;
	return false;
}

bool handleOpSetb(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = consts[op.symbol_id].value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg] = (Tracer){ .type = TRACER_CONST, .symbol_id = op.symbol_id };
	}

	env->op_id++;
	return false;
}

bool handleOpSetm(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->memblocks.data[op.symbol_id].data;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg] = (Tracer){ .type = TRACER_MEMPTR, .symbol_id = op.symbol_id };
	}

	env->op_id++;
	return false;
}

bool handleOpAdd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + op.value;

	if (env->flags & BREX_TRACE_REGS) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_DATAPTR:
			case TRACER_MEMPTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = TRACER_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpAddr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_DATAPTR:
			case TRACER_MEMPTR:
				if (isIntTracer(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = TRACER_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpSub(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] - op.value;

	if (env->flags & BREX_TRACE_REGS) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_DATAPTR:
			case TRACER_MEMPTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = TRACER_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpSubr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] - env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_DATAPTR:
			case TRACER_MEMPTR:
				if (isIntTracer(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = TRACER_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpSyscall(ExecEnv* env, Program* program)
{
	// NOTE: shifting the operation index before calling the function might cause a problem, its ok for now
	return syscall_handlers[program->execblock.data[env->op_id].syscall_id](env, program);
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

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpEqr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] == env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpNeq(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] != op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpNeqr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] != env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLt(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] < (uint64_t)op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLtr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] < env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGt(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] > (uint64_t)op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGtr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] > env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLe(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] <= (uint64_t)op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLer(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] <= env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGe(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >= (uint64_t)op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGer(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >= env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLts(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] < op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLtsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] < (int64_t)env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGts(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] > op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGtsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] > (int64_t)env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLes(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] <= op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLesr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] <= (int64_t)env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGes(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >= op.value;

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGesr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >= (int64_t)env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpPush64(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->stack_head -= 8;

	if (env->stack_brk > env->stack_head) {
		env->exitcode = EC_STACK_OVERFLOW;
		env->err_push_size = 8;
		return true;
	}

	if (env->flags & BREX_TRACE_STACK || env->flags & BREX_TRACE_REGS) {
		if (TracerTypeSizes[env->regs_trace[op.src_reg].type] == 8) {
			TracerArray_append(&env->stack_trace, env->regs_trace[op.src_reg]);
		} else {
			TracerArray_append(&env->stack_trace, (Tracer){ .type = TRACER_INT64 });
		}
	}

	*(int64_t*)env->stack_head = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpPop64(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->stack_head + 8 > env->stack_brk + program->stack_size) {
		env->exitcode = EC_STACK_UNDERFLOW;
		env->err_pop_size = 8;
		return true;
	}

	if (env->flags & BREX_TRACE_STACK || env->flags & BREX_TRACE_REGS) {
		if (TracerTypeSizes[arrayhead(env->stack_trace)->type] != 8) {
			env->exitcode = EC_STACK_MISALIGNMENT;
			env->err_pop_size = 8;
			return true;
		}
		env->regs_trace[op.dst_reg] = TracerArray_pop(&env->stack_trace, -1);
	}

	env->registers[op.dst_reg] = *(int64_t*)env->stack_head;
	env->stack_head += 8;
	env->op_id++;
	return false;
}

bool handleOpPush32(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->stack_head -= 4;

	if (env->stack_brk > env->stack_head) {
		env->exitcode = EC_STACK_OVERFLOW;
		env->err_push_size = 4;
		return true;
	}

	if (env->flags & BREX_TRACE_STACK || env->flags & BREX_TRACE_REGS) {
		if (TracerTypeSizes[env->regs_trace[op.src_reg].type] == 4) {
			TracerArray_append(&env->stack_trace, env->regs_trace[op.src_reg]);
		} else {
			TracerArray_append(&env->stack_trace, (Tracer){ .type = TRACER_INT32 });
		}
	}

	*(int32_t*)env->stack_head = (int32_t)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpPop32(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->stack_head + 4 > env->stack_brk + program->stack_size) {
		env->exitcode = EC_STACK_UNDERFLOW;
		env->err_pop_size = 4;
		return true;
	}

	if (env->flags & BREX_TRACE_STACK || env->flags & BREX_TRACE_REGS) {
		if (TracerTypeSizes[arrayhead(env->stack_trace)->type] != 4) {
			env->exitcode = EC_STACK_MISALIGNMENT;
			env->err_pop_size = 4;
			return true;
		}
		env->regs_trace[op.dst_reg] = TracerArray_pop(&env->stack_trace, -1);
	}

	env->registers[op.dst_reg] = *(int32_t*)env->stack_head;
	env->stack_head += 4;
	env->op_id++;
	return false;
}

bool handleOpPush16(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->stack_head -= 2;

	if (env->stack_brk > env->stack_head) {
		env->exitcode = EC_STACK_OVERFLOW;
		env->err_push_size = 2;
		return true;
	}

	if (env->flags & BREX_TRACE_STACK || env->flags & BREX_TRACE_REGS) {
		if (TracerTypeSizes[env->regs_trace[op.src_reg].type] == 2) {
			TracerArray_append(&env->stack_trace, env->regs_trace[op.src_reg]);
		} else {
			TracerArray_append(&env->stack_trace, (Tracer){ .type = TRACER_INT16 });
		}
	}

	*(int16_t*)env->stack_head = (int16_t)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpPop16(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->stack_head + 2 > env->stack_brk + program->stack_size) {
		env->exitcode = EC_STACK_UNDERFLOW;
		env->err_pop_size = 2;
		return true;
	}

	if (env->flags & BREX_TRACE_STACK || env->flags & BREX_TRACE_REGS) {
		if (TracerTypeSizes[arrayhead(env->stack_trace)->type] != 2) {
			env->exitcode = EC_STACK_MISALIGNMENT;
			env->err_pop_size = 2;
			return true;
		}
		env->regs_trace[op.dst_reg] = TracerArray_pop(&env->stack_trace, -1);
	}

	env->registers[op.dst_reg] = *(int16_t*)env->stack_head;
	env->stack_head += 2;
	env->op_id++;
	return false;
}

bool handleOpPush8(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->stack_head--;

	if (env->stack_brk > env->stack_head) {
		env->exitcode = EC_STACK_OVERFLOW;
		env->err_push_size = 1;
		return true;
	}

	if (env->flags & BREX_TRACE_STACK || env->flags & BREX_TRACE_REGS) {
		if (TracerTypeSizes[env->regs_trace[op.src_reg].type] == 1) {
			TracerArray_append(&env->stack_trace, env->regs_trace[op.src_reg]);
		} else {
			TracerArray_append(&env->stack_trace, (Tracer){ .type = TRACER_INT8 });
		}
	}

	*(int8_t*)env->stack_head = (int8_t)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpPop8(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->stack_head + 1 > env->stack_brk + program->stack_size) {
		env->exitcode = EC_STACK_UNDERFLOW;
		env->err_pop_size = 1;
		return true;
	}

	if (env->flags & BREX_TRACE_STACK || env->flags & BREX_TRACE_REGS) {
		if (TracerTypeSizes[arrayhead(env->stack_trace)->type] != 1) {
			env->exitcode = EC_STACK_MISALIGNMENT;
			env->err_pop_size = 1;
			return true;
		}
		env->regs_trace[op.dst_reg] = TracerArray_pop(&env->stack_trace, -1);
	}

	env->registers[op.dst_reg] = *(int8_t*)env->stack_head;
	env->stack_head++;
	env->op_id++;
	return false;
}

bool handleOpAnd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] & op.value;

	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		if (isIntTracer(env->regs_trace[op.src_reg])) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else {
			env->regs_trace[op.dst_reg].type = TRACER_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpAndr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] & env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		bool left_reg_int = isIntTracer(env->regs_trace[op.src_reg]), right_reg_int = isIntTracer(env->regs_trace[op.src2_reg]);
		if (left_reg_int && right_reg_int) {
			if (env->regs_trace[op.src_reg].type > env->regs_trace[op.src2_reg].type) {
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src2_reg];
			} else {
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
			}
		} else if (left_reg_int) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else if (right_reg_int) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src2_reg];
		} else {
			env->regs_trace[op.dst_reg].type = TRACER_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpOr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] | op.value;

	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		if (isIntTracer(env->regs_trace[op.src_reg])) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else {
			env->regs_trace[op.dst_reg].type = TRACER_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpOrr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] | env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		bool left_reg_int = isIntTracer(env->regs_trace[op.src_reg]), right_reg_int = isIntTracer(env->regs_trace[op.src2_reg]);
		if (left_reg_int && right_reg_int) {
			if (env->regs_trace[op.src_reg].type < env->regs_trace[op.src2_reg].type) {
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src2_reg];
			} else {
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
			}
		} else if (left_reg_int) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else if (right_reg_int) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src2_reg];
		} else {
			env->regs_trace[op.dst_reg].type = TRACER_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpNot(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = ~env->registers[op.src_reg];

	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		env->regs_trace[op.dst_reg].type = TRACER_INT64;
	}

	env->op_id++;
	return false;
}

bool handleOpXor(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ op.value;

	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		if (isIntTracer(env->regs_trace[op.src_reg])) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else {
			env->regs_trace[op.dst_reg].type = TRACER_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpXorr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS || env->flags & BREX_TRACE_STACK) {
		bool left_reg_int = isIntTracer(env->regs_trace[op.src_reg]), right_reg_int = isIntTracer(env->regs_trace[op.src2_reg]);
		if (left_reg_int && right_reg_int) {
			if (env->regs_trace[op.src_reg].type < env->regs_trace[op.src2_reg].type) {
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src2_reg];
			} else {
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
			}
		} else if (left_reg_int) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else if (right_reg_int) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src2_reg];
		} else {
			env->regs_trace[op.dst_reg].type = TRACER_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpShl(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] << op.value;

	if (env->flags & BREX_TRACE_REGS) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_DATAPTR:
			case TRACER_MEMPTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = TRACER_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpShlr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] << env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_DATAPTR:
			case TRACER_MEMPTR:
				if (isIntTracer(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = TRACER_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpShr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >> op.value;

	if (env->flags & BREX_TRACE_REGS) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_DATAPTR:
			case TRACER_MEMPTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = TRACER_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpShrr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >> env->registers[op.src2_reg];

	if (env->flags & BREX_TRACE_REGS) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_DATAPTR:
			case TRACER_MEMPTR:
				if (isIntTracer(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = TRACER_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = TRACER_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpCall(ExecEnv* env, Program* program)
{
	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (env->stack_brk > env->stack_head - 16) {
			env->exitcode = EC_STACK_OVERFLOW;
			env->err_push_size = 16;
			return true;
		}

		TracerArray_append(&env->stack_trace, (Tracer){ .type = TRACER_INT64 });
		TracerArray_append(&env->stack_trace, (Tracer){ .type = TRACER_STACKPTR, .is_stackframe = true });
	}

	env->stack_head -= sizeof(env->op_id);
	*(int64_t*)env->stack_head = env->op_id + 1;

	env->stack_head -= sizeof(env->prev_stack_head);
	*(void**)env->stack_head = env->prev_stack_head;
	env->prev_stack_head = env->stack_head;

	env->op_id = program->execblock.data[env->op_id].symbol_id; 
	return false;
}

bool handleOpRet(ExecEnv* env, Program* program)
{
	if (env->flags & (BREX_TRACE_STACK | BREX_TRACE_REGS)) {
		if (env->stack_head + 16 > env->stack_brk + program->stack_size) {
			env->exitcode = EC_STACK_UNDERFLOW;
			env->err_pop_size = 16;
			return true;
		}

		Tracer stackframe = {0};
		int prev_stack_trace_head = -1;
		array_rev_foreach(Tracer, tracer, env->stack_trace, 
			if (tracer.type == TRACER_STACKPTR && tracer.is_stackframe) { 
				stackframe = tracer;
				prev_stack_trace_head = _tracer;
				break;
			}
		);
		if (stackframe.type != TRACER_STACKPTR ? true : TracerArray_pop(&env->stack_trace, prev_stack_trace_head - 1).type != TRACER_INT64) {
			env->exitcode = EC_NO_STACKFRAME;
			return true;
		}
		env->stack_trace.length = prev_stack_trace_head - 1;

	}

	env->stack_head = env->prev_stack_head;
	env->prev_stack_head = *(void**)env->stack_head;
	env->stack_head += sizeof(env->prev_stack_head);

	env->op_id = *(int64_t*)env->stack_head;
	env->stack_head += sizeof(env->op_id);

	return false;
}


BRFFunc op_handlers[] = {
	&handleNop,
	&handleOpEnd,
	&handleOpMark,
	&handleOpSet,
	&handleOpSetr,
	&handleOpSetd,
	&handleOpSetb,
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
	&handleOpGer,
	&handleOpLts,
	&handleOpLtsr,
	&handleOpGts,
	&handleOpGtsr,
	&handleOpLes,
	&handleOpLesr,
	&handleOpGes,
	&handleOpGesr,
	&handleOpPush64,
	&handleOpPop64,
	&handleOpPush32,
	&handleOpPop32,
	&handleOpPush16,
	&handleOpPop16,
	&handleOpPush8,
	&handleOpPop8,
	&handleOpAnd,
	&handleOpAndr,
	&handleOpOr,
	&handleOpOrr,
	&handleOpNot,
	&handleOpXor,
	&handleOpXorr,
	&handleOpShl,
	&handleOpShlr,
	&handleOpShr,
	&handleOpShrr,
	&handleOpCall,
	&handleOpRet
};
static_assert(N_OPS == sizeof(op_handlers) / sizeof(op_handlers[0]), "Some BRF operations have unmatched execution handlers");

ExecEnv execProgram(Program* program, int8_t flags)
{
	ExecEnv env = initExecEnv(program, flags);
	while (!op_handlers[program->execblock.data[env.op_id].type](&env, program)) { if (interrupt) break; }
	return env;
}


void printUsageMsg(FILE* fd, char* exec_name)
{
	fprintf(fd, "brex - Execute and debug .brf (BRidge Executable) files\n");
	fprintf(fd, "usage: %s [options] <file>\n", exec_name);
	fprintf(fd, "options:\n");
	fprintf(fd, "\t-h     Output this message and exit\n");
	fprintf(fd, "\t-r     Trace register values and output them after execution of the program\n");
	fprintf(fd, "\t-s     Trace stack values and output them after execution of the program\n");
}

int main(int argc, char* argv[]) {
	initBREnv();
	char* input_name = NULL;
	int8_t exec_flags = 0;
	bool time_exec = false;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			for (argv[i]++; *argv[i] != '\0'; argv[i]++) {
				switch (*argv[i]) {
					case 'h': printUsageMsg(stdout, argv[0]); return 0;
					case 'r': exec_flags |= BREX_TRACE_REGS; break;
					case 's': exec_flags |= BREX_TRACE_STACK; break;
					default: eprintf("error: unknown option `-%c`\n", *argv[i]); return 1;
				}
			}
		} else {
			if (input_name) {
				eprintf("error: more than 1 input path provided\n");
				return 1;
			}
			input_name = argv[i];
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
		case BRF_ERR_NO_STACK_SIZE:
			eprintf("BRF error: found stack size segment identifier, but no stack size was provided\n");
			return 1;
	}

	signal(SIGINT, &handleExecInt);
	startTimer();
	ExecEnv res = execProgram(&program, exec_flags);
	signal(SIGINT, SIG_DFL);

	printExecState(stdout, &res, &program);

	switch (res.exitcode) {
		case EC_STACK_MISALIGNMENT:
			eprintf("%llx:\n\truntime error: stack misalignment\n", res.op_id);
			eprintf("\tstack head size: %hhd bytes\n", res.err_pop_size);
			eprintf("\tattempted to pop %hhd bytes\n", TracerTypeSizes[arrayhead(res.stack_trace)->type]);
			break;
		case EC_NO_STACKFRAME:
			eprintf("%llx:\n\truntime error: attempted to return from a global stack frame\n", res.op_id);
			break;
		case EC_ACCESS_FAILURE:
			eprintf("%llx:\n\truntime error: memory access failure\n", res.op_id);
			eprintf(
				"\tattempted to access %lld bytes from %p, which is not in bounds of the stack, the heap or any of the buffers\n",
				res.err_access_length,
				res.err_ptr
			);
			break;
		case EC_NEGATIVE_SIZE_ACCESS:
			eprintf("%llx:\n\truntime error: negative sized memory access\n", res.op_id);
			eprintf(
				"\tattempted to access %lld bytes at %p; accessing memory by negative size is not allowed\n",
				res.err_access_length, 
				res.err_ptr
			);
			break;
		case EC_STACK_OVERFLOW:
			eprintf("%llx:\n\truntime error: stack overflow\n", res.op_id);
			eprintf("\tattempted to expand the stack by %hhd bytes, overflowing the stack\n", res.err_push_size);
			break;
		case EC_STACK_UNDERFLOW:
			eprintf("%llx:\n\truntime error: stack underflow\n", res.op_id);
			eprintf("\tattempted to &#%% the stack by %hhd bytes, underflowing the stack\n", res.err_pop_size);
			break;
		case EC_UNKNOWN_SYSCALL:
			eprintf("%llx:\n\truntime error: invalid system call\n", res.op_id - 1);
			break;
		case EC_ACCESS_MISALIGNMENT:
			eprintf("%llx:\n\truntime error: misaligned memory access\n", res.op_id);
			eprintf("\tbuffer `%s` is at %p\n", res.err_buf.name, res.err_buf.spec.data);
			eprintf("\tbuffer size: %ld\n", res.err_buf.spec.length);
			eprintf("\tattempted to access %lld bytes from %p\n", res.err_access_length, res.err_ptr);
			if ((void*)res.err_ptr < (void*)res.err_buf.spec.data) {
				eprintf("\tthe accessed field is %lld bytes before the start of the buffer\n", (int64_t)(res.err_buf.spec.data - (int64_t)res.err_ptr));
			} else if ((void*)(res.err_ptr + res.err_access_length) > (void*)(res.err_buf.spec.data + res.err_buf.spec.length)) {
				eprintf(
					"\tthe accessed field exceeds the buffer by %lld bytes\n", 
					(int64_t)(res.err_ptr + res.err_access_length - ((int64_t)res.err_buf.spec.data + res.err_buf.spec.length))
				);
			}
			break;
	}

	return res.exitcode;
}
