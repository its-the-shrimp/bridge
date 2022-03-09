#include "brb.h"
#include "stdio.h"
#include "errno.h"
#include "unistd.h"
#include "signal.h"
#include "stdlib.h"

#ifdef LIBBRB

defArray(Op);
defArray(DataBlock);
defArray(MemBlock);
defArray(Tracer)

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

int64_t loadInt(sbuf* input)
{
	int8_t size = input->length ? input->data[0] : 0;
	sbufpshift(input, 1);

	int64_t res;
	switch (size) {
		case 0: res = 0; break;
		case 1: res = loadInt8(input); break;
		case 2: res = loadInt16(input); break;
		case 4: res = loadInt32(input); break;
		case 8: res = loadInt64(input); break;
	}
	return res;
}

typedef BRBLoadError (*OpLoader) (sbuf*, Program*, heapctx_t);
 
void printLoadError(BRBLoadError err)
{
	switch (err.code) {
		case BRB_ERR_OK: break;
		case BRB_ERR_NO_MEMORY:
			eprintf("BRB loading error: memory allocation failure\n");
			break;
		case BRB_ERR_NO_ENTRY_SPEC:
			eprintf("BRB loading error: no entry mark specifier found\n");
			break;
		case BRB_ERR_NO_BLOCK_NAME:
			eprintf("BRB loading error: block name not found\n");
			break;
		case BRB_ERR_NO_BLOCK_SIZE:
			eprintf("BRB loading error: block size not found\n");
			break;
		case BRB_ERR_NO_BLOCK_SPEC:
			eprintf("BRB loading error: data block specifier not found\n");
			break;
		case BRB_ERR_NO_OPCODE:
			eprintf("BRB loading error: operation code not found\n");
			break;
		case BRB_ERR_NO_MARK_NAME:
			eprintf("BRB loading error: mark name not found\n");
			break;
		case BRB_ERR_NO_OP_ARG:
			eprintf(
				"BRB loading error: argument for operation `"sbuf_format"` not found\n", 
				unpack(opNames[err.opcode])
			);
			break;
		case BRB_ERR_INVALID_OPCODE:
			eprintf("BRB loading error: invalid operation code %d found\n", err.opcode);
			break;
		case BRB_ERR_UNKNOWN_SEGMENT_SPEC:
			eprintf("BRB loading error: unknown segment specifier \"");
			fputsbufesc(stderr, err.segment_spec, BYTEFMT_HEX);
			eprintf("\"\n");
			break;
		case BRB_ERR_NO_STACK_SIZE:
			eprintf("BRB loading error: found stack size segment identifier, but no stack size was provided\n");
			break;
	}
}

BRBLoadError loadNoArgOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	return (BRBLoadError){0};
}

BRBLoadError loadOpMark(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	sbuf new;
	putsbuflnesc(*input, BYTEFMT_HEX);
	if (!sbufsplit(input, &new, SEP).data) {
		return (BRBLoadError){.code = BRB_ERR_NO_MARK_NAME};
	}
	op->mark_name = tostr(ctx, new);
	return (BRBLoadError){0};
}

BRBLoadError loadRegImmOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(input);
	op->value = loadInt(input);
	return (BRBLoadError){0};
}

BRBLoadError load2RegOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	return (BRBLoadError){0};
}

BRBLoadError loadOpSetd(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt(input);
	return (BRBLoadError){0};
}

BRBLoadError loadOpSetm(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt(input);
	return (BRBLoadError){0};
}

BRBLoadError loadOpSetb(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(input);
	op->symbol_id = loadInt(input);
	return (BRBLoadError){0};
}

BRBLoadError loadOpSyscall(sbuf* input, Program* dst, heapctx_t ctx)
{	
	Op* op = arrayhead(dst->execblock);
	op->syscall_id = loadInt8(input);
	return (BRBLoadError){0};
}

BRBLoadError loadJumpOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->symbol_id = loadInt(input);
	return (BRBLoadError){0};
}

BRBLoadError loadOpCgoto(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->src_reg = loadInt8(input);
	op->symbol_id = loadInt(input);
	return (BRBLoadError){0};
}

BRBLoadError load2RegImmOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->value = loadInt(input);	
	return (BRBLoadError){0};
}

BRBLoadError load3RegOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(input);
	op->src_reg = loadInt8(input);
	op->src2_reg = loadInt8(input);	
	return (BRBLoadError){0};
}

BRBLoadError loadPushOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	arrayhead(dst->execblock)->src_reg = loadInt8(input);
	return (BRBLoadError){0};
}

BRBLoadError loadPopOp(sbuf* input, Program* dst, heapctx_t ctx)
{
	arrayhead(dst->execblock)->dst_reg = loadInt8(input);
	return (BRBLoadError){0};
}

BRBLoadError loadOpAlloc(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->item_type = loadInt8(input);
	op->value = loadInt(input);
	return (BRBLoadError){0};
}

BRBLoadError loadOpAllocr(sbuf* input, Program* dst, heapctx_t ctx)
{
	Op* op = arrayhead(dst->execblock);
	op->item_type = loadInt8(input);
	op->src_reg = loadInt8(input);
	return (BRBLoadError){0};
}

OpLoader op_loaders[] = {
	&loadNoArgOp, // OP_NONE
	&loadNoArgOp, // OP_END
	&loadOpMark,
	&loadRegImmOp, // OP_SET
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
	&load2RegImmOp, // OP_SHRS
	&load3RegOp, // OP_SHRSR
	&loadJumpOp, // OP_CALL
	&loadNoArgOp, // OP_RET
	&load2RegOp, // OP_LD64
	&load2RegOp, // OP_STR64
	&load2RegOp, // OP_LD32
	&load2RegOp, // OP_STR32
	&load2RegOp, // OP_LD16
	&load2RegOp, // OP_STR16
	&load2RegOp, // OP_LD8
	&load2RegOp, // OP_STR8
	&loadRegImmOp, // OP_SETS
	&load2RegOp,  // OP_SETSR
	&loadOpAlloc,
	&loadOpAllocr
};
static_assert(N_OPS == sizeof(op_loaders) / sizeof(op_loaders[0]), "Some BRB operations have unmatched loaders");

BRBLoadError loadProgram(sbuf input, Program* dst, heapctx_t ctx)
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
			if (input.length < 8) return (BRBLoadError){.code = BRB_ERR_NO_ENTRY_SPEC};
			dst->entry_opid = loadInt(&input);
		} else if (sbufcut(&input, STACKSIZE_SEGMENT_START).data) {
			if (input.length < 8) return (BRBLoadError){.code = BRB_ERR_NO_STACK_SIZE};
			dst->stack_size = loadInt(&input);
		} else if (sbufcut(&input, DATA_SEGMENT_START).data) {
			while (true) {
				if (!sbufsplit(&input, &new, SEP).data) return (BRBLoadError){.code = BRB_ERR_NO_BLOCK_NAME};
				if (!new.length) break;

				if (!(datablock = DataBlockArray_append(&dst->datablocks, (DataBlock){0}))) {
					return (BRBLoadError){.code = BRB_ERR_NO_MEMORY};
				}
				datablock->name = tostr(ctx, new);

				if (input.length < 5) return (BRBLoadError){.code = BRB_ERR_NO_BLOCK_SIZE};
				datablock->spec.length = loadInt(&input);

				if (input.length < datablock->spec.length + 1) {
					return (BRBLoadError){.code = BRB_ERR_NO_BLOCK_SPEC};
				}
				datablock->spec.data = input.data;
				sbufshift(input, datablock->spec.length);
			}
		} else if (sbufcut(&input, MEMBLOCK_SEGMENT_START).data) {
			while (true) {
				if (!sbufsplit(&input, &new, SEP).data) return (BRBLoadError){.code = BRB_ERR_NO_BLOCK_NAME};
				if (!new.length) break;

				if (!(memblock = MemBlockArray_append(&dst->memblocks, (MemBlock){0}))) {
					return (BRBLoadError){.code = BRB_ERR_NO_MEMORY};
				}
				memblock->name = tostr(ctx, new);

				if (input.length < 5) return (BRBLoadError){.code = BRB_ERR_NO_BLOCK_SIZE};
				memblock->size = loadInt(&input);
			}
		} else if (sbufcut(&input, EXEC_SEGMENT_START).data) {
			do {
				if (!(op = OpArray_append(&dst->execblock, (Op){0}))) {
					return (BRBLoadError){.code = BRB_ERR_NO_MEMORY};
				}
				if (!input.length) return (BRBLoadError){.code = BRB_ERR_NO_OPCODE};

				op->type = loadInt8(&input);
				if (!inRange(op->type, 0, N_OPS)) {
					return (BRBLoadError){.code = BRB_ERR_INVALID_OPCODE, .opcode = op->type};
				}

				BRBLoadError err = op_loaders[op->type](&input, dst, ctx);
				if (err.code != BRB_ERR_OK) return err;
			} while (op->type != OP_END);
		} else {
			return (BRBLoadError){
				.code = BRB_ERR_UNKNOWN_SEGMENT_SPEC, 
				.segment_spec = sbufslice(input, 0, input.length < 2 ? input.length : 2)
			};
		}
	}

	return (BRBLoadError){.code = BRB_ERR_OK};
}

ExecEnv initExecEnv(Program* program, int8_t flags, char** args)
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

	res.exec_argc = 0;
	while (args[++res.exec_argc]) {}
	res.exec_argv = ctxalloc_new(res.exec_argc * sizeof(sbuf), memctx);
	for (int i = 0; i < res.exec_argc; i++) {
		res.exec_argv[i] = fromstr(args[i]);
		res.exec_argv[i].length++;
	}

	return res;
}

BufferRef classifyPtr(ExecEnv* env, Program* program, void* ptr)
{
	static_assert(N_BUF_TYPES == 5, "not all buffer types are handled");

	if (inRange(ptr, env->stack_head, env->stack_brk + program->stack_size - env->stack_head)) {
		return ((BufferRef){ .type = BUF_STACK });
	}

	array_foreach(DataBlock, block, program->datablocks,
		if (inRange(ptr, block.spec.data, block.spec.data + block.spec.length)) {
			return ((BufferRef){ .type = BUF_DATA, .id = _block });
		}
	);

	array_foreach(sbuf, block, env->memblocks, 
		if (inRange(ptr, block.data, block.data + block.length)) {
			return ((BufferRef){ .type = BUF_MEMORY, .id = _block });
		}
	);

	for (int i = 0; i < env->exec_argc; i++) {
		if (inRange(ptr, env->exec_argv[i].data, env->exec_argv[i].data + env->exec_argv[i].length)) {
			return (BufferRef){ .type = BUF_ARGV, .id = i };
		}
	}
	return (BufferRef){0};
}

sbuf getBufferByRef(ExecEnv* env, Program* program, BufferRef ref)
{
	static_assert(N_BUF_TYPES == 5, "not all buffer types are handled");
	switch (ref.type) {
		case BUF_DATA: return program->datablocks.data[ref.id].spec;
		case BUF_MEMORY: return env->memblocks.data[ref.id];
		case BUF_STACK: return (sbuf){ .data = env->stack_head, .length = env->stack_brk + program->stack_size - env->stack_head };
		case BUF_ARGV: return env->exec_argv[ref.id];
		default: return (sbuf){0};
	}
}

void printTracer(FILE* fd, Program* program, ExecEnv* env, Tracer tracer, int64_t value)
{
	static_assert(N_TRACER_TYPES == 8, "not all tracer types are handled");
	static_assert(N_BUF_TYPES == 5, "not all buffer types are handled");
	switch (tracer.type) {
			case TRACER_VOID: {
				fprintf(fd, "(void)");
				break;
			} case TRACER_BOOL: {
				fprintf(fd, "(bool)%s", value ? "true" : "false");
				break;
			} case TRACER_INT64: {
				fprintf(fd, "(int64_t)%lld", value);
				break;
			} case TRACER_INT32: {
				fprintf(fd, "(int32_t)%lld", value);
				break;
			} case TRACER_INT16: {
				fprintf(fd, "(int16_t)%lld", value);
				break;
			} case TRACER_INT8: {
				fprintf(fd, "(char)'");
				fputcesc(fd, value, BYTEFMT_HEX);
				fprintf(fd, "' // %lld", value);
				break;
			} case TRACER_PTR: {
				if (!tracer.ref.type) {
					tracer.ref = classifyPtr(env, program, (void*)value);
				}
				sbuf buf = getBufferByRef(env, program, tracer.ref);
				switch (tracer.ref.type) {
					case BUF_DATA:
						fprintf(fd, "(void*)%s ", program->datablocks.data[tracer.ref.id].name);
						break;
					case BUF_MEMORY:
						fprintf(fd, "(void*)%s ", program->memblocks.data[tracer.ref.id].name);
						break;
					case BUF_STACK:
						fprintf(fd, "(stackptr_t)sp ");
						break;
					case BUF_ARGV:
						fprintf(fd, "(char*)argv[%d] ", tracer.ref.id);
						break;
					default:
						fprintf(fd, "(void*)%p\n", (void*)value);
						return;
				}

				if ((uint64_t)value < (uint64_t)buf.data) {
					fprintf(fd, "- %llu ", (uint64_t)buf.data - (uint64_t)value);
				} else if ((uint64_t)value > (uint64_t)buf.data) {
					fprintf(fd, "+ %llu ", (uint64_t)value - (uint64_t)buf.data);
				}
				fprintf(fd, "// %p", (void*)value);
				break;
			} case TRACER_CONST: {
				fprintf(fd, "(const int)%s // %lld", consts[tracer.symbol_id].name, value);
				break;
			}
	}
	fprintf(fd, tracer.is_stackframe ? " [stack frame]\n" : "\n");
}

void printExecState(FILE* fd, ExecEnv* env, Program* program)
{
	if (env->flags & BREX_TRACE_STACK) {
		fprintf(fd, "stack:\n");
		void* cur_stack_pos = env->stack_brk + program->stack_size;
		static_assert(N_TRACER_TYPES == 8, "not all tracer types are handled");

		array_foreach(Tracer, tracer, env->stack_trace,
			fprintf(fd, "\t[%d]\t", env->stack_trace.length - _tracer - 1);
			int64_t input = 0;
			switch (tracer.type) {
				case TRACER_INT64:
				case TRACER_PTR:
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
		printf("\t[end]\n");
		fprintf(fd, "total stack usage: %.3f Kb\n", (float)(env->stack_brk + program->stack_size - env->stack_head) / 1024.0f);
	}

	if (env->flags & BREX_TRACE_REGS) {
		fprintf(fd, "registers:\n");
		for (char i = 0; i < N_REGISTERS; i++) {
			fprintf(fd, "\t[%hhd]\t", i);
			printTracer(fd, program, env, env->regs_trace[i], env->registers[i]);
		}
	}

	if (env->flags & BREX_PRINT_MEMBLOCKS) {
		printf("memory blocks:\n");
		array_foreach(sbuf, block, env->memblocks, 
			printf("\t%s: \"", program->memblocks.data[_block].name);
			putsbufesc(block, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE);
			printf("\"\n");
		);
	}
}

bool validateMemoryAccess(ExecEnv* env, Program* program, Tracer tracer, void* ptr, int64_t size)
{
	static_assert(N_BUF_TYPES == 5, "not all buffer types are handled");

	if (size < 0) {
		env->exitcode = EC_NEGATIVE_SIZE_ACCESS;
		env->err_ptr = ptr,
		env->err_access_length = size;
		return true;
	}

	BufferRef ref;
	if (tracer.type == TRACER_PTR) {
		ref = tracer.ref;
	} else {
		ref = classifyPtr(env, program, ptr);
	}

	if (!ref.type) {
		env->exitcode = EC_ACCESS_FAILURE;
		env->err_access_length = size;
		env->err_ptr = ptr;
		return true;
	}

	sbuf buf = getBufferByRef(env, program, ref);
	if (!isSlice(ptr, size, buf.data, buf.length)) {
		env->exitcode = EC_ACCESS_MISALIGNMENT;
		env->err_buf_ref = ref;
		env->err_access_length = size;
		env->err_ptr = ptr;
		return true;
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
	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK) && env->flags & BREX_CHECK_SYSCALLS) {
		if (validateMemoryAccess(env, program, env->regs_trace[1], (char*)env->registers[1], env->registers[2])) { return true; }
	}

	env->registers[0] = write(env->registers[0], (char*)env->registers[1], env->registers[2]);

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[0].type = TRACER_INT64;
	}

	env->op_id++;
	return false;
}

bool handleArgcSyscall(ExecEnv* env, Program* program)
{
	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[0].type = TRACER_INT32;
	}

	env->registers[0] = env->exec_argc;
	env->op_id++;
	return false;
}

bool handleArgvSyscall(ExecEnv* env, Program* program)
{
	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (env->flags & BREX_CHECK_SYSCALLS ? !inRange(env->registers[0], 0, env->exec_argc) : false) {
			env->exitcode = EC_INVALID_ARG_ID;
			return true;
		}
		env->regs_trace[0] = (Tracer){ 
			.type = TRACER_PTR, 
			.ref = (BufferRef){ 
				.type = BUF_ARGV, 
				.id = env->registers[0] 
			} 
		};
	}

	env->registers[0] = inRange(env->registers[0], 0, env->exec_argc) ? (uint64_t)env->exec_argv[env->registers[0]].data : 0;
	env->op_id++;
	return false;
}

typedef bool (*ExecHandler) (ExecEnv*, Program*);

ExecHandler syscall_handlers[] = {
	&handleInvalidSyscall,
	&handleExitSyscall,
	&handleWriteSyscall,
	&handleArgcSyscall,
	&handleArgvSyscall
};
static_assert(N_SYS_OPS == sizeof(syscall_handlers) / sizeof(syscall_handlers[0]), "not all system calls have matching handlers");

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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_INT64;
	}

	env->op_id++;
	return false;
}

bool handleOpSetr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
	}

	env->op_id++;
	return false;
}

bool handleOpSetd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)program->datablocks.data[op.symbol_id].spec.data;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg] = (Tracer){ 
			.type = TRACER_PTR,
			.ref = (BufferRef){ 
				.type = BUF_DATA,
				.id = op.symbol_id
			}
		};
	}

	env->op_id++;
	return false;
}

bool handleOpSetb(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = consts[op.symbol_id].value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg] = (Tracer){ .type = TRACER_CONST, .symbol_id = op.symbol_id };
	}

	env->op_id++;
	return false;
}

bool handleOpSetm(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->memblocks.data[op.symbol_id].data;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg] = (Tracer){ 
			.type = TRACER_PTR,
			.ref = (BufferRef){
				.type = BUF_MEMORY,
				.id = op.symbol_id
			}
		};
	}

	env->op_id++;
	return false;
}

bool handleOpAdd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpEqr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] == env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpNeq(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] != op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpNeqr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] != env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLt(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] < (uint64_t)op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLtr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] < env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGt(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] > (uint64_t)op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGtr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] > env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLe(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] <= (uint64_t)op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLer(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] <= env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGe(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >= (uint64_t)op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGer(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >= env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLts(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] < op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLtsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] < (int64_t)env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGts(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] > op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGtsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] > (int64_t)env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLes(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] <= op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLesr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] <= (int64_t)env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGes(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >= op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGesr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >= (int64_t)env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg].type = TRACER_INT64;
	}

	env->op_id++;
	return false;
}

bool handleOpXor(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

bool handleOpShrs(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >> op.value;

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

bool handleOpShrsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >> env->registers[op.src2_reg];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		switch (env->regs_trace[op.src_reg].type) {
			case TRACER_PTR:
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

		TracerArray_append(&env->stack_trace, (Tracer){ .type = TRACER_INT64, .is_stackframe = true });
		TracerArray_append(
			&env->stack_trace, 
			(Tracer){ 
				.type = TRACER_PTR, 
				.ref = (BufferRef){ .type = BUF_STACK },
				.is_stackframe = true
			}
		);
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
		for (int i = env->stack_trace.length - 1; i >= 0; i--) {
			if (env->stack_trace.data[i].ref.type == BUF_STACK && env->stack_trace.data[i].is_stackframe) {
				stackframe = env->stack_trace.data[i];
				prev_stack_trace_head = i;
				break;
			}
		}
		if (!stackframe.is_stackframe) {
			env->exitcode = EC_NO_STACKFRAME;
			return true;
		}
		env->stack_trace.length = prev_stack_trace_head;
		if (!arrayhead(env->stack_trace)->is_stackframe) {
			env->exitcode = EC_NO_STACKFRAME;
			return true;
		}
		env->stack_trace.length--;
	}

	env->stack_head = env->prev_stack_head;
	env->prev_stack_head = *(void**)env->stack_head;
	env->stack_head += sizeof(env->prev_stack_head);

	env->op_id = *(int64_t*)env->stack_head;
	env->stack_head += sizeof(env->op_id);

	return false;
}

bool handleOpLd64(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (validateMemoryAccess(env, program, env->regs_trace[op.src_reg], (void*)env->registers[op.src_reg], 8)) { return true; }
		env->regs_trace[op.dst_reg].type = TRACER_INT64;
	}

	env->registers[op.dst_reg] = *(int64_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr64(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (validateMemoryAccess(env, program, env->regs_trace[op.dst_reg], (void*)env->registers[op.dst_reg], 8)) { return true; }
	}

	*(int64_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd32(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (validateMemoryAccess(env, program, env->regs_trace[op.src_reg], (void*)env->registers[op.src_reg], 4)) { return true; }
		env->regs_trace[op.dst_reg].type = TRACER_INT32;
	}

	env->registers[op.dst_reg] = *(int32_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr32(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (validateMemoryAccess(env, program, env->regs_trace[op.dst_reg], (void*)env->registers[op.dst_reg], 4)) { return true; }
	}

	*(int32_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd16(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (validateMemoryAccess(env, program, env->regs_trace[op.src_reg], (void*)env->registers[op.src_reg], 2)) { return true; }
		env->regs_trace[op.dst_reg].type = TRACER_INT16;
	}

	env->registers[op.dst_reg] = *(int16_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr16(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (validateMemoryAccess(env, program, env->regs_trace[op.dst_reg], (void*)env->registers[op.dst_reg], 2)) { return true; }
	}

	*(int16_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd8(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (validateMemoryAccess(env, program, env->regs_trace[op.src_reg], (void*)env->registers[op.src_reg], 1)) { return true; }
		env->regs_trace[op.dst_reg].type = TRACER_INT8;
	}

	env->registers[op.dst_reg] = *(int8_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr8(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		if (validateMemoryAccess(env, program, env->regs_trace[op.dst_reg], (void*)env->registers[op.dst_reg], 1)) { return true; }
	}

	*(int8_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpSets(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg] = (Tracer){ 
			.type = TRACER_PTR, 
			.ref = (BufferRef){ .type = BUF_STACK } 
		};
	}

	env->registers[op.dst_reg] = (uint64_t)env->stack_head + op.value;

	env->op_id++;
	return false;
}

bool handleOpSetsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & (BREX_TRACE_REGS | BREX_TRACE_STACK)) {
		env->regs_trace[op.dst_reg] = (Tracer){ 
			.type = TRACER_PTR, 
			.ref = (BufferRef){ .type = BUF_STACK } 
		};
	}

	env->registers[op.dst_reg] = (uint64_t)env->stack_head + env->registers[op.src_reg];

	env->op_id++;
	return false;
}

bool handleOpAlloc(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	if (env->flags & (BREX_TRACE_STACK | BREX_TRACE_REGS)) {
		if (env->stack_head - env->stack_brk < TracerTypeSizes[op.item_type] * op.value) {
			env->exitcode = EC_STACK_OVERFLOW;
			env->err_push_size = TracerTypeSizes[op.item_type] * op.value;
			return true;
		}
		for (int i = 0; i < op.value; i++) {
			TracerArray_append(&env->stack_trace, (Tracer){ .type = op.item_type });
		}
	}

	env->stack_head -= TracerTypeSizes[op.item_type] * op.value;

	env->op_id++;
	return false;
}

bool handleOpAllocr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	if (env->flags & (BREX_TRACE_STACK | BREX_TRACE_REGS)) {
		if (env->stack_head - env->stack_brk < TracerTypeSizes[op.item_type] * env->registers[op.src_reg]) {
			env->exitcode = EC_STACK_OVERFLOW;
			env->err_push_size = TracerTypeSizes[op.item_type] * env->registers[op.src_reg];
			return true;
		}
		for (int i = 0; i < env->registers[op.src_reg]; i++) {
			TracerArray_append(&env->stack_trace, (Tracer){ .type = op.item_type });
		}
	}

	env->stack_head -= TracerTypeSizes[op.item_type] * env->registers[op.src_reg];

	env->op_id++;
	return false;
}

ExecHandler op_handlers[] = {
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
	&handleOpShrs,
	&handleOpShrsr,
	&handleOpCall,
	&handleOpRet,
	&handleOpLd64,
	&handleOpStr64,
	&handleOpLd32,
	&handleOpStr32,
	&handleOpLd16,
	&handleOpStr16,
	&handleOpLd8,
	&handleOpStr8,
	&handleOpSets,
	&handleOpSetsr,
	&handleOpAlloc,
	&handleOpAllocr
};
static_assert(N_OPS == sizeof(op_handlers) / sizeof(op_handlers[0]), "Some BRB operations have unmatched execution handlers");

void printRuntimeError(FILE* fd, ExecEnv* env, Program* program)
{
	switch (env->exitcode) {
		case EC_STACK_MISALIGNMENT:
			fprintf(fd, "%llx:\n\truntime error: stack misalignment\n", env->op_id);
			fprintf(fd, "\tstack head size: %hhd bytes\n", TracerTypeSizes[arrayhead(env->stack_trace)->type]);
			fprintf(fd,"\tattempted to pop %hhd bytes\n", env->err_pop_size);
			break;
		case EC_NO_STACKFRAME:
			fprintf(fd,"%llx:\n\truntime error: attempted to return from a global stack frame\n", env->op_id);
			break;
		case EC_INVALID_ARG_ID:
			fprintf(fd,"%llx:\n\truntime error: attempted to fetch program argument of invalid ID\n", env->op_id);
			fprintf(fd,"\tamount of arguments provided: %d\n", env->exec_argc);
			fprintf(fd,"\tattempted to get address of argument #%lld, which is out of bounds for the array of arguments\n", env->registers[0]);
			break;
		case EC_ACCESS_FAILURE:
			fprintf(fd,"%llx:\n\truntime error: memory access failure\n", env->op_id);
			fprintf(fd,
				"\tattempted to access %lld bytes from %p, which is not in bounds of the stack, the heap or any of the buffers\n",
				env->err_access_length,
				env->err_ptr
			);
			break;
		case EC_NEGATIVE_SIZE_ACCESS:
			fprintf(fd,"%llx:\n\truntime error: negative sized memory access\n", env->op_id);
			fprintf(fd,
				"\tattempted to access %lld bytes at %p; accessing memory by negative size is not allowed\n",
				env->err_access_length, 
				env->err_ptr
			);
			break;
		case EC_STACK_OVERFLOW:
			fprintf(fd,"%llx:\n\truntime error: stack overflow\n", env->op_id);
			fprintf(fd,"\tattempted to expand the stack by %hhd bytes, overflowing the stack\n", env->err_push_size);
			break;
		case EC_STACK_UNDERFLOW:
			fprintf(fd,"%llx:\n\truntime error: stack underflow\n", env->op_id);
			fprintf(fd,"\tattempted to decrease the stack by %hhd bytes, underflowing the stack\n", env->err_pop_size);
			break;
		case EC_UNKNOWN_SYSCALL:
			fprintf(fd,"%llx:\n\truntime error: invalid system call\n", env->op_id - 1);
			break;
		case EC_ACCESS_MISALIGNMENT:
			fprintf(fd,"%llx:\n\truntime error: misaligned memory access\n", env->op_id);
			sbuf err_buf = getBufferByRef(env, program, env->err_buf_ref);
			static_assert(N_BUF_TYPES == 5, "not all buffer types are handled");
			switch (env->err_buf_ref.type) {
				case BUF_DATA:
					fprintf(fd,
						"\tdata block `%s` is at %p\n\tblock size: %ld bytes\n",
						program->datablocks.data[env->err_buf_ref.id].name,
						(void*)err_buf.data,
						err_buf.length
					);
					break;
				case BUF_MEMORY:
					fprintf(fd,
						"\tmemory block `%s` is at %p\n\tblock size: %ld bytes\n",
						program->memblocks.data[env->err_buf_ref.id].name,
						(void*)err_buf.data,
						err_buf.length
					);
					break;
				case BUF_STACK:
					fprintf(fd,"\tstack head is at %p\n\tstack size: %ld bytes\n", err_buf.data, err_buf.length);
					break;
				case BUF_ARGV:
					fprintf(fd,"\targument #%d is at %p\n\targument length: %ld bytes\n", env->err_buf_ref.id, err_buf.data, err_buf.length);
					break;
				default: break;
			}
			fprintf(fd,"\tattempted to access %lld bytes from %p\n", env->err_access_length, env->err_ptr);
			if (env->err_ptr < (void*)err_buf.data) {
				fprintf(fd,"\tthe accessed field is %lld bytes before the start of the buffer\n", (int64_t)(err_buf.data - (int64_t)env->err_ptr));
			} else if ((void*)(env->err_ptr + env->err_access_length) > (void*)(err_buf.data + err_buf.length)) {
				fprintf(fd,
					"\tthe accessed field exceeds the buffer by %lld bytes\n", 
					(int64_t)(env->err_ptr + env->err_access_length - ((int64_t)err_buf.data + err_buf.length))
				);
			}
			break;
	}
}

ExecEnv execProgram(Program* program, int8_t flags, char** args, volatile bool* interruptor)
{
	ExecEnv env = initExecEnv(program, flags, args);
	if (interruptor) {
		while (!op_handlers[program->execblock.data[env.op_id].type](&env, program)) {
			if (*interruptor) break; 
		}
	} else {
		while (!op_handlers[program->execblock.data[env.op_id].type](&env, program)) { }
	}
	return env;
}

#else // LIBBRB

bool interrupt = false;

void handleExecInt(int sig)
{
	interrupt = true;
}

void printUsageMsg(FILE* fd, char* exec_name)
{
	fprintf(fd, "brex - Execute and debug .brb (BRidge Executable) files\n");
	fprintf(fd, "usage: %s [-chmrs] <file> [program-args...]\n", exec_name);
	fprintf(fd, "options:\n");
	fprintf(fd, "\t-h     Output this message and exit\n");
	fprintf(fd, "\t-r     Trace register values and output them after execution of the program\n");
	fprintf(fd, "\t-s     Trace stack values and output them after execution of the program\n");
	fprintf(fd, "\t-m     Output contents of memory blocks after execution of the program\n");
	fprintf(fd, "\t-c     Stop execution if any system call fails and report an error. Without `-r` or `-s` flags, this flag does nothing\n");
}

int main(int argc, char* argv[]) {
	initBREnv();
	char *input_name = NULL, **program_argv = NULL;
	uint8_t exec_flags = 0;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			for (argv[i]++; *argv[i]; argv[i]++) {
				switch (*argv[i]) {
					case 'h': printUsageMsg(stdout, argv[0]); return 0;
					case 'r': exec_flags |= BREX_TRACE_REGS; break;
					case 's': exec_flags |= BREX_TRACE_STACK; break;
					case 'm': exec_flags |= BREX_PRINT_MEMBLOCKS; break;
					case 'c': exec_flags |= BREX_CHECK_SYSCALLS; break;
					default: eprintf("error: unknown option `-%c`\n", *argv[i]); return 1;
				}
			}
		} else {
			input_name = argv[i];
			program_argv = argv + i;
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
	sbuf input = filecontent(input_fd, GLOBAL_CTX);
	if (!input.data) {
		fprintf(stderr, "error: could not fetch contents of file `%s`\n", input_name);
		return 1;
	}
	fclose(input_fd);

	Program program;
	BRBLoadError err = loadProgram(input, &program, GLOBAL_CTX);
	if (err.code) {
		printLoadError(err);
		return 1;
	}

	signal(SIGINT, &handleExecInt);
	ExecEnv res = execProgram(&program, exec_flags, program_argv, &interrupt);
	signal(SIGINT, SIG_DFL);

	printExecState(stdout, &res, &program);
	printRuntimeError(stderr, &res, &program);

	return res.exitcode;
}
#endif // LIBBRB