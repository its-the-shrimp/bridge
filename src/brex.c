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
defArray(DataSpec)
defArray(ProcFrame);

int8_t loadInt8(FILE* fd, long* n_fetched)
{
	if (feof(fd) || ferror(fd)) { *n_fetched = 0; return 0; }
	*n_fetched = 1;
	return fgetc(fd);
}

int16_t loadInt16(FILE* fd, long* n_fetched)
{
	char res[2];
	if (fread(&res, 2, 1, fd) != 2) { *n_fetched = 0; return 0; };
	*n_fetched = 2;
	return *(int16_t*)BRByteOrder(&res, 2); 
}

int32_t loadInt32(FILE* fd, long* n_fetched)
{
	char res[4];
	if (fread(&res, 4, 1, fd) != 4) { *n_fetched = 0; return 0; };
	*n_fetched = 4;
	return *(int32_t*)BRByteOrder(&res, 4);
}

int64_t loadInt64(FILE* fd, long* n_fetched)
{
	char res[8];
	if (fread(&res, 8, 1, fd) != 8) { *n_fetched = 0; return 0; }
	*n_fetched = 8;
	return *(int64_t*)BRByteOrder(&res, 8);
}

int64_t loadInt(FILE* fd, long* n_fetched)
{
	if (feof(fd) || ferror(fd)) { *n_fetched = 0; return 0; }
	int8_t size = fgetc(fd);
	int64_t res;

	switch (size) {
		case 0: res = 0; break;
		case 1: res = loadInt8(fd, n_fetched); break;
		case 2: res = loadInt16(fd, n_fetched); break;
		case 4: res = loadInt32(fd, n_fetched); break;
		case 8: res = loadInt64(fd, n_fetched); break;
	}
	if (n_fetched) { *n_fetched = *n_fetched + 1; }
	return res;
}

char* loadName(FILE* fd, heapctx_t ctx, long* n_fetched)
{
	long size = 0;
	char c, temp[256];
	while (!feof(fd) && !ferror(fd)) {
		c = fgetc(fd);
		if (c == ':') { 
			temp[size] = '\0'; 
			size++; 
			break;
		}
		temp[size] = c;
		size++;
	}

	if (c != ':') { return NULL; }
	char* res = ctxalloc_new(size, ctx);
	memcpy(res, temp, size);

	*n_fetched = size;
	return res;
}

typedef BRBLoadError (*OpLoader) (FILE*, Program*, heapctx_t);
 
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

BRBLoadError loadNoArgOp(FILE* fd, Program* dst, heapctx_t ctx)
{
	return (BRBLoadError){0};
}

BRBLoadError loadMarkOp(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	if (!(arrayhead(dst->execblock)->mark_name = loadName(fd, ctx, &status))) {
		return (BRBLoadError){.code = BRB_ERR_NO_MARK_NAME};
	}
	return (BRBLoadError){0};
}

BRBLoadError loadRegImmOp(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(fd, &status);
	op->value = loadInt(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

BRBLoadError load2RegOp(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(fd, &status);
	op->src_reg = loadInt8(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

BRBLoadError loadRegSymbolIdOp(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(fd, &status);
	op->symbol_id = loadInt(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

BRBLoadError loadOpSyscall(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->syscall_id = loadInt8(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

BRBLoadError loadJumpOp(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->symbol_id = loadInt(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

BRBLoadError loadOpCgoto(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->src_reg = loadInt8(fd, &status);
	op->symbol_id = loadInt(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

BRBLoadError load2RegImmOp(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(fd, &status);
	op->src_reg = loadInt8(fd, &status);
	op->value = loadInt(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

BRBLoadError load3RegOp(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->dst_reg = loadInt8(fd, &status);
	op->src_reg = loadInt8(fd, &status);
	op->src2_reg = loadInt8(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

BRBLoadError loadOpVar(FILE* fd, Program* dst, heapctx_t ctx)
{
	long status = 0;
	Op* op = arrayhead(dst->execblock);
	op->var_size = loadInt8(fd, &status);

	if (!status) { return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG }; }
	return (BRBLoadError){0};
}

OpLoader op_loaders[] = {
	&loadNoArgOp, // OP_NONE
	&loadNoArgOp, // OP_END
	&loadMarkOp, // OP_MARK
	&loadRegImmOp, // OP_SET
	&load2RegOp, // OP_SETR
	&loadRegSymbolIdOp, // OP_SETD
	&loadRegSymbolIdOp, // OP_SETB
	&loadRegSymbolIdOp, // OP_SETM
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
	&loadMarkOp, // OP_PROC
	&loadJumpOp, // OP_CALL
	&loadNoArgOp, // OP_RET
	&loadNoArgOp, // OP_ENDPROC
	&load2RegOp, // OP_LD64
	&load2RegOp, // OP_STR64
	&load2RegOp, // OP_LD32
	&load2RegOp, // OP_STR32
	&load2RegOp, // OP_LD16
	&load2RegOp, // OP_STR16
	&load2RegOp, // OP_LD8
	&load2RegOp, // OP_STR8
	&loadOpVar,
	&loadRegSymbolIdOp,  // OP_SETV
	&load2RegImmOp, // OP_MUL
	&load3RegOp, // OP_MULR
	&load2RegImmOp, // OP_DIV
	&load3RegOp, // OP_DIVR
	&load2RegImmOp, // OP_DIVS
	&load3RegOp // OP_DIVSR
};
static_assert(N_OPS == sizeof(op_loaders) / sizeof(op_loaders[0]), "Some BRB operations have unmatched loaders");

BRBLoadError loadProgram(FILE* fd, Program* dst, heapctx_t ctx)
{
	dst->execblock = OpArray_new(ctx, -1),
	dst->memblocks = MemBlockArray_new(ctx, 0),
	dst->datablocks = DataBlockArray_new(ctx, 0),
	dst->entry_opid = 0;
	dst->stack_size = DEFAULT_STACK_SIZE;

	DataBlock* datablock;
	MemBlock* memblock;
	Op* op;
	char* name;
	long n_fetched;

	while (!ferror(fd) && !feof(fd)) {
		char segment_spec_data[2] = {0};
		sbuf segment_spec = { .data = segment_spec_data, .length = 2 };
		if (fread(segment_spec_data, 1, 2, fd) != 2) { break; }

		if (sbufeq(segment_spec, ENTRYSPEC_SEGMENT_START)) {
			dst->entry_opid = loadInt(fd, &n_fetched);
			if (!n_fetched) { return (BRBLoadError){ .code = BRB_ERR_NO_ENTRY_SPEC }; }
		} else if (sbufeq(segment_spec, STACKSIZE_SEGMENT_START)) {
			dst->stack_size = loadInt(fd, &n_fetched);
			if (!n_fetched) { return (BRBLoadError){ .code = BRB_ERR_NO_STACK_SIZE }; }
		} else if (sbufeq(segment_spec, DATA_SEGMENT_START)) {
			while (true) {
				if (!(datablock = DataBlockArray_append(&dst->datablocks, (DataBlock){0}))) {
					return (BRBLoadError){.code = BRB_ERR_NO_MEMORY};
				}

				if (!(datablock->name = loadName(fd, ctx, &n_fetched))) return (BRBLoadError){.code = BRB_ERR_NO_BLOCK_NAME};
				if (n_fetched == 1) { dst->datablocks.length--; break; }

				datablock->spec.length = loadInt(fd, &n_fetched);
				if (!n_fetched) { return (BRBLoadError){ .code = BRB_ERR_NO_BLOCK_SIZE }; }

				datablock->spec.data = ctxalloc_new(datablock->spec.length, ctx);
				if (fread(datablock->spec.data, 1, datablock->spec.length, fd) != datablock->spec.length) {
					return (BRBLoadError){.code = BRB_ERR_NO_BLOCK_SPEC};
				}
			}
		} else if (sbufeq(segment_spec, MEMBLOCK_SEGMENT_START)) {
			while (true) {
				if (!(memblock = MemBlockArray_append(&dst->memblocks, (MemBlock){0}))) {
					return (BRBLoadError){.code = BRB_ERR_NO_MEMORY};
				}

				if (!(memblock->name = loadName(fd, ctx, &n_fetched))) return (BRBLoadError){.code = BRB_ERR_NO_BLOCK_NAME};
				if (n_fetched == 1) { dst->datablocks.length--; break; }

				memblock->size = loadInt(fd, &n_fetched);
				if (!n_fetched) { return (BRBLoadError){ .code = BRB_ERR_NO_BLOCK_SIZE }; }
			}
		} else if (sbufeq(segment_spec, EXEC_SEGMENT_START)) {
			do {
				if (!(op = OpArray_append(&dst->execblock, (Op){0}))) {
					return (BRBLoadError){.code = BRB_ERR_NO_MEMORY};
				}
				
				op->type = loadInt8(fd, &n_fetched);
				if (!n_fetched) { return (BRBLoadError){ .code = BRB_ERR_NO_OPCODE }; }

				if (!inRange(op->type, 0, N_OPS)) {
					return (BRBLoadError){.code = BRB_ERR_INVALID_OPCODE, .opcode = op->type};
				}

				BRBLoadError err = op_loaders[op->type](fd, dst, ctx);
				if (err.code != BRB_ERR_OK) return err;
			} while (op->type != OP_END);
		} else {
			return (BRBLoadError){
				.code = BRB_ERR_UNKNOWN_SEGMENT_SPEC, 
				.segment_spec = sbufcopy(segment_spec, ctx)
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
		.call_count = 0
	};
	res.prev_stack_head = res.stack_head = res.stack_brk + program->stack_size;
	memset(res.registers, 0, sizeof(int64_t) * N_REGISTERS);

	sbuf* newblock;
	array_foreach(MemBlock, block, program->memblocks,
		newblock = sbufArray_append(&res.memblocks, sctxalloc_new(block.size, memctx));
		memset(newblock->data, 0, newblock->length);
	);

	if (flags & BREX_TRACING) {
		res.regs_trace = ctxalloc_new(sizeof(DataSpec) * N_REGISTERS, memctx);
		for (int i = 0; i < N_REGISTERS; i++) {
			res.regs_trace[i].type = DS_VOID;
		}
		res.vars = ProcFrameArray_new(
			memctx, 1, 
			(ProcFrame){
				.call_id = 0,
				.prev_opid = 0,
				.vars = DataSpecArray_new(memctx, 0) 
			}
		);
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
		return (BufferRef){ .type = BUF_VAR };
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
		case BUF_VAR: return (sbuf){ .data = env->stack_head, .length = env->stack_brk + program->stack_size - env->stack_head };
		case BUF_ARGV: return env->exec_argv[ref.id];
		default: return (sbuf){0};
	}
}

void printDataSpec(FILE* fd, Program* program, ExecEnv* env, DataSpec spec, int64_t value)
{
	static_assert(N_DS_TYPES == 8, "not all data types are handled");
	static_assert(N_BUF_TYPES == 5, "not all buffer types are handled");
	switch (spec.type) {
		case DS_VOID: {
			fprintf(fd, "(void)\n");
			break;
		} case DS_BOOL: {
			fprintf(fd, "(bool)%s\n", value ? "true" : "false");
			break;
		} case DS_INT64: {
			fprintf(fd, "(int64_t)%lld\n", value);
			break;
		} case DS_INT32: {
			fprintf(fd, "(int32_t)%lld\n", value);
			break;
		} case DS_INT16: {
			fprintf(fd, "(int16_t)%lld\n", value);
			break;
		} case DS_INT8: {
			fprintf(fd, "(char)'");
			fputcesc(fd, value, BYTEFMT_HEX);
			fprintf(fd, "' // %lld\n", value);
			break;
		} case DS_PTR: {
			if (!spec.ref.type) {
				spec.ref = classifyPtr(env, program, (void*)value);
			}
			sbuf buf = getBufferByRef(env, program, spec.ref);
			switch (spec.ref.type) {
				case BUF_DATA:
					fprintf(fd, "(void*)%s ", program->datablocks.data[spec.ref.id].name);
					break;
				case BUF_MEMORY:
					fprintf(fd, "(void*)%s ", program->memblocks.data[spec.ref.id].name);
					break;
				case BUF_VAR:
					fprintf(fd, "(stackptr_t)sp ");
					break;
				case BUF_ARGV:
					fprintf(fd, "(char*)argv[%d] ", spec.ref.id);
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
			fprintf(fd, "// %p\n", (void*)value);
			break;
		} case DS_CONST: {
			fprintf(fd, "(const int)%s // %lld\n", consts[spec.symbol_id].name, value);
			break;
		}
	}
}

void printExecState(FILE* fd, ExecEnv* env, Program* program)
{
	if (env->flags & BREX_TRACING) {
		fprintf(fd, "local stack frame:\n");
		void* cur_stack_pos = env->prev_stack_head;
		static_assert(N_DS_TYPES == 8, "not all tracer types are handled");

		array_foreach(DataSpec, spec, arrayhead(env->vars)->vars,
			int64_t input = 0;
			switch (spec.type) {
				case DS_VOID:
					cur_stack_pos -= spec.size;
					break;
				case DS_INT64:
				case DS_PTR:
					input = *(int64_t*)(cur_stack_pos -= 8);
					break;
				case DS_INT32:
				case DS_CONST:
					input = (int64_t)*(int32_t*)(cur_stack_pos -= 4);
					break;
				case DS_INT16:
					cur_stack_pos -= 2;
					input = (int64_t)*(int16_t*)(cur_stack_pos -= 2);
					break;
				case DS_INT8:
					input = (int64_t)*(int8_t*)(--cur_stack_pos);
					break;
			}
			fprintf(fd, "\t[sp + %ld] ", cur_stack_pos - env->stack_head);
			printDataSpec(fd, program, env, spec, input);
		);
		printf("\t[end]\n");
		fprintf(
			fd, 
			"total stack usage: %.3f/%.3f Kb (%.3f%%)\n",
			(float)(env->stack_brk + program->stack_size - env->stack_head) / 1024.0f,
			DEFAULT_STACK_SIZE / 1024.0f,
			(float)(env->stack_brk + program->stack_size - env->stack_head)	/ (float)DEFAULT_STACK_SIZE * 100.0f
		);

		fprintf(fd, "registers:\n");
		for (char i = 0; i < N_REGISTERS; i++) {
			fprintf(fd, "\t[%hhd]\t", i);
			printDataSpec(fd, program, env, env->regs_trace[i], env->registers[i]);
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
}

BufferRef validateMemoryAccess(ExecEnv* env, Program* program, DataSpec spec, void* ptr, int64_t size)
{
	static_assert(N_BUF_TYPES == 5, "not all buffer types are handled");

	if (size < 0) {
		env->exitcode = EC_NEGATIVE_SIZE_ACCESS;
		env->err_ptr = ptr,
		env->err_access_length = size;
		return (BufferRef){0};
	}

	BufferRef ref;
	if (spec.type == DS_PTR) {
		ref = spec.ref;
	} else {
		ref = classifyPtr(env, program, ptr);
	}

	if (!ref.type) {
		env->exitcode = EC_ACCESS_FAILURE;
		env->err_access_length = size;
		env->err_ptr = ptr;
		return (BufferRef){0};
	}

	if (ref.type == BUF_VAR && ref.id != env->call_count) {
		env->exitcode = EC_OUTDATED_LOCALPTR;
		env->err_ptr = ptr;
		return (BufferRef){0};
	}

	sbuf buf = getBufferByRef(env, program, ref);
	if (!isSlice(ptr, size, buf.data, buf.length)) {
		env->exitcode = EC_ACCESS_MISALIGNMENT;
		env->err_buf_ref = ref;
		env->err_access_length = size;
		env->err_ptr = ptr;
		return (BufferRef){0};
	}

	return ref;
}

DataSpec getStackSpec(ExecEnv* env, void* ptr, int size)
{
	void* cur_stack_pos = env->stack_head;
	array_rev_foreach(DataSpec, spec, arrayhead(env->vars)->vars,
		int spec_size = dataSpecSize(spec);
		if (inRange(ptr, cur_stack_pos, cur_stack_pos + spec_size)) {
			if (ptr == cur_stack_pos && size <= spec_size) { return spec; }
			env->exitcode = EC_ACCESS_FAILURE;
			env->err_ptr = ptr;
			env->err_access_length = size;
			return (DataSpec){.type = DS_VOID};
		}
		cur_stack_pos += spec_size;
	);
	env->exitcode = EC_ACCESS_FAILURE;
	env->err_ptr = ptr;
	env->err_access_length = size;
	return (DataSpec){.type = DS_VOID};
}

bool setStackSpec(ExecEnv* env, void* ptr, DataSpec spec)
{
	void* cur_stack_pos = env->stack_head;
	int spec_size = dataSpecSize(spec);
	array_rev_foreach(DataSpec, cur_spec, arrayhead(env->vars)->vars,
		int cur_spec_size = dataSpecSize(cur_spec);
		if (inRange(ptr, cur_stack_pos, cur_stack_pos + cur_spec_size)) {
			if (ptr == cur_stack_pos) {
				if (spec_size == cur_spec_size) {
					arrayhead(env->vars)->vars.data[_cur_spec] = spec;
					return true;
				} else if (spec_size < cur_spec_size) {
					arrayhead(env->vars)->vars.data[_cur_spec] = intSpecFromSize(cur_spec_size);
					return true;
				}
			}
			return false;
		}
		cur_stack_pos += cur_spec_size;
	);
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
	if (env->flags & BREX_TRACING) {
		env->regs_trace[0].type = DS_INT64;
	}

	env->registers[0] = write(env->registers[0], (char*)env->registers[1], env->registers[2]);

	env->op_id++;
	return false;
}

bool handleArgcSyscall(ExecEnv* env, Program* program)
{
	if (env->flags & BREX_TRACING) {
		env->regs_trace[0].type = DS_INT32;
	}

	env->registers[0] = env->exec_argc;
	env->op_id++;
	return false;
}

bool handleArgvSyscall(ExecEnv* env, Program* program)
{
	if (env->flags & BREX_TRACING) {
		env->regs_trace[0] = (DataSpec){ 
			.type = DS_PTR,
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

bool handleOpSet(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_INT64;
	}

	env->op_id++;
	return false;
}

bool handleOpSetr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
	}

	env->op_id++;
	return false;
}

bool handleOpSetd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)program->datablocks.data[op.symbol_id].spec.data;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg] = (DataSpec){ 
			.type = DS_PTR,
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

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg] = (DataSpec){ .type = DS_CONST, .symbol_id = op.symbol_id };
	}

	env->op_id++;
	return false;
}

bool handleOpSetm(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->memblocks.data[op.symbol_id].data;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg] = (DataSpec){ 
			.type = DS_PTR,
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

	if (env->flags &  BREX_TRACING) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & BREX_TRACING) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				if (isIntSpec(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & BREX_TRACING) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & BREX_TRACING) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				if (isIntSpec(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpEqr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] == env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpNeq(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] != op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpNeqr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] != env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLt(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] < (uint64_t)op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLtr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] < env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGt(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] > (uint64_t)op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGtr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] > env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLe(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] <= (uint64_t)op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLer(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] <= env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGe(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >= (uint64_t)op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGer(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >= env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLts(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] < op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLtsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] < (int64_t)env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGts(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] > op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGtsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] > (int64_t)env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLes(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] <= op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpLesr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] <= (int64_t)env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGes(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >= op.value;

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpGesr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >= (int64_t)env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_BOOL;
	}

	env->op_id++;
	return false;
}

bool handleOpAnd(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] & op.value;

	if (env->flags & BREX_TRACING) {
		if (isIntSpec(env->regs_trace[op.src_reg])) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpAndr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] & env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		bool left_reg_int = isIntSpec(env->regs_trace[op.src_reg]), right_reg_int = isIntSpec(env->regs_trace[op.src2_reg]);
		if (left_reg_int && right_reg_int) { // TODO: proper estimation of data size
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
			env->regs_trace[op.dst_reg].type = DS_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpOr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] | op.value;

	if (env->flags & BREX_TRACING) {
		if (isIntSpec(env->regs_trace[op.src_reg])) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpOrr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] | env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		bool left_reg_int = isIntSpec(env->regs_trace[op.src_reg]), right_reg_int = isIntSpec(env->regs_trace[op.src2_reg]);
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
			env->regs_trace[op.dst_reg].type = DS_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpNot(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = ~env->registers[op.src_reg];

	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg].type = DS_INT64;
	}

	env->op_id++;
	return false;
}

bool handleOpXor(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ op.value;

	if (env->flags & BREX_TRACING) {
		if (isIntSpec(env->regs_trace[op.src_reg])) {
			env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpXorr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ env->registers[op.src2_reg];

	if (env->flags & BREX_TRACING) {
		bool left_reg_int = isIntSpec(env->regs_trace[op.src_reg]), right_reg_int = isIntSpec(env->regs_trace[op.src2_reg]);
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
			env->regs_trace[op.dst_reg].type = DS_INT64;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpShl(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] << op.value;

	if (env->flags & BREX_TRACING) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & BREX_TRACING) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				if (isIntSpec(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & BREX_TRACING) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & BREX_TRACING) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				if (isIntSpec(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & (BREX_TRACING)) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
				break;
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
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

	if (env->flags & (BREX_TRACING)) {
		switch (env->regs_trace[op.src_reg].type) {
			case DS_PTR:
				if (isIntSpec(env->regs_trace[op.src2_reg])) {
					env->regs_trace[op.dst_reg] = env->regs_trace[op.src_reg];
					break;
				}
			default:
				if (env->registers[op.dst_reg] >= (1L << 32)) {
					env->regs_trace[op.dst_reg].type = DS_INT64;
				} else if (env->registers[op.dst_reg] >= (1 << 16)) {
					env->regs_trace[op.dst_reg].type = DS_INT32;
				} else if (env->registers[op.dst_reg] >= (1 << 8)) {
					env->regs_trace[op.dst_reg].type = DS_INT16;
				} else {
					env->regs_trace[op.dst_reg].type = DS_INT8;
				}
				break;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpCall(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	if (env->flags & (BREX_TRACING)) {
		if (env->stack_brk > env->stack_head - 16) {
			env->exitcode = EC_STACK_OVERFLOW;
			env->err_push_size = 16;
			return true;
		}

		ProcFrameArray_append(
			&env->vars,
			(ProcFrame){
				.call_id = ++env->call_count,
				.prev_opid = env->op_id,
				.vars = DataSpecArray_new(envctx(env), 0)
			}
		);
	}

	env->stack_head -= sizeof(env->op_id);
	*(int64_t*)env->stack_head = env->op_id + 1;

	env->stack_head -= sizeof(env->prev_stack_head);
	*(void**)env->stack_head = env->prev_stack_head;
	env->prev_stack_head = env->stack_head;

	env->op_id = op.symbol_id; 
	return false;
}

bool handleOpRet(ExecEnv* env, Program* program)
{
	if (env->flags & BREX_TRACING) {
		if (env->stack_head + 16 > env->stack_brk + program->stack_size) {
			env->exitcode = EC_STACK_UNDERFLOW;
			env->err_pop_size = 16;
			return true;
		}

		if (env->vars.length == 1) {
			env->exitcode = EC_NO_STACKFRAME;
			return true;
		}
		env->vars.length--;
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

	if (env->flags & BREX_TRACING) {
		BufferRef ref = validateMemoryAccess(env, program, env->regs_trace[op.src_reg], (void*)env->registers[op.src_reg], 8);
		if (!ref.type) {
			return true;
		} else if (ref.type == BUF_VAR) {
			env->regs_trace[op.dst_reg] = getStackSpec(env, (void*)env->registers[op.src_reg], 8);
			if (env->regs_trace[op.dst_reg].type == DS_VOID) {
				if (!env->exitcode) {
					env->exitcode = EC_UNDEFINED_STACK_LOAD;
					env->err_ptr = (void*)env->registers[op.src_reg];
				}
				return true;
			}
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		}
	}

	env->registers[op.dst_reg] = *(int64_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr64(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & BREX_TRACING) {
		BufferRef ref = validateMemoryAccess(env, program, env->regs_trace[op.dst_reg], (void*)env->registers[op.dst_reg], 8);
		if (!ref.type) {
			return true;
		} else if (ref.type == BUF_VAR) {
			if (!setStackSpec(
				env,
				(void*)env->registers[op.dst_reg],
				dataSpecSize(env->regs_trace[op.src_reg]) == 8 ? env->regs_trace[op.src_reg] : (DataSpec){.type = DS_INT64}
			)) {
				env->exitcode = EC_ACCESS_FAILURE;
				env->err_ptr = (void*)env->registers[op.dst_reg];
				env->err_access_length = 8;
				return true;
			}
		}
	}

	*(int64_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd32(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & BREX_TRACING) {
		BufferRef ref = validateMemoryAccess(env, program, env->regs_trace[op.src_reg], (void*)env->registers[op.src_reg], 4);
		if (!ref.type) {
			return true;
		} else if (ref.type == BUF_VAR) {
			env->regs_trace[op.dst_reg] = getStackSpec(env, (void*)env->registers[op.src_reg], 4);
			if (env->regs_trace[op.dst_reg].type == DS_VOID) {
				if (!env->exitcode) {
					env->exitcode = EC_UNDEFINED_STACK_LOAD;
					env->err_ptr = (void*)env->registers[op.src_reg];
				}
				return true;
			}
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT32;
		}
	}

	env->registers[op.dst_reg] = *(int32_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr32(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & BREX_TRACING) {
		BufferRef ref = validateMemoryAccess(env, program, env->regs_trace[op.dst_reg], (void*)env->registers[op.dst_reg], 4);
		if (!ref.type) {
			return true;
		} else if (ref.type == BUF_VAR) {
			if (!setStackSpec(
				env,
				(void*)env->registers[op.dst_reg], 
				dataSpecSize(env->regs_trace[op.src_reg]) == 4 ? env->regs_trace[op.src_reg] : (DataSpec){.type = DS_INT32}
			)) {
				env->exitcode = EC_ACCESS_FAILURE;
				env->err_ptr = (void*)env->registers[op.dst_reg];
				env->err_access_length = 4;
				return true;
			}
		}
	}

	*(int32_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd16(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & BREX_TRACING) {
		BufferRef ref = validateMemoryAccess(env, program, env->regs_trace[op.src_reg], (void*)env->registers[op.src_reg], 2);
		if (!ref.type) {
			return true;
		} else if (ref.type == BUF_VAR) {
			env->regs_trace[op.dst_reg] = getStackSpec(env, (void*)env->registers[op.src_reg], 2);
			if (env->regs_trace[op.dst_reg].type == DS_VOID) {
				if (!env->exitcode) {
					env->exitcode = EC_UNDEFINED_STACK_LOAD;
					env->err_ptr = (void*)env->registers[op.src_reg];
				}
				return true;
			}
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT16;
		}
	}

	env->registers[op.dst_reg] = *(int16_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr16(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & BREX_TRACING) {
		BufferRef ref = validateMemoryAccess(env, program, env->regs_trace[op.dst_reg], (void*)env->registers[op.dst_reg], 2);
		if (!ref.type) {
			return true;
		} else if (ref.type == BUF_VAR) {
			if (!setStackSpec(
				env,
				(void*)env->registers[op.dst_reg], 
				dataSpecSize(env->regs_trace[op.src_reg]) == 2 ? env->regs_trace[op.src_reg] : (DataSpec){.type = DS_INT16}
			)) {
				env->exitcode = EC_ACCESS_FAILURE;
				env->err_ptr = (void*)env->registers[op.dst_reg];
				env->err_access_length = 2;
				return true;
			}
		}
	}

	*(int16_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd8(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & BREX_TRACING) {
		BufferRef ref = validateMemoryAccess(env, program, env->regs_trace[op.src_reg], (void*)env->registers[op.src_reg], 1);
		if (!ref.type) {
			return true;
		} else if (ref.type == BUF_VAR) {
			env->regs_trace[op.dst_reg] = getStackSpec(env, (void*)env->registers[op.src_reg], 1);
			if (env->regs_trace[op.dst_reg].type == DS_VOID) {
				if (!env->exitcode) {
					env->exitcode = EC_UNDEFINED_STACK_LOAD;
					env->err_ptr = (void*)env->registers[op.src_reg];
				}
				return true;
			}
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT8;
		}
	}

	env->registers[op.dst_reg] = *(int8_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr8(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & BREX_TRACING) {
		BufferRef ref = validateMemoryAccess(env, program, env->regs_trace[op.dst_reg], (void*)env->registers[op.dst_reg], 1);
		if (!ref.type) {
			return true;
		} else if (ref.type == BUF_VAR) {
			if (!setStackSpec(
				env,
				(void*)env->registers[op.dst_reg], 
				dataSpecSize(env->regs_trace[op.src_reg]) == 1 ? env->regs_trace[op.src_reg] : (DataSpec){.type = DS_INT8}
			)) {
				env->exitcode = EC_ACCESS_FAILURE;
				env->err_ptr = (void*)env->registers[op.dst_reg];
				env->err_access_length = 1;
				return true;
			}
		}
	}

	*(int8_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpVar(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	if (env->flags & BREX_TRACING) {
		DataSpecArray_append(
			&arrayhead(env->vars)->vars, 
			(DataSpec){
				.type = DS_VOID,
				.size = op.var_size
			}
		);
	}

	env->stack_head -= op.var_size;
	env->op_id++;
	return false;
}

bool handleOpSetv(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];
	
	if (env->flags & BREX_TRACING) {
		env->regs_trace[op.dst_reg] = (DataSpec){
			.type = DS_PTR,
			.ref = (BufferRef){
				.type = BUF_VAR,
				.id = arrayhead(env->vars)->call_id
			}
		};
	}

	env->registers[op.dst_reg] = (int64_t)env->stack_head + op.symbol_id;
	env->op_id++;
	return false;
}

bool handleOpMul(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	env->registers[op.dst_reg] = env->registers[op.src_reg] * (uint64_t)op.value;
	
	if (env->flags & BREX_TRACING) {
		if (env->registers[op.dst_reg] >= (1L << 32)) {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		} else if (env->registers[op.dst_reg] >= (1 << 16)) {
			env->regs_trace[op.dst_reg].type = DS_INT32;
		} else if (env->registers[op.dst_reg] >= (1 << 8)) {
			env->regs_trace[op.dst_reg].type = DS_INT16;
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT8;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpMulr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	env->registers[op.dst_reg] = env->registers[op.src_reg] * env->registers[op.src2_reg];
	
	if (env->flags & BREX_TRACING) {
		if (env->registers[op.dst_reg] >= (1L << 32)) {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		} else if (env->registers[op.dst_reg] >= (1 << 16)) {
			env->regs_trace[op.dst_reg].type = DS_INT32;
		} else if (env->registers[op.dst_reg] >= (1 << 8)) {
			env->regs_trace[op.dst_reg].type = DS_INT16;
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT8;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpDiv(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	env->registers[op.dst_reg] = env->registers[op.src_reg] / (uint64_t)op.value;
	
	if (env->flags & BREX_TRACING) {
		if (!op.value) {
			env->exitcode = EC_ZERO_DIVISION;
			return true;
		}

		if (env->registers[op.dst_reg] >= (1L << 32)) {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		} else if (env->registers[op.dst_reg] >= (1 << 16)) {
			env->regs_trace[op.dst_reg].type = DS_INT32;
		} else if (env->registers[op.dst_reg] >= (1 << 8)) {
			env->regs_trace[op.dst_reg].type = DS_INT16;
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT8;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpDivr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	env->registers[op.dst_reg] = env->registers[op.src_reg] / env->registers[op.src2_reg];
	
	if (env->flags & BREX_TRACING) {
		if (!env->registers[op.src2_reg]) {
			env->exitcode = EC_ZERO_DIVISION;
			return true;
		}

		if (env->registers[op.dst_reg] >= (1L << 32)) {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		} else if (env->registers[op.dst_reg] >= (1 << 16)) {
			env->regs_trace[op.dst_reg].type = DS_INT32;
		} else if (env->registers[op.dst_reg] >= (1 << 8)) {
			env->regs_trace[op.dst_reg].type = DS_INT16;
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT8;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpDivs(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] / op.value;
	
	if (env->flags & BREX_TRACING) {
		if (!op.value) {
			env->exitcode = EC_ZERO_DIVISION;
			return true;
		}

		if (env->registers[op.dst_reg] >= (1L << 32)) {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		} else if (env->registers[op.dst_reg] >= (1 << 16)) {
			env->regs_trace[op.dst_reg].type = DS_INT32;
		} else if (env->registers[op.dst_reg] >= (1 << 8)) {
			env->regs_trace[op.dst_reg].type = DS_INT16;
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT8;
		}
	}

	env->op_id++;
	return false;
}

bool handleOpDivsr(ExecEnv* env, Program* program)
{
	Op op = program->execblock.data[env->op_id];

	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] / (int64_t)env->registers[op.src2_reg];
	
	if (env->flags & BREX_TRACING) {
		if (!env->registers[op.src2_reg]) {
			env->exitcode = EC_ZERO_DIVISION;
			return true;
		}

		if (env->registers[op.dst_reg] >= (1L << 32)) {
			env->regs_trace[op.dst_reg].type = DS_INT64;
		} else if (env->registers[op.dst_reg] >= (1 << 16)) {
			env->regs_trace[op.dst_reg].type = DS_INT32;
		} else if (env->registers[op.dst_reg] >= (1 << 8)) {
			env->regs_trace[op.dst_reg].type = DS_INT16;
		} else {
			env->regs_trace[op.dst_reg].type = DS_INT8;
		}
	}

	env->op_id++;
	return false;
}

ExecHandler op_handlers[] = {
	&handleNop,
	&handleOpEnd,
	&handleNop, // OP_MARK
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
	&handleNop, // OP_PROC
	&handleOpCall,
	&handleOpRet,
	&handleNop, // OP_ENDPROC
	&handleOpLd64,
	&handleOpStr64,
	&handleOpLd32,
	&handleOpStr32,
	&handleOpLd16,
	&handleOpStr16,
	&handleOpLd8,
	&handleOpStr8,
	&handleOpVar,
	&handleOpSetv,
	&handleOpMul,
	&handleOpMulr,
	&handleOpDiv,
	&handleOpDivr,
	&handleOpDivs,
	&handleOpDivsr
};
static_assert(N_OPS == sizeof(op_handlers) / sizeof(op_handlers[0]), "Some BRB operations have unmatched execution handlers");

void printRuntimeError(FILE* fd, ExecEnv* env, Program* program)
{
	switch (env->exitcode) {
		case EC_NO_STACKFRAME:
			fprintf(fd,"%llx:\n\truntime error: attempted to return from a global stack frame\n", env->op_id);
			break;
		case EC_ZERO_DIVISION:
			fprintf(fd, "%llx:\n\truntime error: attempted to divide by zero\n", env->op_id);
			break;
		case EC_OUTDATED_LOCALPTR:
			fprintf(fd, "%llx:\n\truntime error: attempted to use an outdated stack pointer\n", env->op_id);
			fprintf(fd, "\tpointer %p references already deallocated stack frame\n", env->err_ptr);
			break;
		case EC_UNDEFINED_STACK_LOAD:
			fprintf(fd, "%llx:\n\truntime error: attempted to read from an unused stack variable\n", env->op_id);
			fprintf(fd, "\tstack variable with undefined value is at %p\n", env->err_ptr);
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
				case BUF_VAR:
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
					case 't': exec_flags |= BREX_TRACING; break;
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

	Program program;
	BRBLoadError err = loadProgram(input_fd, &program, GLOBAL_CTX);
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