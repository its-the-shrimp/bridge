// implementation for execution of BRB modules
#include <brb.h>

sbuf allocateDataBlock(DataBlock block)
{
	static_assert(N_PIECE_TYPES == 8, "not all data piece types are handled in `allocateDataBlock`");

	sbuf_size_t res = 0;
	for (int i = 0; i < block.pieces.length; ++i) {
		DataPiece* piece = block.pieces.data + i;
		switch (piece->type) {
			case PIECE_BYTES:
			case PIECE_TEXT:
				res += piece->data.length;
				break;
			case PIECE_INT16:
				res += 2;
				break;
			case PIECE_INT32:
				res += 4;
				break;
			case PIECE_INT64:
			case PIECE_DB_ADDR:
				res += 8;
				break;
			case PIECE_ZERO:
				res += piece->n_bytes;
				break;
			case PIECE_NONE:
			case N_PIECE_TYPES:
			default:
				assert(false, "invalid data piece type");
		}
	}

	return smalloc(res);
}

void assembleDataBlock(ExecEnv* env, DataBlock block, sbuf dst)
{
	static_assert(N_PIECE_TYPES == 8, "not all data piece types are handled in `assembleDataBlock`");

	int64_t offset = 0;
	for (DataPiece* piece = block.pieces.data; piece - block.pieces.data < block.pieces.length; ++piece) {
		assert(offset < dst.length, "`dst` is of insuffiecent size");
		switch (piece->type) {
			case PIECE_BYTES:
			case PIECE_TEXT:
				memcpy(dst.data + offset, piece->data.data, piece->data.length);
				offset += piece->data.length;
				break;
			case PIECE_INT16:
				*(int16_t*)(dst.data + offset) = piece->integer;
				offset += 2;
				break;
			case PIECE_INT32:
				*(int32_t*)(dst.data + offset) = piece->integer;
				offset += 4;
				break;
			case PIECE_INT64:
				*(int64_t*)(dst.data + offset) = piece->integer;
				offset += 8;
				break;
			case PIECE_DB_ADDR:
				*(char**)(dst.data + offset) = env->seg_data.data[piece->symbol_id].data;
				offset += 8;
				break;
			case PIECE_ZERO:
				memset(dst.data + offset, 0, piece->n_bytes);
				offset += piece->n_bytes;
				break;
			case PIECE_NONE:
			case N_PIECE_TYPES:
			default:
				assert(false, "invalid data piece type");
		}
	}
}

void initExecEnv(ExecEnv* env, Module* module, char** args)
{
	env->exec_callbacks = NULL;
	env->stack_brk = malloc(module->stack_size);
	env->exitcode = 0;
	env->op_id = module->entry_opid;
	env->registers = calloc(N_REGS, sizeof(uint64_t));
	env->prev_stack_head = env->stack_head = env->stack_brk + module->stack_size;

	env->seg_data = sbufArray_new(-module->seg_data.length);
	env->seg_data.length = module->seg_data.length;
	for (int i = 0; i < module->seg_data.length; ++i) {
		env->seg_data.data[i] = allocateDataBlock(module->seg_data.data[i]);
	}
	for (int i = 0; i < module->seg_data.length; ++i) {
		assembleDataBlock(env, module->seg_data.data[i], env->seg_data.data[i]);
	}

	env->exec_argc = 0;
	while (args[++env->exec_argc]);
	env->exec_argv = malloc(env->exec_argc * sizeof(sbuf));
	for (int i = 0; i < env->exec_argc; i++) {
		env->exec_argv[i] = SBUF(args[i]);
		env->exec_argv[i].length++;
	}
}

bool addCallBack(ExecEnv* env, uint8_t op_id, ExecCallback callback)
{
	if (!env->exec_callbacks) {
		env->exec_callbacks = calloc(N_OPS, sizeof(env->exec_callbacks));
		if (!env->exec_callbacks) return false;
	}
	env->exec_callbacks[op_id] = callback;
	return true;
}

bool addDefaultCallback(ExecEnv* env, ExecCallback callback)
{
	if (!env->exec_callbacks) {
		env->exec_callbacks = calloc(N_OPS, sizeof(env->exec_callbacks));
		if (!env->exec_callbacks) return false;
	}
	memset_pattern8(env->exec_callbacks, &callback, N_OPS * sizeof(ExecCallback));
	return true;
}

void setCurrentSrc(ExecEnv* env, Module* module)
{
	env->src_path = NULL;
	env->src_line = 0;
	for (int i = env->op_id; i >= 0; i--) {
		Op op = module->seg_exec.data[i];
		if (op.type == OP_ATF) {
			env->src_path = op.mark_name;
			return;
		} else if (op.type == OP_ATL && !env->src_line) env->src_line = op.symbol_id;
	}
}

bool handleInvalidSyscall(ExecEnv* env, Module* module)
{
	env->exitcode = EC_UNKNOWN_SYSCALL;
	return true;
}

bool handleExitSyscall(ExecEnv* env, Module* module)
{
	env->exitcode = env->registers[0];
	return true;
}

bool handleWriteSyscall(ExecEnv* env, Module* module)
{
	env->registers[0] = write(env->registers[0], (char*)env->registers[1], env->registers[2]);
	env->op_id++;
	return false;
}

bool handleArgcSyscall(ExecEnv* env, Module* module)
{
	env->registers[0] = env->exec_argc;
	env->op_id++;
	return false;
}

bool handleArgvSyscall(ExecEnv* env, Module* module)
{
	env->registers[0] = inRange(env->registers[0], 0, env->exec_argc) ? (uint64_t)env->exec_argv[env->registers[0]].data : 0;
	env->op_id++;
	return false;
}

bool handleReadSyscall(ExecEnv* env, Module* module)
{
	env->registers[0] = read(env->registers[0], (void*)env->registers[1], env->registers[2]);
	env->op_id++;
	return false;
}

bool handleGetErrnoSyscall(ExecEnv* env, Module* module)
{
	env->registers[0] = errno;
	env->op_id++;
	return false;
}

bool handleSetErrnoSyscall(ExecEnv* env, Module* module)
{
	errno = env->registers[0];
	env->op_id++;
	return false;
}

typedef bool (*ExecHandler) (ExecEnv*, Module*);

ExecHandler syscall_handlers[] = {
	[SYS_OP_INVALID] = &handleInvalidSyscall,
	[SYS_OP_EXIT] = &handleExitSyscall,
	[SYS_OP_WRITE] = &handleWriteSyscall,
	[SYS_OP_ARGC] = &handleArgcSyscall,
	[SYS_OP_ARGV] = &handleArgvSyscall,
	[SYS_OP_READ] = &handleReadSyscall,
	[SYS_OP_GET_ERRNO] = &handleGetErrnoSyscall,
	[SYS_OP_SET_ERRNO] = &handleSetErrnoSyscall
};
static_assert(N_SYS_OPS == sizeof(syscall_handlers) / sizeof(syscall_handlers[0]), "not all system calls have matching handlers");

bool handleCondition(ExecEnv* env, ConditionCode cond_id) {
	switch (cond_id) {
		case COND_NON: return true;
		case COND_EQU: return env->registers[CONDREG1_ID] == env->registers[CONDREG2_ID];
		case COND_NEQ: return env->registers[CONDREG1_ID] != env->registers[CONDREG2_ID];
		case COND_LTU: return env->registers[CONDREG1_ID] <  env->registers[CONDREG2_ID];
		case COND_GTU: return env->registers[CONDREG1_ID] >  env->registers[CONDREG2_ID];
		case COND_LEU: return env->registers[CONDREG1_ID] <= env->registers[CONDREG2_ID];
		case COND_GEU: return env->registers[CONDREG1_ID] >= env->registers[CONDREG2_ID];
		case COND_LTS: return (int64_t)env->registers[CONDREG1_ID] <  (int64_t)env->registers[CONDREG2_ID];
		case COND_GTS: return (int64_t)env->registers[CONDREG1_ID] >  (int64_t)env->registers[CONDREG2_ID];
		case COND_LES: return (int64_t)env->registers[CONDREG1_ID] <= (int64_t)env->registers[CONDREG2_ID];
		case COND_GES: return (int64_t)env->registers[CONDREG1_ID] >= (int64_t)env->registers[CONDREG2_ID];
		case N_CONDS: return false;
	}
}

bool handleNop(ExecEnv* env, Module* module)
{
	env->op_id++;
	return false;
}

bool handleOpEnd(ExecEnv* env, Module* module)
{
	env->exitcode = EC_OK;
	return true;
}

bool handleOpSet(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = op.value;
	env->op_id++;
	return false;
}

bool handleOpSetr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpSetd(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = (uint64_t)env->seg_data.data[op.symbol_id].data;
	env->op_id++;
	return false;
}

bool handleOpSetb(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = builtins[op.symbol_id].value;
	env->op_id++;
	return false;
}

bool handleOpAdd(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + op.value;
	env->op_id++;
	return false;
}

bool handleOpAddr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpSub(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] - op.value;
	env->op_id++;
	return false;
}

bool handleOpSubr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] - env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpSyscall(ExecEnv* env, Module* module)
{
	return syscall_handlers[module->seg_exec.data[env->op_id].syscall_id](env, module);
}

bool handleOpGoto(ExecEnv* env, Module* module)
{
	env->op_id += module->seg_exec.data[env->op_id].op_offset;
	return false;
}

bool handleOpCmp(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[CONDREG1_ID] = env->registers[op.src_reg];
	env->registers[CONDREG2_ID] = op.value;
	env->op_id++;
	return false;
}

bool handleOpCmpr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[CONDREG1_ID] = env->registers[op.src_reg];
	env->registers[CONDREG2_ID] = env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpAnd(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] & op.value;
	env->op_id++;
	return false;
}

bool handleOpAndr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] & env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpOr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] | op.value;
	env->op_id++;
	return false;
}

bool handleOpOrr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] | env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpNot(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = ~env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpXor(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ op.value;
	env->op_id++;
	return false;
}

bool handleOpXorr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpShl(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] << op.value;
	env->op_id++;
	return false;
}

bool handleOpShlr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] << env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpShr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >> op.value;
	env->op_id++;
	return false;
}

bool handleOpShrr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >> env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpShrs(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >> op.value;
	env->op_id++;
	return false;
}

bool handleOpShrsr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >> env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpCall(ExecEnv* env, Module* module)
{
	env->stack_head -= 8;
	*(int64_t*)env->stack_head = env->op_id + 1;

	env->stack_head -= 8;
	*(void**)env->stack_head = env->prev_stack_head;
	env->prev_stack_head = env->stack_head;

	env->op_id = module->seg_exec.data[env->op_id].symbol_id;

	return false;
}

bool handleOpRet(ExecEnv* env, Module* module)
{
	env->stack_head = env->prev_stack_head;
	env->prev_stack_head = *(void**)env->stack_head;
	env->stack_head += sizeof(env->prev_stack_head);

	env->op_id = *(int64_t*)env->stack_head;
	env->stack_head += 8;

	return false;
}

bool handleOpLd64(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = *(int64_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr64(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int64_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd32(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = *(int32_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr32(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int32_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd16(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = *(int16_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr16(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int16_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd8(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = *(int8_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr8(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int8_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpVar(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->stack_head -= op.new_var_size;
	env->op_id++;
	return false;
}

bool handleOpSetv(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->prev_stack_head - op.symbol_id;
	env->op_id++;
	return false;
}

bool handleOpMul(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] * (uint64_t)op.value;
	env->op_id++;
	return false;
}

bool handleOpMulr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] * env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpDiv(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] / (uint64_t)op.value;
	env->op_id++;
	return false;
}

bool handleOpDivr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] / env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpDivs(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] / op.value;
	env->op_id++;
	return false;
}

bool handleOpDivsr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] / (int64_t)env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpLdv(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = 0;
	memcpy(env->registers + op.dst_reg, env->prev_stack_head - op.symbol_id, op.var_size);
	env->op_id++;
	return false;
}

bool handleOpStrv(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	memcpy(env->prev_stack_head - op.symbol_id, env->registers + op.src_reg, op.var_size);
	env->op_id++;
	return false;
}

bool handleOpPopv(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = 0;
	memcpy(env->registers + op.dst_reg, env->stack_head, op.var_size);
	env->stack_head += op.var_size;
	env->op_id++;
	return false;
}

bool handleOpPushv(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	memcpy((env->stack_head -= op.var_size), env->registers + op.src_reg, op.var_size);
	env->op_id++;
	return false;
}

bool handleOpAtf(ExecEnv* env, Module* module)
{
	env->src_path = module->seg_exec.data[env->op_id].mark_name;
	env->src_line = 0;
	env->op_id++;
	return false;
}

bool handleOpAtl(ExecEnv* env, Module* module)
{
	env->src_line = module->seg_exec.data[env->op_id].symbol_id;
	env->op_id++;
	return false;
}

bool handleOpSetc(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = handleCondition(env, op.cond_arg);
	env->op_id++;
	return false;
}

bool handleOpDelnv(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->stack_head += op.symbol_id;
	env->op_id++;
	return false;
}

bool handleOpLd64S(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int64_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd32S(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int32_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd16S(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int16_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd8S(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int8_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLdvs(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = 0;
	memcpy(env->registers + op.dst_reg, env->prev_stack_head - op.symbol_id, op.var_size);
	if (op.var_size < 8 ? env->registers[op.dst_reg] & (1LL << (op.var_size * 8 - 1)) : false) {
		env->registers[op.dst_reg] |= ~byteMask(op.var_size);
	}
	env->op_id++;
	return false;
}

bool handleOpSx32(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int32_t*)(env->registers + op.src_reg);
	env->op_id++;
	return false;
}

bool handleOpSx16(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int16_t*)(env->registers + op.src_reg);
	env->op_id++;
	return false;
}

bool handleOpSx8(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int8_t*)(env->registers + op.src_reg);
	env->op_id++;
	return false;
}

bool handleOpMod(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] % op.value;
	env->op_id++;
	return false;
}

bool handleOpMods(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] % (int64_t)op.value;
	env->op_id++;
	return false;
}

bool handleOpModr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] % env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpModsr(ExecEnv* env, Module* module)
{
	Op op = module->seg_exec.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] % (int64_t)env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

ExecHandler op_handlers[] = {
	[OP_NONE] = &handleNop,
	[OP_END] = &handleOpEnd,
	[OP_MARK] = &handleNop,
	[OP_SET] = &handleOpSet,
	[OP_SETR] = &handleOpSetr,
	[OP_SETD] = &handleOpSetd,
	[OP_SETB] = &handleOpSetb,
	[OP_ADD] = &handleOpAdd,
	[OP_ADDR] = &handleOpAddr,
	[OP_SUB] = &handleOpSub,
	[OP_SUBR] = &handleOpSubr,
	[OP_SYS] = &handleOpSyscall,
	[OP_GOTO] = &handleOpGoto,
	[OP_CMP] = &handleOpCmp,
	[OP_CMPR] = &handleOpCmpr,
	[OP_AND] = &handleOpAnd,
	[OP_ANDR] = &handleOpAndr,
	[OP_OR] = &handleOpOr,
	[OP_ORR] = &handleOpOrr,
	[OP_NOT] = &handleOpNot,
	[OP_XOR] = &handleOpXor,
	[OP_XORR] = &handleOpXorr,
	[OP_SHL] = &handleOpShl,
	[OP_SHLR] = &handleOpShlr,
	[OP_SHR] = &handleOpShr,
	[OP_SHRR] = &handleOpShrr,
	[OP_SHRS] = &handleOpShrs,
	[OP_SHRSR] = &handleOpShrsr,
	[OP_PROC] = &handleNop,
	[OP_CALL] = &handleOpCall,
	[OP_RET] = &handleOpRet,
	[OP_ENDPROC] = &handleNop,
	[OP_LD64] = &handleOpLd64,
	[OP_STR64] = &handleOpStr64,
	[OP_LD32] = &handleOpLd32,
	[OP_STR32] = &handleOpStr32,
	[OP_LD16] = &handleOpLd16,
	[OP_STR16] = &handleOpStr16,
	[OP_LD8] = &handleOpLd8,
	[OP_STR8] = &handleOpStr8,
	[OP_VAR] = &handleOpVar,
	[OP_SETV] = &handleOpSetv,
	[OP_MUL] = &handleOpMul,
	[OP_MULR] = &handleOpMulr,
	[OP_DIV] = &handleOpDiv,
	[OP_DIVR] = &handleOpDivr,
	[OP_DIVS] = &handleOpDivs,
	[OP_DIVSR] = &handleOpDivsr,
	[OP_EXTPROC] = &handleNop,
	[OP_LDV] = &handleOpLdv,
	[OP_STRV] = &handleOpStrv,
	[OP_POPV] = &handleOpPopv,
	[OP_PUSHV] = &handleOpPushv,
	[OP_ATF] = &handleOpAtf,
	[OP_ATL] = &handleOpAtl,
	[OP_SETC] = &handleOpSetc,
	[OP_DELNV] = &handleOpDelnv,
	[OP_LD64S] = &handleOpLd64S,
	[OP_LD32S] = &handleOpLd32S,
	[OP_LD16S] = &handleOpLd16S,
	[OP_LD8S] = &handleOpLd8S,
	[OP_LDVS] = &handleOpLdvs,
	[OP_SX32] = &handleOpSx32,
	[OP_SX16] = &handleOpSx16,
	[OP_SX8] = &handleOpSx8,
	[OP_MOD] = &handleOpMod,
	[OP_MODS] = &handleOpMods,
	[OP_MODR] = &handleOpModr,
	[OP_MODSR] = &handleOpModsr
};
static_assert(N_OPS == sizeof(op_handlers) / sizeof(op_handlers[0]), "Some BRB operations have unmatched execution handlers");

void execOp(ExecEnv* env, Module* module)
{
	Op* op = module->seg_exec.data + env->op_id;
	if (op->cond_id) {
		if (!handleCondition(env, op->cond_id)) {
			env->op_id++;
			return;
		}
	}

	if (env->exec_callbacks)
		if (env->exec_callbacks[op->type])
			if (env->exec_callbacks[op->type](env, module, op)) return;

	if (op_handlers[op->type](env, module)) return;
}

void _execModule(ExecEnv* env, Module* module, volatile bool* interruptor)
{
	while (interruptor ? !(*interruptor) : true) {
		register Op* op = module->seg_exec.data + env->op_id;
		if (op->cond_id) {
			if (!handleCondition(env, op->cond_id)) {
				env->op_id++;
				continue;
			}
		}

		if (op_handlers[op->type](env, module)) break;
		env->registers[ZEROREG_ID] = 0;
	}
}

void _execModuleWithCallbacks(ExecEnv* env, Module* module, volatile bool* interruptor)
{
	while (!(*interruptor)) {
		Op* op = module->seg_exec.data + env->op_id;
		if (op->cond_id) {
			if (!handleCondition(env, op->cond_id)) {
				env->op_id++;
				continue;
			}
		}

		if (env->exec_callbacks[op->type])
			if (env->exec_callbacks[op->type](env, module, op)) break;

		if (op_handlers[op->type](env, module)) break;
		env->registers[ZEROREG_ID] = 0;

		if (env->exec_callbacks[N_OPS + op->type])
			if (env->exec_callbacks[N_OPS + op->type](env, module, op)) break;
	}
}

void execModule(ExecEnv* env, Module* module, volatile bool* interruptor)
{
	if (env->exec_callbacks) {
		_execModuleWithCallbacks(env, module, interruptor);
	} else {
		_execModule(env, module, interruptor);
	}
}
