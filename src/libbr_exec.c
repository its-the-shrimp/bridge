// implementation for execution of BRB modules
#include <br.h>
#include <unistd.h>

defArray(sbuf);

static void prepareOpForExec(BR_ModuleBuilder* builder, sbufArray seg_data, BR_id proc_id, uint32_t op_id)
{
	BR_Op *const op = BR_getOp(&builder->module, proc_id, op_id);
	switch (op->type) {
			case BR_OP_NOP:
			case BR_OP_END:
			case BR_OP_I8:
			case BR_OP_I16:
			case BR_OP_I32:
			case BR_OP_I64:
			case BR_OP_PTR:
			case BR_OP_SYS:
			case BR_OP_ADDIAT8:
			case BR_OP_ADDIAT16:
			case BR_OP_ADDIAT32:
			case BR_OP_ADDIATP:
			case BR_OP_ADDIAT64:
			case BR_OP_SUBIAT8:
			case BR_OP_SUBIAT16:
			case BR_OP_SUBIAT32:
			case BR_OP_SUBIATP:
			case BR_OP_SUBIAT64:
			case BR_OP_MULIAT8:
			case BR_OP_MULIAT16:
			case BR_OP_MULIAT32:
			case BR_OP_MULIATP:
			case BR_OP_MULIAT64:
			case BR_OP_DIVIAT8:
			case BR_OP_DIVIAT16:
			case BR_OP_DIVIAT32:
			case BR_OP_DIVIATP:
			case BR_OP_DIVIAT64:
			case BR_OP_DIVSIAT8:
			case BR_OP_DIVSIAT16:
			case BR_OP_DIVSIAT32:
			case BR_OP_DIVSIATP:
			case BR_OP_DIVSIAT64:
			case BR_OP_MODIAT8:
			case BR_OP_MODIAT16:
			case BR_OP_MODIAT32:
			case BR_OP_MODIATP:
			case BR_OP_MODIAT64:
			case BR_OP_MODSIAT8:
			case BR_OP_MODSIAT16:
			case BR_OP_MODSIAT32:
			case BR_OP_MODSIATP:
			case BR_OP_MODSIAT64:
			case BR_OP_ANDIAT8:
			case BR_OP_ANDIAT16:
			case BR_OP_ANDIAT32:
			case BR_OP_ANDIATP:
			case BR_OP_ANDIAT64:
			case BR_OP_ORIAT8:
			case BR_OP_ORIAT16:
			case BR_OP_ORIAT32:
			case BR_OP_ORIATP:
			case BR_OP_ORIAT64:
			case BR_OP_XORIAT8:
			case BR_OP_XORIAT16:
			case BR_OP_XORIAT32:
			case BR_OP_XORIATP:
			case BR_OP_XORIAT64:
			case BR_OP_SHLIAT8:
			case BR_OP_SHLIAT16:
			case BR_OP_SHLIAT32:
			case BR_OP_SHLIATP:
			case BR_OP_SHLIAT64:
			case BR_OP_SHRIAT8:
			case BR_OP_SHRIAT16:
			case BR_OP_SHRIAT32:
			case BR_OP_SHRIATP:
			case BR_OP_SHRIAT64:
			case BR_OP_SHRSIAT8:
			case BR_OP_SHRSIAT16:
			case BR_OP_SHRSIAT32:
			case BR_OP_SHRSIATP:
			case BR_OP_SHRSIAT64:
			case BR_OP_NOTAT8:
			case BR_OP_NOTAT16:
			case BR_OP_NOTAT32:
			case BR_OP_NOTATP:
			case BR_OP_NOTAT64:
				break;
			case BR_OP_ADDR:
				op->operand_u = BR_getStackItemRTOffset(builder, proc_id, op_id, op->operand_u);
				break;
			case BR_OP_DBADDR:
				op->operand_ptr = seg_data.data[~op->operand_s].data;
				break;
			case BR_OP_BUILTIN:
				op->operand_u = BR_builtinValues[op->operand_u];
				break;
			case BR_OP_ADD:
			case BR_OP_SUB:
			case BR_OP_MUL:
			case BR_OP_DIV:
			case BR_OP_DIVS:
			case BR_OP_MOD:
			case BR_OP_MODS:
			case BR_OP_AND:
			case BR_OP_OR:
			case BR_OP_XOR:
			case BR_OP_SHL:
			case BR_OP_SHR:
			case BR_OP_SHRS:
				op->x_op2_size = BR_getStackItemRTSize(builder, proc_id, op_id - 1, 1);
			case BR_OP_ADDI:
			case BR_OP_SUBI:
			case BR_OP_MULI:
			case BR_OP_DIVI:
			case BR_OP_DIVSI:
			case BR_OP_MODI:
			case BR_OP_MODSI:
			case BR_OP_ANDI:
			case BR_OP_ORI:
			case BR_OP_XORI:
			case BR_OP_SHLI:
			case BR_OP_SHRI:
			case BR_OP_SHRSI:
			case BR_OP_NOT:
				op->x_op1_size = BR_getStackItemRTSize(builder, proc_id, op_id - 1, 0);
				break;
			case BR_OP_DROP:
				op->operand_u = BR_getStackItemRTSize(builder, proc_id, op_id - 1, 0);
				break;
			case BR_OP_NEW:
			case BR_OP_ZERO:
				op->operand_u = BR_getTypeRTSize(&builder->module, op->operand_type);
				break;
			case BR_OP_GET:
				op->x_op1_size = BR_getStackItemRTSize(builder, proc_id, op_id, 0);
				op->operand_u = BR_getStackItemRTOffset(builder, proc_id, op_id - 1, op->operand_u) + op->x_op1_size;
				break;
			case BR_OP_SETAT:
				op->operand_u = BR_getStackItemRTSize(builder, proc_id, op_id, 0);
				break;
			case BR_OP_GETFROM:
			case BR_OP_COPY:
				op->operand_u = BR_getTypeRTSize(&builder->module, op->operand_type);
				break;
			case BR_N_OPS:
			default:
				assert(false, "unknown operation type %u\n", op->type);
	}
}

/*
void getSrcLoc(ExecEnv* env, const char** filename_p, size_t* line_p)
{
	*filename_p = NULL;
	*line_p = 0;
	for (int i = env->exec_index; i >= 0; i--) {
		Op op = module->seg_exec.data[i];
		if (op.type == OP_ATF) {
			*filename_p = op.mark_name;
			return;
		} else if (op.type == OP_ATL && !*line_p) *line_p = op.symbol_id;
	}
}
*/

typedef bool (*BR_syscall)(BR_ExecEnv* env);

bool BR_sysExit(BR_ExecEnv* env)
{
	env->exec_status.type = BR_EXC_EXIT;
	env->exec_status.exit_code = *(uintptr_t*)env->stack_head;
	env->stack_head += sizeof(uintptr_t);
	++env->exec_index;
	return true;
}

bool BR_sysWrite(BR_ExecEnv* env)
{
	register const intptr_t res = write(((intptr_t*)env->stack_head)[0], ((void**)env->stack_head)[1], ((uintptr_t*)env->stack_head)[2]);
	((intptr_t*)env->stack_head)[2] = res;
	env->stack_head += 2 * sizeof(void*);
	++env->exec_index;
	return false;
}

bool BR_sysRead(BR_ExecEnv* env)
{
	register const intptr_t res = read(((intptr_t*)env->stack_head)[0], ((void**)env->stack_head)[1], ((uintptr_t*)env->stack_head)[2]);
	((intptr_t*)env->stack_head)[2] = res;
	env->stack_head += 2 * sizeof(void*);
	++env->exec_index;
	return false;
}

static const BR_syscall BR_syscalls[] = {
	[BR_SYS_EXIT] = BR_sysExit,
	[BR_SYS_WRITE] = BR_sysWrite,
	[BR_SYS_READ] = BR_sysRead
};
static_assert(sizeof(BR_syscalls) / sizeof(BR_syscall) == BR_N_SYSCALLS, "not all syscalls have their implementations defined");

bool BR_execOp(BR_ExecEnv* env)
{
#define ALLOC_STACK_SPACE(incr) \
	if ((env->stack_head -= (incr)) < env->stack.data) return (env->exec_status.type = BR_EXC_STACK_OVERFLOW);

	const BR_Op op = env->cur_proc[env->exec_index];
	switch (op.type) {
		case BR_OP_NOP:
			++env->exec_index;
			return false;
		case BR_OP_END:
			env->exec_status.type = BR_EXC_END;
			return true;
		case BR_OP_I8:
			ALLOC_STACK_SPACE(1);
			*(uint8_t*)env->stack_head = op.operand_u;
			++env->exec_index;
			return false;
		case BR_OP_I16:
			ALLOC_STACK_SPACE(2);
			*(uint16_t*)env->stack_head = op.operand_u;
			++env->exec_index;
			return false;
		case BR_OP_I32:
			ALLOC_STACK_SPACE(4);
			*(uint32_t*)env->stack_head = op.operand_u;
			++env->exec_index;
			return false;
		case BR_OP_PTR:
		case BR_OP_DBADDR:
		case BR_OP_BUILTIN:
			ALLOC_STACK_SPACE(sizeof(intptr_t));
			*(uintptr_t*)env->stack_head = op.operand_u;
			++env->exec_index;
			return false;
		case BR_OP_I64:
			ALLOC_STACK_SPACE(8);
			*(uint64_t*)env->stack_head = op.operand_u;
			++env->exec_index;
			return false;
		case BR_OP_ADDR:
			ALLOC_STACK_SPACE(sizeof(void*));
			*(void**)env->stack_head = env->stack_head + op.operand_u;
			++env->exec_index;
			return false;
		case BR_OP_SYS:
			return BR_syscalls[op.operand_u](env);
		case BR_OP_ADD: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head + op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head + op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head + op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head + op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_ADDI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head += op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head += op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head += op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head += op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_ADDIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp += op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ADDIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp += op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ADDIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp += op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ADDIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp += op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ADDIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp += op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SUB: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head - op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head - op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head - op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head - op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_SUBI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head -= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head -= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head -= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head -= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_SUBIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp -= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SUBIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp -= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SUBIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp -= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SUBIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp -= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SUBIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp -= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MUL: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head * op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head * op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head * op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head * op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_MULI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head *= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head *= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head *= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head *= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_MULIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp *= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MULIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp *= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MULIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp *= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MULIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp *= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MULIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp *= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIV: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head / op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head / op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head / op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head / op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head /= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head /= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head /= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head /= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_DIVIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp /= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp /= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp /= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp /= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp /= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVS: {
			int64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(int8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(int16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(int32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(int64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(int8_t *)(env->stack_head + op.x_op2_size) = *(int8_t *)env->stack_head / op2; break;
				case 2: *(int16_t*)(env->stack_head + op.x_op2_size) = *(int16_t*)env->stack_head / op2; break;
				case 4: *(int32_t*)(env->stack_head + op.x_op2_size) = *(int32_t*)env->stack_head / op2; break;
				case 8: *(int64_t*)(env->stack_head + op.x_op2_size) = *(int64_t*)env->stack_head / op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVSI:
			switch (op.x_op1_size) {
				case 1: *(int8_t *)env->stack_head /= op.operand_s; break;
				case 2: *(int16_t*)env->stack_head /= op.operand_s; break;
				case 4: *(int32_t*)env->stack_head /= op.operand_s; break;
				case 8: *(int64_t*)env->stack_head /= op.operand_s; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_DIVSIAT8: {
			register int8_t* temp = *(int8_t**)env->stack_head;
			*(int8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp /= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVSIAT16: {
			register int16_t* temp = *(int16_t**)env->stack_head;
			*(int16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp /= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVSIAT32: {
			register int32_t* temp = *(int32_t**)env->stack_head;
			*(int32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp /= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVSIATP: {
			register intptr_t* temp = *(intptr_t**)env->stack_head;
			*(intptr_t*)env->stack_head = (*temp /= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_DIVSIAT64: {
			register int64_t* temp = *(int64_t**)env->stack_head;
			*(int64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp /= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_MOD: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head % op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head % op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head % op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head % op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_MODI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head %= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head %= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head %= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head %= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_MODIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp %= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp %= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp %= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp %= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp %= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODS: {
			int64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(int8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(int16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(int32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(int64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(int8_t *)(env->stack_head + op.x_op2_size) = *(int8_t *)env->stack_head % op2; break;
				case 2: *(int16_t*)(env->stack_head + op.x_op2_size) = *(int16_t*)env->stack_head % op2; break;
				case 4: *(int32_t*)(env->stack_head + op.x_op2_size) = *(int32_t*)env->stack_head % op2; break;
				case 8: *(int64_t*)(env->stack_head + op.x_op2_size) = *(int64_t*)env->stack_head % op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_MODSI:
			switch (op.x_op1_size) {
				case 1: *(int8_t *)env->stack_head %= op.operand_s; break;
				case 2: *(int16_t*)env->stack_head %= op.operand_s; break;
				case 4: *(int32_t*)env->stack_head %= op.operand_s; break;
				case 8: *(int64_t*)env->stack_head %= op.operand_s; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_MODSIAT8: {
			register int8_t* temp = *(int8_t**)env->stack_head;
			*(int8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp %= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODSIAT16: {
			register int16_t* temp = *(int16_t**)env->stack_head;
			*(int16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp %= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODSIAT32: {
			register int32_t* temp = *(int32_t**)env->stack_head;
			*(int32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp %= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODSIATP: {
			register intptr_t* temp = *(intptr_t**)env->stack_head;
			*(intptr_t*)env->stack_head = (*temp %= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_MODSIAT64: {
			register int64_t* temp = *(int64_t**)env->stack_head;
			*(int64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp %= op.operand_s);
			++env->exec_index;
			return false;
		}
		case BR_OP_AND: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head & op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head & op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head & op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head & op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_ANDI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head &= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head &= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head &= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head &= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_ANDIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp &= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ANDIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp &= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ANDIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp &= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ANDIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp &= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ANDIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp &= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_OR: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head | op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head | op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head | op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head | op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_ORI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head |= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head |= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head |= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head |= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_ORIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp |= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ORIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp |= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ORIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp |= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ORIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp |= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_ORIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp |= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_XOR: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head ^ op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head ^ op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head ^ op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head ^ op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_XORI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head ^= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head ^= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head ^= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head ^= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_XORIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp ^= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_XORIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp ^= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_XORIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp ^= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_XORIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp ^= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_XORIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp ^= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHL: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head << op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head << op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head << op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head << op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_SHLI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head <<= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head <<= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head <<= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head <<= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_SHLIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp <<= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHLIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp <<= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHLIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp <<= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHLIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp <<= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHLIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp <<= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHR: {
			uint64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(uint8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(uint16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(uint32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(uint64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)(env->stack_head + op.x_op2_size) = *(uint8_t *)env->stack_head >> op2; break;
				case 2: *(uint16_t*)(env->stack_head + op.x_op2_size) = *(uint16_t*)env->stack_head >> op2; break;
				case 4: *(uint32_t*)(env->stack_head + op.x_op2_size) = *(uint32_t*)env->stack_head >> op2; break;
				case 8: *(uint64_t*)(env->stack_head + op.x_op2_size) = *(uint64_t*)env->stack_head >> op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRI:
			switch (op.x_op1_size) {
				case 1: *(uint8_t *)env->stack_head >>= op.operand_u; break;
				case 2: *(uint16_t*)env->stack_head >>= op.operand_u; break;
				case 4: *(uint32_t*)env->stack_head >>= op.operand_u; break;
				case 8: *(uint64_t*)env->stack_head >>= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_SHRIAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRIAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRIAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRIATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRIAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRS: {
			int64_t op2;
			switch (op.x_op2_size) {
				case 1:  op2 = *(int8_t *)(env->stack_head + op.x_op1_size); break;
				case 2:  op2 = *(int16_t*)(env->stack_head + op.x_op1_size); break;
				case 4:  op2 = *(int32_t*)(env->stack_head + op.x_op1_size); break;
				case 8:  op2 = *(int64_t*)(env->stack_head + op.x_op1_size); break;
				default: assert(false, "unknown argument size");
			}
			switch (op.x_op1_size) {
				case 1: *(int8_t *)(env->stack_head + op.x_op2_size) = *(int8_t *)env->stack_head >> op2; break;
				case 2: *(int16_t*)(env->stack_head + op.x_op2_size) = *(int16_t*)env->stack_head >> op2; break;
				case 4: *(int32_t*)(env->stack_head + op.x_op2_size) = *(int32_t*)env->stack_head >> op2; break;
				case 8: *(int64_t*)(env->stack_head + op.x_op2_size) = *(int64_t*)env->stack_head >> op2; break;
			}
			env->stack_head += op.x_op2_size;
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRSI:
			switch (op.x_op1_size) {
				case 1: *(int8_t *)env->stack_head >>= op.operand_u; break;
				case 2: *(int16_t*)env->stack_head >>= op.operand_u; break;
				case 4: *(int32_t*)env->stack_head >>= op.operand_u; break;
				case 8: *(int64_t*)env->stack_head >>= op.operand_u; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_SHRSIAT8: {
			register int8_t* temp = *(int8_t**)env->stack_head;
			*(int8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRSIAT16: {
			register int16_t* temp = *(int16_t**)env->stack_head;
			*(int16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRSIAT32: {
			register int32_t* temp = *(int32_t**)env->stack_head;
			*(int32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRSIATP: {
			register intptr_t* temp = *(intptr_t**)env->stack_head;
			*(intptr_t*)env->stack_head = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_SHRSIAT64: {
			register int64_t* temp = *(int64_t**)env->stack_head;
			*(int64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp >>= op.operand_u);
			++env->exec_index;
			return false;
		}
		case BR_OP_NOT:
			switch (op.x_op1_size) {
				case 1: *(uint8_t*)env->stack_head = ~*(uint8_t*)env->stack_head; break;
				case 2: *(uint16_t*)env->stack_head = ~*(uint16_t*)env->stack_head; break;
				case 4: *(uint32_t*)env->stack_head = ~*(uint32_t*)env->stack_head; break;
				case 8: *(uint64_t*)env->stack_head = ~*(uint64_t*)env->stack_head; break;
			}
			++env->exec_index;
			return false;
		case BR_OP_NOTAT8: {
			register uint8_t* temp = *(uint8_t**)env->stack_head;
			*(uint8_t*)(env->stack_head += sizeof(void*) - 1) = (*temp = ~*temp);
			++env->exec_index;
			return false;
		}
		case BR_OP_NOTAT16: {
			register uint16_t* temp = *(uint16_t**)env->stack_head;
			*(uint16_t*)(env->stack_head += sizeof(void*) - 2) = (*temp = ~*temp);
			++env->exec_index;
			return false;
		}
		case BR_OP_NOTAT32: {
			register uint32_t* temp = *(uint32_t**)env->stack_head;
			*(uint32_t*)(env->stack_head += sizeof(void*) - 4) = (*temp = ~*temp);
			++env->exec_index;
			return false;
		}
		case BR_OP_NOTATP: {
			register uintptr_t* temp = *(uintptr_t**)env->stack_head;
			*(uintptr_t*)env->stack_head = (*temp = ~*temp);
			++env->exec_index;
			return false;
		}
		case BR_OP_NOTAT64: {
			register uint64_t* temp = *(uint64_t**)env->stack_head;
			*(uint64_t*)(env->stack_head += sizeof(void*) - 8) = (*temp = ~*temp);
			++env->exec_index;
			return false;
		}
		case BR_OP_DROP:
			env->stack_head += op.x_op1_size;
			++env->exec_index;
			return false;
		case BR_OP_NEW:
			ALLOC_STACK_SPACE(op.operand_u);
			++env->exec_index;
			return false;
		case BR_OP_ZERO:
			ALLOC_STACK_SPACE(op.operand_u);
			memset(env->stack_head, 0, op.operand_u);
			++env->exec_index;
			return false;
		case BR_OP_GET:
			ALLOC_STACK_SPACE(op.x_op1_size);
			memcpy(env->stack_head, env->stack_head + op.operand_u, op.x_op1_size);
			++env->exec_index;
			return false; 
		case BR_OP_SETAT:
			memcpy(*(void**)env->stack_head, env->stack_head += sizeof(void*), op.operand_u);
			++env->exec_index;
			return false;
		case BR_OP_GETFROM:
			ALLOC_STACK_SPACE(op.operand_u - sizeof(void*));
			memcpy(env->stack_head, *(void**)(env->stack_head + op.operand_u - sizeof(void*)), op.operand_u);
			++env->exec_index;
			return false;
		case BR_OP_COPY:
			memcpy(*(void**)env->stack_head, *(void**)(env->stack_head + sizeof(void*)), op.operand_u);
			*(void**)(env->stack_head + sizeof(void*)) = *(void**)env->stack_head;
			env->stack_head += sizeof(void*);
			++env->exec_index;
			return false;
		case BR_N_OPS:
		default:
			env->exec_status.type = BR_EXC_UNKNOWN_OP;
			return true;
	}
}

static sbuf allocDataBlock(BR_ModuleBuilder* builder, BR_id db_id)
{
	return smalloc(BR_getMaxStackRTSize(builder, db_id));
}

BR_Error BR_execModule(BR_Module module, BR_ExecEnv* env, char* args[], size_t stack_size, const volatile bool* interruptor)
{
	BR_ExecEnv env_l;
	if (!env) env = &env_l;
	*env = (BR_ExecEnv){0};
// validating the entry point
	if (module.exec_entry_point >= module.seg_exec.length)
		return (BR_Error){.type = BR_ERR_INVALID_ENTRY};
	const BR_Proc* const proc = &module.seg_exec.data[module.exec_entry_point];
	if (proc->ret_type.kind != BR_TYPE_VOID || proc->args.length)
		return (BR_Error){.type = BR_ERR_INVALID_ENTRY_PROTOTYPE};
// setting the arguments
	env->exec_argc = 0;
	if (*args) while (args[++env->exec_argc]);
	env->exec_argv = malloc(env->exec_argc * sizeof(sbuf));
	for (uint32_t i = 0; i < env->exec_argc; i += 1) {
		env->exec_argv[i] = fromstr((char*)args[i]);
		env->exec_argv[i].length += 1;
	}
// setting a default interruptor if the `interruptor` is NULL
	bool stub_interruptor = false;
	if (!interruptor) interruptor = &stub_interruptor;
// pre-allocating the data blocks array
	if (!sbufArray_incrlen(&env->seg_data, module.seg_data.length) && module.seg_data.length)
		return (BR_Error){.type = BR_ERR_NO_MEMORY};
// initializing the module analyzer
	BR_ModuleBuilder builder;
	BR_Error err;
	if ((err = BR_analyzeModule(&module, &builder)).type) return err;
// allocating the data blocks
	arrayForeach (BR_DataBlock, block, builder.module.seg_data) {
		if (!(env->seg_data.data[block - builder.module.seg_data.data] = allocDataBlock(&builder, ~(block - builder.module.seg_data.data))).data)
			return (BR_Error){.type = BR_ERR_NO_MEMORY};
	}
// pre-evaluating the data blocks
	arrayForeach (BR_DataBlock, block, builder.module.seg_data) {
		for (uint32_t op_id = 0; op_id < block->body.length; ++op_id) {
			prepareOpForExec(&builder, env->seg_data, ~(block - builder.module.seg_data.data), op_id);
		}
		if ((err = BR_addOp(&builder, ~(block - builder.module.seg_data.data), (BR_Op){.type = BR_OP_END})).type)
			return err;
		env->cur_proc = block->body.data;
		env->exec_index = 0;
		env->stack = env->seg_data.data[block - builder.module.seg_data.data];
		env->stack_head = env->stack.data + env->stack.length;
		while (true) {
			if (*interruptor)
				return (BR_Error){.type = BR_ERR_MODULE_LOAD_INTERRUPT};
			if (BR_execOp(env)) break;
		}
	}
// TODO: add recursive pre-evaluation of data blocks when one block is referencing another; this probably has to be done after `call`s and `ret`urns are added
// allocating the stack
	if (!(env->stack = smalloc(stack_size)).data)
		return (BR_Error){.type = BR_ERR_NO_MEMORY};
	env->stack_head = env->stack.data + env->stack.length;
// pre-evaluating operands of the operations for faster execution
	arrayForeach (BR_Proc, proc, builder.module.seg_exec) {
		for (uint32_t op_id = 0; op_id < proc->body.length; ++op_id) {
			prepareOpForExec(&builder, env->seg_data, proc - builder.module.seg_exec.data, op_id);
		}
	}
// setting up the entry point
	BR_addOp(&builder, module.exec_entry_point, (BR_Op){.type = BR_OP_END});
	env->entry_point = module.exec_entry_point;
	env->exec_index = 0;
	BR_setEntryPoint(&builder, module.exec_entry_point);
// cleaning up the analyzer
	if ((err = BR_extractModule(builder, &module)).type) return err;
	BR_deallocDataBlocks(&module);
	env->seg_exec = module.seg_exec;
	env->cur_proc = env->seg_exec.data[env->entry_point].body.data;
// main execution loop
	while (true) {
		if (*interruptor) {
			env->exec_status.type = BR_EXC_INTERRUPT;
			break;
		}
		if (BR_execOp(env)) break;
	}
// cleanup
	env->exec_argv = NULL;
	free(env->exec_argv);
	env->exec_argc = 0;
	if (env == &env_l) BR_delExecEnv(env);
	return (BR_Error){0};
}

void BR_delExecEnv(BR_ExecEnv* env)
// TODO
{}
