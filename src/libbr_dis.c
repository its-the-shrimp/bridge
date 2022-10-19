// implementation of disassembly of BRB modules
#define _BR_INTERNAL
#include <br.h>
defArray(BR_Type);

long BR_printType(BR_Type type, FILE* dst)
{
	return fputsbuf(dst, BR_typeNames[type.kind]) + (type.kind == BR_TYPE_DYNAMIC || type.n_items == 1
		? 0
		: fprintf(dst, "[%u]", type.n_items));

}
static long printStructDecl(BR_Struct _struct, FILE* dst)
{
	return fputstr(dst, "struct ")
		+ fputstresc(dst, _struct.name
			? _struct.name
			: "", BYTEFMT_DQUOTED | BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE);
}

static long printDataBlockDecl(BR_DataBlock block, FILE* dst)
{
	return fprintf(dst, "data%s ", block.is_mutable ? "+" : "")
		+ fputstresc(dst, block.name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE | BYTEFMT_DQUOTED);
}

static long printProcDecl(BR_Proc proc, FILE* dst)
{
	long acc = BR_printType(proc.ret_type, dst)
		+ fputstr(dst, " \"")
		+ fputstresc(dst, proc.name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
		+ fputstr(dst, "\"(");
	arrayForeach (BR_Type, arg, proc.args) {
		acc += BR_printType(*arg, dst)
			+ (arg != arrayhead(proc.args)
				? fputstr(dst, ", ")
				: 0);
	}
	return acc + fputstr(dst, ")");
}

static long printOp(BR_Op op, const BR_Module* module, FILE* dst)
{
	switch (BR_GET_OPERAND_TYPE(op.type)) {
		case BR_OPERAND_INT8:
		case BR_OPERAND_INT:
		case BR_OPERAND_VAR_NAME:
			return fprintf(dst, "\t%.*s %llu\n", unpack(BR_opNames[op.type]), op.operand_u);
		case BR_OPERAND_TYPE:
			return fprintf(dst, "\t%.*s ", unpack(BR_opNames[op.type]))
				+ BR_printType(op.operand_type, dst)
				+ fputstr(dst, "\n");
		case BR_OPERAND_DB_NAME:
			return fprintf(dst, "\t%.*s \"", unpack(BR_opNames[op.type]))
				+ fputstresc(dst, module->seg_data.data[~op.operand_s].name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"\n");
		case BR_OPERAND_SYSCALL_NAME:
			return fprintf(dst, "\t%.*s %.*s\n", unpack(BR_opNames[op.type]), unpack(BR_syscallNames[op.operand_u]));
		case BR_OPERAND_BUILTIN:
			return fprintf(dst, "\t%.*s %.*s\n", unpack(BR_opNames[op.type]), unpack(BR_builtinNames[op.operand_u]));
		case BR_OPERAND_NONE:
			return fprintf(dst, "\t%.*s\n", unpack(BR_opNames[op.type]));
		default:	
			assert(false, "unknown operation type %u", op.type);
	}
}

long BR_disassembleModule(const BR_Module* module, FILE* dst)
{
	long acc = 0;
// printing the structs
	arrayForeach (BR_Struct, _struct, module->seg_typeinfo) {
		acc += printStructDecl(*_struct, dst)
			+ fputstr(dst, " {\n");
		arrayForeach (BR_Type, field, _struct->fields) {
			acc += fputstr(dst, "\t")
				+ BR_printType(*field, dst)
				+ fputstr(dst, "\n");
		}
		acc += fputstr(dst, "}\n");
	}
	acc += fputstr(dst, "\n");
// printing the data block declarations
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		acc += printDataBlockDecl(*block, dst)
			+ fputstr(dst, "\n");
	}
	acc += fputstr(dst, "\n");
// priniting the procedure declarations
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		acc += printProcDecl(*proc, dst)
			+ fputstr(dst, "\n");
	}
	acc += fputstr(dst, "\n");
// printing the data block definitions
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		acc += printDataBlockDecl(*block, dst)
			+ fputstr(dst, " {\n");
		arrayForeach (BR_Op, op, block->body) {
			acc += printOp(*op, module, dst);
		}
		acc += fputstr(dst, "}\n");
	}
	acc += fputstr(dst, "\n");
// printing procedure definitions
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		acc += printProcDecl(*proc, dst)
			+ fputstr(dst, module->exec_entry_point == (uintptr_t)(proc - module->seg_exec.data)
				? " entry {\n"
				: " {\n");
		arrayForeach (BR_Op, op, proc->body) {
			acc += printOp(*op, module, dst);
		}
		acc += fputstr(dst, "}\n");
	}
	return acc;
}
