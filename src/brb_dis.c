// implementation of disassembly of BRB modules
#define _BRB_INTERNAL
#include <brb.h>
defArray(BRB_Type);

long BRB_printType(BRB_Type type, FILE* dst)
{
	return fputsbuf(dst, BRB_typeNames[type.kind]) + (type.kind == BRB_TYPE_DYNAMIC || type.n_items == 1
		? 0
		: fprintf(dst, "[%u]", type.n_items));

}
static long printStructDecl(BRB_Struct _struct, FILE* dst)
{
	return fputstr(dst, "struct ")
		+ fputstresc(dst, _struct.name
			? _struct.name
			: "", BYTEFMT_DQUOTED | BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE);
}

static long printDataBlockDecl(BRB_DataBlock block, FILE* dst)
{
	return fprintf(dst, "data%s ", block.is_mutable ? "+" : "")
		+ fputstresc(dst, block.name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE | BYTEFMT_DQUOTED);
}

static long printProcDecl(BRB_Proc proc, FILE* dst)
{
	long acc = BRB_printType(proc.ret_type, dst)
		+ fputstr(dst, " \"")
		+ fputstresc(dst, proc.name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
		+ fputstr(dst, "\"(");
	arrayForeach (BRB_Type, arg, proc.args) {
		acc += BRB_printType(*arg, dst)
			+ (arg != arrayhead(proc.args)
				? fputstr(dst, ", ")
				: 0);
	}
	return acc + fputstr(dst, ")");
}

static long printOp(BRB_Op op, const BRB_Module* module, FILE* dst)
{
	switch (BRB_GET_OPERAND_TYPE(op.type)) {
		case BRB_OPERAND_INT8:
		case BRB_OPERAND_INT:
		case BRB_OPERAND_VAR_NAME:
			return fprintf(dst, "\t%.*s %llu\n", unpack(BRB_opNames[op.type]), op.operand_u);
		case BRB_OPERAND_TYPE:
			return fprintf(dst, "\t%.*s ", unpack(BRB_opNames[op.type]))
				+ BRB_printType(op.operand_type, dst)
				+ fputstr(dst, "\n");
		case BRB_OPERAND_DB_NAME:
			return fprintf(dst, "\t%.*s \"", unpack(BRB_opNames[op.type]))
				+ fputstresc(dst, module->seg_data.data[~op.operand_s].name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"\n");
		case BRB_OPERAND_SYSCALL_NAME:
			return fprintf(dst, "\t%.*s %.*s\n", unpack(BRB_opNames[op.type]), unpack(BRB_syscallNames[op.operand_u]));
		case BRB_OPERAND_BUILTIN:
			return fprintf(dst, "\t%.*s %.*s\n", unpack(BRB_opNames[op.type]), unpack(BRB_builtinNames[op.operand_u]));
		case BRB_OPERAND_NONE:
			return fprintf(dst, "\t%.*s\n", unpack(BRB_opNames[op.type]));
		default:	
			assert(false, "unknown operation type %u", op.type);
	}
}

long BRB_disassembleModule(const BRB_Module* module, FILE* dst)
{
	long acc = 0;
// printing the structs
	arrayForeach (BRB_Struct, _struct, module->seg_typeinfo) {
		acc += printStructDecl(*_struct, dst)
			+ fputstr(dst, " {\n");
		arrayForeach (BRB_Type, field, _struct->fields) {
			acc += fputstr(dst, "\t")
				+ BRB_printType(*field, dst)
				+ fputstr(dst, "\n");
		}
		acc += fputstr(dst, "}\n");
	}
	acc += fputstr(dst, "\n");
// printing the data block declarations
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		acc += printDataBlockDecl(*block, dst)
			+ fputstr(dst, "\n");
	}
	acc += fputstr(dst, "\n");
// priniting the procedure declarations
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		acc += printProcDecl(*proc, dst)
			+ fputstr(dst, "\n");
	}
	acc += fputstr(dst, "\n");
// printing the data block definitions
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		acc += printDataBlockDecl(*block, dst)
			+ fputstr(dst, " {\n");
		arrayForeach (BRB_Op, op, block->body) {
			acc += printOp(*op, module, dst);
		}
		acc += fputstr(dst, "}\n");
	}
	acc += fputstr(dst, "\n");
// printing procedure definitions
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		acc += printProcDecl(*proc, dst)
			+ fputstr(dst, module->exec_entry_point == (uintptr_t)(proc - module->seg_exec.data)
				? " entry {\n"
				: " {\n");
		arrayForeach (BRB_Op, op, proc->body) {
			acc += printOp(*op, module, dst);
		}
		acc += fputstr(dst, "}\n");
	}
	return acc;
}
