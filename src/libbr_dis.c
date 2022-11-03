// implementation of disassembly of BRB modules
#define _BR_INTERNAL
#include <br.h>
implArray(BR_Type);

long BR_printType(BR_Type type, FILE* dst)
{
	return sbuf_fput(dst, BR_typeNames[type.kind]) + (type.kind == BR_TYPE_DYNAMIC || type.n_items == 1
		? 0
		: fprintf(dst, "[%u]", type.n_items));

}
static long printStructDecl(BR_Struct _struct, FILE* dst)
{
	return str_fput(dst, "struct ")
		+ str_fputesc(dst, _struct.name
			? _struct.name
			: "", SBUF_BFMT_DQUOTED | SBUF_BFMT_HEX | SBUF_BFMT_ESC_DQUOTE);
}

static long printDataBlockDecl(BR_DataBlock block, FILE* dst)
{
	return fprintf(dst, "data%s ", block.is_mutable ? "+" : "")
		+ str_fputesc(dst, block.name, SBUF_BFMT_HEX | SBUF_BFMT_ESC_DQUOTE | SBUF_BFMT_DQUOTED);
}

static long printProcDecl(BR_Proc proc, FILE* dst)
{
	long acc = BR_printType(proc.ret_type, dst)
		+ str_fput(dst, " \"")
		+ str_fputesc(dst, proc.name, SBUF_BFMT_HEX | SBUF_BFMT_ESC_DQUOTE)
		+ str_fput(dst, "\"(");
	arrayForeach (BR_Type, arg, proc.args) {
		acc += BR_printType(*arg, dst)
			+ (arg != arrayhead(proc.args)
				? str_fput(dst, ", ")
				: 0);
	}
	return acc + str_fput(dst, ")");
}

static long printOp(BR_Op op, const BR_Module* module, FILE* dst)
{
	switch (BR_GET_OPERAND_TYPE(op.type)) {
		case BR_OPERAND_INT8:
		case BR_OPERAND_INT:
		case BR_OPERAND_VAR_NAME:
			return fprintf(dst, "\t%.*s %llu\n", sbuf_unpack(BR_opNames[op.type]), op.operand_u);
		case BR_OPERAND_TYPE:
			return fprintf(dst, "\t%.*s ", sbuf_unpack(BR_opNames[op.type]))
				+ BR_printType(op.operand_type, dst)
				+ str_fput(dst, "\n");
		case BR_OPERAND_DB_NAME:
			return fprintf(dst, "\t%.*s \"", sbuf_unpack(BR_opNames[op.type]))
				+ str_fputesc(dst, module->seg_data.data[~op.operand_s].name, SBUF_BFMT_HEX | SBUF_BFMT_ESC_DQUOTE)
				+ str_fput(dst, "\"\n");
		case BR_OPERAND_SYSCALL_NAME:
			return fprintf(dst, "\t%.*s %.*s\n", sbuf_unpack(BR_opNames[op.type]), sbuf_unpack(BR_syscallNames[op.operand_u]));
		case BR_OPERAND_BUILTIN:
			return fprintf(dst, "\t%.*s %.*s\n", sbuf_unpack(BR_opNames[op.type]), sbuf_unpack(BR_builtinNames[op.operand_u]));
		case BR_OPERAND_NONE:
			return fprintf(dst, "\t%.*s\n", sbuf_unpack(BR_opNames[op.type]));
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
			+ str_fput(dst, " {\n");
		arrayForeach (BR_Type, field, _struct->fields) {
			acc += str_fput(dst, "\t")
				+ BR_printType(*field, dst)
				+ str_fput(dst, "\n");
		}
		acc += str_fput(dst, "}\n");
	}
	acc += str_fput(dst, "\n");
// printing the data block declarations
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		acc += printDataBlockDecl(*block, dst)
			+ str_fput(dst, "\n");
	}
	acc += str_fput(dst, "\n");
// priniting the procedure declarations
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		acc += printProcDecl(*proc, dst)
			+ str_fput(dst, "\n");
	}
	acc += str_fput(dst, "\n");
// printing the data block definitions
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		acc += printDataBlockDecl(*block, dst)
			+ str_fput(dst, " {\n");
		arrayForeach (BR_Op, op, block->body) {
			acc += printOp(*op, module, dst);
		}
		acc += str_fput(dst, "}\n");
	}
	acc += str_fput(dst, "\n");
// printing procedure definitions
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		acc += printProcDecl(*proc, dst)
			+ str_fput(dst, module->exec_entry_point == (uintptr_t)(proc - module->seg_exec.data)
				? " entry {\n"
				: " {\n");
		arrayForeach (BR_Op, op, proc->body) {
			acc += printOp(*op, module, dst);
		}
		acc += str_fput(dst, "}\n");
	}
	return acc;
}
