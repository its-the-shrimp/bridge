// implementation of disassembly of BRB modules
#include <brb.h>
defArray(BRB_Type);

static long printType(BRB_Type type, FILE* dst)
{
	return fputsbuf(dst, BRB_typeNames[type.kind])
		+ (type.n_items > 1
			? fprintf(dst, "[%u]", type.n_items)
			: 0);
}

static long printDataBlockDecl(BRB_DataBlock block, FILE* dst)
{
	return fprintf(dst, "data%s \"", block.is_mutable ? "+" : "")
		+ fputstresc(dst, block.name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
		+ fputstr(dst, "\"");

}

static long printProcDecl(BRB_Proc proc, FILE* dst)
{
	long acc = printType(proc.ret_type, dst)
		+ fputstr(dst, " \"")
		+ fputstresc(dst, proc.name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
		+ fputstr(dst, "\"(");
	arrayForeach (BRB_Type, arg, proc.args) {
		acc += printType(*arg, dst)
			+ (arg != arrayhead(proc.args)
				? fputstr(dst, ", ")
				: 0);
	}
	return acc + fputstr(dst, ")");
}

static long printDataPiece(BRB_DataPiece piece, const BRB_Module* module, FILE* dst)
{
	switch (piece.type) {
		case BRB_DP_BYTES:
		case BRB_DP_TEXT:
			return fprintf(dst, "\t%.*s \"", unpack(BRB_dataPieceNames[piece.type]))
				+ fputsbufesc(dst, piece.data, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"\n");
		case BRB_DP_I16:
		case BRB_DP_I32:
		case BRB_DP_PTR:
		case BRB_DP_I64:
			return fprintf(dst, "\t%.*s %llu\n", unpack(BRB_dataPieceNames[piece.type]), piece.content_u);
		case BRB_DP_DBADDR:
			return fprintf(dst, "\t%.*s \"", unpack(BRB_dataPieceNames[piece.type]))
				+ fputstresc(dst, module->seg_data.data[piece.content_u].name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"\n");
		case BRB_DP_ZERO:
			return fprintf(dst, "\t%.*s ", unpack(BRB_dataPieceNames[piece.type]))
				+ printType(piece.content_type, dst)
				+ fputstr(dst, "\n");
		case BRB_DP_BUILTIN:
			return fprintf(dst, "\t%.*s %.*s", unpack(BRB_dataPieceNames[piece.type]), unpack(BRB_builtinNames[piece.content_u]));
		case BRB_N_DP_TYPES:
		case BRB_DP_NONE:
			assert(false, "unknown data piece type %u", piece.type);
	}
}

static long printOp(BRB_Op op, const BRB_Module* module, FILE* dst)
{
	switch (BRB_opFlags[op.type]) {
		case BRB_OPF_OPERAND_INT8:
		case BRB_OPF_OPERAND_INT:
		case BRB_OPF_OPERAND_VAR_NAME:
			return fprintf(dst, "\t%.*s %llu\n", unpack(BRB_opNames[op.type]), op.operand_u);
		case BRB_OPF_OPERAND_TYPE:
			return fprintf(dst, "\t%.*s ", unpack(BRB_opNames[op.type]))
				+ printType(op.operand_type, dst)
				+ fputstr(dst, "\n");
		case BRB_OPF_OPERAND_DB_NAME:
			return fprintf(dst, "\t%.*s \"", unpack(BRB_opNames[op.type]))
				+ fputstresc(dst, module->seg_data.data[op.operand_u].name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"\n");
		case BRB_OPF_OPERAND_SYSCALL_NAME:
			return fprintf(dst, "\t%.*s %.*s\n", unpack(BRB_opNames[op.type]), unpack(BRB_syscallNames[op.operand_u]));
		case BRB_OPF_OPERAND_BUILTIN:
			return fprintf(dst, "\t%.*s %.*s\n", unpack(BRB_opNames[op.type]), unpack(BRB_builtinNames[op.operand_u]));
		case 0:
			return fprintf(dst, "\t%.*s\n", unpack(BRB_opNames[op.type]));
		default:	
			assert(false, "unknown operation type %u", op.type);
	}
}

long BRB_disassembleModule(const BRB_Module* module, FILE* dst)
{
// printing the data block declarations
	long acc = 0;
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
		arrayForeach (BRB_DataPiece, piece, block->pieces) {
			acc += printDataPiece(*piece, module, dst);
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

