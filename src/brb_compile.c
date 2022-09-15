// implementation for AOT compilation of BRB modules to native assembly code
#include <brb.h>
#include <errno.h>
#include <math.h>

#define ARM64_STACK_ALIGNMENT 16

static bool isForDataSegment(BRB_DataBlock* block)
{
	arrayForeach (BRB_DataPiece, piece, block->pieces) {
		if (piece->type == BRB_DP_DBADDR) return true;
	}
	return block->is_mutable;
}

static bool isForBSSSegment(BRB_DataBlock* block)
{
	arrayForeach(BRB_DataPiece, piece, block->pieces) {
		if (piece->type != BRB_DP_ZERO) return false;
	}
	return true;
}

static long compileDataPiece_darwin_arm64(const BRB_ModuleBuilder* builder, uint32_t db_id, uint32_t piece_id, FILE* dst)
{
	BRB_DataPiece* piece = &builder->module.seg_data.data[db_id].pieces.data[piece_id];
	switch (piece->type) {
		case BRB_DP_BYTES:
			return fputstr(dst, "\t.ascii ")
				+ fputsbuflnesc(dst, piece->data, BYTEFMT_HEX | BYTEFMT_DQUOTED | BYTEFMT_ESC_DQUOTE);
		case BRB_DP_TEXT:
			return fputstr(dst, "\t.asciz ")
				+ fputsbuflnesc(dst, piece->data, BYTEFMT_HEX | BYTEFMT_DQUOTED | BYTEFMT_ESC_DQUOTE);
		case BRB_DP_I16:
			return fprintf(dst, "\t.2byte %hu\n", (uint16_t)piece->content_u);
		case BRB_DP_I32:
			return fprintf(dst, "\t.4byte %u\n", (uint32_t)piece->content_u);
		case BRB_DP_PTR:
			return fprintf(dst, "\t.quad %lu\n", (uintptr_t)piece->content_u);
		case BRB_DP_I64:
			return fprintf(dst, "\t.8byte %llu\n", piece->content_u);
		case BRB_DP_DBADDR:
			return fputstr(dst, "\t.quad \"_")
				+ fputstresc(dst, builder->module.seg_data.data[piece->content_u].name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"\n");
		case BRB_DP_ZERO:
			return fprintf(dst, "\t.zero %llu\n", piece->content_u);
		case BRB_DP_BUILTIN:
			return fprintf(dst, "\t.quad %lu\n", BRB_builtinValues[piece->content_u]);
		case BRB_DP_NONE:
		case BRB_N_DP_TYPES:
			assert(false, "unknown data piece type %u", piece->type);
	}
}

typedef enum {
	LI_OFFSET,
	LI_BASE,
	LI_EXT,
	LI_2REG
} arm64_LiteralIntent;

static char* compileIntLiteral_arm64(FILE* dst, int64_t value, uint8_t reg_id, arm64_LiteralIntent intent, long* acc_p)
{
	static int64_t min_bounds[] = {
		[LI_OFFSET] = -256,
		[LI_BASE] = -4096,
		[LI_EXT] = -65536,
		[LI_2REG] = 42 // arbitrary value to make the condition below always fail
	};
	static int64_t max_bounds[] = {
		[LI_OFFSET] = 256,
		[LI_BASE] = 4096,
		[LI_EXT] = 65536,
		[LI_2REG] = -42
	};
	static char retbuf[32];

	if (inRange(value, min_bounds[intent], max_bounds[intent])) { // the condition is this one
		snprintf(retbuf, sizeof(retbuf), "%lld", value);
		return retbuf;
	} else {
		bool inverted;
		if ((inverted = value < 0)) value = -value;
		if (!(value >> 16)) {
			*acc_p += fprintf(dst,
				"\t%s x%hhu, %lld\n",
				inverted ? "movn" : "mov", reg_id, value);
		} else if (!(value >> 32)) {
			*acc_p += fprintf(dst,
				"\t%s x%hhu, %lld\n"
				"\tmovk x%hhu, %llu, lsl 16\n",
				inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
				reg_id, (inverted ? -value : value) >> 16);
		} else if (!(value >> 48)) {
			*acc_p += fprintf(dst,
				"\t%s x%hhu, %lld\n"
				"\tmovk x%hhu, %llu, lsl 16\n"
				"\tmovk x%hhu, %llu, lsl 32\n",
				inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
				reg_id, ((inverted ? -value : value) >> 16) & 0xFFFF,
				reg_id, (inverted ? -value : value) >> 32);
		} else *acc_p += fprintf(dst,
			"\t%s x%hhd, %lld\n"
			"\tmovk x%hhd, %llu, lsl 16\n"
			"\tmovk x%hhd, %llu, lsl 32\n"
			"\tmovk x%hhd, %llu, lsl 48\n",
			inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
			reg_id, ((inverted ? -value : value) >> 16) & 0xFFFF,
			reg_id, ((inverted ? -value : value) >> 32) & 0xFFFF,
			reg_id, (inverted ? -value : value) >> 48);
		return "x8";
	}
}

static long compileOp_darwin_arm64(BRB_ModuleBuilder* builder, uint32_t proc_id, uint32_t op_id, size_t vframe_offset, size_t vframe_offset_before, FILE* dst)
{
	long acc = 0;
	BRB_Op* op = &builder->module.seg_exec.data[proc_id].body.data[op_id];
	char* offset;
	switch (op->type) {
		case BRB_OP_NOP:
			return fputstr(dst, "\tnop\n");
		case BRB_OP_END:
			return fputstr(dst, "\tmov x0, 0\n\tmov x16, 1\n\tsvc 0\n");
		case BRB_OP_I8:
			compileIntLiteral_arm64(dst, op->operand_s, 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstrb w8, [sp, %s]\n", offset);
		case BRB_OP_I16:
			compileIntLiteral_arm64(dst, op->operand_s, 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstrh w8, [sp, %s]\n", offset);
		case BRB_OP_I32:
			compileIntLiteral_arm64(dst, op->operand_s, 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstr w8, [sp, %s]\n", offset);
		case BRB_OP_BUILTIN:
			op->operand_u = BRB_builtinValues[op->operand_u];
		case BRB_OP_PTR:
		case BRB_OP_I64:
			compileIntLiteral_arm64(dst, op->operand_s, 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstr x8, [sp, %s]\n", offset);
		case BRB_OP_ADDR:
			offset = compileIntLiteral_arm64(dst, vframe_offset + BRB_getStackItemRTOffset(builder, proc_id, op_id, op->operand_u), 8, LI_BASE, &acc);
			acc += fprintf(dst, "\tadd x8, sp, %s\n", offset);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstr x8, [sp, %s]\n", offset);
		case BRB_OP_DBADDR: // TODO: utilize arm64 `adr` instruction for data blocks that are put into the `text` segment
			acc += fputstr(dst, "\tadrp x8, \"_")
				+ fputstresc(dst, builder->module.seg_data.data[op->operand_u].name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"@PAGE\n\tadd x8, x8, \"_")
				+ fputstresc(dst, builder->module.seg_data.data[op->operand_u].name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"@PAGEOFF\n");
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\t str x8, [sp, %s]\n", offset);
		case BRB_OP_LD: {
			size_t load_size = BRB_getTypeRTSize(op->operand_type);
			offset = compileIntLiteral_arm64(dst, vframe_offset + load_size - sizeof(void*), 9, LI_OFFSET, &acc);
			acc += fprintf(dst, "\tldr x%c, [sp, %s]\n", load_size > 16 ? '1' : '8', offset);
			switch (load_size) {
				case 1:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldrb w10, [x8]\n\tstrb w10, [sp, %s]\n", offset);
				case 2:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldrh w10, [x8]\n\tstrh w10, [sp, %s]\n", offset);
				case 3:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst, "\tldrh w10, [x8], 2\n\tstrh w10, [sp, %s]\n", offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 2, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldrb w10, [x8]\n\tstrb w10, [sp, %s]\n", offset);
				case 4:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldr w10, [x8]\n\tstr w10, [sp, %s]\n", offset);
				case 5:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst, "\tldr w10, [x8], 4\n\tstr w10, [sp, %s]\n", offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 4, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldrb w10, [x8]\n\tstrb w10, [sp, %s]\n", offset);
				case 6:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst, "\tldr w10, [x8], 4\n\tstr w10, [sp, %s]\n", offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 4, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldrh w10, [x8]\n\tstrh w10, [sp, %s]\n", offset);
				case 7:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  w10, [x8], 4\n"
						"\tstr  w10, [x9], 4\n"
						"\tldrh w10, [x8], 2\n"
						"\tstrh w10, [x9], 2\n"
						"\tldrb w10, [x8]\n"
						"\tstrb w10, [x9]\n",
						offset);
				case 8:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldr x10, [x8]\n\tstr x10, [sp, %s]\n", offset);
				case 9:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst, "\tldr x10, [x8], 8\n\tstr x10, [sp, %s]\n", offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldrb w10, [x8]\n\tstrb w10, [sp, %s]\n", offset);
				case 10:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst, "\tldr x10, [x8], 8\n\tstr x10, [sp, %s]\n", offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldrh w10, [x8]\n\tstrh w10, [sp, %s]\n", offset);
				case 11:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  x10, [x8], 8\n"
						"\tstr  x10, [x9], 8\n"
						"\tldrh w10, [x8], 2\n"
						"\tstrh w10, [x9], 2\n"
						"\tldrb w10, [x8]\n"
						"\tstrb w10, [x9]\n",
						offset);
				case 12:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst, "\tldr x10, [x8], 8\n\tstr x10, [sp, %s]\n", offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst, "\tldr w10, [x8]\n\tstr w10, [sp, %s]\n", offset);
				case 13:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  x10, [x8], 8\n"
						"\tstr  x10, [x9], 8\n"
						"\tldr  w10, [x8], 4\n"
						"\tstr  w10, [x9], 4\n"
						"\tldrb w10, [x8]\n"
						"\tstrb w10, [x9]\n",
						offset);
				case 14:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  x10, [x8], 8\n"
						"\tstr  x10, [x9], 8\n"
						"\tldr  w10, [x8], 4\n"
						"\tstr  w10, [x9], 4\n"
						"\tldrh w10, [x8]\n"
						"\tstrh w10, [x9]\n",
						offset);
				case 15:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  x10, [x8], 8\n"
						"\tstr  x10, [x9], 8\n"
						"\tldr  w10, [x8], 4\n"
						"\tstr  w10, [x9], 4\n"
						"\tldrh w10, [x8], 2\n"
						"\tstrh w10, [x9], 2\n"
						"\tldrb w10, [x8]\n"
						"\tstrb w10, [x9]\n",
						offset);
				case 16:
					if (vframe_offset % 8 == 0 && vframe_offset <= 504)
						return acc + fprintf(dst,
							"\tldp x10, x11, [x8]\n"
							"\tstp x10, x11, [sp, %zu]\n",
							vframe_offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd x9, sp, %s\n"
						"\tldp x10, x11, [x8]\n"
						"\tstp x10, x11, [x9]\n",
						offset);
				default:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 1, LI_BASE, &acc);
					compileIntLiteral_arm64(dst, load_size, 2, LI_2REG, &acc);
					return acc + fprintf(dst,
						"\tadd x0, sp, %s\n"
						"\tcall _memmove\n",
						offset);
			}
		}
		case BRB_OP_STR: {
			size_t store_size = BRB_getStackItemRTSize(builder, proc_id, op_id - 1, 1);
			offset = compileIntLiteral_arm64(dst, vframe_offset_before, 9, LI_OFFSET, &acc);
			acc += fprintf(dst, "\tldr x%c, [sp, %s]\n", store_size > 16 ? '0' : '8', offset);
			vframe_offset -= store_size;
			switch (store_size) {
				case 1:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb w10, [sp, %s]\n"
						"\tstrb w10, [x8]\n",
						offset);
				case 2:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh w10, [sp, %s]\n"
						"\tstrh w10, [x8]\n",
						offset);
				case 3:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldrh w10, [sp, %s]\n"
						"\tstrh w10, [x8], 2\n",
						offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 2, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb w10, [sp, %s]\n"
						"\tstrb w10, [x8]\n",
						offset);
				case 4:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr w10, [sp, %s]\n"
						"\tstr w10, [x8]\n",
						offset);
				case 5:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr w10, [sp, %s]\n"
						"\tstr w10, [x8], 4\n",
						offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 4, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb w10, [sp, %s]\n"
						"\tstrb w10, [x8]\n",
						offset);
				case 6:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr w10, [sp, %s]\n"
						"\tstr w10, [x8], 4\n",
						offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 4, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh w10, [sp, %s]\n"
						"\tstrh w10, [x8]\n",
						offset);
				case 7:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  w10, [x9], 4\n"
						"\tstr  w10, [x8], 4\n"
						"\tldrh w10, [x9], 2\n"
						"\tstrh w10, [x8], 2\n"
						"\tldrb w10, [x9]\n"
						"\tstrb w10, [x8]\n",
						offset);
				case 8:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr x10, [sp, %s]\n"
						"\tstr x10, [x8]\n",
						offset);
				case 9:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr x10, [sp, %s]\n"
						"\tstr x10, [x8], 8\n",
						offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb w10, [sp, %s]\n"
						"\tstrb w10, [x8]\n",
						offset);
				case 10:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr x10, [sp, %s]\n"
						"\tstr x10, [x8], 8\n",
						offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh w10, [sp, %s]\n"
						"\tstrh w10, [x8]\n",
						offset);
				case 11:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  x10, [x9], 8\n"
						"\tstr  x10, [x8], 8\n"
						"\tldrh w10, [x9], 2\n"
						"\tstrh w10, [x8], 2\n"
						"\tldrb w10, [x9]\n"
						"\tstrb w10, [x8]\n",
						offset);
				case 12:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr x10, [sp, %s]\n"
						"\tstr x10, [x8], 8\n",
						offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr w10, [sp, %s]\n"
						"\tstr w10, [x8]\n",
						offset);
				case 13:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  x10, [x9], 8\n"
						"\tstr  x10, [x8], 8\n"
						"\tldr  w10, [x9], 4\n"
						"\tstr  w10, [x8], 4\n"
						"\tldrb w10, [x9]\n"
						"\tstrb w10, [x8]\n",
						offset);
				case 14:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  x10, [x9], 8\n"
						"\tstr  x10, [x8], 8\n"
						"\tldr  w10, [x9], 4\n"
						"\tstr  w10, [x8], 4\n"
						"\tldrh w10, [x9]\n"
						"\tstrh w10, [x8]\n",
						offset);
				case 15:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd  x9,  sp,   %s\n"
						"\tldr  x10, [x9], 8\n"
						"\tstr  x10, [x8], 8\n"
						"\tldr  w10, [x9], 4\n"
						"\tstr  w10, [x8], 4\n"
						"\tldrh w10, [x9], 2\n"
						"\tstrh w10, [x8], 2\n"
						"\tldrb w10, [x9]\n"
						"\tstrb w10, [x8]\n",
						offset);
				case 16:
					if (vframe_offset % 8 == 0 && vframe_offset <= 504)
						return acc + fprintf(dst,
							"\tldp x10, x11, [sp, %zu]\n"
							"\tstp x10, x11, [x8]\n",
							vframe_offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd x9, sp, %s\n"
						"\tldp x10, x11, [x9]\n"
						"\tstp x10, x11, [x8]\n",
						offset);
				default:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 1, LI_BASE, &acc);
					compileIntLiteral_arm64(dst, store_size, 2, LI_2REG, &acc);
					return acc + fprintf(dst,
						"\tadd x1, sp, %s\n"
						"\tbl _memmove\n",
						offset);
			}
		}
		case BRB_OP_SYS: {
			offset = compileIntLiteral_arm64(dst, vframe_offset_before, 8, LI_BASE, &acc);
			acc += fprintf(dst, "\tadd x8, sp, %s\n", offset);
			for (uint8_t i = 0; i < BRB_syscallNArgs[op->operand_u]; ++i) {
				acc += fprintf(dst, "\tldr x%hhu, [x8]%s\n", i, i < BRB_syscallNArgs[op->operand_u] - 1 ? ", 8" : "");
			}
			static sbuf sys_to_proc_name[] = {
				[BRB_SYS_EXIT] = fromcstr("_exit"),
				[BRB_SYS_WRITE] = fromcstr("_write"),
				[BRB_SYS_READ] = fromcstr("_read")
			};
			acc += fprintf(dst, "\tbl %.*s\n", unpack(sys_to_proc_name[op->operand_u]));
			if (op->operand_u != BRB_SYS_EXIT) {
				offset = compileIntLiteral_arm64(dst, vframe_offset, 8, LI_BASE, &acc);
				acc += fprintf(dst, "\tstr x0, [sp, %s]\n", offset);
			}
			return acc;
		}
		case BRB_N_OPS:
		default:
			assert(false, "invalid operation type %u", op->type);
	}
}

long BRB_compileModule_darwin_arm64(const BRB_Module* module, FILE* dst)
{
	BRB_ModuleBuilder builder;
	BRB_Error err = BRB_analyzeModule(module, &builder);
	if (err.type) {
		BRB_printErrorMsg(stderr, err, "error while analyzing module");
		abort();
	}
	
	long acc = 0;
	bool in_text_seg = true;
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		if (isForBSSSegment(block)) {
			in_text_seg = false;
			acc += fputstr(dst, ".bss\n");
		} else if (isForDataSegment(block)) {
			in_text_seg = false;
			acc += fputstr(dst, ".data\n");
		} else {
			in_text_seg = true;
			acc += fputstr(dst, ".text\n");
		}
		acc += fputstr(dst, "\"_")
			+ fputstresc(dst, block->name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
			+ fputstr(dst, "\":\n");
		for (uint32_t i = 0; i < block->pieces.length; ++i) {
			acc += compileDataPiece_darwin_arm64(&builder, block - module->seg_data.data, i, dst);
		}
	}

	acc += fputstr(dst, in_text_seg ? ".align 4\n" : ".text\n");
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
// making the label global if the proc is the entry point
		const bool is_entry = module->exec_entry_point == (uintptr_t)(proc - module->seg_exec.data);
		if (is_entry)
			acc += fputstr(dst, ".global \"_")
				+ fputstresc(dst, proc->name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
				+ fputstr(dst, "\"\n");
// generating the label
		acc += fputstr(dst, "\"_")
			+ fputstresc(dst, proc->name, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
			+ fputstr(dst, "\":\n");
		if (is_entry)
			acc += fprintf(dst, "\tmov x28, x1\n\tmov x27, x0\n");
// pre-computing the stack frame size after each operation in the procedure
		size_t sizes[proc->body.length + 1],
			max_size = 0;
		sizes[0] = BRB_getStackRTSize(&builder, proc - module->seg_exec.data, UINT32_MAX);
		for (uint32_t i = 1; i <= proc->body.length; ++i) {
			sizes[i] = BRB_getStackRTSize(&builder, proc - module->seg_exec.data, i - 1) - sizes[0];
			if (sizes[i] > max_size) max_size = sizes[i];
		}
		max_size = alignby(max_size, ARM64_STACK_ALIGNMENT);
		sizes[0] = 0;
		
		acc += fprintf(dst, "\tsub sp, sp, %zu\n", max_size);
		for (uint32_t i = 0; i < proc->body.length; ++i) {
			acc += compileOp_darwin_arm64(&builder, proc - module->seg_exec.data, i, max_size - sizes[i + 1], max_size - sizes[i], dst);
		}
		acc += fputstr(dst, is_entry
			? "\tmov x0, 0\n"
			  "\tbl _exit\n"
			: "\tret\n");
	}

	return acc;
}
