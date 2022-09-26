// implementation for AOT compilation of BRB modules to native assembly code
#include <brb.h>
#include <errno.h>
#include <math.h>

#define ARM64_STACK_ALIGNMENT 16

static long printLabel(FILE* dst, const char* label, const char* prefix)
{
	return fputstr(dst, "\"")
		+ fputstr(dst, prefix)
		+ fputstresc(dst, label, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE)
		+ fputstr(dst, "\"");
}

typedef enum {
	LI_OFFSET,
	LI_BASE,
	LI_EXT,
	LI_2REG,
	LI_NONE
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
	if (intent == LI_NONE) return "";

	if (inRange(value, min_bounds[intent], max_bounds[intent])) { // the condition is this one
		snprintf(retbuf, sizeof(retbuf), "%lld", value);
		return retbuf;
	} else {
		if (intent == LI_2REG && value == 0) return "xzr";
		bool inverted;
		if ((inverted = value < 0)) value = -value;
		if (!(value >> 16)) {
			*acc_p += fprintf(dst,
				"\t%s\tx%hhu, %lld\n",
				inverted ? "movn" : "mov", reg_id, value);
		} else if (!(value >> 32)) {
			*acc_p += fprintf(dst,
				"\t%s\tx%hhu, %lld\n"
				"\tmovk\tx%hhu, %llu, lsl 16\n",
				inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
				reg_id, (inverted ? -value : value) >> 16);
		} else if (!(value >> 48)) {
			*acc_p += fprintf(dst,
				"\t%s\tx%hhu, %lld\n"
				"\tmovk\tx%hhu, %llu, lsl 16\n"
				"\tmovk\tx%hhu, %llu, lsl 32\n",
				inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
				reg_id, ((inverted ? -value : value) >> 16) & 0xFFFF,
				reg_id, (inverted ? -value : value) >> 32);
		} else *acc_p += fprintf(dst,
			"\t%s\tx%hhd, %lld\n"
			"\tmovk\tx%hhd, %llu, lsl 16\n"
			"\tmovk\tx%hhd, %llu, lsl 32\n"
			"\tmovk\tx%hhd, %llu, lsl 48\n",
			inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
			reg_id, ((inverted ? -value : value) >> 16) & 0xFFFF,
			reg_id, ((inverted ? -value : value) >> 32) & 0xFFFF,
			reg_id, (inverted ? -value : value) >> 48);
		snprintf(retbuf, sizeof(retbuf), "x%hhu", reg_id);
		return retbuf;
	}
}

#define ARM64_LDP64_OP2_MIN -512
#define ARM64_LDP64_OP2_MAX  504
#define ARM64_LDP32_OP2_MIN -256
#define ARM64_LDP32_OP2_MAX  252
#define ARM64_ADDR_OFFSET_MIN -256
#define ARM64_ADDR_OFFSET_MAX  255
static long compileOp_darwin_arm64(BRB_ModuleBuilder* builder, BRB_id_t proc_id, uint32_t op_id, size_t vframe_offset, size_t vframe_offset_before, FILE* dst)
{
	static const char* op_postfix[] = {
		[BRB_ADDR_I8]  = "b\tw",
		[BRB_ADDR_I16] = "h\tw",
		[BRB_ADDR_I32] = "\tw",
		[BRB_ADDR_PTR] = "\tx",
		[BRB_ADDR_I64] = "\tx",
	};
	static const char* rt_op_postfix[] = {
		[1] = "b\tw",
		[2] = "h\tw",
		[4] = "\tw",
		[8] = "\tx"
	};
	long acc = 0;
	const BRB_Op* op = BRB_getOp(&builder->module, proc_id, op_id);
	const char *offset, *sp = proc_id < 0 ? "x12" : "sp";
	switch (op->type) {
		case BRB_OP_NOP:
			return fputstr(dst, "\tnop\n");
		case BRB_OP_END:
			return fputstr(dst,
				"\tmov\tx0,\t0\n"
				"\tmov\tx16,\t1\n"
				"\tsvc\t0\n");
		case BRB_OP_I8:
			compileIntLiteral_arm64(dst, op->operand_s, 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstrb\tw8, [%s, %s]\n", sp, offset);
		case BRB_OP_I16:
			compileIntLiteral_arm64(dst, op->operand_s, 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstrh\tw8, [%s, %s]\n", sp, offset);
		case BRB_OP_I32:
			compileIntLiteral_arm64(dst, op->operand_s, 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstr\tw8, [%s, %s]\n", sp, offset);
		case BRB_OP_PTR:
		case BRB_OP_I64:
			compileIntLiteral_arm64(dst, op->operand_s, 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstr\tx8, [%s, %s]\n", sp, offset);
		case BRB_OP_ADDR:
			offset = compileIntLiteral_arm64(dst, vframe_offset + BRB_getStackItemRTOffset(builder, proc_id, op_id, op->operand_u), 8, LI_BASE, &acc);
			acc += fprintf(dst, "\tadd\tx8, %s, %s\n", sp, offset);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstr\tx8, [%s, %s]\n", sp, offset);
		case BRB_OP_DBADDR: // TODO: utilize arm64 `adr` instruction for data blocks that are put into the `text` segment
			acc += fputstr(dst, "\tadrp\tx8, ")
				+ printLabel(dst, builder->module.seg_data.data[~op->operand_s].name, "_")
				+ fputstr(dst, "@PAGE\n"
					"\tadd\tx8, x8, ")
				+ printLabel(dst, builder->module.seg_data.data[~op->operand_s].name, "_")
				+ fputstr(dst, "@PAGEOFF\n");
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstr\tx8, [%s, %s]\n", sp, offset);
		case BRB_OP_LD: {
			size_t load_size = BRB_getTypeRTSize(op->operand_type);
			offset = compileIntLiteral_arm64(dst, vframe_offset + load_size - sizeof(void*), 9, LI_OFFSET, &acc);
			acc += fprintf(dst, "\tldr\tx%c, [%s, %s]\n", load_size > 16 ? '1' : '8', sp, offset);
			switch (load_size) {
				case 1:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb\tw10, [x8]\n"
						"\tstrb\tw10, [%s, %s]\n", sp, offset);
				case 2:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh\tw10, [x8]\n"
						"\tstrh\tw10, [%s, %s]\n", sp, offset);
				case 3:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldrh\tw10, [x8], 2\n"
						"\tstrh\tw10, [%s, %s]\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 2, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb\tw10, [x8]\n"
						"\tstrb\tw10, [%s, %s]\n", sp, offset);
				case 4:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr\tw10, [x8]\n"
						"\tstr\tw10, [%s, %s]\n", sp, offset);
				case 5:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tw10, [x8], 4\n"
						"\tstr\tw10, [%s, %s]\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 4, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb\tw10, [x8]\n"
						"\tstrb\tw10, [sp, %s]\n", offset);
				case 6:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tw10, [x8], 4\n"
						"\tstr\tw10, [%s, %s]\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 4, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh\tw10, [x8]\n"
						"\tstrh\tw10, [%s, %s]\n", sp, offset);
				case 7:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tw10, [x8], 4\n"
						"\tstr\tw10, [x9], 4\n"
						"\tldrh\tw10, [x8], 2\n"
						"\tstrh\tw10, [x9], 2\n"
						"\tldrb\tw10, [x8]\n"
						"\tstrb\tw10, [x9]\n", sp, offset);
				case 8:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr\tx10, [x8]\n"
						"\tstr\tx10, [%s, %s]\n", sp, offset);
				case 9:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tx10, [x8], 8\n"
						"\tstr\tx10, [%s, %s]\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb\tw10, [x8]\n"
						"\tstrb\tw10, [%s, %s]\n", sp, offset);
				case 10:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tx10, [x8], 8\n"
						"\tstr\tx10, [%s, %s]\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh\tw10, [x8]\n"
						"\tstrh\tw10, [%s, %s]\n", sp, offset);
				case 11:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx10, [x8], 8\n"
						"\tstr\tx10, [x9], 8\n"
						"\tldrh\tw10, [x8], 2\n"
						"\tstrh\tw10, [x9], 2\n"
						"\tldrb\tw10, [x8]\n"
						"\tstrb\tw10, [x9]\n", sp, offset);
				case 12:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tx10, [x8], 8\n"
						"\tstr x10, [%s, %s]\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr\tw10, [x8]\n"
						"\tstr\tw10, [%s, %s]\n", sp, offset);
				case 13:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx10, [x8], 8\n"
						"\tstr\tx10, [x9], 8\n"
						"\tldr\tw10, [x8], 4\n"
						"\tstr\tw10, [x9], 4\n"
						"\tldrb\tw10, [x8]\n"
						"\tstrb\tw10, [x9]\n", sp, offset);
				case 14:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx10, [x8], 8\n"
						"\tstr\tx10, [x9], 8\n"
						"\tldr\tw10, [x8], 4\n"
						"\tstr\tw10, [x9], 4\n"
						"\tldrh\tw10, [x8]\n"
						"\tstrh\tw10, [x9]\n", sp, offset);
				case 15:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx10, [x8], 8\n"
						"\tstr\tx10, [x9], 8\n"
						"\tldr\tw10, [x8], 4\n"
						"\tstr\tw10, [x9], 4\n"
						"\tldrh\tw10, [x8], 2\n"
						"\tstrh\tw10, [x9], 2\n"
						"\tldrb\tw10, [x8]\n"
						"\tstrb\tw10, [x9]\n", sp, offset);
				case 16:
					if (vframe_offset % 8 == 0 && vframe_offset <= 504)
						return acc + fprintf(dst,
							"\tldp\tx10, x11, [x8]\n"
							"\tstp\tx10, x11, [%s, %zu]\n", sp, vframe_offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldp\tx10, x11, [x8]\n"
						"\tstp\tx10, x11, [x9]\n", sp, offset);
				default:
					compileIntLiteral_arm64(dst, load_size, 2, LI_2REG, &acc);
					offset = compileIntLiteral_arm64(dst, vframe_offset, 3, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx0, %s, %s\n"
						"\tcall\t_memmove\n", sp, offset);
			}
		}
		case BRB_OP_STR: {
			size_t store_size = BRB_getStackItemRTSize(builder, proc_id, op_id - 1, 1);
			offset = compileIntLiteral_arm64(dst, vframe_offset_before, 9, LI_OFFSET, &acc);
			acc += fprintf(dst, "\tldr\tx%c, [%s, %s]\n", store_size > 16 ? '0' : '8', sp, offset);
			vframe_offset -= store_size;
			switch (store_size) {
				case 1:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb\tw10, [%s, %s]\n"
						"\tstrb\tw10, [x8]\n", sp, offset);
				case 2:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh\tw10, [%s, %s]\n"
						"\tstrh\tw10, [x8]\n", sp, offset);
				case 3:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldrh\tw10, [%s, %s]\n"
						"\tstrh\tw10, [x8], 2\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 2, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb\tw10, [%s, %s]\n"
						"\tstrb\tw10, [x8]\n", sp, offset);
				case 4:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr\tw10, [%s, %s]\n"
						"\tstr\tw10, [x8]\n", sp, offset);
				case 5:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tw10, [%s, %s]\n"
						"\tstr\tw10, [x8], 4\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 4, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb\tw10, [%s, %s]\n"
						"\tstrb\tw10, [x8]\n", sp, offset);
				case 6:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tw10, [%s, %s]\n"
						"\tstr\tw10, [x8], 4\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 4, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh\tw10, [%s, %s]\n"
						"\tstrh\tw10, [x8]\n", sp, offset);
				case 7:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tw10, [x9], 4\n"
						"\tstr\tw10, [x8], 4\n"
						"\tldrh\tw10, [x9], 2\n"
						"\tstrh\tw10, [x8], 2\n"
						"\tldrb\tw10, [x9]\n"
						"\tstrb\tw10, [x8]\n", sp, offset);
				case 8:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr\tx10, [%s, %s]\n"
						"\tstr\tx10, [x8]\n", sp, offset);
				case 9:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tx10, [%s, %s]\n"
						"\tstr\tx10, [x8], 8\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrb\tw10, [%s, %s]\n"
						"\tstrb\tw10, [x8]\n", sp, offset);
				case 10:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tx10, [%s, %s]\n"
						"\tstr\tx10, [x8], 8\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldrh\tw10, [%s, %s]\n"
						"\tstrh\tw10, [x8]\n", sp, offset);
				case 11:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx10, [x9], 8\n"
						"\tstr\tx10, [x8], 8\n"
						"\tldrh\tw10, [x9], 2\n"
						"\tstrh\tw10, [x8], 2\n"
						"\tldrb\tw10, [x9]\n"
						"\tstrb\tw10, [x8]\n", sp, offset);
				case 12:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
					acc += fprintf(dst,
						"\tldr\tx10, [%s, %s]\n"
						"\tstr\tx10, [x8], 8\n", sp, offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset + 8, 9, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldr\tw10, [%s, %s]\n"
						"\tstr\tw10, [x8]\n", sp, offset);
				case 13:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx10, [x9], 8\n"
						"\tstr\tx10, [x8], 8\n"
						"\tldr\tw10, [x9], 4\n"
						"\tstr\tw10, [x8], 4\n"
						"\tldrb\tw10, [x9]\n"
						"\tstrb\tw10, [x8]\n", sp, offset);
				case 14:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx10, [x9], 8\n"
						"\tstr\tx10, [x8], 8\n"
						"\tldr\tw10, [x9], 4\n"
						"\tstr\tw10, [x8], 4\n"
						"\tldrh\tw10, [x9]\n"
						"\tstrh\tw10, [x8]\n", sp, offset);
				case 15:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx10, [x9], 8\n"
						"\tstr\tx10, [x8], 8\n"
						"\tldr\tw10, [x9], 4\n"
						"\tstr\tw10, [x8], 4\n"
						"\tldrh\tw10, [x9], 2\n"
						"\tstrh\tw10, [x8], 2\n"
						"\tldrb\tw10, [x9]\n"
						"\tstrb\tw10, [x8]\n", sp, offset);
				case 16:
					if (vframe_offset % 8 == 0 && vframe_offset <= 504)
						return acc + fprintf(dst,
							"\tldp\tx10, x11, [%s, %zu]\n"
							"\tstp\tx10, x11, [x8]\n", sp, vframe_offset);
					offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldp\tx10, x11, [x9]\n"
						"\tstp\tx10, x11, [x8]\n", sp, offset);
				default:
					offset = compileIntLiteral_arm64(dst, vframe_offset, 1, LI_BASE, &acc);
					compileIntLiteral_arm64(dst, store_size, 2, LI_2REG, &acc);
					return acc + fprintf(dst,
						"\tadd\tx1, %s, %s\n"
						"\tbl\t_memmove\n", sp, offset);
			}
		}
		case BRB_OP_SYS: {
			offset = compileIntLiteral_arm64(dst, vframe_offset_before, 8, LI_BASE, &acc);
			acc += fprintf(dst, "\tadd\tx8, %s, %s\n", sp, offset);
			for (uint8_t i = 0; i < BRB_syscallNArgs[op->operand_u]; ++i) {
				acc += fprintf(dst, "\tldr\tx%hhu, [x8]%s\n", i, i < BRB_syscallNArgs[op->operand_u] - 1 ? ", 8" : "");
			}
			static sbuf sys_to_proc_name[] = {
				[BRB_SYS_EXIT] = fromcstr("_exit"),
				[BRB_SYS_WRITE] = fromcstr("_write"),
				[BRB_SYS_READ] = fromcstr("_read")
			};
			acc += fprintf(dst, "\tbl\t%.*s\n", unpack(sys_to_proc_name[op->operand_u]));
			if (op->operand_u != BRB_SYS_EXIT) {
				offset = compileIntLiteral_arm64(dst, vframe_offset, 8, LI_OFFSET, &acc);
				acc += fprintf(dst, "\tstr\tx0, [%s, %s]\n", sp, offset);
			}
			return acc;
		}
		case BRB_OP_BUILTIN:
			compileIntLiteral_arm64(dst, BRB_builtinValues[op->operand_u], 8, LI_2REG, &acc);
			offset = compileIntLiteral_arm64(dst, vframe_offset, 9, LI_OFFSET, &acc);
			return acc + fprintf(dst, "\tstr\tx8, [%s, %s]\n", sp, offset);
		case BRB_OP_ADD:
		case BRB_OP_SUB:
		case BRB_OP_MUL:
		case BRB_OP_DIV:
		case BRB_OP_DIVS:
		case BRB_OP_MOD:
		case BRB_OP_MODS:
		case BRB_OP_AND:
		case BRB_OP_OR:
		case BRB_OP_XOR:
		case BRB_OP_SHL:
		case BRB_OP_SHR:
		case BRB_OP_SHRS: {
			static const char* native_op[] = {
				[BRB_OP_ADD]  = "\tadd\tx9, x9, x10\n",
				[BRB_OP_SUB]  = "\tsub\tx9, x9, x10\n",
				[BRB_OP_MUL]  = "\tmul\tx9, x9, x10\n",
				[BRB_OP_DIV]  = "\tudiv\tx9, x9, x10\n",
				[BRB_OP_DIVS] = "\tsdiv\tx9, x9, x10\n",
				[BRB_OP_MOD]  = "\tudiv\tx11, x9, x10\n"
						"\tmsub\tx9, x11, x10, x9\n",
				[BRB_OP_MODS] = "\tsdiv\tx11, x9, x10\n"
						"\tmsub\tx9, x11, x10, x9\n",
				[BRB_OP_AND]  = "\tand\tx9, x9, x10\n",
				[BRB_OP_OR]   = "\torr\tx9, x9, x10\n",
				[BRB_OP_XOR]  = "\teor\tx9, x9, x10\n",
				[BRB_OP_SHL]  = "\tlsl\tx9, x9, x10\n",
				[BRB_OP_SHR]  = "\tlsr\tx9, x9, x10\n",
				[BRB_OP_SHRS] = "\tasr\tx9, x9, x10\n"
			};
			size_t main_op_size = BRB_getStackItemRTSize(builder, proc_id, op_id - 1, 0),
				op2_size = BRB_getStackItemRTSize(builder, proc_id, op_id - 1, 1);
			if (main_op_size == op2_size && op2_size == 8) {
				if (vframe_offset_before <= ARM64_LDP64_OP2_MAX && vframe_offset_before % 8 == 0) {
					offset = compileIntLiteral_arm64(dst, vframe_offset, 8, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldp\tx9, x10, [%s, %zu]\n"
						"%s"
						"\tstr%s9, [%s, %s]\n",
						sp, vframe_offset_before,
						native_op[op->type],
						rt_op_postfix[main_op_size], sp, offset);
				} else {
					offset = compileIntLiteral_arm64(dst, vframe_offset, 8, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx8, %s, %s\n"
						"\tldp\tx9, x10, [x8], 8\n"
						"%s"
						"\tstr%s9, [x8]\n",
						sp, offset,
						native_op[op->type],
						rt_op_postfix[main_op_size]);
				}
			} else if (main_op_size == op2_size && op2_size == 4) {
				if (vframe_offset_before <= ARM64_LDP32_OP2_MAX && vframe_offset_before % 4 == 0) {
					offset = compileIntLiteral_arm64(dst, vframe_offset, 8, LI_OFFSET, &acc);
					return acc + fprintf(dst,
						"\tldp\tw9, w10, [%s, %zu]\n"
						"%s"
						"\tstr%s9, [%s, %s]\n",
						sp, vframe_offset_before,
						native_op[op->type],
						rt_op_postfix[main_op_size], sp, offset);
				} else {
					offset = compileIntLiteral_arm64(dst, vframe_offset, 8, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx8, %s, %s\n"
						"\tldp\tw9, w10, [x8], 4\n"
						"%s"
						"\tstr%s9, [%s, x8]\n",
						sp, offset,
						native_op[op->type],
						rt_op_postfix[main_op_size], sp);
				}
			} else {
				if (vframe_offset_before + main_op_size <= ARM64_ADDR_OFFSET_MAX) {
					return acc + fprintf(dst,
						"\tldr%s9, [%s, %zu]\n"
						"\tldr%s10, [%s, %zu]\n"
						"%s"
						"\tstr%s9, [%s, %zu]\n",
						rt_op_postfix[main_op_size], sp, vframe_offset_before,
						rt_op_postfix[op2_size], sp, vframe_offset_before + main_op_size,
						native_op[op->type],
						rt_op_postfix[main_op_size], sp,  vframe_offset);
				} else {
					offset = compileIntLiteral_arm64(dst, vframe_offset_before, 8, LI_BASE, &acc);
					return acc + fprintf(dst,
						"\tadd\tx8, %s, %s\n"
						"\tldr%s9, [x8], %zu\n"
						"\tldr%s10, [x8], %zi\n"
						"%s"
						"\tstr%s9, [x8]\n",
						sp, offset,
						rt_op_postfix[main_op_size], main_op_size,
						rt_op_postfix[op2_size], op2_size - main_op_size,
						native_op[op->type],
						rt_op_postfix[main_op_size]);
				}
			}
			return acc;
		}
		case BRB_OP_ADDI:
		case BRB_OP_SUBI:
		case BRB_OP_MULI:
		case BRB_OP_DIVI:
		case BRB_OP_DIVSI:
		case BRB_OP_MODI:
		case BRB_OP_MODSI:
		case BRB_OP_ANDI:
		case BRB_OP_ORI:
		case BRB_OP_XORI:
		case BRB_OP_SHLI:
		case BRB_OP_SHRI:
		case BRB_OP_SHRSI:
		case BRB_OP_NOT: {
			static const char* native_op[] = {
				[BRB_OP_ADDI]  = "\tadd\tx8, x8, %s\n",
				[BRB_OP_SUBI]  = "\tsub\tx8, x8, %s\n",
				[BRB_OP_MULI]  = "\tmul\tx8, x8, %s\n",
				[BRB_OP_DIVI]  = "\tudiv\tx8, x8, %s\n",
				[BRB_OP_DIVSI] = "\tsdiv\tx8, x8, %s\n",
				[BRB_OP_MODI]  = "\tudiv\tx11, x8, %s\n"
						 "\tmsub\tx8, x11, x10, x8\n",
				[BRB_OP_MODSI] = "\tsdiv\tx11, x8, %s\n"
						 "\tmsub\tx8, x11, x10, x8\n",
				[BRB_OP_ANDI]  = "\tand\tx8, x8, %s\n",
				[BRB_OP_ORI]   = "\torr\tx8, x8, %s\n",
				[BRB_OP_XORI]  = "\teor\tx8, x8, %s\n",
				[BRB_OP_SHLI]  = "\tlsl\tx8, x8, %s\n",
				[BRB_OP_SHRI]  = "\tlsr\tx8, x8, %s\n",
				[BRB_OP_SHRSI] = "\tasr\tx8, x8, %s\n",
				[BRB_OP_NOT]   = "\tmvn\tx8, x8%s\n"
			};
			static const arm64_LiteralIntent op2_placement[] = {
				[BRB_OP_ADDI]  = LI_BASE,
				[BRB_OP_SUBI]  = LI_BASE,
				[BRB_OP_MULI]  = LI_2REG,
				[BRB_OP_DIVI]  = LI_2REG,
				[BRB_OP_DIVSI] = LI_2REG,
				[BRB_OP_MODI]  = LI_2REG,
				[BRB_OP_MODSI] = LI_2REG,
				[BRB_OP_ANDI]  = LI_2REG,
				[BRB_OP_ORI]   = LI_2REG,
				[BRB_OP_XORI]  = LI_2REG,
				[BRB_OP_SHLI]  = LI_BASE,
				[BRB_OP_SHRI]  = LI_BASE,
				[BRB_OP_SHRSI] = LI_BASE,
				[BRB_OP_NOT]   = LI_NONE
			};
			const size_t op_size = BRB_getStackItemRTSize(builder, proc_id, op_id - 1, 0);
			if (vframe_offset <= ARM64_ADDR_OFFSET_MAX) {
				offset = compileIntLiteral_arm64(dst, op->operand_u, 10, op2_placement[op->type], &acc);
				return acc
					+ fprintf(dst, "\tldr%s8, [%s, %zu]\n", rt_op_postfix[op_size], sp, vframe_offset)
					+ fprintf(dst, native_op[op->type], offset)
					+ fprintf(dst, "\tstr%s8, [%s, %zu]\n", rt_op_postfix[op_size], sp, vframe_offset);
			} else {
				compileIntLiteral_arm64(dst, vframe_offset, 9, LI_2REG, &acc);
				offset = compileIntLiteral_arm64(dst, op->operand_u, 10, op2_placement[op->type], &acc);
				return acc
					+ fprintf(dst, "\tldr%s8, [%s, x9]\n", rt_op_postfix[op_size], sp)
					+ fprintf(dst, native_op[op->type], offset)
					+ fprintf(dst, "\tstr%s8, [%s, x9]\n", rt_op_postfix[op_size], sp);
			}
		}
		case BRB_OP_ADDIAT8:
		case BRB_OP_ADDIAT16:
		case BRB_OP_ADDIAT32:
		case BRB_OP_ADDIATP:
		case BRB_OP_ADDIAT64:
		case BRB_OP_SUBIAT8:
		case BRB_OP_SUBIAT16:
		case BRB_OP_SUBIAT32:
		case BRB_OP_SUBIATP:
		case BRB_OP_SUBIAT64:
		case BRB_OP_MULIAT8:
		case BRB_OP_MULIAT16:
		case BRB_OP_MULIAT32:
		case BRB_OP_MULIATP:
		case BRB_OP_MULIAT64:
		case BRB_OP_DIVIAT8:
		case BRB_OP_DIVIAT16:
		case BRB_OP_DIVIAT32:
		case BRB_OP_DIVIATP:
		case BRB_OP_DIVIAT64:
		case BRB_OP_DIVSIAT8:
		case BRB_OP_DIVSIAT16:
		case BRB_OP_DIVSIAT32:
		case BRB_OP_DIVSIATP:
		case BRB_OP_DIVSIAT64:
		case BRB_OP_MODIAT8:
		case BRB_OP_MODIAT16:
		case BRB_OP_MODIAT32:
		case BRB_OP_MODIATP:
		case BRB_OP_MODIAT64:
		case BRB_OP_MODSIAT8:
		case BRB_OP_MODSIAT16:
		case BRB_OP_MODSIAT32:
		case BRB_OP_MODSIATP:
		case BRB_OP_MODSIAT64:
		case BRB_OP_ANDIAT8:
		case BRB_OP_ANDIAT16:
		case BRB_OP_ANDIAT32:
		case BRB_OP_ANDIATP:
		case BRB_OP_ANDIAT64:
		case BRB_OP_ORIAT8:
		case BRB_OP_ORIAT16:
		case BRB_OP_ORIAT32:
		case BRB_OP_ORIATP:
		case BRB_OP_ORIAT64:
		case BRB_OP_XORIAT8:
		case BRB_OP_XORIAT16:
		case BRB_OP_XORIAT32:
		case BRB_OP_XORIATP:
		case BRB_OP_XORIAT64:
		case BRB_OP_SHLIAT8:
		case BRB_OP_SHLIAT16:
		case BRB_OP_SHLIAT32:
		case BRB_OP_SHLIATP:
		case BRB_OP_SHLIAT64:
		case BRB_OP_SHRIAT8:
		case BRB_OP_SHRIAT16:
		case BRB_OP_SHRIAT32:
		case BRB_OP_SHRIATP:
		case BRB_OP_SHRIAT64:
		case BRB_OP_SHRSIAT8:
		case BRB_OP_SHRSIAT16:
		case BRB_OP_SHRSIAT32:
		case BRB_OP_SHRSIATP:
		case BRB_OP_SHRSIAT64:
		case BRB_OP_NOTAT8:
		case BRB_OP_NOTAT16:
		case BRB_OP_NOTAT32:
		case BRB_OP_NOTATP:
		case BRB_OP_NOTAT64: {
			static const char* native_op[] = {
				[BRB_OP_ADD]  = "\tadd\tx8, x8, %s\n",
				[BRB_OP_SUB]  = "\tsub\tx8, x8, %s\n",
				[BRB_OP_MUL]  = "\tmul\tx8, x8, %s\n",
				[BRB_OP_DIV]  = "\tsdiv\tx8, x8, %s\n",
				[BRB_OP_DIVS] = "\tsdiv\tx8, x8, %s\n",
				[BRB_OP_MOD]  = "\tudiv\tx11, x8, %s\n"
						"\tmsub\tx8, x10, x11, x8\n",
				[BRB_OP_MODS] = "\tsdiv\tx11, x8, %s\n"
						"\tmsub\tx8, x10, x11, x8\n",
				[BRB_OP_AND]  = "\tand\tx8, x8, %s\n",
				[BRB_OP_OR]   = "\torr\tx8, x8, %s\n",
				[BRB_OP_XOR]  = "\teor\tx8, x8, %s\n",
				[BRB_OP_SHL]  = "\tlsl\tx8, x8, %s\n",
				[BRB_OP_SHR]  = "\tlsr\tx8, x8, %s\n",
				[BRB_OP_SHRS] = "\tasr\tx8, x8, %s\n",
				[BRB_OP_NOT]  = "\tmvn\tx8, x8%s\n"
			};
			static const arm64_LiteralIntent op2_placement[] = {
				[BRB_OP_ADD]  = LI_BASE,
				[BRB_OP_SUB]  = LI_BASE,
				[BRB_OP_MUL]  = LI_2REG,
				[BRB_OP_DIV]  = LI_2REG,
				[BRB_OP_DIVS] = LI_2REG,
				[BRB_OP_MOD]  = LI_2REG,
				[BRB_OP_MODS] = LI_2REG,
				[BRB_OP_AND]  = LI_2REG,
				[BRB_OP_OR]   = LI_2REG,
				[BRB_OP_XOR]  = LI_2REG,
				[BRB_OP_SHL]  = LI_BASE,
				[BRB_OP_SHR]  = LI_BASE,
				[BRB_OP_SHRS] = LI_BASE,
				[BRB_OP_NOT]  = LI_NONE
			};
			offset = compileIntLiteral_arm64(dst, op->operand_u, 10, op2_placement[BRB_GET_BASE_OP_TYPE(op->type)], &acc);
			if (vframe_offset <= ARM64_ADDR_OFFSET_MAX) {
				return acc + fprintf(dst,
						"\tldr\tx9, [%s, %zu]\n"
						"\tldr%s8, [x9]\n",
						sp, vframe_offset_before,
						op_postfix[BRB_GET_ADDR_OP_TYPE(op->type)])
					+ fprintf(dst, native_op[BRB_GET_BASE_OP_TYPE(op->type)], offset)
					+ fprintf(dst,
						"\tstr%s8, [x9]\n"
						"\tstr%s8, [%s, %zu]\n",
						op_postfix[BRB_GET_ADDR_OP_TYPE(op->type)],
						op_postfix[BRB_GET_ADDR_OP_TYPE(op->type)], sp, vframe_offset);
			} else {
				static uint8_t native_offset[] = {
					[BRB_ADDR_I8]  = 7,
					[BRB_ADDR_I16] = 6,
					[BRB_ADDR_I32] = 4,
					[BRB_ADDR_PTR] = 0,
					[BRB_ADDR_I64] = 0,
				};
				offset = compileIntLiteral_arm64(dst, vframe_offset_before, 9, LI_BASE, &acc);
				return acc + fprintf(dst,
						"\tadd\tx9, %s, %s\n"
						"\tldr\tx13, [x9], %hhu\n"
						"\tldr%s8, [x13]\n",
						sp, offset,
						native_offset[BRB_GET_ADDR_OP_TYPE(op->type)],
						op_postfix[BRB_GET_ADDR_OP_TYPE(op->type)])
					+ fprintf(dst, native_op[BRB_GET_BASE_OP_TYPE(op->type)], offset)
					+ fprintf(dst,
						"\tstr%s8, [x13]\n"
						"\tstr%s8, [x9]\n",
						op_postfix[BRB_GET_ADDR_OP_TYPE(op->type)],
						op_postfix[BRB_GET_ADDR_OP_TYPE(op->type)]);
			}
		}
		case BRB_OP_DROP:
			return acc;
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
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		acc += fputstr(dst, ".bss\n")
			+ printLabel(dst, block->name, "_")
			+ fprintf(dst, ":\n"
				"\t.zero\t%zu\n"
				".text\n"
				".align 4\n", BRB_getMaxStackRTSize(&builder, ~(block - module->seg_data.data)))
			+ printLabel(dst, block->name, ".brb_db_impl_")
			+ fputstr(dst, ":\n"
				"\tadrp\tx12, ")
			+ printLabel(dst, block->name, "_")
			+ fputstr(dst, "@PAGE\n"
				"\tadd\tx12, x12, ")
			+ printLabel(dst, block->name, "_")
			+ fputstr(dst, "@PAGEOFF\n");
		size_t sizes[block->body.length + 1],
			max_size = 0;
		sizes[0] = BRB_getStackRTSize(&builder, block - module->seg_data.data, UINT32_MAX);
		for (uint32_t i = 1; i <= block->body.length; ++i) {
			sizes[i] = BRB_getStackRTSize(&builder, ~(block - module->seg_data.data), i - 1) - sizes[0];
			if (sizes[i] > max_size) max_size = sizes[i];
		}
		sizes[0] = 0;

		for (uint32_t i = 0; i < block->body.length; ++i) {
			acc += compileOp_darwin_arm64(&builder, ~(block - module->seg_data.data), i, max_size - sizes[i + 1], max_size - sizes[i], dst);
		}
		acc += fputstr(dst, "\tret\n");
	}

	if (!module->seg_data.length) acc += fputstr(dst, ".text\n");
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
// making the label global if the proc is the entry point
		const bool is_entry = module->exec_entry_point == (uintptr_t)(proc - module->seg_exec.data);
// generating the label
		acc += printLabel(dst, proc->name, "_")
			+ fputstr(dst, ":\n");
		if (is_entry) {
			acc += fputstr(dst, ".global ")
				+ printLabel(dst, proc->name, "_")
				+ fputstr(dst, "\n"
					"\tmov\tx28, x1\n"
					"\tmov\tx27, x0\n");
			arrayForeach (BRB_DataBlock, block, module->seg_data) {
				acc += fputstr(dst, "\tbl\t")
					+ printLabel(dst, block->name, ".brb_db_impl_")
					+ fputstr(dst, "\n");
			}
		}
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

		acc += fprintf(dst, "\tsub\tsp, sp, %zu\n", max_size);
		for (uint32_t i = 0; i < proc->body.length; ++i) {
			acc += compileOp_darwin_arm64(&builder, proc - module->seg_exec.data, i, max_size - sizes[i + 1], max_size - sizes[i], dst);
		}
		acc += fputstr(dst, is_entry
			? "\tmov\tx0, 0\n"
			  "\tbl\t_exit\n"
			: "\tret\n");
	}
	BRB_Module _;
	err = BRB_extractModule(builder, &_);
	assert(!err.type, "%s", BRB_getErrorMsg(err, "error while analyzing module for native assembly generation"))
	BRB_deallocDataBlocks(&_);
	BRB_deallocProcs(&_);

	return acc;
}
