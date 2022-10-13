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

typedef struct { // a rule specifying what literal values are allowed
	int16_t r1_min;
	int16_t r1_max;
	u16 r1_step;
	int16_t r2_min;
	int16_t r2_max;
	u16 r2_step;
} arm64_ImmValueRule;
#define ARM64_IVR(min, max, step) ((arm64_ImmValueRule){.r1_min = min, .r1_max = max, .r1_step = step, .r2_step = 1})
#define ARM64_DOUBLE_IVR(min1, max1, step1, min2, max2, step2) ((arm64_ImmValueRule){ \
	.r1_min = min1, .r1_max = max1, .r1_step = step1, \
	.r2_min = min2, .r2_max = max2, .r2_step = step2 \
})

#define ARM64_REGONLY_RULE ARM64_IVR(1, -1, 1)
#define ARM64_ADDSUB_RULE ARM64_DOUBLE_IVR( \
	-4095, 4095, 1, \
	-4095, 4095, 4096 \
)
#define ARM64_NOVALUE_RULE ((arm64_ImmValueRule){0})

static bool matchesRule(arm64_ImmValueRule rule, int64_t value)
{
	return LAZY_OR(inRange(value, (int64_t)rule.r1_min * rule.r1_step, (int64_t)rule.r1_max * rule.r1_step) && value % rule.r1_step == 0,
		inRange(value, (int64_t)rule.r2_min * rule.r2_step, (int64_t)rule.r2_max * rule.r2_step) && value % rule.r2_step == 0);
}

static uptr compileIntLiteral_arm64(FILE* dst, s64 value, u8 reg_id, arm64_ImmValueRule ivr, uptr retbuf_size, char* retbuf)
{
	if (ivr.r1_step == 0) {
		if (retbuf_size > 0) retbuf[0] = '\0';
		return 0;
	}
	if (matchesRule(ivr, value)) {
		snprintf(retbuf, retbuf_size, "%lld", value);
		return 0;
	}
	if (value == 0) {
		strlcpy(retbuf, "xzr", retbuf_size);
		return 0;
	}
	snprintf(retbuf, retbuf_size, "x%hhu", reg_id);
	bool inverted;
	if ((inverted = value < 0)) value = -value;
	if (!(value >> 16))
		return fprintf(dst,
			"\t%s\tx%hhu, %lld\n",
			inverted ? "movn" : "mov", reg_id, value);
	if (!(value >> 32))
		return fprintf(dst,
			"\t%s\tx%hhu, %lld\n"
			"\tmovk\tx%hhu, %llu, lsl 16\n",
			inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
			reg_id, (inverted ? -value : value) >> 16);
	if (!(value >> 48))
		return fprintf(dst,
			"\t%s\tx%hhu, %lld\n"
			"\tmovk\tx%hhu, %llu, lsl 16\n"
			"\tmovk\tx%hhu, %llu, lsl 32\n",
			inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
			reg_id, ((inverted ? -value : value) >> 16) & 0xFFFF,
			reg_id, (inverted ? -value : value) >> 32);
	return fprintf(dst,
		"\t%s\tx%hhd, %lld\n"
		"\tmovk\tx%hhd, %llu, lsl 16\n"
		"\tmovk\tx%hhd, %llu, lsl 32\n"
		"\tmovk\tx%hhd, %llu, lsl 48\n",
		inverted ? "movn" : "mov", reg_id, value & 0xFFFF,
		reg_id, ((inverted ? -value : value) >> 16) & 0xFFFF,
		reg_id, ((inverted ? -value : value) >> 32) & 0xFFFF,
		reg_id, (inverted ? -value : value) >> 48);
}

typedef struct { // contains specification of valid immediate values for an address offset in a certain instruction type
	arm64_ImmValueRule base;
	arm64_ImmValueRule pre;
	arm64_ImmValueRule post;
} arm64_AddrOffsetRule;

#define AOR_LDR8 ((arm64_AddrOffsetRule){ \
	.base = ARM64_IVR(-256, 4095, 1), \
	.pre = ARM64_IVR(-256, 255, 1), \
	.post = ARM64_IVR(-256, 255, 1) \
}) \

#define AOR_LDR16 ((arm64_AddrOffsetRule){ \
	.base = ARM64_DOUBLE_IVR( \
		-256, 255, 1, \
		0, 4095, 2 \
	), \
	.pre = ARM64_IVR(-256, 255, 1), \
	.post = ARM64_IVR(-256, 255, 1) \
})

#define AOR_LDR32 ((arm64_AddrOffsetRule){ \
	.base = ARM64_DOUBLE_IVR( \
		-256, 255, 1, \
		0, 4095, 4 \
	), \
	.pre = ARM64_IVR(-256, 255, 1), \
	.post = ARM64_IVR(-256, 255, 1) \
})

#define AOR_LDR64 ((arm64_AddrOffsetRule){ \
	.base = ARM64_DOUBLE_IVR( \
		-256, 255, 1, \
		0, 4095, 8 \
	), \
	.pre = ARM64_IVR(-256, 255, 1), \
	.post = ARM64_IVR(-256, 255, 1) \
})

#define AOR_LDR128 ((arm64_AddrOffsetRule){ \
	.base = ARM64_DOUBLE_IVR( \
		-256, 255, 1, \
		0, 4095, 16 \
	), \
	.pre = ARM64_IVR(-256, 255, 1), \
	.post = ARM64_IVR(-256, 255, 1) \
})

#define AOR_LDP32 ((arm64_AddrOffsetRule){ \
	.base = ARM64_IVR(-64, 63, 4), \
	.pre = ARM64_IVR(-64, 63, 4), \
	.post = ARM64_IVR(-64, 63, 4) \
})

#define AOR_LDP64 ((arm64_AddrOffsetRule){ \
	.base = ARM64_IVR(-64, 63, 8), \
	.pre = ARM64_IVR(-64, 63, 8), \
	.post = ARM64_IVR(-64, 63, 8) \
})

#define AOR_LDP128 ((arm64_AddrOffsetRule){ \
	.base = ARM64_IVR(-64, 63, 16), \
	.pre = ARM64_IVR(-64, 63, 16), \
	.post = ARM64_IVR(-64, 63, 16) \
})

#define ARM64_N_GP_REGS 29
// even though x29 and x30 are considered general-purpose registers by the arm64 specs, in practice they are used for calling procedures, thus needing special handling
typedef struct {
	BRB_ModuleBuilder builder;
	FILE* dst;
} arm64_CodegenCtx;

typedef struct {
	uint32_t last_offset;
	bool last_use;
} arm64_RegCtx;
static_assert(sizeof(arm64_RegCtx) <= sizeof(int64_t), "");

static long 
getStackAddr_arm64(arm64_CodegenCtx* ctx, const char* sp, uptr offset, u8 reg_id, arm64_RegCtx* reg_ctx, arm64_AddrOffsetRule aor, uptr retbuf_size, char* retbuf)
{
	arm64_RegCtx stub = {.last_use = true};
	if (!reg_ctx) reg_ctx = &stub;

	if (matchesRule(aor.base, offset)) {
		snprintf(retbuf, retbuf_size, "[%s, %zu]", sp, offset);
		return 0;
	}
	char offset_s[32];
	if (reg_ctx->last_use) {
		if (reg_ctx->last_offset) {
			return compileIntLiteral_arm64(ctx->dst, offset - ~reg_ctx->last_offset, reg_id, aor.base, sizeof(offset_s), offset_s)
				+ 0 * snprintf(retbuf, retbuf_size, "[x%hhu, %s]", reg_id, offset_s);
		}
		return compileIntLiteral_arm64(ctx->dst, offset - ~reg_ctx->last_offset, reg_id, ARM64_REGONLY_RULE, sizeof(offset_s), offset_s)
			+ 0 * snprintf(retbuf, retbuf_size, "[%s, %s]", sp, offset_s);
	}
	if (reg_ctx->last_offset) {
		if (matchesRule(aor.pre, offset - ~reg_ctx->last_offset))
			return 0 * snprintf(retbuf, retbuf_size, "[x%hhu, %zi]!", reg_id, offset - ~reg_ctx->last_offset);
		return compileIntLiteral_arm64(ctx->dst, offset - ~reg_ctx->last_offset, 13, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s)
			+ fprintf(ctx->dst, "\tadd\tx%hhu, x%hhu, %s\n", reg_id, reg_id, offset_s)
			+ 0 * (reg_ctx->last_offset = ~offset)
			+ 0 * snprintf(retbuf, retbuf_size, "[x%hhu]", reg_id);
	}
	return compileIntLiteral_arm64(ctx->dst, offset - ~reg_ctx->last_offset, reg_id, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s)
		+ fprintf(ctx->dst, "\tadd\tx%hhu, %s, %s\n", reg_id, sp, offset_s)
		+ 0 * (reg_ctx->last_offset = ~offset)
		+ 0 * snprintf(retbuf, retbuf_size, "[x%hhu]", reg_id);
}

static long compileOp_darwin_arm64(arm64_CodegenCtx* ctx, BRB_id proc_id, uint32_t op_id, size_t vframe_offset, size_t vframe_offset_before, FILE* dst)
{
	static arm64_AddrOffsetRule addr_rules[] = {
		[1] = AOR_LDR8,
		[2] = AOR_LDR16,
		[4] = AOR_LDR32,
		[8] = AOR_LDR64
	};
	static const char* op_postfix[] = {
		[1]  = "b\tw",
		[2]  = "h\tw",
		[4]  = "\tw",
		[8]  = "\tx",
		[16] = "\tq"
	};
	long acc = 0;
	const BRB_Op* op = BRB_getOp(&ctx->builder.module, proc_id, op_id);
	const char *sp = proc_id < 0 ? "x12" : "sp";
	char offset_s[32], value_s[32];
	switch (op->type) {
		case BRB_OP_NOP:
			return acc + fputstr(dst, "\tnop\n");
		case BRB_OP_END:
			return acc + fputstr(dst,
				"\tmov\tx0,\t0\n"
				"\tmov\tx16,\t1\n"
				"\tsvc\t0\n");
		case BRB_OP_I8:
		case BRB_OP_I16:
		case BRB_OP_I32:
		case BRB_OP_PTR:
		case BRB_OP_I64: {
			static arm64_AddrOffsetRule rules[] = {
				[BRB_OP_I8]  = AOR_LDR8,
				[BRB_OP_I16] = AOR_LDR16,
				[BRB_OP_I32] = AOR_LDR32,
				[BRB_OP_PTR] = AOR_LDR64,
				[BRB_OP_I64] = AOR_LDR64,
			};
			static char* suffixes[] = {
				[BRB_OP_I8] = "b\tw",
				[BRB_OP_I16] = "h\tw",
				[BRB_OP_I32] = "\tw",
				[BRB_OP_PTR] = "\tx",
				[BRB_OP_I64] = "\tx"
			};
			return compileIntLiteral_arm64(ctx->dst, op->operand_s, 8, ARM64_REGONLY_RULE, sizeof(value_s), value_s)
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, NULL, rules[op->type], sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tstr%s%s, %s\n", suffixes[op->type], &value_s[1], offset_s);
		}
		case BRB_OP_ADDR: {
			ssize_t offset = BRB_getStackItemRTOffset(&ctx->builder, proc_id, op_id, op->operand_u);
			return compileIntLiteral_arm64(ctx->dst, vframe_offset + offset, 8, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tadd\tx8, %s, %s\n", sp, offset_s)
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tstr\tx8, %s\n", offset_s);
		}
		case BRB_OP_DBADDR: // TODO: utilize arm64 `adr` instruction for data blocks that are put into the `text` segment
			return fputstr(ctx->dst, "\tadrp\tx8, ")
				+ printLabel(ctx->dst, ctx->builder.module.seg_data.data[~op->operand_s].name, "_")
				+ fputstr(ctx->dst, "@PAGE\n\tadd\tx8, x8, ")
				+ printLabel(ctx->dst, ctx->builder.module.seg_data.data[~op->operand_s].name, "_")
				+ fputstr(ctx->dst, "@PAGEOFF\n")
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tstr\tx8, %s\n", offset_s);
		case BRB_OP_LD:
		case BRB_OP_STR:
			assert(false, "deprecated operation, as in I don't care about it anymore");
		case BRB_OP_SYS: {
			static sbuf sys_to_proc_name[] = {
				[BRB_SYS_EXIT] = fromcstr("\tbl\t_exit\n"),
				[BRB_SYS_WRITE] = fromcstr("\tbl\t_write\n"),
				[BRB_SYS_READ] = fromcstr("\tbl\t_read\n")
			};
			arm64_RegCtx reg_ctx = {0};
			for (uint8_t i = 0; i < BRB_syscallNArgs[op->operand_u]; ++i) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + i * sizeof(uintptr_t), 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(dst, "\tldr\tx%hhu, %s\n", i, offset_s);
			}
			return acc
				+ fputsbuf(dst, sys_to_proc_name[op->operand_u])
				+ (op->operand_u == BRB_SYS_EXIT ? 0
					: getStackAddr_arm64(ctx, sp, vframe_offset, 8, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
						+ fprintf(dst, "\tstr\tx0, %s\n", offset_s));
		}
		case BRB_OP_BUILTIN:
			return acc
				+ compileIntLiteral_arm64(dst, BRB_builtinValues[op->operand_u], 8, ARM64_REGONLY_RULE, sizeof(value_s), value_s)
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
				+ fprintf(dst, "\tstr\t%s, %s\n", value_s, offset_s);
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
			static const sbuf native_op[] = {
				[BRB_OP_ADD]  = fromcstr("\tadd\tx9, x9, x10\n"),
				[BRB_OP_SUB]  = fromcstr("\tsub\tx9, x9, x10\n"),
				[BRB_OP_MUL]  = fromcstr("\tmul\tx9, x9, x10\n"),
				[BRB_OP_DIV]  = fromcstr("\tudiv\tx9, x9, x10\n"),
				[BRB_OP_DIVS] = fromcstr("\tsdiv\tx9, x9, x10\n"),
				[BRB_OP_MOD]  = fromcstr("\tudiv\tx11, x9, x10\n"
							 "\tmsub\tx9, x11, x10, x9\n"),
				[BRB_OP_MODS] = fromcstr("\tsdiv\tx11, x9, x10\n"
							 "\tmsub\tx9, x11, x10, x9\n"),
				[BRB_OP_AND]  = fromcstr("\tand\tx9, x9, x10\n"),
				[BRB_OP_OR]   = fromcstr("\torr\tx9, x9, x10\n"),
				[BRB_OP_XOR]  = fromcstr("\teor\tx9, x9, x10\n"),
				[BRB_OP_SHL]  = fromcstr("\tlsl\tx9, x9, x10\n"),
				[BRB_OP_SHR]  = fromcstr("\tlsr\tx9, x9, x10\n"),
				[BRB_OP_SHRS] = fromcstr("\tasr\tx9, x9, x10\n")
			};
			arm64_RegCtx reg_ctx = {0};
			size_t  main_op_size = BRB_getStackItemRTSize(&ctx->builder, proc_id, op_id - 1, 0),
			        op2_size     = BRB_getStackItemRTSize(&ctx->builder, proc_id, op_id - 1, 1);
			if (main_op_size == op2_size) {
				char reg_type = op2_size == 8 ? 'x' : 'w';
				arm64_AddrOffsetRule rule = op2_size == 4 ? AOR_LDP32 : AOR_LDP64;
				return acc
					+ getStackAddr_arm64(ctx, sp, vframe_offset_before, 8, &reg_ctx, rule, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldp%s9, %c10, %s\n", op_postfix[main_op_size], reg_type, offset_s)
					+ fputsbuf(dst, native_op[op->type])
					+ getStackAddr_arm64(ctx, sp, vframe_offset, 8, &reg_ctx, rule, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstr\t%c9, %s\n", reg_type, offset_s);
			}
			return acc
				+ getStackAddr_arm64(ctx, sp, vframe_offset_before, 8, &reg_ctx, addr_rules[main_op_size], sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tldr%s9, %s\n", op_postfix[main_op_size], offset_s)
				+ getStackAddr_arm64(ctx, sp, vframe_offset_before + main_op_size, 8, &reg_ctx, addr_rules[op2_size], sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tldr%s10, %s\n", op_postfix[op2_size], offset_s)
				+ fputsbuf(ctx->dst, native_op[op->type])
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 8, &reg_ctx, addr_rules[main_op_size], sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tstr%s9, %s\n", op_postfix[main_op_size], offset_s);
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
		case BRB_OP_NOT:
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
			static const arm64_ImmValueRule op2_placement[] = {
				[BRB_OP_ADD]  = ARM64_ADDSUB_RULE,
				[BRB_OP_SUB]  = ARM64_ADDSUB_RULE,
				[BRB_OP_MUL]  = ARM64_REGONLY_RULE,
				[BRB_OP_DIV]  = ARM64_REGONLY_RULE,
				[BRB_OP_DIVS] = ARM64_REGONLY_RULE,
				[BRB_OP_MOD]  = ARM64_REGONLY_RULE,
				[BRB_OP_MODS] = ARM64_REGONLY_RULE,
				[BRB_OP_AND]  = ARM64_REGONLY_RULE,
				[BRB_OP_OR]   = ARM64_REGONLY_RULE,
				[BRB_OP_XOR]  = ARM64_REGONLY_RULE,
				[BRB_OP_SHL]  = ARM64_ADDSUB_RULE,
				[BRB_OP_SHR]  = ARM64_ADDSUB_RULE,
				[BRB_OP_SHRS] = ARM64_ADDSUB_RULE,
				[BRB_OP_NOT]  = ARM64_NOVALUE_RULE
			};
			arm64_RegCtx reg_ctx = {0};
			size_t op_size = BRB_getStackItemRTSize(&ctx->builder, proc_id, op_id, 0);
			return acc
				+ getStackAddr_arm64(ctx, sp, vframe_offset_before, 9, &reg_ctx, addr_rules[op_size], sizeof(offset_s), offset_s)
				+ (BRB_GET_ADDR_OP_TYPE(op->type) == 0 ? 0
					: fprintf(ctx->dst, "\tldr\tx11, %s\n", offset_s))
				+ fprintf(ctx->dst, "\tldr%s8, %s\n",
					op_postfix[op_size], BRB_GET_ADDR_OP_TYPE(op->type) ? offset_s : "[x11]")
				+ compileIntLiteral_arm64(dst, op->operand_u, 10, op2_placement[BRB_GET_BASE_OP_TYPE(op->type)], sizeof(value_s), value_s)
				+ fprintf(ctx->dst, native_op[op->type], value_s)
				+ (BRB_GET_ADDR_OP_TYPE(op->type) == 0 ? 0
					: getStackAddr_arm64(ctx, sp, vframe_offset, 9, &reg_ctx, addr_rules[op_size], sizeof(offset_s), offset_s))
				+ fprintf(ctx->dst, "\tstr%s8, %s\n", op_postfix[op_size], offset_s);
		}
		case BRB_OP_DROP:
		case BRB_OP_NEW:
			return acc;
		case BRB_OP_ZERO: {
			arm64_RegCtx reg_ctx = {0};
			size_t span = BRB_getTypeRTSize(&ctx->builder.module, op->operand_type), orig_span = span;
			if (span >= 32) acc += fprintf(ctx->dst, "\teor\tv0.16b, v0.16b, v0.16b\n");
			while (span >= 32) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDP128, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstp\tq0, q0, %s\n", offset_s);
				span -= 32;
			}
			if (span >= 16) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDP64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstp\txzr, xzr, %s\n", offset_s);
				span -= 16;
			}
			if (span >= 8) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstr\txzr, %s\n", offset_s);
				span -= 8;
			}
			if (span >= 4) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR32, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstr\twzr, %s\n", offset_s);
				span -= 4;
			}
			if (span >= 2) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR16, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstrh\twzr, %s\n", offset_s);
				span -= 2;
			}
			if (span)
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR8, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstrb\twzr, %s\n", offset_s);
		}
		case BRB_OP_COPY: {
			size_t  span = vframe_offset_before - vframe_offset,
				orig_span = span;
			arm64_RegCtx reg_ctx = {0};
			uint8_t n_regs = 0;
			vframe_offset_before += BRB_getStackItemRTOffset(&ctx->builder, proc_id, op_id - 1, op->operand_u);
			if (span > 511) // 511 bytes is the maximum size of the object that can be copied without using `memcpy`
				return acc
					+ compileIntLiteral_arm64(ctx->dst, vframe_offset, 0, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tadd\tx0, %s, %s\n", sp, offset_s)
					+ compileIntLiteral_arm64(ctx->dst, vframe_offset_before, 1, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s) 
					+ fprintf(ctx->dst, "\tadd\tx1, %s, %s\n", sp, offset_s)
					+ compileIntLiteral_arm64(ctx->dst, span, 2, ARM64_REGONLY_RULE, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tbl\t_memcpy\n");
			while (span >= 32) {
				const uint8_t reg1 = n_regs++;
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDP128, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldp\tq%u, q%u, %s\n", reg1, n_regs++, offset_s);
				span -= 32;
			}
			if (span >= 16) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR128, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\tq%u, %s\n", n_regs++, offset_s);
				span -= 16;
			}
			if (span >= 8) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\td%u, %s\n", n_regs++, offset_s);
				span -= 8;
			}
			if (span >= 4) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR32, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\tw9, %s\n", offset_s);
				span -= 4;
			}
			if (span >= 2) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR16, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldrh\tw10, %s\n", offset_s);
				span -= 2;
			}
			if (span) 
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR8, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldrb\tw11, %s\n", offset_s);
			span = orig_span;
			n_regs = 0;
			while (span >= 32) {
				uint8_t reg1 = n_regs++;
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDP128, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstp\tq%u, q%u, %s\n", reg1, n_regs++, offset_s);
				span -= 32;
			}
			if (span >= 16) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR128, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstr\tq%u, %s\n", n_regs++, offset_s);
				span -= 16;
			}
			if (span >= 8) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstr\tq%u, %s\n", n_regs++, offset_s);
				span -= 8;
			}
			if (span >= 4) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR32, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstr\tw9, %s\n", offset_s);
				span -= 4;
			}
			if (span >= 2) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR16, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstrh\tw10, %s\n", offset_s);
				span -= 2;
			}
			if (span) 
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR8, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstrb\tw11, %s\n", offset_s);
			return acc;
		}
		case BRB_OP_COPYTO: {
			size_t  span = BRB_getStackItemRTSize(&ctx->builder, proc_id, op_id, 0),
				orig_span = span;
			arm64_RegCtx reg_ctx = {0};
			uint8_t n_regs = 0;
			if (span > 511) // 511 bytes is the maximum size of the object that can be copied without using `memcpy`
				return acc
					+ getStackAddr_arm64(ctx, sp, vframe_offset_before, 0, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\tx0, %s\n", offset_s)
					+ compileIntLiteral_arm64(ctx->dst, vframe_offset, 1, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s) 
					+ fprintf(ctx->dst, "\tadd\tx1, %s, %s\n", sp, offset_s)
					+ compileIntLiteral_arm64(ctx->dst, span, 2, ARM64_REGONLY_RULE, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tbl\t_memcpy\n");
			while (span >= 32) {
				const uint8_t reg1 = n_regs++;
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDP128, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldp\tq%u, q%u, %s\n", reg1, n_regs++, offset_s);
				span -= 32;
			}
			if (span >= 16) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR128, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\tq%u, %s\n", n_regs++, offset_s);
				span -= 16;
			}
			if (span >= 8) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\td%u, %s\n", n_regs++, offset_s);
				span -= 8;
			}
			if (span >= 4) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR32, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\tw9, %s\n", offset_s);
				span -= 4;
			}
			if (span >= 2) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR16, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldrh\tw10, %s\n", offset_s);
				span -= 2;
			}
			if (span) 
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR8, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldrb\tw11, %s\n", offset_s);
			span = orig_span;
			n_regs = 0;
			reg_ctx.last_use = true;
			acc += getStackAddr_arm64(ctx, sp, vframe_offset_before, 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tldr\tx8, %s\n", offset_s);
			while (span >= 32) {
				const uint8_t reg1 = n_regs++;
				acc += fprintf(ctx->dst, "\tstp\tq%u, q%u, [x8], 32\n", reg1, n_regs++);
				span -= 32;
			}
			if (span >= 16) {
				acc += fprintf(ctx->dst, "\tstr\tq%u, [x8], 16\n", n_regs++);
				span -= 16;
			}
			if (span >= 8) {
				acc += fprintf(ctx->dst, "\tstr\tq%u, [x8], 8\n", n_regs++);
				span -= 8;
			}
			if (span >= 4) {
				acc += fprintf(ctx->dst, "\tstr\tw9, [x8], 4\n");
				span -= 4;
			}
			if (span >= 2) {
				acc += fprintf(ctx->dst, "\tstrh\tw10, [x8], 2\n");
				span -= 2;
			}
			if (span) 
				acc += fprintf(ctx->dst, "\tstrb\tw11, [x8]\n");
			return acc;
		}
		case BRB_N_OPS:
		default:
			assert(false, "invalid operation type %u", op->type);
	}
}

long BRB_compileModule_darwin_arm64(const BRB_Module* module, FILE* dst)
{
	arm64_CodegenCtx ctx = {.dst = dst};
	BRB_Error err = BRB_analyzeModule(module, &ctx.builder);
	if (err.type) {
		BRB_printErrorMsg(stderr, err, "error while analyzing module");
		abort();
	}
// adding macros for the pseudo-instructions `ldpb` and `ldrh` which load respectively 2 bytes or half-words from memory into 2 registers
	long acc = fputsbuf(dst, fromcstr(
		".macro\tldph ra, rb, rs, off=0\n"
		"\tldr\t\\ra, \\rs, \\off\n"
		"\tlsr\t\\rb, \\ra, 16\n"
		".endmacro\n"
		".macro\tldpb ra, rb, rs, off=0\n"
		"\tldrh\t\\ra, \\rs, \\off\n"
		"\tlsr\t\\rb, \\ra, 8\n"
		".endmacro\n"));
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		acc += fputstr(dst, ".bss\n")
			+ printLabel(dst, block->name, "_")
			+ fprintf(dst, ":\n"
				"\t.zero\t%zu\n"
				".text\n"
				".align 4\n", BRB_getMaxStackRTSize(&ctx.builder, ~(block - module->seg_data.data)))
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
		sizes[0] = BRB_getStackRTSize(&ctx.builder, block - module->seg_data.data, UINT32_MAX);
		for (uint32_t i = 1; i <= block->body.length; ++i) {
			sizes[i] = BRB_getStackRTSize(&ctx.builder, ~(block - module->seg_data.data), i - 1) - sizes[0];
			if (sizes[i] > max_size) max_size = sizes[i];
		}
		sizes[0] = 0;

		for (uint32_t i = 0; i < block->body.length; ++i) {
			acc += compileOp_darwin_arm64(&ctx, ~(block - module->seg_data.data), i, max_size - sizes[i + 1], max_size - sizes[i], dst);
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
		sizes[0] = BRB_getStackRTSize(&ctx.builder, proc - module->seg_exec.data, UINT32_MAX);
		for (uint32_t i = 1; i <= proc->body.length; ++i) {
			sizes[i] = BRB_getStackRTSize(&ctx.builder, proc - module->seg_exec.data, i - 1) - sizes[0];
			if (sizes[i] > max_size) max_size = sizes[i];
		}
		max_size = alignby(max_size, ARM64_STACK_ALIGNMENT);
		sizes[0] = 0;

		char stacksize_s[16];
		acc += compileIntLiteral_arm64(dst, max_size, 0, ARM64_ADDSUB_RULE, sizeof(stacksize_s), stacksize_s)
			+ fprintf(dst, "\tsub\tsp, sp, %s\n", stacksize_s);
		for (uint32_t i = 0; i < proc->body.length; ++i) {
			acc += compileOp_darwin_arm64(&ctx, proc - module->seg_exec.data, i, max_size - sizes[i + 1], max_size - sizes[i], dst);
		}
		acc += fputstr(dst, is_entry
			? "\tmov\tx0, 0\n"
			  "\tbl\t_exit\n"
			: "\tret\n");
	}
	BRB_Module _;
	err = BRB_extractModule(ctx.builder, &_);
	assert(!err.type, "%s", BRB_getErrorMsg(err, "error while analyzing module for native assembly generation"))
	BRB_deallocDataBlocks(&_);
	BRB_deallocProcs(&_);
	BRB_deallocStructs(&_);

	return acc;
}
