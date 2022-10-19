// implementation for AOT compilation of BRB modules to native assembly code
#include <br.h>
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
	BR_ModuleBuilder builder;
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

static long compileOp_darwin_arm64(arm64_CodegenCtx* ctx, BR_id proc_id, uint32_t op_id, size_t vframe_offset, size_t vframe_offset_before, FILE* dst)
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
	const BR_Op* op = BR_getOp(&ctx->builder.module, proc_id, op_id);
	const char *sp = proc_id < 0 ? "x12" : "sp";
	char offset_s[32], value_s[32];
	switch (op->type) {
		case BR_OP_NOP:
			return acc + fputstr(dst, "\tnop\n");
		case BR_OP_END:
			return acc + fputstr(dst,
				"\tmov\tx0,\t0\n"
				"\tmov\tx16,\t1\n"
				"\tsvc\t0\n");
		case BR_OP_I8:
		case BR_OP_I16:
		case BR_OP_I32:
		case BR_OP_PTR:
		case BR_OP_I64: {
			static arm64_AddrOffsetRule rules[] = {
				[BR_OP_I8]  = AOR_LDR8,
				[BR_OP_I16] = AOR_LDR16,
				[BR_OP_I32] = AOR_LDR32,
				[BR_OP_PTR] = AOR_LDR64,
				[BR_OP_I64] = AOR_LDR64,
			};
			static char* suffixes[] = {
				[BR_OP_I8] = "b\tw",
				[BR_OP_I16] = "h\tw",
				[BR_OP_I32] = "\tw",
				[BR_OP_PTR] = "\tx",
				[BR_OP_I64] = "\tx"
			};
			return compileIntLiteral_arm64(ctx->dst, op->operand_s, 8, ARM64_REGONLY_RULE, sizeof(value_s), value_s)
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, NULL, rules[op->type], sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tstr%s%s, %s\n", suffixes[op->type], &value_s[1], offset_s);
		}
		case BR_OP_ADDR: {
			ssize_t offset = BR_getStackItemRTOffset(&ctx->builder, proc_id, op_id, op->operand_u);
			return compileIntLiteral_arm64(ctx->dst, vframe_offset + offset, 8, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tadd\tx8, %s, %s\n", sp, offset_s)
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tstr\tx8, %s\n", offset_s);
		}
		case BR_OP_DBADDR: // TODO: utilize arm64 `adr` instruction for data blocks that are put into the `text` segment
			return fputstr(ctx->dst, "\tadrp\tx8, ")
				+ printLabel(ctx->dst, ctx->builder.module.seg_data.data[~op->operand_s].name, "_")
				+ fputstr(ctx->dst, "@PAGE\n\tadd\tx8, x8, ")
				+ printLabel(ctx->dst, ctx->builder.module.seg_data.data[~op->operand_s].name, "_")
				+ fputstr(ctx->dst, "@PAGEOFF\n")
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tstr\tx8, %s\n", offset_s);
		case BR_OP_SYS: {
			static sbuf sys_to_proc_name[] = {
				[BR_SYS_EXIT] = fromcstr("\tbl\t_exit\n"),
				[BR_SYS_WRITE] = fromcstr("\tbl\t_write\n"),
				[BR_SYS_READ] = fromcstr("\tbl\t_read\n")
			};
			arm64_RegCtx reg_ctx = {0};
			for (uint8_t i = 0; i < BR_syscallNArgs[op->operand_u]; ++i) {
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before + i * sizeof(uintptr_t), 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(dst, "\tldr\tx%hhu, %s\n", i, offset_s);
			}
			return acc
				+ fputsbuf(dst, sys_to_proc_name[op->operand_u])
				+ (op->operand_u == BR_SYS_EXIT ? 0
					: getStackAddr_arm64(ctx, sp, vframe_offset, 8, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
						+ fprintf(dst, "\tstr\tx0, %s\n", offset_s));
		}
		case BR_OP_BUILTIN:
			return acc
				+ compileIntLiteral_arm64(dst, BR_builtinValues[op->operand_u], 8, ARM64_REGONLY_RULE, sizeof(value_s), value_s)
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
				+ fprintf(dst, "\tstr\t%s, %s\n", value_s, offset_s);
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
		case BR_OP_SHRS: {
			static const sbuf native_op[] = {
				[BR_OP_ADD]  = fromcstr("\tadd\tx9, x9, x10\n"),
				[BR_OP_SUB]  = fromcstr("\tsub\tx9, x9, x10\n"),
				[BR_OP_MUL]  = fromcstr("\tmul\tx9, x9, x10\n"),
				[BR_OP_DIV]  = fromcstr("\tudiv\tx9, x9, x10\n"),
				[BR_OP_DIVS] = fromcstr("\tsdiv\tx9, x9, x10\n"),
				[BR_OP_MOD]  = fromcstr("\tudiv\tx11, x9, x10\n"
							 "\tmsub\tx9, x11, x10, x9\n"),
				[BR_OP_MODS] = fromcstr("\tsdiv\tx11, x9, x10\n"
							 "\tmsub\tx9, x11, x10, x9\n"),
				[BR_OP_AND]  = fromcstr("\tand\tx9, x9, x10\n"),
				[BR_OP_OR]   = fromcstr("\torr\tx9, x9, x10\n"),
				[BR_OP_XOR]  = fromcstr("\teor\tx9, x9, x10\n"),
				[BR_OP_SHL]  = fromcstr("\tlsl\tx9, x9, x10\n"),
				[BR_OP_SHR]  = fromcstr("\tlsr\tx9, x9, x10\n"),
				[BR_OP_SHRS] = fromcstr("\tasr\tx9, x9, x10\n")
			};
			arm64_RegCtx reg_ctx = {0};
			size_t  main_op_size = BR_getStackItemRTSize(&ctx->builder, proc_id, op_id - 1, 0),
			        op2_size     = BR_getStackItemRTSize(&ctx->builder, proc_id, op_id - 1, 1);
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
		case BR_OP_NOTAT64: {
			static const BR_OpType base_op[] = {
				[BR_OP_ADDI]      = BR_OP_ADD,
				[BR_OP_SUBI]      = BR_OP_SUB,
				[BR_OP_MULI]      = BR_OP_MUL,
				[BR_OP_DIVI]      = BR_OP_DIV,
				[BR_OP_DIVSI]     = BR_OP_DIVS,
				[BR_OP_MODI]      = BR_OP_MOD,
				[BR_OP_MODSI]     = BR_OP_MODS,
				[BR_OP_ANDI]      = BR_OP_AND,
				[BR_OP_ORI]       = BR_OP_OR,
				[BR_OP_XORI]      = BR_OP_XOR,
				[BR_OP_SHLI]      = BR_OP_SHL,
				[BR_OP_SHRI]      = BR_OP_SHR,
				[BR_OP_SHRSI]     = BR_OP_SHRS,
				[BR_OP_NOT]       = BR_OP_NOT,
				[BR_OP_ADDIAT8]   = BR_OP_ADD,
				[BR_OP_ADDIAT16]  = BR_OP_ADD,
				[BR_OP_ADDIAT32]  = BR_OP_ADD,
				[BR_OP_ADDIATP]   = BR_OP_ADD,
				[BR_OP_ADDIAT64]  = BR_OP_ADD,
				[BR_OP_SUBIAT8]   = BR_OP_SUB,
				[BR_OP_SUBIAT16]  = BR_OP_SUB,
				[BR_OP_SUBIAT32]  = BR_OP_SUB,
				[BR_OP_SUBIATP]   = BR_OP_SUB,
				[BR_OP_SUBIAT64]  = BR_OP_SUB,
				[BR_OP_MULIAT8]   = BR_OP_MUL,
				[BR_OP_MULIAT16]  = BR_OP_MUL,
				[BR_OP_MULIAT32]  = BR_OP_MUL,
				[BR_OP_MULIATP]   = BR_OP_MUL,
				[BR_OP_MULIAT64]  = BR_OP_MUL,
				[BR_OP_DIVIAT8]   = BR_OP_DIV,
				[BR_OP_DIVIAT16]  = BR_OP_DIV,
				[BR_OP_DIVIAT32]  = BR_OP_DIV,
				[BR_OP_DIVIATP]   = BR_OP_DIV,
				[BR_OP_DIVIAT64]  = BR_OP_DIV,
				[BR_OP_DIVSIAT8]  = BR_OP_DIVS,
				[BR_OP_DIVSIAT16] = BR_OP_DIVS,
				[BR_OP_DIVSIAT32] = BR_OP_DIVS,
				[BR_OP_DIVSIATP]  = BR_OP_DIVS,
				[BR_OP_DIVSIAT64] = BR_OP_DIVS,
				[BR_OP_MODIAT8]   = BR_OP_MOD,
				[BR_OP_MODIAT16]  = BR_OP_MOD,
				[BR_OP_MODIAT32]  = BR_OP_MOD,
				[BR_OP_MODIATP]   = BR_OP_MOD,
				[BR_OP_MODIAT64]  = BR_OP_MOD,
				[BR_OP_MODSIAT8]  = BR_OP_MODS,
				[BR_OP_MODSIAT16] = BR_OP_MODS,
				[BR_OP_MODSIAT32] = BR_OP_MODS,
				[BR_OP_MODSIATP]  = BR_OP_MODS,
				[BR_OP_MODSIAT64] = BR_OP_MODS,
				[BR_OP_ANDIAT8]   = BR_OP_AND,
				[BR_OP_ANDIAT16]  = BR_OP_AND,
				[BR_OP_ANDIAT32]  = BR_OP_AND,
				[BR_OP_ANDIATP]   = BR_OP_AND,
				[BR_OP_ANDIAT64]  = BR_OP_AND,
				[BR_OP_ORIAT8]    = BR_OP_OR,
				[BR_OP_ORIAT16]   = BR_OP_OR,
				[BR_OP_ORIAT32]   = BR_OP_OR,
				[BR_OP_ORIATP]    = BR_OP_OR,
				[BR_OP_ORIAT64]   = BR_OP_OR,
				[BR_OP_XORIAT8]   = BR_OP_XOR,
				[BR_OP_XORIAT16]  = BR_OP_XOR,
				[BR_OP_XORIAT32]  = BR_OP_XOR,
				[BR_OP_XORIATP]   = BR_OP_XOR,
				[BR_OP_XORIAT64]  = BR_OP_XOR,
				[BR_OP_SHLIAT8]   = BR_OP_SHL,
				[BR_OP_SHLIAT16]  = BR_OP_SHL,
				[BR_OP_SHLIAT32]  = BR_OP_SHL,
				[BR_OP_SHLIATP]   = BR_OP_SHL,
				[BR_OP_SHLIAT64]  = BR_OP_SHL,
				[BR_OP_SHRIAT8]   = BR_OP_SHR,
				[BR_OP_SHRIAT16]  = BR_OP_SHR,
				[BR_OP_SHRIAT32]  = BR_OP_SHR,
				[BR_OP_SHRIATP]   = BR_OP_SHR,
				[BR_OP_SHRIAT64]  = BR_OP_SHR,
				[BR_OP_SHRSIAT8]  = BR_OP_SHRS,
				[BR_OP_SHRSIAT16] = BR_OP_SHRS,
				[BR_OP_SHRSIAT32] = BR_OP_SHRS,
				[BR_OP_SHRSIATP]  = BR_OP_SHRS,
				[BR_OP_SHRSIAT64] = BR_OP_SHRS,
				[BR_OP_NOTAT8]    = BR_OP_NOT,
				[BR_OP_NOTAT16]   = BR_OP_NOT,
				[BR_OP_NOTAT32]   = BR_OP_NOT,
				[BR_OP_NOTATP]    = BR_OP_NOT,
				[BR_OP_NOTAT64]   = BR_OP_NOT
			};
			static const char* native_op[] = {
				[BR_OP_ADD]  = "\tadd\tx8, x8, %s\n",
				[BR_OP_SUB]  = "\tsub\tx8, x8, %s\n",
				[BR_OP_MUL]  = "\tmul\tx8, x8, %s\n",
				[BR_OP_DIV]  = "\tudiv\tx8, x8, %s\n",
				[BR_OP_DIVS] = "\tsdiv\tx8, x8, %s\n",
				[BR_OP_MOD]  = "\tudiv\tx14, x8, %s\n"
						"\tmsub\tx8, x14, x10, x8\n",
				[BR_OP_MODS] = "\tsdiv\tx14, x8, %s\n"
						"\tmsub\tx8, x14, x10, x8\n",
				[BR_OP_AND]  = "\tand\tx8, x8, %s\n",
				[BR_OP_OR]   = "\torr\tx8, x8, %s\n",
				[BR_OP_XOR]  = "\teor\tx8, x8, %s\n",
				[BR_OP_SHL]  = "\tlsl\tx8, x8, %s\n",
				[BR_OP_SHR]  = "\tlsr\tx8, x8, %s\n",
				[BR_OP_SHRS] = "\tasr\tx8, x8, %s\n",
				[BR_OP_NOT]  = "\tmvn\tx8, x8%s\n"
			};
			static const arm64_ImmValueRule op2_placement[] = {
				[BR_OP_ADD]  = ARM64_ADDSUB_RULE,
				[BR_OP_SUB]  = ARM64_ADDSUB_RULE,
				[BR_OP_MUL]  = ARM64_REGONLY_RULE,
				[BR_OP_DIV]  = ARM64_REGONLY_RULE,
				[BR_OP_DIVS] = ARM64_REGONLY_RULE,
				[BR_OP_MOD]  = ARM64_REGONLY_RULE,
				[BR_OP_MODS] = ARM64_REGONLY_RULE,
				[BR_OP_AND]  = ARM64_REGONLY_RULE,
				[BR_OP_OR]   = ARM64_REGONLY_RULE,
				[BR_OP_XOR]  = ARM64_REGONLY_RULE,
				[BR_OP_SHL]  = ARM64_ADDSUB_RULE,
				[BR_OP_SHR]  = ARM64_ADDSUB_RULE,
				[BR_OP_SHRS] = ARM64_ADDSUB_RULE,
				[BR_OP_NOT]  = ARM64_NOVALUE_RULE
			};
			arm64_RegCtx reg_ctx = {0};
			size_t op_size = BR_getStackItemRTSize(&ctx->builder, proc_id, op_id, 0);
			return acc
				+ getStackAddr_arm64(ctx, sp, vframe_offset_before, 9, &reg_ctx, addr_rules[op_size], sizeof(offset_s), offset_s)
				+ (BR_GET_ADDR_OP_TYPE(op->type) == 0 ? 0
					: fprintf(ctx->dst, "\tldr\tx11, %s\n", offset_s))
				+ fprintf(ctx->dst, "\tldr%s8, %s\n", op_postfix[op_size], BR_GET_ADDR_OP_TYPE(op->type) == 0 ? offset_s : "[x11]")
				+ compileIntLiteral_arm64(dst, op->operand_u, 10, op2_placement[base_op[op->type]], sizeof(value_s), value_s)
				+ fprintf(ctx->dst, native_op[base_op[op->type]], value_s)
				+ (BR_GET_ADDR_OP_TYPE(op->type) == 0 ? 0
					: fprintf(ctx->dst, "\tstr%s8, [x11]\n", op_postfix[op_size]))
				+ getStackAddr_arm64(ctx, sp, vframe_offset, 9, (reg_ctx.last_use = true, &reg_ctx), addr_rules[op_size], sizeof(offset_s), offset_s)
				+ fprintf(ctx->dst, "\tstr%s8, %s\n", op_postfix[op_size], offset_s);
		}
		case BR_OP_DROP:
		case BR_OP_NEW:
			return acc;
		case BR_OP_ZERO: {
			arm64_RegCtx reg_ctx = {0};
			size_t span = BR_getTypeRTSize(&ctx->builder.module, op->operand_type), orig_span = span;
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
			reg_ctx.last_use = true;
			if (span)
				acc += getStackAddr_arm64(ctx, sp, vframe_offset + (orig_span - span), 8, &reg_ctx, AOR_LDR8, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tstrb\twzr, %s\n", offset_s);
			return acc;
		}
		case BR_OP_GET:
		case BR_OP_SETAT:
		case BR_OP_GETFROM:
		case BR_OP_COPY: {
			static const bool output_to_stack[] = {
				[BR_OP_GET]     = true,
				[BR_OP_SETAT]   = false,
				[BR_OP_GETFROM] = true,
				[BR_OP_COPY]    = false
			};
			static const bool input_from_stack[] = {
				[BR_OP_GET]     = true,
				[BR_OP_SETAT]   = true,
				[BR_OP_GETFROM] = false,
				[BR_OP_COPY]    = false
			};
			size_t  span = BR_getStackItemRTSize(&ctx->builder, proc_id, op_id, 0),
				orig_span = span;
			arm64_RegCtx reg_ctx = {0};
			uint8_t n_regs = 0;
			if (op->type != BR_OP_GETFROM)
				vframe_offset_before += op->type == BR_OP_GET ? BR_getStackItemRTOffset(&ctx->builder, proc_id, op_id - 1, op->operand_u) : 8;
			if (span > 511) // 511 bytes is the maximum size of the object that can be copied without using `memcpy`
				return acc
					+ (output_to_stack[op->type]
						? compileIntLiteral_arm64(ctx->dst, vframe_offset, 0, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s)
							+ fprintf(ctx->dst, "\tadd\tx0, %s, %s\n", sp, offset_s)
						: getStackAddr_arm64(ctx, sp, vframe_offset_before, 0, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
							+ fprintf(ctx->dst, "\tldr\tx0, %s\n", offset_s))
					+ (input_from_stack[op->type]
						? compileIntLiteral_arm64(ctx->dst, vframe_offset_before, 1, ARM64_ADDSUB_RULE, sizeof(offset_s), offset_s)
							+ fprintf(ctx->dst, "\tadd\tx1, %s, %s\n", sp, offset_s)
						: getStackAddr_arm64(ctx, sp, vframe_offset_before, 1, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
							+ fprintf(ctx->dst, "\tldr\tx1, %s\n", offset_s))
					+ compileIntLiteral_arm64(ctx->dst, span, 2, ARM64_REGONLY_RULE, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tbl\t_memcpy\n");
			if (!input_from_stack[op->type])
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before, 8, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\tx8, %s\n", offset_s);
			while (span >= 32) {
				const uint8_t reg1 = n_regs++;
				acc += input_from_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDP128, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tldp\tq%u, q%u, %s\n", reg1, n_regs++, offset_s)
					: fprintf(ctx->dst, "\tldp\tq%u q%u, [x8], 32\n", reg1, n_regs++);
				span -= 32;
			}
			if (span >= 16) {
				acc += input_from_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR128, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tldr\tq%u, %s\n", n_regs++, offset_s)
					: fprintf(ctx->dst, "\tldr\tq%u, [x8], 16\n", n_regs++);
				span -= 16;
			}
			if (span >= 8) {
				acc += input_from_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tldr\td%u, %s\n", n_regs++, offset_s)
					: fprintf(ctx->dst, "\tldr\td%u, [x8], 8\n", n_regs++);
				span -= 8;
			}
			if (span >= 4) {
				acc += input_from_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR32, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tldr\tw9, %s\n", offset_s)
					: fprintf(ctx->dst, "\tldr\tw9, [x8], 4\n");
				span -= 4;
			}
			if (span >= 2) {
				acc += input_from_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR16, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tldrh\tw10, %s\n", offset_s)
					: fprintf(ctx->dst, "\tldrh\tw10, [x8], 2\n");
				span -= 2;
			}
			if (span) 
				acc += input_from_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR8, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tldrb\tw11, %s\n", offset_s)
					: fprintf(ctx->dst, "\tldrb\tw11, [x8]\n");
			span = orig_span;
			n_regs = 0;
			if (!output_to_stack[op->type])
				acc += getStackAddr_arm64(ctx, sp, vframe_offset_before - 8, 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\tx8, %s\n", offset_s)
					+ (op->type != BR_OP_COPY ? 0
						: fprintf(ctx->dst, "\tmov\tx14, x8\n"));
			while (span >= 32) {
				uint8_t reg1 = n_regs++;
				acc += output_to_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDP128, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tstp\tq%u, q%u, %s\n", reg1, n_regs++, offset_s)
					: fprintf(ctx->dst, "\tstp\tq%u, q%u, [x8], 32\n", reg1, n_regs++);
				span -= 32;
			}
			if (span >= 16) {
				acc += output_to_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR128, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tstr\tq%u, %s\n", n_regs++, offset_s)
					: fprintf(ctx->dst, "\tstr\tq%u, [x8], 16\n", n_regs++);
				span -= 16;
			}
			if (span >= 8) {
				acc += output_to_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR64, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tstr\td%u, %s\n", n_regs++, offset_s)
					: fprintf(ctx->dst, "\tstr\td%u, [x8], 8\n", n_regs++);
				span -= 8;
			}
			if (span >= 4) {
				acc += output_to_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR32, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tstr\tw9, %s\n", offset_s)
					: fprintf(ctx->dst, "\tstr\tw9, [x8], 4\n");
				span -= 4;
			}
			if (span >= 2) {
				acc += output_to_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR16, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tstrh\tw10, %s\n", offset_s)
					: fprintf(ctx->dst, "\tstrh\tw10, [x8], 2\n");
				span -= 2;
			}
			reg_ctx.last_use = true;
			if (span) 
				acc += output_to_stack[op->type]
					? getStackAddr_arm64(ctx, sp, vframe_offset_before + (orig_span - span), 8, &reg_ctx, AOR_LDR8, sizeof(offset_s), offset_s)
						+ fprintf(ctx->dst, "\tstrb\tw11, %s\n", offset_s)
					: fprintf(ctx->dst, "\tstrb\tw11, [x8]\n");
			if (op->type == BR_OP_COPY)
				acc += getStackAddr_arm64(ctx, sp, vframe_offset, 8, NULL, AOR_LDR64, sizeof(offset_s), offset_s)
					+ fprintf(ctx->dst, "\tldr\tx14, %s\n", offset_s);
			return acc;
		}
		case BR_N_OPS:
		default:
			assert(false, "invalid operation type %u", op->type);
	}
}

long BR_compileModule_darwin_arm64(const BR_Module* module, FILE* dst)
{
	arm64_CodegenCtx ctx = {.dst = dst};
	BR_Error err = BR_analyzeModule(module, &ctx.builder);
	if (err.type) {
		BR_printErrorMsg(stderr, err, "error while analyzing module");
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
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		acc += fputstr(dst, ".bss\n")
			+ printLabel(dst, block->name, "_")
			+ fprintf(dst, ":\n"
				"\t.zero\t%zu\n"
				".text\n"
				".align 4\n", BR_getMaxStackRTSize(&ctx.builder, ~(block - module->seg_data.data)))
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
		sizes[0] = BR_getStackRTSize(&ctx.builder, block - module->seg_data.data, UINT32_MAX);
		for (uint32_t i = 1; i <= block->body.length; ++i) {
			sizes[i] = BR_getStackRTSize(&ctx.builder, ~(block - module->seg_data.data), i - 1) - sizes[0];
			if (sizes[i] > max_size) max_size = sizes[i];
		}
		sizes[0] = 0;

		for (uint32_t i = 0; i < block->body.length; ++i) {
			acc += compileOp_darwin_arm64(&ctx, ~(block - module->seg_data.data), i, max_size - sizes[i + 1], max_size - sizes[i], dst);
		}
		acc += fputstr(dst, "\tret\n");
	}

	if (!module->seg_data.length) acc += fputstr(dst, ".text\n");
	arrayForeach (BR_Proc, proc, module->seg_exec) {
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
			arrayForeach (BR_DataBlock, block, module->seg_data) {
				acc += fputstr(dst, "\tbl\t")
					+ printLabel(dst, block->name, ".brb_db_impl_")
					+ fputstr(dst, "\n");
			}
		}
// pre-computing the stack frame size after each operation in the procedure
		size_t sizes[proc->body.length + 1],
			max_size = 0;
		sizes[0] = BR_getStackRTSize(&ctx.builder, proc - module->seg_exec.data, UINT32_MAX);
		for (uint32_t i = 1; i <= proc->body.length; ++i) {
			sizes[i] = BR_getStackRTSize(&ctx.builder, proc - module->seg_exec.data, i - 1) - sizes[0];
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
	BR_Module _;
	err = BR_extractModule(ctx.builder, &_);
	assert(!err.type, "%s", BR_getErrorMsg(err, "error while analyzing module for native assembly generation"))
	BR_deallocDataBlocks(&_);
	BR_deallocProcs(&_);
	BR_deallocStructs(&_);

	return acc;
}
