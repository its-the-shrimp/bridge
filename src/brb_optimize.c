// implemenatation for optimizations of BRB modules
#include <brb.h>
#include <math.h>

defArray(Submodule);
defArray(Op);
defArray(DataBlock);

typedef int Symbol;
declArray(Symbol);
defArray(Symbol);

typedef struct {
	SymbolArray used_mb;
	SymbolArray used_db;
	SymbolArray vars;
	int frame_size;
	int frame_start;
	FILE* temp_out;
} OptimizerCtx;

static const char* BRBRegNames[N_REGS] = {
	[0] = "r0",
	[1] = "r1",
	[2] = "r2",
	[3] = "r3",
	[4] = "r4",
	[5] = "r5",
	[6] = "r6",
	[7] = "r7",
	[8] = "rZ"
};

typedef void (*Optimizer) (Module*, OptimizerCtx*, Op*);

void optimizeNop(Module* module, OptimizerCtx* ctx, Op* op)
{}

void optimizeOpEnd(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "\tend\n");
}

void optimizeOpMark(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "\tmark \".m%ld\"\n", op - module->seg_exec.data);
}

void optimizeOpSet(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->value == 0) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tset:%s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], op->value);
	}
}

void optimizeOpSetr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
}

void optimizeOpSetd(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->symbol_id >= arrayhead(module->submodules)->ds_offset) {
		int db_iter = 0;
		for (; db_iter < ctx->used_db.length; ++db_iter) {
			if (ctx->used_db.data[db_iter] == op->symbol_id) break;
		}

		if (db_iter >= ctx->used_db.length)
			SymbolArray_append(&ctx->used_db, op->symbol_id);
	}

	fprintf(
		ctx->temp_out,
		"\tsetd:%s %s %s \"%s\"\n",
		conditionNames[op->cond_id].data,
		BRBRegNames[op->dst_reg],
		module->submodules.data[op->module_id].name,
		module->seg_data.data[op->symbol_id].name
	);
}

void optimizeOpSetb(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	fprintf(ctx->temp_out, "\tsetb:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], builtins[op->symbol_id].name);
}

void optimizeOpAdd(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->value == 0 && op->dst_reg == op->src_reg || op->dst_reg == ZEROREG_ID) return; 
	if (op->value == 0) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tset:%s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], op->value);
	} else {
		fprintf(ctx->temp_out, "\tadd:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpAddr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src2_reg]);
	} else {
		fprintf(ctx->temp_out, "\taddr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpSub(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->value == 0 && op->dst_reg == op->src_reg || op->dst_reg == ZEROREG_ID) return; 

	if (op->value == 0) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tset:%s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], -op->value);
	} else {
		fprintf(ctx->temp_out, "\tsub:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpSubr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID && op->src_reg == op->src2_reg) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tsubr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpSys(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "\tsys:%s %s\n", conditionNames[op->cond_id].data, syscallNames[op->syscall_id].data);
}

void optimizeOpGoto(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "\tgoto:%s \".m%ld\"\n", conditionNames[op->cond_id].data, (op + op->op_offset) - module->seg_exec.data);
}

void optimizeOpCmp(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->value == 0) {
		fprintf(ctx->temp_out, "\tcmpr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->src_reg]);
	} else {
		fprintf(ctx->temp_out, "\tcmp:%s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpCmpr(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "\tcmpr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
}

void optimizeOpAnd(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->value == 0 || op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tand:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpAndr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->src_reg == ZEROREG_ID || op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tandr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}


void optimizeOpOr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if ((int64_t)op->value == -1) {
		fprintf(ctx->temp_out, "\tnot:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tor:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpOrr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->src_reg == op->src2_reg && op->src_reg == op->dst_reg) return;
	if (op->src_reg == op->src2_reg) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else {
		fprintf(ctx->temp_out, "\torr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpNot(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	fprintf(ctx->temp_out, "\tnot:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
}

void optimizeOpXor(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->dst_reg == op->src_reg && op->value == 0) return;
	if (op->value == 0) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tset:%s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], op->value);
	} else {
		fprintf(ctx->temp_out, "\txor:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpXorr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->dst_reg == op->src_reg && op->src_reg == op->src2_reg) return;
	if (op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src2_reg]);
	} else {
		fprintf(ctx->temp_out, "\txorr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpShl(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->dst_reg == op->src_reg && op->value == 0) return;
	if (op->value == 0) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tshl:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpShlr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->dst_reg == op->src_reg && op->src2_reg == ZEROREG_ID) return;
	if (op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tshlr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpShr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->dst_reg == op->src_reg && op->value == 0) return;
	if (op->value == 0) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tshr:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpShrr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->dst_reg == op->src_reg && op->src2_reg == ZEROREG_ID) return;
	if (op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tshrr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpShrs(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->dst_reg == op->src_reg && op->value == 0) return;
	if (op->value == 0) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tshr:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpShrsr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->dst_reg == op->src_reg && op->src2_reg == ZEROREG_ID) return;
	if (op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if (op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tshrr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpProc(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "proc \"%s\"\n", op->mark_name);
}

void optimizeOpCall(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(
		ctx->temp_out,
		"\tcall:%s %s \"%s\"\n",
		conditionNames[op->cond_id].data,
		module->submodules.data[op->module_id].name,
		module->seg_exec.data[op->symbol_id].mark_name
	);
}

void optimizeOpRet(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "\tret:%s\n", conditionNames[op->cond_id].data);
}

void optimizeOpEndproc(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "endproc\n");
	ctx->frame_size = 0;
	ctx->frame_start = 0;
	SymbolArray_clear(&ctx->vars);
}

void optimizeLoadOp(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->type >= OP_LD64S) {
		fprintf(
			ctx->temp_out,
			"\tld%ds:%s %s %s\n",
			8 << (3 - op->type + OP_LD64S),
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg]
		);
	} else {
		fprintf(
			ctx->temp_out,
			"\tld%d:%s %s %s\n",
			8 << (3 - ((op->type - OP_LD64) >> 1)),
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg]
		);
	}
}

void optimizeStoreOp(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	fprintf(
		ctx->temp_out,
		"\tstr%d:%s %s %s\n",
		8 << (3 - ((op->type - OP_STR64) >> 1)),
		conditionNames[op->cond_id].data,
		BRBRegNames[op->dst_reg],
		BRBRegNames[op->src_reg]
	);
}

void optimizeOpVar(Module* module, OptimizerCtx* ctx, Op* op)
{
	ctx->frame_size += op->new_var_size;
	SymbolArray_append(&ctx->vars, op->new_var_size);
	fprintf(ctx->temp_out, "\tvar \".v%d\" %lld\n", ctx->frame_size, op->new_var_size);
}

void printVar(OptimizerCtx* ctx, int32_t offset, int32_t var_size)
{
	if (!var_size) {
		fprintf(ctx->temp_out, " \".v%d\"", offset);
		return;
	}

	int32_t acc = ctx->frame_start;
	arrayForeach (Symbol, cur_var_size, ctx->vars) {
		if (inRange(offset, acc, acc + *cur_var_size)) break;
		acc += *cur_var_size;
	}
	offset -= acc;

	fprintf(ctx->temp_out, " \".v%d\":%d :%d", acc, var_size, offset);
}

void optimizeOpSetv(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	fprintf(
		ctx->temp_out,
		"\tsetv:%s %s",
		conditionNames[op->cond_id].data,
		BRBRegNames[op->dst_reg]
	);
	printVar(ctx, op->symbol_id, op->var_size);
	fputc('\n', ctx->temp_out);
}

void optimizeOpMul(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->src_reg == op->dst_reg && op->value == 1) return;
	if (op->value == 0 || op->src_reg == ZEROREG_ID) {
		fprintf(
			ctx->temp_out, 
			"\tsetr:%s %s rZ\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg]
		);
	} else if (op->value == 1) {
		fprintf(
			ctx->temp_out,
			"\tsetr:%s %s %s\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg]
		);
	} else if ((op->value & (op->value - 1)) == 0) {
		fprintf(
			ctx->temp_out,
			"\tshl:%s %s %s %d\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			(int)log2f(op->value)
		);
	} else {
		fprintf(
			ctx->temp_out,
			"\tmul:%s %s %s %lld\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			op->value
		);
	}
}

void optimizeOpMulr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->src_reg == ZEROREG_ID || op->src2_reg == ZEROREG_ID) {
		fprintf(
			ctx->temp_out,
			"\tsetr:%s %s rZ\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg]
		);
	} else {
		fprintf(
			ctx->temp_out, 
			"\tmulr:%s %s %s %s\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			BRBRegNames[op->src2_reg]
		);
	}
}

void optimizeOpDiv(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->src_reg == op->dst_reg && op->value == 1) return;
	if (op->src_reg == ZEROREG_ID) {
		fprintf(
			ctx->temp_out,
			"\tsetr:%s %s rZ\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg]
		);
	} else if (op->value == 1) {
		fprintf(
			ctx->temp_out,
			"\tsetr:%s %s %s\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg]
		);
	} else if ((op->value & (op->value - 1)) == 0) {
		fprintf(
			ctx->temp_out,
			"\tshr:%s %s %s %d\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			(int)log2f(op->value)
		);
	} else {
		fprintf(
			ctx->temp_out,
			"\tdiv:%s %s %s %lld\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			op->value
		);
	}
}

void optimizeOpDivr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->src_reg == ZEROREG_ID) {
		fprintf(
			ctx->temp_out,
			"\tsetr:%s %s rZ\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg]
		);
	} else {
		fprintf(
			ctx->temp_out,
			"\tdivr:%s %s %s %s\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			BRBRegNames[op->src2_reg]
		);
	}
}

void optimizeOpDivs(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID || op->src_reg == op->dst_reg && op->value == 1) return;
	if (op->src_reg == ZEROREG_ID) {
		fprintf(
			ctx->temp_out,
			"\tsetr:%s %s rZ\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg]
		);
	} else if (op->value == 1) {
		fprintf(
			ctx->temp_out,
			"\tsetr:%s %s %s\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg]
		);
	} else if ((op->value & (op->value - 1)) == 0) {
		fprintf(
			ctx->temp_out,
			"\tshrs:%s %s %s %d\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			(int)log2f(op->value)
		);
	} else {
		fprintf(
			ctx->temp_out,
			"\tdivs:%s %s %s %lld\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			op->value
		);
	}
}

void optimizeOpDivsr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->src_reg == ZEROREG_ID) {
		fprintf(
			ctx->temp_out,
			"\tsetr:%s %s rZ\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg]
		);
	} else {
		fprintf(
			ctx->temp_out,
			"\tdivsr:%s %s %s %s\n",
			conditionNames[op->cond_id].data,
			BRBRegNames[op->dst_reg],
			BRBRegNames[op->src_reg],
			BRBRegNames[op->src2_reg]
		);
	}
}

void optimizeOpExtproc(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "extproc \"%s\"\n", op->mark_name);
}

void optimizeOpLdv(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	fprintf(
		ctx->temp_out,
		"\tldv%s:%s %s",
		op->type == OP_LDVS ? "s" : "",
		conditionNames[op->cond_id].data,
		BRBRegNames[op->dst_reg]
	);
	printVar(ctx, op->symbol_id, op->var_size);
	fputc('\n', ctx->temp_out);
}

void optimizeOpStrv(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "\tstrv:%s", conditionNames[op->cond_id].data);
	printVar(ctx, op->symbol_id, op->var_size);
	fprintf(ctx->temp_out, " %s\n", BRBRegNames[op->src_reg]);
}

void optimizeOpPopv(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "\tpopv %s\n", BRBRegNames[op->dst_reg]);
	ctx->frame_size -= SymbolArray_pop(&ctx->vars, -1);
}

void optimizeOpPushv(Module* module, OptimizerCtx* ctx, Op* op)
{
	ctx->frame_size += op->var_size;
	SymbolArray_append(&ctx->vars, op->var_size);
	fprintf(ctx->temp_out, "\tpushv \".v%d\" %hhu %s\n", ctx->frame_size, op->var_size, BRBRegNames[op->src_reg]);
}

void optimizeOpAtf(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "@f \"%s\"\n", op->mark_name);
}

void optimizeOpAtl(Module* module, OptimizerCtx* ctx, Op* op)
{
	fprintf(ctx->temp_out, "@l %d\n", op->symbol_id);
}

void optimizeOpSetc(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	fprintf(ctx->temp_out, "\tsetc:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], conditionNames[op->cond_arg].data);
}

void optimizeOpDelnv(Module* module, OptimizerCtx* ctx, Op* op)
{
	ctx->frame_size -= op->symbol_id;
	int n_vars = 0;
	for (int64_t i = op->symbol_id; i > 0; i -= ctx->vars.data[--ctx->vars.length]) {
		n_vars += 1;
	}
	fprintf(ctx->temp_out, "\tdelnv %d\n", n_vars);
}

void optimizeSignExtendOp(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	fprintf(
		ctx->temp_out,
		"\tsx%d:%s %s %s\n",
		4 << (3 - op->type + OP_SX32),
		conditionNames[op->cond_id].data,
		BRBRegNames[op->dst_reg],
		BRBRegNames[op->src_reg]
	);
}

void optimizeOpMod(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->value <= 1 || op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else if ((int64_t)op->value == -1) {
		fprintf(ctx->temp_out, "\tsetr:%s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg]);
	} else if ((op->value & (op->value - 1)) == 0) {
		fprintf(ctx->temp_out, "\tand:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value - 1);
	} else {
		fprintf(ctx->temp_out, "\tmod:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpModr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->src_reg == ZEROREG_ID || op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tmodr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpMods(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (inRange((int64_t)op->value, -1, 2) || op->src_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else if ((op->value & (op->value - 1)) == 0) {
		fprintf(ctx->temp_out, "\tand:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value - 1);
	} else {
		fprintf(ctx->temp_out, "\tmods:%s %s %s %lld\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], op->value);
	}
}

void optimizeOpModsr(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (op->dst_reg == ZEROREG_ID) return;
	if (op->src_reg == ZEROREG_ID || op->src2_reg == ZEROREG_ID) {
		fprintf(ctx->temp_out, "\tsetr:%s %s rZ\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg]);
	} else {
		fprintf(ctx->temp_out, "\tmodsr:%s %s %s %s\n", conditionNames[op->cond_id].data, BRBRegNames[op->dst_reg], BRBRegNames[op->src_reg], BRBRegNames[op->src2_reg]);
	}
}

void optimizeOpArg(Module* module, OptimizerCtx* ctx, Op* op)
{
	if (!ctx->frame_start) {
		ctx->frame_start = -STACKFRAME_SIZE;
		ctx->frame_size += STACKFRAME_SIZE;
		SymbolArray_prepend(&ctx->vars, STACKFRAME_SIZE);
	}
	fprintf(ctx->temp_out, "\targ \".v%d\" %lld\n", ctx->frame_start, op->new_var_size);
	SymbolArray_prepend(&ctx->vars, op->new_var_size);
	ctx->frame_start -= op->new_var_size;
	ctx->frame_size += op->new_var_size;
}

static Optimizer optimizers[] = {
	[OP_NONE] = optimizeNop,
	[OP_END] = optimizeOpEnd,
	[OP_MARK] = optimizeOpMark,
	[OP_SET] = optimizeOpSet,
	[OP_SETR] = optimizeOpSetr,
	[OP_SETD] = optimizeOpSetd,
	[OP_SETB] = optimizeOpSetb,
	[OP_ADD] = optimizeOpAdd,
	[OP_ADDR] = optimizeOpAddr,
	[OP_SUB] = optimizeOpSub,
	[OP_SUBR] = optimizeOpSubr,
	[OP_SYS] = optimizeOpSys,
	[OP_GOTO] = optimizeOpGoto,
	[OP_CMP] = optimizeOpCmp,
	[OP_CMPR] = optimizeOpCmpr,
	[OP_AND] = optimizeOpAnd,
	[OP_ANDR] = optimizeOpAndr,
	[OP_OR] = optimizeOpOr,
	[OP_ORR] = optimizeOpOrr,
	[OP_NOT] = optimizeOpNot,
	[OP_XOR] = optimizeOpXor,
	[OP_XORR] = optimizeOpXorr,
	[OP_SHL] = optimizeOpShl,
	[OP_SHLR] = optimizeOpShlr,
	[OP_SHR] = optimizeOpShr,
	[OP_SHRR] = optimizeOpShrr,
	[OP_SHRS] = optimizeOpShrs,
	[OP_SHRSR] = optimizeOpShrsr,
	[OP_PROC] = optimizeOpProc,
	[OP_CALL] = optimizeOpCall,
	[OP_RET] = optimizeOpRet,
	[OP_ENDPROC] = optimizeOpEndproc,
	[OP_LD64] = optimizeLoadOp,
	[OP_STR64] = optimizeStoreOp,
	[OP_LD32] = optimizeLoadOp,
	[OP_STR32] = optimizeStoreOp,
	[OP_LD16] = optimizeLoadOp,
	[OP_STR16] = optimizeStoreOp,
	[OP_LD8] = optimizeLoadOp,
	[OP_STR8] = optimizeStoreOp,
	[OP_VAR] = optimizeOpVar,
	[OP_SETV] = optimizeOpSetv,
	[OP_MUL] = optimizeOpMul,
	[OP_MULR] = optimizeOpMulr,
	[OP_DIV] = optimizeOpDiv,
	[OP_DIVR] = optimizeOpDivr,
	[OP_DIVS] = optimizeOpDivs,
	[OP_DIVSR] = optimizeOpDivsr,
	[OP_EXTPROC] = optimizeOpExtproc,
	[OP_LDV] = optimizeOpLdv,
	[OP_STRV] = optimizeOpStrv,
	[OP_POPV] = optimizeOpPopv,
	[OP_PUSHV] = optimizeOpPushv,
	[OP_ATF] = optimizeOpAtf,
	[OP_ATL] = optimizeOpAtl,
	[OP_SETC] = optimizeOpSetc,
	[OP_DELNV] = optimizeOpDelnv,
	[OP_LD64S] = optimizeLoadOp,
	[OP_LD32S] = optimizeLoadOp,
	[OP_LD16S] = optimizeStoreOp,
	[OP_LD8S] = optimizeLoadOp,
	[OP_LDVS] = optimizeOpLdv,
	[OP_SX32] = optimizeSignExtendOp,
	[OP_SX16] = optimizeSignExtendOp,
	[OP_SX8] = optimizeSignExtendOp,
	[OP_MOD] = optimizeOpMod,
	[OP_MODR] = optimizeOpModr,
	[OP_MODS] = optimizeOpMods,
	[OP_MODSR] = optimizeOpModsr,
	[OP_ARG] = optimizeOpArg
};
static_assert(N_OPS == sizeof(optimizers) / sizeof(optimizers[0]), "not all BRB operations have optimizers defined");

void optimizeModule(Module* module, const char* search_paths[], FILE* output, unsigned int level)
{
	assert(level <= 1, "invalid optimization level");
	if (level == 0) return;
	sbuf temp_buf = {0};

	OptimizerCtx ctx = {
		.used_db = SymbolArray_new(-module->seg_data.length),
		.vars = SymbolArray_new(0),
		.frame_size = 0,
		.temp_out = open_memstream(&temp_buf.data, (size_t*)&temp_buf.length)
	};

	if (module->submodules.length) {
		for (
			Submodule* submodule = module->submodules.data;
			submodule - module->submodules.data < module->submodules.length;
			++submodule
		) {
			if (submodule->direct && submodule - module->submodules.data < module->submodules.length - 1)
				fprintf(ctx.temp_out, ".load %s\n", submodule->name);
		}
	}

	if (module->seg_exec.length) {
		for (Op* op = module->seg_exec.data + arrayhead(module->submodules)->es_offset; op->type != OP_END; ++op) {
			optimizers[op->type](module, &ctx, op);
		}
	}

	static_assert(N_PIECE_TYPES == 8, "not all data piece types are handled in `optimizeModule`");

	if (ctx.used_db.length) {
		for (int i = 0; i < ctx.used_db.length; ++i) {
			DataBlock* block = module->seg_data.data + ctx.used_db.data[i];
			fprintf(ctx.temp_out, ".data %s\"%s\" { ", block->is_mutable ? "mut " : "", block->name);
			for (DataPiece* piece = block->pieces.data; piece - block->pieces.data < block->pieces.length; ++piece) {
				switch (piece->type) {
					case PIECE_BYTES:
					case PIECE_TEXT:
						fputc('"', ctx.temp_out);
						fputsbufesc(ctx.temp_out, piece->data, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE);
						fputs("\" ", ctx.temp_out);
						break;
					case PIECE_INT16:
						fprintf(ctx.temp_out, "int16 %lld ", piece->integer);
						break;
					case PIECE_INT32:
						fprintf(ctx.temp_out, "int32 %lld ", piece->integer);
						break;
					case PIECE_INT64:
						fprintf(ctx.temp_out, "int64 %lld ", piece->integer);
						break;
					case PIECE_DB_ADDR:
						fprintf(
							ctx.temp_out,
							"db_addr %s \"%s\" ",
							module->submodules.data[piece->module_id].name,
							module->seg_data.data[piece->symbol_id].name
						);
						break;
					case PIECE_ZERO:
						fprintf(ctx.temp_out, "zero %lld ", piece->n_bytes);
						break;
					case PIECE_NONE:
					case N_PIECE_TYPES:
					default:
						assert(false, "unexpected data piece type");
				}
			}
			fprintf(ctx.temp_out, "}\n");
		}
	}

	DataBlockArray_clear(&module->seg_data);
	OpArray_clear(&module->seg_exec);
	SubmoduleArray_clear(&module->submodules);

	fclose(ctx.temp_out);
	if (output)
		fwrite(temp_buf.data, temp_buf.length, 1, output);
	ctx.temp_out = fmemopen(temp_buf.data, temp_buf.length, "r");

	VBRBError err = compileVBRB(ctx.temp_out, "<optimizer output>", module, search_paths);
	if (err.code) {
		eprintf("unexpected internal error during optimization:\n");
		printVBRBError(stderr, err);
		putsbufln(temp_buf);
		abort();
	}

	SymbolArray_clear(&ctx.used_db);
	SymbolArray_clear(&ctx.used_mb);
	fclose(ctx.temp_out);
	sfree(&temp_buf);
}
