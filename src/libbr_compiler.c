// implementation of the BRidge Compiler
#include <br.h>

typedef enum {
	BRC_SYM_BRACKET_L,
	BRC_SYM_BRACKET_R,
	/*BRC_SYM_SQBRACKET_L,
	BRC_SYM_SQBRACKET_R,*/
	BRC_SYM_CBRACKET_L,
	BRC_SYM_CBRACKET_R,
	BRC_SYM_SEMICOLON,
	BRC_SYM_PLUS,
	BRC_SYM_STAR,
	BRC_N_SYMBOLS
} BRC_Symbol;

static sbuf BRC_symbols[] = {
	[BRC_SYM_BRACKET_L] = sbuf_fromcstr("("),
	[BRC_SYM_BRACKET_R] = sbuf_fromcstr(")"),
	/*[BRC_SYM_SQBRACKET_L] = sbuf_fromcstr("["),
	[BRC_SYM_SQBRACKET_R] = sbuf_fromcstr("]"),*/
	[BRC_SYM_CBRACKET_L] = sbuf_fromcstr("{"),
	[BRC_SYM_CBRACKET_R] = sbuf_fromcstr("}"),
	[BRC_SYM_SEMICOLON] = sbuf_fromcstr(";"),
	[BRC_SYM_PLUS] = BRP_SYMBOL("+"),
	[BRC_SYM_STAR] = BRP_SYMBOL("*"),
	BRP_HIDDEN_SYMBOL(" "),
	BRP_HIDDEN_SYMBOL("\t"),
	BRP_HIDDEN_SYMBOL("\n"),
	(sbuf){0}
};
static_assert(sizeof(BRC_symbols) / sizeof(BRC_symbols[0]) - 4 == BRC_N_SYMBOLS, "not all BRidge symbols are defined");

typedef enum {
	BRC_KW_S8,
	BRC_KW_S16,
	BRC_KW_S32,
	BRC_KW_SPTR,
	BRC_KW_S64,
	BRC_KW_VOID,
	BRC_N_KWS
} BRC_Keyword;

static sbuf BRC_kws[] = {
	[BRC_KW_S8]   = sbuf_fromcstr("s8"),
	[BRC_KW_S16]  = sbuf_fromcstr("s16"),
	[BRC_KW_S32]  = sbuf_fromcstr("s32"),
	[BRC_KW_SPTR] = sbuf_fromcstr("sptr"),
	[BRC_KW_S64]  = sbuf_fromcstr("s64"),
	[BRC_KW_VOID] = sbuf_fromcstr("void"),
	(sbuf){0}
};
static_assert(sizeof(BRC_kws) / sizeof(BRC_kws[0]) - 1 == BRC_N_KWS, "not all BRidge keywords are defined");

typedef enum {
	BRC_STATE_STMT,
	BRC_STATE_ADDITIVE_EXPR,
	BRC_STATE_MULTIPLICATIVE_EXPR,
	BRC_N_STATES
} BRC_State;

typedef enum {
	BRC_EXPR_NONE,
	BRC_EXPR_INT,
	BRC_EXPR_ADD,
	BRC_EXPR_MUL,
	BRC_EXPR_WRAP,
	BRC_EXPR_BLOCK,
	BRC_N_EXPRTYPES
} BRC_ExprType;

static BRC_State exprtype_to_state[] = {
	[BRC_EXPR_NONE]  = 0,
	[BRC_EXPR_INT]   = 0,
	[BRC_EXPR_ADD]   = BRC_STATE_ADDITIVE_EXPR,
	[BRC_EXPR_MUL]   = BRC_STATE_MULTIPLICATIVE_EXPR,
	[BRC_EXPR_WRAP]  = BRC_STATE_STMT,
	[BRC_EXPR_BLOCK] = BRC_STATE_STMT
};
static_assert(sizeof(exprtype_to_state) / sizeof(exprtype_to_state[0]) == BRC_N_EXPRTYPES, "not all BRidge expression types have compiler states associated with them");

typedef struct BRC_BaseExpr       BRC_BaseExpr;
typedef struct BRC_IntLiteralExpr BRC_IntLiteralExpr;
typedef struct BRC_UnaryExpr      BRC_UnaryExpr;
typedef struct BRC_BinaryExpr     BRC_BinaryExpr;
typedef struct BRC_TernaryExpr    BRC_TernaryExpr;
typedef struct BRC_VariadicExpr   BRC_VariadicExpr;
typedef union {
	BRC_BaseExpr* base;
	BRC_IntLiteralExpr* as_int_literal;
	BRC_UnaryExpr* as_unary;
	BRC_BinaryExpr* as_binary;
	BRC_TernaryExpr* as_ternary;
	BRC_VariadicExpr* as_variadic;
} BRC_Expr;
static Arena* ator_ctx;
#define BRC_allocNode(n_bytes) arena_alloc(ator_ctx, n_bytes)
#define BRC_freeNode(ptr, size) 
defChain_customAllocator(BRC_Expr, BRC_allocNode, BRC_freeNode);

struct BRC_BaseExpr {
	BRC_ExprType type;
	BRP_TokenLoc loc;
	BRC_Expr parent;
};

struct BRC_IntLiteralExpr {
	BRC_BaseExpr base;
	union {
		uint64_t value_u;
		int64_t value_s;
	};
};

struct BRC_UnaryExpr {
	BRC_BaseExpr base;
	BRC_Expr child;
};

struct BRC_BinaryExpr {
	BRC_BaseExpr base;
	BRC_Expr child1;
	BRC_Expr child2;
};

struct BRC_TernaryExpr {
	BRC_BaseExpr base;
	BRC_Expr child1;
	BRC_Expr child2;
	BRC_Expr child3;
};

struct BRC_VariadicExpr {
	BRC_BaseExpr base;
	BRC_ExprChain children;
};

typedef struct {
	BRP prep;
	BRC_Expr* cur_expr;
	BRC_State state;
	BR_CompilationError error;
	BRP_Token cur_token;
	Arena arena;
	BRC_BaseExpr temp; // used as a placeholder for expressions that are not yet defined;
	// hopefully my assumption that there may only be one undefined expression at a time is correct
} BRC;

typedef bool (*BRC_CaseHandler) (BRC*);

static size_t exprtype_sizes[] = {
	[BRC_EXPR_INT]   = sizeof(BRC_IntLiteralExpr),
	[BRC_EXPR_ADD]   = sizeof(BRC_BinaryExpr),
	[BRC_EXPR_MUL]   = sizeof(BRC_BinaryExpr),
	[BRC_EXPR_WRAP]  = sizeof(BRC_UnaryExpr),
	[BRC_EXPR_BLOCK] = sizeof(BRC_VariadicExpr)
};
static_assert(sizeof(exprtype_sizes) / sizeof(exprtype_sizes[0]) == BRC_N_EXPRTYPES, "not all BRidge expressions have their sizes defined");

typedef enum {
	BRC_OT_LITERAL,
	BRC_OT_UNARY,
	BRC_OT_BINARY,
	BRC_OT_TERNARY,
	BRC_OT_VARIADIC,
	BRC_N_OPERANDTYPES
} BRC_OperandType;
#define BRC_SET_OT(name)     (BRC_OT_##name << 0)
#define BRC_GET_OT(exprtype) (BRC_OperandType)((exprtype_flags[exprtype] >> 0) & 7)
#define BRC_STMT 0x8

static uint8_t exprtype_flags[] = {
	[BRC_EXPR_INT]   = BRC_SET_OT(LITERAL),
	[BRC_EXPR_ADD]   = BRC_SET_OT(BINARY),
	[BRC_EXPR_MUL]   = BRC_SET_OT(BINARY),
	[BRC_EXPR_WRAP]  = BRC_SET_OT(UNARY),
	[BRC_EXPR_BLOCK] = BRC_SET_OT(VARIADIC) | BRC_STMT
};
static_assert(sizeof(exprtype_flags) / sizeof(exprtype_flags[0]) == BRC_N_EXPRTYPES, "not all BRidge expressions have their flags defined");

static void setExprType(BRC* ctx, BRC_Expr* expr_p, BRC_ExprType new_type)
{
	assert(!expr_p->base->type, "attempted to re-set expression type; current expression type: %u", expr_p->base->type);
	expr_p->base = memcpy(arena_alloc(&ctx->arena, exprtype_sizes[new_type]), expr_p->base, sizeof(BRC_BaseExpr));
	expr_p->base->type = new_type;
	switch (BRC_GET_OT(new_type)) {
		case BRC_OT_LITERAL:
			return;
		case BRC_OT_UNARY:
			expr_p->as_unary->child.base = memset(&ctx->temp, 0, sizeof(BRC_BaseExpr));
			expr_p->as_unary->child.base->parent = *expr_p;
			return;
		case BRC_OT_VARIADIC:
			ator_ctx = &ctx->arena;
			expr_p->as_variadic->children = BRC_ExprChain_new(1, (BRC_Expr){.base = memset(&ctx->temp, 0, sizeof(BRC_BaseExpr))});
			expr_p->as_variadic->children.start->data.base->parent = *expr_p;
			return;
		case BRC_OT_BINARY:
		case BRC_OT_TERNARY:
		case BRC_N_OPERANDTYPES:
		default:
			unreachable();
	}
}

static BRC_Expr* getExprRef(BRC* ctx, BRC_Expr expr)
{
	assert(expr.base->parent.base, "an expression must have a parent");
	switch (BRC_GET_OT(expr.base->parent.base->type)) {
		case BRC_OT_UNARY:
			return &expr.base->parent.as_unary->child;
		case BRC_OT_BINARY:
			return expr.base->parent.as_binary->child1.base == expr.base
				? &expr.base->parent.as_binary->child1
				: &expr.base->parent.as_binary->child2;
		case BRC_OT_TERNARY:
			if (expr.base->parent.as_ternary->child1.base == expr.base)
				return &expr.as_ternary->child1;
			if (expr.base->parent.as_ternary->child2.base == expr.base)
				return &expr.as_ternary->child2;
			return &expr.as_ternary->child3;
		case BRC_OT_VARIADIC:
			chainForeach (BRC_Expr, suspect, expr.base->parent.as_variadic->children)
				if (suspect->base == expr.base) return suspect;
			unreachable();
		case BRC_OT_LITERAL:
		case BRC_N_OPERANDTYPES:
		default:
			unreachable();
	}
}

/*
static BRC_Expr newUnaryExpr(BRC* ctx, BRC_ExprType type, BRC_Expr child)
{
	BRC_UnaryExpr* new = arena_alloc(&ctx->arena, sizeof(BRC_UnaryExpr));
	new->base.type = type;
	getExprRef(ctx, child1)->as_unary = new;
	new->base.parent = child.base->parent;
	child.base->parent.as_unary = new;
	new->child = child;
	return (BRC_Expr){.as_unary = new};
}
*/

static BRC_Expr newBinaryExpr(BRC* ctx, BRC_ExprType type, BRC_Expr child1)
{
	BRC_BinaryExpr* new = arena_alloc(&ctx->arena, sizeof(BRC_BinaryExpr));
	new->base.type = type;
	getExprRef(ctx, child1)->as_binary = new;
	new->base.parent = child1.base->parent;
	child1.base->parent.as_binary = new;
	new->child1 = child1;
	new->child2.base = memset(&ctx->temp, 0, sizeof(BRC_BaseExpr));
	new->child2.base->parent.as_binary = new;
	return (BRC_Expr){.as_binary = new};
}

/*
static BRC_Expr newVariadicExpr(BRC* ctx, BRC_ExprType type, BRC_Expr child1)
{
	BRC_VariadicExpr* new = arena_alloc(&ctx->arena, sizeof(BRC_VariadicExpr));
	new->base.type = type;
	getExprRef(ctx, child1)->as_variadic = new;
	new->base.parent = child1.base->parent;
	child1.base->parent.as_variadic = new;
	ator_ctx = &ctx->arena;
	new->children = BRC_ExprChain_new(1, child1);
	return (BRC_Expr){.as_variadic = new};
}
*/

static __unused void debugExpr(BRC* ctx, BRC_Expr expr, int indent_level)
{
	repeat(indent_level - 1) str_put("\t|");
	if (indent_level) putchar('\t');
	if (indent_level >= 10) {
		puts("...");
		return;
	}
	if (expr.base->loc.src_name) {
		BRP_printTokenLoc(expr.base->loc);
		sbuf_put(sbuf_fromcstr(": "));
	}
	switch (expr.base->type) {
		case BRC_EXPR_NONE:
			printf("NONE\n");
			break;
		case BRC_EXPR_INT:
			printf("INT %lli\n", expr.as_int_literal->value_u);
			return;
		case BRC_EXPR_ADD:
			sbuf_put(sbuf_fromcstr("ADD:\n"));
			debugExpr(ctx, expr.as_binary->child1, indent_level + 1);
			debugExpr(ctx, expr.as_binary->child2, indent_level + 1);
			return;
		case BRC_EXPR_MUL:
			sbuf_put(sbuf_fromcstr("MUL:\n"));
			debugExpr(ctx, expr.as_binary->child1, indent_level + 1);
			debugExpr(ctx, expr.as_binary->child2, indent_level + 1);
			return;
		case BRC_EXPR_WRAP:
			sbuf_put(sbuf_fromcstr("WRAP:\n"));
			debugExpr(ctx, expr.as_unary->child, indent_level + 1);
			return;
		case BRC_EXPR_BLOCK:
			sbuf_put(sbuf_fromcstr("BLOCK:\n"));
			chainForeach (BRC_Expr, child_p, expr.as_variadic->children)
				debugExpr(ctx, *child_p, indent_level + 1);
			return;
		case BRC_N_EXPRTYPES:
		default:
			assert(false, "unknown expression type: %u", expr.base->type);
	}
}

// NOTE: case handlers, ending at `_hp` are those handling cases when the precedence of the new expression is higher than that of the current expression

static bool handleIntLiteral(BRC* ctx)
{
	if (ctx->cur_expr->base->type != BRC_EXPR_NONE) {
		ctx->error.type = BR_CERR_OPERATOR_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
	setExprType(ctx, ctx->cur_expr, BRC_EXPR_INT);
	ctx->cur_expr->as_int_literal->value_u = ctx->cur_token.value;
	ctx->cur_expr->base->loc = ctx->cur_token.loc;
	return false;
}

static bool handleSymPlus(BRC* ctx)
{
	if (!ctx->cur_expr->base->type) {
		ctx->error.type = BR_CERR_OPERAND_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
	BRC_Expr new = newBinaryExpr(ctx, BRC_EXPR_ADD, ctx->cur_expr->base->parent);
	new.base->loc = ctx->cur_token.loc;
	ctx->cur_expr = &new.as_binary->child2;
	ctx->state = BRC_STATE_ADDITIVE_EXPR;
	return false;
}

static bool handleSymPlus_hp(BRC* ctx)
{
	if (!ctx->cur_expr->base->type) {
		ctx->error.type = BR_CERR_OPERAND_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
	BRC_Expr new = newBinaryExpr(ctx, BRC_EXPR_ADD, *ctx->cur_expr);
	new.base->loc = ctx->cur_token.loc;
	ctx->cur_expr = &new.as_binary->child2;
	ctx->state = BRC_STATE_ADDITIVE_EXPR;
	return false;
}

static bool handleSymStar_hp(BRC* ctx)
{
	if (!ctx->cur_expr->base->type) {
		ctx->error.type = BR_CERR_OPERAND_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
	BRC_Expr new = newBinaryExpr(ctx, BRC_EXPR_MUL, *ctx->cur_expr);
	new.base->loc = ctx->cur_token.loc;
	ctx->cur_expr = &new.as_binary->child2;
	ctx->state = BRC_STATE_MULTIPLICATIVE_EXPR;
	return false;
}

static bool handleSymStar(BRC* ctx)
{
	if (!ctx->cur_expr->base->type) {
		ctx->error.type = BR_CERR_OPERAND_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
	BRC_Expr new = newBinaryExpr(ctx, BRC_EXPR_MUL, ctx->cur_expr->base->parent);
	new.base->loc = ctx->cur_token.loc;
	ctx->cur_expr = &new.as_binary->child2;
	return false;
}

static bool handleSymBracketL(BRC* ctx)
{
	assert(!ctx->cur_expr->base->type, "function calls are not supported yet");
	setExprType(ctx, ctx->cur_expr, BRC_EXPR_WRAP);
	ctx->cur_expr->base->loc = ctx->cur_token.loc;
	ctx->cur_expr = &ctx->cur_expr->as_unary->child;
	ctx->state = BRC_STATE_STMT;
	return false;
}

static bool handleSymBracketR(BRC* ctx)
{
// handling a case like this: `(2 * 3 +)`
	if (!ctx->cur_expr->base->type) {
		ctx->error.type = BR_CERR_OPERAND_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
// finding the nearest bracket from the bottom up
	BRC_Expr bracket = *ctx->cur_expr;
	while (bracket.base->type != BRC_EXPR_WRAP && bracket.base->type != BRC_EXPR_BLOCK)
		bracket = bracket.base->parent;
// handling a case like this: `3 + 5)`
	if (!bracket.base->parent.base) {
		ctx->error.type = BR_CERR_EXTRA_BRACKET;
		ctx->error.loc = ctx->cur_token;
		return true;
	}
// handling a case like this: `{ 3 + 5)`
	if (bracket.base->type != BRC_EXPR_WRAP) {
		ctx->error.type = BR_CERR_MISMATCHED_BRACKET;
		ctx->error.loc = ctx->cur_token;
		ctx->error.other_loc = (BRP_Token){
			.type = BRP_TOKEN_SYMBOL,
			.symbol_id = BRC_SYM_CBRACKET_L,
			.loc = bracket.base->loc};
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
	ctx->cur_expr = getExprRef(ctx, bracket);
	ctx->state = exprtype_to_state[bracket.base->parent.base->type];
	return false;
}

static bool handleSymCBracketL(BRC* ctx)
{
	if (ctx->cur_expr->base->type) {
		ctx->error.type = BR_CERR_OPERATOR_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
	setExprType(ctx, ctx->cur_expr, BRC_EXPR_BLOCK);
	ctx->cur_expr->base->loc = ctx->cur_token.loc;
	ctx->cur_expr = &ctx->cur_expr->as_variadic->children.start->data;
	ctx->state = BRC_STATE_STMT;
	return false;
}

static bool handleSymCBracketR(BRC* ctx)
{
// handling a case like this: `{2 * 3 +}`
	if (!ctx->cur_expr->base->type) {
		ctx->error.type = BR_CERR_OPERAND_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
// finding the nearest bracket from the bottom up
	BRC_Expr bracket = *ctx->cur_expr;
	while (bracket.base->type != BRC_EXPR_WRAP && bracket.base->type != BRC_EXPR_BLOCK)
		bracket = bracket.base->parent;
// handling a case like this: `3 + 5}`
	if (!bracket.base->parent.base) {
		ctx->error.type = BR_CERR_EXTRA_BRACKET;
		ctx->error.loc = ctx->cur_token;
		return true;
	}
// handling a case like this: `( 3 + 5}`
	if (bracket.base->type != BRC_EXPR_BLOCK) {
		ctx->error.type = BR_CERR_MISMATCHED_BRACKET;
		ctx->error.loc = ctx->cur_token;
		ctx->error.other_loc = (BRP_Token){
			.type = BRP_TOKEN_SYMBOL,
			.symbol_id = BRC_SYM_BRACKET_L,
			.loc = bracket.base->loc};
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
	ctx->cur_expr = getExprRef(ctx, bracket);
	ctx->state = exprtype_to_state[bracket.base->parent.base->type];
	return false;
}

static bool handleSymSemicolon(BRC* ctx)
{
// handling a case like this: `3 +;`
	if (!ctx->cur_expr->base->type && !(exprtype_flags[ctx->cur_expr->base->parent.base->type] & BRC_STMT)) {
		ctx->error.type = BR_CERR_OPERAND_EXPECTED;
		ctx->error.loc = ctx->cur_token;
		ctx->error.prep = objdup(BRP, ctx->prep);
		return true;
	}
// handling a case like `(3 + 2;)
	do {
		ctx->cur_expr = &ctx->cur_expr->base->parent;
	} while (ctx->cur_expr->base->type != BRC_EXPR_WRAP && ctx->cur_expr->base->type != BRC_EXPR_BLOCK);
	if (ctx->cur_expr->base->type != BRC_EXPR_BLOCK) {
		ctx->error.type = BR_CERR_MULTIPLE_EXPRS_IN_BRACKETS;
		ctx->error.loc = ctx->cur_token;
		ctx->error.other_loc = (BRP_Token){.loc = ctx->cur_expr->base->loc};
		return true;
	}
	ator_ctx = &ctx->arena;
	ctx->temp = (BRC_BaseExpr){.parent = *ctx->cur_expr};
	ctx->cur_expr = BRC_ExprChain_append(&ctx->cur_expr->as_variadic->children, (BRC_Expr){.base = &ctx->temp});
	ctx->state = BRC_STATE_STMT;
	return false;
}

static inline void handleEndOfInput(BRC* ctx)
{
	do {
		ctx->cur_expr = &ctx->cur_expr->base->parent;
	} while (ctx->cur_expr->base->type != BRC_EXPR_BLOCK && ctx->cur_expr->base->type != BRC_EXPR_WRAP);
// handling a case like this: `2 * (3 + 4`
	if (ctx->cur_expr->base->parent.base) {
		ctx->error.type = BR_CERR_UNCLOSED_BRACKET;
		ctx->error.loc = (BRP_Token){.loc = ctx->cur_expr->base->loc};
	}
}

BRC_CaseHandler sym_handlers[][BRC_N_SYMBOLS] = {
	[BRC_STATE_STMT] = {
		[BRC_SYM_PLUS]       = handleSymPlus_hp,
		[BRC_SYM_STAR]       = handleSymStar_hp,
		[BRC_SYM_BRACKET_L]  = handleSymBracketL,
		[BRC_SYM_BRACKET_R]  = handleSymBracketR,
		[BRC_SYM_CBRACKET_L] = handleSymCBracketL,
		[BRC_SYM_CBRACKET_R] = handleSymCBracketR,
		[BRC_SYM_SEMICOLON]  = handleSymSemicolon
	},
	[BRC_STATE_ADDITIVE_EXPR] = {
		[BRC_SYM_PLUS]       = handleSymPlus,
		[BRC_SYM_STAR]       = handleSymStar_hp,
		[BRC_SYM_BRACKET_L]  = handleSymBracketL,
		[BRC_SYM_BRACKET_R]  = handleSymBracketR,
		[BRC_SYM_CBRACKET_L] = handleSymCBracketL,
		[BRC_SYM_CBRACKET_R] = handleSymCBracketR,
		[BRC_SYM_SEMICOLON]  = handleSymSemicolon
	},
	[BRC_STATE_MULTIPLICATIVE_EXPR] = {
		[BRC_SYM_PLUS]       = handleSymPlus,
		[BRC_SYM_STAR]       = handleSymStar,
		[BRC_SYM_BRACKET_L]  = handleSymBracketL,
		[BRC_SYM_BRACKET_R]  = handleSymBracketR,
		[BRC_SYM_CBRACKET_L] = handleSymCBracketL,
		[BRC_SYM_CBRACKET_R] = handleSymCBracketR,
		[BRC_SYM_SEMICOLON]  = handleSymSemicolon
	}
};
static_assert(BRC_N_SYMBOLS == 7, "not all symbols have handlers defined");
static_assert(sizeof(sym_handlers) / sizeof(sym_handlers[0]) == BRC_N_STATES, "not all BRidge compiler states have handlers defined");

static void stubErrorHandler(BRP* brp) {}

// change the return type to `size_t`
void BR_printCompilationErrorMsg(FILE* dst, BR_CompilationError err, const char* prefix)
{
	if (err.type == BR_CERR_OK) return;
	if (err.loc.loc.src_name) {
		BRP_fprintTokenLoc(dst, err.loc.loc);
		fputs(": ", dst);
	}
	if (prefix) {
		fputs(prefix, dst);
		fputs(": ", dst);
	}
	switch (err.type) {
		case BR_CERR_OPERATOR_EXPECTED:
			fputs("expected an operator, instead got ", dst);
			BRP_fprintTokenStr(dst, err.loc, err.prep);
			fputc('\n', dst);
			return;
		case BR_CERR_OPERAND_EXPECTED:
			fputs("expected an operand, instead got ", dst);
			BRP_fprintTokenStr(dst, err.loc, err.prep);
			fputc('\n', dst);
			return;
		case BR_CERR_EXTRA_BRACKET:
			fputs("redundant closing bracket found\n", dst);
			return;
		case BR_CERR_PREP_FAILURE:
			BRP_printErrorMsg(dst, err.prep);
			fputc('\n', dst);
			return;
		case BR_CERR_UNCLOSED_BRACKET:
			fputs("unclosed parentheses\n", dst);
			return;
		case BR_CERR_MISMATCHED_BRACKET:
			fputs("expected this bracket\n", dst);
			BRP_fprintTokenLoc(dst, err.other_loc.loc);
			fputs(": to match this\n", dst);
			return;
		case BR_CERR_MULTIPLE_EXPRS_IN_BRACKETS:
			fputs("only one expression can be enclosed in brackets\n", dst);
			BRP_fprintTokenLoc(dst, err.other_loc.loc);
			fputs(": the opening bracket is here\n", dst);
			return;
		case BR_CERR_OK:
		default:
			fputs("undefined error\n", dst);
			return;
	}
}

BR_CompilationError BR_loadFromSource(FILE* input, const char* input_name, BR_ModuleBuilder* dst)
{
	BRC compiler = {0};
// setting up the preprocessor
	if (!BRP_initBRP(&compiler.prep, stubErrorHandler, BRP_ESC_STR_LITERALS)) goto brp_error;
	if (!BRP_setKeywords(&compiler.prep, BRC_kws)) goto brp_error;
	if (!BRP_setSymbols(&compiler.prep, BRC_symbols)) goto brp_error;
	if (!BRP_setInput(&compiler.prep, input_name, input)) goto brp_error;
// setting up the expression tree
	BRC_VariadicExpr root = {.base.type = BRC_EXPR_BLOCK};
	ator_ctx = &compiler.arena;
	compiler.cur_expr = BRC_ExprChain_append(&root.children, (BRC_Expr){.base = memset(&compiler.temp, 0, sizeof(BRC_BaseExpr))});
	compiler.cur_expr->base->parent.as_variadic = &root;
// main loop
	while (true) {
		compiler.cur_token = BRP_fetchToken(&compiler.prep);
		if (compiler.prep.error_code) goto brp_error;
		switch (compiler.cur_token.type) {
			case BRP_TOKEN_NONE:
				handleEndOfInput(&compiler);
				goto epilogue;
			case BRP_TOKEN_INT:
				if (handleIntLiteral(&compiler)) goto epilogue;
				break;
			case BRP_TOKEN_SYMBOL:
				if (sym_handlers[compiler.state][compiler.cur_token.symbol_id](&compiler)) goto epilogue;
				break;
			case BRP_TOKEN_WORD:
			case BRP_TOKEN_KEYWORD:
			case BRP_TOKEN_STRING:
				assert(false, "not supported yet");
		}
	}
	epilogue:
		debugExpr(&compiler, *compiler.cur_expr, 0);
		if (!compiler.error.prep) BRP_delBRP(&compiler.prep);
		arena_log(&compiler.arena);
		arena_free(&compiler.arena);
		return compiler.error;
	brp_error:
		arena_free(&compiler.arena);
		return (BR_CompilationError){
			.type = BR_CERR_PREP_FAILURE,
			.prep = objdup(BRP, compiler.prep)
		};
}
