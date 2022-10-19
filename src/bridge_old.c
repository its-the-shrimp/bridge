#include <br.h>
#include <errno.h>
#include <signal.h>

defArray(sbuf);
declArray(int);
defArray(int);

#define STR_PREFIX "._s"

typedef enum {
	KW_VOID,
	KW_INT8,
	KW_INT16,
	KW_INT32,
	KW_INT64,
	KW_SYS,
	KW_BUILTIN,
	KW_RETURN,
	KW_CAST,
	KW_BOOL,
	KW_IF,
	KW_ELSE,
	KW_WHILE,
	KW_FOR,
	KW_DO,
	KW_STATIC,
	KW_TYPE,
	N_KWS
} BRKeyword;

typedef enum {
	SYMBOL_ARGSPEC_START,
	SYMBOL_ARGSPEC_END,
	SYMBOL_COMMA,
	SYMBOL_BLOCK_START,
	SYMBOL_BLOCK_END,
	SYMBOL_SEMICOLON,
	SYMBOL_ADD_ASSIGN,
	SYMBOL_SUB_ASSIGN,
	SYMBOL_MUL_ASSIGN,
	SYMBOL_DIV_ASSIGN,
	SYMBOL_AND_ASSIGN,
	SYMBOL_XOR_ASSIGN,
	SYMBOL_OR_ASSIGN,
	SYMBOL_SHL_ASSIGN,
	SYMBOL_SHR_ASSIGN,
	SYMBOL_EQ,
	SYMBOL_NEQ,
	SYMBOL_LE,
	SYMBOL_GE,
	SYMBOL_NOT,
	SYMBOL_OR,
	SYMBOL_AND,
	SYMBOL_ASSIGNMENT,
	SYMBOL_PLUS,
	SYMBOL_MINUS,
	SYMBOL_STAR,
	SYMBOL_DIV,
	SYMBOL_MOD,
	SYMBOL_AMPERSAND,
	SYMBOL_PIPE,
	SYMBOL_CARET,
	SYMBOL_LSHIFT,
	SYMBOL_RSHIFT,
	SYMBOL_TILDE,
	SYMBOL_LT,
	SYMBOL_GT,
	SYMBOL_INDEX_START,
	SYMBOL_INDEX_END,
	SYMBOL_DOT,
	N_SYMBOLS
} BRSymbol;

typedef enum {
	KIND_NONE,
	KIND_INT,
	KIND_PTR,
	KIND_BUILTIN_VAL,
	KIND_VOID,
	KIND_BOOL,
	KIND_ARRAY,
	KIND_CUSTOM,
	N_TYPE_KINDS
} TypeKind;

typedef enum {
	EXPR_INVALID,
	EXPR_SYSCALL, // variadic, evaluatable
	EXPR_NAME, // nullary, non-evaluatable
	EXPR_BUILTIN, // nullary, evaluatable
	EXPR_STRING, // nullary, evaluatable
	EXPR_INT, // nullary, evaluatable
	EXPR_NEW_VAR, // variadic, non-evaluatable
	EXPR_ASSIGN, // binary, evaluatable
	EXPR_ADD_ASSIGN, // binary, evaluatable
	EXPR_SUB_ASSIGN, // binary, evaluatable
	EXPR_MUL_ASSIGN, // binary, evaluatable
	EXPR_DIV_ASSIGN, // binary, evaluatable
	EXPR_AND_ASSIGN, // binary, evaluatable
	EXPR_XOR_ASSIGN, // binary, evaluatable
	EXPR_OR_ASSIGN, // binary, evaluatable
	EXPR_SHL_ASSIGN, // binary, evaluatable
	EXPR_SHR_ASSIGN, // binary, evaluatable
	EXPR_GET, // unary, evaluatable
	EXPR_BLOCK, // variadic, non-evaluatable
	EXPR_DECL_INFO, // nullary, non-evaluatable
	EXPR_REF, // unary, non-evaluatable
	EXPR_PROC_CALL, // variadic, evaluatable
	EXPR_RETURN, // unary, non-evaluatable
	EXPR_ADD, // binary, evaluatable
	EXPR_SUB, // binary, evaluatable
	EXPR_MUL, // binary, evaluatable
	EXPR_DIV, // binary, evaluatable
	EXPR_MOD, // binary, evaluatable
	EXPR_AND, // binary, evaluatable
	EXPR_OR, // binary, evaluatable
	EXPR_XOR, // binary, evaluatable
	EXPR_SHL, // binary, evaluatable
	EXPR_SHR, // binary, evaluatable
	EXPR_NOT, // unary, evaluatable
	EXPR_CAST, // binary, evaluatable
	EXPR_LOGICAL_EQ, // binary, evaluatable
	EXPR_LOGICAL_NEQ, // binary, evaluatable
	EXPR_LOGICAL_LE, // binary, evaluatable
	EXPR_LOGICAL_GE, // binary, evaluatable
	EXPR_LOGICAL_LT, // binary, evaluatable
	EXPR_LOGICAL_GT, // binary, evaluatable
	EXPR_LOGICAL_AND, // binary, evaluatable
	EXPR_LOGICAL_OR, // binary, evaluatable
	EXPR_LOGICAL_NOT, // unary, evaluatable
	EXPR_WRAPPER, // unary, evaluatable
	EXPR_IF, // ternary, non-evaluatable
	EXPR_VOID, // nullary, evaluatable
	EXPR_WHILE, // binary, non-evaluatable
	EXPR_DOWHILE, // binary, non-evaluatable
	EXPR_GET_REF, // unary, evaluatable
	EXPR_DEREF, // unary, evaluatable
	EXPR_GET_ITEM, // binary, evaluatable
	EXPR_ARRAY, // variadic, evaluatable
	EXPR_NEW_PROC, // variadic, non-evaluatable
	EXPR_NEW_TYPE, // variadic, non-evaluatable
	EXPR_GET_ATTR, // binary, evaluatable
	N_EXPR_TYPES
} ExprType;

// expression attributes
#define ATTR_STATIC   1
// for variables: it is statically allocated in the program
#define ATTR_EXTERNAL 2
// for procedures and variables: the field is supposed to be accessed outside the current compilation unit
#define ATTR_AUTO_STATIC 4
// for variables: it was declared as `static` implicitly; this attribute is used for error reporting
#define ATTR_NOT_DEFINED 8
// for procedures: the implementation is not defined

typedef struct expr { // in variadic expressions, first 16 bytes (i.e. first 2 argument fields) are used as array of sub-expressions
	union {
		struct expr* arg1;
	};
	union {
		struct {
			union {
				struct expr* arg2; // for binary expressions
				uint64_t int_literal; // for EXPR_INT
				struct { // for EXPR_DECL_INFO
					int attrs;
					int n_args; // defined if EXPR_DECL_INFO is used as the info about the procedure declaration
				};
				char* name; // for EXPR_NAME or EXPR_REF
			};
			union {
				struct expr* arg3; // for ternary expressions
				struct typedef_t {
					TypeKind kind;
					size_t n_items;
					union {
						size_t size;
						struct typedef_t* base;
						struct expr* decl;
					};
				}* var_type;
				struct typedef_t* element_type; // for EXPR_ARRAY
				int n_local_vars; // for EXPR_BLOCK
				BRB_Syscall syscall_id; // for EXPR_SYSCALL
				BRB_Builtin builtin_id; // for EXPR_BUILTIN
			};
		};
		sbuf string; // for EXPR_STRING
	};
	struct expr* block;
	TokenLoc loc;
	ExprType type;
} Expr;
declChain(Expr);
defChain(Expr);

typedef struct typedef_t TypeDef;

#define BUILTIN_VAL_TYPE ((TypeDef){ .kind = KIND_BUILTIN_VAL })
#define VOID_TYPE ((TypeDef){ .kind = KIND_VOID })
#define BOOL_TYPE ((TypeDef){ .kind = KIND_BOOL })
#define INT_TYPE(_size) ((TypeDef){ .kind = KIND_INT, .size = _size })
static TypeDef PTR_TYPE(TypeDef base)
{
	TypeDef* base_p = malloc(sizeof(TypeDef));
	*base_p = base;
	return (TypeDef){ .kind = KIND_PTR, .base = base_p };
}
static TypeDef ARRAY_TYPE(TypeDef base, int n_items)
{
	TypeDef* base_p = malloc(sizeof(TypeDef));
	*base_p = base;
	return (TypeDef){ .kind = KIND_PTR, .base = base_p, .n_items = n_items };
}
#define CUSTOM_TYPE(_decl) ((TypeDef){ .kind = KIND_CUSTOM, .decl = _decl })
static TypeDef TYPE_STR_LITERAL;
#define isPtrType(tdef) ((tdef).kind == KIND_ARRAY || (tdef).kind == KIND_PTR)
#define isIntType(tdef) ((tdef).kind == KIND_BUILTIN_VAL || (tdef).kind == KIND_INT || (tdef).kind == KIND_BOOL)

typedef struct {
	Expr root;
	BRP* preprocessor;
} AST;

// flags for parseExpr desribing how the expression will be used
#define EXPRTERM_FULL         1
#define EXPRTERM_ARG          2
#define EXPRTERM_BRACKET      4
#define EXPRTERM_SQBRACKET    8
#define EXPRTERM_ARRAY_ARG    16
#define EXPRTERM_DOWHILE_BODY 32
#define EXPRTERM_IF_BODY      1024
#define EXPRTERM              ( \
	EXPRTERM_FULL \
	| EXPRTERM_ARG \
	| EXPRTERM_BRACKET \
	| EXPRTERM_SQBRACKET \
	| EXPRTERM_ARRAY_ARG \
	| EXPRTERM_DOWHILE_BODY \
	| EXPRTERM_IF_BODY \
)

#define EXPRTYPE_EVALUATABLE  64
#define EXPRTYPE_VOIDABLE     128
#define EXPRTYPE_LOGICAL      256
#define EXPRTYPE_DECL         512
#define EXPRTYPE_TYPE_ATTR    2048
#define EXPRTYPE              ( \
	EXPRTYPE_EVALUATABLE \
	| EXPRTYPE_VOIDABLE \
	| EXPRTYPE_LOGICAL \
	| EXPRTYPE_DECL \
	| EXPRTYPE_TYPE_ATTR \
)

#define EXPR_VARIADIC    1
#define EXPR_NULLARY     2
#define EXPR_UNARY       4
#define EXPR_BINARY      8
#define EXPR_TERNARY     16
#define EXPR_ARITY       (EXPR_VARIADIC | EXPR_NULLARY | EXPR_UNARY | EXPR_BINARY | EXPR_TERNARY)
#define EXPR_EVALUATABLE 32
#define EXPR_CONSTANT    64
#define EXPR_DECL        128

uint8_t expr_flags[N_EXPR_TYPES] = {
	[EXPR_INVALID    ] = EXPR_NULLARY,
	[EXPR_SYSCALL    ] = EXPR_VARIADIC | EXPR_EVALUATABLE,
	[EXPR_NAME       ] = EXPR_NULLARY,
	[EXPR_BUILTIN    ] = EXPR_NULLARY | EXPR_EVALUATABLE | EXPR_CONSTANT,
	[EXPR_STRING     ] = EXPR_NULLARY | EXPR_EVALUATABLE | EXPR_CONSTANT,
	[EXPR_INT        ] = EXPR_NULLARY | EXPR_EVALUATABLE | EXPR_CONSTANT,
	[EXPR_NEW_VAR    ] = EXPR_VARIADIC | EXPR_DECL,
	[EXPR_GET        ] = EXPR_UNARY | EXPR_EVALUATABLE,
	[EXPR_BLOCK      ] = EXPR_VARIADIC, // TODO: make blocks evaluate to their "return" value
	[EXPR_DECL_INFO  ] = EXPR_NULLARY,
	[EXPR_REF        ] = EXPR_UNARY,
	[EXPR_PROC_CALL  ] = EXPR_VARIADIC | EXPR_EVALUATABLE,
	[EXPR_RETURN     ] = EXPR_UNARY,
	[EXPR_NOT        ] = EXPR_UNARY  | EXPR_EVALUATABLE,
	[EXPR_MUL        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_DIV        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_MOD        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_SUB        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_ADD        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_SHL        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_SHR        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_AND        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_XOR        ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_OR         ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_ASSIGN     ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_ADD_ASSIGN ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_SUB_ASSIGN ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_MUL_ASSIGN ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_DIV_ASSIGN ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_AND_ASSIGN ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_XOR_ASSIGN ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_OR_ASSIGN  ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_SHR_ASSIGN ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_SHL_ASSIGN ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_CAST       ] = EXPR_UNARY  | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_EQ ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_NEQ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_LT ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_GT ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_LE ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_GE ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_AND] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_OR ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_LOGICAL_NOT] = EXPR_UNARY  | EXPR_EVALUATABLE,
	[EXPR_WRAPPER    ] = EXPR_UNARY,
	[EXPR_IF         ] = EXPR_TERNARY,
	[EXPR_VOID       ] = EXPR_NULLARY | EXPR_EVALUATABLE | EXPR_CONSTANT,
	[EXPR_WHILE      ] = EXPR_BINARY,
	[EXPR_DOWHILE    ] = EXPR_BINARY,
	[EXPR_GET_REF    ] = EXPR_UNARY | EXPR_EVALUATABLE,
	[EXPR_DEREF      ] = EXPR_UNARY | EXPR_EVALUATABLE,
	[EXPR_GET_ITEM   ] = EXPR_BINARY | EXPR_EVALUATABLE,
	[EXPR_ARRAY      ] = EXPR_VARIADIC | EXPR_EVALUATABLE,
	[EXPR_NEW_PROC   ] = EXPR_VARIADIC | EXPR_DECL,
	[EXPR_NEW_TYPE   ] = EXPR_VARIADIC | EXPR_DECL,
	[EXPR_GET_ATTR   ] = EXPR_UNARY | EXPR_EVALUATABLE
};

static char expr_order_table[] = {
	[EXPR_INVALID    ] = 0,
	[EXPR_SYSCALL    ] = 0,
	[EXPR_NAME       ] = 0,
	[EXPR_BUILTIN    ] = 0,
	[EXPR_STRING     ] = 0,
	[EXPR_INT        ] = 0,
	[EXPR_NEW_VAR    ] = 0,
	[EXPR_NEW_PROC   ] = 0,
	[EXPR_NEW_TYPE   ] = 0,
	[EXPR_GET        ] = 0,
	[EXPR_BLOCK      ] = 0, // TODO: make blocks evaluate to their "return" value
	[EXPR_DECL_INFO  ] = 0,
	[EXPR_REF        ] = 0,
	[EXPR_PROC_CALL  ] = 0,
	[EXPR_CAST       ] = 0,
	[EXPR_WRAPPER    ] = 0,
	[EXPR_IF         ] = 0,
	[EXPR_WHILE      ] = 0,
	[EXPR_DOWHILE    ] = 0,
	[EXPR_VOID       ] = 0,
	[EXPR_GET_ITEM   ] = 0,
	[EXPR_GET_ATTR   ] = 0,
	[EXPR_ARRAY      ] = 0,
	[EXPR_NOT        ] = 1,
	[EXPR_LOGICAL_NOT] = 1,
	[EXPR_GET_REF    ] = 1,
	[EXPR_DEREF      ] = 1,
	[EXPR_MUL        ] = 2,
	[EXPR_DIV        ] = 2,
	[EXPR_MOD        ] = 2,
	[EXPR_SUB        ] = 3,
	[EXPR_ADD        ] = 3,
	[EXPR_SHL        ] = 4,
	[EXPR_SHR        ] = 4,
	[EXPR_AND        ] = 5,
	[EXPR_XOR        ] = 6,
	[EXPR_OR         ] = 7,
	[EXPR_LOGICAL_GT ] = 8,
	[EXPR_LOGICAL_LT ] = 8,
	[EXPR_LOGICAL_LE ] = 8,
	[EXPR_LOGICAL_GE ] = 8,
	[EXPR_LOGICAL_EQ ] = 9,
	[EXPR_LOGICAL_NEQ] = 9,
	[EXPR_LOGICAL_AND] = 10,
	[EXPR_LOGICAL_OR ] = 11,
	[EXPR_ASSIGN     ] = 12,
	[EXPR_MUL_ASSIGN ] = 12,
	[EXPR_DIV_ASSIGN ] = 12,
	[EXPR_ADD_ASSIGN ] = 12,
	[EXPR_SUB_ASSIGN ] = 12,
	[EXPR_AND_ASSIGN ] = 12,
	[EXPR_XOR_ASSIGN ] = 12,
	[EXPR_OR_ASSIGN  ] = 12,
	[EXPR_SHL_ASSIGN ] = 12,
	[EXPR_SHR_ASSIGN ] = 12,
	[EXPR_RETURN     ] = 13
};
static_assert(N_EXPR_TYPES == 56, "not all expression types have their arity and order set");

static inline void initExpr(Expr* expr)
{
	*(ExprChain*)expr = ExprChain_new(0);
}

static inline ExprChain* getSubexprs(Expr* expr)
{
	assert(expr_flags[expr->type] & EXPR_VARIADIC, "attempted to get subexpressions of a non-variadic expression %d", expr->type);
	return (ExprChain*)expr;
}

static inline Expr* addSubexpr(Expr* expr, Expr new)
{
	assert(expr_flags[expr->type] & EXPR_VARIADIC, "attempted to add a subexpression to a non-varaidic expression %d", expr->type);
	return ExprChain_append(getSubexprs(expr), new);
}

static inline Expr* getSubexpr(Expr* expr, int id)
{
	assert(expr_flags[expr->type] & EXPR_VARIADIC, "attempted to get a subexpression of a non-variadic expression %d", expr->type);
	return ExprChain_getref(*getSubexprs(expr), id);
}

static inline int getSubexprsCount(Expr* expr)
{
	return ExprChain_length(*getSubexprs(expr));
}

static char* getDeclName(Expr* decl)
{
	assert(expr_flags[decl->type] & EXPR_DECL, "expression of type %d is not a declaration", decl->type);
	decl = getSubexpr(decl, 0);
	assert(decl->type == EXPR_NAME, "declaration name must be an expression of type EXPR_NAME, instead got an expression of type %d", decl->type);
	return decl->name;
}

static Expr* getPrevDecl(Expr* decl)
{
	assert(expr_flags[decl->type] & EXPR_DECL, "expression of type %d is not a declaration", decl->type);
	decl = getSubexpr(decl, 1)->arg1;
	if (decl) assert(expr_flags[decl->type] & EXPR_DECL, "expression of type %d is not a declaration", decl->type);
	return decl;
}

static Expr* getProcDef(Expr* expr)
{
	assert(expr->type == EXPR_NEW_PROC, "expected an expression of type EXPR_NEW_PROC, instead got expression of type %d", expr->type);
	Expr* decl_info = getSubexpr(expr, 2);
	return decl_info->attrs & ATTR_NOT_DEFINED ? NULL : getSubexpr(getSubexpr(expr, 3), decl_info->n_args + 1);
}

static char* getFullDeclName(Expr* expr)
{
	char* res = strdup(getDeclName(expr));
	expr = expr->block;
	for (; expr->block != NULL; expr = expr->block) {
		char* namespace = getSubexpr(expr->block, 0)->name;
		if (namespace) res = tostr(fromstr(namespace), fromcstr("."), fromstr(res));
	}
	return res;
}

bool isLValue(Expr expr)
{
	if (expr.type == EXPR_GET_ATTR) {
		return isLValue(*expr.arg1);
	} else  return expr.type == EXPR_GET || expr.type == EXPR_DEREF;
}

const int COUNTER_BASE = __COUNTER__;
#define BR_ERROR_DEF(error_name, args) \
const short error_name##Code = (__COUNTER__ - COUNTER_BASE); \
DEF_WITH_ATTRS(void raise##error_name args, __abortlike)

void fprintDeclAttrs(FILE* dst, int attrs)
{
	if (attrs & ATTR_STATIC) {
		fputs("static ", dst);
	} else if (attrs & ATTR_EXTERNAL) {
		fputs("__external ", dst);
	}
}

void fprintType(FILE* dst, TypeDef type)
{
	static_assert(N_TYPE_KINDS == 8, "not all type kinds are handled in fprintType");
	switch (type.kind) {
		case KIND_INT:
			fprintf(dst, "int%zu", type.size * 8);
			break;
		case KIND_PTR:
			fprintType(dst, *type.base);
			fputc('*', dst);
			break;
		case KIND_VOID:
			fputs("void", dst);
			break;
		case KIND_BUILTIN_VAL:
			fputs("__builtin_val", dst);
			break;
		case KIND_BOOL:
			fputs("bool", dst);
			break;
		case KIND_ARRAY:
			fprintType(dst, *type.base);
			fprintf(dst, "[%zu]", type.n_items);
			break;
		case KIND_CUSTOM:
			fprintf(dst, "%s", getDeclName(type.decl));
			break;
		case KIND_NONE:
		case N_TYPE_KINDS:
		default:
			assert(false, "unknown type kind: %d", type.kind);
	}
}
#define printType(type) fprintType(stdout, type)

BR_ERROR_DEF(InvalidGlobalStmtError, (AST* ast, Token token))
{
	fprintTokenLoc(stderr, token.loc);
	eprintf("error %04hx: expected a global statement specifier, instead got ", InvalidGlobalStmtErrorCode);
	fprintTokenStr(stderr, token, ast->preprocessor);
	eputc('\n');
	exit(1);
}

BR_ERROR_DEF(VarDeclAsStmtBodyError, (AST* ast, TokenLoc loc, const char* stmt_name))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: variable declarations cannot be a body of %s\n", VarDeclAsStmtBodyErrorCode, stmt_name);
	exit(1);
}

BR_ERROR_DEF(UnknownBuiltinNameError, (AST* ast, Token token))
{
	fprintTokenLoc(stderr, token.loc);
	eprintf("error %04hx: unknown built-in value `%s`\n", UnknownBuiltinNameErrorCode, token.word);
	exit(1);
}

BR_ERROR_DEF(UnknownSyscallNameError, (AST* ast, Token token))
{
	fprintTokenLoc(stderr, token.loc);
	eprintf("error %04hx: unknown syscall name `%s`\n", UnknownSyscallNameErrorCode, token.word);
	exit(1);
}

BR_ERROR_DEF(InvalidExprError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: invalid expression\n", InvalidExprErrorCode);
	exit(1);
}

BR_ERROR_DEF(TooManySyscallArgsError, (AST* ast, TokenLoc loc, int n_args))
{
	fprintTokenLoc(stderr, loc);
	eprintf("expected %04hx at most 6 arguments for a syscall, instead got %d\n", TooManySyscallArgsErrorCode, n_args);
	exit(1);
}

BR_ERROR_DEF(NoValueExprError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: expected a value\n", NoValueExprErrorCode);
	exit(1);
}

BR_ERROR_DEF(UnknownNameError, (AST* ast, Token token))
{
	fprintTokenLoc(stderr, token.loc);
	eprintf("error %04hx: unknown name `%s`\n", UnknownNameErrorCode, token.word);
	exit(1);
}

BR_ERROR_DEF(NameTakenError, (AST* ast, Token token, TokenLoc decl_loc))
{
	fprintTokenLoc(stderr, token.loc);
	eprintf("error %04hx: name `%s` is already taken\n", NameTakenErrorCode, token.word);
	fprintTokenLoc(stderr, decl_loc);
	eprintf("note: declaration, which uses this name is here\n");
	exit(1);
}

BR_ERROR_DEF(InvalidTypeError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: invalid type specifier\n", InvalidTypeErrorCode);
	exit(1);
}

BR_ERROR_DEF(VoidVarDeclError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: can't declare a variable of type `", VoidVarDeclErrorCode);
	fprintType(stderr, VOID_TYPE);
	eputs("`\n");
	exit(1);
}

BR_ERROR_DEF(TooManyProcArgsError, (AST* ast, TokenLoc loc, int n_args))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: expected at most 6 arguments to a function, instead got %d", TooManyProcArgsErrorCode, n_args);
	exit(1);
}

BR_ERROR_DEF(ArgCountMismatchError, (AST* ast, TokenLoc loc, Expr* proc, int n_args))
{
	fprintTokenLoc(stderr, loc);
	assert(proc->type == EXPR_NEW_PROC, "`proc` expression must be of type EXPR_NEW_PROC");
	if (n_args >= 0) {
		eprintf(
			"error %04hx: procedure `%s` expects exactly %d argument(s), instead got %d\n",
			ArgCountMismatchErrorCode, getDeclName(proc), getSubexpr(proc, 2)->n_args, n_args
		);
	} else {
		eprintf(
			"error %04hx: procedure `%s` expects exactly %d argument(s), instead got more\n",
			ArgCountMismatchErrorCode, proc->name, getSubexpr(proc, 2)->n_args
		);
	}
	fprintTokenLoc(stderr, proc->loc);
	eprintf("note: procedure `%s` is declared here\n", getDeclName(proc));
	exit(1);
}

BR_ERROR_DEF(InvalidCastTargetTypeError, (AST* ast, TokenLoc loc, TypeDef target))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: cannot cast a value to type `", InvalidCastTargetTypeErrorCode);
	fprintType(stderr, target);
	eputs("`\nnote: `cast` built-in can only convert from/to numbers/pointers\n");
	exit(1);
}

BR_ERROR_DEF(MainProcRetTypeMismatchError, (AST* ast, TokenLoc loc, TypeDef ret_type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: the `main` procedure must return nothing, instead the return type of `", MainProcRetTypeMismatchErrorCode);
	fprintType(stderr, ret_type);
	eputs("` is declared\n");
	exit(1);
}

BR_ERROR_DEF(MainProcArgCountMismatchError, (AST* ast, TokenLoc loc, int n_args))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: the `main` procedure must accept exactly 0 arguments, instead %d arguments were declared\n", MainProcArgCountMismatchErrorCode, n_args);
	exit(1);
}

BR_ERROR_DEF(UnbracketedAssignExprError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: to use assignment expression as a value, wrap it in brackets\n", UnbracketedAssignExprErrorCode);
	exit(1);
}

void _raiseUnexpectedTokenError(AST* ast, Token actual, const char* msg, ...)
{
	va_list args;
	va_start(args, msg);
	int n_tokens = 0;
	while (va_arg(args, Token).type != TOKEN_NONE) n_tokens++;
	va_end(args);

	fprintTokenLoc(stderr, actual.loc);
	eprintf("error %04hx: expected ", (uint16_t)(__COUNTER__ - COUNTER_BASE));
	va_start(args, msg);
	for (int n = 0; n < n_tokens; n++) {
		Token iter = va_arg(args, Token);

		if (iter.type == TOKEN_WORD) {
			eputs("a word");
		} else if (iter.type == TOKEN_INT) {
			eputs("an integer");
		} else fprintTokenStr(stderr, iter, ast->preprocessor);
		if (n == n_tokens - 2) {
			eputs(" or ");
		} else if (n != n_tokens - 1) eputs(", ");
	}
	va_end(args);
	
	if (msg) {
		eprintf(" as %s, ", msg);
	} else eputs(", ");

	eputs("instead got ");
	fprintTokenStr(stderr, actual, ast->preprocessor);
	eputc('\n');
	exit(1);
}
#define raiseUnexpectedTokenError(ast, token, msg, ...) _raiseUnexpectedTokenError(ast, token, msg, __VA_ARGS__, (Token){0})

BR_ERROR_DEF(ForLoopTermExpectedError, (AST* ast, Token actual))
{
	fprintTokenLoc(stderr, actual.loc);
	eprintf("error %04hx: expected a `for` loop terminating expression\n", ForLoopTermExpectedErrorCode);
	exit(1);
}

BR_ERROR_DEF(ForLoopIncrExpectedError, (AST* ast, Token actual))
{
	fprintTokenLoc(stderr, actual.loc);
	eprintf("error %04hx: expected a `for` loop incrementing expression\n", ForLoopIncrExpectedErrorCode);
	exit(1);
}

BR_ERROR_DEF(UnreferrableExprError, (AST* ast, TokenLoc loc, TypeDef subexpr_type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: the following expression is an rvalue of type `", UnreferrableExprErrorCode);
	printType(subexpr_type);
	eputs("`, which cannot be referenced\n");
	exit(1);
}

BR_ERROR_DEF(UndereferrableExprError, (AST* ast, TokenLoc loc, TypeDef subexpr_type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: the following expression is of type `", UndereferrableExprErrorCode);
	fprintType(stderr, subexpr_type);
	eputs("`, which can't be dereferenced\n");
	exit(1);
}

BR_ERROR_DEF(ComparisonTypeMismatchError, (AST* ast, TokenLoc loc, TypeDef entry_type, TypeDef field_type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: cannot compare a value of type `", ComparisonTypeMismatchErrorCode);
	fprintType(stderr, entry_type);
	eputs("` to a value of type `");
	fprintType(stderr, field_type);
	eputs("`\n");
	exit(1);
}

BR_ERROR_DEF(AssignmentTypeMismatchError, (AST* ast, TokenLoc loc, Expr* var_decl, TypeDef entry_type))
{
	fprintTokenLoc(stderr, loc);
	char* var_name = getSubexpr(var_decl, 0)->name;
	eprintf("error %04hx: variable `%s` expects a value of type `", AssignmentTypeMismatchErrorCode, var_name);
	fprintType(stderr, *getSubexpr(var_decl, 2)->var_type);
	eputs("`, instead got a value of type `");
	fprintType(stderr, entry_type);
	eputs("`\n");
	fprintTokenLoc(stderr, var_decl->loc);
	eprintf("note: variable `%s` is declared here\n", var_name);
	exit(1);
}

BR_ERROR_DEF(ReturnTypeMismatchError, (AST* ast, TokenLoc loc, TypeDef entry_type, Expr* proc))
{
	fprintTokenLoc(stderr, loc);
	assert(proc->type == EXPR_NEW_PROC, "`proc` expression must be of type EXPR_NEW_PROC");
	eprintf("error %04hx: procedure `%s` is declared to return a value of type `", ReturnTypeMismatchErrorCode, getDeclName(proc));
	fprintType(stderr, *getSubexpr(proc, 2)->var_type);
	eputs("`, instead attempted to return value of type `");
	fprintType(stderr, entry_type);
	eputs("`\n");
	fprintTokenLoc(stderr, proc->loc);
	eprintf("note: procedure `%s` is declared here\n", getDeclName(proc));
	exit(1);
}

BR_ERROR_DEF(ArgTypeMismatchError, (AST* ast, TokenLoc loc, Expr* proc, Expr* arg_decl, TypeDef entry_type))
{
	fprintTokenLoc(stderr, loc);
	if (proc) {
		eprintf(
			"error %04hx: argument `%s` of procedure `%s` expects a value of type `",
			ArgTypeMismatchErrorCode,
			getSubexpr(arg_decl, 0)->name,
			getDeclName(proc)
		);
		fprintType(stderr, *getSubexpr(arg_decl, 2)->var_type);
	} else {
		eprintf("error %04hx: a syscall argument expects a value of a primitive type `", ArgTypeMismatchErrorCode);
	}
	eputs("`, instead got value of type `");
	fprintType(stderr, entry_type);
	eputs("`\n");
	if (proc) {
		fprintTokenLoc(stderr, proc->loc);
		eprintf("note: procedure `%s` is declared here\n", getDeclName(proc));
	}
	exit(1);
}

BR_ERROR_DEF(DivByZeroError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: attempted to divide by zero, which is an undefined operation\n", DivByZeroErrorCode);
	exit(1);
}

BR_ERROR_DEF(ModZeroError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: attempted to get modulo zero of a value, which is an undefined operation\n", ModZeroErrorCode);
	exit(1);
}

BR_ERROR_DEF(NegativeBitShiftError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: attempted to bit-shift a value by a negative amount of bits, which is an undefined operation\n", NegativeBitShiftErrorCode);
	exit(1);
}

BR_ERROR_DEF(InvalidComplexAssignmentError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: complex assignment operator cannot be used with a variable declaration\n", InvalidComplexAssignmentErrorCode);
	exit(1);
}

BR_ERROR_DEF(NoProcReturnError, (AST* ast, Expr* decl))
{
	fprintTokenLoc(stderr, getSubexpr(decl, 3)->loc);
	eprintf(
		"error %04hx: the procedure `%s` is declared to return a value, but its definition does not return a value in all control paths\n",
		NoProcReturnErrorCode,
		getDeclName(decl)
	);
	exit(1);
}

BR_ERROR_DEF(VoidArrayError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: invalid type; an array cannot contain items of type `", VoidArrayErrorCode);
	fprintType(stderr, VOID_TYPE);
	eputs("`\n");
	exit(1);
}

BR_ERROR_DEF(UnindexableExprError, (AST* ast, TokenLoc loc, TypeDef entry_type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: value of type `", UnindexableExprErrorCode);
	fprintType(stderr, entry_type);
	eputs("` can't be interpreted as an array\n");
	exit(1);
}

BR_ERROR_DEF(InvalidArrayIndexError, (AST* ast, TokenLoc loc, TypeDef index_type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: array index must be of an integer type, instead got value of type `", InvalidArrayIndexErrorCode);
	fprintType(stderr, index_type);
	eputs("`\n");
	exit(1);
}

BR_ERROR_DEF(OutOfBoundsIndexError, (AST* ast, TokenLoc loc, TypeDef array_type, int64_t index))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: value of type `", OutOfBoundsIndexErrorCode);
	fprintType(stderr, array_type);
	eprintf("` doesn't have an element at index %lld\n", index);
	exit(1);
}

BR_ERROR_DEF(NegativeArraySizeError, (AST* ast, TokenLoc loc, int64_t n_items))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: an array can't have %lld items\n", NegativeArraySizeErrorCode, n_items);
	exit(1);
}
BR_ERROR_DEF(ElementTypeMismatchError, (AST* ast, TokenLoc loc, int index, TypeDef expected, TypeDef actual))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: expected all elements of an array literal to be of type `", ElementTypeMismatchErrorCode);
	fprintType(stderr, expected);
	eprintf("`, instead the element at index %d is of type `", index);
	fprintType(stderr, actual);
	eputs("`\n");
	exit(1);
}

BR_ERROR_DEF(InvalidArrayLiteralUseError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: invalid use of array literals; an array literal can only be used in a variable declaration\n", InvalidArrayLiteralUseErrorCode);
	exit(1);
}

BR_ERROR_DEF(UnexpectedArrayInitError, (AST* ast, TokenLoc loc, TypeDef decl_type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: invalid use of array literals; cannot initialize a variable of type `", UnexpectedArrayInitErrorCode);
	fprintType(stderr, decl_type);
	eprintf("` with an array literal\n");
	exit(1);
}

BR_ERROR_DEF(NestedProcError, (AST* ast, TokenLoc loc)) // TODO: allow nested procedures, i.e. closures
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: nested procedures are not allowed\n", NestedProcErrorCode);
	exit(1);
}

BR_ERROR_DEF(NonCallableError, (AST* ast, TokenLoc loc, TypeDef decl_type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: instance of type `", NonCallableErrorCode);
	fprintType(stderr, decl_type);
	eprintf("` is not callable\n");
	exit(1);
}

BR_ERROR_DEF(TypeSpecExpectedError, (AST* ast, TokenLoc loc))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: expected a type specifier after declaration parameters\n", TypeSpecExpectedErrorCode);
	exit(1);
}

BR_ERROR_DEF(UnexpectedStaticKwError, (AST* ast, TokenLoc loc, const char* msg))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: `static` declaration parameter is not allowed in %s\n", UnexpectedStaticKwErrorCode, msg);
	exit(1);
}

BR_ERROR_DEF(NoProcDefError, (AST* ast, Expr proc))
{
	fprintTokenLoc(stderr, proc.loc);
	eprintf("error %04hx: procedure `%s` was declared, but was never defined\n", NoProcDefErrorCode, getDeclName(&proc));
	exit(1);
}

BR_ERROR_DEF(ClassAttrInitError, (AST* ast, Expr field, Expr type_decl))
{
	fprintTokenLoc(stderr, field.loc);
	eprintf("error %04hx: default values for class attributes are not supported yet\n", ClassAttrInitErrorCode);
	fprintTokenLoc(stderr, type_decl.loc);
	eprintf("note: type `%s` is declared here\n", getDeclName(&type_decl));
	exit(1);
}

BR_ERROR_DEF(InvalidClassFieldKindError, (AST* ast, Expr field, Expr type_decl))
{
	fprintTokenLoc(stderr, field.loc);
	eprintf("error %04hx: only attributes' and methods' declarations are allowed inside a type definition\n", InvalidClassFieldKindErrorCode);
	fprintTokenLoc(stderr, type_decl.loc);
	eprintf("note: type `%s` is declared here\n", getDeclName(&type_decl));
	exit(1);
}

TypeDef getBaseType(TypeDef type)
{
	TypeDef* iter;
	for (iter = &type; isPtrType(*iter); iter = iter->base);
	return *iter;
}

BR_ERROR_DEF(ExcessBaseTypeError, (AST* ast, TokenLoc loc, TypeDef type1, TypeDef type2))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: expected exactly one base type in a type specifier, instead got both `", ExcessBaseTypeErrorCode);
	fprintType(stderr, getBaseType(type1));
	eputs("` and `");
	fprintType(stderr, getBaseType(type2));
	eputs("`\n");
	exit(1);
}

BR_ERROR_DEF(AttrOfPrimitiveError, (AST* ast, TokenLoc loc, TypeDef type))
{
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: type `", AttrOfPrimitiveErrorCode);
	fprintType(stderr, type);
	eputs("` cannot have any attributes\n");
	exit(1);
}

BR_ERROR_DEF(AttrNotFoundError, (AST* ast, TokenLoc loc, TypeDef type, char* attr_name))
{
	char* type_name = getFullDeclName(getBaseType(type).decl);
	fprintTokenLoc(stderr, loc);
	eprintf("error %04hx: type `%s` doesn't have an attribute named `%s`\n", AttrNotFoundErrorCode, type_name, attr_name);
	fprintTokenLoc(stderr, type.decl->loc);
	eprintf("note: type `%s` is defined here\n", type_name);
	free(type_name);
	exit(1);
}

BR_ERROR_DEF(NoTypeDefError, (AST* ast, Expr decl))
{
	fprintTokenLoc(stderr, decl.loc);
	eprintf("error %04hx: type `%s` was declared but was never defined\n", NoTypeDefErrorCode, getFullDeclName(&decl));
	exit(1);
}

int getTypeSize(TypeDef type)
{
	static_assert(N_TYPE_KINDS == 8, "not all type kinds are handled in getTypeSize");
	switch (type.kind) {
		case KIND_INT: return type.size;
		case KIND_PTR:
		case KIND_ARRAY:
		case KIND_BUILTIN_VAL:
			return 8;
		case KIND_BOOL: return 1;
		case KIND_CUSTOM: {
			int res = 0;
			chainForeach (Expr, attr, ExprChain_slice(*getSubexprs(getSubexpr(type.decl, 2)), 1, -1)) {
				if (attr->type == EXPR_NEW_VAR) {
					Expr* attr_info = getSubexpr(attr, 2);
					if (attr_info->attrs & ATTR_STATIC) continue;
					res += getTypeSize(*attr_info->var_type);
				}
			}
			return alignby(res, 8);
		}
		case KIND_VOID: assert(false, "attempted to get size of type `void`");
		case KIND_NONE:
		case N_TYPE_KINDS:
		default:
			assert(false, "unknown type kind: %d", type.kind);
	}
}

int getTypeMemorySize(TypeDef type)
{
	return type.kind == KIND_ARRAY ? getTypeMemorySize(*type.base) * type.n_items : getTypeSize(type);
}

int getAttrOffset(Expr* attr)
{
	int res = 0;
	for (attr = getPrevDecl(attr); attr; attr = getPrevDecl(attr)) {
		Expr* decl_info = getSubexpr(attr, 2);
		if (attr->type == EXPR_NEW_VAR ? !(decl_info->attrs & ATTR_STATIC) : false)
			res += getTypeMemorySize(*decl_info->var_type);
	}
	return res;
}

uint64_t getIndirLevel(TypeDef type)
{
	uint64_t res = 0;
	for (TypeDef* iter = &type; isPtrType(*iter); iter = iter->base) res += 1;
	return res;
}

bool _typeMatches(TypeDef field, TypeDef entry)
{
	static_assert(N_TYPE_KINDS == 8, "not all type kinds are handled in typeMatches");
	if (entry.kind == KIND_VOID) return true;
	switch (field.kind) {
		case KIND_VOID: return true;
		case KIND_PTR:
			return isPtrType(entry) ? _typeMatches(*field.base, *entry.base) : entry.kind == KIND_BUILTIN_VAL;
		case KIND_ARRAY:
			if (entry.kind == KIND_PTR) {
				return _typeMatches(*field.base, *entry.base);
			} else if (entry.kind == KIND_ARRAY) {
				return _typeMatches(*field.base, *entry.base) && entry.n_items <= field.n_items;
			} else return entry.kind == KIND_BUILTIN_VAL;
		case KIND_INT:
			return (entry.kind == KIND_INT && entry.size == field.size) ||
				(entry.kind == KIND_BUILTIN_VAL && field.size == 8) ||
				(entry.kind == KIND_BOOL && field.size == 1);
		case KIND_BUILTIN_VAL: return getTypeSize(entry) == 8;
		case KIND_BOOL: return entry.kind == KIND_BOOL;
		case KIND_CUSTOM:
			return entry.kind == KIND_CUSTOM ? field.decl == entry.decl : false;
		case KIND_NONE:
		case N_TYPE_KINDS:
		default:
			assert(false, "unknown type kind: %d", field.kind);
	}
}

bool typeMatches(TypeDef field, TypeDef entry)
{
	return field.kind == KIND_VOID ? false : _typeMatches(field, entry);
}

static Expr* getDecl(Expr* block, char* name, Expr* bound)
{
	for (; block != bound; block = block->block) {
		assert(block->type == EXPR_BLOCK, "`block` argument is expected to be an expression of type EXPR_BLOCK");
		for (Expr* expr = getSubexpr(block, 0)->arg1; expr; expr = getPrevDecl(expr)) {
			assert(
				expr_flags[expr->type] & EXPR_DECL,
				"unexpected expression type %d in the declaration chain",
				expr->type
			);
			if (streq(name, getDeclName(expr))) return expr;
		}
	}
	return NULL;
}

bool parseType(AST* ast, TypeDef* dst, int* attrs_p, int allowed_attrs, const char* ctx_msg, Expr* block)
{
	static_assert(N_TYPE_KINDS == 8, "not all type kinds are handled in parseType");
	*dst = (TypeDef){0};
	*attrs_p = 0;
	bool fetched = false;
	while (true) {
		Token token = peekToken(ast->preprocessor);
		if (token.type == TOKEN_KEYWORD) {
			switch (token.keyword_id) {
				case KW_INT8:
				case KW_INT16:
				case KW_INT32:
				case KW_INT64:
					if (dst->kind != KIND_NONE) raiseExcessBaseTypeError(ast, token.loc, *dst, INT_TYPE(1 << (token.keyword_id - KW_INT8)));

					fetchToken(ast->preprocessor);
					dst->kind = KIND_INT;
					fetched = true;
					dst->size = 1 << (token.keyword_id - KW_INT8);
					break;
				case KW_VOID:
					if (dst->kind != KIND_NONE) raiseExcessBaseTypeError(ast, token.loc, *dst, VOID_TYPE);

					fetchToken(ast->preprocessor);
					fetched = true;
					dst->kind = KIND_VOID;
					break;
				case KW_BOOL:
					if (dst->kind != KIND_NONE) raiseExcessBaseTypeError(ast, token.loc, *dst, BOOL_TYPE);

					fetchToken(ast->preprocessor);
					fetched = true;
					dst->kind = KIND_BOOL;
					break;
				case KW_STATIC:
					if (!(allowed_attrs & ATTR_STATIC)) raiseUnexpectedStaticKwError(ast, token.loc, ctx_msg);
					*attrs_p |= ATTR_STATIC;
					fetchToken(ast->preprocessor);
					fetched = true;
					break;
				default:
					if (fetched && !dst->kind) raiseTypeSpecExpectedError(ast, token.loc);
					return fetched;
			}
		} else if (token.type == TOKEN_SYMBOL) {
			switch (token.symbol_id) {
				case SYMBOL_STAR: {
					if (dst->kind == KIND_NONE) {
						if (fetched && !dst->kind) raiseTypeSpecExpectedError(ast, token.loc);
						return fetched;
					}

					fetchToken(ast->preprocessor);
					TypeDef* new_base = malloc(sizeof(TypeDef));
					*new_base = *dst;
					dst->base = new_base;
					dst->kind = KIND_PTR;
					break;
				} case SYMBOL_INDEX_START: {
					if (dst->kind == KIND_NONE) return fetched;
					if (dst->kind == KIND_VOID) raiseVoidArrayError(ast, token.loc);

					fetchToken(ast->preprocessor);
					Token index_spec = fetchToken(ast->preprocessor);
					int64_t n_items = 0;
					if (index_spec.type == TOKEN_INT) {
						n_items = index_spec.value;
						if (n_items < 0) raiseNegativeArraySizeError(ast, index_spec.loc, n_items);
						token = fetchToken(ast->preprocessor);
						if (getTokenSymbolId(token) != SYMBOL_INDEX_END)
							raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_INDEX_END));
					} else if (getTokenSymbolId(index_spec) != SYMBOL_INDEX_END)
						raiseUnexpectedTokenError(ast, index_spec, "the array size specifier", symbolToken(SYMBOL_INDEX_END), intToken(0));
					
					TypeDef* new_base = malloc(sizeof(TypeDef));
					*new_base = *dst;
					dst->base = new_base;
					dst->kind = n_items ? KIND_ARRAY : KIND_PTR;
					dst->n_items = n_items;
					break;
				} default:
					if (fetched && !dst->kind) raiseTypeSpecExpectedError(ast, token.loc);
					return fetched;
			}
		} else if (token.type == TOKEN_WORD) {
			Expr* decl = getDecl(block, token.word, NULL);
			if (decl ? decl->type == EXPR_NEW_TYPE : false) {
				if (dst->kind != KIND_NONE) raiseExcessBaseTypeError(ast, token.loc, *dst, CUSTOM_TYPE(decl));
				dst->kind = KIND_CUSTOM;
				dst->decl = decl;
				fetched = true;
				fetchToken(ast->preprocessor);
			} else {
				if (fetched && !dst->kind) raiseTypeSpecExpectedError(ast, token.loc);
				return fetched;
			}
		} else {
			if (fetched && !dst->kind) raiseTypeSpecExpectedError(ast, token.loc);
			return fetched;
		}
	}
}

bool isExprEvaluatable(Expr* expr)
{
	return expr->type == EXPR_WRAPPER ?
		isExprEvaluatable(expr->arg1) :
		expr_flags[expr->type] & EXPR_EVALUATABLE;
} 

bool isExprConstant(Expr* expr) {
	static_assert(N_EXPR_TYPES == 56, "not all expression types are handled in isExprEvaluatable");

	if (expr->type == EXPR_ARRAY) {
		chainForeach(Expr, element, *getSubexprs(expr)) {
			if (!isExprConstant(element)) return false;
		}
	}
	return expr_flags[expr->type] & EXPR_CONSTANT;
}

TypeDef getExprValueType(AST* ast, Expr expr);

bool isCBracketEnclosing(Expr* expr)
{
	switch (expr->type) {
		case EXPR_BLOCK:
			return true;
		case EXPR_IF:
			return isCBracketEnclosing(expr->arg3 ? expr->arg3 : expr->arg2);
		case EXPR_WHILE:
			return isCBracketEnclosing(expr->arg2);
		case EXPR_DOWHILE:
			return isCBracketEnclosing(expr->arg1);
		case EXPR_NEW_PROC:
			return getSubexpr(expr, 2)->attrs & ATTR_NOT_DEFINED
				? false
				: isCBracketEnclosing(getProcDef(expr));
		case EXPR_NEW_TYPE:
			return getSubexprsCount(expr) == 3 ? isCBracketEnclosing(getSubexpr(expr, 2)) : false;
		case EXPR_SYSCALL:
		case EXPR_NAME:
		case EXPR_BUILTIN:
		case EXPR_STRING:
		case EXPR_INT:
		case EXPR_NEW_VAR:
		case EXPR_ASSIGN:
		case EXPR_ADD_ASSIGN:
		case EXPR_SUB_ASSIGN:
		case EXPR_MUL_ASSIGN:
		case EXPR_DIV_ASSIGN:
		case EXPR_AND_ASSIGN:
		case EXPR_XOR_ASSIGN:
		case EXPR_OR_ASSIGN:
		case EXPR_SHL_ASSIGN:
		case EXPR_SHR_ASSIGN:
		case EXPR_GET:
		case EXPR_DECL_INFO:
		case EXPR_REF:
		case EXPR_PROC_CALL:
		case EXPR_RETURN:
		case EXPR_ADD:
		case EXPR_SUB:
		case EXPR_MUL:
		case EXPR_DIV:
		case EXPR_MOD:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_XOR:
		case EXPR_SHL:
		case EXPR_SHR:
		case EXPR_NOT:
		case EXPR_CAST:
		case EXPR_LOGICAL_EQ:
		case EXPR_LOGICAL_NEQ:
		case EXPR_LOGICAL_LE:
		case EXPR_LOGICAL_GE:
		case EXPR_LOGICAL_LT:
		case EXPR_LOGICAL_GT:
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR:
		case EXPR_LOGICAL_NOT:
		case EXPR_WRAPPER:
		case EXPR_VOID:
		case EXPR_GET_REF:
		case EXPR_DEREF:
		case EXPR_GET_ITEM:
		case EXPR_ARRAY:
		case EXPR_GET_ATTR:
			return false;
		case EXPR_INVALID:
		case N_EXPR_TYPES:
		default:
			assert(false, "unknown expression type: %u\n", expr->type);
	}
}

void fprintExpr(AST* ast, FILE* dst, Expr expr, int indent_level)
{
	static_assert(N_EXPR_TYPES == 56, "not all expression types are handled in fprintExpr");

	switch (expr.type) {
		case EXPR_SYSCALL: {
			bool first = true;
			fprintf(dst, "sys %s(", getSubexpr(&expr, 0)->name);
			chainForeach (Expr, subexpr, ExprChain_slice(*getSubexprs(&expr), 1, -1)) {
				if (first) {
					first = false;
				} else fputs(", ", dst);
				fprintExpr(ast, dst, *subexpr, indent_level + 1);
			}
			fputc(')', dst);
			break;
		}
		case EXPR_BUILTIN:
			fputs("builtin ", dst);
		case EXPR_NAME:
			fprintf(dst, "%s", expr.name);
			break;
		case EXPR_STRING:
			fprintf(dst, "\"%.*s\"", unpack(expr.string));
			break;
		case EXPR_INT:
			fprintf(dst, "%lld", expr.int_literal);
			break;
		case EXPR_GET:
			fprintf(dst, "%s", getDeclName(expr.arg1));
			break;
		case EXPR_ASSIGN:
		case EXPR_ADD_ASSIGN:
		case EXPR_SUB_ASSIGN:
		case EXPR_MUL_ASSIGN:
		case EXPR_DIV_ASSIGN:
		case EXPR_AND_ASSIGN:
		case EXPR_OR_ASSIGN:
		case EXPR_XOR_ASSIGN:
		case EXPR_SHL_ASSIGN:
		case EXPR_SHR_ASSIGN:{
			static char* assignment_symbols[] = {
				[EXPR_ASSIGN    ] = " = ",
				[EXPR_ADD_ASSIGN] = " += ",
				[EXPR_SUB_ASSIGN] = " -= ",
				[EXPR_MUL_ASSIGN] = " *= ",
				[EXPR_DIV_ASSIGN] = " /= ",
				[EXPR_AND_ASSIGN] = " &= ",
				[EXPR_OR_ASSIGN ] = " |= ",
				[EXPR_XOR_ASSIGN] = " ^= ",
				[EXPR_SHL_ASSIGN] = " <<= ",
				[EXPR_SHR_ASSIGN] = " >>= "
			};
			fputc('(', dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			fputs(assignment_symbols[expr.type], dst);
			fprintExpr(ast, dst, *expr.arg2, indent_level);
			fputc(')', dst);
			break;
		}
		case EXPR_NEW_VAR:
			fprintDeclAttrs(dst, getSubexpr(&expr, 2)->attrs);
			fprintType(dst, *getSubexpr(&expr, 2)->var_type);
			fprintf(dst, " %s", getDeclName(&expr));
			if (getSubexprsCount(&expr) == 4) {
				fputs(" = ", dst);
				fprintExpr(ast, dst, *getSubexpr(&expr, 3), indent_level);
			}
			break;
		case EXPR_DECL_INFO:
		case EXPR_REF:
			assert(false, "expression of type %d cannot be printed", expr.type);
			break;
		case EXPR_NEW_PROC: {
			bool first = true;
			Expr* decl_info = getSubexpr(&expr, 2);
			fprintDeclAttrs(dst, decl_info->attrs);
			fprintType(dst, *decl_info->var_type);
			fprintf(dst, " %s(", getDeclName(&expr));
			chainForeach (Expr, arg, ExprChain_slice(*getSubexprs(getSubexpr(&expr, 3)), 1, -2 + (decl_info->attrs & ATTR_NOT_DEFINED))) {
				if (first) {
					first = false;
				} else {
					fputs(", ", dst);
				}
				fprintExpr(ast, dst, *arg, indent_level + 1);
			}
			fputc(')', dst);
			if (!(decl_info->attrs & ATTR_NOT_DEFINED)) {
				fputc(' ', dst);
				fprintExpr(ast, dst, *getProcDef(&expr), indent_level);
			}
			break;
		}
		case EXPR_BLOCK:
		case EXPR_ARRAY:
			if (expr.block) fputs("{\n", dst);
			chainForeach (Expr, subexpr, ExprChain_slice(*getSubexprs(&expr), 1, -1)) {
				if (expr.block) repeat(indent_level + 1) fputs("    ", dst);
				fprintExpr(ast, dst, *subexpr, indent_level + 1);
				const char delim = expr.type == EXPR_BLOCK ? ';' : ',';
				if (!isCBracketEnclosing(subexpr)) fputc(delim, dst);
				fputc('\n', dst);
			}
			if (expr.block) {
				repeat(indent_level - 1) fputs("    ", dst);
				fputc('}', dst);
			}
			break;
		case EXPR_PROC_CALL: {
			fprintf(dst, "%s(", getDeclName(getSubexpr(&expr, 0)->arg1));
			bool first = true;
			chainForeach (Expr, subexpr, ExprChain_slice(*getSubexprs(&expr), 1, -1)) {
				if (first) {
					first = false;
				} else {
					fputs(", ", dst);
				}
				fprintExpr(ast, dst, *subexpr, indent_level + 1);
			}
			fputc(')', dst);
			break;
		}
		case EXPR_RETURN:
			fputs("return ", dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			break;
		case EXPR_ADD:
		case EXPR_SUB:
		case EXPR_MUL:
		case EXPR_DIV:
		case EXPR_MOD:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_XOR:
		case EXPR_SHL:
		case EXPR_SHR:
		case EXPR_LOGICAL_EQ:
		case EXPR_LOGICAL_NEQ:
		case EXPR_LOGICAL_LT:
		case EXPR_LOGICAL_GT:
		case EXPR_LOGICAL_LE:
		case EXPR_LOGICAL_GE:
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR:{
			static char* binary_op_symbols[] = {
				[EXPR_ADD] = " + ",
				[EXPR_SUB] = " - ",
				[EXPR_MUL] = " * ",
				[EXPR_DIV] = " / ",
				[EXPR_MOD] = " % ",
				[EXPR_AND] = " & ",
				[EXPR_OR ] = " | ",
				[EXPR_XOR] = " ^ ",
				[EXPR_SHL] = " << ",
				[EXPR_SHR] = " >> ",
				[EXPR_LOGICAL_EQ] = " == ",
				[EXPR_LOGICAL_NEQ] = " != ",
				[EXPR_LOGICAL_LT] = " < ",
				[EXPR_LOGICAL_GT] = " > ",
				[EXPR_LOGICAL_LE] = " <= ",
				[EXPR_LOGICAL_GE] = " >= ",
				[EXPR_LOGICAL_AND] = " && ",
				[EXPR_LOGICAL_EQ] = " || "
			};
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			fputs(binary_op_symbols[expr.type], dst);
			fprintExpr(ast, dst, *expr.arg2, indent_level);
			break;
		}
		case EXPR_NOT:
			fputc('~', dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			break;
		case EXPR_CAST:
			fputs("cast(", dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
			fputs(", ", dst);
			fprintType(dst, *expr.var_type);
			fputc(')', dst);
			break;
		case EXPR_LOGICAL_NOT:
			fputc('!', dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			break;
		case EXPR_WRAPPER:
			fputc('(', dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
			fputc(')', dst);
			break;
		case EXPR_IF:
			fputs("if (", dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
			fputs(") ", dst);
			fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
			if (expr.arg3) {
				fputs(" else ", dst);
				fprintExpr(ast, dst, *expr.arg3, indent_level + 1);
			}
			break;
		case EXPR_VOID:
			break;
		case EXPR_WHILE:
			fputs("while (", dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
			fputs(") ", dst);
			fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
			break;
		case EXPR_DOWHILE:
			fputs("do ", dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
			fputs(" while (", dst);
			fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
			fputc(')', dst);
			break;
		case EXPR_GET_REF:
			fputc('&', dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			break;
		case EXPR_DEREF:
			fputs("*(", dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			fputc(')', dst);
			break;
		case EXPR_GET_ITEM:
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			fputc('[', dst);
			fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
			fputc(']', dst);
			break;
		case EXPR_NEW_TYPE:
			fprintf(dst, "type %s ", getDeclName(&expr));
			if (getSubexprsCount(&expr) == 3)
				fprintExpr(ast, dst, *getSubexpr(&expr, 2), indent_level);
			break;
		case EXPR_GET_ATTR:
			fputc('(', dst);
			fprintExpr(ast, dst, *expr.arg1, indent_level);
			fprintf(dst, ").%s", getDeclName(expr.arg2));
			break;
		case EXPR_INVALID:
		case N_EXPR_TYPES:
		default:
			assert(false, "unknown expression type %d", expr.type);
	}
}
#define printExpr(ast, expr, indent_level) fprintExpr(ast, stdout, expr, indent_level)

bool isExprIntLiteral(Expr* expr)
{
	while (expr->type == EXPR_CAST) expr = expr->arg1;
	return expr->type == EXPR_INT;
}

uint64_t getIntLiteral(Expr* expr)
{
	while (expr->type == EXPR_CAST) expr = expr->arg1;
	assert(expr->type == EXPR_INT, "attempted to get integer literal of an expression that doesn't produce an integer literal");
	return expr->int_literal;
}

static Expr* wrapExpr(Expr* expr, ExprType new_expr_type, ...)
{
	Expr arg = *expr;
	expr->type = new_expr_type;
	va_list args;
	va_start(args, new_expr_type);
	
	switch (expr_flags[new_expr_type] & EXPR_ARITY) {
		case EXPR_VARIADIC:
			initExpr(expr);
			addSubexpr(expr, arg);
		case EXPR_NULLARY:
			break;
		case EXPR_BINARY:
			*(expr->arg1 = malloc(sizeof(Expr))) = arg;
			*(expr->arg2 = malloc(sizeof(Expr))) = va_arg(args, Expr);
			break;
		case EXPR_UNARY:
			*(expr->arg1 = malloc(sizeof(Expr))) = arg;
			break;
		default:
			assert(false, "unknown expression arity type %d", expr_flags[new_expr_type] & EXPR_ARITY);
	}
	va_end(args);
	return expr;
}

static void wrapExprInCast(Expr* expr, TypeDef new_type)
{
	if (expr->type != EXPR_INT) {
		Expr* arg1 = malloc(sizeof(Expr));
		*arg1 = *expr;
		expr->type = EXPR_CAST;
		expr->arg1 = arg1;
	}
	*(expr->var_type = malloc(sizeof(TypeDef))) = new_type;
}

typedef void (*ExprTypeSetter) (AST*, Expr*, ExprType);

void defaultExprTypeSetter(AST* ast, Expr* expr, ExprType new_type)
{
	expr->type = new_type;
}

void setExprProcCall(AST* ast, Expr* expr, ExprType new_type)
{
	if (expr->arg1->type == EXPR_NEW_VAR)
		raiseNonCallableError(ast, expr->loc, *getSubexpr(expr->arg1, 2)->var_type);
	expr->type = new_type;
	Expr new_subexpr = *expr;
	new_subexpr.type = EXPR_REF;
	initExpr(expr);
	addSubexpr(expr, new_subexpr);
}

void setBinaryExprType(AST* ast, Expr* expr, ExprType new_type)
{
	wrapExpr(expr, new_type, (Expr){0});
}

DEF_WITH_ATTRS(bool setExprType(AST* ast, Expr* expr, Expr* parent_expr, ExprType new_type), __result_use_check)
// changes the type of the expression if the new type is suitable in place of the current expression type
{
#ifndef __GNUC__
#error "Range initializers are not supported by the compiler"
#endif
	static_assert(N_EXPR_TYPES == 56, "not all expression types are handled in setExprType");
	static ExprTypeSetter override_table[N_EXPR_TYPES][N_EXPR_TYPES] = {
		[EXPR_INVALID   ] = {
			[EXPR_SYSCALL ... N_EXPR_TYPES - 1] = defaultExprTypeSetter,
			[EXPR_INVALID] = NULL,
			[EXPR_ADD ... EXPR_SHR] = NULL,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = NULL,
			[EXPR_GET_ITEM] = NULL,
			[EXPR_GET_ATTR] = NULL
		},
		[EXPR_SYSCALL   ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType
		},
		[EXPR_NAME      ] = {[0 ... N_EXPR_TYPES - 1] = NULL},
		[EXPR_BUILTIN   ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
		},
		[EXPR_STRING    ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType
		},
		[EXPR_INT       ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
		},
		[EXPR_REF       ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_NEW_VAR   ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_GET       ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType,
			[EXPR_PROC_CALL] = setExprProcCall,
			[EXPR_GET_ATTR] = setBinaryExprType
		},
		[EXPR_BLOCK     ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_DECL_INFO      ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_PROC_CALL ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType,
			[EXPR_GET_ATTR] = setBinaryExprType
		},
		[EXPR_RETURN    ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_ADD ... EXPR_NOT] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
		},
		[EXPR_CAST ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType
		},
		[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_NOT] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
		},
		[EXPR_WRAPPER ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType,
			[EXPR_GET_ATTR] = setBinaryExprType
		},
		[EXPR_IF      ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_VOID    ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_WHILE   ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_DOWHILE ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_GET_REF ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType,
			[EXPR_GET_ATTR] = setBinaryExprType
		},
		[EXPR_DEREF  ] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType,
			[EXPR_GET_ATTR] = setBinaryExprType
		},
		[EXPR_GET_ITEM] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType,
			[EXPR_GET_ATTR] = setBinaryExprType
		},
		[EXPR_ARRAY] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_GET_ITEM] = setBinaryExprType
		},
		[EXPR_NEW_PROC] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_NEW_TYPE] = { [0 ... N_EXPR_TYPES - 1] = NULL },
		[EXPR_GET_ITEM] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType,
			[EXPR_GET_ATTR] = setBinaryExprType
		},
		[EXPR_GET_ATTR] = {
			[0 ... N_EXPR_TYPES - 1] = NULL,
			[EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
			[EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
			[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
			[EXPR_GET_ITEM] = setBinaryExprType,
			[EXPR_GET_ATTR] = setBinaryExprType
		}
	};

	ExprTypeSetter setter = override_table[expr->type][new_type];
	if (!setter) raiseInvalidExprError(ast, expr->loc);
	if (parent_expr ? expr_order_table[parent_expr->type] <= expr_order_table[new_type] && expr_order_table[parent_expr->type] > 0 : false) return true;

	setter(ast, expr, new_type);
	return false;
}

TypeDef getExprValueType(AST* ast, Expr expr)
{
	static_assert(N_EXPR_TYPES == 56, "not all expression types are handled in getExprValueType");
	static_assert(N_TYPE_KINDS == 8, "not all type kinds are handled in getExprValueType");
	switch (expr.type) {
		case EXPR_INVALID:
		case EXPR_NAME:
		case EXPR_NEW_VAR:
		case EXPR_DECL_INFO:
		case EXPR_REF:
		case EXPR_BLOCK:
		case EXPR_RETURN:
		case EXPR_IF:
		case EXPR_WHILE:
		case EXPR_DOWHILE:
		case EXPR_NEW_TYPE:
		case EXPR_NEW_PROC:
			return (TypeDef){0};
		case EXPR_BUILTIN:
		case EXPR_SYSCALL:
			return BUILTIN_VAL_TYPE;
		case EXPR_STRING: return TYPE_STR_LITERAL;
		case EXPR_INT:
			if (FITS_IN_8BITS(expr.int_literal)) {
				return INT_TYPE(1);
			} else if (FITS_IN_16BITS(expr.int_literal)) {
				return INT_TYPE(2);
			} else if (FITS_IN_32BITS(expr.int_literal)) {
				return INT_TYPE(4);
			} else return INT_TYPE(8);
		case EXPR_PROC_CALL: return *getSubexpr(getSubexpr(&expr, 0)->arg1, 2)->var_type;
		case EXPR_GET: return *getSubexpr(expr.arg1, 2)->var_type;
		case EXPR_ASSIGN:
		case EXPR_ADD_ASSIGN:
		case EXPR_SUB_ASSIGN:
		case EXPR_MUL_ASSIGN:
		case EXPR_DIV_ASSIGN:
		case EXPR_AND_ASSIGN:
		case EXPR_XOR_ASSIGN:
		case EXPR_OR_ASSIGN:
		case EXPR_SHL_ASSIGN:
		case EXPR_SHR_ASSIGN:
			return getExprValueType(ast, *expr.arg1);
		case EXPR_ADD: {
			TypeDef arg1_type = getExprValueType(ast, *expr.arg1), arg2_type = getExprValueType(ast, *expr.arg2);
			if (arg1_type.kind == KIND_INT && arg2_type.kind != KIND_INT) swap(arg1_type, arg2_type, TypeDef);

			static_assert(N_TYPE_KINDS == 8, "not all type kinds are handled in handling EXPR_ADD in getExprValueType");
			assert(arg2_type.kind != KIND_CUSTOM, "operator overloading for user-defined types is not supported yet");
			switch (arg1_type.kind) {
				case KIND_INT:
					assert(arg2_type.kind == KIND_INT, "unexpected type kind %d", arg2_type.kind);
					return arg1_type.size == arg2_type.size ? INT_TYPE(minInt(arg1_type.size * 2, 8)) : (arg1_type.size > arg2_type.size ? arg1_type : arg2_type);
				case KIND_ARRAY:
				case KIND_PTR:
					return isPtrType(arg2_type) || (expr.arg2->type == EXPR_INT && expr.arg2->int_literal < 0) ? INT_TYPE(8) : arg1_type; 
				case KIND_BUILTIN_VAL:
					return INT_TYPE(8);
				case KIND_BOOL: {
					int arg1_size = getTypeSize(arg1_type), arg2_size = getTypeSize(arg2_type);
					return arg1_size == arg2_size ? INT_TYPE(minInt(arg1_size * 2, 8)) : (arg1_size > arg2_size ? INT_TYPE(arg1_size) : INT_TYPE(arg2_size));
				}
				case KIND_VOID:
				case KIND_NONE:
				case KIND_CUSTOM:
				case N_TYPE_KINDS:
				default:
					assert(false, "unexpected type kind %d", arg1_type.kind);
			}
			break;
		} case EXPR_SUB:
		case EXPR_MUL: {
			int arg1_size = getTypeSize(getExprValueType(ast, *expr.arg1));
			int arg2_size = getTypeSize(getExprValueType(ast, *expr.arg2));
			return arg1_size == arg2_size ? INT_TYPE(minInt(arg1_size * 2, 8)) : INT_TYPE(maxInt(arg1_size, arg2_size));
		} case EXPR_DIV: {
			int arg1_size = getTypeSize(getExprValueType(ast, *expr.arg1));
			int arg2_size = getTypeSize(getExprValueType(ast, *expr.arg2));
			return arg1_size == arg2_size ? INT_TYPE(arg1_size) : INT_TYPE(maxInt(arg1_size, arg2_size));
		} case EXPR_MOD: return INT_TYPE(getTypeSize(getExprValueType(ast, *expr.arg2)));
		case EXPR_AND: {
			int arg1_size = getTypeSize(getExprValueType(ast, *expr.arg1));
			int arg2_size = getTypeSize(getExprValueType(ast, *expr.arg2));
			return INT_TYPE(minInt(arg1_size, arg2_size));
		} case EXPR_OR: 
		case EXPR_XOR: {
			int arg1_size = getTypeSize(getExprValueType(ast, *expr.arg1));
			int arg2_size = getTypeSize(getExprValueType(ast, *expr.arg2));
			return INT_TYPE(maxInt(arg1_size, arg2_size));
		} case EXPR_SHL: return INT_TYPE(8);
		case EXPR_SHR:
		case EXPR_NOT:
			return INT_TYPE(getTypeSize(getExprValueType(ast, *expr.arg1)));
		case EXPR_CAST: return *expr.var_type;
		case EXPR_LOGICAL_EQ:
		case EXPR_LOGICAL_NEQ:
		case EXPR_LOGICAL_LT:
		case EXPR_LOGICAL_GT:
		case EXPR_LOGICAL_LE:
		case EXPR_LOGICAL_GE:
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR:
		case EXPR_LOGICAL_NOT:
			return BOOL_TYPE;
		case EXPR_WRAPPER: return getExprValueType(ast, *expr.arg1);
		case EXPR_VOID: return VOID_TYPE;
		case EXPR_GET_REF: return PTR_TYPE(getExprValueType(ast, *expr.arg1));
		case EXPR_DEREF:
		case EXPR_GET_ITEM:
			return *getExprValueType(ast, *expr.arg1).base;
		case EXPR_ARRAY:
			return ARRAY_TYPE(*expr.element_type, getSubexprsCount(&expr));
		case EXPR_GET_ATTR:
			return *getSubexpr(expr.arg2, 2)->var_type;
		case N_EXPR_TYPES:
		default:
			assert(false, "unknown expression type %d", expr.type);
	}
}

void printAST(AST* ast)
{
	printExpr(ast, ast->root, 0);
}

bool matchType(AST* ast, TypeDef field_type, Expr* expr)
{
	static_assert(N_TYPE_KINDS == 8, "not all type kinds are handled in typeMatches");

	TypeDef expr_val_type = getExprValueType(ast, *expr);
	if (expr_val_type.kind == KIND_VOID) return false;
	if (typeMatches(field_type, expr_val_type)) return true;

	TypeDef new_type = {0};
	if ((expr_val_type.kind == KIND_BUILTIN_VAL || expr_val_type.kind == KIND_INT || expr_val_type.kind == KIND_BOOL) && field_type.kind == KIND_INT) {
		new_type = field_type;
	} else if (field_type.kind == KIND_BUILTIN_VAL) {
		new_type = INT_TYPE(8);
	} else if (field_type.kind == KIND_BOOL) {
		new_type = BOOL_TYPE;
	}

	if (new_type.kind != KIND_NONE) {
		wrapExprInCast(expr, new_type);
		return true;
	}
	return false;
}

void removeExprWrappers(Expr* expr)
{
	if (expr->type == EXPR_WRAPPER) {
		Expr new_expr = *expr->arg1;
		free(expr->arg1);
		*expr = new_expr;
	}

	switch (expr_flags[expr->type] & EXPR_ARITY) {
		case EXPR_VARIADIC: {
			chainForeach (Expr, subexpr, ExprChain_slice(*getSubexprs(expr), 1, -1)) {
				removeExprWrappers(subexpr);
			}
			return;
		}
		case EXPR_NULLARY:
			return;
		case EXPR_TERNARY:
			if (expr->arg3) removeExprWrappers(expr->arg3);
		case EXPR_BINARY:
			removeExprWrappers(expr->arg2);
		case EXPR_UNARY:
			if (expr->arg1) removeExprWrappers(expr->arg1);
			return;
		default:
			assert(false, "expression type %d has unknown arity type %d", expr->type, expr_flags[expr->type] & EXPR_ARITY);
	}
}

void makeLogicalExpr(Expr* expr)
{
	static bool expr_logicality_table[] = {
		[0 ... N_EXPR_TYPES - 1] = false,
		[EXPR_LOGICAL_EQ ... EXPR_LOGICAL_NOT] = true,
	};
	if (!expr_logicality_table[expr->type])
		wrapExprInCast(expr, BOOL_TYPE);
}

bool validateReturnStmt(Expr expr)
{
	switch (expr.type) {
		case EXPR_BLOCK: {
			chainForeach(Expr, subexpr, *getSubexprs(&expr)) {
				if (validateReturnStmt(*subexpr)) return true;
			}
			return false;
		}
		case EXPR_RETURN:
			return true;
		case EXPR_IF:
			if (expr.arg3) return validateReturnStmt(*expr.arg2) && validateReturnStmt(*expr.arg3);
			return false;
		case EXPR_DOWHILE:
			return validateReturnStmt(*expr.arg1);
		case EXPR_SYSCALL:
		case EXPR_NAME:
		case EXPR_BUILTIN:
		case EXPR_STRING:
		case EXPR_INT:
		case EXPR_NEW_VAR:
		case EXPR_ASSIGN:
		case EXPR_ADD_ASSIGN:
		case EXPR_SUB_ASSIGN:
		case EXPR_MUL_ASSIGN:
		case EXPR_DIV_ASSIGN:
		case EXPR_AND_ASSIGN:
		case EXPR_XOR_ASSIGN:
		case EXPR_OR_ASSIGN:
		case EXPR_SHL_ASSIGN:
		case EXPR_SHR_ASSIGN:
		case EXPR_GET:
		case EXPR_DECL_INFO:
		case EXPR_REF:
		case EXPR_PROC_CALL:
		case EXPR_ADD:
		case EXPR_SUB:
		case EXPR_MUL:
		case EXPR_DIV:
		case EXPR_MOD:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_XOR:
		case EXPR_SHL:
		case EXPR_SHR:
		case EXPR_NOT:
		case EXPR_CAST:
		case EXPR_LOGICAL_EQ:
		case EXPR_LOGICAL_NEQ:
		case EXPR_LOGICAL_LE:
		case EXPR_LOGICAL_GE:
		case EXPR_LOGICAL_LT:
		case EXPR_LOGICAL_GT:
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR:
		case EXPR_LOGICAL_NOT:
		case EXPR_WRAPPER:
		case EXPR_VOID:
		case EXPR_WHILE:
		case EXPR_GET_REF:
		case EXPR_DEREF:
		case EXPR_GET_ITEM:
		case EXPR_ARRAY:
		case EXPR_NEW_PROC:
		case EXPR_NEW_TYPE:
		case EXPR_GET_ATTR:
			return false;
		case EXPR_INVALID:
		case N_EXPR_TYPES:
		default:
			assert(false, "unknown expression type: %u\n", expr.type);
	}
}

void parseExpr(AST* ast, Expr* dst, Expr* parent_expr, Expr* block, Expr* proc, int flags);

bool parseDecl(AST* ast, Token token, TypeDef var_type, int decl_attrs, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	TokenLoc type_loc = token.loc;

	token = fetchToken(ast->preprocessor);
	if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(ast, token, "a variable or procedure name", wordToken(NULL));
	Expr* decl = getDecl(dst->block, token.word, NULL);
	if (decl) raiseNameTakenError(ast, token, decl->loc);
	initExpr(dst);

	if (getTokenSymbolId(peekToken(ast->preprocessor)) == SYMBOL_ARGSPEC_START) {
		if (!(flags & EXPRTYPE_DECL)) raiseNestedProcError(ast, type_loc);
		if (setExprType(ast, dst, parent_expr, EXPR_NEW_PROC)) return true;
		fetchToken(ast->preprocessor);

		char* proc_name = addSubexpr(dst, (Expr){
			.type = EXPR_NAME,
			.block = dst->block,
			.loc = token.loc,
			.name = token.word
		})->name;

		addSubexpr(dst, (Expr){
			.type = EXPR_REF,
			.block = dst->block,
			.loc = type_loc,
			.arg1 = getSubexpr(dst->block, 0)->arg1
		});
		getSubexpr(dst->block, 0)->arg1 = dst;

		if (var_type.kind == KIND_ARRAY) var_type.kind = KIND_PTR;
		Expr* proc_info = addSubexpr(dst, (Expr){
			.type = EXPR_DECL_INFO,
			.block = dst->block,
			.loc = type_loc,
			.var_type = malloc(sizeof(TypeDef)),
			.attrs = decl_attrs
		});
		*proc_info->var_type = var_type;

// fetching procedure prototype specification
		Expr* proc_args = addSubexpr(dst, (Expr){
			.type = EXPR_BLOCK,
			.loc = peekToken(ast->preprocessor).loc,
			.block = dst->block
		});
		initExpr(proc_args);
		Expr* arg_block_ref = addSubexpr(proc_args, (Expr){
			.type = EXPR_REF,
			.loc = proc_args->loc,
			.block = proc_args,
			.arg1 = NULL
		});
	
		while (true) {
			Token type_spec = peekToken(ast->preprocessor);
			if (parseType(ast, &var_type, &decl_attrs, 0, "a declaration of procedure arguments", dst->block)) {
				if (var_type.kind == KIND_ARRAY) var_type.kind = KIND_PTR;
				token = fetchToken(ast->preprocessor);
				if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(ast, token, "an argument name", wordToken(NULL));

				Expr* new_arg = addSubexpr(proc_args, (Expr){
					.type = EXPR_NEW_VAR,
					.block = proc_args,
					.loc = type_spec.loc
				});
				initExpr(new_arg);

				addSubexpr(new_arg, (Expr){
					.type = EXPR_NAME,
					.block = proc_args,
					.name = token.word,
					.loc = token.loc
				});

				addSubexpr(new_arg, (Expr){
					.type = EXPR_REF,
					.block = proc_args,
					.arg1 = arg_block_ref->arg1,
					.loc = new_arg->loc
				});
				arg_block_ref->arg1 = new_arg;

				*addSubexpr(new_arg, (Expr){
					.type = EXPR_DECL_INFO,
					.block = proc_args,
					.var_type = malloc(sizeof(TypeDef)),
					.loc = new_arg->loc
				})->var_type = var_type;

				proc_info->n_args += 1;
			} else {
				token = fetchToken(ast->preprocessor);
				if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_END) {
					break;
				} else if (getTokenSymbolId(token) != SYMBOL_COMMA)
					raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_COMMA), symbolToken(SYMBOL_ARGSPEC_END));
			}
		}

		if (sbufeq(fromstr(proc_name), fromcstr("main"))) {
			proc_info->attrs |= ATTR_EXTERNAL;
			if (proc_info->var_type->kind != KIND_VOID) raiseMainProcRetTypeMismatchError(ast, type_loc, *proc_info->var_type);
			if (proc_info->n_args) raiseMainProcArgCountMismatchError(ast, token.loc, proc_info->n_args);
		}
		// parsing the procedure body
		token = peekToken(ast->preprocessor);
		if (getTokenSymbolId(token) == SYMBOL_SEMICOLON) {
			proc_info->attrs |= ATTR_NOT_DEFINED;
		} else {
			Expr* proc_body = addSubexpr(proc_args, (Expr){0});
			parseExpr(ast, proc_body, proc_args, proc_args, dst, EXPRTERM_FULL);
			if (proc_body->type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(ast, token.loc, "a procedure definition");
			if (proc_body->type == EXPR_BLOCK) {
				proc_body->n_local_vars = 0;
				getSubexpr(proc_body, 0)->name = proc_name;
			}
			if (proc_info->var_type->kind != KIND_VOID)
				if (!validateReturnStmt(*proc_body)) raiseNoProcReturnError(ast, dst);
		}
	} else {
		if (setExprType(ast, dst, parent_expr, EXPR_NEW_VAR)) return true;
		if (var_type.kind == KIND_VOID) raiseVoidVarDeclError(ast, type_loc);
		if (flags & EXPRTYPE_DECL && !(decl_attrs & ATTR_STATIC) && !(flags & EXPRTYPE_TYPE_ATTR))
			decl_attrs |= ATTR_AUTO_STATIC | ATTR_STATIC;

		addSubexpr(dst, (Expr){
			.type = EXPR_NAME,
			.block = dst->block,
			.loc = token.loc,
			.name = token.word
		});

		addSubexpr(dst, (Expr){
			.type = EXPR_REF,
			.arg1 = getSubexpr(dst->block, 0)->arg1,
			.block = dst->block,
			.loc = dst->loc
		});
		getSubexpr(dst->block, 0)->arg1 = dst;
		if (!(decl_attrs & ATTR_STATIC)) dst->block->n_local_vars += 1;

		*addSubexpr(dst, (Expr){
			.type = EXPR_DECL_INFO,
			.var_type = malloc(sizeof(TypeDef)),
			.attrs = decl_attrs,
			.block = dst->block,
			.loc = dst->loc
		})->var_type = var_type;

	}

	return false;
}

bool parseIntLiteral(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_INT)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}
	dst->int_literal = token.value;

	return false;
}

bool parseStrLiteral(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_STRING)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}
	dst->string = token.string;

	return false;
}

typedef bool (*KwParser) (AST*, Token, Expr*, Expr*, Expr*, int);

bool parseKwSys(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_SYSCALL)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	initExpr(dst);
	token = fetchToken(ast->preprocessor);
	if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(ast, token, "a syscall name", wordToken(NULL));
	bool name_vaildated = false;
	for (unsigned int i = 0; i < BRB_N_SYSCALLS; ++i) {
		if (sbufeq(BRB_syscallNames[i], fromstr(token.word))) {
			name_vaildated = true;
			dst->syscall_id = i;
			break;
		}
	}
	if (!name_vaildated) raiseUnknownSyscallNameError(ast, token);

	token = fetchToken(ast->preprocessor);
	if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));

	token = peekToken(ast->preprocessor);
	if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_END) {
		while (true) {
			Expr* arg = addSubexpr(dst, (Expr){0});
			parseExpr(ast, arg, dst, dst->block, proc, EXPRTERM_ARG | EXPRTYPE_EVALUATABLE);

			token = fetchToken(ast->preprocessor);
			if (token.symbol_id == SYMBOL_ARGSPEC_END) {
				break;
			} else if (token.symbol_id != SYMBOL_COMMA)
				raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_COMMA), symbolToken(SYMBOL_ARGSPEC_END));
		}
	} else fetchToken(ast->preprocessor);
	if (getSubexprsCount(dst) > 6) raiseTooManySyscallArgsError(ast, token.loc, getSubexprsCount(dst) - 1);

	return false;
}

bool parseKwBuiltin(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_BUILTIN)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	token = fetchToken(ast->preprocessor);
	if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(ast, token, "a built-in name", wordToken(NULL));
	bool name_vaildated = false;
	for (unsigned int i = 0; i < BRB_N_BUILTINS; ++i) {
		if (sbufeq(BRB_builtinNames[i], fromstr(token.word))) {
			name_vaildated = true;
			dst->builtin_id = i;
			break;
		}
	}
	if (!name_vaildated) raiseUnknownBuiltinNameError(ast, token);

	return false;
}

bool parseKwReturn(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_RETURN)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	dst->arg1 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg1, dst, dst->block, proc, EXPRTERM_FULL | EXPRTYPE_EVALUATABLE | EXPRTYPE_VOIDABLE);

	return false;
}

bool parseKwCast(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_CAST)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}
	
	token = fetchToken(ast->preprocessor);
	if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));

	dst->arg1 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg1, dst, dst->block, proc, EXPRTERM_ARG | EXPRTYPE_EVALUATABLE);
	token = fetchToken(ast->preprocessor);
	if (getTokenSymbolId(token) != SYMBOL_COMMA) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_COMMA));

	token = peekToken(ast->preprocessor);
	dst->var_type = malloc(sizeof(TypeDef));
	int stub;
	if (!parseType(ast, dst->var_type, &stub, 0, "a type specifier for a type cast", dst->block)) raiseInvalidTypeError(ast, token.loc);
	if (dst->var_type->kind == KIND_VOID || dst->var_type->kind == KIND_CUSTOM)
		raiseInvalidCastTargetTypeError(ast, token.loc, *dst->var_type);
	token = fetchToken(ast->preprocessor);
	if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_END)
		raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_END));

	return false;
}

bool parseSymbolNot(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_LOGICAL_NOT)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	dst->arg1 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg1, dst, dst->block, proc, (flags & EXPRTERM) | EXPRTYPE_EVALUATABLE);

	return false;
}

bool parseKwIf(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_IF)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	token = fetchToken(ast->preprocessor);
	if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));

	dst->arg1 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg1, dst, dst->block, proc, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE | EXPRTYPE_LOGICAL);
	fetchToken(ast->preprocessor);

	dst->arg2 = calloc(1, sizeof(Expr));
	token = peekToken(ast->preprocessor);
	parseExpr(ast, dst->arg2, dst, dst->block, proc, EXPRTERM_IF_BODY);
	if (dst->arg2->type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(ast, token.loc, "`if` statement");

	Token stmt_term = fetchToken(ast->preprocessor);
	if (getTokenKeywordId(stmt_term) != KW_ELSE) {
		token = peekToken(ast->preprocessor);
		if (getTokenKeywordId(token) != KW_ELSE) {
			unfetchToken(ast->preprocessor, stmt_term);
			return false;
		}
		fetchToken(ast->preprocessor);
	}
	dst->arg3 = calloc(1, sizeof(Expr));
	
	token = peekToken(ast->preprocessor);
	parseExpr(ast, dst->arg3, dst, dst->block, proc, EXPRTERM_FULL);
	if (dst->arg3->type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(ast, token.loc, "`else` statement");

	return false;
}

bool parseKwWhile(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (getTokenSymbolId(peekToken(ast->preprocessor)) == SYMBOL_ARGSPEC_START) {
		if (setExprType(ast, dst, parent_expr, EXPR_WHILE)) {
			unfetchToken(ast->preprocessor, token);
			return true;
		}
		fetchToken(ast->preprocessor);
		
		dst->arg1 = calloc(1, sizeof(Expr));
		parseExpr(ast, dst->arg1, dst, dst->block, proc, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE | EXPRTYPE_LOGICAL);
		fetchToken(ast->preprocessor);

		dst->arg2 = calloc(1, sizeof(Expr));
		token = peekToken(ast->preprocessor);
		parseExpr(ast, dst->arg2, dst, dst->block, proc, EXPRTERM_FULL);
		if (dst->arg2->type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(ast, token.loc, "`while` statement");
	} else raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));

	return false;
}

bool parseKwFor(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_BLOCK)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	token = fetchToken(ast->preprocessor);
	if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));
// fetching the initializer
	initExpr(dst);
	addSubexpr(dst, (Expr){ .type = EXPR_REF, .loc = token.loc, .block = dst });
	parseExpr(ast, addSubexpr(dst, (Expr){0}), dst, dst, proc, EXPRTERM_FULL | EXPRTYPE_VOIDABLE);
	fetchToken(ast->preprocessor);
// fetching the terminator
	token = peekToken(ast->preprocessor);
	if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_END) raiseForLoopTermExpectedError(ast, token);
	Expr* loop = addSubexpr(dst, (Expr){
		.type = EXPR_WHILE,
		.loc = token.loc,
		.block = dst,
		.arg1 = calloc(1, sizeof(Expr)),
		.arg2 = calloc(1, sizeof(Expr))
	});
	parseExpr(ast, loop->arg1, dst, dst, proc, EXPRTERM_FULL | EXPRTYPE_EVALUATABLE | EXPRTYPE_LOGICAL);
	fetchToken(ast->preprocessor);
// fetching the incrementer
	token = peekToken(ast->preprocessor);
	if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_END) raiseForLoopIncrExpectedError(ast, token);
	*loop->arg2 = (Expr){
		.type = EXPR_BLOCK,
		.loc = token.loc,
		.block = dst
	};
	initExpr(loop->arg2);
	addSubexpr(loop->arg2, (Expr){ .type = EXPR_REF, .loc = token.loc, .block = loop->arg2 });
	Expr* iter = calloc(1, sizeof(Expr));
	parseExpr(ast, iter, loop->arg2, loop->arg2, proc, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE);
	fetchToken(ast->preprocessor);
// fetching the body
	parseExpr(ast, addSubexpr(loop->arg2, (Expr){0}), loop->arg2, loop->arg2, proc, EXPRTERM_FULL);
	addSubexpr(loop->arg2, *iter);
	free(iter);

	return false;
}

bool parseKwDo(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_DOWHILE)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	dst->arg1 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg1, dst, dst->block, proc, EXPRTERM_DOWHILE_BODY);
	fetchToken(ast->preprocessor);

	token = fetchToken(ast->preprocessor);
	if (getTokenKeywordId(token) == KW_WHILE) token = fetchToken(ast->preprocessor);
	if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));
	
	dst->arg2 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg2, dst, dst->block, proc, EXPRTERM_BRACKET | EXPRTYPE_LOGICAL | EXPRTYPE_EVALUATABLE);
	fetchToken(ast->preprocessor);

	return false;
}

bool parseKwType(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_NEW_TYPE)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	token = fetchToken(ast->preprocessor);
	if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(ast, token, "the type name", wordToken(NULL));
	initExpr(dst);
	addSubexpr(dst, (Expr){
		.type = EXPR_NAME,
		.loc = token.loc,
		.block = dst->block,
		.name = token.word
	});
	
	addSubexpr(dst, (Expr){
		.type = EXPR_REF,
		.block = dst->block,
		.loc = dst->loc,
		.arg1 = getSubexpr(dst->block, 0)->arg1
	});
	getSubexpr(dst->block, 0)->arg1 = dst;

	token = peekToken(ast->preprocessor);
	if (getTokenSymbolId(token) == SYMBOL_SEMICOLON) {
		return false;
	} else if (getTokenSymbolId(token) != SYMBOL_BLOCK_START) {
		raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_BLOCK_START));
	}

	Expr* def = addSubexpr(dst, (Expr){0});
	parseExpr(ast, def, dst, dst->block, proc, EXPRTERM_FULL | EXPRTYPE_DECL | EXPRTYPE_TYPE_ATTR);
	assert(def->type == EXPR_BLOCK, "definition of a type must be a block");
	getSubexpr(def, 0)->name = getDeclName(dst);

	return false;
}

bool parseInvalidKw(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* proc, int flags)
{
	raiseInvalidExprError(ast, token.loc);
	return false;
}

typedef bool (*SymbolParser) (AST*, Token, Expr*, Expr*, Expr*, int);

bool parseSymbolAssignment(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	static ExprType symbol_to_expr[N_SYMBOLS] = {
		[SYMBOL_ASSIGNMENT] = EXPR_ASSIGN,
		[SYMBOL_ADD_ASSIGN] = EXPR_ADD_ASSIGN,
		[SYMBOL_SUB_ASSIGN] = EXPR_SUB_ASSIGN,
		[SYMBOL_MUL_ASSIGN] = EXPR_MUL_ASSIGN,
		[SYMBOL_DIV_ASSIGN] = EXPR_DIV_ASSIGN,
		[SYMBOL_AND_ASSIGN] = EXPR_AND_ASSIGN,
		[SYMBOL_XOR_ASSIGN] = EXPR_XOR_ASSIGN,
		[SYMBOL_OR_ASSIGN ] = EXPR_OR_ASSIGN,
		[SYMBOL_SHL_ASSIGN] = EXPR_SHL_ASSIGN,
		[SYMBOL_SHR_ASSIGN] = EXPR_SHR_ASSIGN
	};

	if (dst->type == EXPR_NEW_VAR) {
		if (token.symbol_id != SYMBOL_ASSIGNMENT) raiseInvalidComplexAssignmentError(ast, token.loc);
		parseExpr(ast, addSubexpr(dst, (Expr){0}), dst, dst->block, func, EXPRTERM_FULL | EXPRTYPE_EVALUATABLE);
	} else {
		dst->loc = token.loc;
		if (setExprType(ast, dst, parent_expr, symbol_to_expr[token.symbol_id])) {
			unfetchToken(ast->preprocessor, token);
			return true;
		}
		if (flags & EXPRTYPE_EVALUATABLE && !(flags & (EXPRTERM_BRACKET | EXPRTERM_ARG))) raiseUnbracketedAssignExprError(ast, token.loc);

		dst->arg2 = calloc(1, sizeof(Expr));
		parseExpr(ast, dst->arg2, dst, dst->block, func, (flags & EXPRTERM) | EXPRTYPE_EVALUATABLE);
	}

	return false;
}

bool parseSymbolBlockStart(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	dst->loc = token.loc;

	if (flags & EXPRTYPE_EVALUATABLE) {
		if (setExprType(ast, dst, parent_expr, EXPR_ARRAY)) {
			unfetchToken(ast->preprocessor, token);
			return true;
		}

		if (parent_expr->type == EXPR_NEW_VAR) {
			TypeDef decl_type = *getSubexpr(parent_expr, 2)->var_type;
			if (decl_type.kind != KIND_ARRAY) raiseUnexpectedArrayInitError(ast, token.loc, decl_type);
			dst->element_type = malloc(sizeof(TypeDef));
			*dst->element_type = *decl_type.base;
		}

		initExpr(dst);
		if (getTokenSymbolId(peekToken(ast->preprocessor)) != SYMBOL_BLOCK_END) {
			while (true) {
				parseExpr(ast, addSubexpr(dst, (Expr){0}), dst, dst->block, func, EXPRTERM_ARRAY_ARG | EXPRTYPE_EVALUATABLE);
				if (getTokenSymbolId(fetchToken(ast->preprocessor)) == SYMBOL_BLOCK_END) break;
			}
		} else fetchToken(ast->preprocessor);
	} else {
		if (setExprType(ast, dst, parent_expr, EXPR_BLOCK)) {
			unfetchToken(ast->preprocessor, token);
			return true;
		}

		initExpr(dst);
		addSubexpr(dst, (Expr){
			.type = EXPR_REF,
			.block = dst,
			.loc = dst->loc,
			.arg1 = NULL,
		});

		while (true) {
			token = peekToken(ast->preprocessor);
			if (token.type == TOKEN_NONE) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_BLOCK_END));
			if (getTokenSymbolId(token) == SYMBOL_BLOCK_END) break;

			Expr* expr = addSubexpr(dst, (Expr){0});
			parseExpr(ast, expr, dst, dst, func, (flags & EXPRTYPE) | EXPRTERM_FULL);
			fetchToken(ast->preprocessor);
		}
	}

	return false;
}

bool parseBinaryExprSymbol(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_ADD + token.symbol_id - SYMBOL_PLUS)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	dst->arg2 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg2, dst, dst->block, func, (flags & EXPRTERM) | EXPRTYPE_EVALUATABLE);

	return false;
}

bool parseSymbolTilde(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_NOT)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	dst->arg1 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg1, dst, dst->block, func, (flags & EXPRTERM) | EXPRTYPE_EVALUATABLE);

	return false;
}

bool parseSymbolArgspecStart(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	dst->loc = token.loc;
	if (dst->type != EXPR_INVALID) {
		if (setExprType(ast, dst, parent_expr, EXPR_PROC_CALL)) {
			unfetchToken(ast->preprocessor, token);
			return true;
		}

		if (getTokenSymbolId(peekToken(ast->preprocessor)) != SYMBOL_ARGSPEC_END) {
			while (true) {
				parseExpr(ast, addSubexpr(dst, (Expr){0}), dst, dst->block, func, EXPRTERM_ARG | EXPRTYPE_EVALUATABLE);
				if (getTokenSymbolId(fetchToken(ast->preprocessor)) == SYMBOL_ARGSPEC_END) break;
			}
		} else fetchToken(ast->preprocessor);

	} else {
		dst->type = EXPR_WRAPPER;
		dst->arg1 = calloc(1, sizeof(Expr));
		parseExpr(ast, dst->arg1, dst, dst->block, func, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE);
		fetchToken(ast->preprocessor);
	}

	return false;
}

bool parseComparisonOperatorSymbol(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	static ExprType symbol_to_expr[N_SYMBOLS] = {
		[SYMBOL_EQ] = EXPR_LOGICAL_EQ,
		[SYMBOL_NEQ] = EXPR_LOGICAL_NEQ,
		[SYMBOL_LT] = EXPR_LOGICAL_LT,
		[SYMBOL_GT] = EXPR_LOGICAL_GT,
		[SYMBOL_LE] = EXPR_LOGICAL_LE,
		[SYMBOL_GE] = EXPR_LOGICAL_GE,
		[SYMBOL_OR] = EXPR_LOGICAL_OR,
		[SYMBOL_AND] = EXPR_LOGICAL_AND
	};

	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, symbol_to_expr[token.symbol_id])) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	dst->arg2 = calloc(1, sizeof(Expr));
	token = peekToken(ast->preprocessor);
	parseExpr(ast, dst->arg2, dst, dst->block, func, (flags & EXPRTERM) | EXPRTYPE_EVALUATABLE);

	return false;
}

bool parseSymbolAmpersand(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	if (dst->type == EXPR_INVALID) {
		dst->loc = token.loc;
		if (setExprType(ast, dst, parent_expr, EXPR_GET_REF)) {
			unfetchToken(ast->preprocessor, token);
			return true;
		}

		dst->arg1 = calloc(1, sizeof(Expr));
		parseExpr(ast, dst->arg1, dst, dst->block, func, (flags & EXPRTERM) | EXPRTYPE_EVALUATABLE);
	} else return parseBinaryExprSymbol(ast, token, dst, parent_expr, func, flags);

	return false;
}

bool parseSymbolStar(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	if (dst->type == EXPR_INVALID) {
		dst->loc = token.loc;
		if (setExprType(ast, dst, parent_expr, EXPR_DEREF)) {
			unfetchToken(ast->preprocessor, token);
			return true;
		}

		dst->arg1 = calloc(1, sizeof(Expr));
		parseExpr(ast, dst->arg1, dst, dst->block, func, (flags & EXPRTERM) | EXPRTYPE_EVALUATABLE);
	} else return parseBinaryExprSymbol(ast, token, dst, parent_expr, func, flags);

	return false;
}

bool parseSymbolIndexStart(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_GET_ITEM)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	dst->arg2 = calloc(1, sizeof(Expr));
	parseExpr(ast, dst->arg2, dst, dst->block, func, EXPRTERM_SQBRACKET | EXPRTYPE_EVALUATABLE);
	fetchToken(ast->preprocessor);
	return false;
}

bool parseSymbolDot(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	dst->loc = token.loc;
	if (setExprType(ast, dst, parent_expr, EXPR_GET_ATTR)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}

	token = fetchToken(ast->preprocessor);
	if (token.type != TOKEN_WORD)
		raiseUnexpectedTokenError(ast, token, "an attribute name", wordToken(NULL));

	dst->name = token.word;
	return false;
}

bool parseInvalidSymbol(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	raiseInvalidExprError(ast, token.loc);
	return false;
}

bool parseWord(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	dst->loc = token.loc;
	Expr* decl = getDecl(dst->block, token.word, NULL);
	if (!decl) raiseUnknownNameError(ast, token);
	if (setExprType(ast, dst, parent_expr, EXPR_GET)) {
		unfetchToken(ast->preprocessor, token);
		return true;
	}
	dst->arg1 = decl;

	return false;
}

bool parseEmptyToken(AST* ast, Token token, Expr* dst, Expr* parent_expr, Expr* func, int flags)
{
	raiseInvalidExprError(ast, token.loc);
	return false;
}

bool isExprTerm(AST* ast, Token token, Expr* expr, int flags)
{
	static_assert(N_EXPR_TYPES == 56, "not all expression types are handled in isExprTerm");
	int64_t symbol_id = getTokenSymbolId(token);
	switch (flags & EXPRTERM) {
		case EXPRTERM_FULL:
			return isCBracketEnclosing(expr)
				? symbol_id == SYMBOL_BLOCK_END
				: symbol_id == SYMBOL_SEMICOLON;
		case EXPRTERM_DOWHILE_BODY:
			return isCBracketEnclosing(expr)
				? symbol_id == SYMBOL_BLOCK_END
				: symbol_id == SYMBOL_SEMICOLON
					|| getTokenKeywordId(token) == KW_WHILE;
		case EXPRTERM_IF_BODY:
			return isCBracketEnclosing(expr)
				? symbol_id == SYMBOL_BLOCK_END
				: symbol_id == SYMBOL_SEMICOLON
					|| getTokenKeywordId(token) == KW_ELSE;
		case EXPRTERM_ARG:
			if (symbol_id == SYMBOL_SEMICOLON)
				raiseUnexpectedTokenError(
					ast,
					token,
					NULL,
					symbolToken(SYMBOL_COMMA),
					symbolToken(SYMBOL_ARGSPEC_END)
				);
			return symbol_id == SYMBOL_COMMA || symbol_id == SYMBOL_ARGSPEC_END;
		case EXPRTERM_ARRAY_ARG:
			if (symbol_id == SYMBOL_SEMICOLON)
				raiseUnexpectedTokenError(
					ast,
					token,
					NULL,
					symbolToken(SYMBOL_COMMA),
					symbolToken(SYMBOL_BLOCK_END)
				);
			return symbol_id == SYMBOL_COMMA || symbol_id == SYMBOL_BLOCK_END;
		case EXPRTERM_BRACKET:
			return symbol_id == SYMBOL_ARGSPEC_END;
		case EXPRTERM_SQBRACKET:
			return symbol_id == SYMBOL_INDEX_END;
		default:
			assert(false, "invalid expression terminator flag: %d", flags & EXPRTERM);
	}
}

void parseExpr(AST* ast, Expr* dst, Expr* parent_expr, Expr* block, Expr* func, int flags)
{
	static_assert(N_EXPR_TYPES == 56, "not all expression types are handled in parseExpr");
	TypeDef new_type;
	int decl_attrs;
	Token token;

	dst->block = block;
	while (true) {
		token = peekToken(ast->preprocessor);
		if (isExprTerm(ast, token, dst, flags)) break;

		if (parseType(ast, &new_type, &decl_attrs, ATTR_STATIC, "a variable or procedure declaration", block)) {
			parseDecl(ast, token, new_type, decl_attrs, dst, parent_expr, func, flags);
			continue;
		}
		
		fetchToken(ast->preprocessor);
		if (token.type == TOKEN_INT) {
			if (parseIntLiteral(ast, token, dst, parent_expr, func, flags)) break;
		} else if (token.type == TOKEN_STRING) {
			if (parseStrLiteral(ast, token, dst, parent_expr, func, flags)) break;
		} else if (token.type == TOKEN_KEYWORD) {
			static KwParser kw_parsers[] = {
				[KW_VOID] = parseInvalidKw,
				[KW_INT8] = parseInvalidKw,
				[KW_INT16] = parseInvalidKw,
				[KW_INT32] = parseInvalidKw,
				[KW_INT64] = parseInvalidKw,
				[KW_SYS] = parseKwSys,
				[KW_BUILTIN] = parseKwBuiltin,
				[KW_RETURN] = parseKwReturn,
				[KW_CAST] = parseKwCast,
				[KW_BOOL] = parseInvalidKw,
				[KW_IF] = parseKwIf,
				[KW_ELSE] = parseInvalidKw,
				[KW_WHILE] = parseKwWhile,
				[KW_FOR] = parseKwFor,
				[KW_DO] = parseKwDo,
				[KW_STATIC] = parseInvalidSymbol,
				[KW_TYPE] = parseKwType
			};
			if (kw_parsers[token.keyword_id](ast, token, dst, parent_expr, func, flags)) break;
		} else if (token.type == TOKEN_SYMBOL) {
			static SymbolParser symbol_parsers[] = {
				[SYMBOL_ARGSPEC_START] = parseSymbolArgspecStart,
				[SYMBOL_ARGSPEC_END] = parseInvalidSymbol,
				[SYMBOL_COMMA] = parseInvalidSymbol,
				[SYMBOL_BLOCK_START] = parseSymbolBlockStart,
				[SYMBOL_BLOCK_END] = parseInvalidSymbol,
				[SYMBOL_SEMICOLON] = parseInvalidSymbol,
				[SYMBOL_EQ] = parseComparisonOperatorSymbol,
				[SYMBOL_NEQ] = parseComparisonOperatorSymbol,
				[SYMBOL_LE] = parseComparisonOperatorSymbol,
				[SYMBOL_GE] = parseComparisonOperatorSymbol,
				[SYMBOL_NOT] = parseSymbolNot,
				[SYMBOL_OR] = parseComparisonOperatorSymbol,
				[SYMBOL_AND] = parseComparisonOperatorSymbol,
				[SYMBOL_ASSIGNMENT] = parseSymbolAssignment,
				[SYMBOL_PLUS] = parseBinaryExprSymbol,
				[SYMBOL_MINUS] = parseBinaryExprSymbol,
				[SYMBOL_STAR] = parseSymbolStar,
				[SYMBOL_DIV] = parseBinaryExprSymbol,
				[SYMBOL_MOD] = parseBinaryExprSymbol,
				[SYMBOL_AMPERSAND] = parseSymbolAmpersand,
				[SYMBOL_PIPE] = parseBinaryExprSymbol,
				[SYMBOL_CARET] = parseBinaryExprSymbol,
				[SYMBOL_LSHIFT] = parseBinaryExprSymbol,
				[SYMBOL_RSHIFT] = parseBinaryExprSymbol,
				[SYMBOL_TILDE] = parseSymbolTilde,
				[SYMBOL_LT] = parseComparisonOperatorSymbol,
				[SYMBOL_GT] = parseComparisonOperatorSymbol,
				[SYMBOL_ADD_ASSIGN] = parseSymbolAssignment,
				[SYMBOL_SUB_ASSIGN] = parseSymbolAssignment,
				[SYMBOL_MUL_ASSIGN] = parseSymbolAssignment,
				[SYMBOL_DIV_ASSIGN] = parseSymbolAssignment,
				[SYMBOL_AND_ASSIGN] = parseSymbolAssignment,
				[SYMBOL_XOR_ASSIGN] = parseSymbolAssignment,
				[SYMBOL_OR_ASSIGN ] = parseSymbolAssignment,
				[SYMBOL_SHL_ASSIGN] = parseSymbolAssignment,
				[SYMBOL_SHR_ASSIGN] = parseSymbolAssignment,
				[SYMBOL_INDEX_START] = parseSymbolIndexStart,
				[SYMBOL_INDEX_END] = parseInvalidSymbol,
				[SYMBOL_DOT] = parseSymbolDot
			};
			if (symbol_parsers[token.symbol_id](ast, token, dst, parent_expr, func, flags)) break;
		} else if (token.type == TOKEN_WORD) {
			if (getTokenSymbolId(peekToken(ast->preprocessor)) == SYMBOL_ARGSPEC_START && (flags & EXPRTYPE_DECL)) {
				unfetchToken(ast->preprocessor, token);
				if (parseDecl(
					ast,
					(Token){.type = TOKEN_KEYWORD, .loc = token.loc, .keyword_id = KW_VOID},
					VOID_TYPE,
					0,
					dst,
					parent_expr,
					func,
					flags
				)) break;
			} else {
				if (parseWord(ast, token, dst, parent_expr, func, flags)) break;
			}
		} else if (token.type == TOKEN_NONE) {
			if (parseEmptyToken(ast, token, dst, parent_expr, func, flags)) break;
		} else
			assert(false, "invalid token type %hhd", token.type);
	}

	if (dst->type == EXPR_NAME) (void)setExprType(ast, dst, parent_expr, EXPR_GET);
	if (dst->type == EXPR_INVALID && flags & EXPRTYPE_VOIDABLE) dst->type = EXPR_VOID;
	if (!isExprEvaluatable(dst) && flags & EXPRTYPE_EVALUATABLE) raiseNoValueExprError(ast, token.loc);
}


typedef void (*ExprOptimizer) (AST*, Expr*, Expr*);

static ExprOptimizer expr_optimizers[N_EXPR_TYPES];

static inline void optimizeExpr(AST* ast, Expr* expr, Expr* proc)
{
	expr_optimizers[expr->type](ast, expr, proc);
}

void optimizeExprSyscall(AST* ast, Expr* expr, Expr* proc)
{
	chainForeach (Expr, iter, *getSubexprs(expr)) {
		optimizeExpr(ast, iter, proc);
		if (!matchType(ast, BUILTIN_VAL_TYPE, iter))
			raiseArgTypeMismatchError(ast, iter->loc, NULL, NULL, getExprValueType(ast, *iter));
	}
}

void emptyOptimizer(AST* ast, Expr* expr, Expr* proc) {}

void optimizeExprAssign(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (!matchType(ast, getExprValueType(ast, *expr->arg1), expr->arg2))
		raiseAssignmentTypeMismatchError(ast, expr->loc, expr->arg1->arg1, getExprValueType(ast, *expr->arg2));
}

void optimizeComplexAssignExpr(AST* ast, Expr* expr, Expr* proc)
{
	static ExprType assigners_to_ops[N_EXPR_TYPES] = {
		[EXPR_ADD_ASSIGN] = EXPR_ADD,
		[EXPR_SUB_ASSIGN] = EXPR_SUB,
		[EXPR_MUL_ASSIGN] = EXPR_MUL,
		[EXPR_DIV_ASSIGN] = EXPR_DIV,
		[EXPR_AND_ASSIGN] = EXPR_AND,
		[EXPR_XOR_ASSIGN] = EXPR_XOR,
		[EXPR_OR_ASSIGN ] = EXPR_OR,
		[EXPR_SHL_ASSIGN] = EXPR_SHL,
		[EXPR_SHR_ASSIGN] = EXPR_SHR
	};
	wrapExpr(expr->arg2, assigners_to_ops[expr->type], *expr->arg1);
	swap(expr->arg2->arg1, expr->arg2->arg2, Expr*);
	expr->type = EXPR_ASSIGN;
	optimizeExprAssign(ast, expr, proc);
}

void optimizeExprBlock(AST* ast, Expr* expr, Expr* proc)
{
	chainForeach (Expr, subexpr, ExprChain_slice(*getSubexprs(expr), 1, -1)) {
		optimizeExpr(ast, subexpr, proc);
	}
}

void optimizeExprReturn(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	if (!matchType(ast, *getSubexpr(proc, 2)->var_type, expr->arg1))
		raiseReturnTypeMismatchError(ast, expr->loc, getExprValueType(ast, *expr->arg1), proc);
}

void optimizeExprProcCall(AST* ast, Expr* expr, Expr* proc)
{
	ExprNode* arg_iter = getSubexprs(expr)->start;
	Expr* decl = arg_iter->value.arg1;
	arg_iter = arg_iter->next; 

	if (getSubexpr(decl, 2)->attrs & ATTR_NOT_DEFINED) raiseNoProcDefError(ast, *decl);
	ExprNode* decl_iter = getSubexprs(getSubexpr(decl, 3))->start->next;

	int decl_n_args = getSubexpr(decl, 2)->n_args, n_args = getSubexprsCount(expr) - 1;
	if (decl_n_args != n_args) raiseArgCountMismatchError(ast, expr->loc, decl, n_args);

	while (decl_n_args) {
		optimizeExpr(ast, &arg_iter->value, proc);
		if (!matchType(ast, *getSubexpr(&decl_iter->value, 2)->var_type, &arg_iter->value))
			raiseArgTypeMismatchError(
				ast,
				arg_iter->value.loc,
				decl,
				&decl_iter->value,
				getExprValueType(ast, arg_iter->value)
			);
		decl_iter = decl_iter->next;
		arg_iter = arg_iter->next;
		decl_n_args -= 1;
	}
}

void optimizeExprAdd(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
		int64_t sum = expr->arg1->int_literal + expr->arg2->int_literal;
		free(expr->arg1);
		free(expr->arg2);
		expr->type = EXPR_INT;
		expr->int_literal = sum;
	} else if (expr->arg1->type == EXPR_INT && expr->arg1->int_literal == 0) {
		free(expr->arg1);
		Expr temp = *expr->arg2;
		*expr = temp;
	} else if (expr->arg2->type == EXPR_INT && expr->arg2->int_literal == 0) {
		free(expr->arg2);
		Expr temp = *expr->arg1;
		*expr = temp;
	} else {
		TypeDef arg1_type = getExprValueType(ast, *expr->arg1);
		TypeDef arg2_type = getExprValueType(ast, *expr->arg2);

		if (isPtrType(arg1_type) && !isPtrType(arg2_type) ? getTypeSize(*arg1_type.base) > 1 : false) {
			if (expr->arg2->type == EXPR_INT) {
				expr->arg2->int_literal *= getTypeSize(*arg1_type.base);
			} else {
				wrapExpr(expr->arg2, EXPR_MUL, (Expr){
					.type = EXPR_INT,
					.block = expr->arg2->block,
					.loc = expr->arg2->loc,
					.int_literal = getTypeSize(*arg1_type.base)
				});
			}
		} else if (isPtrType(arg2_type) && !isPtrType(arg1_type) ? getTypeSize(*arg2_type.base) > 1 : false) {
			if (expr->arg1->type == EXPR_INT) {
				expr->arg1->int_literal *= getTypeSize(*arg2_type.base);
			} else {
				wrapExpr(expr->arg1, EXPR_MUL, (Expr){
					.type = EXPR_INT,
					.block = expr->arg1->block,
					.loc = expr->arg1->loc,
					.int_literal = getTypeSize(*arg1_type.base)
				});
			}
		}
	}
}

void optimizeExprSub(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
		int64_t diff = expr->arg1->int_literal - expr->arg2->int_literal;
		free(expr->arg1);
		free(expr->arg2);
		expr->type = EXPR_INT;
		expr->int_literal = diff;
	} else if (expr->arg1->type == EXPR_INT && expr->arg1->int_literal == 0) {
		free(expr->arg1);
		Expr temp = *expr->arg2;
		*expr = temp;
	} else if (expr->arg2->type == EXPR_INT && expr->arg2->int_literal == 0) {
		free(expr->arg2);
		Expr temp = *expr->arg1;
		*expr = temp;
	} else {
		TypeDef arg1_type = getExprValueType(ast, *expr->arg1);
		TypeDef arg2_type = getExprValueType(ast, *expr->arg2);

		if (isPtrType(arg1_type) && isPtrType(arg2_type)) {
			if (typeMatches(*arg1_type.base, *arg2_type.base) && getTypeSize(*arg1_type.base) > 1) {
				wrapExpr(expr, EXPR_DIV, (Expr){
					.type = EXPR_INT,
					.block = expr->block,
					.loc = expr->loc,
					.int_literal = getTypeSize(*arg1_type.base)
				});
			}
		}
	}
}

void optimizeExprMul(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
		int64_t product = expr->arg1->int_literal * expr->arg2->int_literal;
		free(expr->arg1);
		free(expr->arg2);
		expr->type = EXPR_INT;
		expr->int_literal = product;
	} else if (expr->arg1->type == EXPR_INT && expr->arg1->int_literal == 1) {
		free(expr->arg1);
		Expr temp = *expr->arg2;
		*expr = temp;
	} else if (expr->arg2->type == EXPR_INT && expr->arg2->int_literal == 1) {
		free(expr->arg2);
		Expr temp = *expr->arg1;
		*expr = temp;
	}
}

void optimizeExprDiv(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
		if (expr->arg2->int_literal == 0) raiseDivByZeroError(ast, expr->loc);
		int64_t quotient = expr->arg1->int_literal / expr->arg2->int_literal;
		free(expr->arg1);
		free(expr->arg2);
		expr->type = EXPR_INT;
		expr->int_literal = quotient;
	} else if (expr->arg2->type == EXPR_INT) {
		if (expr->arg2->int_literal == 0) raiseDivByZeroError(ast, expr->loc);
		else if (expr->arg2->int_literal == 1) {
			free(expr->arg2);
			Expr temp = *expr->arg1;
			*expr = temp;
		}
	}
}

void optimizeExprMod(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg2->type == EXPR_INT) {
		if (expr->arg2->int_literal == 0) raiseModZeroError(ast, expr->loc);
		if (expr->arg1->type == EXPR_INT) {
			int64_t remainder = expr->arg1->int_literal % expr->arg2->int_literal;
			free(expr->arg1);
			free(expr->arg2);
			expr->type = EXPR_INT;
			expr->int_literal = remainder;
		}
	}
}

void optimizeExprAnd(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
		int64_t res = expr->arg1->int_literal & expr->arg2->int_literal;
		free(expr->arg1);
		free(expr->arg2);
		expr->type = EXPR_INT;
		expr->int_literal = res;
	}
}

void optimizeExprOr(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
		int64_t res = expr->arg1->int_literal | expr->arg2->int_literal;
		free(expr->arg1);
		free(expr->arg2);
		expr->type = EXPR_INT;
		expr->int_literal = res;
	} else if (expr->arg1->type == EXPR_INT && expr->arg1->int_literal == 0) {
		free(expr->arg1);
		Expr temp = *expr->arg2;
		*expr = temp;
	} else if (expr->arg2->type == EXPR_INT && expr->arg2->int_literal == 0) {
		if (expr->arg2->int_literal == 0) raiseDivByZeroError(ast, expr->loc);
		free(expr->arg2);
		Expr temp = *expr->arg1;
		*expr = temp;
	}
}

void optimizeExprXor(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
		int64_t res = expr->arg1->int_literal ^ expr->arg2->int_literal;
		free(expr->arg1);
		free(expr->arg2);
		expr->type = EXPR_INT;
		expr->int_literal = res;
	}
}

void optimizeExprShl(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg2->type == EXPR_INT) {
		if (expr->arg2->int_literal < 0) raiseNegativeBitShiftError(ast, expr->loc);
		if (expr->arg1->type == EXPR_INT) {
			int64_t res = expr->arg1->int_literal << expr->arg2->int_literal;
			free(expr->arg1);
			free(expr->arg2);
			expr->type = EXPR_INT;
			expr->int_literal = res;
		}
	} else if (expr->arg2->type == EXPR_INT && expr->arg2->int_literal == 0) {
		free(expr->arg2);
		Expr temp = *expr->arg1;
		*expr = temp;
	}
}

void optimizeExprShr(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg2->type == EXPR_INT) {
		if (expr->arg2->int_literal < 0) raiseNegativeBitShiftError(ast, expr->loc);
		if (expr->arg1->type == EXPR_INT) {
			int64_t res = expr->arg1->int_literal >> expr->arg2->int_literal;
			free(expr->arg1);
			free(expr->arg2);
			expr->type = EXPR_INT;
			expr->int_literal = res;
		}
	} else if (expr->arg2->type == EXPR_INT && expr->arg2->int_literal == 0) {
		free(expr->arg2);
		Expr temp = *expr->arg1;
		*expr = temp;
	}
}

void optimizeExprNot(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	if (expr->arg1->type == EXPR_INT) {
		int64_t int_literal = ~expr->arg1->int_literal;
		free(expr->arg1);
		expr->int_literal = int_literal;
		expr->type = EXPR_INT;
	}
}

void optimizeUnaryExpr(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
}

void optimizeExprDeref(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	if (!isPtrType(getExprValueType(ast, *expr->arg1))) raiseUndereferrableExprError(ast, expr->loc, getExprValueType(ast, *expr->arg1));
}

void optimizeExprGetRef(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	if (expr->arg1->type == EXPR_DEREF) {
		Expr* new_expr = expr->arg1->arg1;
		free(expr->arg1);
		expr->arg1 = new_expr;
	} else if (expr->arg1->type != EXPR_GET) raiseUnreferrableExprError(ast, expr->loc, getExprValueType(ast, *expr->arg1));
}

void optimizeComparisonExpr(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (!matchType(ast, getExprValueType(ast, *expr->arg1), expr->arg2))
		raiseComparisonTypeMismatchError(ast, expr->loc, getExprValueType(ast, *expr->arg2), getExprValueType(ast, *expr->arg1));
}

void optimizeBinaryExpr(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
}

void optimizeControlFlowExpr(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);
	if (expr->arg3) optimizeExpr(ast, expr->arg3, proc);

	makeLogicalExpr(expr->type == EXPR_DOWHILE ? expr->arg2 : expr->arg1);
}

void optimizeExprNewVar(AST* ast, Expr* expr, Expr* proc)
{
	if (getSubexprsCount(expr) == 4) {
		Expr* initializer = getSubexpr(expr, 3);
		Expr* decl_info = getSubexpr(expr, 2);
		optimizeExpr(ast, initializer, proc);
		if (!matchType(ast, *decl_info->var_type, initializer))
			raiseAssignmentTypeMismatchError(ast, expr->loc, expr, getExprValueType(ast, *initializer));
	}
}

void optimizeExprGetItem(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	optimizeExpr(ast, expr->arg2, proc);

	TypeDef array_type = getExprValueType(ast, *expr->arg1);
	if (!isPtrType(array_type)) raiseUnindexableExprError(ast, expr->arg1->loc, array_type);

	TypeDef index_type = getExprValueType(ast, *expr->arg2);
	if (!isIntType(index_type)) raiseInvalidArrayIndexError(ast, expr->arg2->loc, index_type);

	if (isExprIntLiteral(expr->arg2) && array_type.kind == KIND_ARRAY && array_type.n_items > 0) {
		uint64_t index_literal = getIntLiteral(expr->arg2);
		if (!inRange(index_literal, 0, array_type.n_items)) raiseOutOfBoundsIndexError(ast, expr->arg2->loc, array_type, index_literal);
	}

	if (expr->arg1->type == EXPR_ARRAY && isExprIntLiteral(expr->arg2)) {
		Expr temp = *getSubexpr(expr->arg1, getIntLiteral(expr->arg2));
		ExprChain_clear(getSubexprs(expr->arg1));
		free(expr->arg2);
		free(expr->arg1);
		*expr = temp;
	} else {
		Expr* new_subexpr = malloc(sizeof(Expr));
		*new_subexpr = (Expr){
			.type = EXPR_ADD,
			.block = expr->block,
			.loc = expr->loc,
			.arg1 = expr->arg1,
			.arg2 = expr->arg2
		};
		expr->type = EXPR_DEREF;
		expr->arg1 = new_subexpr;
		optimizeExpr(ast, expr->arg1, proc);
	}
}

void optimizeExprArray(AST* ast, Expr* expr, Expr* proc)
{
	int index = 0;
	chainForeach(Expr, item, *getSubexprs(expr)) {
		optimizeExpr(ast, item, proc);
		if (!matchType(ast, *expr->element_type, item))
			raiseElementTypeMismatchError(ast, item->loc, index, *expr->element_type, getExprValueType(ast, *item));
		index++;
	}
}

void optimizeExprNewProc(AST* ast, Expr* expr, Expr* proc)
{
	Expr* def = getProcDef(expr);
	if (def) optimizeExpr(ast, def, expr);
}

void optimizeExprNewType(AST* ast, Expr* expr, Expr* proc)
{
	if (getSubexprsCount(expr) == 3) {
		chainForeach (Expr, field, ExprChain_slice(*getSubexprs(getSubexpr(expr, 2)), 1, -1)) {
			if (field->type == EXPR_NEW_VAR) {
				if (getSubexprsCount(field) == 4)
					raiseClassAttrInitError(ast, *field, *expr);
			} else if (field->type != EXPR_NEW_PROC)
				raiseInvalidClassFieldKindError(ast, *field, *expr);
			optimizeExpr(ast, field, proc);
		}
	} else raiseNoTypeDefError(ast, *expr);
}

void optimizeExprGetAttr(AST* ast, Expr* expr, Expr* proc)
{
	optimizeExpr(ast, expr->arg1, proc);
	TypeDef obj_type = getExprValueType(ast, *expr->arg1);
	TypeDef base_obj_type = getBaseType(obj_type);

	if (base_obj_type.kind != KIND_CUSTOM) raiseAttrOfPrimitiveError(ast, expr->loc, obj_type);
	Expr* block = getSubexpr(base_obj_type.decl, 2);

	if (!block) raiseNoTypeDefError(ast, *obj_type.decl);
	Expr* attr = getDecl(block, expr->name, block->block);

	if (!attr) raiseAttrNotFoundError(ast, expr->loc, obj_type, expr->name);
	expr->arg2 = attr;
// handling case if the attribute is a static variable or a procedure
	if (attr->type == EXPR_NEW_PROC ? true : getSubexpr(attr, 2)->attrs & ATTR_STATIC) {
		expr->type = EXPR_GET;
		expr->arg1 = attr;
		return;
	} 

	repeat (getIndirLevel(obj_type))
		wrapExpr(expr->arg1, EXPR_DEREF);

	if (expr->arg1->type == EXPR_GET)
		wrapExpr(wrapExpr(expr->arg1, EXPR_GET_REF), EXPR_DEREF);

	if (expr->arg1->type == EXPR_DEREF) {
		*(wrapExpr(wrapExpr(expr->arg1->arg1, EXPR_ADD, (Expr){
			.type = EXPR_INT,
			.int_literal = getAttrOffset(expr->arg2),
			.block = expr->block,
			.loc = expr->loc
		}), EXPR_CAST)->var_type = malloc(sizeof(Expr))) = PTR_TYPE(*getSubexpr(expr->arg2, 2)->var_type);
		Expr lvalue = *expr->arg1;
		free(expr->arg1);
		*expr = lvalue;
	} else assert(false, "codegen for getting attributes of r-values is not implmented yet");
}

static_assert(N_EXPR_TYPES == 56, "not all expressions have any optimizations defined");
static ExprOptimizer expr_optimizers[N_EXPR_TYPES] = {
	[EXPR_SYSCALL] = optimizeExprSyscall,
	[EXPR_BUILTIN] = emptyOptimizer, 
	[EXPR_STRING] = emptyOptimizer,
	[EXPR_INT] = emptyOptimizer, 
	[EXPR_NEW_VAR] = optimizeExprNewVar,
	[EXPR_ASSIGN] = optimizeExprAssign, 
	[EXPR_ADD_ASSIGN] = optimizeComplexAssignExpr, 
	[EXPR_SUB_ASSIGN] = optimizeComplexAssignExpr, 
	[EXPR_MUL_ASSIGN] = optimizeComplexAssignExpr, 
	[EXPR_DIV_ASSIGN] = optimizeComplexAssignExpr, 
	[EXPR_AND_ASSIGN] = optimizeComplexAssignExpr, 
	[EXPR_XOR_ASSIGN] = optimizeComplexAssignExpr, 
	[EXPR_OR_ASSIGN] = optimizeComplexAssignExpr,
	[EXPR_SHL_ASSIGN] = optimizeComplexAssignExpr,
	[EXPR_SHR_ASSIGN] = optimizeComplexAssignExpr, 
	[EXPR_GET] = emptyOptimizer, 
	[EXPR_BLOCK] = optimizeExprBlock, 
	[EXPR_PROC_CALL] = optimizeExprProcCall, 
	[EXPR_RETURN] = optimizeExprReturn, 
	[EXPR_ADD] = optimizeExprAdd, 
	[EXPR_SUB] = optimizeExprSub, 
	[EXPR_MUL] = optimizeExprMul, 
	[EXPR_DIV] = optimizeExprDiv, 
	[EXPR_MOD] = optimizeExprMod, 
	[EXPR_AND] = optimizeExprAnd, 
	[EXPR_OR] = optimizeExprOr, 
	[EXPR_XOR] = optimizeExprXor, 
	[EXPR_SHL] = optimizeExprShl, 
	[EXPR_SHR] = optimizeExprShr, 
	[EXPR_NOT] = optimizeExprNot, 
	[EXPR_CAST] = optimizeUnaryExpr, 
	[EXPR_LOGICAL_EQ] = optimizeComparisonExpr, 
	[EXPR_LOGICAL_NEQ] = optimizeComparisonExpr, 
	[EXPR_LOGICAL_LE] = optimizeComparisonExpr, 
	[EXPR_LOGICAL_GE] = optimizeComparisonExpr, 
	[EXPR_LOGICAL_LT] = optimizeComparisonExpr, 
	[EXPR_LOGICAL_GT] = optimizeComparisonExpr, 
	[EXPR_LOGICAL_AND] = optimizeBinaryExpr, 
	[EXPR_LOGICAL_OR] = optimizeBinaryExpr, 
	[EXPR_LOGICAL_NOT] = optimizeBinaryExpr, 
	[EXPR_IF] = optimizeControlFlowExpr, 
	[EXPR_VOID] = emptyOptimizer, 
	[EXPR_WHILE] = optimizeControlFlowExpr, 
	[EXPR_DOWHILE] = optimizeControlFlowExpr, 
	[EXPR_GET_REF] = optimizeExprGetRef, 
	[EXPR_DEREF] = optimizeExprDeref, 
	[EXPR_GET_ITEM] = optimizeExprGetItem, 
	[EXPR_ARRAY] = optimizeExprArray, 
	[EXPR_NEW_PROC] = optimizeExprNewProc,
	[EXPR_NEW_TYPE] = optimizeExprNewType,
	[EXPR_GET_ATTR] = optimizeExprGetAttr
};

void optimizeSourceCode(AST* ast)
{
	chainForeach (Expr, expr, ExprChain_slice(*getSubexprs(&ast->root), 1, -1)) {
		removeExprWrappers(expr);
		optimizeExpr(ast, expr, NULL);
	}
}

void parseSourceCode(BRP* obj, AST* dst) // br -> temporary AST
{
	dst->preprocessor = obj;
	dst->root.type = EXPR_BLOCK;
	dst->root.loc = peekToken(obj).loc;
	dst->root.block = NULL;
	initExpr(&dst->root);
	addSubexpr(&dst->root, (Expr){ .type = EXPR_REF });

	while (true) {
		if (peekToken(obj).type) {
			parseExpr(dst, addSubexpr(&dst->root, (Expr){0}), &dst->root, &dst->root, NULL, EXPRTYPE_DECL | EXPRTERM_FULL);
			fetchToken(obj);
		} else break;
	}

	optimizeSourceCode(dst);
}

// Bytecode Generation

typedef struct {
	sbufArray data_blocks;
	sbuf cur_src_path;
	uint32_t cur_src_line;
	AST* ast;
	BRB_ModuleBuilder dst;
	ExprChain static_vars;
} CodegenCtx;

typedef void (*ExprCompiler) (CodegenCtx*, Expr, BRB_DataBlock*);
ExprCompiler expr_compilers[N_EXPR_TYPES];

#define ASSERT_NO_BRB_ERR(err) { \
	BRB_Error _brb_temp_err = err; \
	if (_brb_temp_err.type != BRB_ERR_OK) { \
		eprintf("unexpected codegen failure:\n"); \
		BRB_printErrorMsg(stderr, _brb_temp_err, "codegen error"); \
		abort(); \
	} \
}

void compileSrcRef(CodegenCtx* ctx, TokenLoc loc)
{
	sbuf path_s = fromstr((char*)loc.src_name);
	if (!sbufeq(ctx->cur_src_path, path_s)) {
		ctx->cur_src_path = path_s;
		ctx->cur_src_line = loc.lineno;
	} else if (loc.lineno != ctx->cur_src_line) {
		ctx->cur_src_line = loc.lineno;
	}
}

void compileExprInvalid(CodegenCtx* ctx, Expr expr, BRB_DataBlock* block)
{
	assert(false, "uncompilable expression type %d", expr.type);
}

void compileExprSyscall(CodegenCtx* ctx, Expr expr, BRB_DataBlock* block)
{
	compileSrcRef(ctx, expr.loc);

	chainForeach (Expr, subexpr, ExprChain_slice(*getSubexprs(&expr), 1, -1)) {
		expr_compilers[subexpr->type](ctx, *subexpr);
	}
	ASSERT_NO_BRB_ERR(BRB_addOp_sys(&ctx->dst, expr.syscall_id));
}

void compileExprBuiltin(CodegenCtx* ctx, Expr expr, BRB_DataBlock* block)
{
	compileSrcRef(ctx, expr.loc);
	if (block) {
		ASSERT_NO_BRB_ERR(BRB_addDataPiece_builtin(block, expr.builtin_id));
	} else ASSERT_NO_BRB_ERR(BRB_addOp_builtin(&ctx->dst, expr.builtin_id));
}

void compileExprString(CodegenCtx* ctx, Expr expr, BRB_DataBlock* block)
{
	compileSrcRef(ctx, expr.loc);

	int str_index = -1;
	arrayForeach (sbuf, data_block, ctx->data_blocks) { // search for identical string literals in the AST
		if (sbufeq(*data_block, expr.string)) {
			str_index = data_block - ctx->data_blocks.data;
			break;
		}
	}
	if (str_index < 0) { // add the string literal to the data segment of the resulting module if it's unique
		str_index = ctx->data_blocks.length;
		BRB_DataBlock* new_db;
		char str_name[32];
		snprintf(str_name, sizeof(str_name), STR_PREFIX"%d", str_index);
		ASSERT_NO_BRB_ERR(BRB_addDataBlock(&ctx->dst, &new_db, str_name, false));
		sbufArray_append(&ctx->data_blocks, expr.string);
	}

	char str_name[32];
	snprintf(str_name, sizeof(str_name), STR_PREFIX"%d", str_index);
	ASSERT_NO_BRB_ERR(BRB_addOp_dbaddr_byName(&ctx->dst, str_name));
}

void compileExprInt(CodegenCtx* ctx, Expr expr, BRB_DataBlock* block)
{
	compileSrcRef(ctx, expr.loc);
	if (expr.var_type->kind == KIND_PTR) {
		ASSERT_NO_BRB_ERR(BRB_addOp_intptr(&ctx->dst, expr.int_literal));
	} else {
		switch (expr.var_type->size) {
			case 1:
				ASSERT_NO_BRB_ERR(BRB_addOp_int8(&ctx->dst, expr.int_literal));
				break;
			case 2:
				ASSERT_NO_BRB_ERR(BRB_addOp_int16(&ctx->dst, expr.int_literal));
				break;
			case 4:
				ASSERT_NO_BRB_ERR(BRB_addOp_int32(&ctx->dst, expr.int_literal));
				break;
			case 8:
				ASSERT_NO_BRB_ERR(BRB_addOp_int64(&ctx->dst, expr.int_literal));
				break;
			default:
				assert(false, "invalid integer size: %u\n", expr.var_type->size);
		}
	}
}

static int array_init_counter;

void compileExprNewVar(CodegenCtx* ctx, Expr expr, BRB_DataBlock* block)
{
	compileSrcRef(ctx, expr.loc);
	char* name = getFullDeclName(&expr);
	Expr* decl_info = getSubexpr(&expr, 2);
	Expr* initializer = getSubexpr(&expr, 3);
	int size = getTypeMemorySize(*decl_info->var_type);

	if (decl_info->attrs & ATTR_STATIC) {
		if (initializer) {
			fprintf(ctx->dst, ".data mut \"%s\" {\n", name);
			expr_compilers[initializer->type](ctx, *initializer, 0, codegenValueLiteral(size));
			fprintf(ctx->dst, "}\n");
		} else fprintf(ctx->dst, ".data mut \"%s\" zero %u\n", size);
	} else {
		if (initializer) {
			expr_compilers[initializer->type](ctx, *initializer, reg_state, codegenValueNewVar(name, size));
		} else fprintf(ctx->dst, "\tvar \"%s\" %d\n", name, size);
	}

	free(name);
}

void compileExprAssign(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	int field_size = getTypeSize(getExprValueType(ctx->ast, *expr.arg2));

	if (expr.arg1->type == EXPR_GET) {
		if (getSubexpr(expr.arg1->arg1, 2)->attrs & ATTR_STATIC) {
			

	switch (field_size) {
		case 4:
			fprintf(ctx->dst, "\tsx32 r%d r%d\n", dst_reg, dst_reg);
			break;
		case 2:
			fprintf(ctx->dst, "\tsx16 r%d r%d\n", dst_reg, dst_reg);
			break;
		case 1:
			fprintf(ctx->dst, "\tsx8 r%d r%d\n", dst_reg, dst_reg);
			break;
		default: break;
	}
}

void compileExprGetVar(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);
	Expr* decl_info = getSubexpr(expr.arg1, 2);
	if (decl_info->var_type->kind == KIND_ARRAY) {
		fprintf(
			ctx->dst,
			decl_info->attrs & ATTR_STATIC ? "\tsetd r%d . \"%s\"\n" : "\tsetv r%d \"%s\"\n",
			dst_reg, getSubexpr(expr.arg1, 0)->name
		);
	} else {
		if (decl_info->attrs & ATTR_STATIC) {
			fprintf(
				ctx->dst,
				"\tsetd r%1$d . \"%2$s\"\n"
				"\tld%3$d r%1$d r%1$d\n",
				dst_reg, getDeclName(expr.arg1), getTypeSize(*decl_info->var_type) * 8
			);
		} else fprintf(ctx->dst, "\tldvs r%d \"%s\"\n", dst_reg, getSubexpr(expr.arg1, 0)->name);
	}
}

void compileExprBlock(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);
	chainForeach (Expr, subexpr, ExprChain_slice(*getSubexprs(&expr), 1, -1)) {
		expr_compilers[subexpr->type](ctx, *subexpr, reg_state, 0);
	}

	if (expr.n_local_vars > 0) fprintf(ctx->dst, "\tdelnv %d\n", expr.n_local_vars);
}

void compileExprProcCall(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);
	int cache_id = compileRegCaching(ctx, reg_state);
	Expr* proc_decl = getSubexpr(&expr, 0)->arg1;

// evaluating arguments
	int i = 0;
	chainForeach (Expr, subexpr, ExprChain_slice(*getSubexprs(&expr), 1, -1)) {
		expr_compilers[subexpr->type](ctx, *subexpr, (1 << i) - 1, i);
		i += 1;
	}
// allocating space for the return value if it's more than the size of a register
	TypeDef* ret_type = getSubexpr(proc_decl, 2)->var_type;
	int retval_size = 0;
	if (ret_type->kind != KIND_VOID) {
		retval_size = getTypeSize(*ret_type);
		if (retval_size > 8)
			fprintf(ctx->dst, "\tvar \".retval\" %d\n", retval_size);
	}

	fprintf(ctx->dst, "\tcall . \"%s\"\n", getDeclName(proc_decl));

	if (ret_type->kind != KIND_VOID) {
		if (retval_size > 8) {
			fprintf(ctx->dst, "\t.renamev \".retval\" \".r%d\"\n", dst_reg);
		} else if (dst_reg != 0) fprintf(ctx->dst, "\tsetr r%d r0\n", dst_reg);
	}
	compileRegUncaching(ctx, reg_state, cache_id);
}

void compileExprReturn(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, 0);
	fprintf(ctx->dst, "\tret\n");
}

void compileExprAdd(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tadd r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tadd r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);
		
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\taddr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprSub(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(
			ctx->dst,
			"\tnot r%d r%d\n"
			"\tadd r%d r%d %lld\n",
			dst_reg, dst_reg,
			dst_reg, dst_reg, getIntLiteral(expr.arg1) + 1
		);
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tsub r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tsubr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprMul(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tmul r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tmul r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tmulr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprDiv(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tdivs r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tdivsr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprMod(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tmods r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tmodsr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprAnd(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tand r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tand r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tandr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprOr(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tor r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tor r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\torr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprXor(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\txor r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\txor r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\txorr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprShl(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tshl r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tshlr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprShr(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tshrs r%d r%d %lld\n", dst_reg, dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tshrsr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void compileExprNot(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
	fprintf(ctx->dst, "\tnot r%d r%d\n", dst_reg, dst_reg);
}

void _compileExprLogicalEq(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tnot r%d r%d\n\tcmp r%d %lld\n", dst_reg, dst_reg, dst_reg, ~getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tcmp r%d %lld\n", dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tcmpr r%d r%d\n", dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void _compileExprLogicalNeq(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tnot r%d r%d\n\tcmp r%d %lld\n", dst_reg, dst_reg, dst_reg, ~getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tcmp r%d %lld\n", dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);
		printf("%d %d\n", dst_reg, arg2_dst_reg);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tcmpr r%d r%d\n", dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void _compileExprLogicalLt(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tnot r%d r%d\n\tcmp r%d %lld\n", dst_reg, dst_reg, dst_reg, ~getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tcmp r%d %lld\n", dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tcmpr r%d r%d\n", dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void _compileExprLogicalGt(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tnot r%d r%d\n\tcmp r%d %lld\n", dst_reg, dst_reg, dst_reg, ~getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tcmp r%d %lld\n", dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tcmpr r%d r%d\n", dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void _compileExprLogicalLe(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tnot r%d r%d\n\tcmp r%d %lld\n", dst_reg, dst_reg, dst_reg, ~getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tcmp r%d %lld\n", dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tcmpr r%d r%d\n", dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void _compileExprLogicalGe(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tnot r%d r%d\n\tcmp r%d %lld\n", dst_reg, dst_reg, dst_reg, ~getIntLiteral(expr.arg1));
	} else if (isExprIntLiteral(expr.arg2)) {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		fprintf(ctx->dst, "\tcmp r%d %lld\n", dst_reg, getIntLiteral(expr.arg2));
	} else {
		int arg2_dst_reg, cache_id;
		getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
		fprintf(ctx->dst, "\tcmpr r%d r%d\n", dst_reg, arg2_dst_reg);

		freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
	}
}

void _compileExprLogicalAnd(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	int arg2_dst_reg, cache_id;
	getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

	expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
	expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
	fprintf(ctx->dst, "\tcmp r%d 0\n\tcmp:neq r%d 0\n\tsetc r%d neq\n", dst_reg, arg2_dst_reg, dst_reg);

	freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
}

void _compileExprLogicalOr(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	int arg2_dst_reg, cache_id;
	getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

	expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
	expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
	fprintf(ctx->dst, "\tcmp r%d 0\n\tcmp:equ r%d 0\n", dst_reg, arg2_dst_reg);

	freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
}

void _compileExprLogicalNot(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
	fprintf(ctx->dst, "\tcmp r%d 0\n", dst_reg);
}

void compileLogicalExpr(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	switch (expr.type) {
		case EXPR_LOGICAL_EQ:
			_compileExprLogicalEq(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d equ\n", dst_reg);
			return;
		case EXPR_LOGICAL_NEQ:
			_compileExprLogicalNeq(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d neq\n", dst_reg);
			return;
		case EXPR_LOGICAL_LT:
			_compileExprLogicalLt(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d lts\n", dst_reg);
			return;
		case EXPR_LOGICAL_GT:
			_compileExprLogicalGt(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d gts\n", dst_reg);
			return;
		case EXPR_LOGICAL_LE:
			_compileExprLogicalLe(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d les\n", dst_reg);
			return;
		case EXPR_LOGICAL_GE:
			_compileExprLogicalGe(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d ges\n", dst_reg);
			return;
		case EXPR_LOGICAL_AND:
			_compileExprLogicalAnd(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d neq\n", dst_reg);
			return;
		case EXPR_LOGICAL_OR:
			_compileExprLogicalOr(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d neq\n", dst_reg);
			return;
		case EXPR_LOGICAL_NOT:
			_compileExprLogicalNot(ctx, expr, reg_state, dst_reg);
			fprintf(ctx->dst, "\tsetc r%d equ\n", dst_reg);
			return;
		default:
			assert(false, "invalid expression type %d", expr.type);
	}
}

static int block_counter = 0;

void compileExprIf(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);
	int block_id = block_counter++;

	switch (expr.arg1->type) {
		case EXPR_LOGICAL_EQ:
			_compileExprLogicalEq(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:neq \".else%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_NEQ:
			_compileExprLogicalNeq(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:equ \".else%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_LT:
			_compileExprLogicalLt(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:ges \".else%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_GT:
			_compileExprLogicalGt(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:les \".else%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_LE:
			_compileExprLogicalLe(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:gts \".else%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_GE:
			_compileExprLogicalGe(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:lts \".else%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_AND:
			_compileExprLogicalAnd(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:equ \".else%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_OR:
			_compileExprLogicalOr(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:equ \".else%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_NOT:
			_compileExprLogicalNot(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:neq \".else%d\"\n", block_id);
			break;
		case EXPR_CAST:
			if (expr.arg1->var_type->kind == KIND_BOOL) {
				expr_compilers[expr.arg1->arg1->type](ctx, *expr.arg1->arg1, reg_state, dst_reg);
				fprintf(ctx->dst, "\tcmp r%d 0\n\tgoto:equ \".else%d\"\n", dst_reg, block_id);
				break;
			}
		default:
			assert(false, "invalid expression type %d", expr.arg1->type);
	}
	if (expr.arg3) {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tgoto \".end%d\"\n\tmark \".else%d\"\n", block_id, block_id);
		expr_compilers[expr.arg3->type](ctx, *expr.arg3, reg_state, dst_reg);
		fprintf(ctx->dst, "\tmark \".end%d\"\n", block_id);
	} else {
		expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
		fprintf(ctx->dst, "\tmark \".else%d\"\n", block_id);
	}
}

void compileExprVoid(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{}

void compileExprWhile(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);
	int block_id = block_counter++;

	fprintf(ctx->dst, "\tmark \".start%d\"\n", block_id);
	switch (expr.arg1->type) {
		case EXPR_LOGICAL_EQ:
			_compileExprLogicalEq(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:neq \".end%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_NEQ:
			_compileExprLogicalNeq(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:equ \".end%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_LT:
			_compileExprLogicalLt(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:ges \".end%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_GT:
			_compileExprLogicalGt(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:les \".end%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_LE:
			_compileExprLogicalLe(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:gts \".end%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_GE:
			_compileExprLogicalGe(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:lts \".end%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_AND:
			_compileExprLogicalAnd(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:equ \".end%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_OR:
			_compileExprLogicalOr(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:equ \".end%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_NOT:
			_compileExprLogicalNot(ctx, *expr.arg1, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:neq \".end%d\"\n", block_id);
			break;
		case EXPR_CAST:
			if (expr.arg1->var_type->kind == KIND_BOOL) {
				expr_compilers[expr.arg1->arg1->type](ctx, *expr.arg1->arg1, reg_state, dst_reg);
				fprintf(ctx->dst, "\tcmp r%d 0\n\tgoto:equ \".end%d\"\n", dst_reg, block_id);
				break;
			}
		default:
			assert(false, "invalid expression type %d", expr.arg1->type);
	}

	expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
	fprintf(ctx->dst, "\tgoto \".start%d\"\n\tmark \".end%d\"\n", block_id, block_id);
}

void compileExprDoWhile(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);
	int block_id = block_counter++;

	fprintf(ctx->dst, "\tmark \".start%d\"\n", block_id);
	expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);

	switch (expr.arg2->type) {
		case EXPR_LOGICAL_EQ:
			_compileExprLogicalEq(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:equ \".start%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_NEQ:
			_compileExprLogicalNeq(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:neq \".start%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_LT:
			_compileExprLogicalLt(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:lts \".start%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_GT:
			_compileExprLogicalGt(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:gts \".start%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_LE:
			_compileExprLogicalLe(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:les \".start%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_GE:
			_compileExprLogicalGe(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:ges \".start%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_AND:
			_compileExprLogicalAnd(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:neq \".start%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_OR:
			_compileExprLogicalOr(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:neq \".start%d\"\n", block_id);
			break;
		case EXPR_LOGICAL_NOT:
			_compileExprLogicalNot(ctx, *expr.arg2, reg_state, dst_reg);
			fprintf(ctx->dst, "\tgoto:equ \".start%d\"\n", block_id);
			break;
		case EXPR_CAST:
			if (expr.arg2->var_type->kind == KIND_BOOL) {
				expr_compilers[expr.arg2->arg1->type](ctx, *expr.arg2->arg1, reg_state, dst_reg);
				fprintf(ctx->dst, "\tcmp r%d 0\n\tgoto:neq \".start%d\"\n", dst_reg, block_id);
				break;
			}
		default:
			assert(false, "invalid expression type %d", expr.arg2->type);
	}
}

void compileExprGetRef(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	fprintf(ctx->dst, "\tsetv r%d \"%s\"\n", dst_reg, getSubexpr(expr.arg1->arg1, 0)->name);
}

void compileExprDeref(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
	fprintf(ctx->dst, "\tld%ds r%d r%d\n", getTypeSize(*getExprValueType(ctx->ast, *expr.arg1).base) * 8, dst_reg, dst_reg);
}

void compileExprCast(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);

	if (isExprIntLiteral(expr.arg1)) {
		if (expr.var_type->kind == KIND_BOOL) {
			fprintf(ctx->dst, "\tset r%d %d\n", dst_reg, getIntLiteral(expr.arg1) != 0);
		} else fprintf(ctx->dst, "\tset r%d %lld\n", dst_reg, getIntLiteral(expr.arg1));
	} else {
		expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
		if (expr.var_type->kind == KIND_BOOL)
			fprintf(ctx->dst, "\tcmp r%d 0\n\tsetc r%d neq\n", dst_reg, dst_reg);
	}
}

void compileExprNewProc(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	sbuf proc_name = fromstr(getDeclName(&expr));
	Expr* proc_info = getSubexpr(&expr, 2);
	Expr* proc_args = getSubexpr(&expr, 3);
	Expr* proc_body = getSubexpr(proc_args, proc_info->n_args + 1);

	if (sbufeq(proc_name, fromcstr("main"))) ctx->has_entry_point = true;

	compileSrcRef(ctx, expr.loc);
	fprintf(ctx->dst, "%sproc \"%.*s\"\n", proc_info->attrs & ATTR_EXTERNAL ? "ext" : "", unpack(proc_name));
// initializing arguments
	int i = proc_info->n_args + 1;
	chainRevForeach (Expr, iter, ExprChain_slice(*getSubexprs(proc_args), 1, proc_info->n_args)) {
		const int arg_size = getTypeSize(*getSubexpr(iter, 2)->var_type);
		if (arg_size > 8) {
			fprintf(ctx->dst, "\targ \"%s\" %d\n", getDeclName(iter), arg_size);
		} else fprintf(ctx->dst, "\tpushv \"%s\" %d r%d\n", getDeclName(iter), arg_size, --i);
	}
// initializing space for the return value
	if (proc_info->var_type->kind != KIND_VOID) {
		int retval_size = getTypeSize(*proc_info->var_type);
		if (retval_size > 8)
			fprintf(ctx->dst, "\targ \".retval\" %d\n", retval_size);
	}
// compiling the body of the procedure
	expr_compilers[proc_body->type](ctx, *proc_body, 0, 0);
	if (proc_info->var_type->kind == KIND_VOID) fputs("\tret\n", ctx->dst);
	fprintf(ctx->dst, "endproc\n");
// compiling deferred declarations found in the procedure
	chainForeach(Expr, static_var_decl, ctx->static_vars) {
		expr_compilers[static_var_decl->type](ctx, *static_var_decl, 0, -1);
	}
	ExprChain_clear(&ctx->static_vars);
}

void compileExprNewType(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);
	chainForeach (Expr, field, ExprChain_slice(*getSubexprs(&expr), 1, -1)) {
		if (field->type == EXPR_NEW_PROC)
			compileExprNewProc(ctx, *field, reg_state, 0);
	}
}

void compileExprGetAttr(CodegenCtx* ctx, Expr expr, regstate_t reg_state, CodegenValueTarget dst)
{
	compileSrcRef(ctx, expr.loc);
	/*if (expr.arg1->type == EXPR_GET) {
		Expr* decl_info = getSubexpr(expr.arg1->arg1, 2);
		Expr* attr_info = getSubexpr(expr.arg2, 2);
		int attr_size = getTypeMemorySize(*attr_info->var_type);
		if (attr_size <= 8) {
			if (decl_info->attrs & ATTR_STATIC) {
				char* decl_name = getFullDeclName(expr.arg1->arg1);
				fprintf(
					ctx->dst,
					"ldds r%x \"%s\":%d :%d\n",
					dst_reg,
					decl_name,
					getTypeMemorySize(*getSubexpr(expr.arg2, 2)->var_type),
					getAttrOffset(expr.arg2)
				);
				free(decl_name);
			} else {
				fprintf(
					ctx->dst,
					"ldvs r%x \"%s\":%d :%d\n",
					dst_reg,
					getDeclName(expr.arg1->arg1),
					getTypeMemorySize(*getSubexpr(expr.arg2, 2)->var_type),
					getAttrOffset(expr.arg2)
				);
			}
		} else {
			printf("%d\n", attr_size);
			assert(false, "codegen for EXPR_GET_ATTR for this case is not implemented yet");
		}
		return;
	}*/

	int obj_size = getTypeSize(getExprValueType(ctx->ast, *expr.arg1));
	TypeDef attr_type = *getSubexpr(expr.arg2, 2)->var_type;
	int attr_size = getTypeSize(attr_type);

	expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
	if (obj_size <= 8) {
		printf("\tand r%d r%d %llu\n", dst_reg, dst_reg, byteMask(attr_size, getAttrOffset(expr.arg2)));
	} else {
		if (attr_size <= 8) {
			fprintf(ctx->dst,
				"\tldvs r%d \".r%d\":%d :%d\n"
				"\tdel \".r%d\n",
				dst_reg, dst_reg, attr_size, getAttrOffset(expr.arg2), dst_reg);
		} else fprintf(ctx->dst,
				"\treszv \".r%d\" %d %d\n",
				dst_reg, attr_size, getAttrOffset(expr.arg2));
	}
}

ExprCompiler expr_compilers[] = {
	[EXPR_INVALID    ] = &compileExprInvalid,
	[EXPR_SYSCALL    ] = &compileExprSyscall,
	[EXPR_NAME       ] = &compileExprInvalid,
	[EXPR_BUILTIN    ] = &compileExprBuiltin,
	[EXPR_STRING     ] = &compileExprString,
	[EXPR_INT        ] = &compileExprInt,
	[EXPR_NEW_VAR    ] = &compileExprNewVar,
	[EXPR_ASSIGN     ] = &compileExprAssign,
	[EXPR_GET        ] = &compileExprGetVar,
	[EXPR_BLOCK      ] = &compileExprBlock,
	[EXPR_DECL_INFO  ] = &compileExprInvalid,
	[EXPR_REF        ] = &compileExprInvalid,
	[EXPR_PROC_CALL  ] = &compileExprProcCall,
	[EXPR_RETURN     ] = &compileExprReturn,
	[EXPR_ADD        ] = &compileExprAdd,
	[EXPR_SUB        ] = &compileExprSub,
	[EXPR_MUL        ] = &compileExprMul,
	[EXPR_DIV        ] = &compileExprDiv,
	[EXPR_MOD        ] = &compileExprMod,
	[EXPR_AND        ] = &compileExprAnd,
	[EXPR_OR         ] = &compileExprOr,
	[EXPR_XOR        ] = &compileExprXor,
	[EXPR_SHL        ] = &compileExprShl,
	[EXPR_SHR        ] = &compileExprShr,
	[EXPR_NOT        ] = &compileExprNot,
	[EXPR_CAST       ] = &compileExprCast,
	[EXPR_LOGICAL_EQ ] = &compileLogicalExpr,
	[EXPR_LOGICAL_NEQ] = &compileLogicalExpr,
	[EXPR_LOGICAL_LT ] = &compileLogicalExpr,
	[EXPR_LOGICAL_GT ] = &compileLogicalExpr,
	[EXPR_LOGICAL_LE ] = &compileLogicalExpr,
	[EXPR_LOGICAL_GE ] = &compileLogicalExpr,
	[EXPR_LOGICAL_AND] = &compileLogicalExpr,
	[EXPR_LOGICAL_OR ] = &compileLogicalExpr,
	[EXPR_LOGICAL_NOT] = &compileLogicalExpr,
	[EXPR_WRAPPER    ] = &compileExprInvalid,
	[EXPR_IF         ] = &compileExprIf,
	[EXPR_VOID       ] = &compileExprVoid,
	[EXPR_WHILE      ] = &compileExprWhile,
	[EXPR_DOWHILE    ] = &compileExprDoWhile,
	[EXPR_GET_REF    ] = &compileExprGetRef,
	[EXPR_DEREF      ] = &compileExprDeref,
	[EXPR_ADD_ASSIGN ] = &compileExprInvalid,
	[EXPR_SUB_ASSIGN ] = &compileExprInvalid,
	[EXPR_MUL_ASSIGN ] = &compileExprInvalid,
	[EXPR_DIV_ASSIGN ] = &compileExprInvalid,
	[EXPR_AND_ASSIGN ] = &compileExprInvalid,
	[EXPR_XOR_ASSIGN ] = &compileExprInvalid,
	[EXPR_OR_ASSIGN  ] = &compileExprInvalid,
	[EXPR_SHL_ASSIGN ] = &compileExprInvalid,
	[EXPR_SHR_ASSIGN ] = &compileExprInvalid,
	[EXPR_GET_ITEM   ] = &compileExprInvalid,
	[EXPR_ARRAY      ] = &compileExprInvalid,
	[EXPR_NEW_PROC   ] = &compileExprNewProc,
	[EXPR_NEW_TYPE   ] = &compileExprNewType,
	[EXPR_GET_ATTR   ] = &compileExprGetAttr
};
static_assert(N_EXPR_TYPES == 56, "not all expression types have corresponding compilers defined");

BRB_Module compileAST(AST src)
{
	CodegenCtx ctx = {.ast = &src};

	chainForeach (Expr, stmt, ExprChain_slice(*getSubexprs(&src->root), 1, -1)) {
		expr_compilers[stmt->type](&ctx, *stmt);
	}

	sbufArray_clear(&ctx.data_blocks);
	ExprChain_clear(&ctx.static_vars);

	return 
}

void printUsageMsg(FILE* dst, char* program_name)
{
	fprintf(
		dst,
		"bridge - Multi-tool for the Bridge language and VM\n"
		"usage: %s [options...] <source path>\n"
		"options:\n"
		"\t-h		Display this message and quit\n"
		"\t-dE		Output the parsed source code and quit. This is different from the `-Ap` option because this mode also performs syntactical analysis\n"
		"\t-dM		Output all macros defined in the source code\n"
		"\t--no-opt	Disable bytecode-level optimizations\n"
		"\t-Ap		Preprocess the input and output it back to the terminal\n"
		"\t-Ab		Generate BRidge Bytecode from the input and save it to a `.brb` file. Default option for `br` input\n"
		"\t-As		Generate native assembly from the input and save it to a `.S` file\n"
		"\t-Ao		Generate native object file from the input and save it to a `.o` file\n"
		"\t-Ax		Generate native executable program from the input and save it to a file with no extension\n"
		"\t-Ai		Interpret the program, compiling it beforehand if necessary. Default option for `brb` input\n"
		"\t-o <path>	Write the output to <path>;\n"
		"\t\tif <path> is a directory, output will be at <path>/<source name>.<appropriate extension>;\n"
		"\t\tby default, the output is saved at <source dir>/<source name>.<appropriate extension>\n",
		program_name
	);
}

sbuf EXTS[] = {
	['p'] = fromcstr("br"),
	['b'] = fromcstr("brb"),
	['s'] = fromcstr("S"),
	['o'] = fromcstr("o"),
	['x'] = fromcstr(""),
	['i'] = fromcstr("brb.temp")
};

void printMacros(BRP* prep) {
	arrayForeach(Macro, macro, prep->macros) {
		printTokenLoc(macro->def_loc);
		printf("#define %s%c", macro->name, macro->args.length ? '(' : '\0');
		arrayForeach(MacroArg, arg, macro->args) {
			printf("%s%s", arg->name, (uint64_t)(arg - macro->args.data) < macro->args.length - 1 ? ", " : "");
		}
		printf("%c %.*s\n", macro->args.length ? ')' : '\0', unpack(macro->def));
	}
}

sbuf br_symbols[] = {
	[SYMBOL_ARGSPEC_START] = BRP_SYMBOL("("),
	[SYMBOL_ARGSPEC_END] = BRP_SYMBOL(")"),
	[SYMBOL_COMMA] = BRP_SYMBOL(","),
	[SYMBOL_BLOCK_START] = BRP_SYMBOL("{"),
	[SYMBOL_BLOCK_END] = BRP_SYMBOL("}"),
	[SYMBOL_SEMICOLON] = BRP_SYMBOL(";"),
	[SYMBOL_ADD_ASSIGN] = BRP_SYMBOL("+="),
	[SYMBOL_SUB_ASSIGN] = BRP_SYMBOL("-="),
	[SYMBOL_MUL_ASSIGN] = BRP_SYMBOL("*="),
	[SYMBOL_DIV_ASSIGN] = BRP_SYMBOL("/="),
	[SYMBOL_AND_ASSIGN] = BRP_SYMBOL("&="),
	[SYMBOL_XOR_ASSIGN] = BRP_SYMBOL("^="),
	[SYMBOL_OR_ASSIGN] = BRP_SYMBOL("|="),
	[SYMBOL_SHL_ASSIGN] = BRP_SYMBOL("<<="),
	[SYMBOL_SHR_ASSIGN] = BRP_SYMBOL(">>="),
	[SYMBOL_EQ] = BRP_SYMBOL("=="),
	[SYMBOL_NEQ] = BRP_SYMBOL("!="),
	[SYMBOL_LE] = BRP_SYMBOL("<="),
	[SYMBOL_GE] = BRP_SYMBOL(">="),
	[SYMBOL_NOT] = BRP_SYMBOL("!"),
	[SYMBOL_OR] = BRP_SYMBOL("||"),
	[SYMBOL_AND] = BRP_SYMBOL("&&"),
	[SYMBOL_ASSIGNMENT] = BRP_SYMBOL("="),
	[SYMBOL_PLUS] = BRP_SYMBOL("+"),
	[SYMBOL_MINUS] = BRP_SYMBOL("-"),
	[SYMBOL_STAR] = BRP_SYMBOL("*"),
	[SYMBOL_DIV] = BRP_SYMBOL("/"),
	[SYMBOL_MOD] = BRP_SYMBOL("%"),
	[SYMBOL_AMPERSAND] = BRP_SYMBOL("&"),
	[SYMBOL_PIPE] = BRP_SYMBOL("|"),
	[SYMBOL_CARET] = BRP_SYMBOL("^"),
	[SYMBOL_LSHIFT] = BRP_SYMBOL("<<"),
	[SYMBOL_RSHIFT] = BRP_SYMBOL(">>"),
	[SYMBOL_TILDE] = BRP_SYMBOL("~"),
	[SYMBOL_LT] = BRP_SYMBOL("<"),
	[SYMBOL_GT] = BRP_SYMBOL(">"),
	[SYMBOL_INDEX_START] = BRP_SYMBOL("["),
	[SYMBOL_INDEX_END] = BRP_SYMBOL("]"),
	[SYMBOL_DOT] = BRP_SYMBOL("."),
	BRP_HIDDEN_SYMBOL(" "),
	BRP_HIDDEN_SYMBOL("\t"),
	BRP_HIDDEN_SYMBOL("\n"),
	(sbuf){0}
};
static_assert(N_SYMBOLS == 39, "not all symbols are handled");

sbuf br_keywords[] = {
		BRP_KEYWORD("void"),
		BRP_KEYWORD("int8"),
		BRP_KEYWORD("int16"),
		BRP_KEYWORD("int32"),
		BRP_KEYWORD("int64"),
		BRP_KEYWORD("sys"),
		BRP_KEYWORD("builtin"),
		BRP_KEYWORD("return"),
		BRP_KEYWORD("cast"),
		BRP_KEYWORD("bool"),
		BRP_KEYWORD("if"),
		BRP_KEYWORD("else"),
		BRP_KEYWORD("while"),
		BRP_KEYWORD("for"),
		BRP_KEYWORD("do"),
		BRP_KEYWORD("static"),
		BRP_KEYWORD("type"),
		(sbuf){0}
};
static_assert(N_KWS == 17, "not all keywords are handled");

const char* search_paths[] = { ".", NULL };

void preprocessSourceCode(const char* input_path, bool output_macros, sbuf symbols[], sbuf keywords[])
{
	BRP prep;
	if (!initBRP(&prep, NULL, BRP_ESC_STR_LITERALS)) {
		eprintf("error: could not initialize the preprocessor due to memory shortage\n");
		exit(1);
	}

	int n_symbols = -1;
	while (symbols[++n_symbols].data);
	symbols[n_symbols - 3] = BRP_SYMBOL("\n");
	symbols[n_symbols - 2] = BRP_SYMBOL("\t");
	symbols[n_symbols - 1] = BRP_SYMBOL(" ");
	setSymbols(&prep, symbols);
	setKeywords(&prep, keywords);
	setInput(&prep, (char*)input_path, fopen(input_path, "r"));

	while (true) {
		Token token = fetchToken(&prep);
		if (!token.type) break;
		printTokenText(token, &prep);
	}
	putchar('\n');
	if (output_macros) printMacros(&prep);
	delBRP(&prep);

	symbols[n_symbols - 3] = BRP_HIDDEN_SYMBOL("\n");
	symbols[n_symbols - 2] = BRP_HIDDEN_SYMBOL("\t");
	symbols[n_symbols - 1] = BRP_HIDDEN_SYMBOL(" ");
}

static BRB_Module sourceCode2ByteCode(const char* input_path, bool output_ast, bool output_macros)
{
	BRP prep;
	if (!initBRP(&prep, NULL, BRP_ESC_STR_LITERALS)) {
		eprintf("error: could not initialize the preprocessor due to memory shortage\n");
		exit(1);
	}

	setSymbols(&prep, br_symbols);
	setKeywords(&prep, br_keywords);
	FILE* fd;
	if (!(fd = fopen(input_path, "r"))) {
		eprintf("error: could not open file: `%s` (reason: %s)\n", input_path, strerror(errno));
		exit(1);
	}
	setInput(&prep, (char*)input_path, fd);

	AST ast;
	parseSourceCode(&prep, &ast);

	if (output_ast) printAST(&ast);
	if (output_macros) printMacros(&prep);

	if (!(fd = fopen(output_path, "w"))) {
		eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
		exit(1);
	}

	compileAST(&ast, fd);
	fclose(fd);
	delBRP(&prep);
}

static BRB_Module loadByteCodeFromFile(const char* input_path)
{
	BRB_Module res;
	BRB_Error err = loadModule(fopen(input_path, "rb"), &res);
	if (err.code) {
		eprintf("error while loading bytecode from `%s`:\n", input_path);
		BRB_printErrorMsg(stderr, err);
	}
	return res;
}

static void callNativeAssembler(const char* input_path, const char* output_path)
{
	char cmd[1024] = {0};
	snprintf(cmd, sizeof(cmd), "as -arch arm64 -o \"%s\" \"%s\"", input_path, output_path);
	int exitcode = system(cmd);
	if (exitcode) {
		eprintf("error: native assembler exited abnormally with exit code %d\n", exitcode);
		exit(1);
	}
}

static void callNativeLinker(const char* input_path, const char* output_path)
{
	char cmd[1024] = {0};
	snprintf(
		cmd,
		sizeof(cmd),
		"ld -e .entry -syslibroot `xcrun --show-sdk-path` -lSystem -arch arm64 -o \"%s\" \"%s\"",
		output_path, input_path
	);
	int exitcode = system(cmd);
	if (exitcode) {
		eprintf("error: native linker exited abnormally with exit code %d\n", exitcode);
		exit(1);
	}
}

#define DEBUG_OPT_MACROS 0x2
#define DEBUG_OPT_EXPRS 0x4

bool interrupt = false;
void handleExecInt(int sig)
{
	interrupt = true;
}

int main(int argc, char* argv[])
{
	TYPE_STR_LITERAL.kind = KIND_PTR;
	TypeDef str_lit_base = (TypeDef){ .kind = KIND_INT, .size = 1 };
	TYPE_STR_LITERAL.base = &str_lit_base;

	bool optimize = true, output_macros = false, output_ast = false, compiler_action_set = false;
	char compiler_action = 'b', *input_path = NULL, *output_path = NULL, *input_type = NULL;
	for (int i = 1; i < argc; ++i) {
		bool go_on = false;
		if (argv[i][0] == '-') {
			for (++argv[i]; *argv[i]; ++argv[i]) {
				if (go_on) { go_on = false; break; }
				switch (*argv[i]) {
					case 'h':
						printUsageMsg(stdout, argv[0]);
						return 0;
					case 'd':
						argv[i] += 1;
						if (argv[i][0] == 'M') {
							output_macros = true;
						} else if (argv[i][0] == 'E') {
							output_ast = true;
						} else {
							eprintf("error: unknown option `-d%c`\n", argv[i][0]);
							return 1;
						}
						break;
					case 'A': 
						argv[i] += 1;
						if (!strchr("pbsoxi", argv[i][0])) {
							eprintf("error: unknown option `-S%c`\n", argv[i][0]);
							return 1;
						}
						
						compiler_action = argv[i][0];
						compiler_action_set = true;
						break;
					case 'o':
						if (!argv[++i]) {
							eprintf("error: `-o` option specified but no executable output file path provided\n");
							return 1;
						}
						output_path = argv[i];
						go_on = true;
						break;
					case 't':
						if (!argv[++i]) {
							eprintf("error: `-t` option specified but no input type provided\n");
							return 1;
						}
						input_type = argv[i];
						go_on = true;
						break;
					case '-':
						argv[i] += 1;
						if (streq(argv[i], "no-opt")) {
							optimize = false;
						} else {
							eprintf("error: unknown option `--%s`\n", argv[i]);
							return 1;
						}
						go_on = true;
						break;
					default:
						eprintf("error: unknown option `-%c`\n", *argv[i]);
						return 1;
				}
			}
		} else {
			if (input_path) {
				eprintf("error: got more than one input path\n");
				return 1;
			}
			input_path = argv[i];
		}
	}
	if (!input_path) {
		eprintf("error: no input path provided\n");
		return 1;
	}

	if (!input_type) input_type = getFileExt_s(fromstr(input_path)).data;

	if (output_path) {
		if (isPathDir(output_path))
			output_path = tostr(
				fromstr(output_path),
				PATHSEP,
				fileBaseName_s(fromstr(input_path)),
				EXTS[(unsigned)compiler_action]
			);
	} else output_path = setFileExt(input_path, EXTS[(unsigned)compiler_action].data);

	BRB_Module res;
	FILE* fd;

	if (strcmp(input_type, "br") == 0) switch (compiler_action) {
		case 'p':
			preprocessSourceCode(input_path, output_macros, br_symbols, br_keywords);
			return 0;
		case 'b':
			res = sourceCode2ByteCode(input_path);
			
			if (!(fd = fopen(output_path, "wb"))) {
				eprintf("error: could not open file `%s` for writing BRB output (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			writeModule(&res, fd);
			fclose(fd);
			return 0;
		case 'i':
			sourceCode2VBRB(input_path, output_path, output_ast, output_macros);
			
			signal(SIGINT, handleExecInt);
			ExecEnv env;
			const char* module_argv[] = { input_path, NULL };
			initExecEnv(&env, &res, module_argv);
			execModule(&env, &res, &interrupt);
			signal(SIGINT, SIG_DFL);
			unlink(output_path);
			return 0;
		case 's':
			sourceCode2VBRB(input_path, output_path, output_ast, output_macros);
			res = loadVBRB(output_path, NULL, optimize);
			if (!(fd = fopen(output_path, "wb"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			return 0;
		case 'o':
			sourceCode2VBRB(input_path, output_path, output_ast, output_macros);
			res = loadVBRB(output_path, NULL, optimize);
			if (!(fd = fopen(output_path, "wb"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			callNativeAssembler(output_path, output_path);
			return 0;
		case 'x':
			sourceCode2VBRB(input_path, output_path, output_ast, output_macros);
			res = loadVBRB(output_path, NULL, optimize);
			if (!(fd = fopen(output_path, "wb"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			callNativeAssembler(output_path, output_path);
			callNativeLinker(output_path, output_path);
			return 0;
		default:
			assert(false, "");
	} else if (strcmp(input_type, "vbrb") == 0) switch (compiler_action) {
		case 'p':
		case 'v':
			preprocessSourceCode(input_path, output_macros, vbrb_symbols, vbrb_keywords);
			return 0;
		case 'b':
			res = loadVBRB(input_path, NULL, optimize);
			
			if (!(fd = fopen(output_path, "wb"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			writeModule(&res, fd);
			fclose(fd);
			return 0;
		case 'i':
			res = loadVBRB(input_path, NULL, optimize);

			signal(SIGINT, handleExecInt);
			ExecEnv env;
			const char* module_argv[] = { input_path, NULL };
			initExecEnv(&env, &res, module_argv);
			execModule(&env, &res, &interrupt);
			signal(SIGINT, SIG_DFL);
			return 0;
		case 's':
			res = loadVBRB(input_path, NULL, optimize);
			if (!(fd = fopen(output_path, "w"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			return 0;
		case 'o':
			res = loadVBRB(input_path, NULL, optimize);
			if (!(fd = fopen(output_path, "w"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			callNativeAssembler(output_path, output_path);
			return 0;
		case 'x':
			res = loadVBRB(input_path, NULL, optimize);
			if (!(fd = fopen(output_path, "w"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			callNativeAssembler(output_path, output_path);
			callNativeLinker(output_path, output_path);
			return 0;
	} else if (strcmp(input_type, "brb") == 0) switch (compiler_action_set ? compiler_action : 'i') {
		case 'p':
			eprintf("error: cannot preprocess binary data\n");
			return 1;
		case 'v':
			assert(false, "bytecode disassembly is not implemented yet");
			return 0;
		case 'b':
			eprintf("error: due to the combination of options and the input, program is forced to do literally nothing\n");
			return 1;
		case 'i':
			res = loadByteCodeFromFile(input_path, optimize);
			signal(SIGINT, handleExecInt);
			ExecEnv env;
			const char* module_argv[] = { input_path, NULL };
			initExecEnv(&env, &res, module_argv);
			execModule(&env, &res, &interrupt);
			signal(SIGINT, SIG_DFL);
			return 0;
		case 's':
			res = loadBRB(input_path, optimize);
			if (!(fd = fopen(output_path, "w"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			return 0;
		case 'o':
			res = loadBRB(input_path, optimize);
			if (!(fd = fopen(output_path, "w"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			callNativeAssembler(output_path, output_path);
			return 0;
		case 'x':
			res = loadBRB(input_path, optimize);
			if (!(fd = fopen(output_path, "w"))) {
				eprintf("error: could not open file `%s` (reason: %s)\n", output_path, strerror(errno));
				return 1;
			}
			compileModule(&res, fd);
			fclose(fd);
			callNativeAssembler(output_path, output_path);
			callNativeLinker(output_path, output_path);
			return 0;
	}

	eprintf("error: unknown input type: %s\n"
		"note: supported input types: br, vbrb, brb\n",
		input_type);
	return 1;
}
