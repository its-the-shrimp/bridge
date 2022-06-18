#include <brb.h>
#include <errno.h>
#include <sys/cdefs.h>

declArray(int);
defArray(int);
defArray(str);

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
    KW_AND,
    KW_OR,
    KW_NOT,
    KW_IF,
    KW_ELSE,
    KW_WHILE,
    KW_FOR,
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
    N_TYPE_KINDS
} TypeKind;

typedef struct typedef_t {
    TypeKind kind;
    int32_t n_items;
    union {
        int size;
        struct typedef_t* base;
    };
} TypeDef;

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
static TypeDef TYPE_STR_LITERAL;
#define isPtrType(tdef) ((tdef).kind == KIND_ARRAY || (tdef).kind == KIND_PTR)
#define isIntType(tdef) ((tdef).kind == KIND_BUILTIN_VAL || (tdef).kind == KIND_INT || (tdef).kind == KIND_BOOL)

typedef enum {
    EXPR_INVALID,
    EXPR_SYSCALL, // main, composite, evaluatable
    EXPR_NAME, // auxillary
    EXPR_BUILTIN, // main, unary, evaluatable
    EXPR_STRING, // main, unary, evaluatable
    EXPR_INT, // main, unary, evaluatable
    EXPR_NEW_VAR, // main, composite, non-evaluatable
    EXPR_ASSIGN, // main, binary, evaluatable
    EXPR_ADD_ASSIGN, // main, binary, evaluatable
    EXPR_SUB_ASSIGN, // main, binary, evaluatable
    EXPR_MUL_ASSIGN, // main, binary, evaluatable
    EXPR_DIV_ASSIGN, // main, binary, evaluatable
    EXPR_AND_ASSIGN, // main, binary, evaluatable
    EXPR_XOR_ASSIGN, // main, binary, evaluatable
    EXPR_OR_ASSIGN, // main, binary, evaluatable
    EXPR_SHL_ASSIGN, // main, binary, evaluatable
    EXPR_SHR_ASSIGN, // main, binary, evaluatable
    EXPR_GET_VAR, // main, unary, evaluatable
    EXPR_BLOCK, // main, composite, non-evaluatable
    EXPR_TYPE, // auxillary
    EXPR_REF, // auxillary
    EXPR_PROC_REF, // auxillary
    EXPR_PROC_CALL, // main, composite, evaluatable
    EXPR_RETURN, // main, unary, non-evaluatable
    EXPR_ADD, // main, binary, evaluatable
    EXPR_SUB, // main, binary, evaluatable
    EXPR_MUL, // main, binary, evaluatable
    EXPR_DIV, // main, binary, evaluatable
    EXPR_MOD, // main, binary, evaluatable
    EXPR_AND, // main, binary, evaluatable
    EXPR_OR, // main, binary, evaluatable
    EXPR_XOR, // main, binary, evaluatable
    EXPR_SHL, // main, binary, evaluatable
    EXPR_SHR, // main, binary, evaluatable
    EXPR_NOT, // main, unary, evaluatable
    EXPR_CAST, // main, binary, evaluatable
    EXPR_LOGICAL_EQ, // main, binary, evaluatable
    EXPR_LOGICAL_NEQ, // main, binary, evaluatable
    EXPR_LOGICAL_LE, // main, binary, evaluatable
    EXPR_LOGICAL_GE, // main, binary, evaluatable
    EXPR_LOGICAL_LT, // main, binary, evaluatable
    EXPR_LOGICAL_GT, // main, binary, evaluatable
    EXPR_LOGICAL_AND, // main, binary, evaluatable
    EXPR_LOGICAL_OR, // main, binary, evaluatable
    EXPR_LOGICAL_NOT, // main, unary, evaluatable
    EXPR_WRAPPER, // main, unary, evaluatable
    EXPR_IF, // main, ternary, non-evaluatable
    EXPR_VOID, // main, nullary, evaluatable
    EXPR_WHILE, // main, binary, non-evaluatable
    EXPR_DOWHILE, // main, binary, non-evaluatable
    EXPR_GET_REF, // main, unary, evaluatable
    EXPR_DEREF, // main, unary, evaluatable
    EXPR_GET_ITEM, // main, binary, evaluatable
    EXPR_ARRAY, // main, variadic, evaluatable
    N_EXPR_TYPES
} ExprType;

typedef struct expr {
    union {
        struct expr* arg1;
    };
    union {
        struct {
            union {
                struct expr* arg2;
                int64_t int_literal; // for EXPR_INT
                TypeDef* var_type; // for EXPR_TYPE
                char* name; // for EXPR_NAME
            };
            union {
                struct expr* arg3; // for ternary expressions
                TypeDef* element_type; // for EXPR_ARRAY
            };
        };
        sbuf string;
    };
    struct expr* block;
    TokenLoc loc;
    ExprType type;
} Expr;
declChain(Expr);
defChain(Expr);


typedef struct {
    TokenLoc loc;
    char* name;
    Expr args;
    TypeDef return_type;
    Expr body;
} FuncDecl;
declChain(FuncDecl);
defChain(FuncDecl);

typedef struct {
    FuncDeclChain functions;
    Expr globals;
    BRP* prep;
} AST;

// flags for parseExpr desribing how the expression will be used
#define EXPRTERM_FULL        0b1
#define EXPRTERM_ARG         0b10
#define EXPRTERM_BRACKET     0b100
#define EXPRTERM_SQBRACKET   0b1000
#define EXPRTERM_ARRAY_ARG   0b10000
#define EXPRTYPE_EVALUATABLE 0b100000
#define EXPRTYPE_VOIDABLE    0b1000000
#define EXPRTYPE_LOGICAL     0b10000000
#define EXPRTERM             (EXPRTERM_FULL | EXPRTERM_ARG | EXPRTERM_BRACKET | EXPRTERM_SQBRACKET | EXPRTERM_ARRAY_ARG)
#define EXPRTYPE             (EXPRTYPE_EVALUATABLE | EXPRTYPE_VOIDABLE | EXPRTYPE_LOGICAL)

#define VARIADIC -1
#define NULLARY 0
#define UNARY 1
#define BINARY 2
#define TERNARY 3

static char expr_arity_table[] = {
    [EXPR_INVALID    ] = NULLARY,
    [EXPR_SYSCALL    ] = VARIADIC,
    [EXPR_NAME       ] = NULLARY,
    [EXPR_BUILTIN    ] = NULLARY,
    [EXPR_STRING     ] = NULLARY,
    [EXPR_INT        ] = NULLARY,
    [EXPR_NEW_VAR    ] = VARIADIC,
    [EXPR_GET_VAR    ] = UNARY,
    [EXPR_BLOCK      ] = VARIADIC, // TODO: make blocks evaluate to their "return" value
    [EXPR_TYPE       ] = NULLARY,
    [EXPR_REF        ] = UNARY,
    [EXPR_PROC_REF   ] = NULLARY,
    [EXPR_PROC_CALL  ] = VARIADIC,
    [EXPR_RETURN     ] = UNARY,
    [EXPR_NOT        ] = UNARY,
    [EXPR_MUL        ] = BINARY,
    [EXPR_DIV        ] = BINARY,
    [EXPR_MOD        ] = BINARY,
    [EXPR_SUB        ] = BINARY,
    [EXPR_ADD        ] = BINARY,
    [EXPR_SHL        ] = BINARY,
    [EXPR_SHR        ] = BINARY,
    [EXPR_AND        ] = BINARY,
    [EXPR_XOR        ] = BINARY,
    [EXPR_OR         ] = BINARY,
    [EXPR_ASSIGN     ] = BINARY,
    [EXPR_ADD_ASSIGN ] = BINARY,
    [EXPR_SUB_ASSIGN ] = BINARY,
    [EXPR_MUL_ASSIGN ] = BINARY,
    [EXPR_DIV_ASSIGN ] = BINARY,
    [EXPR_AND_ASSIGN ] = BINARY,
    [EXPR_XOR_ASSIGN ] = BINARY,
    [EXPR_OR_ASSIGN  ] = BINARY,
    [EXPR_SHR_ASSIGN ] = BINARY,
    [EXPR_SHL_ASSIGN ] = BINARY,
    [EXPR_CAST       ] = UNARY,
    [EXPR_LOGICAL_EQ ] = BINARY,
    [EXPR_LOGICAL_NEQ] = BINARY,
    [EXPR_LOGICAL_LT ] = BINARY,
    [EXPR_LOGICAL_GT ] = BINARY,
    [EXPR_LOGICAL_LE ] = BINARY,
    [EXPR_LOGICAL_GE ] = BINARY,
    [EXPR_LOGICAL_AND] = BINARY,
    [EXPR_LOGICAL_OR ] = BINARY,
    [EXPR_LOGICAL_NOT] = UNARY,
    [EXPR_WRAPPER    ] = UNARY,
    [EXPR_IF         ] = TERNARY,
    [EXPR_VOID       ] = NULLARY,
    [EXPR_WHILE      ] = BINARY,
    [EXPR_DOWHILE    ] = BINARY,
    [EXPR_GET_REF    ] = UNARY,
    [EXPR_DEREF      ] = UNARY,
    [EXPR_GET_ITEM   ] = BINARY,
    [EXPR_ARRAY      ] = VARIADIC
};
static char expr_order_table[] = {
    [EXPR_INVALID    ] = 0,
    [EXPR_SYSCALL    ] = 0,
    [EXPR_NAME       ] = 0,
    [EXPR_BUILTIN    ] = 0,
    [EXPR_STRING     ] = 0,
    [EXPR_INT        ] = 0,
    [EXPR_NEW_VAR    ] = 0,
    [EXPR_GET_VAR    ] = 0,
    [EXPR_BLOCK      ] = 0, // TODO: make blocks evaluate to their "return" value
    [EXPR_TYPE       ] = 0,
    [EXPR_REF        ] = 0,
    [EXPR_PROC_REF   ] = 0,
    [EXPR_PROC_CALL  ] = 0,
    [EXPR_CAST       ] = 0,
    [EXPR_WRAPPER    ] = 0,
    [EXPR_IF         ] = 0,
    [EXPR_WHILE      ] = 0,
    [EXPR_DOWHILE    ] = 0,
    [EXPR_VOID       ] = 0,
    [EXPR_GET_ITEM   ] = 0,
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
static_assert(N_EXPR_TYPES == 54, "not all expression types have their arity and order set");

static inline void initExpr(Expr* expr)
{
    *(ExprChain*)expr = ExprChain_new(0);
}

static inline ExprChain* getSubexprs(Expr* expr)
{
    assert(expr_arity_table[expr->type] == VARIADIC, "attempted to get subexpressions of a non-variadic expression");
    return (ExprChain*)expr;
}

static inline Expr* addSubexpr(Expr* expr, Expr new)
{
    assert(expr_arity_table[expr->type] == VARIADIC, "attempted to add a subexpression to a non-varaidic expression");
    return ExprChain_append(getSubexprs(expr), new);
}

static inline Expr* getSubexpr(Expr* expr, int id)
{
    assert(expr_arity_table[expr->type] == VARIADIC, "attempted to get a subexpression of a non-variadic expression");
    return ExprChain_getref(*getSubexprs(expr), id);
}

static inline int getSubexprsCount(Expr* expr)
{
    assert(expr_arity_table[expr->type] == VARIADIC, "attempted to get the amount of subexpressions of a non-variadic expression");
    return ExprChain_length(*getSubexprs(expr));
}

const int COUNTER_BASE = __COUNTER__;
#define BR_ERROR_DEF(error_name, args) \
const short error_name##Code = (__COUNTER__ - COUNTER_BASE); \
DEF_WITH_ATTRS(void raise##error_name args, __abortlike)

DEF_WITH_ATTRS(void _reportCompilerBug(const char* func_name, int lineno, const char* msg), __abortlike)
{
    eprintf("internal compiler bug in %s on line %d: %s\n", func_name, lineno, msg);
    abort();
}
#define reportCompilerBug(msg, ...) do { \
    char* _compiler_bug_msg; \
    asprintf(&_compiler_bug_msg, msg, __VA_ARGS__); \
    _reportCompilerBug(__func__, __LINE__, _compiler_bug_msg); \
} while (0)

void fprintType(FILE* dst, TypeDef type)
{
    static_assert(N_TYPE_KINDS == 7, "not all type kinds are handled in fprintType");
    if (type.kind == KIND_INT) {
        fprintf(dst, "int%d", type.size * 8);
    } else if (type.kind == KIND_PTR) {
        fprintType(dst, *type.base);
        fputc('*', dst);
    } else if (type.kind == KIND_VOID) {
        fputs("void", dst);
    } else if (type.kind == KIND_BUILTIN_VAL) {
        fputs("__builtin_val", dst);
    } else if (type.kind == KIND_BOOL) {
        fputs("bool", dst);
    } else if (type.kind == KIND_ARRAY) {
        fprintType(dst, *type.base);
        fprintf(dst, "[%d]", type.n_items);
    } else reportCompilerBug("unknown type kind: %d", type.kind);
}
#define printType(type) fprintType(stdout, type)

BR_ERROR_DEF(InvalidGlobalStmtError, (AST* ast, Token token))
{
    fprintTokenLoc(stderr, token.loc);
    eprintf("error %04hx: expected a global statement specifier, instead got ", InvalidGlobalStmtErrorCode);
    fprintTokenStr(stderr, token, ast->prep);
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

BR_ERROR_DEF(ArgCountMismatchError, (AST* ast, TokenLoc loc, FuncDecl* func, int n_args))
{
    fprintTokenLoc(stderr, loc);
    if (n_args >= 0) {
        eprintf(
            "error %04hx: function `%s` expects exactly %d argument(s), instead got %d\n",
            ArgCountMismatchErrorCode, func->name, getSubexprsCount(&func->args) - 1, n_args
        );
    } else {
        eprintf(
            "error %04hx: function `%s` expects exactly %d argument(s), instead got more\n",
            ArgCountMismatchErrorCode, func->name, getSubexprsCount(&func->args) - 1
        );
    }
    fprintTokenLoc(stderr, func->loc);
    eprintf("note: function `%s` is declared here\n", func->name);
    exit(1);
}

BR_ERROR_DEF(VoidCastError, (AST* ast, TokenLoc loc))
{
    fprintTokenLoc(stderr, loc);
    eprintf("error %04hx: cannot cast a value to type `", VoidCastErrorCode);
    fprintType(stderr, VOID_TYPE);
    eputs("`\n");
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
        } else fprintTokenStr(stderr, iter, ast->prep);
        if (n == n_tokens - 2) {
            eputs(" or ");
        } else if (n != n_tokens - 1) eputs(", ");
    }
    va_end(args);
    
    if (msg) {
        eprintf(" as %s, ", msg);
    } else eputs(", ");

    eputs("instead got ");
    fprintTokenStr(stderr, actual, ast->prep);
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
    fprintType(stderr, *getSubexpr(var_decl, 1)->var_type);
    eputs("`, instead got a value of type `");
    fprintType(stderr, entry_type);
    eputs("`\n");
    fprintTokenLoc(stderr, var_decl->loc);
    eprintf("note: variable `%s` is declared here\n", var_name);
    exit(1);
}

BR_ERROR_DEF(ReturnTypeMismatchError, (AST* ast, TokenLoc loc, TypeDef entry_type, FuncDecl* func))
{
    fprintTokenLoc(stderr, loc);
    eprintf("error %04hx: function `%s` is declared to return a value of type `", ReturnTypeMismatchErrorCode, func->name);
    fprintType(stderr, func->return_type);
    eputs("`, instead attempted to return value of type `");
    fprintType(stderr, entry_type);
    eputs("`\n");
    fprintTokenLoc(stderr, func->loc);
    eprintf("note: function `%s` is declared here\n", func->name);
    exit(1);
}

BR_ERROR_DEF(ArgTypeMismatchError, (AST* ast, TokenLoc loc, FuncDecl* func, Expr* arg_decl, TypeDef entry_type))
{
    fprintTokenLoc(stderr, loc);
    if (func) {
        eprintf("error %04hx: argument `%s` of function `%s` expects a value of type `", ArgTypeMismatchErrorCode, getSubexpr(arg_decl, 0)->name, func->name);
        fprintType(stderr, *getSubexpr(arg_decl, 1)->var_type);
    } else {
        eprintf("error %04hx: a syscall argument expects a value of a primitive type `", ArgTypeMismatchErrorCode);
    }
    eputs("`, instead got value of type `");
    fprintType(stderr, entry_type);
    eputs("`\n");
    if (func) {
        fprintTokenLoc(stderr, func->loc);
        eprintf("note: function `%s` is declared here\n", func->name);
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

BR_ERROR_DEF(NoProcReturnError, (AST* ast, FuncDecl* decl))
{
    fprintTokenLoc(stderr, decl->body.loc);
    eprintf("error %04hx: the function `%s` is declared to return a value, but its definition does not return a value in all control paths\n", NoProcReturnErrorCode, decl->name);
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

int getTypeSize(TypeDef type)
{
    static_assert(N_TYPE_KINDS == 7, "not all type kinds are handled in getTypeSize");
    switch (type.kind) {
        case KIND_INT: return type.size;
        case KIND_PTR:
        case KIND_ARRAY:
        case KIND_BUILTIN_VAL:
            return 8;
        case KIND_VOID: return 0;
        case KIND_BOOL: return 1;
        case KIND_NONE:
        default:
            reportCompilerBug("unknown type kind: %d", type.kind);
    }
}

bool isExprTerm(AST* ast, Token token, Expr* expr, int flags)
{
    static_assert(N_EXPR_TYPES == 54, "not all expression types are handled in isExprTerm");
    int64_t symbol_id = getTokenSymbolId(token);
    if ((expr->type == EXPR_BLOCK || expr->type == EXPR_IF || expr->type == EXPR_WHILE) && symbol_id == SYMBOL_BLOCK_END) return true; 
    if (flags & EXPRTERM_FULL) return symbol_id == SYMBOL_SEMICOLON;
    if (flags & EXPRTERM_ARG) {
        if (symbol_id == SYMBOL_ARGSPEC_END || symbol_id == SYMBOL_COMMA) return true;
        if (symbol_id == SYMBOL_SEMICOLON)
            raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_COMMA), symbolToken(SYMBOL_ARGSPEC_END));
        return false;
    }
    if (flags & EXPRTERM_ARRAY_ARG) {
        if (symbol_id == SYMBOL_BLOCK_END || symbol_id == SYMBOL_COMMA) return true;
        if (symbol_id == SYMBOL_SEMICOLON)
            raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_COMMA), symbolToken(SYMBOL_BLOCK_END));
        return false;
    }
    if (flags & EXPRTERM_BRACKET) return symbol_id == SYMBOL_ARGSPEC_END;
    if (flags & EXPRTERM_SQBRACKET) return symbol_id == SYMBOL_INDEX_END;
    return false;
}

bool _typeMatches(TypeDef field, TypeDef entry)
{
    static_assert(N_TYPE_KINDS == 7, "not all type kinds are handled in typeMatches");
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
        case KIND_NONE:
        default:
            reportCompilerBug("unknown type kind: %d", field.kind);
    }
}

bool typeMatches(TypeDef field, TypeDef entry)
{
    return field.kind == KIND_VOID ? false : _typeMatches(field, entry);
}

bool parseType(AST* ast, TypeDef* dst)
{
    static_assert(N_TYPE_KINDS == 7, "not all type kinds are handled in parseType");
    *dst = (TypeDef){0};
    bool fetched = false;
    while (true) {
        Token token = peekToken(ast->prep);
        if (token.type == TOKEN_KEYWORD) {
            switch (token.keyword_id) {
                case KW_INT8:
                case KW_INT16:
                case KW_INT32:
                case KW_INT64:
                    if (dst->kind != KIND_NONE) raiseInvalidTypeError(ast, token.loc);

                    fetchToken(ast->prep);
                    dst->kind = KIND_INT;
                    fetched = true;
                    dst->size = 1 << (token.keyword_id - KW_INT8);
                    break;
                case KW_VOID:
                    if (dst->kind != KIND_NONE) raiseInvalidTypeError(ast, token.loc);

                    fetchToken(ast->prep);
                    fetched = true;
                    dst->kind = KIND_VOID;
                    break;
                case KW_BOOL:
                    if (dst->kind != KIND_NONE) raiseInvalidTypeError(ast, token.loc);

                    fetchToken(ast->prep);
                    fetched = true;
                    dst->kind = KIND_BOOL;
                    break;
                default:
                    return fetched;
            }
        } else if (token.type == TOKEN_SYMBOL) {
            switch (token.symbol_id) {
                case SYMBOL_STAR: {
                    if (dst->kind == KIND_NONE) return fetched;

                    fetchToken(ast->prep);
                    TypeDef* new_base = malloc(sizeof(TypeDef));
                    *new_base = *dst;
                    dst->base = new_base;
                    dst->kind = KIND_PTR;
                    break;
                } case SYMBOL_INDEX_START: {
                    if (dst->kind == KIND_NONE) return fetched;
                    if (dst->kind == KIND_VOID) raiseVoidArrayError(ast, token.loc);

                    fetchToken(ast->prep);
                    Token index_spec = fetchToken(ast->prep);
                    int64_t n_items = 0;
                    if (index_spec.type == TOKEN_INT) {
                        n_items = index_spec.value;
                        if (n_items < 0) raiseNegativeArraySizeError(ast, index_spec.loc, n_items);
                        token = fetchToken(ast->prep);
                        if (getTokenSymbolId(token) != SYMBOL_INDEX_END) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_INDEX_END));
                    } else if (getTokenSymbolId(index_spec) != SYMBOL_INDEX_END)
                        raiseUnexpectedTokenError(ast, index_spec, "the array size specifier", symbolToken(SYMBOL_INDEX_END), intToken(0));
                    
                    TypeDef* new_base = malloc(sizeof(TypeDef));
                    *new_base = *dst;
                    dst->base = new_base;
                    dst->kind = n_items ? KIND_ARRAY : KIND_PTR;
                    dst->n_items = n_items;
                    break;
                } default:
                    return fetched;
            }
        } else return fetched;
    }
}

bool isExprEvaluatable(ExprType type) {
    static_assert(N_EXPR_TYPES == 54, "not all expression types are handled in isExprEvaluatable");
    static bool expr_evaluatability_info[N_EXPR_TYPES] = {
        [EXPR_INVALID   ] = false,
        [EXPR_INVALID + 1 ... N_EXPR_TYPES - 1] = true,
        [EXPR_NAME      ] = false,
        [EXPR_BLOCK     ] = false, // TODO: make blocks evaluate to their "return" value
        [EXPR_TYPE      ] = false,
        [EXPR_REF       ] = false,
        [EXPR_PROC_REF  ] = false,
        [EXPR_RETURN    ] = false,
        [EXPR_IF        ] = false,
        [EXPR_WHILE     ] = false,
        [EXPR_DOWHILE   ] = false,
        [EXPR_NEW_VAR   ] = false
    };
    assert(inRange(type, 0, N_EXPR_TYPES), "unknown expression type was previded");
    return expr_evaluatability_info[type];
}

TypeDef getExprValueType(AST* ast, Expr expr);

void fprintExpr(AST* ast, FILE* dst, Expr expr, int indent_level)
{
    for (int i = 0; i < indent_level; i++) {
        fputc('\t', dst);
    }
    static_assert(N_EXPR_TYPES == 54, "not all expression types are handled in fprintExpr");

    fprintTokenLoc(dst, expr.loc);
    if (isExprEvaluatable(expr.type)) {
        fprintf(dst, "(type: ");
        fprintType(dst, getExprValueType(ast, expr));
        fprintf(dst, ") ");
    }
    switch (expr.type) {
        case EXPR_SYSCALL:
            fprintf(dst, "SYSCALL %s\n", expr.name);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(ast, dst, subexpr, indent_level + 1); );
            break;
        case EXPR_NAME:
            fprintf(dst, "NAME %s\n", expr.name);
            break;
        case EXPR_BUILTIN:
            fprintf(dst, "BUILTIN %s\n", expr.name);
            break;
        case EXPR_STRING:
            fprintf(dst, "STRING \"%.*s\"\n", unpack(expr.string));
            break;
        case EXPR_INT:
            fprintf(dst, "INTEGER %lld\n", expr.int_literal);
            break;
        case EXPR_GET_VAR:
            fputs("GET VARIABLE `", dst);
            fprintType(dst, *getSubexpr(expr.arg1, 1)->var_type);
            fprintf(dst, " %s;`\n", getSubexpr(expr.arg1, 0)->name);
            break;
        case EXPR_ASSIGN:
            fputs("ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_ADD_ASSIGN:
            fputs("ADD & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SUB_ASSIGN:
            fputs("SUBTRACT & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_MUL_ASSIGN:
            fputs("MULTIPLY & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_DIV_ASSIGN:
            fputs("DIVIDE & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_AND_ASSIGN:
            fputs("BITWISE AND & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_XOR_ASSIGN:
            fputs("BITWISE EXCLUSIVE OR & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_OR_ASSIGN:
            fputs("BITWISE OR & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SHL_ASSIGN:
            fputs("SHIFT LEFT & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SHR_ASSIGN:
            fputs("SHIFT RIGHT & ASSIGN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_NEW_VAR:
            fputs("NEW VARIABLE:\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(ast, dst, subexpr, indent_level + 1); );
            break;
        case EXPR_TYPE:
            fputs("TYPE ", dst);
            fprintType(dst, *expr.var_type);
            fputs(" \n", dst);
            break;
        case EXPR_REF:
            fputs("REF -> `", dst);
            if (expr.arg1) {
                assert(expr.arg1->type == EXPR_NEW_VAR, "expected the variable chain to have an expression of type EXPR_NEW_VAR in it");
                fprintType(dst, *getSubexpr(expr.arg1, 1)->var_type);
                fprintf(dst, " %s;", getSubexpr(expr.arg1, 0)->name);
            } else fputs("NULL", dst);
            fputs("`\n", dst);
            break;
        case EXPR_PROC_REF:
            fputs("PROCEDURE REF -> `", dst);
            if (expr.arg1) {
                fprintType(dst, ((FuncDecl*)expr.arg1)->return_type);
                fprintf(dst, " %s", ((FuncDecl*)expr.arg1)->name);
                ExprNode* first_arg = getSubexprs(&((FuncDecl*)expr.arg1)->args)->start->next;
                if (!first_arg) fputc('(', dst);
                chain_foreach_from(Expr, arg, *getSubexprs(&((FuncDecl*)expr.arg1)->args), 1, 
                    fputs(_arg == first_arg ? "(" : ", ", dst);
                    fprintType(dst, *getSubexpr(&arg, 1)->var_type);
                    fprintf(dst, " %s", getSubexpr(&arg, 0)->name);
                );
                fputs(");", dst);
            } else fputs("NULL", dst);
            fputs("`\n", dst);
            break;
        case EXPR_BLOCK:
            fputs("BLOCK\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(ast, dst, subexpr, indent_level + 1); );
            break;
        case EXPR_PROC_CALL:
            fputs("CALL:\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(ast, dst, subexpr, indent_level + 1); );
            break;
        case EXPR_RETURN:
            fputs("RETURN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_ADD:
            fputs("ADD:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SUB:
            fputs("SUBTRACT:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_MUL:
            fputs("MULTIPLY:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_DIV:
            fputs("DIVIDE:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_MOD:
            fputs("MODULO:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_AND:
            fputs("BITWISE AND:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_OR:
            fputs("BITWISE OR:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_XOR:
            fputs("BITWISE EXCLUSIVE OR:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SHL:
            fputs("SHIFT LEFT:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SHR:
            fputs("SHIFT RIGHT:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_NOT:
            fputs("BITWISE NOT:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_CAST:
            fputs("CAST TO TYPE `", dst);
            fprintType(dst, *expr.var_type);
            fputs("`:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_LOGICAL_EQ:
            fputs("EQUALS:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_NEQ:
            fputs("NOT EQUALS:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_LT:
            fputs("LESS THAN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_GT:
            fputs("GREATER THAN:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_LE:
            fputs("LESS THAN OR EQUALS:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_GE:
            fputs("GREATER THAN OR EQUALS:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_AND:
            fputs("LOGICAL AND:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_OR:
            fputs("LOGICAL OR:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_NOT:
            fputs("LOGICAL NOT:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_WRAPPER:
            fputs("WRAPPED:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_IF:
            fputs("IF:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg3, indent_level + 1);
            break;
        case EXPR_VOID:
            fputs("VOID\n", dst);
            break;
        case EXPR_WHILE:
            fputs("LOOP WHILE:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_DOWHILE:
            fputs("LOOP UNTIL:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_GET_REF:
            fputs("GET REFERENCE:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_DEREF:
            fputs("DEREFERENCE:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_GET_ITEM:
            fputs("GET ITEM:\n", dst);
            fprintExpr(ast, dst, *expr.arg1, indent_level + 1);
            fprintExpr(ast, dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_ARRAY:
            fputs("ARRAY:\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(ast, dst, subexpr, indent_level + 1); );
            break;
        case EXPR_INVALID:
        case N_EXPR_TYPES:
        default:
            reportCompilerBug("unknown expression type %d", expr.type);
    }
}
#define printExpr(ast, expr, indent_level) fprintExpr(ast, stdout, expr, indent_level)

Expr* getVarDecl(Expr* block, char* name)
{
    for (; block; block = block->block) {
        assert(block->type == EXPR_BLOCK, "`block` argument is expected to be an expression of type EXPR_BLOCK");
        for (Expr* expr = getSubexpr(block, 0)->arg1; expr; expr = getSubexpr(expr, 2)->arg1) {
            assert(expr->type == EXPR_NEW_VAR, "expected the variable chain to have an expression of type EXPR_NEW_VAR in it");
            if (sbufeq(name, getSubexpr(expr, 0)->name)) return expr;
        }
    }

    return NULL;
}

FuncDecl* getFuncDecl(AST* ast, char* name)
{
    chain_foreach(FuncDecl, func, ast->functions,
        if (sbufeq(name, func.name)) return &_func->value;
    );
    return NULL;
}

bool isExprIntLiteral(Expr* expr)
{
    if (expr->type == EXPR_INT) return true;
    if (expr->type == EXPR_CAST) return isExprIntLiteral(expr->arg1);
    return false;
}

int64_t getIntLiteral(Expr* expr)
{
    if (expr->type == EXPR_INT) return expr->int_literal;
    if (expr->type == EXPR_CAST) {
        if (expr->var_type->kind == KIND_BOOL) return getIntLiteral(expr->arg1) != 0;
        return getIntLiteral(expr->arg1) & byteMask(getTypeSize(*expr->var_type));
    }
    assert(false, "attempted to get an integer literal from an expression which does not produce an integer literal");
}

static void wrapExpr(Expr* expr, ExprType new_expr_type, Expr arg2)
{
    Expr arg1 = *expr;
    expr->type = new_expr_type;
    switch (expr_arity_table[new_expr_type]) {
        case VARIADIC:
            initExpr(expr);
            addSubexpr(expr, arg1);
            if (arg2.type) addSubexpr(expr, arg2);
        case NULLARY:
            return;
        case BINARY:
            if (arg2.type) {
                expr->arg2 = malloc(sizeof(Expr));
                *expr->arg2 = arg2;
            }
        case UNARY:
            expr->arg1 = malloc(sizeof(Expr));
            *expr->arg1 = arg1;
            return;
        default:
            reportCompilerBug("unknown expression arity type %d", expr_arity_table[new_expr_type]);
    }
}

static void wrapExprInCast(Expr* expr, TypeDef new_type)
{
    Expr* arg1 = malloc(sizeof(Expr));
    *arg1 = *expr;
    expr->type = EXPR_CAST;
    expr->arg1 = arg1;
    expr->var_type = malloc(sizeof(TypeDef));
    *expr->var_type = new_type;
}

typedef void (*ExprTypeSetter) (AST*, Expr*, ExprType);

void defaultExprTypeSetter(AST* ast, Expr* expr, ExprType new_type)
{
    expr->type = new_type;
}

void setExprNameToExprGetVar(AST* ast, Expr* expr, ExprType new_type)
{
    Expr* decl = getVarDecl(expr->block, expr->name);
    if (!decl) raiseUnknownNameError(ast, (Token){ .loc = expr->loc, .type = TOKEN_WORD, .word = expr->name });

    expr->type = EXPR_GET_VAR;
    expr->arg1 = decl;
}

void setExprNameToExprFuncCall(AST* ast, Expr* expr, ExprType new_type)
{
    FuncDecl* decl = getFuncDecl(ast, expr->name);
    if (!decl) raiseUnknownNameError(ast, (Token){ .loc = expr->loc, .type = TOKEN_WORD, .word = expr->name });

    expr->type = new_type;
    initExpr(expr);
    addSubexpr(expr, (Expr){
        .type = EXPR_PROC_REF,
        .block = expr->block,
        .loc = expr->loc,
        .arg1 = (Expr*)decl
    });
}

void setBinaryExprType(AST* ast, Expr* expr, ExprType new_type)
{
    if (expr->type == EXPR_NAME) setExprNameToExprGetVar(ast, expr, EXPR_GET_VAR);
    wrapExpr(expr, new_type, (Expr){0});
}

DEF_WITH_ATTRS(bool setExprType(AST* ast, Expr* expr, Expr* parent_expr, ExprType new_type), __result_use_check)
// changes the type of the expression if the new type is suitable in place of the current expression type
{
    static_assert(N_EXPR_TYPES == 54, "not all expression types are handled in setExprType");
    static ExprTypeSetter override_table[N_EXPR_TYPES][N_EXPR_TYPES] = {
        [EXPR_INVALID   ] = {
            [EXPR_SYSCALL ... N_EXPR_TYPES - 1] = defaultExprTypeSetter,
            [EXPR_INVALID] = NULL,
            [EXPR_ADD ... EXPR_SHR] = NULL,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = NULL,
            [EXPR_GET_ITEM] = NULL
        },
        [EXPR_SYSCALL   ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
            [EXPR_GET_ITEM] = setBinaryExprType
        },
        [EXPR_NAME      ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_GET_VAR] = setExprNameToExprGetVar,
            [EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
            [EXPR_PROC_CALL] = setExprNameToExprFuncCall,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
            [EXPR_GET_ITEM] = setBinaryExprType
        },
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
        [EXPR_PROC_REF  ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_NEW_VAR   ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_GET_VAR   ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
            [EXPR_GET_ITEM] = setBinaryExprType
        },
        [EXPR_BLOCK     ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_TYPE      ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_PROC_CALL ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
            [EXPR_GET_ITEM] = setBinaryExprType
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
            [EXPR_GET_ITEM] = setBinaryExprType
        },
        [EXPR_IF      ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_VOID    ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_WHILE   ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_DOWHILE ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_GET_REF ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
            [EXPR_GET_ITEM] = setBinaryExprType
        },
        [EXPR_DEREF  ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
            [EXPR_GET_ITEM] = setBinaryExprType
        },
        [EXPR_GET_ITEM] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ASSIGN ... EXPR_SHR_ASSIGN] = setBinaryExprType,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType,
            [EXPR_GET_ITEM] = setBinaryExprType
        },
        [EXPR_ARRAY] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_GET_ITEM] = setBinaryExprType
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
    static_assert(N_EXPR_TYPES == 54, "not all expression types are handled in getExprValueType");
    static_assert(N_TYPE_KINDS == 7, "not all type kinds are handled in getExprValueType");
    switch (expr.type) {
        case EXPR_INVALID:
        case EXPR_NAME:
        case EXPR_NEW_VAR:
        case EXPR_TYPE:
        case EXPR_REF:
        case EXPR_PROC_REF:
        case EXPR_BLOCK:
        case EXPR_RETURN:
        case EXPR_IF:
        case EXPR_WHILE:
        case EXPR_DOWHILE:
            return (TypeDef){0};
        case EXPR_BUILTIN:
        case EXPR_SYSCALL:
            return BUILTIN_VAL_TYPE;
        case EXPR_STRING: return TYPE_STR_LITERAL;
        case EXPR_INT: return INT_TYPE(4);
        case EXPR_PROC_CALL: return ((FuncDecl*)getSubexpr(&expr, 0)->arg1)->return_type;
        case EXPR_GET_VAR: return *getSubexpr(expr.arg1, 1)->var_type;
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
            TypeDef arg1_type = getExprValueType(ast, *expr.arg1);
            TypeDef arg2_type = getExprValueType(ast, *expr.arg2);
            if (arg1_type.kind == KIND_INT && arg2_type.kind != KIND_INT) swap(arg1_type, arg2_type, TypeDef);

            static_assert(N_TYPE_KINDS == 7, "not all type kinds are handled in handling EXPR_ADD in getExprValueType");
            switch (arg1_type.kind) {
                case KIND_INT:
                    if (arg2_type.kind == KIND_INT) {
                        return arg1_type.size == arg2_type.size ? INT_TYPE(minInt(arg1_type.size * 2, 8)) : (arg1_type.size > arg2_type.size ? arg1_type : arg2_type);
                    } else reportCompilerBug("unexpected type kind %d", arg2_type.kind);
                    break;
                case KIND_ARRAY:
                case KIND_PTR:
                    return isPtrType(arg2_type) || expr.arg2->type == EXPR_INT && expr.arg2->int_literal < 0 ? INT_TYPE(8) : arg1_type; 
                case KIND_BUILTIN_VAL:
                    return INT_TYPE(8);
                case KIND_BOOL: {
                    int arg1_size = getTypeSize(arg1_type), arg2_size = getTypeSize(arg2_type);
                    return arg1_size == arg2_size ? INT_TYPE(minInt(arg1_size * 2, 8)) : (arg1_size > arg2_size ? INT_TYPE(arg1_size) : INT_TYPE(arg2_size));
                }
                case KIND_VOID:
                case KIND_NONE:
                case N_TYPE_KINDS:
                default:
                    reportCompilerBug("unexpected type kind %d", arg1_type.kind);
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
        case N_EXPR_TYPES:
        default:
            reportCompilerBug("unknown expression type %d", expr.type);
    }
}

void printAST(AST* ast)
{
    chain_foreach(FuncDecl, func, ast->functions, 
        printTokenLoc(func.loc);
        printf("function %s:\n", func.name);
        puts("arguments:");
        printExpr(ast, func.args, 0);
        puts("body:");
        printExpr(ast, func.body, 0);
    );
}

bool matchType(AST* ast, TypeDef field_type, Expr* expr)
{
    static_assert(N_TYPE_KINDS == 7, "not all type kinds are handled in typeMatches");

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

void parseExpr(AST* ast, Expr* dst, Expr* parent_expr, Expr* block, FuncDecl* func, int flags);

bool parseVarDecl(AST* ast, Token token, TypeDef var_type, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_NEW_VAR)) return true;

    if (var_type.kind == KIND_VOID) raiseVoidVarDeclError(ast, token.loc);
    initExpr(dst);

    token = fetchToken(ast->prep);
    if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(ast, token, "a variable name", wordToken(NULL));
    Expr* decl = getVarDecl(dst->block, token.word);
    if (decl) raiseNameTakenError(ast, token, decl->loc);
    addSubexpr(dst, (Expr){
        .type = EXPR_NAME,
        .name = token.word,
        .block = dst->block,
        .loc = dst->loc
    });

    TypeDef* new_var_type = malloc(sizeof(TypeDef));
    *new_var_type = var_type;
    addSubexpr(dst, (Expr){
        .type = EXPR_TYPE,
        .var_type = new_var_type,
        .block = dst->block,
        .loc = dst->loc
    });
    addSubexpr(dst, (Expr){
        .type = EXPR_REF,
        .arg1 = getSubexpr(dst->block, 0)->arg1,
        .block = dst->block,
        .loc = dst->loc
    });
    getSubexpr(dst->block, 0)->arg1 = dst;

    return false;
}

bool parseIntLiteral(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_INT)) {
        unfetchToken(ast->prep, token);
        return true;
    }
    dst->int_literal = token.value;

    return false;
}

bool parseStrLiteral(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_STRING)) {
        unfetchToken(ast->prep, token);
        return true;
    }
    dst->string = token.string;

    return false;
}

typedef bool (*KwParser) (AST*, Token, Expr*, Expr*, FuncDecl*, int);

bool parseKwSys(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_SYSCALL)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    initExpr(dst);
    token = fetchToken(ast->prep);
    if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(ast, token, "a syscall name", wordToken(NULL));
    bool name_vaildated = false;
    for (int i = 0; i < N_SYS_OPS; i++) {
        if (sbufeq(syscallNames[i], token.word)) {
            name_vaildated = true;
            break;
        }
    }
    if (!name_vaildated) raiseUnknownSyscallNameError(ast, token);
    addSubexpr(dst, (Expr){
        .type = EXPR_NAME,
        .name = token.word,
        .block = dst->block,
        .loc = token.loc
    });

    token = fetchToken(ast->prep);
    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));

    token = peekToken(ast->prep);
    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_END) {
        while (true) {
            Expr* arg = addSubexpr(dst, (Expr){0});
            parseExpr(ast, arg, dst, dst->block, func, EXPRTERM_ARG | EXPRTYPE_EVALUATABLE);

            token = fetchToken(ast->prep);
            if (token.symbol_id == SYMBOL_ARGSPEC_END) {
                break;
            } else if (token.symbol_id != SYMBOL_COMMA)
                raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_COMMA), symbolToken(SYMBOL_ARGSPEC_END));
        }
    } else fetchToken(ast->prep);
    if (getSubexprsCount(dst) > 7) raiseTooManySyscallArgsError(ast, token.loc, getSubexprsCount(dst) - 1);

    return false;
}

bool parseKwBuiltin(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_BUILTIN)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    token = fetchToken(ast->prep);
    if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(ast, token, "a built-in name", wordToken(NULL));
    bool name_vaildated = false;
    for (int i = 0; i < N_BUILTINS; i++) {
        if (sbufeq(builtins[i].name, token.word)) {
            name_vaildated = true;
            break;
        }
    }
    if (!name_vaildated) raiseUnknownBuiltinNameError(ast, token);
    dst->name = token.word;

    return false;
}

bool parseKwReturn(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_RETURN)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    dst->arg1 = calloc(1, sizeof(Expr));
    parseExpr(ast, dst->arg1, dst, dst->block, func, EXPRTERM_FULL | EXPRTYPE_EVALUATABLE | EXPRTYPE_VOIDABLE);

    return false;
}

bool parseKwCast(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_CAST)) {
        unfetchToken(ast->prep, token);
        return true;
    }
    
    token = fetchToken(ast->prep);
    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));

    dst->arg1 = calloc(1, sizeof(Expr));
    parseExpr(ast, dst->arg1, dst, dst->block, func, EXPRTERM_ARG | EXPRTYPE_EVALUATABLE);
    token = fetchToken(ast->prep);
    if (getTokenSymbolId(token) != SYMBOL_COMMA) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_COMMA));

    token = peekToken(ast->prep);
    dst->var_type = malloc(sizeof(TypeDef));
    if (!parseType(ast, dst->var_type)) raiseInvalidTypeError(ast, token.loc);
    if (dst->var_type->kind == KIND_VOID) raiseVoidCastError(ast, token.loc);
    token = fetchToken(ast->prep);
    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_END) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_END));

    return false;
}

bool parseKwAndOr(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_LOGICAL_AND + token.keyword_id - KW_AND)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    dst->arg2 = calloc(1, sizeof(Expr));
    token = peekToken(ast->prep);
    parseExpr(ast, dst->arg2, dst, dst->block, func, flags & EXPRTERM | EXPRTYPE_EVALUATABLE);

    return false;
}

bool parseKwNot(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_LOGICAL_NOT)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    dst->arg1 = calloc(1, sizeof(Expr));
    parseExpr(ast, dst->arg1, dst, dst->block, func, flags & EXPRTERM | EXPRTYPE_EVALUATABLE);

    return false;
}

bool parseKwIf(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_IF)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    token = fetchToken(ast->prep);
    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));

    dst->arg1 = calloc(1, sizeof(Expr));
    parseExpr(ast, dst->arg1, dst, dst->block, func, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE | EXPRTYPE_LOGICAL);
    fetchToken(ast->prep);

    dst->arg2 = calloc(1, sizeof(Expr));
    token = peekToken(ast->prep);
    parseExpr(ast, dst->arg2, dst, dst->block, func, EXPRTERM_FULL);
    if (dst->arg2->type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(ast, token.loc, "`if` statement");

    Token stmt_term = fetchToken(ast->prep);
    token = peekToken(ast->prep);
    dst->arg3 = calloc(1, sizeof(Expr));
    if (getTokenKeywordId(token) != KW_ELSE) {
        *dst->arg3 = (Expr){
            .type = EXPR_VOID,
            .block = dst->block,
            .loc = stmt_term.loc
        };
        unfetchToken(ast->prep, stmt_term);
        return false;
    }
    fetchToken(ast->prep);
    
    token = peekToken(ast->prep);
    parseExpr(ast, dst->arg3, dst, dst->block, func, EXPRTERM_FULL);
    if (dst->arg3->type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(ast, token.loc, "`else` statement");

    return false;
}

bool parseKwWhile(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    token = peekToken(ast->prep);
    int64_t symbol_id = getTokenSymbolId(token);
    if (symbol_id == SYMBOL_ARGSPEC_START) {
        if (setExprType(ast, dst, parent_expr, EXPR_WHILE)) {
            unfetchToken(ast->prep, token);
            return true;
        }
        fetchToken(ast->prep);
        
        dst->arg1 = calloc(1, sizeof(Expr));
        parseExpr(ast, dst->arg1, dst, dst->block, func, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE | EXPRTYPE_LOGICAL);
        fetchToken(ast->prep);

        dst->arg2 = calloc(1, sizeof(Expr));
        token = peekToken(ast->prep);
        parseExpr(ast, dst->arg2, dst, dst->block, func, EXPRTERM_FULL);
        if (dst->arg2->type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(ast, token.loc, "`while` statement");
    } else {
        if (setExprType(ast, dst, parent_expr, EXPR_DOWHILE)) {
            unfetchToken(ast->prep, token);
            return true;
        }

        dst->arg1 = calloc(1, sizeof(Expr));
        parseExpr(ast, dst->arg1, dst, dst->block, func, EXPRTERM_FULL);
        if (dst->arg1->type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(ast, dst->arg1->loc, "`while` statement");
        fetchToken(ast->prep);

        token = fetchToken(ast->prep);
        if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));
        dst->arg2 = calloc(1, sizeof(Expr));
        parseExpr(ast, dst->arg2, dst, dst->block, func, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE | EXPRTYPE_LOGICAL);
        fetchToken(ast->prep);
    }

    return false;
}

bool parseKwFor(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    if (setExprType(ast, dst, parent_expr, EXPR_BLOCK)) {
        unfetchToken(ast->prep, token);
        return true;
    }
    dst->loc = token.loc;

    token = fetchToken(ast->prep);
    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));
// fetching the initializer
    initExpr(dst);
    addSubexpr(dst, (Expr){ .type = EXPR_REF, .loc = token.loc, .block = dst });
    parseExpr(ast, addSubexpr(dst, (Expr){0}), dst, dst, func, EXPRTERM_FULL | EXPRTYPE_VOIDABLE);
    fetchToken(ast->prep);
// fetching the terminator
    token = peekToken(ast->prep);
    if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_END) raiseForLoopTermExpectedError(ast, token);
    Expr* loop = addSubexpr(dst, (Expr){
        .type = EXPR_WHILE,
        .loc = token.loc,
        .block = dst,
        .arg1 = malloc(sizeof(Expr)),
        .arg2 = malloc(sizeof(Expr))
    });
    parseExpr(ast, loop->arg1, dst, dst, func, EXPRTERM_FULL | EXPRTYPE_EVALUATABLE | EXPRTYPE_LOGICAL);
    fetchToken(ast->prep);
// fetching the incrementer
    token = peekToken(ast->prep);
    if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_END) raiseForLoopIncrExpectedError(ast, token);
    *loop->arg2 = (Expr){
        .type = EXPR_BLOCK,
        .loc = token.loc,
        .block = dst
    };
    initExpr(loop->arg2);
    addSubexpr(loop->arg2, (Expr){ .type = EXPR_REF, .loc = token.loc, .block = loop->arg2 });
    Expr* iter = calloc(1, sizeof(Expr));
    parseExpr(ast, iter, loop->arg2, loop->arg2, func, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE);
    fetchToken(ast->prep);
// fetching the body
    parseExpr(ast, addSubexpr(loop->arg2, (Expr){0}), loop->arg2, loop->arg2, func, EXPRTERM_FULL);
    addSubexpr(loop->arg2, *iter);
    free(iter);

    return false;
}

bool parseInvalidKw(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    raiseInvalidExprError(ast, token.loc);
    return false;
}

typedef bool (*SymbolParser) (AST*, Token, Expr*, Expr*, FuncDecl*, int);

bool parseSymbolAssignment(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
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
            unfetchToken(ast->prep, token);
            return true;
        }
        if (flags & EXPRTYPE_EVALUATABLE && !(flags & EXPRTERM_BRACKET)) raiseUnbracketedAssignExprError(ast, token.loc);

        dst->arg2 = malloc(sizeof(Expr));
        Token entry_loc = peekToken(ast->prep);
        parseExpr(ast, dst->arg2, dst, dst->block, func, flags & EXPRTERM | EXPRTYPE_EVALUATABLE);
    }

    return false;
}

bool parseSymbolBlockStart(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;

    if (flags & EXPRTYPE_EVALUATABLE) {
        if (setExprType(ast, dst, parent_expr, EXPR_ARRAY)) {
            unfetchToken(ast->prep, token);
            return true;
        }

        if (parent_expr->type == EXPR_NEW_VAR) {
            TypeDef decl_type = *getSubexpr(parent_expr, 1)->var_type;
            if (decl_type.kind != KIND_ARRAY) raiseUnexpectedArrayInitError(ast, token.loc, decl_type);
            dst->element_type = malloc(sizeof(TypeDef));
            *dst->element_type = *decl_type.base;
        }

        initExpr(dst);
        if (getTokenSymbolId(peekToken(ast->prep)) != SYMBOL_BLOCK_END) {
            while (true) {
                parseExpr(ast, addSubexpr(dst, (Expr){0}), dst, dst->block, func, EXPRTERM_ARRAY_ARG | EXPRTYPE_EVALUATABLE);
                if (getTokenSymbolId(fetchToken(ast->prep)) == SYMBOL_BLOCK_END) break;
            }
        } else fetchToken(ast->prep);
    } else {
        if (setExprType(ast, dst, parent_expr, EXPR_BLOCK)) {
            unfetchToken(ast->prep, token);
            return true;
        }

        initExpr(dst);
        addSubexpr(dst, (Expr){
            .type = EXPR_REF,
            .block = dst,
            .loc = dst->loc
        });

        while (true) {
            token = peekToken(ast->prep);
            if (token.type == TOKEN_NONE) raiseUnexpectedTokenError(ast, token, NULL, symbolToken(SYMBOL_BLOCK_END));
            if (getTokenSymbolId(token) == SYMBOL_BLOCK_END) break;

            Expr* expr = addSubexpr(dst, (Expr){0});
            parseExpr(ast, expr, dst, dst, func, EXPRTERM_FULL);
            fetchToken(ast->prep);
        }
    }


    return false;
}

bool parseBinaryExprSymbol(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_ADD + token.symbol_id - SYMBOL_PLUS)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    dst->arg2 = calloc(1, sizeof(Expr));
    parseExpr(ast, dst->arg2, dst, dst->block, func, flags & EXPRTERM | EXPRTYPE_EVALUATABLE);

    return false;
}

bool parseSymbolTilde(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_NOT)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    dst->arg1 = calloc(1, sizeof(Expr));
    parseExpr(ast, dst->arg1, dst, dst->block, func, flags & EXPRTERM | EXPRTYPE_EVALUATABLE);

    return false;
}

bool parseSymbolArgspecStart(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (dst->type != EXPR_INVALID) {
        if (setExprType(ast, dst, parent_expr, EXPR_PROC_CALL)) {
            unfetchToken(ast->prep, token);
            return true;
        }

        FuncDecl* def = (FuncDecl*)getSubexpr(dst, 0)->arg1;
        token = peekToken(ast->prep);
        int arg_id = 0, expected_arg_count = getSubexprsCount(&def->args) - 1;

        if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_END) {
            while (true) {
                parseExpr(ast, addSubexpr(dst, (Expr){0}), dst, dst->block, func, EXPRTERM_ARG | EXPRTYPE_EVALUATABLE);
                arg_id++;
                token = fetchToken(ast->prep);
                if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_END) break;
            }
        } else fetchToken(ast->prep);

        if (arg_id != expected_arg_count) raiseArgCountMismatchError(ast, token.loc, def, arg_id);
    } else {
        dst->type = EXPR_WRAPPER;
        dst->arg1 = calloc(1, sizeof(Expr));
        parseExpr(ast, dst->arg1, dst, dst->block, func, EXPRTERM_BRACKET | EXPRTYPE_EVALUATABLE);
        fetchToken(ast->prep);
    }

    return false;
}

bool parseComparisonOperatorSymbol(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    static ExprType symbol_to_expr[N_SYMBOLS] = {
        [SYMBOL_EQ] = EXPR_LOGICAL_EQ,
        [SYMBOL_NEQ] = EXPR_LOGICAL_NEQ,
        [SYMBOL_LT] = EXPR_LOGICAL_LT,
        [SYMBOL_GT] = EXPR_LOGICAL_GT,
        [SYMBOL_LE] = EXPR_LOGICAL_LE,
        [SYMBOL_GE] = EXPR_LOGICAL_GE
    };

    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, symbol_to_expr[token.symbol_id])) {
        unfetchToken(ast->prep, token);
        return true;
    }

    dst->arg2 = calloc(1, sizeof(Expr));
    token = peekToken(ast->prep);
    parseExpr(ast, dst->arg2, dst, dst->block, func, flags & EXPRTERM | EXPRTYPE_EVALUATABLE);

    return false;
}

bool parseSymbolAmpersand(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    if (dst->type == EXPR_INVALID) {
        dst->loc = token.loc;
        if (setExprType(ast, dst, parent_expr, EXPR_GET_REF)) {
            unfetchToken(ast->prep, token);
            return true;
        }

        dst->arg1 = calloc(1, sizeof(Expr));
        parseExpr(ast, dst->arg1, dst, dst->block, func, flags & EXPRTERM | EXPRTYPE_EVALUATABLE);
    } else return parseBinaryExprSymbol(ast, token, dst, parent_expr, func, flags);

    return false;
}

bool parseSymbolStar(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    if (dst->type == EXPR_INVALID) {
        dst->loc = token.loc;
        if (setExprType(ast, dst, parent_expr, EXPR_DEREF)) {
            unfetchToken(ast->prep, token);
            return true;
        }

        dst->arg1 = calloc(1, sizeof(Expr));
        parseExpr(ast, dst->arg1, dst, dst->block, func, flags & EXPRTERM | EXPRTYPE_EVALUATABLE);
    } else return parseBinaryExprSymbol(ast, token, dst, parent_expr, func, flags);

    return false;
}

bool parseSymbolIndexStart(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_GET_ITEM)) {
        unfetchToken(ast->prep, token);
        return true;
    }

    dst->arg2 = calloc(1, sizeof(Expr));
    parseExpr(ast, dst->arg2, dst, dst->block, func, EXPRTERM_SQBRACKET | EXPRTYPE_EVALUATABLE);
    fetchToken(ast->prep);
    return false;
}

bool parseInvalidSymbol(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    raiseInvalidExprError(ast, token.loc);
    return false;
}

bool parseWord(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    dst->loc = token.loc;
    if (setExprType(ast, dst, parent_expr, EXPR_NAME)) {
        unfetchToken(ast->prep, token);
        return true;
    }
    dst->name = token.word;

    return false;
}

bool parseEmptyToken(AST* ast, Token token, Expr* dst, Expr* parent_expr, FuncDecl* func, int flags)
{
    raiseInvalidExprError(ast, token.loc);
    return false;
}

void parseExpr(AST* ast, Expr* dst, Expr* parent_expr, Expr* block, FuncDecl* func, int flags)
{
    static_assert(N_EXPR_TYPES == 54, "not all expression types are handled in parseExpr");
    TypeDef new_type;
    Token token;

    dst->block = block;
    while (true) {
        token = peekToken(ast->prep);
        if (isExprTerm(ast, token, dst, flags)) break;

        if (parseType(ast, &new_type)) {
            parseVarDecl(ast, token, new_type, dst, parent_expr, func, flags);
            continue;
        }
        
        fetchToken(ast->prep);
        if (token.type == TOKEN_INT) {
            if (parseIntLiteral(ast, token, dst, parent_expr, func, flags)) break;
        } else if (token.type == TOKEN_STRING) {
            if (parseStrLiteral(ast, token, dst, parent_expr, func, flags)) break;
        } else if (token.type == TOKEN_KEYWORD) {
            static KwParser kw_parsers[] = {
                [KW_VOID] = &parseInvalidKw,
                [KW_INT8] = &parseInvalidKw,
                [KW_INT16] = &parseInvalidKw,
                [KW_INT32] = &parseInvalidKw,
                [KW_INT64] = &parseInvalidKw,
                [KW_SYS] = &parseKwSys,
                [KW_BUILTIN] = &parseKwBuiltin,
                [KW_RETURN] = &parseKwReturn,
                [KW_CAST] = &parseKwCast,
                [KW_BOOL] = &parseInvalidKw,
                [KW_AND] = &parseKwAndOr,
                [KW_OR] = &parseKwAndOr,
                [KW_NOT] = &parseKwNot,
                [KW_IF] = &parseKwIf,
                [KW_ELSE] = &parseInvalidKw,
                [KW_WHILE] = &parseKwWhile,
                [KW_FOR] = &parseKwFor
            };
            if (kw_parsers[token.keyword_id](ast, token, dst, parent_expr, func, flags)) break;
        } else if (token.type == TOKEN_SYMBOL) {
            static SymbolParser symbol_parsers[] = {
                [SYMBOL_ARGSPEC_START] = &parseSymbolArgspecStart,
                [SYMBOL_ARGSPEC_END] = &parseInvalidSymbol,
                [SYMBOL_COMMA] = &parseInvalidSymbol,
                [SYMBOL_BLOCK_START] = &parseSymbolBlockStart,
                [SYMBOL_BLOCK_END] = &parseInvalidSymbol,
                [SYMBOL_SEMICOLON] = &parseInvalidSymbol,
                [SYMBOL_EQ] = &parseComparisonOperatorSymbol,
                [SYMBOL_NEQ] = &parseComparisonOperatorSymbol,
                [SYMBOL_LE] = &parseComparisonOperatorSymbol,
                [SYMBOL_GE] = &parseComparisonOperatorSymbol,
                [SYMBOL_ASSIGNMENT] = &parseSymbolAssignment,
                [SYMBOL_PLUS] = &parseBinaryExprSymbol,
                [SYMBOL_MINUS] = &parseBinaryExprSymbol,
                [SYMBOL_STAR] = &parseSymbolStar,
                [SYMBOL_DIV] = &parseBinaryExprSymbol,
                [SYMBOL_MOD] = &parseBinaryExprSymbol,
                [SYMBOL_AMPERSAND] = &parseSymbolAmpersand,
                [SYMBOL_PIPE] = &parseBinaryExprSymbol,
                [SYMBOL_CARET] = &parseBinaryExprSymbol,
                [SYMBOL_LSHIFT] = &parseBinaryExprSymbol,
                [SYMBOL_RSHIFT] = &parseBinaryExprSymbol,
                [SYMBOL_TILDE] = &parseSymbolTilde,
                [SYMBOL_LT] = &parseComparisonOperatorSymbol,
                [SYMBOL_GT] = &parseComparisonOperatorSymbol,
                [SYMBOL_ADD_ASSIGN] = &parseSymbolAssignment,
                [SYMBOL_SUB_ASSIGN] = &parseSymbolAssignment,
                [SYMBOL_MUL_ASSIGN] = &parseSymbolAssignment,
                [SYMBOL_DIV_ASSIGN] = &parseSymbolAssignment,
                [SYMBOL_AND_ASSIGN] = &parseSymbolAssignment,
                [SYMBOL_XOR_ASSIGN] = &parseSymbolAssignment,
                [SYMBOL_OR_ASSIGN ] = &parseSymbolAssignment,
                [SYMBOL_SHL_ASSIGN] = &parseSymbolAssignment,
                [SYMBOL_SHR_ASSIGN] = &parseSymbolAssignment,
                [SYMBOL_INDEX_START] = &parseSymbolIndexStart,
                [SYMBOL_INDEX_END] = &parseInvalidSymbol
            };
            if (symbol_parsers[token.symbol_id](ast, token, dst, parent_expr, func, flags)) break;
        } else if (token.type == TOKEN_WORD) {
            if (parseWord(ast, token, dst, parent_expr, func, flags)) break;
        } else if (token.type == TOKEN_NONE) {
            if (parseEmptyToken(ast, token, dst, parent_expr, func, flags)) break;
        } else
            reportCompilerBug("invalid token type %hhd", token.type);
    }

    if (dst->type == EXPR_NAME) (void)setExprType(ast, dst, parent_expr, EXPR_GET_VAR);
    if (dst->type == EXPR_INVALID && flags & EXPRTYPE_VOIDABLE) dst->type = EXPR_VOID;
    if (!isExprEvaluatable(dst->type) && flags & EXPRTYPE_EVALUATABLE) raiseNoValueExprError(ast, token.loc);
}

void removeExprWrappers(Expr* expr)
{
    if (expr->type == EXPR_WRAPPER) {
        Expr new_expr = *expr->arg1;
        free(expr->arg1);
        *expr = new_expr;
    }

    switch (expr_arity_table[expr->type]) {
        case VARIADIC:
            chain_foreach_from(Expr, subexpr, *getSubexprs(expr), 1, 
                removeExprWrappers(&_subexpr->value);
            );
            return;
        case NULLARY:
            return;
        case TERNARY:
            removeExprWrappers(expr->arg3);
        case BINARY:
            removeExprWrappers(expr->arg2);
        case UNARY:
            if (expr->arg1) removeExprWrappers(expr->arg1);
            return;
        default:
            reportCompilerBug("expression type %d has unknown arity type %d", expr->type, expr_arity_table[expr->type]);
    }
}

void makeLogicalExpr(Expr* expr)
{
    static bool expr_logicality_table[] = {
        [0 ... N_EXPR_TYPES - 1] = false,
        [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_NOT] = true,
    };
    if (!expr_logicality_table[expr->type]) {
        Expr* new_subexpr = malloc(sizeof(Expr));
        *new_subexpr = *expr;
        expr->type = EXPR_CAST;
        expr->arg1 = new_subexpr;
        expr->var_type = malloc(sizeof(TypeDef));
        *expr->var_type = BOOL_TYPE;
    }
}

void optimizeExpr(AST* ast, Expr* expr, FuncDecl* func)
{
    static_assert(N_EXPR_TYPES == 54, "not all expressions have any optimizations defined");
    switch (expr->type) {
        case EXPR_SYSCALL:
            for (ExprNode* iter = getSubexprs(expr)->start->next; iter != NULL; iter = iter->next) {
                optimizeExpr(ast, &iter->value, func);
                if (!matchType(ast, BUILTIN_VAL_TYPE, &iter->value))
                    raiseArgTypeMismatchError(ast, iter->value.loc, NULL, NULL, getExprValueType(ast, iter->value));
            }
            break;
        case EXPR_SUB_ASSIGN:
        case EXPR_ADD_ASSIGN:
        case EXPR_MUL_ASSIGN:
        case EXPR_DIV_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN: {
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
        } case EXPR_ASSIGN:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
            if (!matchType(ast, getExprValueType(ast, *expr->arg1), expr->arg2))
                raiseAssignmentTypeMismatchError(ast, expr->loc, expr->arg1->arg1, getExprValueType(ast, *expr->arg2));
            break;
        case EXPR_BLOCK:
            chain_foreach_from(Expr, subexpr, *getSubexprs(expr), 1, 
                optimizeExpr(ast, &_subexpr->value, func);
            );
            break;
        case EXPR_RETURN:
            optimizeExpr(ast, expr->arg1, func);
            if (!matchType(ast, func->return_type, expr->arg1))
                raiseReturnTypeMismatchError(ast, expr->loc, getExprValueType(ast, *expr->arg1), func);
            break;
        case EXPR_PROC_CALL: {
            ExprNode* arg_iter = getSubexprs(expr)->start;
            FuncDecl* decl = (FuncDecl*)arg_iter->value.arg1;
            arg_iter = arg_iter->next;
 
            ExprNode* decl_iter = getSubexprs(&decl->args)->start->next;

            while (decl_iter != NULL) {
                optimizeExpr(ast, &arg_iter->value, func);
                if (!matchType(ast, *getSubexpr(&decl_iter->value, 1)->var_type, &arg_iter->value))
                    raiseArgTypeMismatchError(ast, arg_iter->value.loc, decl, &decl_iter->value, getExprValueType(ast, arg_iter->value));
                decl_iter = decl_iter->next;
                arg_iter = arg_iter->next;
            }
        } break;
        case EXPR_ADD:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
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
            break;
        case EXPR_SUB:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
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
            break;
        case EXPR_MUL:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
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
            break;
        case EXPR_DIV:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
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
            break;
        case EXPR_MOD:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
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
            break;
        case EXPR_AND:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
            if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
                int64_t res = expr->arg1->int_literal & expr->arg2->int_literal;
                free(expr->arg1);
                free(expr->arg2);
                expr->type = EXPR_INT;
                expr->int_literal = res;
            }
            break;
        case EXPR_OR:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
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
            break;
        case EXPR_XOR:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
            if (expr->arg1->type == EXPR_INT && expr->arg2->type == EXPR_INT) {
                int64_t res = expr->arg1->int_literal ^ expr->arg2->int_literal;
                free(expr->arg1);
                free(expr->arg2);
                expr->type = EXPR_INT;
                expr->int_literal = res;
            }
            break;
        case EXPR_SHL:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
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
            break;
        case EXPR_SHR:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
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
            break;
        case EXPR_NOT:
            optimizeExpr(ast, expr->arg1, func);
            if (expr->arg1->type == EXPR_INT) {
                int64_t int_literal = ~expr->arg1->int_literal;
                free(expr->arg1);
                expr->int_literal = int_literal;
                expr->type = EXPR_INT;
            }
            break;
        case EXPR_CAST:
        case EXPR_LOGICAL_NOT:
            optimizeExpr(ast, expr->arg1, func);
            break;
        case EXPR_DEREF:
            optimizeExpr(ast, expr->arg1, func);
            if (!isPtrType(getExprValueType(ast, *expr->arg1))) raiseUndereferrableExprError(ast, expr->loc, getExprValueType(ast, *expr->arg1));
            break;
        case EXPR_GET_REF:
            optimizeExpr(ast, expr->arg1, func);
            if (expr->arg1->type == EXPR_DEREF) {
                Expr* new_expr = expr->arg1->arg1;
                free(expr->arg1);
                expr->arg1 = new_expr;
            } else if (expr->arg1->type != EXPR_GET_VAR) raiseUnreferrableExprError(ast, expr->loc, getExprValueType(ast, *expr->arg1));
            break;
        case EXPR_LOGICAL_EQ:
        case EXPR_LOGICAL_NEQ:
        case EXPR_LOGICAL_LT:
        case EXPR_LOGICAL_GT:
        case EXPR_LOGICAL_LE:
        case EXPR_LOGICAL_GE:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
            if (!matchType(ast, getExprValueType(ast, *expr->arg1), expr->arg2))
                raiseComparisonTypeMismatchError(ast, expr->loc, getExprValueType(ast, *expr->arg2), getExprValueType(ast, *expr->arg1));
            break;
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
            break;
        case EXPR_WHILE:
        case EXPR_DOWHILE:
        case EXPR_IF:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);
            if (expr->arg3) optimizeExpr(ast, expr->arg3, func);

            makeLogicalExpr(expr->type == EXPR_DOWHILE ? expr->arg2 : expr->arg1);
            break;
        case EXPR_NEW_VAR:
            if (getSubexprsCount(expr) == 4) {
                Expr* initializer = getSubexpr(expr, 3);
                optimizeExpr(ast, initializer, func);
                if (!matchType(ast, *getSubexpr(expr, 1)->var_type, initializer))
                    raiseAssignmentTypeMismatchError(ast, expr->loc, expr, getExprValueType(ast, *initializer));
            }
            break;
        case EXPR_GET_ITEM:
            optimizeExpr(ast, expr->arg1, func);
            optimizeExpr(ast, expr->arg2, func);

            TypeDef array_type = getExprValueType(ast, *expr->arg1);
            if (!isPtrType(array_type)) raiseUnindexableExprError(ast, expr->arg1->loc, array_type);

            TypeDef index_type = getExprValueType(ast, *expr->arg2);
            if (!isIntType(index_type)) raiseInvalidArrayIndexError(ast, expr->arg2->loc, index_type);

            if (isExprIntLiteral(expr->arg2) && array_type.kind == KIND_ARRAY && array_type.n_items > 0) {
                int64_t index_literal = getIntLiteral(expr->arg2);
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
                optimizeExpr(ast, expr->arg1, func);
            }
            break;
        case EXPR_ARRAY: {
            int index = 0;
            chain_foreach(Expr, item, *getSubexprs(expr),
                optimizeExpr(ast, &_item->value, func);
                if (!matchType(ast, *expr->element_type, &_item->value))
                    raiseElementTypeMismatchError(
                        ast, item.loc, index, *expr->element_type, getExprValueType(ast, item)
                    );
                index++;
            );
        }
        case EXPR_GET_VAR:
        case EXPR_INT:
        case EXPR_BUILTIN:
        case EXPR_STRING:
        case EXPR_VOID:
            break;
        default:
            reportCompilerBug("expression with no defined optimizations: %d", expr->type);
    }
}

bool validateReturnStmt(Expr expr)
{
    switch (expr.type) {
        case EXPR_BLOCK:
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), 
                if (validateReturnStmt(subexpr)) return true;
            );
            return false;
        case EXPR_RETURN:
            return true;
        case EXPR_IF:
            if (expr.arg3) return validateReturnStmt(*expr.arg2) && validateReturnStmt(*expr.arg3);
        default:
            return false;
    }
}

void parseSourceCode(BRP* obj, AST* dst) // br -> temporary AST
{
    Token token;
    dst->prep = obj;
    dst->functions = FuncDeclChain_new(0);
    dst->globals.type = EXPR_BLOCK;
    dst->globals.block = NULL;
    initExpr(&dst->globals);
    addSubexpr(&dst->globals, (Expr){ .type = EXPR_REF });

    while (true) {
        TypeDef type;
        if (!parseType(dst, &type)) type = VOID_TYPE;
// fetching function name
        Token func_name_spec = fetchToken(obj);
        if (func_name_spec.type == TOKEN_WORD) {
            token = fetchToken(obj);
            FuncDecl* new_func = FuncDeclChain_append(
                &dst->functions,
                (FuncDecl){
                    .return_type = type,
                    .name = tostr(SBUF(func_name_spec.word)),
                    .loc = func_name_spec.loc,
                    .args = (Expr){
                        .type = EXPR_BLOCK,
                        .block = &dst->globals,
                        .loc = token.loc
                    },
                }
            );
            initExpr(&new_func->args);
            Expr* arg_block_ref = addSubexpr(&new_func->args, (Expr){
                .type = EXPR_REF,
                .block = &new_func->args,
                .arg1 = NULL,
                .loc = new_func->args.loc
            });
    // fetching function prototype specification
            if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) raiseUnexpectedTokenError(dst, token, NULL, symbolToken(SYMBOL_ARGSPEC_START));
            while (true) {
                Token type_spec = peekToken(obj);
                if (parseType(dst, &type)) {
                    if (type.kind == KIND_ARRAY) type.kind = KIND_PTR;
                    token = fetchToken(obj);
                    if (token.type != TOKEN_WORD) raiseUnexpectedTokenError(dst, token, "an argument name", wordToken(NULL));

                    Expr* new_arg = addSubexpr(&new_func->args, (Expr){
                        .type = EXPR_NEW_VAR,
                        .block = &new_func->args,
                        .loc = type_spec.loc
                    });
                    initExpr(new_arg);
                    addSubexpr(new_arg, (Expr){
                        .type = EXPR_NAME,
                        .block = &new_func->args,
                        .name = token.word,
                        .loc = token.loc
                    });
                    Expr* new_arg_type_spec = addSubexpr(new_arg, (Expr){
                        .type = EXPR_TYPE,
                        .block = &new_func->args,
                        .var_type = malloc(sizeof(TypeDef)),
                        .loc = new_arg->loc
                    });
                    *new_arg_type_spec->var_type = type;
                    Expr* _tmp = addSubexpr(new_arg, (Expr){
                        .type = EXPR_REF,
                        .block = &new_func->args,
                        .arg1 = arg_block_ref->arg1,
                        .loc = new_arg->loc
                    });
                    arg_block_ref->arg1 = new_arg;
                } else {
                    token = fetchToken(obj);
                    if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_END) {
                        break;
                    } else if (getTokenSymbolId(token) != SYMBOL_COMMA)
                        raiseUnexpectedTokenError(dst, token, NULL, symbolToken(SYMBOL_COMMA), symbolToken(SYMBOL_ARGSPEC_END));
                }
            }

            if (sbufeq(new_func->name, "main")) {
                if (new_func->return_type.kind != KIND_VOID) raiseMainProcRetTypeMismatchError(dst, func_name_spec.loc, new_func->return_type);
                int main_proc_n_args = getSubexprsCount(&new_func->args) - 1;
                if (main_proc_n_args != 0) raiseMainProcArgCountMismatchError(dst, token.loc, main_proc_n_args);
            }
    // parsing the function body
            token = peekToken(obj);
            if (getTokenSymbolId(token) != SYMBOL_SEMICOLON) {
                parseExpr(dst, &new_func->body, NULL, &new_func->args, new_func, EXPRTERM_FULL);
                if (new_func->body.type == EXPR_NEW_VAR) raiseVarDeclAsStmtBodyError(dst, token.loc, "a function definition");
                fetchToken(obj);
                removeExprWrappers(&new_func->body);
                optimizeExpr(dst, &new_func->body, new_func);
                if (new_func->return_type.kind != KIND_VOID) {
                    if (!validateReturnStmt(new_func->body)) raiseNoProcReturnError(dst, new_func);
                }
            } else fetchToken(obj);
        } else if (func_name_spec.type == TOKEN_NONE) {
            break;
        } else raiseInvalidGlobalStmtError(dst, func_name_spec);
    }
}

typedef uint8_t regstate_t;
typedef struct {
    sbufArray data_blocks;
    intArray mem_blocks;
    sbuf cur_src_path;
    int cur_src_line;
    regstate_t arg_cache_state;
    AST* ast;
    FILE* dst;
} ASTCompilerCtx;

typedef void (*ExprCompiler) (ASTCompilerCtx*, Expr, regstate_t, int);
ExprCompiler expr_compilers[N_EXPR_TYPES];

static int reg_cache_counter = 0;
int compileRegCaching(ASTCompilerCtx* ctx, regstate_t state)
{
    for (uint8_t i = 0; i < 8; i++) {
        if (state & (1 << i)) {
            fprintf(ctx->dst, "\tpushv .rc%d_%hhd 8 r%hhd\n", reg_cache_counter, i, i);
        }
    }

    return reg_cache_counter++;
}

void compileRegUncaching(ASTCompilerCtx* ctx, regstate_t state, int cache_id)
{
    for (int8_t i = 7; i >= 0; i--) {
        if (state & (1 << i)) {
            fprintf(ctx->dst, "\tpopv r%hhd\n", i);
        }
    }
}

void getRegister(ASTCompilerCtx* ctx, regstate_t* reg_state, int excluded, int* reg_id, int* cache_id)
{
    if (~(*reg_state) == 0) {
        *reg_id = (excluded + 1) % N_USER_REGS;
        *reg_state &= ~(1 << *reg_id);
        *cache_id = compileRegCaching(ctx, 1 << *reg_id);
    } else {
        *cache_id = -1;
        for (int i = 0; i < 8; i++) {
            if ((*reg_state & (1 << i)) == 0 && i != excluded) {
                *reg_id = i;
                break;
            }
        }
    }
}

void freeRegister(ASTCompilerCtx* ctx, regstate_t* reg_state, int reg_id, int cache_id)
{
    if (cache_id >= 0) {
        *reg_state |= 1 << reg_id;
        compileRegUncaching(ctx, 1 << reg_id, cache_id);
    }
}

void compileSrcRef(ASTCompilerCtx* ctx, TokenLoc loc)
{
    sbuf path_s = SBUF((char*)loc.src_name);
    if (!sbufeq(ctx->cur_src_path, path_s)) {
        fprintf(
            ctx->dst, 
            "\t@f \"%.*s\"\n"
            "\t@l %d\n",
            unpack(path_s), loc.lineno
        );
        ctx->cur_src_path = path_s;
        ctx->cur_src_line = loc.lineno;
    } else if (loc.lineno != ctx->cur_src_line) {
        fprintf(ctx->dst, "\t@l %d\n", loc.lineno);
        ctx->cur_src_line = loc.lineno;
    }
}

void compileExprInvalid(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    reportCompilerBug("uncompilable expression type %d", expr.type);
}

void compileExprSyscall(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    regstate_t reg_cache_state = ((1 << (getSubexprsCount(&expr) - 1)) - 1 | 1) & reg_state;
    int cache_id = compileRegCaching(ctx, reg_cache_state);
    int i = 0;
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        expr_compilers[subexpr.type](ctx, subexpr, (1 << i) - 1, i);
        i++;
    );
    fprintf(ctx->dst, "\tsys %s\n", getSubexpr(&expr, 0)->name);

    if (dst_reg != 0) fprintf(ctx->dst, "\tsetr r%d r0\n", dst_reg);
    compileRegUncaching(ctx, reg_cache_state, cache_id);

}

void compileExprBuiltin(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    fprintf(ctx->dst, "\tsetb r%d %s\n", dst_reg, expr.name);
}

void compileExprString(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    int str_index = -1;
    for (int i = 0; i < ctx->data_blocks.length; i++) {
        if (sbufeq(ctx->data_blocks.data[i], expr.string)) {
            str_index = i;
            break;
        }
    }
    if (str_index < 0) {
        str_index = ctx->data_blocks.length;
        sbufArray_append(&ctx->data_blocks, expr.string);
    }

    fprintf(ctx->dst, "\tsetd r%d "STR_PREFIX"%d\n", dst_reg, str_index);
}

void compileExprInt(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    fprintf(ctx->dst, "\tset r%d %lld\n", dst_reg, expr.int_literal);
}

static int array_init_counter;

void compileExprNewVar(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    char* name = getSubexpr(&expr, 0)->name;
    TypeDef var_type = *getSubexpr(&expr, 1)->var_type;
    int size = getTypeSize(var_type);

    if (getSubexprsCount(&expr) == 4) {
        Expr* initializer = getSubexpr(&expr, 3);
        if (initializer->type == EXPR_ARRAY) {
            int element_size = getTypeSize(*var_type.base), initializer_size = getSubexprsCount(initializer), array_size = element_size * var_type.n_items;
            if (initializer_size) {
                int element_reg, cache_id;
                getRegister(ctx, &reg_state, dst_reg, &element_reg, &cache_id);

                fprintf(ctx->dst, "\tvar %s %d\n", name, array_size);
                fprintf(ctx->dst, "\tsetv r%d %s\n", dst_reg, name);

                if (initializer_size == 1) {
                    int iter_reg, iter_cache_id;
                    getRegister(ctx, &reg_state, dst_reg, &iter_reg, &cache_id);

                    initializer = getSubexpr(initializer, 0);
                    expr_compilers[initializer->type](ctx, *initializer, reg_state | (1 << dst_reg) | (1 << iter_reg), element_reg);

                    fprintf(ctx->dst, "\tset r%d %d\n", iter_reg, var_type.n_items);
                    fprintf(ctx->dst, "\tmark .ai%d\n", ++array_init_counter);
                    fprintf(ctx->dst, "\tstr%d r%d r%d\n", element_size * 8, dst_reg, element_reg);
                    fprintf(ctx->dst, "\tadd r%d r%d %d\n", dst_reg, dst_reg, element_size);
                    fprintf(ctx->dst, "\tsub r%d r%d 1\n", iter_reg, iter_reg);
                    fprintf(ctx->dst, "\tcmp r%d 0\n", iter_reg);
                    fprintf(ctx->dst, "\tgoto:neq .ai%d\n", array_init_counter);

                    freeRegister(ctx, &reg_state, iter_reg, iter_cache_id);
                } else {
                    chain_foreach(Expr, element, *getSubexprs(initializer),
                        expr_compilers[element.type](ctx, element, reg_state | (1 << dst_reg), element_reg);
                        fprintf(ctx->dst, "\tstr%d r%d r%d\n", element_size, dst_reg, element_reg);
                        fprintf(ctx->dst, "\tadd r%d r%d %d\n", dst_reg, dst_reg, element_size);
                    );
                }

                freeRegister(ctx, &reg_state, element_reg, cache_id);
            } else {
                if (array_size <= 8) {
                    fprintf(ctx->dst, "\tpushv %s %d rZ\n", name, array_size);
                } else {
                    int iter_reg, cache_id, step = 1;
                    getRegister(ctx, &reg_state, dst_reg, &iter_reg, &cache_id);

                    fprintf(ctx->dst, "\tvar %s %d\n", name, array_size);
                    fprintf(ctx->dst, "\tsetv r%d %s\n", dst_reg, name);

                    if (array_size % 8 == 0) {
                        step = 8;
                    } else if (array_size % 4 == 0) {
                        step = 4;
                    } else if (array_size % 2 == 0) {
                        step = 2;
                    }

                    fprintf(
                        ctx->dst,
                        "\tset r%1$d %2$d\n"
                        "\tmark .ai%6$d\n"
                        "\tstr%3$d r%4$d rZ\n"
                        "\tadd r%4$d r%4$d %5$d\n"
                        "\tsub r%1$d r%1$d %5$d\n"
                        "\tcmp r%1$d 0\n"
                        "\tgoto:neq .ai%6$d\n",
                        iter_reg, array_size,
                        step * 8, dst_reg, step, ++array_init_counter
                    );

                    freeRegister(ctx, &reg_state, iter_reg, cache_id);
                }
            }
        } else {
            expr_compilers[initializer->type](ctx, *initializer, reg_state, dst_reg);
            fprintf(ctx->dst, "\tpushv %s %d r%d\n", name, size, dst_reg);
        }
    } else {
        fprintf(ctx->dst, "\tvar %s %d\n", name, var_type.kind == KIND_ARRAY ? var_type.n_items * getTypeSize(*var_type.base) : size);
    }
}

void compileExprAssign(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    int field_size = getTypeSize(getExprValueType(ctx->ast, *expr.arg2));
    expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
    if (expr.arg1->type == EXPR_GET_VAR) {
        fprintf(ctx->dst, "\tstrv %s r%d\n", getSubexpr(expr.arg1->arg1, 0)->name, dst_reg);
    } else if (expr.arg1->type == EXPR_DEREF) {
        int ptr_reg, cache_id;
        getRegister(ctx, &reg_state, dst_reg, &ptr_reg, &cache_id);

        expr_compilers[expr.arg1->arg1->type](ctx, *expr.arg1->arg1, reg_state | (1 << dst_reg), ptr_reg);
        fprintf(ctx->dst, "\tstr%d r%d r%d\n", field_size * 8, ptr_reg, dst_reg);

        freeRegister(ctx, &reg_state, ptr_reg, cache_id);
    }
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

void compileExprGetVar(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    if (getSubexpr(expr.arg1, 1)->var_type->kind == KIND_ARRAY) {
        fprintf(ctx->dst, "\tsetv r%d %s\n", dst_reg, getSubexpr(expr.arg1, 0)->name);
    } else {
        fprintf(ctx->dst, "\tldvs r%d %s\n", dst_reg, getSubexpr(expr.arg1, 0)->name);
    }
}

void compileExprBlock(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    int n_vars = 0;
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        expr_compilers[subexpr.type](ctx, subexpr, reg_state, 0);
        if (subexpr.type == EXPR_NEW_VAR) n_vars++;
    );

// the following line explicitly deallocates local variables if the block is not a function definition
    if (expr.block->block->block != NULL && n_vars > 0) fprintf(ctx->dst, "\tdelnv %d\n", n_vars);
}

void compileExprProcCall(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    regstate_t reg_cache_state = ((1 << (getSubexprsCount(&expr) - 1)) - 1 | 1) & reg_state;
    int cache_id = compileRegCaching(ctx, reg_cache_state);
    int i = 0;
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        expr_compilers[subexpr.type](ctx, subexpr, (1 << i) - 1, i);
        i++;
    );
    fprintf(ctx->dst, "\tcall %s\n", ((FuncDecl*)getSubexpr(&expr, 0)->arg1)->name);

    if (dst_reg != 0) fprintf(ctx->dst, "\tsetr r%d r0\n", dst_reg);
    compileRegUncaching(ctx, reg_cache_state, cache_id);
}

void compileExprReturn(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, 0);
    fprintf(ctx->dst, "\tret\n");
}

void compileExprAdd(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprSub(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprMul(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprDiv(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprMod(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprAnd(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprOr(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprXor(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprShl(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprShr(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void compileExprNot(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
    fprintf(ctx->dst, "\tnot r%d r%d\n", dst_reg, dst_reg);
}

void _compileExprLogicalEq(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void _compileExprLogicalNeq(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void _compileExprLogicalLt(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void _compileExprLogicalGt(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void _compileExprLogicalLe(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void _compileExprLogicalGe(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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

void _compileExprLogicalAnd(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    int arg2_dst_reg, cache_id;
    getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

    expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
    expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
    fprintf(ctx->dst, "\tcmp r%d 0\n\tcmp:neq r%d 0\n\tsetc r%d neq\n", dst_reg, arg2_dst_reg, dst_reg);

    freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
}

void _compileExprLogicalOr(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    int arg2_dst_reg, cache_id;
    getRegister(ctx, &reg_state, dst_reg, &arg2_dst_reg, &cache_id);

    expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
    expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | (1 << dst_reg), arg2_dst_reg);
    fprintf(ctx->dst, "\tcmp r%d 0\n\tcmp:equ r%d 0\n", dst_reg, arg2_dst_reg);

    freeRegister(ctx, &reg_state, arg2_dst_reg, cache_id);
}

void _compileExprLogicalNot(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
    fprintf(ctx->dst, "\tcmp r%d 0\n", dst_reg);
}

void compileLogicalExpr(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
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
            reportCompilerBug("invalid expression type %d", expr.type);
    }
}

static int block_counter = 0;

void compileExprIf(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    int block_id = block_counter++;

    switch (expr.arg1->type) {
        case EXPR_LOGICAL_EQ:
            _compileExprLogicalEq(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:neq .else%d\n", block_id);
            break;
        case EXPR_LOGICAL_NEQ:
            _compileExprLogicalNeq(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:equ .else%d\n", block_id);
            break;
        case EXPR_LOGICAL_LT:
            _compileExprLogicalLt(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:ges .else%d\n", block_id);
            break;
        case EXPR_LOGICAL_GT:
            _compileExprLogicalGt(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:les .else%d\n", block_id);
            break;
        case EXPR_LOGICAL_LE:
            _compileExprLogicalLe(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:gts .else%d\n", block_id);
            break;
        case EXPR_LOGICAL_GE:
            _compileExprLogicalGe(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:lts .else%d\n", block_id);
            break;
        case EXPR_LOGICAL_AND:
            _compileExprLogicalAnd(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:equ .else%d\n", block_id);
            break;
        case EXPR_LOGICAL_OR:
            _compileExprLogicalOr(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:equ .else%d\n", block_id);
            break;
        case EXPR_LOGICAL_NOT:
            _compileExprLogicalNot(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:neq .else%d\n", block_id);
            break;
        case EXPR_CAST:
            if (expr.arg1->var_type->kind == KIND_BOOL) {
                expr_compilers[expr.arg1->arg1->type](ctx, *expr.arg1->arg1, reg_state, dst_reg);
                fprintf(ctx->dst, "\tcmp r%d 0\n\tgoto:equ .else%d\n", dst_reg, block_id);
                break;
            }
        default:
            reportCompilerBug("invalid expression type %d", expr.arg1->type);
    }
    if (expr.arg3->type != EXPR_VOID) {
        expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
        fprintf(ctx->dst, "\tgoto .end%d\n\tmark .else%d\n", block_id, block_id);
        expr_compilers[expr.arg3->type](ctx, *expr.arg3, reg_state, dst_reg);
        fprintf(ctx->dst, "\tmark .end%d\n", block_id);
    } else {
        expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
        fprintf(ctx->dst, "\tmark .else%d\n", block_id);
    }
}

void compileExprVoid(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{}

void compileExprWhile(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    int block_id = block_counter++;

    fprintf(ctx->dst, "\tmark .start%d\n", block_id);
    switch (expr.arg1->type) {
        case EXPR_LOGICAL_EQ:
            _compileExprLogicalEq(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:neq .end%d\n", block_id);
            break;
        case EXPR_LOGICAL_NEQ:
            _compileExprLogicalNeq(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:equ .end%d\n", block_id);
            break;
        case EXPR_LOGICAL_LT:
            _compileExprLogicalLt(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:ges .end%d\n", block_id);
            break;
        case EXPR_LOGICAL_GT:
            _compileExprLogicalGt(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:les .end%d\n", block_id);
            break;
        case EXPR_LOGICAL_LE:
            _compileExprLogicalLe(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:gts .end%d\n", block_id);
            break;
        case EXPR_LOGICAL_GE:
            _compileExprLogicalGe(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:lts .end%d\n", block_id);
            break;
        case EXPR_LOGICAL_AND:
            _compileExprLogicalAnd(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:equ .end%d\n", block_id);
            break;
        case EXPR_LOGICAL_OR:
            _compileExprLogicalOr(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:equ .end%d\n", block_id);
            break;
        case EXPR_LOGICAL_NOT:
            _compileExprLogicalNot(ctx, *expr.arg1, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:neq .end%d\n", block_id);
            break;
        case EXPR_CAST:
            if (expr.arg1->var_type->kind == KIND_BOOL) {
                expr_compilers[expr.arg1->arg1->type](ctx, *expr.arg1->arg1, reg_state, dst_reg);
                fprintf(ctx->dst, "\tcmp r%d 0\n\tgoto:equ .end%d\n", dst_reg, block_id);
                break;
            }
        default:
            reportCompilerBug("invalid expression type %d", expr.arg1->type);
    }

    expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg);
    fprintf(ctx->dst, "\tgoto .start%d\n\tmark .end%d\n", block_id, block_id);
}

void compileExprDoWhile(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    int block_id = block_counter++;

    fprintf(ctx->dst, "\tmark .start%d\n", block_id);
    expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);

    switch (expr.arg2->type) {
        case EXPR_LOGICAL_EQ:
            _compileExprLogicalEq(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:equ .start%d\n", block_id);
            break;
        case EXPR_LOGICAL_NEQ:
            _compileExprLogicalNeq(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:neq .start%d\n", block_id);
            break;
        case EXPR_LOGICAL_LT:
            _compileExprLogicalLt(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:lts .start%d\n", block_id);
            break;
        case EXPR_LOGICAL_GT:
            _compileExprLogicalGt(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:gts .start%d\n", block_id);
            break;
        case EXPR_LOGICAL_LE:
            _compileExprLogicalLe(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:les .start%d\n", block_id);
            break;
        case EXPR_LOGICAL_GE:
            _compileExprLogicalGe(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:ges .start%d\n", block_id);
            break;
        case EXPR_LOGICAL_AND:
            _compileExprLogicalAnd(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:neq .start%d\n", block_id);
            break;
        case EXPR_LOGICAL_OR:
            _compileExprLogicalOr(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:neq .start%d\n", block_id);
            break;
        case EXPR_LOGICAL_NOT:
            _compileExprLogicalNot(ctx, *expr.arg2, reg_state, dst_reg);
            fprintf(ctx->dst, "\tgoto:equ .start%d\n", block_id);
            break;
        case EXPR_CAST:
            if (expr.arg2->var_type->kind == KIND_BOOL) {
                expr_compilers[expr.arg2->arg1->type](ctx, *expr.arg2->arg1, reg_state, dst_reg);
                fprintf(ctx->dst, "\tcmp r%d 0\n\tgoto:neq .start%d\n", dst_reg, block_id);
                break;
            }
        default:
            reportCompilerBug("invalid expression type %d", expr.arg2->type);
    }
}

void compileExprGetRef(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    fprintf(ctx->dst, "\tsetv r%d %s\n", dst_reg, getSubexpr(expr.arg1->arg1, 0)->name);
}

void compileExprDeref(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
    fprintf(ctx->dst, "\tld%ds r%d r%d\n", getTypeSize(*getExprValueType(ctx->ast, *expr.arg1).base) * 8, dst_reg, dst_reg);
}

void compileExprCast(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    int old_type_size = getTypeSize(getExprValueType(ctx->ast, *expr.arg1));
    int new_type_size = getTypeSize(*expr.var_type);

    if (isExprIntLiteral(expr.arg1)) {
        if (expr.var_type->kind == KIND_BOOL) {
            fprintf(ctx->dst, "\tset r%d %d\n", dst_reg, getIntLiteral(expr.arg1) != 0);
        } else {
            fprintf(ctx->dst, "\tset r%d %lld\n", dst_reg, getIntLiteral(expr.arg1) & byteMask(new_type_size));
        }
    } else {
        expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg);
        if (expr.var_type->kind == KIND_BOOL) {
            fprintf(ctx->dst, "\tcmp r%d 0\n\tsetc r%d neq\n", dst_reg, dst_reg);
        } else if (new_type_size < old_type_size) {
            fprintf(ctx->dst, "\tand r%d r%d %lld\n", dst_reg, dst_reg, byteMask(new_type_size));
        }
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
    [EXPR_ASSIGN    ] = &compileExprAssign,
    [EXPR_GET_VAR    ] = &compileExprGetVar,
    [EXPR_BLOCK      ] = &compileExprBlock,
    [EXPR_TYPE       ] = &compileExprInvalid,
    [EXPR_REF        ] = &compileExprInvalid,
    [EXPR_PROC_REF   ] = &compileExprInvalid,
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
    [EXPR_ARRAY      ] = &compileExprInvalid
};
static_assert(N_EXPR_TYPES == 54, "not all expression types have corresponding compilers defined");

bool compileAST(AST* src, FILE* dst)
{
    ASTCompilerCtx ctx = {
        .data_blocks = sbufArray_new(0),
        .dst = dst
    };

    fputs("exec {\n", dst);
    chain_foreach(FuncDecl, func, src->functions,
        compileSrcRef(&ctx, func.loc);
        fprintf(dst, "\tproc %s\n", func.name);

        int i = 0;
        for (ExprNode* iter = getSubexprs(&func.args)->start->next; iter != NULL; iter = iter->next) {
            fprintf(dst, "\tpushv %s %d r%d\n", getSubexpr(&iter->value, 0)->name, getTypeSize(*getSubexpr(&iter->value, 1)->var_type), i++);
        }

        expr_compilers[func.body.type](&ctx, func.body, 0, 0);
        if (!sbufeq(func.name, "main") && func.return_type.kind == KIND_VOID) fputs("\tret\n", dst);
        fputs("\tendproc\n", dst);
    );
    fputs("}\n", dst);

    if (ctx.data_blocks.length) {
        fputs("data {\n", dst);
        array_foreach(sbuf, literal, ctx.data_blocks, 
            fprintf(dst, "\t"STR_PREFIX"%d \"", _literal);
            fputsbufesc(dst, literal, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE);
            fputs("\\0\"\n", dst);
        );
        fputs("}\n", dst);
    }

    sbufArray_clear(&ctx.data_blocks);
    return true;
}

void printUsageMsg(FILE* dst, char* program_name)
{
    fprintf(
        dst,
        "brc - Compiler for BRidge Source Code `.br` files\n"
        "usage: %s [options...] <source path>\n"
        "options:\n"
        "\t-h                     Display this message and quit\n"
        "\t-d                     Print the generated syntax tree and quit\n"
        "\t-r                     Run the program immediately after the compilation with brbx\n"
        "\t-s                     Compile silently, i.e. don't output compilation times\n"
        "\t--vbrb-output <path>   Output BRidge Assembly (`.vbrb` file) to <path>;\n"
        "\t\tif <path> is a directory, output will be at <path>/<source name>.vbrb;\n"
        "\t\tby default, BRidge Assembly is stored in a temporary file and is deleted after compilation\n"
        "\t-o <path>              Output BRidge Bytecode (`.brb` file) to <path>;\n"
        "\t\tif <path> is a directory, output will be at <path>/<source name>.brb;\n"
        "\t\tby default, BRidge Bytecode is saved at <source dir>/<source name>.brb\n",
        program_name
    );
}


#define DEBUG_OPT_TOKENS 0x1
#define DEBUG_OPT_MACROS 0x2
#define DEBUG_OPT_EXPRS 0x4

int main(int argc, char* argv[])
{
    startTimer();
    TYPE_STR_LITERAL.kind = KIND_PTR;
    TypeDef str_lit_base = (TypeDef){ .kind = KIND_INT, .size = 1 };
    TYPE_STR_LITERAL.base = &str_lit_base;

    bool run_program = false, silent = false;
    unsigned int optimization_level = 1, debug_opts = 0;
    char *input_path = NULL, *brb_output_path = NULL, *vbrb_output_path = NULL;
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
                        while (*(++argv[i])) {
                            char debug_opt = *argv[i];
                            if (debug_opt == 'T') {
                                debug_opts |= DEBUG_OPT_TOKENS;
                            } else if (debug_opt == 'M') {
                                debug_opts |= DEBUG_OPT_MACROS;
                            } else if (debug_opt == 'E') {
                                debug_opts |= DEBUG_OPT_EXPRS;
                            }
                        }
                        argv[i]--;
                        break;
                    case 'r':
                        run_program = true;
                        break;
                    case 's':
                        silent = true;
                        break;
                    case 'o':
						if (!argv[++i]) {
							eprintf("error: `-o` option specified but no executable output file path provided\n");
							return 1;
						}
						brb_output_path = argv[i];
						go_on = true;
						break;
                    case 'O':
                        if (!inRange(argv[i][1], '0', '2')) {
                            eprintf("error: invalid option `-O%c`; expected either `-O0` or `-O1`\n", argv[i][1]);
                            exit(1);
                        }
                        optimization_level = *(++argv[i]) - '0';
                        break;
                    case '-':
                        argv[i]++;
                        if (sbufeq(argv[i], "vbrb-output")) {
                            if (!argv[++i]) {
                                eprintf("error: `--vbrb-output` option specified but no path is provided\n");
                                return 1;
                            }
                            vbrb_output_path = argv[i];
                            go_on = true;
                        } else {
                            eprintf("error: unknown option `--%s`\n", argv[i]);
                            return 1;
                        }
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
    sbuf basename = fileBaseName_s(SBUF(input_path));

    if (brb_output_path) {
        if (isPathDir(brb_output_path)) {
            brb_output_path = tostr(SBUF(brb_output_path), PATHSEP, basename, CSBUF(BRB_EXT));
        }
    } else {
        brb_output_path = setFileExt(input_path, BRB_EXT);
    }

    if (vbrb_output_path)
        if (isPathDir(vbrb_output_path))
            vbrb_output_path = tostr(SBUF(vbrb_output_path), PATHSEP, basename, CSBUF(VBRB_EXT));

    char* vbrb_visual_output_path = vbrb_output_path;
    if (!vbrb_output_path)
        vbrb_visual_output_path = tostr(CSBUF("~"), setFileExt_s(SBUF(input_path), CSBUF(VBRB_EXT)));

    BRP prep;
    if (!initBRP(&prep, NULL, BRP_ESC_STR_LITERALS)) {
        eprintf("error: could not initialize the preprocessor due to memory shortage\n");
        return 1;
    }
    static_assert(N_SYMBOLS == 35, "not all symbols are handled");
    setSymbols(
        &prep,
        BRP_SYMBOL("("),
        BRP_SYMBOL(")"),
        BRP_SYMBOL(","),
        BRP_SYMBOL("{"),
        BRP_SYMBOL("}"),
        BRP_SYMBOL(";"),
        BRP_SYMBOL("+="),
        BRP_SYMBOL("-="),
        BRP_SYMBOL("*="),
        BRP_SYMBOL("/="),
        BRP_SYMBOL("&="),
        BRP_SYMBOL("^="),
        BRP_SYMBOL("|="),
        BRP_SYMBOL("<<="),
        BRP_SYMBOL(">>="),
        BRP_SYMBOL("=="),
        BRP_SYMBOL("!="),
        BRP_SYMBOL("<="),
        BRP_SYMBOL(">="),
        BRP_SYMBOL("="),
        BRP_SYMBOL("+"),
        BRP_SYMBOL("-"),
        BRP_SYMBOL("*"),
        BRP_SYMBOL("/"),
        BRP_SYMBOL("%"),
        BRP_SYMBOL("&"),
        BRP_SYMBOL("|"),
        BRP_SYMBOL("^"),
        BRP_SYMBOL("<<"),
        BRP_SYMBOL(">>"),
        BRP_SYMBOL("~"),
        BRP_SYMBOL("<"),
        BRP_SYMBOL(">"),
        BRP_SYMBOL("["),
        BRP_SYMBOL("]"),
        BRP_HIDDEN_SYMBOL(" "),
        BRP_HIDDEN_SYMBOL("\t"),
        BRP_HIDDEN_SYMBOL("\n")
    );
    static_assert(N_KWS == 17, "not all keywords are handled");
    setKeywords(
        &prep,
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
        BRP_KEYWORD("and"),
        BRP_KEYWORD("or"),
        BRP_KEYWORD("not"),
        BRP_KEYWORD("if"),
        BRP_KEYWORD("else"),
        BRP_KEYWORD("while"),
        BRP_KEYWORD("for")
    );
	setInput(&prep, input_path, fopen(input_path, "r"));

    if (debug_opts & DEBUG_OPT_TOKENS) {
        while (true) {
            Token token = fetchToken(&prep);
            if (token.type == TOKEN_NONE) return 0;
            printToken(token, &prep);
        }
    }

    if (debug_opts & DEBUG_OPT_MACROS) {
        while (true) {
            Token token = fetchToken(&prep);
            if (token.type == TOKEN_NONE) {
                array_foreach(Macro, macro, prep.macros, 
                    printTokenLoc(macro.def_loc);
                    printf("#define %s(", macro.name);
                    array_foreach(MacroArg, arg, macro.args, 
                        printf("%s%s", arg.name, _arg < macro.args.length - 1 ? ", " : "");
                    );
                    printf(") %.*s\n", unpack(macro.def));
                );
                return 0;
            } 
        }
    }

    AST ast;
    parseSourceCode(&prep, &ast);

    if (debug_opts & DEBUG_OPT_EXPRS) printAST(&ast);

    FILE *vbrb_output;
    sbuf temp_vbrb_buffer = {0};
    if (!vbrb_output_path) {
        if (!(vbrb_output = open_memstream(&temp_vbrb_buffer.data, (size_t*)&temp_vbrb_buffer.length))) {
            eprintf("error: could not open temporary file for writing VBRB output (reason: %s)\n", strerror(errno));
            return 1;
        }
    } else {
        if (!(vbrb_output = fopen(vbrb_output_path, "w"))) {
            eprintf("error: could not open file `%s` (reason: %s)\n", vbrb_output_path, strerror(errno));
            return 1;
        }
    }

    compileAST(&ast, vbrb_output);

    fclose(vbrb_output);
    if (!vbrb_output_path) {
        vbrb_output = fmemopen(temp_vbrb_buffer.data, (size_t)temp_vbrb_buffer.length, "r");
    } else {
        vbrb_output = fopen(vbrb_output_path, "r");
    }
    Module res;
    char* search_paths[] = { ".", NULL };
    VBRBError vbrb_err = compileModule(vbrb_output, vbrb_visual_output_path, &res, search_paths, 0);
    if (vbrb_err.code) {
        eputs("BRidge assembler failure; assembler output:\n\t");
        printVBRBError(stderr, vbrb_err);
        return 1;
    }
    if (optimization_level) {
        fclose(vbrb_output);
        vbrb_output = NULL;
        if (vbrb_output_path) {
            if (!(vbrb_output = fopen(vbrb_output_path, "w"))) {
                eprintf("error: could not open file `%s` (reason: %s)\n", vbrb_output_path, strerror(errno));
                return 1;
            }
        }
        optimizeModule(&res, search_paths, vbrb_output, optimization_level);
    }
    if (!silent) printf("%s -> %s in %.3f ms\n", input_path, vbrb_visual_output_path, endTimer());

    startTimer();
    FILE* brb_output = fopen(brb_output_path, "wb");
    if (!brb_output) {
        eprintf("error: could not open file `%s` for writing BRB output (reason: %s)\n", brb_output_path, strerror(errno));
        return 1;
    }
    writeModule(&res, brb_output);
    if (!silent) printf("%s -> %s in %.3f ms\n", vbrb_visual_output_path, brb_output_path, endTimer());

    fclose(brb_output);
    fclose(vbrb_output);
    delBRP(&prep);

    if (run_program) {
        char* cmd = NULL;
        asprintf(&cmd, "brbx %s", brb_output_path);
        if (!cmd) {
            printf("error: could not invoke the command to execute the program\n");
            return 1;
        }

        system(cmd);
    }
}