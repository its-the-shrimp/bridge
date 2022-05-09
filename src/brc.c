#include "brb.h"
#include "errno.h"

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
    N_KWS
} BRKeyword;

typedef enum {
    SYMBOL_ARGSPEC_START,
    SYMBOL_ARGSPEC_END,
    SYMBOL_COMMA,
    SYMBOL_BLOCK_START,
    SYMBOL_BLOCK_END,
    SYMBOL_SEMICOLON,
    SYMBOL_EQ,
    SYMBOL_NEQ,
    SYMBOL_LE,
    SYMBOL_GE,
    SYMBOL_ASSIGNMENT,
    SYMBOL_PLUS,
    SYMBOL_MINUS,
    SYMBOL_STAR,
    SYMBOL_DIV,
    SYMBOL_AMPERSAND,
    SYMBOL_PIPE,
    SYMBOL_CARET,
    SYMBOL_LSHIFT,
    SYMBOL_RSHIFT,
    SYMBOL_TILDE,
    SYMBOL_LT,
    SYMBOL_GT,
    N_SYMBOLS
} BRSymbol;

typedef enum {
    BR_ERR_NONE,
    BR_ERR_INVALID_GLOBAL_STMT,
    BR_ERR_FUNC_NAME_EXPECTED,
    BR_ERR_ARG_DEF_START_EXPECTED,
    BR_ERR_ARG_DEF_EXPECTED,
    BR_ERR_ARG_NAME_EXPECTED,
    BR_ERR_INVALID_SYSCALL_NAME,
    BR_ERR_BUILTIN_NAME_EXPECTED,
    BR_ERR_INVALID_BUILTIN_NAME,
    BR_ERR_INVALID_EXPR,
    BR_ERR_TOO_MANY_SYSCALL_ARGS,
    BR_ERR_INVALID_VAR_NAME,
    BR_ERR_NO_VALUE_EXPR,
    BR_ERR_UNCLOSED_BLOCK,
    BR_ERR_INVALID_FUNC_DEF,
    BR_ERR_UNKNOWN_VAR,
    BR_ERR_VAR_EXISTS,
    BR_ERR_INVALID_TYPE,
    BR_ERR_VAR_TYPE_MISMATCH,
    BR_ERR_VOID_VAR_DECL,
    BR_ERR_UNKNOWN_FUNC,
    BR_ERR_TOO_MANY_FUNC_ARGS, // TODO: lift the 6 function arguments limitation
    BR_ERR_RETURN_TYPE_MISMATCH,
    BR_ERR_ARG_COUNT_MISMATCH,
    BR_ERR_ARG_TYPE_MISMATCH,
    BR_ERR_CAST_TYPE_EXPECTED,
    BR_ERR_ARG_DEF_END_EXPECTED,
    BR_ERR_VOID_TYPE_CAST,
    BR_ERR_MAIN_PROC_RET_TYPE_MISMATCH,
    BR_ERR_MAIN_PROC_ARG_COUNT_MISMATCH,
    BR_ERR_COMPARISON_TYPE_MISMATCH,
    N_BR_ERRORS
} BRErrorCode;


typedef enum {
    KIND_NONE,
    KIND_INT,
    KIND_PTR,
    KIND_BUILTIN_VAL,
    KIND_VOID,
    KIND_BOOL,
    N_TYPE_KINDS
} TypeKind;

typedef struct typedef_t {
    TypeKind kind;
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
static TypeDef TYPE_STR_LITERAL;

typedef enum {
    EXPR_INVALID,
    EXPR_SYSCALL, // main, composite, evaluatable
    EXPR_NAME, // auxillary
    EXPR_BUILTIN, // main, unary, evaluatable
    EXPR_STRING, // main, unary, evaluatable
    EXPR_INT, // main, unary, evaluatable
    EXPR_NEW_VAR, // main, composite, non-evaluatable
    EXPR_SET_VAR, // main, binary, evaluatable
    EXPR_GET_VAR, // main, unary, evaluatable
    EXPR_BLOCK, // main, composite, non-evaluatable
    EXPR_TYPE, // auxillary
    EXPR_REF, // auxillary
    EXPR_FUNC_REF, // auxillary
    EXPR_FUNC_CALL, // main, composite, evaluatable
    EXPR_NEW_ARG, // main, composite, non-evaluatable
    EXPR_GET_ARG, // main, unary, evaluatable
    EXPR_SET_ARG, // main, binary, evaluatable
    EXPR_RETURN, // main, unary, non-evaluatable
    EXPR_ADD, // main, binary, evaluatable
    EXPR_SUB, // main, binary, evaluatable
    EXPR_MUL, // main, binary, evaluatable
    EXPR_DIV, // main, binary, evaluatable
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
    N_EXPR_TYPES
} ExprType;

typedef struct expr {
    union {
        struct expr* arg1;
    };
    union {
        struct expr* arg2;
        int64_t int_literal; // for EXPR_INT
        TypeDef* var_type; // for EXPR_TYPE
        char* name; // for EXPR_NAME
        char* str_literal; // for EXPR_STRING
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
    BRErrorCode code;
    Token loc;
    union {
        int n_args; // for BR_ERR_TOO_MANY_SYSCALL_ARGS and BR_ERR_TOO_MANY_FUNC_ARGS
        Expr* arg_decl; // for BR_ERR_ARG_TYPE_MISMATCH
        Expr* left_expr; // for BR_ERR_COMPARISON_TYPE_MISMATCH
    };
    union {
        FuncDecl* func; // for errors related to function calls
        Expr* var_decl; // for errors related to variable accesses
    };
    TypeDef entry_type; // for type-related errors
} BRError;

typedef struct {
    FuncDeclChain functions;
    Expr globals;
} AST;

BRError parseType(BRP* obj, TypeDef* dst)
{
    static_assert(N_TYPE_KINDS == 6, "not all type kinds are handled in parseType");
    *dst = (TypeDef){0};
    bool fetched = false;
    while (true) {
        Token token = peekToken(obj);
        if (token.type == TOKEN_KEYWORD) {
            switch (token.keyword_id) {
                case KW_INT8:
                case KW_INT16:
                case KW_INT32:
                case KW_INT64:
                    if (dst->kind != KIND_NONE) return (BRError){
                        .code = BR_ERR_INVALID_TYPE,
                        .loc = token
                    };

                    fetchToken(obj);
                    dst->kind = KIND_INT;
                    fetched = true;
                    dst->size = 1 << (token.keyword_id - KW_INT8);
                    break;
                case KW_VOID:
                    if (dst->kind != KIND_NONE) return (BRError){
                        .code = BR_ERR_INVALID_TYPE,
                        .loc = token
                    };

                    fetchToken(obj);
                    fetched = true;
                    dst->kind = KIND_VOID;
                    break;
                case KW_BOOL:
                    if (dst->kind != KIND_NONE) return (BRError){
                        .code = BR_ERR_INVALID_TYPE,
                        .loc = token
                    };

                    fetchToken(obj);
                    fetched = true;
                    dst->kind = KIND_BOOL;
                    break;
                default:
                    return (BRError){ .code = fetched ? 0 : -1 };
            }
        } else if (token.type == TOKEN_SYMBOL) {
            switch (token.symbol_id) {
                case SYMBOL_STAR:
                    if (dst->kind == KIND_NONE) return (BRError){ .code = fetched ? 0 : -1 };

                    fetchToken(obj);
                    TypeDef* new_base = malloc(sizeof(TypeDef));
                    *new_base = *dst;
                    dst->base = new_base;
                    dst->kind = KIND_PTR;
                    break;
                default:
                    return (BRError){ .code = fetched ? 0 : -1 };
            }
        } else return (BRError){ .code = fetched ? 0 : -1 };
    }
}

// flags for parseExpr desribing how the expression will be used
#define EXPRTERM_FULL    0x1
#define EXPRTERM_ARG     0x2
#define EXPR_EVALUATABLE 0x8

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
    [EXPR_FUNC_REF   ] = NULLARY,
    [EXPR_FUNC_CALL  ] = VARIADIC,
    [EXPR_NEW_ARG    ] = VARIADIC,
    [EXPR_GET_ARG    ] = NULLARY,
    [EXPR_RETURN     ] = UNARY,
    [EXPR_NOT        ] = UNARY,
    [EXPR_MUL        ] = BINARY,
    [EXPR_DIV        ] = BINARY,
    [EXPR_SUB        ] = BINARY,
    [EXPR_ADD        ] = BINARY,
    [EXPR_SHL        ] = BINARY,
    [EXPR_SHR        ] = BINARY,
    [EXPR_AND        ] = BINARY,
    [EXPR_XOR        ] = BINARY,
    [EXPR_OR         ] = BINARY,
    [EXPR_SET_ARG    ] = BINARY,
    [EXPR_SET_VAR    ] = BINARY,
    [EXPR_CAST       ] = UNARY,
    [EXPR_LOGICAL_EQ ] = BINARY,
    [EXPR_LOGICAL_NEQ] = BINARY,
    [EXPR_LOGICAL_LT ] = BINARY,
    [EXPR_LOGICAL_GT ] = BINARY,
    [EXPR_LOGICAL_LE ] = BINARY,
    [EXPR_LOGICAL_GE ] = BINARY,
    [EXPR_LOGICAL_AND] = BINARY,
    [EXPR_LOGICAL_OR ] = BINARY,
    [EXPR_LOGICAL_NOT] = UNARY
};
static_assert(N_EXPR_TYPES == 38, "not all expression types have their arity set");

static void initExpr(Expr* expr)
{
    *(ExprChain*)expr = ExprChain_new(0);
}

static ExprChain* getSubexprs(Expr* expr)
{
    assert(expr_arity_table[expr->type] == VARIADIC);
    return (ExprChain*)expr;
}

static Expr* addSubexpr(Expr* expr, Expr new)
{
    assert(expr_arity_table[expr->type] == VARIADIC);
    return ExprChain_append(getSubexprs(expr), new);
}

static Expr* getSubexpr(Expr* expr, int id)
{
    assert(expr_arity_table[expr->type] == VARIADIC);
    return ExprChain_getref(*getSubexprs(expr), id);
}

static int getSubexprsCount(Expr* expr)
{
    assert(expr_arity_table[expr->type] == VARIADIC);
    return ExprChain_length(*getSubexprs(expr));
}


bool isExprTerm(Token token, int flags, BRError* errp)
{
    int64_t symbol_id = getTokenSymbolId(token);
    if (flags & (EXPRTERM_FULL | EXPRTERM_FULL)) return symbol_id == SYMBOL_SEMICOLON;
    if (flags & EXPRTERM_ARG) {
        if (symbol_id == SYMBOL_ARGSPEC_END || symbol_id == SYMBOL_COMMA) return true;
        if (symbol_id == SYMBOL_SEMICOLON) {
            *errp = (BRError){
                .code = BR_ERR_ARG_DEF_EXPECTED,
                .loc = token
            };
        }
        return false;
    }
    return false;
}

void fprintType(FILE* dst, TypeDef type)
{
    static_assert(N_TYPE_KINDS == 6, "not all type kinds are handled in fprintType");
    if (type.kind == KIND_INT) {
        switch (type.size) {
            case 1: fputs("int8", dst); break;
            case 2: fputs("int16", dst); break;
            case 4: fputs("int32", dst); break;
            case 8: fputs("int64", dst); break;
        }
    } else if (type.kind == KIND_PTR) {
        fprintType(dst, *type.base);
        fputc('*', dst);
    } else if (type.kind == KIND_VOID) {
        fputs("void", dst);
    } else if (type.kind == KIND_BUILTIN_VAL) {
        fputs("__builtin_val", dst);
    } else if (type.kind == KIND_BOOL) {
        fputs("bool", dst);
    } else {
        eprintf("internal compiler bug in fprintType: unknown type kind %d\n", type.kind);
        abort();
    }
}
#define printType(type) fprintType(stdout, type)

int getTypeSize(TypeDef type)
{
    static_assert(N_TYPE_KINDS == 6, "not all type kinds are handled in getTypeSize");
    switch (type.kind) {
        case KIND_INT: return type.size;
        case KIND_PTR: 
        case KIND_BUILTIN_VAL:
            return 8;
        case KIND_VOID: return 0;
        case KIND_BOOL: return 1;
        case KIND_NONE:
        default:
            eprintf("internal compiler bug in getTypeSize: unknown type kind %d\n", type.kind);
            abort();
    }
}

bool typeMatches(TypeDef field, TypeDef entry)
{
    static_assert(N_TYPE_KINDS == 6, "not all type kinds are handled in typeMatches");
    switch (field.kind) {
        case KIND_VOID: return entry.kind != KIND_PTR;
        case KIND_PTR:
            return entry.kind == KIND_PTR ? typeMatches(*field.base, *entry.base) : entry.kind == KIND_BUILTIN_VAL;
        case KIND_INT:
            return (entry.kind == KIND_INT && entry.size == field.size) ||
                (entry.kind == KIND_BUILTIN_VAL && field.size == 8) ||
                (entry.kind == KIND_BOOL && field.size == 1);
        case KIND_BUILTIN_VAL: return getTypeSize(entry) == 8;
        case KIND_BOOL: return entry.kind == KIND_BOOL;
        case KIND_NONE:
        default:
            eprintf("internal compiler bug in typeMatches: unknown field type kind %d\n", field.kind);
            abort();
    }
}

bool isExprEvaluatable(ExprType type) {
    static_assert(N_EXPR_TYPES == 38, "not all expression types are handled in isExprEvaluatable");
    static bool expr_evaluatability_info[N_EXPR_TYPES] = {
        [EXPR_INVALID   ] = false,
        [EXPR_INVALID + 1 ... N_EXPR_TYPES - 1] = true,
        [EXPR_NAME      ] = false,
        [EXPR_SET_VAR   ] = false,
        [EXPR_BLOCK     ] = false, // TODO: make blocks evaluate to their "return" value
        [EXPR_TYPE      ] = false,
        [EXPR_REF       ] = false,
        [EXPR_FUNC_REF  ] = false,
        [EXPR_NEW_ARG   ] = false,
        [EXPR_RETURN    ] = false
    };
    assert(inRange(type, 0, N_EXPR_TYPES));
    return expr_evaluatability_info[type];
}

void fprintExpr(FILE* dst, Expr expr, int indent_level)
{
    for (int i = 0; i < indent_level; i++) {
        fputc('\t', dst);
    }
    static_assert(N_EXPR_TYPES == 38, "not all expression types are handled in fprintExpr");

    fprintTokenLoc(dst, expr.loc);
    switch (expr.type) {
        case EXPR_SYSCALL:
            fputs("SYSCALL\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_NAME:
            fprintf(dst, "NAME %s\n", expr.str_literal);
            break;
        case EXPR_BUILTIN:
            fprintf(dst, "BUILTIN %s\n", expr.str_literal);
            break;
        case EXPR_STRING:
            fprintf(dst, "STRING \"%s\"\n", expr.str_literal);
            break;
        case EXPR_INT:
            fprintf(dst, "INTEGER %lld\n", expr.int_literal);
            break;
        case EXPR_GET_VAR:
            fputs("GET_VAR `", dst);
            fprintType(dst, *getSubexpr(expr.arg1, 1)->var_type);
            fprintf(dst, " %s;`\n", getSubexpr(expr.arg1, 0)->name);
            break;
        case EXPR_SET_VAR:
            fputs("SET_VAR:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_NEW_VAR:
            fputs("NEW_VAR:\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_TYPE:
            fputs("TYPE ", dst);
            fprintType(dst, *expr.var_type);
            fputs(" \n", dst);
            break;
        case EXPR_REF:
            fputs("REF -> `", dst);
            if (expr.arg1) {
                assert(expr.arg1->type == EXPR_NEW_VAR || expr.arg1->type == EXPR_NEW_ARG);
                    fprintType(dst, *getSubexpr(expr.arg1, 1)->var_type);
                    fprintf(dst, " %s;", getSubexpr(expr.arg1, 0)->name);
            } else fputs("NULL", dst);
            fputs("`\n", dst);
            break;
        case EXPR_FUNC_REF:
            fputs("FUNC_REF -> `", dst);
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
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_FUNC_CALL:
            fputs("FUNC_CALL:\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_NEW_ARG:
            fputs("NEW_ARG:\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_GET_ARG:
            fputs("GET_ARG `", dst);
            fprintType(dst, *getSubexpr(expr.arg1, 1)->var_type);
            fprintf(dst, " %s;`\n", getSubexpr(expr.arg1, 0)->name);
            break;
        case EXPR_SET_ARG:
            fputs("SET_ARG:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_RETURN:
            fputs("RETURN:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_ADD:
            fputs("ADD:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SUB:
            fputs("SUBTRACT:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_MUL:
            fputs("MULTIPLY:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_DIV:
            fputs("DIVIDE:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_AND:
            fputs("BITWISE AND:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_OR:
            fputs("BITWISE OR:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_XOR:
            fputs("BITWISE EXCLUSIVE OR:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SHL:
            fputs("SHIFT LEFT:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_SHR:
            fputs("SHIFT RIGHT:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_NOT:
            fputs("BITWISE NOT:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_CAST:
            fputs("CAST TO TYPE `", dst);
            fprintType(dst, *expr.var_type);
            fputs("`:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_LOGICAL_EQ:
            fputs("EQUALS:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_NEQ:
            fputs("NOT EQUALS:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_LT:
            fputs("LESS THAN:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_GT:
            fputs("GREATER THAN:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_LE:
            fputs("LESS THAN OR EQUALS:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_GE:
            fputs("GREATER THAN OR EQUALS:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_AND:
            fputs("LOGICAL AND:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_OR:
            fputs("LOGICAL OR:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
            break;
        case EXPR_LOGICAL_NOT:
            fputs("LOGICAL NOT:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            break;
        case EXPR_INVALID:
        case N_EXPR_TYPES:
        default:
            eprintf("internal compiler bug: unknown expression type %d\n", expr.type);
            abort();
    }
}
#define printExpr(expr, indent_level) fprintExpr(stdout, expr, indent_level)

Expr* getVarDecl(Expr* block, char* name)
{

    for (; block; block = block->block) {
        assert(block->type == EXPR_BLOCK);
        for (Expr* expr = getSubexpr(block, 0)->arg1; expr; expr = getSubexpr(expr, 2)->arg1) {
            assert(expr->type == EXPR_NEW_VAR || expr->type == EXPR_NEW_ARG);
            if (streq(name, getSubexpr(expr, 0)->name)) return expr;
        }
    }

    return NULL;
}

FuncDecl* getFuncDecl(AST* ast, char* name)
{
    chain_foreach(FuncDecl, func, ast->functions,
        if (streq(name, func.name)) return &_func->value;
    );
    return NULL;
}

typedef BRError (*ExprTypeSetter) (AST*, Expr*, ExprType);

BRError defaultExprTypeSetter(AST* ast, Expr* expr, ExprType new_type)
{
    expr->type = new_type;
    return (BRError){0};
}

BRError setExprNameToExprGetVar(AST* ast, Expr* expr, ExprType new_type)
{
    Expr* decl = getVarDecl(expr->block, expr->name);
    if (!decl) return (BRError){
        .code = BR_ERR_UNKNOWN_VAR,
        .loc = (Token){ .loc = expr->loc, .type = TOKEN_WORD, .word = expr->name }
    };

    expr->type = decl->type == EXPR_NEW_ARG ? EXPR_GET_ARG : EXPR_GET_VAR;
    expr->arg1 = decl;

    return (BRError){0};
}

BRError setExprNameToExprFuncCall(AST* ast, Expr* expr, ExprType new_type)
{
    FuncDecl* decl = getFuncDecl(ast, expr->name);
    if (!decl) return (BRError){
        .code = BR_ERR_UNKNOWN_FUNC,
        .loc = (Token){ .loc = expr->loc, .type = TOKEN_WORD, .word = expr->name }
    };

    expr->type = new_type;
    initExpr(expr);
    addSubexpr(expr, (Expr){
        .type = EXPR_FUNC_REF,
        .block = expr->block,
        .loc = expr->loc,
        .arg1 = (Expr*)decl
    });
    
    return (BRError){0};
}

BRError setBinaryExprType(AST* ast, Expr* expr, ExprType new_type)
{
    if (expr->type == EXPR_NAME) {
        BRError err = setExprNameToExprGetVar(ast, expr, EXPR_GET_VAR);
        if (err.code) return err;
    }
    Expr* arg1 = malloc(sizeof(Expr));
    *arg1 = *expr;
    expr->type = new_type;
    expr->arg1 = arg1;

    return (BRError){0};
}

BRError setExprSetVar(AST* ast, Expr* expr, ExprType new_type)
{
    if (expr->type == EXPR_NAME) {
        BRError err = setExprNameToExprGetVar(ast, expr, new_type);
        if (err.code) return err;
    }

    Expr* ref = expr->arg1;
    expr->arg1 = malloc(sizeof(Expr));
    *expr->arg1 = (Expr){
        .type = EXPR_REF,
        .block = expr->block,
        .arg1 = ref,
        .loc = expr->loc
    };
    expr->type = expr->type == EXPR_GET_ARG ? EXPR_SET_ARG : EXPR_SET_VAR;
    
    return (BRError){0};
}

static BRError setExprType(AST* ast, Expr* expr, ExprType new_type)
// changes the type of the expression if the new type is suitable in place of the current expression type
{
    static_assert(N_EXPR_TYPES == 38, "not all expression types are handled in setExprType");
    static ExprTypeSetter override_table[N_EXPR_TYPES][N_EXPR_TYPES] = {
        [EXPR_INVALID   ] = {
            [EXPR_SYSCALL ... N_EXPR_TYPES - 1] = defaultExprTypeSetter,
            [EXPR_INVALID] = NULL,
            [EXPR_ADD ... EXPR_SHR] = NULL,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = NULL
        },
        [EXPR_SYSCALL   ] = { 
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_NAME      ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_GET_VAR] = setExprNameToExprGetVar,
            [EXPR_GET_ARG] = setExprNameToExprGetVar,
            [EXPR_SET_VAR] = setExprSetVar,
            [EXPR_FUNC_CALL] = setExprNameToExprFuncCall,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_BUILTIN   ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_STRING    ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_INT       ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_REF       ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_FUNC_REF  ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_NEW_VAR   ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_SET_VAR   ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_GET_VAR   ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_SET_VAR] = setExprSetVar,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_BLOCK     ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_TYPE      ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_FUNC_CALL ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_NEW_ARG   ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_GET_ARG   ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_SET_ARG] = setExprSetVar,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_SET_ARG   ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_RETURN    ] = { [0 ... N_EXPR_TYPES - 1] = NULL },
        [EXPR_ADD ... EXPR_NOT] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_CAST ] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        },
        [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_NOT] = {
            [0 ... N_EXPR_TYPES - 1] = NULL,
            [EXPR_ADD ... EXPR_SHR] = setBinaryExprType,
            [EXPR_LOGICAL_EQ ... EXPR_LOGICAL_OR] = setBinaryExprType
        }
    };

    ExprTypeSetter setter = override_table[expr->type][new_type];
    if (!setter) return (BRError){
        .code = BR_ERR_INVALID_EXPR,
        .loc = (Token){ .loc = expr->loc }
    };

    return setter(ast, expr, new_type);
}

TypeDef getExprValueType(Expr expr)
{
    static_assert(N_EXPR_TYPES == 38, "not all expression types are handled in getExprValueType");
    static_assert(N_TYPE_KINDS == 6, "not all type kinds are handled in getExprValueType");
    switch (expr.type) {
        case EXPR_INVALID:
        case EXPR_NAME:
        case EXPR_NEW_VAR:
        case EXPR_NEW_ARG:
        case EXPR_TYPE:
        case EXPR_REF:
        case EXPR_FUNC_REF:
        case EXPR_BLOCK:
        case EXPR_RETURN:
            return (TypeDef){0};
        case EXPR_BUILTIN:
        case EXPR_SYSCALL:
            return BUILTIN_VAL_TYPE;
        case EXPR_STRING: return TYPE_STR_LITERAL;
        case EXPR_INT: return INT_TYPE(4);
        case EXPR_FUNC_CALL: return ((FuncDecl*)getSubexpr(&expr, 0)->arg1)->return_type;
        case EXPR_GET_ARG:
        case EXPR_SET_ARG:
        case EXPR_GET_VAR:
        case EXPR_SET_VAR:
            return *getSubexpr(expr.arg1, 1)->var_type;
        case EXPR_SUB:
        case EXPR_ADD: {
            TypeDef arg1_type = getExprValueType(*expr.arg1);
            TypeDef arg2_type = getExprValueType(*expr.arg2);

            if (arg1_type.kind == KIND_PTR && arg2_type.kind == KIND_PTR) return INT_TYPE(8);
            if (arg1_type.kind == KIND_PTR) {
                if (arg2_type.kind == KIND_INT && arg2_type.kind == KIND_BUILTIN_VAL) return arg1_type;
            } else if (arg2_type.kind == KIND_PTR) {
                if (arg1_type.kind == KIND_INT && arg1_type.kind == KIND_BUILTIN_VAL) return arg2_type;
            } else if (arg1_type.kind == KIND_BUILTIN_VAL || arg2_type.kind == KIND_BUILTIN_VAL) return INT_TYPE(8);

            assert(arg1_type.kind == KIND_INT && arg2_type.kind == KIND_INT);
            return arg1_type.size == arg2_type.size ? INT_TYPE(arg1_type.size * 2) : (arg1_type.size > arg2_type.size ? arg1_type : arg2_type);
            break;
        } case EXPR_MUL: {
            int arg1_size = getTypeSize(getExprValueType(*expr.arg1));
            int arg2_size = getTypeSize(getExprValueType(*expr.arg2));
            return arg1_size == arg2_size ? INT_TYPE(arg1_size * 2) : INT_TYPE(maxInt(arg1_size, arg2_size));
        } case EXPR_DIV: {
            int arg1_size = getTypeSize(getExprValueType(*expr.arg1));
            int arg2_size = getTypeSize(getExprValueType(*expr.arg2));
            return arg1_size == arg2_size ? INT_TYPE(arg1_size) : INT_TYPE(maxInt(arg1_size, arg2_size));
        } case EXPR_AND: {
            int arg1_size = getTypeSize(getExprValueType(*expr.arg1));
            int arg2_size = getTypeSize(getExprValueType(*expr.arg2));
            return INT_TYPE(minInt(arg1_size, arg2_size));
        } case EXPR_OR: 
        case EXPR_XOR: {
            int arg1_size = getTypeSize(getExprValueType(*expr.arg1));
            int arg2_size = getTypeSize(getExprValueType(*expr.arg2));
            return INT_TYPE(maxInt(arg1_size, arg2_size));
        } case EXPR_SHL: return INT_TYPE(8);
        case EXPR_SHR:
        case EXPR_NOT:
            return INT_TYPE(getTypeSize(getExprValueType(*expr.arg1)));
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
        case N_EXPR_TYPES:
        default:
            eprintf("internal compiler bug in getExprValueType: unknown expression type %d\n", expr.type);
            abort();
    }
}

int getVarIndex(Expr expr)
{
    int res = -1;
    for (Expr* expr_iter = &expr; expr_iter != NULL; expr_iter = getSubexpr(expr_iter, 2)->arg1) {
        assert(expr_iter->type == EXPR_NEW_VAR || expr_iter->type == EXPR_NEW_ARG);
        res++;
    }
    return res;
}

void printAST(AST* ast, BRP* obj)
{
    chain_foreach(FuncDecl, func, ast->functions, 
        printTokenLoc(func.loc);
        printf("function %s:\n", func.name);
        puts("arguments:");
        printExpr(func.args, 0);
        puts("body:");
        printExpr(func.body, 0);
    );
}

bool matchType(TypeDef field_type, Expr* expr)
{
    static_assert(N_TYPE_KINDS == 6, "not all type kinds are handled in typeMatches");

    TypeDef expr_val_type = getExprValueType(*expr);
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
        Expr* new_subexpr = malloc(sizeof(Expr));
        *new_subexpr = *expr;
        expr->type = EXPR_CAST;
        expr->arg1 = new_subexpr;
        expr->var_type = malloc(sizeof(TypeDef));
        *expr->var_type = new_type;
        return true;
    }
    return false;
}

BRError parseExpr(BRP* obj, Expr* dst, Expr* block, FuncDecl* func, AST* ast, int flags)
{
    static_assert(N_EXPR_TYPES == 38, "not all expression types are handled in parseExpr");
    TypeDef new_type;
    BRError expr_term_err = {0};
    Token token;

    dst->block = block;
    while (!BRPempty(obj)) {
        token = peekToken(obj);
        if (isExprTerm(token, flags, &expr_term_err)) {
            break;
        } else if (expr_term_err.code) return expr_term_err;

        BRError type_err = parseType(obj, &new_type);
        if ((int)type_err.code > 0) return type_err;
        if (type_err.code == 0) {
            dst->loc = token.loc;
            BRError err = setExprType(ast, dst, EXPR_NEW_VAR);
            if (err.code) return err;

            if (new_type.kind == KIND_VOID) return (BRError){
                .code = BR_ERR_VOID_VAR_DECL,
                .loc = token
            };
            initExpr(dst);

            token = fetchToken(obj);
            if (token.type != TOKEN_WORD) return (BRError){
                .code = BR_ERR_INVALID_VAR_NAME,
                .loc = token
            };
            if (getVarDecl(block, token.word)) return (BRError){
                .code = BR_ERR_VAR_EXISTS,
                .loc = token
            };
            addSubexpr(dst, (Expr){
                .type = EXPR_NAME,
                .str_literal = token.word,
                .block = block,
                .loc = dst->loc
            });

            TypeDef* new_var_type = malloc(sizeof(TypeDef));
            *new_var_type = new_type;
            addSubexpr(dst, (Expr){
                .type = EXPR_TYPE,
                .var_type = new_var_type,
                .block = block,
                .loc = dst->loc
            });
            addSubexpr(dst, (Expr){
                .type = EXPR_REF,
                .arg1 = getSubexpr(block, 0)->arg1,
                .block = block,
                .loc = dst->loc
            });
            getSubexpr(block, 0)->arg1 = dst;
        } else if (token.type == TOKEN_INT) {
            dst->loc = token.loc;
            BRError err = setExprType(ast, dst, EXPR_INT);
            if (err.code) return err;

            fetchToken(obj);
            dst->int_literal = token.value;
        } else if (token.type == TOKEN_STRING) {
            dst->loc = token.loc;
            BRError err = setExprType(ast, dst, EXPR_STRING);
            if (err.code) return err;

            fetchToken(obj);
            dst->str_literal = token.word;
        } else if (token.type == TOKEN_KEYWORD) {
            switch (token.keyword_id) {
                case KW_SYS: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_SYSCALL);
                    if (err.code) return err;
                    fetchToken(obj);

                    initExpr(dst);
                    token = fetchToken(obj);
                    if (token.type != TOKEN_WORD) return (BRError){
                        .code = BR_ERR_INVALID_SYSCALL_NAME,
                        .loc = token
                    };
                    bool name_vaildated = false;
                    for (int i = 0; i < N_SYS_OPS; i++) {
                        if (sbufeq(syscallNames[i], fromstr(token.word))) {
                            name_vaildated = true;
                            break;
                        }
                    }
                    if (!name_vaildated) return (BRError){
                        .code = BR_ERR_INVALID_SYSCALL_NAME,
                        .loc = token
                    };
                    addSubexpr(dst, (Expr){
                        .type = EXPR_NAME,
                        .name = token.word,
                        .block = block,
                        .loc = dst->loc
                    });

                    token = fetchToken(obj);
                    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) return (BRError){
                        .code = BR_ERR_ARG_DEF_START_EXPECTED,
                        .loc = token
                    };

                    token = peekToken(obj);
                    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_END) {
                        while (true) {
                            token = peekToken(obj);
                            Expr* arg = addSubexpr(dst, (Expr){0});
                            BRError err = parseExpr(obj, arg, dst->block, func, ast, EXPRTERM_ARG | EXPR_EVALUATABLE);
                            if (err.code) return err;

                            token = fetchToken(obj);
                            if (token.symbol_id == SYMBOL_ARGSPEC_END) {
                                break;
                            } else if (token.symbol_id != SYMBOL_COMMA) return (BRError){
                                .code = BR_ERR_ARG_DEF_EXPECTED,
                                .loc = token
                            };
                        }
                    } else fetchToken(obj);
                    if (getSubexprsCount(dst) > 7) return (BRError){
// syscalls cannot accept more than 6 arguments; 7 is 6 maximum arguments + the first expression, which holds the name of the syscall
                        .code = BR_ERR_TOO_MANY_SYSCALL_ARGS,
                        .loc = token,
                        .n_args = getSubexprsCount(dst) - 1
                    };
                    break;
                } case KW_BUILTIN: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_BUILTIN);
                    if (err.code) return err;
                    fetchToken(obj);

                    token = fetchToken(obj);
                    if (token.type != TOKEN_WORD) return (BRError){
                        .code = BR_ERR_BUILTIN_NAME_EXPECTED,
                        .loc = token
                    };
                    bool name_vaildated = false;
                    for (int i = 0; i < N_BUILTINS; i++) {
                        if (sbufeq(fromstr(builtins[i].name), fromstr(token.word))) {
                            name_vaildated = true;
                            break;
                        }
                    }
                    if (!name_vaildated) return (BRError){
                        .code = BR_ERR_INVALID_BUILTIN_NAME,
                        .loc = token
                    };
                    dst->str_literal = token.word;
                    break;
                } case KW_RETURN: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_RETURN);
                    if (err.code) return err;
                    fetchToken(obj);

                    dst->arg1 = calloc(1, sizeof(Expr));
                    err = parseExpr(obj, dst->arg1, block, func, ast, EXPRTERM_FULL | EXPR_EVALUATABLE);
                    if (err.code) return err;

                    if (!matchType(func->return_type, dst->arg1)) return (BRError){
                        .code = BR_ERR_RETURN_TYPE_MISMATCH,
                        .loc = token,
                        .func = func,
                        .entry_type = getExprValueType(*dst->arg1)
                    };
                    break;
                } case KW_CAST: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_CAST);
                    if (err.code) return err;
                    fetchToken(obj);
                    
                    token = fetchToken(obj);
                    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) return (BRError){
                        .code = BR_ERR_ARG_DEF_START_EXPECTED,
                        .loc = token
                    };

                    dst->arg1 = calloc(1, sizeof(Expr));
                    err = parseExpr(obj, dst->arg1, block, func, ast, EXPRTERM_ARG | EXPR_EVALUATABLE);
                    if (err.code) return err;
                    token = fetchToken(obj);
                    if (getTokenSymbolId(token) != SYMBOL_COMMA) return (BRError){
                        .code = BR_ERR_CAST_TYPE_EXPECTED,
                        .loc = token
                    };

                    token = peekToken(obj);
                    dst->var_type = malloc(sizeof(TypeDef));
                    err = parseType(obj, dst->var_type);
                    if (err.code == -1) return (BRError){
                        .code = BR_ERR_INVALID_TYPE,
                        .loc = token
                    };
                    if (err.code) return err;
                    if (dst->var_type->kind == KIND_VOID) return (BRError){
                        .code = BR_ERR_VOID_TYPE_CAST,
                        .loc = token
                    };
                    token = fetchToken(obj);
                    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_END) return (BRError){
                        .code = BR_ERR_ARG_DEF_END_EXPECTED,
                        .loc = token
                    };
                    break;
                } case KW_AND:
                case KW_OR: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_LOGICAL_AND + token.keyword_id - KW_AND);
                    if (err.code) return err;
                    fetchToken(obj);

                    dst->arg2 = calloc(1, sizeof(Expr));
                    token = peekToken(obj);
                    err = parseExpr(obj, dst->arg2, block, func, ast, flags | EXPR_EVALUATABLE);
                    if (err.code) return err;

                    if (!matchType(getExprValueType(*dst->arg1), dst->arg2)) return (BRError){
                        .code = BR_ERR_COMPARISON_TYPE_MISMATCH,
                        .loc = token,
                        .left_expr = dst->arg1,
                        .entry_type = getExprValueType(*dst->arg2)
                    };
                    break;
                } case KW_NOT: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_LOGICAL_NOT);
                    if (err.code) return err;
                    fetchToken(obj);

                    dst->arg1 = calloc(1, sizeof(Expr));
                    err = parseExpr(obj, dst->arg1, block, func, ast, flags | EXPR_EVALUATABLE);
                    if (err.code) return err;

                    break;
                } default: return (BRError){
                    .code = BR_ERR_INVALID_EXPR,
                    .loc = token
                };
            }
        } else if (token.type == TOKEN_SYMBOL) {
            fetchToken(obj);
            switch (token.symbol_id) {
                case SYMBOL_ASSIGNMENT: {
                    switch (dst->type) {
                        case EXPR_NEW_VAR: {
                            Expr* var_initializer = addSubexpr(dst, (Expr){0});

                            token = peekToken(obj);
                            BRError err = parseExpr(obj, var_initializer, block, func, ast, EXPRTERM_FULL | EXPR_EVALUATABLE);
                            if (err.code) return err;

                            if (!matchType(*getSubexpr(dst, 1)->var_type, var_initializer)) return (BRError){
                                .code = BR_ERR_VAR_TYPE_MISMATCH,
                                .loc = token,
                                .var_decl = dst,
                                .entry_type = getExprValueType(*var_initializer)
                            };
                            break;
                        } case EXPR_GET_VAR:
                        case EXPR_GET_ARG:
                        case EXPR_NAME: {
                            dst->loc = token.loc;
                            BRError err = setExprType(ast, dst, EXPR_SET_VAR);
                            if (err.code) return err;

                            dst->arg2 = malloc(sizeof(Expr));
                            Token entry_loc = peekToken(obj);
                            err = parseExpr(obj, dst->arg2, block, func, ast, EXPRTERM_FULL | EXPR_EVALUATABLE);
                            if (err.code) return err;

                            if (!matchType(*getSubexpr(dst->arg1->arg1, 1)->var_type, dst->arg2)) return (BRError){
                                .code = BR_ERR_VAR_TYPE_MISMATCH,
                                .loc = entry_loc,
                                .var_decl = dst->arg1->arg1,
                                .entry_type = getExprValueType(*dst->arg2)
                            };
                            break;
                        } default: return (BRError){
                            .code = BR_ERR_INVALID_EXPR,
                            .loc = token
                        };
                    }
                    break;
                } case SYMBOL_BLOCK_START: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_BLOCK);
                    if (err.code) return err;

                    initExpr(dst);
                    addSubexpr(dst, (Expr){
                        .type = EXPR_REF,
                        .block = dst,
                        .loc = dst->loc
                    });

                    while (true) {
                        token = peekToken(obj);
                        if (token.type == TOKEN_NONE) return (BRError){
                            .code = BR_ERR_UNCLOSED_BLOCK,
                            .loc = token
                        };
                        if (getTokenSymbolId(token) == SYMBOL_BLOCK_END) {
                            fetchToken(obj);
                            return (BRError){0};
                        }

                        Expr* expr = addSubexpr(dst, (Expr){0});
                        BRError err = parseExpr(obj, expr, dst, func, ast, EXPRTERM_FULL);
                        if (err.code) return err;

                        fetchToken(obj);
                    }
                    break;
                } case SYMBOL_MINUS:
                case SYMBOL_PLUS:
                case SYMBOL_STAR:
                case SYMBOL_DIV:
                case SYMBOL_AMPERSAND:
                case SYMBOL_PIPE:
                case SYMBOL_CARET:
                case SYMBOL_LSHIFT:
                case SYMBOL_RSHIFT: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_ADD + token.symbol_id - SYMBOL_PLUS);
                    if (err.code) return err;

                    dst->arg2 = calloc(1, sizeof(Expr));
                    err = parseExpr(obj, dst->arg2, block, func, ast, flags | EXPR_EVALUATABLE);
                    if (err.code) return err;
                    break;
                } case SYMBOL_TILDE: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_NOT);
                    if (err.code) return err;

                    dst->arg1 = calloc(1, sizeof(Expr));
                    err = parseExpr(obj, dst->arg1, block, func, ast, flags | EXPR_EVALUATABLE);
                    if (err.code) return err;
                    break;
                } case SYMBOL_ARGSPEC_START: {
                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, EXPR_FUNC_CALL);
                    if (err.code) return err;

                    FuncDecl* def = (FuncDecl*)getSubexpr(dst, 0)->arg1;
                    token = peekToken(obj);
                    int arg_id = 0, expected_arg_count = getSubexprsCount(&def->args) - 1;

                    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_END) {
                        ExprNode* arg_decl_iter = getSubexprs(&def->args)->start->next;
                        while (true) {
                            token = peekToken(obj);
                            if (arg_id == expected_arg_count) return (BRError){
                                .code = BR_ERR_ARG_COUNT_MISMATCH,
                                .loc = token,
                                .func = def,
                                .n_args = -1
                            };

                            Expr* arg = addSubexpr(dst, (Expr){0});
                            BRError err = parseExpr(obj, arg, dst->block, func, ast, EXPRTERM_ARG | EXPR_EVALUATABLE);
                            if (err.code) return err;
                            if (!matchType(*getSubexpr(&arg_decl_iter->value, 1)->var_type, arg)) return (BRError){
                                .code = BR_ERR_ARG_TYPE_MISMATCH,
                                .loc = token,
                                .entry_type = getExprValueType(*arg),
                                .func = def,
                                .arg_decl = &arg_decl_iter->value
                            };

                            arg_decl_iter = arg_decl_iter->next;
                            arg_id++;
                            token = fetchToken(obj);
                            if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_END) {
                                break;
                            } else if (token.symbol_id != SYMBOL_COMMA) return (BRError){
                                .code = BR_ERR_ARG_DEF_EXPECTED,
                                .loc = token
                            };
                        }

                    } else fetchToken(obj);

                    if (arg_id < expected_arg_count) return (BRError){
                        .code = BR_ERR_ARG_COUNT_MISMATCH,
                        .loc = token,
                        .func = def,
                        .n_args = arg_id
                    };
                    break;
                } case SYMBOL_EQ:
                case SYMBOL_NEQ:
                case SYMBOL_LT:
                case SYMBOL_GT:
                case SYMBOL_LE: 
                case SYMBOL_GE: {
                    static ExprType symbol_to_expr[N_SYMBOLS] = {
                        [SYMBOL_EQ] = EXPR_LOGICAL_EQ,
                        [SYMBOL_NEQ] = EXPR_LOGICAL_NEQ,
                        [SYMBOL_LT] = EXPR_LOGICAL_LT,
                        [SYMBOL_GT] = EXPR_LOGICAL_GT,
                        [SYMBOL_LE] = EXPR_LOGICAL_LE,
                        [SYMBOL_GE] = EXPR_LOGICAL_GE
                    };

                    dst->loc = token.loc;
                    BRError err = setExprType(ast, dst, symbol_to_expr[token.symbol_id]);
                    if (err.code) return err;

                    dst->arg2 = calloc(1, sizeof(Expr));
                    token = peekToken(obj);
                    err = parseExpr(obj, dst->arg2, block, func, ast, flags | EXPR_EVALUATABLE);
                    if (err.code) return err;

                    if (!matchType(getExprValueType(*dst->arg1), dst->arg2)) return (BRError){
                        .code = BR_ERR_COMPARISON_TYPE_MISMATCH,
                        .loc = token,
                        .left_expr = dst->arg1,
                        .entry_type = getExprValueType(*dst->arg2)
                    };
                    break;
                } default: return (BRError){
                    .code = BR_ERR_INVALID_EXPR,
                    .loc = token
                };
            }
        } else if (token.type == TOKEN_WORD) {
            dst->loc = token.loc;
            BRError err = setExprType(ast, dst, EXPR_NAME);
            if (err.code) return err;
            dst->name = token.word;
            fetchToken(obj);
        } else if (token.type == TOKEN_NONE) {
            return (BRError){
                .code = BR_ERR_INVALID_EXPR,
                .loc = token
            };
        } else {
            eprintf("internal compiler bug: invalid token type %hhd\n", token.type);
            abort();
        }
    }

    if (dst->type == EXPR_NAME) {
        BRError err = setExprType(ast, dst, EXPR_GET_VAR);
        if (err.code) return err;
    }
    if (!isExprEvaluatable(dst->type) && flags & EXPR_EVALUATABLE) {
        return (BRError){
            .code = BR_ERR_NO_VALUE_EXPR,
            .loc = token
        };
    }
    return (BRError){0};
}

void reorderExpr(Expr* expr)
{
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
        [EXPR_FUNC_REF   ] = 0,
        [EXPR_FUNC_CALL  ] = 0,
        [EXPR_NEW_ARG    ] = 0,
        [EXPR_GET_ARG    ] = 0,
        [EXPR_CAST       ] = 0,
        [EXPR_NOT        ] = 1,
        [EXPR_LOGICAL_NOT] = 1,
        [EXPR_MUL        ] = 2,
        [EXPR_DIV        ] = 2,
        [EXPR_SUB        ] = 3,
        [EXPR_ADD        ] = 3,
        [EXPR_SHL        ] = 4,
        [EXPR_SHR        ] = 4,
        [EXPR_LOGICAL_GT ] = 5,
        [EXPR_LOGICAL_LT ] = 5,
        [EXPR_LOGICAL_LE ] = 5,
        [EXPR_LOGICAL_GE ] = 5,
        [EXPR_LOGICAL_EQ ] = 6,
        [EXPR_LOGICAL_NEQ] = 6,
        [EXPR_AND        ] = 7,
        [EXPR_XOR        ] = 8,
        [EXPR_OR         ] = 9,
        [EXPR_LOGICAL_AND] = 10,
        [EXPR_LOGICAL_OR ] = 11,
        [EXPR_SET_ARG    ] = 12,
        [EXPR_SET_VAR    ] = 12,
        [EXPR_RETURN     ] = 13
    };
    static_assert(N_EXPR_TYPES == 38, "not all expression types are handled in reorderExpr");

    int expr_order = expr_order_table[expr->type];
    switch (expr_arity_table[expr->type]) {
        case VARIADIC:
            chain_foreach_from(Expr, subexpr, *getSubexprs(expr), 1, 
                reorderExpr(&_subexpr->value);
            );
        case NULLARY:
            return;
        case UNARY: {
            if (!expr->arg1 || !expr_order) return;
            reorderExpr(expr->arg1);
            int arg_order = expr_order_table[expr->arg1->type];
            if (expr_arity_table[expr->arg1->type] <= UNARY) return;
            if (arg_order > expr_order) {
                swap(expr->type, expr->arg1->type, ExprType);
                expr->arg2 = expr->arg1->arg2;
            }
            return;
        }
        case BINARY: {
            reorderExpr(expr->arg1);
            reorderExpr(expr->arg2);
            if (!expr_order || expr_arity_table[expr->arg2->type] <= UNARY) return;
            int arg2_order = expr_order_table[expr->arg2->type];
            if (arg2_order > expr_order) {
                swap(expr->type, expr->arg2->type, ExprType);
                swap(expr->arg1, expr->arg2, Expr*);
                swap(expr->arg1->arg2, expr->arg2, Expr*);
                swap(expr->arg1->arg1, expr->arg1->arg2, Expr*);
                reorderExpr(expr->arg1);
                reorderExpr(expr->arg2);
            } else if (arg2_order == expr_order) {
                swap(expr->type, expr->arg2->type, ExprType);
                swap(expr->arg1, expr->arg2, Expr*);
                swap(expr->arg2, expr->arg1->arg1, Expr*);
                swap(expr->arg2, expr->arg1->arg2, Expr*);
                reorderExpr(expr->arg1);
                reorderExpr(expr->arg2);
            }
        }
    }
}

void lowerExpr(Expr* expr)
{
    switch (expr->type) {
        case EXPR_CAST: {
            lowerExpr(expr->arg1);
            int subexpr_type_size = getTypeSize(getExprValueType(*expr->arg1));
            int new_type_size = getTypeSize(*expr->var_type);

            if (expr->var_type->kind == KIND_BOOL) {
                if (expr->arg1->type == EXPR_INT) {
                    int64_t new_int = expr->arg1->int_literal != 0;
                    free(expr->arg1);
                    free(expr->var_type);
                    expr->type = EXPR_INT;
                    expr->int_literal = new_int;
                } else {
                    expr->type = EXPR_LOGICAL_NEQ;
                    free(expr->var_type);
                    expr->arg2 = malloc(sizeof(Expr));
                    *expr->arg2 = (Expr){
                        .type = EXPR_INT,
                        .block = expr->block,
                        .loc = expr->arg1->loc,
                        .int_literal = 0
                    };
                }
            } else if (new_type_size < subexpr_type_size) {
                if (expr->arg1->type == EXPR_INT) {
                    int64_t new_int = expr->arg1->int_literal & ((1LL << (new_type_size * 8)) - 1);
                    free(expr->arg1);
                    free(expr->var_type);
                    expr->type = EXPR_INT;
                    expr->int_literal = new_int;
                } else {
                    expr->type = EXPR_AND;
                    Expr* temp_arg2 = malloc(sizeof(Expr));
                    *temp_arg2 = (Expr){
                        .type = EXPR_INT,
                        .block = expr->arg1->block,
                        .int_literal = (1LL << (new_type_size * 8)) - 1,
                        .loc = expr->arg1->loc
                    };
                    free(expr->var_type);
                    expr->arg2 = temp_arg2;
                }
            } else {
                free(expr->var_type);
                Expr new_expr = *expr->arg1;
                *expr = new_expr;
            }
            break;
        }
        default: switch (expr_arity_table[expr->type]) {
            case VARIADIC:
                chain_foreach_from(Expr, subexpr, *getSubexprs(expr), 1, 
                    lowerExpr(&_subexpr->value);
                );
                return;
            case NULLARY:
                return;
            case BINARY:
                lowerExpr(expr->arg2);
            case UNARY:
                if (expr->arg1) lowerExpr(expr->arg1);
                return;
        }
    }
}

BRError parseSourceCode(BRP* obj, AST* dst) // br -> temporary AST
{
    Token token;
    dst->functions = FuncDeclChain_new(0);
    dst->globals.type = EXPR_BLOCK;
    dst->globals.block = NULL;
    initExpr(&dst->globals);
    addSubexpr(&dst->globals, (Expr){ .type = EXPR_REF });

    while (true) {
        TypeDef type;
        BRError type_err = parseType(obj, &type);
        if ((int)type_err.code > 0) return type_err;
        if (type_err.code == -1) type = VOID_TYPE;
// fetching function name
        Token func_name_spec = fetchToken(obj);
        if (func_name_spec.type == TOKEN_WORD) {
            token = fetchToken(obj);
            FuncDecl* new_func = FuncDeclChain_append(
                &dst->functions,
                (FuncDecl){
                    .return_type = type,
                    .name = func_name_spec.word,
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
            if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) return (BRError){
                .code = BR_ERR_ARG_DEF_START_EXPECTED,
                .loc = token
            };
            while (true) {
                Token type_spec = peekToken(obj);
                BRError err = parseType(obj, &type);
                if ((int)err.code > 0) return err;
                if (err.code == 0) {
                    token = fetchToken(obj);
                    if (token.type != TOKEN_WORD) return (BRError){
                        .code = BR_ERR_ARG_NAME_EXPECTED,
                        .loc = token
                    };

                    Expr* new_arg = addSubexpr(&new_func->args, (Expr){
                        .type = EXPR_NEW_ARG,
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
                    } else if (getTokenSymbolId(token) != SYMBOL_COMMA) return (BRError){
                        .code = BR_ERR_ARG_DEF_EXPECTED,
                        .loc = token
                    };
                }
            }

            if (streq(func_name_spec.word, "main")) {
                if (new_func->return_type.kind != KIND_VOID) return (BRError){
                    .code = BR_ERR_MAIN_PROC_RET_TYPE_MISMATCH,
                    .loc = func_name_spec,
                    .entry_type = new_func->return_type
                };
                int main_proc_n_args = getSubexprsCount(&new_func->args) - 1;
                if (main_proc_n_args != 0) return (BRError){
                    .code = BR_ERR_MAIN_PROC_ARG_COUNT_MISMATCH,
                    .loc = token,
                    .n_args = main_proc_n_args
                };
            }
    // parsing the function body
            token = peekToken(obj);
            if (getTokenSymbolId(token) != SYMBOL_SEMICOLON) {
                BRError err = parseExpr(obj, &new_func->body, &new_func->args, new_func, dst, EXPRTERM_FULL);
                if (err.code) return err;
                if (new_func->body.type == EXPR_NEW_VAR) return (BRError){
                    .code = BR_ERR_INVALID_FUNC_DEF,
                    .loc = token
                };
                lowerExpr(&new_func->body);
                reorderExpr(&new_func->body);
            } else fetchToken(obj);
        } else if (func_name_spec.type == TOKEN_NONE) {
            break;
        } else return (BRError){
            .code = BR_ERR_INVALID_GLOBAL_STMT,
            .loc = func_name_spec
        };
    }

    return (BRError){0};
}

typedef uint8_t regstate_t;
typedef struct {
    strArray data_blocks;
    intArray mem_blocks;
    sbuf cur_src_path;
    int cur_src_line;
    regstate_t arg_cache_state;
    FILE* dst;
} ASTCompilerCtx;

typedef bool (*ExprCompiler) (ASTCompilerCtx*, Expr, regstate_t, int);
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

void compileSrcRef(ASTCompilerCtx* ctx, TokenLoc loc)
{
    sbuf path_s = fromstr(loc.src_name);
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

regstate_t getArgCacheState(Expr expr)
{
    static_assert(N_EXPR_TYPES == 38, "not all expression types are handled in getArgCacheState");
    regstate_t res = 0;
    switch (expr.type) {
        case EXPR_SYSCALL:
        case EXPR_FUNC_CALL:
            res |= (1 << (getSubexprsCount(&expr) - 1)) - 1;
        case EXPR_BLOCK:
            chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1, 
                res |= getArgCacheState(subexpr);
            );
            break;
        case EXPR_RETURN: res |= getArgCacheState(*expr.arg1); break;
        case EXPR_ADD:
        case EXPR_SUB:
        case EXPR_MUL:
        case EXPR_DIV:
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
        case EXPR_LOGICAL_OR:
            res |= getArgCacheState(*expr.arg1);
            res |= getArgCacheState(*expr.arg2);
            break;
        case EXPR_NOT:
        case EXPR_CAST:
        case EXPR_LOGICAL_NOT:
            res |= getArgCacheState(*expr.arg1);
        default: break;
    }
    return res;
}

bool compileExprInvalid(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    return false;
}

bool compileExprSyscall(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    regstate_t reg_cache_state = ((1 << (getSubexprsCount(&expr) - 1)) - 1) & reg_state;
    int cache_id = compileRegCaching(ctx, reg_cache_state);
    int i = 0;
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        if (!expr_compilers[subexpr.type](ctx, subexpr, (1 << i) - 1, i)) return false;
        i++;
    );
    fprintf(ctx->dst, "\tsys %s\n", getSubexpr(&expr, 0)->name);

    if (dst_reg != 0) fprintf(ctx->dst, "\tsetr r%d r0\n", dst_reg);
    compileRegUncaching(ctx, reg_cache_state, cache_id);

    return true;
}

bool compileExprBuiltin(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    fprintf(ctx->dst, "\tsetb r%d %s\n", dst_reg, expr.name);
    return true;
}

bool compileExprString(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    int str_index = -1;
    for (int i = 0; i < ctx->data_blocks.length; i++) {
        if (streq(ctx->data_blocks.data[i], expr.str_literal)) {
            str_index = i;
            break;
        }
    }
    if (str_index < 0) {
        str_index = ctx->data_blocks.length;
        if (!strArray_append(&ctx->data_blocks, expr.str_literal)) return false;
    }

    fprintf(ctx->dst, "\tsetd r%d "STR_PREFIX"%d\n", dst_reg, str_index);
    return true;
}

bool compileExprInt(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    fprintf(ctx->dst, "\tset r%d %lld\n", dst_reg, expr.int_literal);
    return true;
}

bool compileExprNewVar(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    char* name = getSubexpr(&expr, 0)->name;
    int size = getTypeSize(*getSubexpr(&expr, 1)->var_type);

    if (getSubexprsCount(&expr) == 4) {
        Expr* initializer = getSubexpr(&expr, 3);
        if (!expr_compilers[initializer->type](ctx, *initializer, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tpushv %s %d r%d\n", name, size, dst_reg);
    } else {
        fprintf(ctx->dst, "\tvar %s %d\n", name, size);
    }
    return true;
}

bool compileExprSetVar(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
    fprintf(ctx->dst, "\tstrv %s r%d\n", getSubexpr(expr.arg1->arg1, 0)->name, dst_reg);
    return true;
}

bool compileExprGetVar(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    fprintf(ctx->dst, "\tldv r%d %s\n", dst_reg, getSubexpr(expr.arg1, 0)->name);
    return true;
}

bool compileExprBlock(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        if (!expr_compilers[subexpr.type](ctx, subexpr, reg_state, 0)) return false;
    );
    return true;
}

bool compileExprFuncCall(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    regstate_t reg_cache_state = reg_state;
    int cache_id = compileRegCaching(ctx, reg_cache_state);
    int i = 0;
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        if (!expr_compilers[subexpr.type](ctx, subexpr, (1 << i) - 1, i)) return false;
        i++;
    );
    fprintf(ctx->dst, "\tcall %s\n", ((FuncDecl*)getSubexpr(&expr, 0)->arg1)->name);

    if (dst_reg != 0) fprintf(ctx->dst, "\tsetr r%d r0\n", dst_reg);
    compileRegUncaching(ctx, reg_cache_state, cache_id);

    return true;
}

bool compileExprGetArg(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    int arg_id = getVarIndex(*expr.arg1);
    if (ctx->arg_cache_state & (1 << arg_id)) {
        fprintf(ctx->dst, "\tldv r%d %s\n", dst_reg, getSubexpr(expr.arg1, 0)->name);
    } else {
        if (arg_id != dst_reg) fprintf(ctx->dst, "\tsetr r%d r%d\n", dst_reg, arg_id);
    }
    return true;
}

bool compileExprSetArg(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);
    if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;

    int arg_id = getVarIndex(*expr.arg1->arg1);
    if (ctx->arg_cache_state & (1 << arg_id)) {
        fprintf(ctx->dst, "\tstrv %s r%d\n", getSubexpr(expr.arg1->arg1, 0)->name, dst_reg);
    } else {
        fprintf(ctx->dst, "\tsetr r%d r%d\n", arg_id, dst_reg);
    }
    return true;
}

bool compileExprReturn(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, 0)) return false;
    fprintf(ctx->dst, "\tret\n");
    return true;
}

bool compileExprAdd(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tadd r%d r%d %lld\n", dst_reg, dst_reg, expr.arg1->int_literal);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tadd r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\taddr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\taddr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprSub(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(
            ctx->dst,
            "\tnot r%d r%d\n"
            "\tadd r%d r%d %lld\n",
            dst_reg, dst_reg,
            dst_reg, dst_reg, expr.arg1->int_literal + 1
        );
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tsub r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tsubr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tsubr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprMul(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tmul r%d r%d %lld\n", dst_reg, dst_reg, expr.arg1->int_literal);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tmul r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tmulr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tmulr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprDiv(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tdivs r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tdivsr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tdivsr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprAnd(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tand r%d r%d %lld\n", dst_reg, dst_reg, expr.arg1->int_literal);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tand r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tandr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tandr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprOr(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tor r%d r%d %lld\n", dst_reg, dst_reg, expr.arg1->int_literal);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tor r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\torr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\torr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprXor(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\txor r%d r%d %lld\n", dst_reg, dst_reg, expr.arg1->int_literal);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\txor r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\txorr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\txorr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprShl(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tshl r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tshlr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tshlr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprShr(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tshrs r%d r%d %lld\n", dst_reg, dst_reg, expr.arg2->int_literal);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tshrsr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tshrsr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprNot(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
    fprintf(ctx->dst, "\tnot r%d r%d\n", dst_reg, dst_reg);
    return true;
}

bool compileExprLogicalEq(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d equ\n", dst_reg, expr.arg1->int_literal, dst_reg);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d equ\n", dst_reg, expr.arg2->int_literal, dst_reg);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d equ\n", dst_reg, arg2_dst_reg, dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d equ\n", dst_reg, arg2_dst_reg, dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprLogicalNeq(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d neq\n", dst_reg, expr.arg1->int_literal, dst_reg);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d neq\n", dst_reg, expr.arg2->int_literal, dst_reg);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d neq\n", dst_reg, arg2_dst_reg, dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d neq\n", dst_reg, arg2_dst_reg, dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprLogicalLt(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d gts\n", dst_reg, expr.arg1->int_literal, dst_reg);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d lts\n", dst_reg, expr.arg2->int_literal, dst_reg);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d lts\n", dst_reg, arg2_dst_reg, dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d lts\n", dst_reg, arg2_dst_reg, dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprLogicalGt(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d lts\n", dst_reg, expr.arg1->int_literal, dst_reg);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d gts\n", dst_reg, expr.arg2->int_literal, dst_reg);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d gts\n", dst_reg, arg2_dst_reg, dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d gts\n", dst_reg, arg2_dst_reg, dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprLogicalLe(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d ges\n", dst_reg, expr.arg1->int_literal, dst_reg);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d les\n", dst_reg, expr.arg2->int_literal, dst_reg);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d les\n", dst_reg, arg2_dst_reg, dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d les\n", dst_reg, arg2_dst_reg, dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprLogicalGe(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d les\n", dst_reg, expr.arg1->int_literal, dst_reg);
    } else if (expr.arg2->type == EXPR_INT) {
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        fprintf(ctx->dst, "\tcmp r%d %lld\n\tsetc r%d ges\n", dst_reg, expr.arg2->int_literal, dst_reg);
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d ges\n", dst_reg, arg2_dst_reg, dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmpr r%d r%d\n\tsetc r%d ges\n", dst_reg, arg2_dst_reg, dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprLogicalAnd(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (expr.arg1->int_literal != 0) {
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
            fprintf(ctx->dst, "\tcmp r%d 0\n\tsetc r%d neq\n", dst_reg, dst_reg);
        } else {
            fprintf(ctx->dst, "\tset r%d 0\n", dst_reg);
        }
    } else if (expr.arg2->type == EXPR_INT) {
        if (expr.arg2->int_literal != 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            fprintf(ctx->dst, "\tcmp r%d 0\n\tsetc r%d neq\n", dst_reg, dst_reg);
        } else {
            fprintf(ctx->dst, "\tset r%d 0\n", dst_reg);
        }
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmp r%d 0\n\tcmp:neq r%d 0\n\tsetc r%d neq\n", dst_reg, arg2_dst_reg, dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmp r%d 0\n\tcmp:neq r%d 0\n\tsetc r%d neq\n", dst_reg, arg2_dst_reg, dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprLogicalOr(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (expr.arg1->type == EXPR_INT) {
        if (expr.arg1->int_literal != 0) {
            fprintf(ctx->dst, "\tset r%d 1\n", dst_reg);
        } else {
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state, dst_reg)) return false;
            fprintf(ctx->dst, "\tcmp r%d 0\n\tsetc r%d neq\n", dst_reg, dst_reg);
        }
    } else if (expr.arg2->type == EXPR_INT) {
        if (expr.arg2->int_literal != 0) {
            fprintf(ctx->dst, "\tset r%d 1\n", dst_reg);
        } else {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            fprintf(ctx->dst, "\tcmp r%d 0\n\tsetc r%d neq\n", dst_reg, dst_reg);
        }
    } else {
        int arg2_dst_reg = -1;
        for (int i = 0; i < 8; i++) {
            if (reg_state & (1 << i) && i != dst_reg) {
                arg2_dst_reg = i;
                break;
            }
        }

        if (arg2_dst_reg >= 0) {
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmp r%d 0\n\tcmp:equ r%d 0\n\tsetc r%d neq\n", dst_reg, arg2_dst_reg, dst_reg);
        } else {
            arg2_dst_reg = (dst_reg + 1) % 8;
            reg_state &= ~(1 << arg2_dst_reg);
            int cache_id = compileRegCaching(ctx, 1 << arg2_dst_reg);
            if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
            if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
            fprintf(ctx->dst, "\tcmp r%d 0\n\tcmp:equ r%d 0\n\tsetc r%d neq\n", dst_reg, arg2_dst_reg, dst_reg);
            compileRegUncaching(ctx, 1 << arg2_dst_reg, cache_id);
        }
    }

    return true;
}

bool compileExprLogicalNot(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.loc);

    if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
    fprintf(ctx->dst, "\tcmp r%d 0\n\t setc r%d equ", dst_reg, dst_reg);
    return true;
}

ExprCompiler expr_compilers[] = {
    [EXPR_INVALID    ] = &compileExprInvalid,
    [EXPR_SYSCALL    ] = &compileExprSyscall,
    [EXPR_NAME       ] = &compileExprInvalid,
    [EXPR_BUILTIN    ] = &compileExprBuiltin,
    [EXPR_STRING     ] = &compileExprString,
    [EXPR_INT        ] = &compileExprInt,
    [EXPR_NEW_VAR    ] = &compileExprNewVar,
    [EXPR_SET_VAR    ] = &compileExprSetVar,
    [EXPR_GET_VAR    ] = &compileExprGetVar,
    [EXPR_BLOCK      ] = &compileExprBlock,
    [EXPR_TYPE       ] = &compileExprInvalid,
    [EXPR_REF        ] = &compileExprInvalid,
    [EXPR_FUNC_REF   ] = &compileExprInvalid,
    [EXPR_NEW_ARG    ] = &compileExprInvalid,
    [EXPR_FUNC_CALL  ] = &compileExprFuncCall,
    [EXPR_GET_ARG    ] = &compileExprGetArg,
    [EXPR_SET_ARG    ] = &compileExprSetArg,
    [EXPR_RETURN     ] = &compileExprReturn,
    [EXPR_ADD        ] = &compileExprAdd,
    [EXPR_SUB        ] = &compileExprSub,
    [EXPR_MUL        ] = &compileExprMul,
    [EXPR_DIV        ] = &compileExprDiv,
    [EXPR_AND        ] = &compileExprAnd,
    [EXPR_OR         ] = &compileExprOr,
    [EXPR_XOR        ] = &compileExprXor,
    [EXPR_SHL        ] = &compileExprShl,
    [EXPR_SHR        ] = &compileExprShr,
    [EXPR_NOT        ] = &compileExprNot,
    [EXPR_CAST       ] = &compileExprInvalid,
    [EXPR_LOGICAL_EQ ] = &compileExprLogicalEq,
    [EXPR_LOGICAL_NEQ] = &compileExprLogicalNeq,
    [EXPR_LOGICAL_LT ] = &compileExprLogicalLt,
    [EXPR_LOGICAL_GT ] = &compileExprLogicalGt,
    [EXPR_LOGICAL_LE ] = &compileExprLogicalLe,
    [EXPR_LOGICAL_GE ] = &compileExprLogicalGe,
    [EXPR_LOGICAL_AND] = &compileExprLogicalAnd,
    [EXPR_LOGICAL_OR ] = &compileExprLogicalOr,
    [EXPR_LOGICAL_NOT] = &compileExprLogicalNot
};
static_assert(N_EXPR_TYPES == 38, "not all expression types have corresponding compilers defined");

bool compileAST(AST* src, FILE* dst)
{
    ASTCompilerCtx ctx = {
        .data_blocks = strArray_new(0),
        .dst = dst
    };

    fputs("exec {\n", dst);
    chain_foreach(FuncDecl, func, src->functions,
        compileSrcRef(&ctx, func.loc);
        fprintf(dst, "\tproc %s\n", func.name);

        ctx.arg_cache_state = getArgCacheState(func.body);
        for (int i = 0; i < getSubexprsCount(&func.args) - 1; i++) {
            if (ctx.arg_cache_state & (1 << i)) {
                Expr* arg_decl = getSubexpr(&func.args, i + 1);
                fprintf(dst, "\tpushv %s %d r%d\n", getSubexpr(arg_decl, 0)->name, getTypeSize(*getSubexpr(arg_decl, 1)->var_type), i);
            }
        }

        if (!expr_compilers[func.body.type](&ctx, func.body, 0, 0)) return false;
        fputs("\tendproc\n", dst);
    );
    fputs("}\n", dst);

    if (ctx.data_blocks.length) {
        fputs("data {\n", dst);
        array_foreach(str, literal, ctx.data_blocks, 
            fprintf(dst, "\t"STR_PREFIX"%d \"", _literal);
            fputsbuf(dst, fromstr(literal));
            fputs("\"\n", dst);
        );
        fputs("}\n", dst);
    }

    strArray_clear(&ctx.data_blocks);
    return true;
}

void handleBRPError(BRP* obj)
{
    fprintTokenLoc(stderr, obj->error_loc);
    eprintf("preprocessor error: \"");
    printBRPErrorStr(stderr, obj);
    eputs("\"\n");
    exit(1);
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
        "\t--vbrb-output <path>   Output BRidge Assembly (`.vbrb` file) to <path>;\n"
        "\t\tif <path> is a directory, output will be at <path>/<source name>.vbrb;\n"
        "\t\tby default, BRidge Assembly is stored in a temporary file and is deleted after compilation\n"
        "\t-o <path>              Output BRidge Bytecode (`.brb` file) to <path>;\n"
        "\t\tif <path> is a directory, output will be at <path>/<source name>.brb;\n"
        "\t\tby default, BRidge Bytecode is saved at <source dir>/<source name>.brb\n",
        program_name
    );
}

int main(int argc, char* argv[])
{
    initBREnv();
    startTimer();
    TYPE_STR_LITERAL.kind = KIND_PTR;
    TypeDef str_lit_base = (TypeDef){ .kind = KIND_INT, .size = 1 };
    TYPE_STR_LITERAL.base = &str_lit_base;

    bool print_ast = false;
    char *input_path = NULL, *brb_output_path = NULL, *vbrb_output_path = NULL;
    for (int i = 1; i < argc; i++) {
        bool go_on = false;
        if (argv[i][0] == '-') {
			for (argv[i]++; *argv[i]; argv[i]++) {
				if (go_on) { go_on = false; break; }
                switch (*argv[i]) {
                    case 'h':
                        printUsageMsg(stdout, argv[0]);
                        return 0;
                    case 'd':
                        print_ast = true;
                        break;
                    case 'o':
						if (!argv[++i]) {
							eprintf("error: `-o` option specified but no executable output file path provided\n");
							return 1;
						}
						brb_output_path = argv[i];
						go_on = true;
						break;
                    case '-':
                        argv[i]++;
                        if (streq(argv[i], "vbrb-output")) {
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
    sbuf basename = fileBaseName_s(fromstr(input_path));

    if (brb_output_path) {
        if (isPathDir(brb_output_path)) {
            brb_output_path = tostr(fromstr(brb_output_path), PATHSEP, basename, fromcstr(BRB_EXT));
        }
    } else {
        brb_output_path = setFileExt(input_path, BRB_EXT);
    }

    if (vbrb_output_path) {
        if (isPathDir(vbrb_output_path))
            vbrb_output_path = tostr(fromstr(vbrb_output_path), PATHSEP, basename, fromcstr(VBRB_EXT));
    }

    char* vbrb_visual_output_path;
    if (!vbrb_output_path) {
        vbrb_visual_output_path = tostr(fromcstr("~"), setFileExt_s(fromstr(input_path), fromcstr(VBRB_EXT)));
    } else {
        vbrb_visual_output_path = vbrb_output_path;
    }
    
    BRP prep;
    if (!initBRP(&prep, &handleBRPError)) {
        eprintf("error: could not initialize the preprocessor due to memory shortage\n");
        return 1;
    }
    static_assert(N_SYMBOLS == 23, "not all symbols are handled");
    setSymbols(
        &prep,
        BRP_SYMBOL("("),
        BRP_SYMBOL(")"),
        BRP_SYMBOL(","),
        BRP_SYMBOL("{"),
        BRP_SYMBOL("}"),
        BRP_SYMBOL(";"),
        BRP_SYMBOL("=="),
        BRP_SYMBOL("!="),
        BRP_SYMBOL("<="),
        BRP_SYMBOL(">="),
        BRP_SYMBOL("="),
        BRP_SYMBOL("+"),
        BRP_SYMBOL("-"),
        BRP_SYMBOL("*"),
        BRP_SYMBOL("/"),
        BRP_SYMBOL("&"),
        BRP_SYMBOL("|"),
        BRP_SYMBOL("^"),
        BRP_SYMBOL("<<"),
        BRP_SYMBOL(">>"),
        BRP_SYMBOL("~"),
        BRP_SYMBOL("<"),
        BRP_SYMBOL(">"),
        BRP_HIDDEN_SYMBOL(" "),
        BRP_HIDDEN_SYMBOL("\t")
    );
    static_assert(N_KWS == 13, "not all keywords are handled");
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
        BRP_KEYWORD("not")
    );
	if (!setInput(&prep, input_path)) {
		printBRPError(stderr, &prep);
		return 1;
	}

    AST ast;
    BRError err = parseSourceCode(&prep, &ast);
    
    static_assert(N_BR_ERRORS == 31, "not all BRidge errors are handled");
    if (err.code) {
        fprintTokenLoc(stderr, err.loc.loc);
        eputs("error: ");
        switch (err.code) {
            case BR_ERR_INVALID_GLOBAL_STMT:
                eputs("expected a global statement specifier, instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_FUNC_NAME_EXPECTED:
                eputs("expected a word as function name specifier, instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_ARG_DEF_START_EXPECTED:
                eputs("expected ");
                fprintTokenStr(stderr, (Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_ARGSPEC_START }, &prep);
                eputs(", instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_ARG_DEF_EXPECTED:
                eputs("expected ");
                fprintTokenStr(stderr, (Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_COMMA }, &prep);
                eputs(" or ");
                fprintTokenStr(stderr, (Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_ARGSPEC_END }, &prep);
                eputs(", instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_ARG_NAME_EXPECTED:
                eputs("expected word as a name of the function argument, instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_INVALID_SYSCALL_NAME:
                eputs("expected word as the syscall name specifier, instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_INVALID_FUNC_DEF:
                eputs("invalid funtion definition\n");
                return 1;
            case BR_ERR_BUILTIN_NAME_EXPECTED:
                eputs("expected a word as the name of the built-in value, instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_INVALID_BUILTIN_NAME:
                eprintf("unknown built-in value `%s`\n", err.loc.word);
                return 1;
            case BR_ERR_INVALID_EXPR:
                eputs("invalid expression\n");
                return 1;
            case BR_ERR_TOO_MANY_SYSCALL_ARGS:
                eprintf("expected at most 6 arguments for a syscall, instead got %d\n", err.n_args);
                return 1;
            case BR_ERR_INVALID_VAR_NAME:
                eputs("expected a word as the variable name, instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_NO_VALUE_EXPR:
                eputs("expected a value\n");
                return 1;
            case BR_ERR_UNCLOSED_BLOCK:
                eputs("expected ");
                fprintTokenStr(stderr, (Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_BLOCK_END }, &prep);
                eputs(", instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_UNKNOWN_VAR:
                eprintf("unknown variable name `%s`\n", err.loc.word);
                return 1;
            case BR_ERR_VAR_EXISTS:
                eprintf("variable `%s` is aliready declared\n", err.loc.word);
                return 1;
            case BR_ERR_INVALID_TYPE:
                eputs("invalid type specifier\n");
                return 1;
            case BR_ERR_VAR_TYPE_MISMATCH: {
                char* var_name = getSubexpr(err.var_decl, 0)->name;
                eprintf("variable `%s` expects a value of type `", var_name);
                fprintType(stderr, *getSubexpr(err.var_decl, 1)->var_type);
                eputs("`, instead got a value of type `");
                fprintType(stderr, err.entry_type);
                eputs("`\n");
                fprintTokenLoc(stderr, err.var_decl->loc);
                eprintf("note: variable `%s` is declared here\n", var_name);
                return 1;
            } case BR_ERR_VOID_VAR_DECL:
                eputs("cannot declare a variable of type `");
                fprintType(stderr, VOID_TYPE);
                eputs("`\n");
                return 1;
            case BR_ERR_UNKNOWN_FUNC:
                eprintf("unknown function name `%s`\n", err.loc.word);
                return 1;
            case BR_ERR_TOO_MANY_FUNC_ARGS:
                eprintf("expected at most 6 arguments to a function, instead got %d", err.n_args);
                return 1;
            case BR_ERR_RETURN_TYPE_MISMATCH:
                eprintf("function `%s` is declared to return a value of type `", err.func->name);
                fprintType(stderr, err.func->return_type);
                eputs("`, instead attempted to return value of type `");
                fprintType(stderr, err.entry_type);
                eputs("`\n");
                fprintTokenLoc(stderr, err.func->loc);
                eprintf("note: function `%s` is declared here\n", err.func->name);
                return 1;
            case BR_ERR_ARG_COUNT_MISMATCH:
                if (err.n_args >= 0) {
                    eprintf(
                        "function `%s` expects exactly %d argument(s), instead got %d\n",
                        err.func->name, getSubexprsCount(&err.func->args) - 1, err.n_args
                    );
                } else {
                    eprintf(
                        "function `%s` expects exactly %d argument(s), instead got more\n",
                        err.func->name, getSubexprsCount(&err.func->args) - 1
                    );
                }
                fprintTokenLoc(stderr, err.func->loc);
                eprintf("note: function `%s` is declared here\n", err.func->name);
                return 1;
            case BR_ERR_ARG_TYPE_MISMATCH:
                if (err.func) {
                    eprintf("argument `%s` of function `%s` expects a value of type `", getSubexpr(err.arg_decl, 0)->name, err.func->name);
                    fprintType(stderr, *getSubexpr(err.arg_decl, 1)->var_type);
                } else {
                    eputs("a syscall argument expects a value of type `");
                    fprintType(stderr, BUILTIN_VAL_TYPE);
                }
                eputs("`, instead got value of type `");
                fprintType(stderr, err.entry_type);
                eputs("`\n");
                fprintTokenLoc(stderr, err.func->loc);
                eprintf("note: function `%s` is declared here\n", err.func->name);
                return 1;
            case BR_ERR_CAST_TYPE_EXPECTED:
                eputs("expected ");
                fprintTokenStr(stderr, (Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_COMMA }, &prep);
                eputs(", instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_ARG_DEF_END_EXPECTED:
                eputs("expected ");
                fprintTokenStr(stderr, (Token){ .type = TOKEN_SYMBOL, .symbol_id = SYMBOL_ARGSPEC_END }, &prep);
                eputs(", instead got ");
                fprintTokenStr(stderr, err.loc, &prep);
                eputc('\n');
                return 1;
            case BR_ERR_VOID_TYPE_CAST:
                eputs("cannot cast a value to type `");
                fprintType(stderr, VOID_TYPE);
                eputs("`\n");
                return 1;
            case BR_ERR_MAIN_PROC_RET_TYPE_MISMATCH:
                eputs("the `main` procedure must return nothing, instead the return type of `");
                fprintType(stderr, err.entry_type);
                eputs("` is declared\n");
                return 1;
            case BR_ERR_MAIN_PROC_ARG_COUNT_MISMATCH:
                eprintf("the `main` procedure must accept exactly 0 arguments, instead %d arguments were declared\n", err.n_args);
                return 1;
            case BR_ERR_COMPARISON_TYPE_MISMATCH:
                eputs("cannot compare a value of type `");
                fprintType(stderr, getExprValueType(*err.left_expr));
                eputs("` to a value of type `");
                fprintType(stderr, err.entry_type);
                eputs("`\n");
                return 1;
            case N_BR_ERRORS:
            case BR_ERR_NONE:
            default:
                eprintf("undefined error code %d\n", err.code);
                return 1;
        }
    }

    if (print_ast) {
        printAST(&ast, &prep);
        return 0;
    }

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

    if (!compileAST(&ast, vbrb_output)) {
        eprintf("internal compiler bug: could not write VBRB output\n");
        return 1;
    }

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
    printf("%s -> %s in %.3f ms\n", input_path, vbrb_visual_output_path, endTimer());

    startTimer();
    FILE* brb_output = fopen(brb_output_path, "wb");
    if (!brb_output) {
        eprintf("error: could not open file `%s` for writing BRB output (reason: %s)\n", brb_output_path, strerror(errno));
        return 1;
    }
    writeModule(&res, brb_output);
    printf("%s -> %s in %.3f ms\n", vbrb_visual_output_path, brb_output_path, endTimer());

    fclose(brb_output);
    fclose(vbrb_output);
    delBRP(&prep);

}