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
    N_KWS
} BRKeyword;

typedef enum {
    SYMBOL_ARGSPEC_START,
    SYMBOL_ARGSPEC_END,
    SYMBOL_COMMA,
    SYMBOL_BLOCK_START,
    SYMBOL_BLOCK_END,
    SYMBOL_SEMICOLON,
    SYMBOL_ASSIGNMENT,
    SYMBOL_PTR,
    SYMBOL_PLUS,
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
    N_BR_ERRORS
} BRErrorCode;


typedef enum {
    KIND_NONE,
    KIND_INT,
    KIND_PTR,
    KIND_BUILTIN_VAL,
    KIND_VOID,
    N_TYPE_KINDS
} TypeKind;

typedef struct typedef_t {
    TypeKind kind;
    union {
        int size;
        struct typedef_t* base;
    };
} TypeDef;

#define INT_TYPE(_size) ((TypeDef){ .kind = KIND_INT, .size = _size })
static TypeDef TYPE_STR_LITERAL;

typedef struct {
    TokenLoc loc;
    char* name;
    TypeDef type;
} VarDecl;
declArray(VarDecl);
defArray(VarDecl);

typedef enum {
    EXPR_INVALID,
    EXPR_SYSCALL, // main, composite, evaluatable
    EXPR_NAME, // auxillary
    EXPR_BUILTIN, // main, self-defined, evaluatable
    EXPR_STRING, // main, self-defined, evaluatable
    EXPR_INT, // main, self-defined, evaluatable
    EXPR_NEW_VAR, // main, composite, non-evaluatable
    EXPR_SET_VAR, // main, composite, evaluatable
    EXPR_GET_VAR, // main, self-defined, evaluatable
    EXPR_BLOCK, // main, composite, non-evaluatable
    EXPR_TYPE, // auxillary
    EXPR_REF, // auxillary
    EXPR_FUNC_CALL, // main, composite, evaluatable
    EXPR_NEW_ARG, // main, composite, non-evaluatable
    EXPR_GET_ARG, // main, self-defined, evaluatable
    EXPR_SET_ARG, // main, composite, evaluatable
    EXPR_RETURN, // main, self-defined, non-evaluatable
    EXPR_SUM, // main, composite, evaluatable
    N_EXPR_TYPES
} ExprType;

typedef struct expr {
    union {
        bool is_func_ref; // for EXPR_REF
        struct expr* arg1; // for EXPR_SUM
    };
    union {
        struct expr* ref; // for EXPR_REF
        struct expr* arg2; // for EXPR_SUM
        int64_t int_literal; // for EXPR_INT
        TypeDef* var_type; // for EXPR_TYPE
        char* name; // for EXPR_NAME
        char* str_literal; // for EXPR_STRING
    };
    struct expr* block;
    char* src_path;
    int src_line;
    ExprType type;
} Expr;
declChain(Expr);
defChain(Expr);

typedef struct {
    char* src_path;
    int src_line;
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
                default:
                    return (BRError){ .code = fetched ? 0 : -1 };
            }
        } else if (token.type == TOKEN_SYMBOL) {
            switch (token.symbol_id) {
                case SYMBOL_PTR:
                    if (dst->kind == KIND_NONE) return (BRError){
                        .code = BR_ERR_INVALID_TYPE,
                        .loc = token
                    };

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

static void initExpr(Expr* expr)
{
    *(ExprChain*)expr = ExprChain_new(0);
}

static ExprChain* getSubexprs(Expr* expr)
{
    return (ExprChain*)expr;
}

static Expr* addSubexpr(Expr* expr, Expr new) 
{
    return ExprChain_append(getSubexprs(expr), new);
}

static Expr* getSubexpr(Expr* expr, int id)
{
    return ExprChain_getref(*getSubexprs(expr), id);
}

static int getSubexprsCount(Expr* expr)
{
    return ExprChain_length(*getSubexprs(expr));
}

static bool setExprType(Expr* expr, ExprType new_type)
// changes the type of the expression if the new type is suitable in place of the current expression type
{
    static_assert(N_EXPR_TYPES == 18, "not all expression types are handled in setExprType");
// override_table contains information on which expression types can be replaced by which
    static bool override_table[N_EXPR_TYPES][N_EXPR_TYPES] = {
        [EXPR_INVALID  ] = {
            [EXPR_INVALID] = false,
            [EXPR_SYSCALL ... N_EXPR_TYPES - 1] = true 
        },
        [EXPR_SYSCALL  ] = { 
            [0 ... N_EXPR_TYPES - 1] = false,
            [EXPR_SUM] = true
        },
        [EXPR_NAME     ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_BUILTIN  ] = {
            [0 ... N_EXPR_TYPES - 1] = false,
            [EXPR_SUM] = true
        },
        [EXPR_STRING   ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_INT      ] = {
            [0 ... N_EXPR_TYPES - 1] = false,
            [EXPR_SUM] = true
        },
        [EXPR_REF      ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_NEW_VAR  ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_SET_VAR  ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_GET_VAR  ] = {
            [0 ... N_EXPR_TYPES - 1] = false,
            [EXPR_SET_VAR] = true,
            [EXPR_SUM] = true
        },
        [EXPR_BLOCK    ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_TYPE     ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_FUNC_CALL] = {
            [0 ... N_EXPR_TYPES - 1] = false,
            [EXPR_SUM] = true
        },
        [EXPR_NEW_ARG  ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_GET_ARG  ] = {
            [0 ... N_EXPR_TYPES - 1] = false,
            [EXPR_SET_ARG] = true,
            [EXPR_SUM] = true
        },
        [EXPR_SET_ARG  ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_RETURN   ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_SUM      ] = {
            [0 ... N_EXPR_TYPES - 1] = false,
            [EXPR_SUM] = true
        }
    };

    bool overridable = override_table[expr->type][new_type];
    if (overridable) expr->type = new_type;
    return overridable;
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
    static_assert(N_TYPE_KINDS == 5, "not all type kinds are handled in fprintType");
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
    } else {
        eprintf("internal compiler bug in fprintType: unknown type kind %d\n", type.kind);
        abort();
    }
}
#define printType(type) fprintType(stdout, type)

int getTypeSize(TypeDef type)
{
    static_assert(N_TYPE_KINDS == 5, "not all type kinds are handled in getTypeSize");
    switch (type.kind) {
        case KIND_INT: return type.size;
        case KIND_PTR: 
        case KIND_BUILTIN_VAL:
            return 8;
        case KIND_VOID: return 0;
        case KIND_NONE:
        default:
            eprintf("internal compiler bug in getTypeSize: unknown type kind %d\n", type.kind);
            abort();
    }
}

bool typeMatches(TypeDef field, TypeDef entry)
{
    static_assert(N_TYPE_KINDS == 5, "not all type kinds are handled in typeMatches");
    switch (field.kind) {
        case KIND_VOID: return entry.kind != KIND_PTR;
        case KIND_PTR: return entry.kind == KIND_PTR ? typeMatches(*field.base, *entry.base) : entry.kind == KIND_BUILTIN_VAL;
        case KIND_INT: return (entry.kind == KIND_INT && entry.size == field.size) || entry.kind == KIND_BUILTIN_VAL;
        case KIND_BUILTIN_VAL: return getTypeSize(entry) == 8;
        case KIND_NONE:
        default:
            eprintf("internal compiler bug in typeMatches: unknown field type kind %d\n", field.kind);
            abort();
    }
}

bool isExprEvaluatable(ExprType type) {
    static_assert(N_EXPR_TYPES == 18, "not all expression types are handled in isExprEvaluatable");
    static bool expr_evaluatability_info[N_EXPR_TYPES] = {
        [EXPR_INVALID  ] = false,
        [EXPR_SYSCALL  ] = true,
        [EXPR_NAME     ] = false,
        [EXPR_BUILTIN  ] = true,
        [EXPR_STRING   ] = true,
        [EXPR_INT      ] = true,
        [EXPR_NEW_VAR  ] = true,
        [EXPR_SET_VAR  ] = false, // TODO: make variable assignment expression evaluatable
        [EXPR_GET_VAR  ] = true,
        [EXPR_BLOCK    ] = false, // TODO: make blocks evaluate to their "return" value
        [EXPR_TYPE     ] = false,
        [EXPR_REF      ] = false,
        [EXPR_FUNC_CALL] = true,
        [EXPR_NEW_ARG  ] = false,
        [EXPR_GET_ARG  ] = true,
        [EXPR_SET_ARG  ] = true,
        [EXPR_RETURN   ] = false,
        [EXPR_SUM      ] = true
    };
    assert(inRange(type, 0, N_EXPR_TYPES));
    return expr_evaluatability_info[type];

}

void fprintExpr(FILE* dst, Expr expr, int indent_level)
{
    for (int i = 0; i < indent_level; i++) {
        fputc('\t', dst);
    }
    static_assert(N_EXPR_TYPES == 18, "not all expression types are handled in fprintExpr");

    fprintf(dst, "[%s:%d] ", expr.src_path, expr.src_line);
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
            fprintType(dst, *getSubexpr(expr.ref, 1)->var_type);
            fprintf(dst, " %s;`\n", getSubexpr(expr.ref, 0)->name);
            break;
        case EXPR_SET_VAR:
            fputs("SET_VAR\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_NEW_VAR:
            fputs("NEW_VAR\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_TYPE:
            fputs("TYPE ", dst);
            fprintType(dst, *expr.var_type);
            fputs(" \n", dst);
            break;
        case EXPR_REF:
            fputs("REF -> `", dst);
            if (expr.ref) {
                if (expr.ref->type == EXPR_NEW_VAR || expr.ref->type == EXPR_NEW_ARG) {
                    fprintType(dst, *getSubexpr(expr.ref, 1)->var_type);
                    fprintf(dst, " %s;", getSubexpr(expr.ref, 0)->name);
                } else if (expr.is_func_ref) {
                    fprintType(dst, ((FuncDecl*)expr.ref)->return_type);
                    fprintf(dst, " %s", ((FuncDecl*)expr.ref)->name);
                    ExprNode* first_arg = getSubexprs(&((FuncDecl*)expr.ref)->args)->start->next;
                    if (!first_arg) fputc('(', dst);
                    chain_foreach_from(Expr, arg, *getSubexprs(&((FuncDecl*)expr.ref)->args), 1, 
                        fputs(_arg == first_arg ? "(" : ", ", dst);
                        fprintType(dst, *getSubexpr(&arg, 1)->var_type);
                        fprintf(dst, " %s", getSubexpr(&arg, 0)->name);
                    );
                    fputs(");", dst);
                } else {
                    eprintf("internal compiler bug: EXPR_REF expression points to neither a variable declaration, nor to a function definition\n");
                    abort();
                }
            } else fputs("NULL", dst);
            fputs("`\n", dst);
            break;
        case EXPR_BLOCK:
            fputs("BLOCK\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_FUNC_CALL:
            fputs("FUNC_CALL\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_NEW_ARG:
            fputs("NEW_ARG:\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_GET_ARG:
            fputs("GET_ARG `", dst);
            fprintType(dst, *getSubexpr(expr.ref, 1)->var_type);
            fprintf(dst, " %s;`\n", getSubexpr(expr.ref, 0)->name);
            break;
        case EXPR_SET_ARG:
            fputs("SET_ARG:\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
            break;
        case EXPR_RETURN:
            fputs("RETURN:\n", dst);
            fprintExpr(dst, *expr.ref, indent_level + 1);
            break;
        case EXPR_SUM:
            fputs("SUM:\n", dst);
            fprintExpr(dst, *expr.arg1, indent_level + 1);
            fprintExpr(dst, *expr.arg2, indent_level + 1);
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
        for (Expr* expr = getSubexpr(block, 0)->ref; expr; expr = getSubexpr(expr, 2)->ref) {
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

TypeDef getVarType(Expr* block, char* name)
{
    Expr* var_decl = getVarDecl(block, name);
    return var_decl ? *getSubexpr(var_decl, 1)->var_type : (TypeDef){0};
}

TypeDef getExprValueType(Expr expr)
{
    static_assert(N_EXPR_TYPES == 18, "not all expression types are handled in getExprValueType");
    switch (expr.type) {
        case EXPR_INVALID:
        case EXPR_NAME:
        case EXPR_NEW_VAR:
        case EXPR_NEW_ARG:
        case EXPR_TYPE:
        case EXPR_REF:
        case EXPR_BLOCK:
        case EXPR_RETURN:
            return (TypeDef){0};
        case EXPR_BUILTIN:
        case EXPR_SYSCALL:
            return (TypeDef){ .kind = KIND_BUILTIN_VAL };
        case EXPR_STRING: return TYPE_STR_LITERAL;
        case EXPR_INT: return INT_TYPE(4);
        case EXPR_SET_VAR: return getVarType(expr.block, getSubexpr(&expr, 0)->name);
        case EXPR_GET_VAR: return getVarType(expr.block, expr.name);
        case EXPR_FUNC_CALL: return ((FuncDecl*)getSubexpr(&expr, 0)->ref)->return_type;
        case EXPR_GET_ARG: return *getSubexpr(expr.ref, 1)->var_type;
        case EXPR_SET_ARG: return *getSubexpr(getSubexpr(&expr, 0)->ref, 1)->var_type;
        case EXPR_SUM: {
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
        } case N_EXPR_TYPES:
        default:
            eprintf("internal compiler bug in getExprValueType: unknown expression type %d\n", expr.type);
            abort();
    }
}

int getVarIndex(Expr expr)
{
    int res = -1;
    for (Expr* expr_iter = &expr; expr_iter; expr_iter = getSubexpr(expr_iter, 2)->ref) {
        assert(expr_iter->type == EXPR_NEW_VAR || expr_iter->type == EXPR_NEW_ARG);
        res++;
    }
    return res;
}

void printAST(AST* ast, BRP* obj)
{
    chain_foreach(FuncDecl, func, ast->functions, 
        printf("[%s:%d] function %s:\n", func.src_path, func.src_line, func.name);
        puts("arguments:");
        printExpr(func.args, 0);
        puts("body:");
        printExpr(func.body, 0);
    );
}

BRError parseExpr(BRP* obj, Expr* dst, Expr* block, FuncDecl* func, AST* ast, int flags);

BRError parseFuncArgs(BRP* obj, Expr* dst, FuncDecl* func, AST* ast)
{
    Token arg_term;
    if (getTokenSymbolId(peekToken(obj)) != SYMBOL_ARGSPEC_END) {
        while (true) {
            Expr* arg = addSubexpr(dst, (Expr){0});
            BRError err = parseExpr(obj, arg, dst->block, func, ast, EXPRTERM_ARG | EXPR_EVALUATABLE);
            if (err.code) return err;
            arg_term = fetchToken(obj);
            if (arg_term.symbol_id == SYMBOL_ARGSPEC_END) {
                break;
            } else if (arg_term.symbol_id != SYMBOL_COMMA) return (BRError){
                .code = BR_ERR_ARG_DEF_EXPECTED,
                .loc = arg_term
            };
        }
        if (getTokenSymbolId(arg_term) != SYMBOL_ARGSPEC_END) return (BRError){
            .code = BR_ERR_ARG_DEF_EXPECTED,
            .loc = arg_term
        };
    } else fetchToken(obj);

    return (BRError){0};
}

BRError parseExpr(BRP* obj, Expr* dst, Expr* block, FuncDecl* func, AST* ast, int flags)
{
    static_assert(N_EXPR_TYPES == 18, "not all expression types are handled in parseExpr");
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
            if (!setExprType(dst, EXPR_NEW_VAR)) return (BRError){
                .code = BR_ERR_INVALID_EXPR,
                .loc = token
            };
            if (new_type.kind == KIND_VOID) return (BRError){
                .code = BR_ERR_VOID_VAR_DECL,
                .loc = token
            };
            initExpr(dst);
            dst->src_path = getTokenSrcPath(obj, token);
            dst->src_line = token.loc.lineno;

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
                .src_path = dst->src_path,
                .src_line = dst->src_line
            });

            TypeDef* new_var_type = malloc(sizeof(TypeDef));
            *new_var_type = new_type;
            addSubexpr(dst, (Expr){
                .type = EXPR_TYPE,
                .var_type = new_var_type,
                .block = block,
                .src_path = dst->src_path,
                .src_line = dst->src_line
            });
            addSubexpr(dst, (Expr){
                .type = EXPR_REF,
                .ref = getSubexpr(block, 0)->ref,
                .block = block,
                .src_path = dst->src_path,
                .src_line = dst->src_line
            });
            getSubexpr(block, 0)->ref = dst;
        } else if (token.type == TOKEN_INT) {
            if (!setExprType(dst, EXPR_INT)) return (BRError){
                .code = BR_ERR_INVALID_EXPR,
                .loc = token
            };
            fetchToken(obj);
            dst->src_path = getTokenSrcPath(obj, token);
            dst->src_line = token.loc.lineno;

            dst->int_literal = token.value;
        } else if (token.type == TOKEN_STRING) {
            if (!setExprType(dst, EXPR_STRING)) return (BRError){
                .code = BR_ERR_INVALID_EXPR,
                .loc = token
            };
            fetchToken(obj);
            dst->src_path = getTokenSrcPath(obj, token);
            dst->src_line = token.loc.lineno;

            dst->str_literal = token.word;
        } else if (token.type == TOKEN_KEYWORD) {
            switch (token.keyword_id) {
                case KW_SYS: {
                    if (!setExprType(dst, EXPR_SYSCALL)) return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token
                    };
                    fetchToken(obj);
                    initExpr(dst);
                    dst->src_path = getTokenSrcPath(obj, token);
                    dst->src_line = token.loc.lineno;
                    
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
                        .src_path = dst->src_path,
                        .src_line = dst->src_line
                    });

                    token = fetchToken(obj);
                    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) return (BRError){
                        .code = BR_ERR_ARG_DEF_START_EXPECTED,
                        .loc = token
                    };

                    Token arg_term = peekToken(obj);
                    BRError err = parseFuncArgs(obj, dst, func, ast);
                    if (err.code) return err;
                    if (getSubexprsCount(dst) > 7) return (BRError){
// syscalls cannot accept more than 6 arguments; 7 is 6 maximum arguments + the first expression, which holds the name of the syscall
                        .code = BR_ERR_TOO_MANY_SYSCALL_ARGS,
                        .loc = arg_term,
                        .n_args = getSubexprsCount(dst) - 1
                    };
                    break;
                } case KW_BUILTIN: {
                    if (!setExprType(dst, EXPR_BUILTIN)) return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token,
                    };
                    fetchToken(obj);
                    dst->src_path = getTokenSrcPath(obj, token);
                    dst->src_line = token.loc.lineno;

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
                    if (!setExprType(dst, EXPR_RETURN)) return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token
                    };
                    fetchToken(obj);
                    dst->src_path = getTokenSrcPath(obj, token);
                    dst->src_line = token.loc.lineno;

                    dst->ref = calloc(1, sizeof(Expr));
                    BRError err = parseExpr(obj, dst->ref, block, func, ast, EXPRTERM_FULL | EXPR_EVALUATABLE);
                    if (err.code) return err;

                    TypeDef return_type = getExprValueType(*dst->ref);
                    if (!typeMatches(func->return_type, return_type)) return (BRError){
                        .code = BR_ERR_RETURN_TYPE_MISMATCH,
                        .loc = token,
                        .func = func,
                        .entry_type = return_type
                    };
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
                            Expr* var_initializer = ExprChain_append(
                                getSubexprs(block),
                                (Expr){ 
                                    .type = EXPR_SET_VAR,
                                    .block = block,
                                    .src_path = getTokenSrcPath(obj, token),
                                    .src_line = token.loc.lineno
                                }
                            );
                            addSubexpr(var_initializer, (Expr){
                                .type = EXPR_REF,
                                .ref = dst,
                                .block = block,
                                .src_path = var_initializer->src_path,
                                .src_line = var_initializer->src_line
                            });
                            BRError err = parseExpr(
                                obj,
                                addSubexpr(var_initializer, (Expr){0}),
                                block,
                                func,
                                ast, 
                                EXPRTERM_FULL | EXPR_EVALUATABLE
                            );
                            if (err.code) return err;
                            break;
                        } case EXPR_GET_VAR:
                        case EXPR_GET_ARG: {
                            dst->type = dst->type == EXPR_GET_VAR ? EXPR_SET_VAR : EXPR_SET_ARG;
                            Expr* var_ref = dst->ref;
                            initExpr(dst);
                            addSubexpr(dst, (Expr){
                                .type = EXPR_REF,
                                .ref = var_ref,
                                .block = block,
                                .src_path = dst->src_path,
                                .src_line = dst->src_line
                            });

                            Token entry_loc = peekToken(obj);
                            BRError err = parseExpr(
                                obj,
                                addSubexpr(dst, (Expr){0}),
                                block,
                                func,
                                ast,
                                EXPRTERM_FULL | EXPR_EVALUATABLE
                            );
                            if (err.code) return err;

                            TypeDef var_type = *getSubexpr(var_ref, 1)->var_type;
                            TypeDef entry_type = getExprValueType(*getSubexpr(dst, 1));
                            if (!typeMatches(var_type, entry_type)) return (BRError){
                                .code = BR_ERR_VAR_TYPE_MISMATCH,
                                .loc = entry_loc,
                                .var_decl = var_ref,
                                .entry_type = entry_type
                            };
                            break;
                        } default: return (BRError){
                            .code = BR_ERR_INVALID_EXPR,
                            .loc = token
                        };
                    }
                    break;
                } case SYMBOL_BLOCK_START: {
                    if (!setExprType(dst, EXPR_BLOCK)) return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token
                    };
                    initExpr(dst);
                    dst->src_path = getTokenSrcPath(obj, token);
                    dst->src_line = token.loc.lineno;
                    addSubexpr(dst, (Expr){
                        .type = EXPR_REF,
                        .block = dst,
                        .src_path = dst->src_path,
                        .src_line = dst->src_line
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
                } case SYMBOL_PLUS: {
                    Expr* arg1 = malloc(sizeof(Expr));
                    *arg1 = *dst;
                    if (!setExprType(dst, EXPR_SUM)) return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token
                    };
                    dst->src_path = getTokenSrcPath(obj, token);
                    dst->src_line = token.loc.lineno;

                    dst->arg1 = arg1;
                    dst->arg2 = calloc(1, sizeof(Expr));
                    BRError err = parseExpr(obj, dst->arg2, block, func, ast, flags | EXPR_EVALUATABLE);
                    if (err.code) return err;
                    break;
                } default: return (BRError){
                    .code = BR_ERR_INVALID_EXPR,
                    .loc = token
                };
            }
        } else if (token.type == TOKEN_WORD) {
            Token name_spec_token = fetchToken(obj);
            dst->src_path = getTokenSrcPath(obj, token);
            dst->src_line = token.loc.lineno;
            
            token = peekToken(obj);
            if (getTokenSymbolId(token) == SYMBOL_ARGSPEC_START) {
                fetchToken(obj);
                if (!setExprType(dst, EXPR_FUNC_CALL)) return (BRError){
                    .code = BR_ERR_INVALID_EXPR,
                    .loc = name_spec_token
                };

                FuncDecl* def = getFuncDecl(ast, name_spec_token.word);
                if (!def) return (BRError){
                    .code = BR_ERR_UNKNOWN_FUNC,
                    .loc = name_spec_token
                };
                addSubexpr(dst, (Expr){
                    .type = EXPR_REF,
                    .is_func_ref = true,
                    .ref = (Expr*)def,
                    .block = block,
                    .src_path = getTokenSrcPath(obj, name_spec_token),
                    .src_line = name_spec_token.loc.lineno
                });

                token = peekToken(obj);
                BRError err = parseFuncArgs(obj, dst, func, ast);
                if (err.code) return err;

                int arg_count = getSubexprsCount(dst) - 1;
                if (arg_count > 7) return (BRError){
                    .code = BR_ERR_TOO_MANY_FUNC_ARGS,
                    .loc = token,
                    .n_args = getSubexprsCount(dst) - 1
                };
                if (arg_count != getSubexprsCount(&def->args) - 1) return (BRError){
                    .code = BR_ERR_ARG_COUNT_MISMATCH,
                    .loc = token,
                    .func = def,
                    .n_args = arg_count
                };
                if (arg_count > 0) {
                    ExprNode* arg_decl_iter = getSubexprs(&def->args)->start->next;
                    chain_foreach_from(Expr, arg_entry, *getSubexprs(dst), 1,
                        TypeDef arg_entry_type = getExprValueType(arg_entry);
                        if (!typeMatches(*getSubexpr(&arg_decl_iter->value, 1)->var_type, arg_entry_type)) return ((BRError){
                            .code = BR_ERR_ARG_TYPE_MISMATCH,
                            .loc = token,
                            .func = def,
                            .entry_type = arg_entry_type,
                            .arg_decl = &arg_decl_iter->value
                        });
                        arg_decl_iter = arg_decl_iter->next;
                    );
                }
            } else {
                Expr* var_decl = getVarDecl(block, name_spec_token.word);
                if (!var_decl) return (BRError){
                    .code = BR_ERR_UNKNOWN_VAR,
                    .loc = name_spec_token
                };
                dst->ref = var_decl;
                if (!setExprType(dst, var_decl->type == EXPR_NEW_ARG ? EXPR_GET_ARG : EXPR_GET_VAR)) return (BRError){
                    .code = BR_ERR_INVALID_EXPR,
                    .loc = name_spec_token
                };
            }
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

    if (!isExprEvaluatable(dst->type) && flags & EXPR_EVALUATABLE) return (BRError){
        .code = BR_ERR_NO_VALUE_EXPR,
        .loc = token
    };
    return (BRError){0};
}

BRError parseSourceCode(BRP* obj, AST* dst) // br -> temporary AST
{
    Token token;
    dst->functions = FuncDeclChain_new(0);
    dst->globals.type = EXPR_BLOCK;
    dst->globals.block = NULL;
    initExpr(&dst->globals);
    addSubexpr(&dst->globals, (Expr){ .type = EXPR_REF });

    while (!BRPempty(obj)) {
        TypeDef type;
        BRError type_err = parseType(obj, &type);
        if ((int)type_err.code > 0) return type_err;
        if (type_err.code == 0) { // parsing function declaration/definition
// fetching function name
            Token func_name_spec = fetchToken(obj);
            if (func_name_spec.type != TOKEN_WORD) return (BRError){
                .code = BR_ERR_FUNC_NAME_EXPECTED,
                .loc = func_name_spec
            };
            token = fetchToken(obj);
            FuncDecl* new_func = FuncDeclChain_append(
                &dst->functions,
                (FuncDecl){
                    .return_type = type,
                    .name = func_name_spec.word,
                    .src_path = getTokenSrcPath(obj, func_name_spec),
                    .src_line = func_name_spec.loc.lineno,
                    .args = (Expr){
                        .type = EXPR_BLOCK,
                        .block = &dst->globals,
                        .src_path = getTokenSrcPath(obj, token),
                        .src_line = token.loc.lineno
                    },
                }
            );
            initExpr(&new_func->args);
            Expr* arg_block_ref = addSubexpr(&new_func->args, (Expr){
                .type = EXPR_REF,
                .block = &new_func->args,
                .ref = NULL,
                .src_path = new_func->args.src_path,
                .src_line = new_func->args.src_line
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
                        .src_path = getTokenSrcPath(obj, type_spec),
                        .src_line = type_spec.loc.lineno
                    });
                    initExpr(new_arg);
                    addSubexpr(new_arg, (Expr){
                        .type = EXPR_NAME,
                        .block = &new_func->args,
                        .name = token.word,
                        .src_path = getTokenSrcPath(obj, token),
                        .src_line = token.loc.lineno
                    });
                    Expr* new_arg_type_spec = addSubexpr(new_arg, (Expr){
                        .type = EXPR_TYPE,
                        .block = &new_func->args,
                        .var_type = malloc(sizeof(TypeDef)),
                        .src_path = new_arg->src_path,
                        .src_line = new_arg->src_line
                    });
                    *new_arg_type_spec->var_type = type;
                    Expr* _tmp = addSubexpr(new_arg, (Expr){
                        .type = EXPR_REF,
                        .block = &new_func->args,
                        .ref = arg_block_ref->ref,
                        .src_path = new_arg->src_path,
                        .src_line = new_arg->src_line
                    });
                    arg_block_ref->ref = new_arg;
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
// parsing the function body
            token = peekToken(obj);
            if (getTokenSymbolId(token) != SYMBOL_SEMICOLON) {
                BRError err = parseExpr(obj, &new_func->body, &new_func->args, new_func, dst, EXPRTERM_FULL);
                if (err.code) return err;
                if (new_func->body.type == EXPR_NEW_VAR) return (BRError){
                    .code = BR_ERR_INVALID_FUNC_DEF,
                    .loc = token
                };
            } else fetchToken(obj);
        } else {
            token = fetchToken(obj);
            if (token.type != TOKEN_NONE) {
                return (BRError){
                    .code = BR_ERR_INVALID_GLOBAL_STMT,
                    .loc = token
                };
            }
        }
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

void compileSrcRef(ASTCompilerCtx* ctx, char* path, int line)
{
    sbuf path_s = fromstr(path);
    if (!sbufeq(ctx->cur_src_path, path_s)) {
        fprintf(
            ctx->dst, 
            "\t@f \"%.*s\"\n"
            "\t@l %d\n",
            unpack(path_s), line
        );
        ctx->cur_src_path = path_s;
        ctx->cur_src_line = line;
    } else if (line != ctx->cur_src_line) {
        fprintf(ctx->dst, "\t@l %d\n", line);
        ctx->cur_src_line = line;
    }
}

regstate_t getArgCacheState(Expr expr)
{
    static_assert(N_EXPR_TYPES == 18, "not all expression types are handled in getArgCacheState");
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
        case EXPR_RETURN: res |= getArgCacheState(*expr.ref); break;
        case EXPR_SUM:
            res |= getArgCacheState(*expr.arg1);
            res |= getArgCacheState(*expr.arg2);
            break;
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
    compileSrcRef(ctx, expr.src_path, expr.src_line);

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
    compileSrcRef(ctx, expr.src_path, expr.src_line);
    fprintf(ctx->dst, "\tsetb r%d %s\n", dst_reg, expr.name);
    return true;
}

bool compileExprString(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.src_path, expr.src_line);

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
    compileSrcRef(ctx, expr.src_path, expr.src_line);
    fprintf(ctx->dst, "\tset r%d %lld\n", dst_reg, expr.int_literal);
    return true;
}

bool compileExprNewVar(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.src_path, expr.src_line);
    fprintf(ctx->dst, "\tvar %s %d\n", getSubexpr(&expr, 0)->name, getTypeSize(*getSubexpr(&expr, 1)->var_type));
    return true;
}

bool compileExprSetVar(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.src_path, expr.src_line);

    Expr* var_decl = getSubexpr(&expr, 0);
    Expr* subexpr = getSubexpr(&expr, 1);
    if (!expr_compilers[subexpr->type](ctx, *subexpr, reg_state, dst_reg)) return false;
    fprintf(ctx->dst, "\tstrv %s r%d\n", getSubexpr(var_decl->ref, 0)->name, dst_reg);
    return true;
}

bool compileExprGetVar(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.src_path, expr.src_line);
    fprintf(ctx->dst, "\tldv r%d %s\n", dst_reg, getSubexpr(expr.ref, 0)->name);
    return true;
}

bool compileExprBlock(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.src_path, expr.src_line);
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        if (!expr_compilers[subexpr.type](ctx, subexpr, reg_state, 0)) return false;
    );
    return true;
}

bool compileExprFuncCall(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.src_path, expr.src_line);

    regstate_t reg_cache_state = reg_state;
    int cache_id = compileRegCaching(ctx, reg_cache_state);
    int i = 0;
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        if (!expr_compilers[subexpr.type](ctx, subexpr, (1 << i) - 1, i)) return false;
        i++;
    );
    fprintf(ctx->dst, "\tcall %s\n", ((FuncDecl*)getSubexpr(&expr, 0)->ref)->name);

    if (dst_reg != 0) fprintf(ctx->dst, "\tsetr r%d r0\n", dst_reg);
    compileRegUncaching(ctx, reg_cache_state, cache_id);

    return true;
}

bool compileExprGetArg(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    int arg_id = getVarIndex(*expr.ref);
    if (ctx->arg_cache_state & (1 << arg_id)) {
        fprintf(ctx->dst, "\tldv r%d %s\n", dst_reg, getSubexpr(expr.ref, 0)->name);
    } else {
        if (arg_id != dst_reg) fprintf(ctx->dst, "\tsetr r%d r%d\n", dst_reg, arg_id);
    }
    return true;
}

bool compileExprSetArg(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    compileSrcRef(ctx, expr.src_path, expr.src_line);
    Expr* subexpr = getSubexpr(&expr, 1);
    if (!expr_compilers[subexpr->type](ctx, *subexpr, reg_state, dst_reg)) return false;

    Expr* arg_decl = getSubexpr(&expr, 0);
    int arg_id = getVarIndex(*arg_decl);
    if (ctx->arg_cache_state & (1 << arg_id)) {
        fprintf(ctx->dst, "\tstrv %s r%d\n", getSubexpr(arg_decl->ref, 0)->name, dst_reg);
    } else {
        fprintf(ctx->dst, "\tsetr r%d r%d\n", arg_id, dst_reg);
    }
    return true;
}

bool compileExprReturn(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
{
    if (!expr_compilers[expr.ref->type](ctx, *expr.ref, reg_state, 0)) return false;
    fprintf(ctx->dst, "\tret\n");
    return true;
}

bool compileExprSum(ASTCompilerCtx* ctx, Expr expr, regstate_t reg_state, int dst_reg)
// TODO: handle the case when there are no free registers for the second argument
{
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
        if (!expr_compilers[expr.arg1->type](ctx, *expr.arg1, reg_state, dst_reg)) return false;
        if (!expr_compilers[expr.arg2->type](ctx, *expr.arg2, reg_state | dst_reg, arg2_dst_reg)) return false;
        fprintf(ctx->dst, "\taddr r%d r%d r%d\n", dst_reg, dst_reg, arg2_dst_reg);
    }

    return true;
}


ExprCompiler expr_compilers[] = {
    [EXPR_INVALID  ] = &compileExprInvalid,
    [EXPR_SYSCALL  ] = &compileExprSyscall,
    [EXPR_NAME     ] = &compileExprInvalid,
    [EXPR_BUILTIN  ] = &compileExprBuiltin,
    [EXPR_STRING   ] = &compileExprString,
    [EXPR_INT      ] = &compileExprInt,
    [EXPR_NEW_VAR  ] = &compileExprNewVar,
    [EXPR_SET_VAR  ] = &compileExprSetVar,
    [EXPR_GET_VAR  ] = &compileExprGetVar,
    [EXPR_BLOCK    ] = &compileExprBlock,
    [EXPR_TYPE     ] = &compileExprInvalid,
    [EXPR_REF      ] = &compileExprInvalid,
    [EXPR_NEW_ARG  ] = &compileExprInvalid,
    [EXPR_FUNC_CALL] = &compileExprFuncCall,
    [EXPR_GET_ARG  ] = &compileExprGetArg,
    [EXPR_SET_ARG  ] = &compileExprSetArg,
    [EXPR_RETURN   ] = &compileExprReturn,
    [EXPR_SUM      ] = &compileExprSum
};
static_assert(N_EXPR_TYPES == 18, "not all expression types have corresponding compilers defined");

bool compileAST(AST* src, FILE* dst)
{
    ASTCompilerCtx ctx = {
        .data_blocks = strArray_new(0),
        .dst = dst
    };

    fputs("exec {\n", dst);
    chain_foreach(FuncDecl, func, src->functions,
        compileSrcRef(&ctx, func.src_path, func.src_line);
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
    if (!initBRP(&prep)) {
        eprintf("error: could not initialize the preprocessor due to memory shortage\n");
        return 1;
    }
    static_assert(N_SYMBOLS == 9, "not all symbols are handled");
    setSymbols(
        &prep,
        BRP_SYMBOL("("),
        BRP_SYMBOL(")"),
        BRP_SYMBOL(","),
        BRP_SYMBOL("{"),
        BRP_SYMBOL("}"),
        BRP_SYMBOL(";"),
        BRP_SYMBOL("="),
        BRP_SYMBOL("*"),
        BRP_SYMBOL("+"),
        BRP_HIDDEN_SYMBOL(" "),
        BRP_HIDDEN_SYMBOL("\t")
    );
    static_assert(N_KWS == 8, "not all keywords are handled");
    setKeywords(
        &prep,
        BRP_KEYWORD("void"),
        BRP_KEYWORD("int8"),
        BRP_KEYWORD("int16"),
        BRP_KEYWORD("int32"),
        BRP_KEYWORD("int64"),
        BRP_KEYWORD("sys"),
        BRP_KEYWORD("builtin"),
        BRP_KEYWORD("return")
    );
	if (!setInput(&prep, input_path)) {
		printBRPError(stderr, &prep);
		return 1;
	}

    AST ast;
    BRError err = parseSourceCode(&prep, &ast);
    
    static_assert(N_BR_ERRORS == 25, "not all BRidge errors are handled");
    if (err.code) {
        fprintTokenLoc(stderr, err.loc.loc, &prep);
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
                eprintf("`\n[ %s:%d ] note: variable `%s` is declared here\n", err.var_decl->src_path, err.var_decl->src_line, var_name);
                return 1;
            } case BR_ERR_VOID_VAR_DECL:
                eputs("cannot declare a variable of type `");
                fprintType(stderr, (TypeDef){ .kind = KIND_VOID });
                eputs("`\n");
                return 1;
            case BR_ERR_UNKNOWN_FUNC:
                eprintf("unknown function name %s\n", err.loc.word);
                return 1;
            case BR_ERR_TOO_MANY_FUNC_ARGS:
                eprintf("expected at most 6 arguments to a function, instead got %d", err.n_args);
                return 1;
            case BR_ERR_RETURN_TYPE_MISMATCH:
                eprintf("function `%s` is declared to return a value of type `", err.func->name);
                fprintType(stderr, err.func->return_type);
                eputs("`, instead attempted to return value of type `");
                fprintType(stderr, err.entry_type);
                eprintf("`\n[ %s:%d ] note: function `%s` is declared here\n", err.func->src_path, err.func->src_line, err.func->name);
                return 1;
            case BR_ERR_ARG_COUNT_MISMATCH:
                eprintf(
                    "function `%s` expects exactly %d argument(s), instead got %d\n"
                    "[ %s:%d ] note: function `%s` is declared here\n",
                    err.func->name, getSubexprsCount(&err.func->args) - 1, err.n_args,
                    err.func->src_path, err.func->src_line, err.func->name
                );
                return 1;
            case BR_ERR_ARG_TYPE_MISMATCH:
                eprintf("argument `%s` of function `%s` expects a value of type `", getSubexpr(err.arg_decl, 0)->name, err.func->name);
                fprintType(stderr, *getSubexpr(err.arg_decl, 1)->var_type);
                eputs("`, instead got value of type `");
                fprintType(stderr, err.entry_type);
                eprintf("`\n[ %s:%d ] note: function `%s` is declared here\n", err.func->src_path, err.func->src_line, err.func->name);
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