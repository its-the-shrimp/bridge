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
    SYMBOL_REF,
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
    BR_ERR_TYPE_MISMATCH,
    BR_ERR_VOID_VAR_DECL,
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
        int int_size;
        struct typedef_t* base;
    };
} TypeDef;

static TypeDef STR_LITERAL_TYPE;

typedef struct {
    BRErrorCode code;
    Token loc;
    union {
        int n_syscall_args; // for BR_ERR_TOO_MANY_SYSCALL_ARGS
        struct { // for BR_ERR_VAR_TYPE_MISMATCH
            TypeDef field_type;
            TypeDef entry_type;
        };
    };
} BRError;

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
    EXPR_SET_VAR, // main, composite, non-evaluatable
    EXPR_GET_VAR, // main, self-defined, evaluatable
    EXPR_BLOCK, // main, composite, non-evaluatable
    EXPR_TYPE, // auxillary
    EXPR_REF, // auxillary
    N_EXPR_TYPES
} ExprType;

typedef struct expr {
    long _; // padding
    union {
        void* subexprs;
        struct expr* ref;
        int64_t int_literal;
        TypeDef var_type;
        char* name;
        char* str_literal;
    };
    struct expr* block;
    ExprType type;
} Expr;
declChain(Expr);
defChain(Expr);

typedef struct {
    TokenLoc loc;
    char* name;
    VarDeclArray args;
    TypeDef return_type;
    Expr body;
} FuncDef;
declArray(FuncDef);
defArray(FuncDef);

typedef struct {
    FuncDefArray functions;
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
                    dst->int_size = 1 << (token.keyword_id - KW_INT8);
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
                case SYMBOL_REF:
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
    static_assert(N_EXPR_TYPES == 12, "not all expression types are handled in setExprType");
// override_table contains information on which expression types can be replaced by which
    static bool override_table[N_EXPR_TYPES][N_EXPR_TYPES] = {
        [EXPR_INVALID] = {
            [EXPR_INVALID] = false,
            [EXPR_SYSCALL ... N_EXPR_TYPES - 1] = true 
        },
        [EXPR_SYSCALL] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_NAME   ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_BUILTIN] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_STRING ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_INT    ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_NEW_VAR] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_SET_VAR] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_GET_VAR] = {
            [0 ... N_EXPR_TYPES - 1] = false,
            [EXPR_SET_VAR] = true
        },
        [EXPR_BLOCK  ] = { [0 ... N_EXPR_TYPES - 1] = false },
        [EXPR_TYPE   ] = { [0 ... N_EXPR_TYPES - 1] = false }
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
        switch (type.int_size) {
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

bool typeMatches(TypeDef field, TypeDef entry)
{
    static_assert(N_TYPE_KINDS == 5, "not all type kinds are handled in typeMatches");
    switch (field.kind) {
        case KIND_VOID: return entry.kind != KIND_PTR;
        case KIND_PTR: return entry.kind == KIND_PTR ? typeMatches(*field.base, *entry.base) : false;
        case KIND_INT: return entry.kind == KIND_INT && entry.int_size == field.int_size;
        case KIND_BUILTIN_VAL: return entry.kind == KIND_PTR || entry.kind == KIND_INT;
        case KIND_NONE:
        default:
            eprintf("internal compiler bug in typeMatches: unknown field type kind %d\n", field.kind);
            abort();
    }
}

int getTypeSize(TypeDef type)
{
    switch (type.kind) {
        case KIND_INT: return type.int_size;
        case KIND_PTR: return 8;
        case KIND_VOID: return 0;
        case KIND_NONE:
        default:
            eprintf("internal compiler bug in getTypeSize: unknown type kind %d\n", type.kind);
            abort();
    }
}

bool isExprComposite(ExprType type) {
    static bool expr_compositeness_info[N_EXPR_TYPES] = {
        [EXPR_INVALID] = false,
        [EXPR_SYSCALL] = true,
        [EXPR_NAME   ] = false,
        [EXPR_BUILTIN] = false,
        [EXPR_STRING ] = false,
        [EXPR_INT    ] = false,
        [EXPR_NEW_VAR] = true,
        [EXPR_SET_VAR] = true,
        [EXPR_GET_VAR] = false,
        [EXPR_BLOCK  ] = true,
        [EXPR_TYPE   ] = false,
        [EXPR_REF    ] = false,
    };
    assert(inRange(type, 0, N_EXPR_TYPES));
    return expr_compositeness_info[type];
}

bool isExprEvaluatable(ExprType type) {
    static bool expr_evaluatability_info[N_EXPR_TYPES] = {
        [EXPR_INVALID] = false,
        [EXPR_SYSCALL] = true,
        [EXPR_NAME   ] = false,
        [EXPR_BUILTIN] = true,
        [EXPR_STRING ] = true,
        [EXPR_INT    ] = true,
        [EXPR_NEW_VAR] = true,
        [EXPR_SET_VAR] = false, // TODO: make variable assignment expression evaluatable
        [EXPR_GET_VAR] = true,
        [EXPR_BLOCK  ] = false, // TODO: make blocks evaluate to their "return" value
        [EXPR_TYPE   ] = false,
        [EXPR_REF    ] = false,

    };
    assert(inRange(type, 0, N_EXPR_TYPES));
    return expr_evaluatability_info[type];

}

void fprintExpr(FILE* dst, Expr expr, int indent_level)
{
    for (int i = 0; i < indent_level; i++) {
        fputc('\t', dst);
    }
    static_assert(N_EXPR_TYPES == 12, "not all expression types are handled");
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
            fprintf(dst, "GET_VAR %s\n", expr.str_literal);
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
            fprintType(dst, expr.var_type);
            fputs(" \n", dst);
            break;
        case EXPR_REF:
            fprintf(dst, "REF @%p\n", expr.ref);
            break;
        case EXPR_BLOCK:
            fputs("BLOCK\n", dst);
            chain_foreach(Expr, subexpr, *getSubexprs(&expr), fprintExpr(dst, subexpr, indent_level + 1); );
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
    assert(block->type == EXPR_BLOCK);
    for (Expr* expr = getSubexpr(block, 0)->ref; expr; expr = getSubexpr(expr, 2)->ref) {
        assert(expr->type == EXPR_NEW_VAR);
        if (streq(name, getSubexpr(expr, 0)->name)) return expr;
    }

    return NULL;
}

TypeDef getVarType(Expr* block, char* name)
{
    Expr* var_decl = getVarDecl(block, name);
    return var_decl ? getSubexpr(var_decl, 1)->var_type : (TypeDef){0};
}

TypeDef getExprValueType(Expr expr)
{
    static_assert(N_EXPR_TYPES == 12, "not all expression types are handled in getExprValueType");
    switch (expr.type) {
        case EXPR_INVALID:
        case EXPR_NAME:
        case EXPR_NEW_VAR:
        case EXPR_TYPE:
        case EXPR_REF:
        case EXPR_BLOCK:
            return (TypeDef){0};
        case EXPR_BUILTIN:
        case EXPR_SYSCALL:
            return (TypeDef){ .kind = KIND_BUILTIN_VAL };
        case EXPR_STRING: return STR_LITERAL_TYPE;
        case EXPR_INT: return (TypeDef){ .kind = KIND_INT, .int_size = 4 };
        case EXPR_SET_VAR: return getVarType(expr.block, getSubexpr(&expr, 0)->name);
        case EXPR_GET_VAR: return getVarType(expr.block, expr.name);
        case N_EXPR_TYPES:
        default:
            eprintf("internal compiler bug in getExprValueType: unknown expression type %d\n", expr.type);
            abort();
    }
}

void printAST(AST* ast, BRP* obj)
{
    array_foreach(FuncDef, func, ast->functions, 
        printTokenLoc(func.loc, obj);
        printf("function %s:\n", func.name);
        printExpr(func.body, 0);
    );
}

BRError parseExpr(BRP* obj, Expr* dst, Expr* block, int flags)
{
    static_assert(N_EXPR_TYPES == 12, "not all expression types are handled");
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

            token = fetchToken(obj);
            if (token.type != TOKEN_WORD) return (BRError){
                .code = BR_ERR_INVALID_VAR_NAME,
                .loc = token
            };
            if (getVarDecl(block, token.word)) return (BRError){
                .code = BR_ERR_VAR_EXISTS,
                .loc = token
            };
            addSubexpr(dst, (Expr){ .type = EXPR_NAME, .str_literal = token.word, .block = block });

            addSubexpr(dst, (Expr){ .type = EXPR_TYPE, .var_type = new_type, .block = block });
            addSubexpr(dst, (Expr){ .type = EXPR_REF, .ref = getSubexpr(block, 0)->ref, .block = block });
            getSubexpr(block, 0)->ref = dst;
        } else if (token.type == TOKEN_INT) {
            fetchToken(obj);
            if (!setExprType(dst, EXPR_INT)) return (BRError){
                .code = BR_ERR_INVALID_EXPR,
                .loc = token
            };
            dst->int_literal = token.value;
        } else if (token.type == TOKEN_STRING) {
            fetchToken(obj);
            if (!setExprType(dst, EXPR_STRING)) return (BRError){
                .code = BR_ERR_INVALID_EXPR,
                .loc = token
            };
            dst->str_literal = token.word;
        } else if (token.type == TOKEN_KEYWORD) {
            switch (token.keyword_id) {
                case KW_SYS: {
                    fetchToken(obj);
                    if (!setExprType(dst, EXPR_SYSCALL)) return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token
                    };
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
                    addSubexpr(dst, (Expr){ .type = EXPR_NAME, .name = token.word });

                    token = fetchToken(obj);
                    if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) return (BRError){
                        .code = BR_ERR_ARG_DEF_START_EXPECTED,
                        .loc = token
                    };

                    if (getTokenSymbolId(peekToken(obj)) != SYMBOL_ARGSPEC_END) {
                        Token arg_term;
                        while (true) {
                            Expr* arg = addSubexpr(dst, (Expr){0});
                            BRError err = parseExpr(obj, arg, block, EXPRTERM_ARG | EXPR_EVALUATABLE);
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
                            .loc = token
                        };
                        if (getSubexprsCount(dst) > 7) return (BRError){
// syscalls cannot accept more than 6 arguments; 7 is 6 maximum arguments + the first expression, which holds the name of the syscall
                            .code = BR_ERR_TOO_MANY_SYSCALL_ARGS,
                            .loc = arg_term,
                            .n_syscall_args = getSubexprsCount(dst) - 1
                        };
                    } else fetchToken(obj);
                    break;
                } case KW_BUILTIN: {
                    if (!setExprType(dst, EXPR_BUILTIN)) return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token,
                    };
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
                } default: return (BRError){
                    .code = BR_ERR_INVALID_EXPR,
                    .loc = token
                };
            }
        } else if (token.type == TOKEN_SYMBOL) {
            switch (token.symbol_id) {
                case SYMBOL_ASSIGNMENT: {
                    if (dst->type == EXPR_NEW_VAR) {
                        fetchToken(obj);
                        Expr* var_initializer = ExprChain_append(getSubexprs(block), (Expr){ .type = EXPR_SET_VAR, .block = block });
                        addSubexpr(var_initializer, (Expr){ .type = EXPR_NAME, .name = getSubexpr(dst, 0)->name });
                        BRError err = parseExpr(obj, addSubexpr(var_initializer, (Expr){0}), block, EXPRTERM_FULL | EXPR_EVALUATABLE);
                        if (err.code) return err;
                    } else if (dst->type == EXPR_GET_VAR) {
                        fetchToken(obj);
                        dst->type = EXPR_SET_VAR;
                        char* var_name = dst->name;
                        initExpr(dst);
                        addSubexpr(dst, (Expr){ .type = EXPR_NAME, .name = var_name, .block = block });

                        Token entry_loc = peekToken(obj);
                        BRError err = parseExpr(obj, addSubexpr(dst, (Expr){0}), block, EXPRTERM_FULL | EXPR_EVALUATABLE);
                        if (err.code) return err;

                        TypeDef var_type = getVarType(block, var_name);
                        TypeDef entry_type = getExprValueType(*getSubexpr(dst, 1));
                        if (!typeMatches(var_type, entry_type)) return (BRError){
                            .code = BR_ERR_TYPE_MISMATCH,
                            .loc = entry_loc,
                            .field_type = var_type,
                            .entry_type = entry_type
                        };
                    } else return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token
                    };

                    break;
                } case SYMBOL_BLOCK_START: {
                    if (!setExprType(dst, EXPR_BLOCK)) return (BRError){
                        .code = BR_ERR_INVALID_EXPR,
                        .loc = token
                    };
                    fetchToken(obj);

                    initExpr(dst);
                    addSubexpr(dst, (Expr){ .type = EXPR_REF, .block = dst });

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
                        BRError err = parseExpr(obj, expr, dst, EXPRTERM_FULL);
                        if (err.code) return err;

                        fetchToken(obj);
                    }
                    break;
                } default: return (BRError){
                    .code = BR_ERR_INVALID_EXPR,
                    .loc = token
                };
            }
        } else if (token.type == TOKEN_WORD) {
            if (!setExprType(dst, EXPR_GET_VAR)) return (BRError){
                .code = BR_ERR_INVALID_EXPR,
                .loc = token
            };
            fetchToken(obj);

            if (!getVarDecl(block, token.word)) return (BRError){
                .code = BR_ERR_UNKNOWN_VAR,
                .loc = token
            };

            dst->name = token.word;
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
    dst->functions = FuncDefArray_new(0);
    dst->globals.type = EXPR_BLOCK;
    initExpr(&dst->globals);
    addSubexpr(&dst->globals, (Expr){ .type = EXPR_REF });

    while (!BRPempty(obj)) {
        TypeDef type;
        BRError type_err = parseType(obj, &type);
        if ((int)type_err.code > 0) return type_err;
        if (type_err.code == 0) { // parsing function declaration/definition
// fetching function name
            token = fetchToken(obj);                         // void main() { ... }
            if (token.type != TOKEN_WORD) return (BRError){ //      ^
                .code = BR_ERR_FUNC_NAME_EXPECTED,
                .loc = token
            };
            FuncDef* new_func = FuncDefArray_append(
                &dst->functions,
                (FuncDef){
                    .return_type = type,
                    .name = token.word,
                    .loc = token.loc,
                    .args = VarDeclArray_new(0),
                }
            );

            token = fetchToken(obj);                                               // void main() { ... }
            if (getTokenSymbolId(token) != SYMBOL_ARGSPEC_START) return (BRError){ //          ^
                .code = BR_ERR_ARG_DEF_START_EXPECTED,
                .loc = token
            };
// fetching function prototype specification
            while (true) {
                BRError err = parseType(obj, &type);
                if ((int)err.code > 0) return err;
                if (err.code == 0) {
                    token = fetchToken(obj);
                    if (token.type != TOKEN_WORD) return (BRError){
                        .code = BR_ERR_ARG_NAME_EXPECTED,
                        .loc = token
                    };
                    VarDeclArray_append(&new_func->args, (VarDecl){ .loc = token.loc, .name = token.word, .type = type });
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
                BRError err = parseExpr(obj, &new_func->body, &dst->globals, EXPRTERM_FULL);
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
} ASTCompilerCtx;

typedef bool (*ExprCompiler) (FILE*, Expr, ASTCompilerCtx*, regstate_t, int);
ExprCompiler expr_compilers[N_EXPR_TYPES];

static int reg_cache_counter = 0;
int compileRegCaching(FILE* dst, regstate_t state)
{
    for (uint8_t i = 0; i < 8; i++) {
        if (state & (1 << i)) {
            fprintf(dst, "\tpushv .rc%d_%hhd 8 r%hhd\n", reg_cache_counter, i, i);
        }
    }

    return reg_cache_counter++;
}

void compileRegUncaching(FILE* dst, regstate_t state, int cache_id)
{
    for (int8_t i = 7; i >= 0; i--) {
        if (state & (1 << i)) {
            fprintf(dst, "\tpopv r%hhd\n", i);
        }
    }
}

bool compileExprInvalid(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
    return false;
}

bool compileExprSyscall(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
    regstate_t reg_cache_state = ((1 << (getSubexprsCount(&expr) - 1)) - 1) & reg_state;
    int cache_id = compileRegCaching(dst, reg_cache_state);
    int i = 0;
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        if (!expr_compilers[subexpr.type](dst, subexpr, ctx, (1 << i) - 1, i)) return false;
        i++;
    );
    fprintf(dst, "\tsys %s\n", getSubexpr(&expr, 0)->name);

    if (dst_reg != 0) fprintf(dst, "\tsetr r%d r0\n", dst_reg);
    compileRegUncaching(dst, reg_cache_state, cache_id);

    return true;
}

bool compileExprBuiltin(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
    fprintf(dst, "\tsetb r%d %s\n", dst_reg, expr.name);
    return true;
}

bool compileExprString(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
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

    fprintf(dst, "\tsetd r%d "STR_PREFIX"%d\n", dst_reg, str_index);
    return true;
}

bool compileExprInt(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
    fprintf(dst, "\tset r%d %lld\n", dst_reg, expr.int_literal);
    return true;
}

bool compileExprNewVar(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
    fprintf(dst, "\tvar %s %d\n", getSubexpr(&expr, 0)->name, getTypeSize(getSubexpr(&expr, 1)->var_type));
    return true;
}

bool compileExprSetVar(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
    Expr* subexpr = getSubexpr(&expr, 1);
    if (!expr_compilers[subexpr->type](dst, *subexpr, ctx, reg_state, dst_reg)) return false;
    fprintf(dst, "\tstrv %s r%d\n", getSubexpr(&expr, 0)->name, dst_reg);
    return true;
}

bool compileExprGetVar(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
    fprintf(dst, "\tldv r%d %s\n", dst_reg, expr.name);
    return true;
}

bool compileExprBlock(FILE* dst, Expr expr, ASTCompilerCtx* ctx, regstate_t reg_state, int dst_reg)
{
    chain_foreach_from(Expr, subexpr, *getSubexprs(&expr), 1,
        if (!expr_compilers[subexpr.type](dst, subexpr, ctx, reg_state, 0)) return false;
    );
    return true;
}

ExprCompiler expr_compilers[] = {
    [EXPR_INVALID] = &compileExprInvalid,
    [EXPR_SYSCALL] = &compileExprSyscall,
    [EXPR_NAME   ] = &compileExprInvalid,
    [EXPR_BUILTIN] = &compileExprBuiltin,
    [EXPR_STRING ] = &compileExprString,
    [EXPR_INT    ] = &compileExprInt,
    [EXPR_NEW_VAR] = &compileExprNewVar,
    [EXPR_SET_VAR] = &compileExprSetVar,
    [EXPR_GET_VAR] = &compileExprGetVar,
    [EXPR_BLOCK  ] = &compileExprBlock,
    [EXPR_TYPE   ] = &compileExprInvalid,
    [EXPR_REF    ] = &compileExprInvalid,
};
const int x = sizeof(expr_compilers) / sizeof(expr_compilers[0]);
static_assert(sizeof(expr_compilers) / sizeof(ExprCompiler) == N_EXPR_TYPES, "not all expression types are handled");

bool compileAST(AST* src, FILE* dst)
{
    ASTCompilerCtx ctx = {
        .data_blocks = strArray_new(0),
    };

    fputs("exec {\n", dst);
    array_foreach(FuncDef, func, src->functions,
        fprintf(dst, "\tproc %s\n", func.name);
        if (!expr_compilers[func.body.type](dst, func.body, &ctx, 0, 0)) return false;
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
        "\t-h    display this message and quit\n"
        "\t-d    print the generated syntax tree and quit\n",
        program_name
    );
}

int main(int argc, char* argv[])
{
    initBREnv();
    startTimer();
    STR_LITERAL_TYPE.kind = KIND_PTR;
    TypeDef str_lit_base = (TypeDef){ .kind = KIND_INT, .int_size = 1 };
    STR_LITERAL_TYPE.base = &str_lit_base;

    bool go_on = false, print_ast = false;
    char *input_path = NULL, *brb_output_path = NULL, *vbrb_output_path = NULL;
    for (int i = 1; i < argc; i++) {
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
                    case '-':
                        argv[i]++;
                        if (streq(argv[i], "brb-output")) {
                            if (!argv[++i]) {
                                eprintf("error: `--brb-output` option specified but no path is provided\n");
                                return 1;
                            }
                            brb_output_path = argv[i];
                            go_on = true;
                        } else if (streq(argv[i], "vbrb-output")) {
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
    static_assert(N_SYMBOLS == 8, "not all symbols are handled");
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
        BRP_HIDDEN_SYMBOL(" "),
        BRP_HIDDEN_SYMBOL("\t")
    );
    static_assert(N_KWS == 7, "not all keywords are handled");
    setKeywords(
        &prep,
        BRP_KEYWORD("void"),
        BRP_KEYWORD("int8"),
        BRP_KEYWORD("int16"),
        BRP_KEYWORD("int32"),
        BRP_KEYWORD("int64"),
        BRP_KEYWORD("sys"),
        BRP_KEYWORD("builtin")
    );
	if (!setInput(&prep, input_path)) {
		printBRPError(stderr, &prep);
		return 1;
	}

    AST ast;
    BRError err = parseSourceCode(&prep, &ast);
    
    static_assert(N_BR_ERRORS == 20, "not all BRidge errors are handled");
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
                eprintf("syscalls expect not more than 6 arguments, instead got %d\n", err.n_syscall_args);
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
            case BR_ERR_TYPE_MISMATCH:
                eputs("cannot assign a value of type `");
                fprintType(stderr, err.entry_type);
                eputs("` to a variable of type `");
                fprintType(stderr, err.field_type);
                eputs("`\n");
                return 1;
            case BR_ERR_VOID_VAR_DECL:
                eputs("cannot declare a variable of type `");
                fprintType(stderr, (TypeDef){ .kind = KIND_VOID });
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
    writeModule(&res, brb_output);
    printf("%s -> %s in %.3f ms\n", vbrb_visual_output_path, brb_output_path, endTimer());

    fclose(brb_output);
    fclose(vbrb_output);
    delBRP(&prep);

}