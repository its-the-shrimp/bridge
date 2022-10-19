// implementation of the BRB Assembler
#include <br.h>
#define BRP_IMPLEMENTATION
#define _SBUF_IMPL_LOCK
#define _BR_UTILS_IMPL_LOCK
#include <brp.h>

defArray(BR_Type);

typedef enum {
	BR_KW_NOP,
	BR_KW_END,
	BR_KW_I8,
	BR_KW_I16,
	BR_KW_I32,
	BR_KW_PTR,
	BR_KW_I64,
	BR_KW_ADDR,
	BR_KW_DBADDR,
	BR_KW_SYS,
	BR_KW_BUILTIN,
	BR_KW_ADD,
	BR_KW_ADDI,
	BR_KW_ADDIAT8,
	BR_KW_ADDIAT16,
	BR_KW_ADDIAT32,
	BR_KW_ADDIATP,
	BR_KW_ADDIAT64,
	BR_KW_SUB,
	BR_KW_SUBI,
	BR_KW_SUBIAT8,
	BR_KW_SUBIAT16,
	BR_KW_SUBIAT32,
	BR_KW_SUBIATP,
	BR_KW_SUBIAT64,
	BR_KW_MUL,
	BR_KW_MULI,
	BR_KW_MULIAT8,
	BR_KW_MULIAT16,
	BR_KW_MULIAT32,
	BR_KW_MULIATP,
	BR_KW_MULIAT64,
	BR_KW_DIV,
	BR_KW_DIVI,
	BR_KW_DIVIAT8,
	BR_KW_DIVIAT16,
	BR_KW_DIVIAT32,
	BR_KW_DIVIATP,
	BR_KW_DIVIAT64,
	BR_KW_DIVS,
	BR_KW_DIVSI,
	BR_KW_DIVSIAT8,
	BR_KW_DIVSIAT16,
	BR_KW_DIVSIAT32,
	BR_KW_DIVSIATP,
	BR_KW_DIVSIAT64,
	BR_KW_MOD,
	BR_KW_MODI,
	BR_KW_MODIAT8,
	BR_KW_MODIAT16,
	BR_KW_MODIAT32,
	BR_KW_MODIATP,
	BR_KW_MODIAT64,
	BR_KW_MODS,
	BR_KW_MODSI,
	BR_KW_MODSIAT8,
	BR_KW_MODSIAT16,
	BR_KW_MODSIAT32,
	BR_KW_MODSIATP,
	BR_KW_MODSIAT64,
	BR_KW_AND,
	BR_KW_ANDI,
	BR_KW_ANDIAT8,
	BR_KW_ANDIAT16,
	BR_KW_ANDIAT32,
	BR_KW_ANDIATP,
	BR_KW_ANDIAT64,
	BR_KW_OR,
	BR_KW_ORI,
	BR_KW_ORIAT8,
	BR_KW_ORIAT16,
	BR_KW_ORIAT32,
	BR_KW_ORIATP,
	BR_KW_ORIAT64,
	BR_KW_XOR,
	BR_KW_XORI,
	BR_KW_XORIAT8,
	BR_KW_XORIAT16,
	BR_KW_XORIAT32,
	BR_KW_XORIATP,
	BR_KW_XORIAT64,
	BR_KW_SHL,
	BR_KW_SHLI,
	BR_KW_SHLIAT8,
	BR_KW_SHLIAT16,
	BR_KW_SHLIAT32,
	BR_KW_SHLIATP,
	BR_KW_SHLIAT64,
	BR_KW_SHR,
	BR_KW_SHRI,
	BR_KW_SHRIAT8,
	BR_KW_SHRIAT16,
	BR_KW_SHRIAT32,
	BR_KW_SHRIATP,
	BR_KW_SHRIAT64,
	BR_KW_SHRS,
	BR_KW_SHRSI,
	BR_KW_SHRSIAT8,
	BR_KW_SHRSIAT16,
	BR_KW_SHRSIAT32,
	BR_KW_SHRSIATP,
	BR_KW_SHRSIAT64,
	BR_KW_NOT,
	BR_KW_NOTAT8,
	BR_KW_NOTAT16,
	BR_KW_NOTAT32,
	BR_KW_NOTATP,
	BR_KW_NOTAT64,
	BR_KW_DROP,
	BR_KW_NEW,
	BR_KW_ZERO,
	BR_KW_GET,
	BR_KW_SETAT,
	BR_KW_GETFROM,
	BR_KW_COPY,
	BR_KW_DATA,
	BR_KW_VOID,
	BR_KW_ENTRY,
	BR_KW_STRUCT,
	BR_N_KWS
} BR_AsmKw;

static sbuf asm_kws[] = {
	[BR_KW_NOP]       = fromcstr("nop"),
	[BR_KW_END]       = fromcstr("end"),
	[BR_KW_I8]        = fromcstr("i8"),
	[BR_KW_I16]       = fromcstr("i16"),
	[BR_KW_I32]       = fromcstr("i32"),
	[BR_KW_PTR]       = fromcstr("ptr"),
	[BR_KW_I64]       = fromcstr("i64"),
	[BR_KW_ADDR]      = fromcstr("addr"),
	[BR_KW_DBADDR]    = fromcstr("dbaddr"),
	[BR_KW_SYS]       = fromcstr("sys"),
	[BR_KW_BUILTIN]   = fromcstr("builtin"),
	[BR_KW_ADD]       = fromcstr("add"),
	[BR_KW_ADDI]      = fromcstr("add-i"),
	[BR_KW_ADDIAT8]   = fromcstr("add-i@8"),
	[BR_KW_ADDIAT16]  = fromcstr("add-i@16"),
	[BR_KW_ADDIAT32]  = fromcstr("add-i@32"),
	[BR_KW_ADDIATP]   = fromcstr("add-i@p"),
	[BR_KW_ADDIAT64]  = fromcstr("add-i@64"),
	[BR_KW_SUB]       = fromcstr("sub"),
	[BR_KW_SUBI]      = fromcstr("sub-i"),
	[BR_KW_SUBIAT8]   = fromcstr("sub-i@8"),
	[BR_KW_SUBIAT16]  = fromcstr("sub-i@16"),
	[BR_KW_SUBIAT32]  = fromcstr("sub-i@32"),
	[BR_KW_SUBIATP]   = fromcstr("sub-i@p"),
	[BR_KW_SUBIAT64]  = fromcstr("sub-i@64"),
	[BR_KW_MUL]       = fromcstr("mul"),
	[BR_KW_MULI]      = fromcstr("mul-i"),
	[BR_KW_MULIAT8]   = fromcstr("mul-i@8"),
	[BR_KW_MULIAT16]  = fromcstr("mul-i@16"),
	[BR_KW_MULIAT32]  = fromcstr("mul-i@32"),
	[BR_KW_MULIATP]   = fromcstr("mul-i@p"),
	[BR_KW_MULIAT64]  = fromcstr("mul-i@64"),
	[BR_KW_DIV]       = fromcstr("div"),
	[BR_KW_DIVI]      = fromcstr("div-i"),
	[BR_KW_DIVIAT8]   = fromcstr("div-i@8"),
	[BR_KW_DIVIAT16]  = fromcstr("div-i@16"),
	[BR_KW_DIVIAT32]  = fromcstr("div-i@32"),
	[BR_KW_DIVIATP]   = fromcstr("div-i@p"),
	[BR_KW_DIVIAT64]  = fromcstr("div-i@64"),
	[BR_KW_DIVS]      = fromcstr("divs"),
	[BR_KW_DIVSI]     = fromcstr("divs-i"),
	[BR_KW_DIVSIAT8]  = fromcstr("divs-i@8"),
	[BR_KW_DIVSIAT16] = fromcstr("divs-i@16"),
	[BR_KW_DIVSIAT32] = fromcstr("divs-i@32"),
	[BR_KW_DIVSIATP]  = fromcstr("divs-i@p"),
	[BR_KW_DIVSIAT64] = fromcstr("divs-i@64"),
	[BR_KW_MOD]       = fromcstr("mod"),
	[BR_KW_MODI]      = fromcstr("mod-i"),
	[BR_KW_MODIAT8]   = fromcstr("mod-i@8"),
	[BR_KW_MODIAT16]  = fromcstr("mod-i@16"),
	[BR_KW_MODIAT32]  = fromcstr("mod-i@32"),
	[BR_KW_MODIATP]   = fromcstr("mod-i@p"),
	[BR_KW_MODIAT64]  = fromcstr("mod-i@64"),
	[BR_KW_MODS]      = fromcstr("mods"),
	[BR_KW_MODSI]     = fromcstr("mods-i"),
	[BR_KW_MODSIAT8]  = fromcstr("mods-i@8"),
	[BR_KW_MODSIAT16] = fromcstr("mods-i@16"),
	[BR_KW_MODSIAT32] = fromcstr("mods-i@32"),
	[BR_KW_MODSIATP]  = fromcstr("mods-i@p"),
	[BR_KW_MODSIAT64] = fromcstr("mods-i@64"),
	[BR_KW_AND]       = fromcstr("and"),
	[BR_KW_ANDI]      = fromcstr("and-i"),
	[BR_KW_ANDIAT8]   = fromcstr("and-i@8"),
	[BR_KW_ANDIAT16]  = fromcstr("and-i@16"),
	[BR_KW_ANDIAT32]  = fromcstr("and-i@32"),
	[BR_KW_ANDIATP]   = fromcstr("and-i@p"),
	[BR_KW_ANDIAT64]  = fromcstr("and-i@64"),
	[BR_KW_OR]        = fromcstr("or"),
	[BR_KW_ORI]       = fromcstr("or-i"),
	[BR_KW_ORIAT8]    = fromcstr("or-i@8"),
	[BR_KW_ORIAT16]   = fromcstr("or-i@16"),
	[BR_KW_ORIAT32]   = fromcstr("or-i@32"),
	[BR_KW_ORIATP]    = fromcstr("or-i@p"),
	[BR_KW_ORIAT64]   = fromcstr("or-i@64"),
	[BR_KW_XOR]       = fromcstr("xor"),
	[BR_KW_XORI]      = fromcstr("xor-i"),
	[BR_KW_XORIAT8]   = fromcstr("xor-i@8"),
	[BR_KW_XORIAT16]  = fromcstr("xor-i@16"),
	[BR_KW_XORIAT32]  = fromcstr("xor-i@32"),
	[BR_KW_XORIATP]   = fromcstr("xor-i@p"),
	[BR_KW_XORIAT64]  = fromcstr("xor-i@64"),
	[BR_KW_SHL]       = fromcstr("shl"),
	[BR_KW_SHLI]      = fromcstr("shl-i"),
	[BR_KW_SHLIAT8]   = fromcstr("shl-i@8"),
	[BR_KW_SHLIAT16]  = fromcstr("shl-i@16"),
	[BR_KW_SHLIAT32]  = fromcstr("shl-i@32"),
	[BR_KW_SHLIATP]   = fromcstr("shl-i@p"),
	[BR_KW_SHLIAT64]  = fromcstr("shl-i@64"),
	[BR_KW_SHR]       = fromcstr("shr"),
	[BR_KW_SHRI]      = fromcstr("shr-i"),
	[BR_KW_SHRIAT8]   = fromcstr("shr-i@8"),
	[BR_KW_SHRIAT16]  = fromcstr("shr-i@16"),
	[BR_KW_SHRIAT32]  = fromcstr("shr-i@32"),
	[BR_KW_SHRIATP]   = fromcstr("shr-i@p"),
	[BR_KW_SHRIAT64]  = fromcstr("shr-i@64"),
	[BR_KW_SHRS]      = fromcstr("shrs"),
	[BR_KW_SHRSI]     = fromcstr("shrs-i"),
	[BR_KW_SHRSIAT8]  = fromcstr("shrs-i@8"),
	[BR_KW_SHRSIAT16] = fromcstr("shrs-i@16"),
	[BR_KW_SHRSIAT32] = fromcstr("shrs-i@32"),
	[BR_KW_SHRSIATP]  = fromcstr("shrs-i@p"),
	[BR_KW_SHRSIAT64] = fromcstr("shrs-i@64"),
	[BR_KW_NOT]       = fromcstr("not"),
	[BR_KW_NOTAT8]    = fromcstr("not-@8"),
	[BR_KW_NOTAT16]   = fromcstr("not-@16"),
	[BR_KW_NOTAT32]   = fromcstr("not-@32"),
	[BR_KW_NOTATP]    = fromcstr("not-@p"),
	[BR_KW_NOTAT64]   = fromcstr("not-@64"),
	[BR_KW_DROP]      = fromcstr("drop"),
	[BR_KW_NEW]       = fromcstr("new"),
	[BR_KW_ZERO]      = fromcstr("zero"),
	[BR_KW_GET]       = fromcstr("get"),
	[BR_KW_SETAT]     = fromcstr("set-at"),
	[BR_KW_GETFROM]   = fromcstr("get-from"),
	[BR_KW_COPY]      = fromcstr("copy"),
	[BR_KW_DATA]      = fromcstr("data"),
	[BR_KW_VOID]      = fromcstr("void"),
	[BR_KW_ENTRY]     = fromcstr("entry"),
	[BR_KW_STRUCT]    = fromcstr("struct"),
	(sbuf){0}
};
static_assert(BR_N_OPS == 115, "not all operations have their names defined in the assembler");

typedef enum {
	BR_SYM_BRACKET_L,
	BR_SYM_BRACKET_R,
	BR_SYM_SQBRACKET_L,
	BR_SYM_SQBRACKET_R,
	BR_SYM_CBRACKET_L,
	BR_SYM_CBRACKET_R,
	BR_SYM_COMMA,
	BR_SYM_PLUS,
	BR_SYM_DOLLAR,
	BR_N_SYMS
} BR_AsmSymbol;

static sbuf asm_symbols[] = {
	[BR_SYM_BRACKET_L] = fromcstr("("),
	[BR_SYM_BRACKET_R] = fromcstr(")"),
	[BR_SYM_SQBRACKET_L] = fromcstr("["),
	[BR_SYM_SQBRACKET_R] = fromcstr("]"),
	[BR_SYM_CBRACKET_L] = fromcstr("{"),
	[BR_SYM_CBRACKET_R] = fromcstr("}"),
	[BR_SYM_COMMA] = fromcstr(","),
	[BR_SYM_PLUS] = fromcstr("+"),
	[BR_SYM_DOLLAR] = fromcstr("$"),
	BRP_HIDDEN_SYMBOL(" "),
	BRP_HIDDEN_SYMBOL("\t"),
	BRP_HIDDEN_SYMBOL("\n"),
	(sbuf){0}
};

static BR_TypeKind kw_to_typekind[] = {
	[BR_KW_I8] = BR_TYPE_I8,
	[BR_KW_I16] = BR_TYPE_I16,
	[BR_KW_I32] = BR_TYPE_I32,
	[BR_KW_PTR] = BR_TYPE_PTR,
	[BR_KW_I64] = BR_TYPE_I64,
	[BR_KW_VOID] = BR_TYPE_VOID,
	[BR_KW_STRUCT] = BR_TYPE_STRUCT
};

static BR_Error addLoc(BR_Error err, BRP_Token token)
{
	err.loc = objdup(BRP_TokenLoc, token.loc);
	return err;
}

static BR_Error getName(BRP* obj, char** name_p)
{
	BRP_Token res = BRP_fetchToken(obj);
	if ((*name_p = BRP_getTokenWord(obj, res))) return (BR_Error){0};
	if (res.type != TOKEN_STRING)
		return addLoc((BR_Error){.type = BR_ERR_INVALID_NAME}, res);
	if (memchr(res.string.data, '\0', res.string.length))
		return addLoc((BR_Error){.type = BR_ERR_INVALID_NAME}, res);
	*name_p = tostr(res.string);
	return (BR_Error){0};
}

static BR_Error getType(BRP* obj, BR_Module* module, BR_Type* res_p)
{
	*res_p = (BR_Type){0};
// fetching the type kind
	BRP_Token token = BRP_fetchToken(obj);
	if (token.type != TOKEN_KEYWORD)
		return addLoc((BR_Error){.type = BR_ERR_TYPE_EXPECTED}, token);
	res_p->kind = kw_to_typekind[token.keyword_id];
// special handling for a struct type
	if (res_p->kind == BR_TYPE_STRUCT) {
		token = BRP_peekToken(obj);
		if (BRP_getTokenSymbolId(token) == BR_SYM_DOLLAR) {
			BRP_fetchToken(obj);
			if ((token = BRP_fetchToken(obj)).type != TOKEN_INT)
				return addLoc((BR_Error){.type = BR_ERR_STRUCT_ID_EXPECTED}, token);
			res_p->struct_id = token.value;
		} else {
			char* struct_name;
			BR_Error err = getName(obj, &struct_name);
			if (err.type) return err;
			BR_id struct_id = BR_getStructIdByName(module, struct_name);
			if (struct_id == BR_INVALID_ID)
				return addLoc((BR_Error){.type = BR_ERR_UNKNOWN_STRUCT, .name = struct_name}, token);
			res_p->struct_id = struct_id;
		}
	}
// fetching the amount of items
	if (BRP_getTokenSymbolId(BRP_peekToken(obj)) == BR_SYM_SQBRACKET_L) {
		BRP_fetchToken(obj);
		token = BRP_fetchToken(obj);
		if (token.type != TOKEN_INT)
			return addLoc((BR_Error){.type = BR_ERR_INVALID_ARRAY_SIZE_SPEC}, token);
		res_p->n_items = token.value;
		if (BRP_getTokenSymbolId(token = BRP_fetchToken(obj)) != BR_SYM_SQBRACKET_R)
			return addLoc((BR_Error){.type = BR_ERR_INVALID_ARRAY_SIZE_SPEC}, token);
	} else res_p->n_items = 1;
	return (BR_Error){0};
}

static BR_Error getOp(BRP* obj, BR_ModuleBuilder* builder, BR_id proc_id, BR_Op* op)
{
	BR_Error err;
	BRP_Token token = BRP_fetchToken(obj);
	if (token.type != TOKEN_KEYWORD || token.keyword_id >= BR_N_OPS)
		return addLoc((BR_Error){.type = BR_ERR_OP_NAME_EXPECTED}, token);

	op->type = (BR_OpType)token.keyword_id;
	switch (BR_GET_OPERAND_TYPE(op->type)) {
		case BR_OPERAND_NONE:
			break;
		case BR_OPERAND_SYSCALL_NAME: {
			op->type = BR_OP_SYS;
			char* name_c;
			if ((err = getName(obj, &name_c)).type) return (BR_Error){.type = BR_ERR_SYSCALL_NAME_EXPECTED, .loc = err.loc};
			op->operand_u = 0;
			sbuf name = fromstr(name_c);
			repeat (BR_N_SYSCALLS) {
				if (sbufeq(name, BR_syscallNames[op->operand_u])) {
					name.data = NULL;
					break;
				}
				++op->operand_u;
			}
			if (name.data) return addLoc((BR_Error){.type = BR_ERR_UNKNOWN_SYSCALL, .name = name_c}, token);
		} break;
		case BR_OPERAND_INT8:
		case BR_OPERAND_INT:
			if ((token = BRP_fetchToken(obj)).type != TOKEN_INT)
				return addLoc((BR_Error){.type = BR_ERR_INT_OPERAND_EXPECTED}, token);
			op->operand_u = token.value;
			break;
		case BR_OPERAND_VAR_NAME:
			if ((token = BRP_fetchToken(obj)).type != TOKEN_INT)
				assert(false, "named stack items are not implemented yet");
			op->operand_u = token.value;
			break;
		case BR_OPERAND_DB_NAME:
			token = BRP_peekToken(obj);
			if (token.type == TOKEN_INT) {
				op->operand_u = token.value;
				BRP_fetchToken(obj);
			} else {
				char* name;
				if ((err = getName(obj, &name)).type)
					return addLoc((BR_Error){.type = BR_ERR_INT_OR_DB_NAME_EXPECTED}, token);
				op->operand_s = BR_getDataBlockIdByName(&builder->module, name);
				if (op->operand_s == BR_INVALID_ID)
					return addLoc((BR_Error){.type = BR_ERR_UNKNOWN_DB, .name = name}, token);
			}
			break;
		case BR_OPERAND_TYPE:
			if ((err = getType(obj, &builder->module, &op->operand_type)).type) return err;
			break;
		case BR_OPERAND_BUILTIN: {
			char* name_c;
			if ((err = getName(obj, &name_c)).type) return addLoc((BR_Error){.type = BR_ERR_BUILTIN_NAME_EXPECTED}, token);
			op->operand_u = 0;
			sbuf name = fromstr(name_c);
			repeat (BR_N_BUILTINS) {
				if (sbufeq(name, BR_builtinNames[op->operand_u])) {
					name.data = NULL;
					break;
				}
				++op->operand_u;
			}
			if (name.data) return addLoc((BR_Error){.type = BR_ERR_UNKNOWN_BUILTIN, .name = name_c}, token);
		} break;
		default:
			return addLoc((BR_Error){.type = BR_ERR_OP_NAME_EXPECTED}, token);
	}
	return (BR_Error){0};
}

BR_Error BR_loadFromAssembly(FILE* input, const char* input_name, BR_ModuleBuilder* dst)
{
	if (dst->error.type) return dst->error;
	BR_Error err;
	BRP prep = {0};
	BRP_initBRP(&prep, NULL, BRP_ESC_STR_LITERALS);
	BRP_setKeywords(&prep, asm_kws);
	BRP_setSymbols(&prep, asm_symbols);
	BRP_setInput(&prep, (char*)input_name, input);

	while (true) {
		BRP_Token token = BRP_fetchToken(&prep);
		if (token.type == TOKEN_NONE) break;
		if (token.type != TOKEN_KEYWORD)
			return addLoc((BR_Error){.type = BR_ERR_INVALID_DECL}, token);
		switch (token.keyword_id) {
			case BR_KW_I8:
			case BR_KW_I16:
			case BR_KW_I32:
			case BR_KW_I64:
			case BR_KW_PTR:
			case BR_KW_VOID:
			proc_decl: { // a label is set to jump to it from `case BR_KW_STRUCT`
				BRP_unfetchToken(&prep, token);
// getting return type of the procedure
				BR_Type ret_type;
				if ((err = getType(&prep, &dst->module, &ret_type)).type) return err;
// getting procedure name
				char* proc_name;
				if ((err = getName(&prep, &proc_name)).type) return err;
// getting procedure arguments
				token = BRP_fetchToken(&prep);
				if (token.type != TOKEN_SYMBOL || token.symbol_id != BR_SYM_BRACKET_L)
					return addLoc((BR_Error){.type = BR_ERR_ARGS_EXPECTED}, token);
				BR_TypeArray args = {0};
				if (BRP_getTokenSymbolId(BRP_peekToken(&prep)) != BR_SYM_BRACKET_R) {
					do {
						if (!BR_TypeArray_incrlen(&args, 1)) return (BR_Error){.type = BR_ERR_NO_MEMORY};
						if ((err = getType(&prep, &dst->module, arrayhead(args))).type) return err;
					} while (BRP_getTokenSymbolId(BRP_fetchToken(&prep)) != BR_SYM_BRACKET_R);
				} else BRP_fetchToken(&prep);
// adding the declaration
				BR_id proc_id = BR_getProcIdByName(&dst->module, proc_name);
				if (proc_id == BR_INVALID_ID) {
					if ((err = BR_addProc(dst, &proc_id, proc_name, args.length, args.data, ret_type, 0)).type) return addLoc(err, token);
				} else {
					if (memcmp(&ret_type, &dst->module.seg_exec.data[proc_id].ret_type, sizeof(BR_Type)))
						return addLoc((BR_Error){.type = BR_ERR_PROTOTYPE_MISMATCH, .name = proc_name}, token);
					if (args.length == dst->module.seg_exec.data[proc_id].args.length
						? memcmp(args.data, dst->module.seg_exec.data[proc_id].args.data, args.length) : true)
						return addLoc((BR_Error){.type = BR_ERR_PROTOTYPE_MISMATCH, .name = proc_name}, token);
				}
				BR_TypeArray_clear(&args);
// checking if the procedure is declared as an entry point
				if (BRP_getTokenKeywordId(BRP_peekToken(&prep)) == BR_KW_ENTRY) {
					dst->module.exec_entry_point = proc_id;
					BRP_fetchToken(&prep);
				}
// optionally fetching the body
				if (BRP_getTokenSymbolId(BRP_peekToken(&prep)) != BR_SYM_CBRACKET_L) break;
				BRP_fetchToken(&prep);
				BR_Op op;
				while (BRP_getTokenSymbolId(token = BRP_peekToken(&prep)) != BR_SYM_CBRACKET_R) {
					if ((err = getOp(&prep, dst, proc_id, &op)).type) return err;
					if ((err = BR_addOp(dst, proc_id, op)).type) return addLoc(err, token);
				}
				BRP_fetchToken(&prep);
			} break;
			case BR_KW_DATA: {
				bool is_mutable = false;
				char* db_name;
// fetching the `+` symbol if found after the `data` keyword
				if (BRP_getTokenSymbolId(BRP_peekToken(&prep)) == BR_SYM_PLUS) {
					BRP_fetchToken(&prep);
					is_mutable = true;
				}
// fetching the declaration name
				token = BRP_peekToken(&prep);
				if ((err = getName(&prep, &db_name)).type) return err;
// adding the declaration
				BR_id db_id = BR_getDataBlockIdByName(&dst->module, db_name);
				if (db_id == BR_INVALID_ID)
					if ((err = BR_addDataBlock(dst, &db_id, db_name, is_mutable, 0)).type) return addLoc(err, token);
// optionally fetching the body
				if (BRP_getTokenSymbolId(BRP_peekToken(&prep)) != BR_SYM_CBRACKET_L) break;
				BRP_fetchToken(&prep);
				BR_Op op;
				while (BRP_getTokenSymbolId(token = BRP_peekToken(&prep)) != BR_SYM_CBRACKET_R) {
					if ((err = getOp(&prep, dst, db_id, &op)).type) return err;
					if ((err = BR_addOp(dst, db_id, op)).type) return addLoc(err, token);
				}
				BRP_fetchToken(&prep);
			} break;
			case BR_KW_STRUCT: {
// saving the `struct` keyword token to unfetch it when switching to parsing a procedure declaration
				BRP_Token struct_kw = token;
// fetching the struct name
				char* name;
				BRP_Token struct_name = BRP_peekToken(&prep);
				if ((err = getName(&prep, &name)).type) return (BR_Error){.type = BR_ERR_STRUCT_NAME_EXPECTED, .loc = err.loc};
// fetching the struct fields if the symbol after the struct name is a closing curly bracket, otherwise attempting to parse it as a procedure declaration
				token = BRP_peekToken(&prep);
				if (BRP_getTokenSymbolId(token) != BR_SYM_CBRACKET_L) {
					if (!BRP_unfetchToken(&prep, struct_name)
						|| !BRP_unfetchToken(&prep, struct_kw))
						return addLoc((BR_Error){.type = BR_ERR_NO_MEMORY}, token);
					goto proc_decl;
				}
				BRP_fetchToken(&prep);
				BR_TypeArray fields = {0};
				while (BRP_getTokenSymbolId(token = BRP_peekToken(&prep)) != BR_SYM_CBRACKET_R) {
					BR_Type* field = BR_TypeArray_incrlen(&fields, 1);
					if ((err = getType(&prep, &dst->module, field)).type) return err;
				}
				token = BRP_fetchToken(&prep);
				BR_id struct_id;
				if ((err = BR_addStruct(dst, &struct_id, name, fields.length, fields.data)).type) return addLoc(err, token);
				BR_TypeArray_clear(&fields);
			} break;
			default:
				return addLoc((BR_Error){.type = BR_ERR_INVALID_DECL}, token);
		}
	}
	BRP_delBRP(&prep);
	return (BR_Error){0};
}
