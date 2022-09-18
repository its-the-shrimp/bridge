// implementation of the BRB Assembler
#include <brb.h>
#define BRP_IMPLEMENTATION
#define _SBUF_IMPL_LOCK
#define _BR_UTILS_IMPL_LOCK
#include <brp.h>

defArray(BRB_Type);

typedef enum {
	BRB_KW_NOP,
	BRB_KW_END,
	BRB_KW_I8,
	BRB_KW_I16,
	BRB_KW_I32,
	BRB_KW_I64,
	BRB_KW_PTR,
	BRB_KW_ADDR,
	BRB_KW_DBADDR,
	BRB_KW_LD,
	BRB_KW_STR,
	BRB_KW_SYS,
	BRB_KW_BUILTIN,
	BRB_KW_ADD,
	BRB_KW_ADDI,
	BRB_KW_ADDIAT8,
	BRB_KW_ADDIAT16,
	BRB_KW_ADDIAT32,
	BRB_KW_ADDIATP,
	BRB_KW_ADDIAT64,
	BRB_KW_DROP,
	BRB_KW_DATA,
	BRB_KW_BYTES,
	BRB_KW_TEXT,
	BRB_KW_ZERO,
	BRB_KW_VOID,
	BRB_KW_ENTRY,
	BRB_N_KWS
} BRB_AsmKw;
static sbuf asm_kws[] = {
	[BRB_KW_NOP]      = fromcstr("nop"),
	[BRB_KW_END]      = fromcstr("end"),
	[BRB_KW_I8]       = fromcstr("i8"),
	[BRB_KW_I16]      = fromcstr("i16"),
	[BRB_KW_I32]      = fromcstr("i32"),
	[BRB_KW_I64]      = fromcstr("i64"),
	[BRB_KW_PTR]      = fromcstr("ptr"),
	[BRB_KW_ADDR]     = fromcstr("addr"),
	[BRB_KW_DBADDR]   = fromcstr("dbaddr"),
	[BRB_KW_LD]       = fromcstr("ld"),
	[BRB_KW_STR]      = fromcstr("str"),
	[BRB_KW_SYS]      = fromcstr("sys"),
	[BRB_KW_ADD]      = fromcstr("add"),
	[BRB_KW_ADDI]     = fromcstr("add-i"),
	[BRB_KW_ADDIAT8]  = fromcstr("add-i@8"),
	[BRB_KW_ADDIAT16] = fromcstr("add-i@16"),
	[BRB_KW_ADDIAT32] = fromcstr("add-i@32"),
	[BRB_KW_ADDIATP]  = fromcstr("add-i@p"),
	[BRB_KW_ADDIAT64] = fromcstr("add-i@64"),
	[BRB_KW_DROP]     = fromcstr("drop"),
	[BRB_KW_BUILTIN]  = fromcstr("builtin"),
	[BRB_KW_DATA]     = fromcstr("data"),
	[BRB_KW_BYTES]    = fromcstr("bytes"),
	[BRB_KW_TEXT]     = fromcstr("text"),
	[BRB_KW_ZERO]     = fromcstr("zero"),
	[BRB_KW_VOID]     = fromcstr("void"),
	[BRB_KW_ENTRY]    = fromcstr("entry"),
	(sbuf){0}
};
static_assert(BRB_N_OPS == 21, "not all operations have their names defined in the assembler");
static_assert(BRB_N_DP_TYPES == 10, "not all data pieces have their names defined in the assemblr");


typedef enum {
	BRB_SYM_BRACKET_L,
	BRB_SYM_BRACKET_R,
	BRB_SYM_SQBRACKET_L,
	BRB_SYM_SQBRACKET_R,
	BRB_SYM_CBRACKET_L,
	BRB_SYM_CBRACKET_R,
	BRB_SYM_COMMA,
	BRB_SYM_PLUS,
	BRB_N_SYMS
} BRB_AsmSymbol;

static sbuf asm_symbols[] = {
	[BRB_SYM_BRACKET_L] = fromcstr("("),
	[BRB_SYM_BRACKET_R] = fromcstr(")"),
	[BRB_SYM_SQBRACKET_L] = fromcstr("["),
	[BRB_SYM_SQBRACKET_R] = fromcstr("]"),
	[BRB_SYM_CBRACKET_L] = fromcstr("{"),
	[BRB_SYM_CBRACKET_R] = fromcstr("}"),
	[BRB_SYM_COMMA] = fromcstr(","),
	[BRB_SYM_PLUS] = fromcstr("+"),
	BRP_HIDDEN_SYMBOL(" "),
	BRP_HIDDEN_SYMBOL("\t"),
	BRP_HIDDEN_SYMBOL("\n"),
	(sbuf){0}
};

static BRB_TypeKind kw_to_typekind[] = {
	[BRB_KW_I8] = BRB_TYPE_I8,
	[BRB_KW_I16] = BRB_TYPE_I16,
	[BRB_KW_I32] = BRB_TYPE_I32,
	[BRB_KW_PTR] = BRB_TYPE_PTR,
	[BRB_KW_I64] = BRB_TYPE_I64,
	[BRB_KW_VOID] = BRB_TYPE_VOID
};

static BRB_Error addLoc(BRB_Error err, BRP_Token token)
{
	err.loc = objdup(BRP_TokenLoc, token.loc);
	return err;
}

static BRB_Error getName(BRP* obj, char** name_p)
{
	BRP_Token res = BRP_fetchToken(obj);
	if (res.type != TOKEN_STRING)
		return addLoc((BRB_Error){.type = BRB_ERR_INVALID_NAME}, res);
	if (memchr(res.string.data, '\0', res.string.length))
		return addLoc((BRB_Error){.type = BRB_ERR_INVALID_NAME}, res);
	*name_p = tostr(res.string);
	return (BRB_Error){0};
}

static BRB_Error getType(BRP* obj, BRB_Type* res_p)
{
	BRP_Token token = BRP_fetchToken(obj);
	if (token.type != TOKEN_KEYWORD)
		return addLoc((BRB_Error){.type = BRB_ERR_TYPE_EXPECTED}, token);
	res_p->kind = kw_to_typekind[token.keyword_id];
	if (BRP_getTokenSymbolId(BRP_peekToken(obj)) == BRB_SYM_SQBRACKET_L) {
		BRP_fetchToken(obj);
		token = BRP_fetchToken(obj);
		if (token.type != TOKEN_INT)
			return addLoc((BRB_Error){.type = BRB_ERR_INVALID_ARRAY_SIZE_SPEC}, token);
		res_p->n_items = token.value;
		if (BRP_getTokenSymbolId(token = BRP_fetchToken(obj)) != BRB_SYM_SQBRACKET_R)
			return addLoc((BRB_Error){.type = BRB_ERR_INVALID_ARRAY_SIZE_SPEC}, token);
	} else res_p->n_items = 1;
	return (BRB_Error){0};
}

static BRB_OpType kw_to_op[] = {
	[BRB_KW_NOP]      = BRB_OP_NOP,
	[BRB_KW_END]      = BRB_OP_END,
	[BRB_KW_I8]       = BRB_OP_I8,
	[BRB_KW_I16]      = BRB_OP_I16,
	[BRB_KW_I32]      = BRB_OP_I32,
	[BRB_KW_I64]      = BRB_OP_I64,
	[BRB_KW_PTR]      = BRB_OP_PTR,
	[BRB_KW_ADDR]     = BRB_OP_ADDR,
	[BRB_KW_DBADDR]   = BRB_OP_DBADDR,
	[BRB_KW_LD]       = BRB_OP_LD,
	[BRB_KW_STR]      = BRB_OP_STR,
	[BRB_KW_SYS]      = BRB_OP_SYS,
	[BRB_KW_BUILTIN]  = BRB_OP_BUILTIN,
	[BRB_KW_ADD]      = BRB_OP_ADD,
	[BRB_KW_ADDI]     = BRB_OP_ADDI,
	[BRB_KW_ADDIAT8]  = BRB_OP_ADDIAT8,
	[BRB_KW_ADDIAT16] = BRB_OP_ADDIAT16,
	[BRB_KW_ADDIAT32] = BRB_OP_ADDIAT32,
	[BRB_KW_ADDIATP]  = BRB_OP_ADDIATP,
	[BRB_KW_ADDIAT64] = BRB_OP_ADDIAT64,
	[BRB_KW_DROP]     = BRB_OP_DROP
};
static_assert(BRB_N_OPS == 21, "not all BRB operations are defined in the assembler");

static BRB_Error getOp(BRP* obj, BRB_ModuleBuilder* builder, uint32_t proc_id, BRB_Op* op)
{
	BRB_Error err;
	BRP_Token token = BRP_fetchToken(obj);
	if (token.type != TOKEN_KEYWORD || token.keyword_id >= BRB_N_OPS)
		return addLoc((BRB_Error){.type = BRB_ERR_OP_NAME_EXPECTED}, token);
		
	op->type = kw_to_op[token.keyword_id];
	switch (BRB_opFlags[op->type]) {
		case 0:
			break;
		case BRB_OPF_OPERAND_SYSCALL_NAME: {
			op->type = BRB_OP_SYS;
			if ((token = BRP_fetchToken(obj)).type != TOKEN_WORD)
				return addLoc((BRB_Error){.type = BRB_ERR_SYSCALL_NAME_EXPECTED}, token);
			op->operand_u = 0;
			sbuf name = fromstr(token.word);
			repeat (BRB_N_SYSCALLS) {
				if (sbufeq(name, BRB_syscallNames[op->operand_u])) {
					name.data = NULL;
					break;
				}
				++op->operand_u;
			}
			if (name.data) return addLoc((BRB_Error){.type = BRB_ERR_SYSCALL_NAME_EXPECTED}, token);
		} break;
		case BRB_OPF_OPERAND_INT8:
		case BRB_OPF_OPERAND_INT:
			if ((token = BRP_fetchToken(obj)).type != TOKEN_INT)
				return addLoc((BRB_Error){.type = BRB_ERR_INT_OPERAND_EXPECTED}, token);
			op->operand_u = token.value;
			break;
		case BRB_OPF_OPERAND_VAR_NAME:
			if ((token = BRP_fetchToken(obj)).type != TOKEN_INT)
				assert(false, "named stack items are not implemented yet");
			op->operand_u = token.value;
			break;
		case BRB_OPF_OPERAND_DB_NAME:
			token = BRP_fetchToken(obj);
			switch (token.type) {
				case TOKEN_INT:
					op->operand_u = token.value;
					break;
				case TOKEN_STRING: {
					char* name;
					BRP_unfetchToken(obj, token);
					if ((err = getName(obj, &name)).type) return err;
					op->operand_u = BRB_getDataBlockIdByName(&builder->module, name);
					if (op->operand_u == SIZE_MAX)
						return addLoc((BRB_Error){.type = BRB_ERR_INT_OR_NAME_OPERAND_EXPECTED}, token);
				} break;
				case TOKEN_SYMBOL:
				case TOKEN_KEYWORD:
				case TOKEN_NONE:
				default:
					return addLoc((BRB_Error){.type = BRB_ERR_INT_OR_NAME_OPERAND_EXPECTED}, token);
			}
			break;
		case BRB_OPF_OPERAND_TYPE:
			if ((err = getType(obj, &op->operand_type)).type) return err;
			break;
		case BRB_OPF_OPERAND_BUILTIN: {
			if ((token = BRP_fetchToken(obj)).type != TOKEN_WORD)
				return addLoc((BRB_Error){.type = BRB_ERR_BUILTIN_OPERAND_EXPECTED}, token);
			op->operand_u = 0;
			sbuf name = fromstr(token.word);
			repeat (BRB_N_BUILTINS) {
				if (sbufeq(name, BRB_builtinNames[op->operand_u])) {
					name.data = NULL;
					break;
				}
				++op->operand_u;
			}
			if (name.data) return addLoc((BRB_Error){.type = BRB_ERR_BUILTIN_OPERAND_EXPECTED}, token);
		} break;
		case BRB_N_KWS:
		default:
			return addLoc((BRB_Error){.type = BRB_ERR_OP_NAME_EXPECTED}, token);
	}
	return (BRB_Error){0};
}

static BRB_DataPieceType kw_to_dp[] = {
	[BRB_KW_NOP] = BRB_DP_NONE,
	[BRB_KW_BYTES] = BRB_DP_BYTES,
	[BRB_KW_I16] = BRB_DP_I16,
	[BRB_KW_I32] = BRB_DP_I32,
	[BRB_KW_PTR] = BRB_DP_PTR,
	[BRB_KW_I64] = BRB_DP_I64,
	[BRB_KW_TEXT] = BRB_DP_TEXT,
	[BRB_KW_DBADDR] = BRB_DP_DBADDR,
	[BRB_KW_ZERO] = BRB_DP_ZERO,
	[BRB_KW_BUILTIN] = BRB_DP_BUILTIN,
};

static BRB_Error getDataPiece(BRP* obj, BRB_ModuleBuilder* builder, uint32_t db_id, BRB_DataPiece* piece)
{
	BRB_Error err;
	BRP_Token token = BRP_fetchToken(obj);
	if (token.type != TOKEN_KEYWORD)
		return addLoc((BRB_Error){.type = BRB_ERR_DP_NAME_EXPECTED}, token);

	switch (token.keyword_id) {
		case BRB_KW_I16:
		case BRB_KW_I32:
		case BRB_KW_I64:
		case BRB_KW_PTR:
			piece->type = kw_to_dp[token.keyword_id];
			if ((token = BRP_fetchToken(obj)).type != TOKEN_INT)
				return addLoc((BRB_Error){.type = BRB_ERR_INT_OPERAND_EXPECTED}, token);
			piece->content_u = token.value;
			break;
		case BRB_KW_ZERO: {
			piece->type = BRB_DP_ZERO;
			if ((err = getType(obj, &piece->content_type)).type) return err;
		} break;
		case BRB_KW_DBADDR:
			piece->type = BRB_DP_DBADDR;
			token = BRP_fetchToken(obj);
			switch (token.type) {
				case TOKEN_INT:
					piece->content_u = token.value;
					break;
				case TOKEN_STRING: {
					char* name;
					BRP_unfetchToken(obj, token);
					if ((err = getName(obj, &name)).type) return err;
					piece->content_u = BRB_getDataBlockIdByName(&builder->module, name);
					if (piece->content_u == SIZE_MAX)
						return addLoc((BRB_Error){.type = BRB_ERR_INT_OR_NAME_OPERAND_EXPECTED}, token);
				} break;
				case TOKEN_SYMBOL:
				case TOKEN_KEYWORD:
				case TOKEN_NONE:
				default:
					return addLoc((BRB_Error){.type = BRB_ERR_INT_OR_NAME_OPERAND_EXPECTED}, token);
			} break;
		case BRB_KW_BUILTIN: {
			piece->type = BRB_DP_BUILTIN;
			if ((token = BRP_fetchToken(obj)).type != TOKEN_WORD)
				return addLoc((BRB_Error){.type = BRB_ERR_BUILTIN_OPERAND_EXPECTED}, token);
			piece->content_u = 0;
			sbuf name = fromstr(token.word);
			repeat (BRB_N_BUILTINS) {
				if (sbufeq(name, BRB_builtinNames[piece->content_u])) {
					name.data = NULL;
					break;
				}
				++piece->content_u;
			}
			if (name.data) return addLoc((BRB_Error){.type = BRB_ERR_BUILTIN_OPERAND_EXPECTED}, token);
		} break;
		case BRB_KW_BYTES:
		case BRB_KW_TEXT:
			piece->type = kw_to_dp[token.keyword_id];
			if ((token = BRP_fetchToken(obj)).type != TOKEN_STRING)
				return addLoc((BRB_Error){.type = BRB_ERR_TEXT_OPERAND_EXPECTED}, token);
			if (piece->type == BRB_DP_TEXT ? memchr(token.string.data, '\0', token.string.length) : false)
				return addLoc((BRB_Error){.type = BRB_ERR_INVALID_TEXT_OPERAND}, token);
			piece->data = token.string;
			break;
		case BRB_KW_VOID:
		case BRB_KW_ADDR:
		case BRB_KW_LD:
		case BRB_KW_STR:
		case BRB_KW_SYS:
		case BRB_KW_DATA:
		case BRB_KW_NOP:
		case BRB_KW_END:
		case BRB_KW_I8:
		case BRB_N_KWS:
		default:
			return addLoc((BRB_Error){.type = BRB_ERR_DP_NAME_EXPECTED}, token);
	}
	return (BRB_Error){0};
}

BRB_Error BRB_assembleModule(FILE* input, const char* input_name, BRB_Module* dst)
{
	BRB_Error err;
	BRP prep = {0};
	BRP_initBRP(&prep, NULL, BRP_ESC_STR_LITERALS);
	BRP_setKeywords(&prep, asm_kws);
	BRP_setSymbols(&prep, asm_symbols);
	BRP_setInput(&prep, (char*)input_name, input);

	BRB_ModuleBuilder builder;
	if ((err = BRB_initModuleBuilder(&builder)).type) return err;

	while (true) {
		BRP_Token token = BRP_fetchToken(&prep);
		if (token.type == TOKEN_NONE) break;
		if (token.type != TOKEN_KEYWORD)
			return addLoc((BRB_Error){.type = BRB_ERR_INVALID_DECL}, token);

		switch (token.keyword_id) {
			case BRB_KW_I8:
			case BRB_KW_I16:
			case BRB_KW_I32:
			case BRB_KW_I64:
			case BRB_KW_PTR:
			case BRB_KW_VOID: {
				BRP_unfetchToken(&prep, token);
// getting return type of the procdure
				BRB_Type ret_type;
				if ((err = getType(&prep, &ret_type)).type) return err;
// getting procedure name
				char* proc_name;
				if ((err = getName(&prep, &proc_name)).type) return err;
// getting procedure arguments
				token = BRP_fetchToken(&prep);
				if (token.type != TOKEN_SYMBOL || token.symbol_id != BRB_SYM_BRACKET_L)
					return addLoc((BRB_Error){.type = BRB_ERR_ARGS_EXPECTED}, token);
				BRB_TypeArray args = {0};
				if (BRP_getTokenSymbolId(BRP_peekToken(&prep)) != BRB_SYM_BRACKET_R) {
					do {
						if (!BRB_TypeArray_incrlen(&args, 1)) return (BRB_Error){.type = BRB_ERR_NO_MEMORY};
						if ((err = getType(&prep, arrayhead(args))).type) return err;
					} while (BRP_getTokenSymbolId(BRP_fetchToken(&prep)) != BRB_SYM_BRACKET_R);
				} else BRP_fetchToken(&prep);
// adding the declaration
				uint32_t proc_id;
				size_t _proc_id = BRB_getProcIdByName(&builder.module, proc_name);
				if (_proc_id == SIZE_MAX) {
					if ((err = BRB_addProc(&builder, &proc_id, proc_name, args.length, args.data, ret_type, 0)).type) return addLoc(err, token);
				} else {
					if (memcmp(&ret_type, &builder.module.seg_exec.data[_proc_id].ret_type, sizeof(BRB_Type)))
						return addLoc((BRB_Error){.type = BRB_ERR_PROTOTYPE_MISMATCH, .name = proc_name}, token);
					if (args.length == builder.module.seg_exec.data[_proc_id].args.length
						? memcmp(args.data, builder.module.seg_exec.data[_proc_id].args.data, args.length) : true)
						return addLoc((BRB_Error){.type = BRB_ERR_PROTOTYPE_MISMATCH, .name = proc_name}, token);
					proc_id = _proc_id;
				}
				BRB_TypeArray_clear(&args);
// checking if the procedure is declared as an entry point
				if (BRP_getTokenKeywordId(BRP_peekToken(&prep)) == BRB_KW_ENTRY) {
					builder.module.exec_entry_point = proc_id;
					BRP_fetchToken(&prep);
				}
// optionally fetching the body
				if (BRP_getTokenSymbolId(BRP_peekToken(&prep)) != BRB_SYM_CBRACKET_L) break;
				BRP_fetchToken(&prep);
				BRB_Op op;
				while (BRP_getTokenSymbolId(token = BRP_peekToken(&prep)) != BRB_SYM_CBRACKET_R) {
					if ((err = getOp(&prep, &builder, proc_id, &op)).type) return err;
					if ((err = BRB_addOp(&builder, proc_id, op)).type) return addLoc(err, token);
				}
				BRP_fetchToken(&prep);
			} break;
			case BRB_KW_DATA: {
				bool is_mutable = false;
				char* db_name;
// fetching the `+` symbol if found after the `data` keyword
				if (BRP_getTokenSymbolId(BRP_peekToken(&prep)) == BRB_SYM_PLUS) {
					BRP_fetchToken(&prep);
					is_mutable = true;
				}
// fetching the declaration name
				token = BRP_peekToken(&prep);
				if ((err = getName(&prep, &db_name)).type) return err;
// adding the declaration
				uint32_t db_id;
				size_t _db_id = BRB_getDataBlockIdByName(&builder.module, db_name);
				if (_db_id == SIZE_MAX) {
					if ((err = BRB_addDataBlock(&builder, &db_id, db_name, is_mutable, 0)).type) return addLoc(err, token);
				} else db_id = _db_id;
// optionally fetching the body
				if (BRP_getTokenSymbolId(BRP_peekToken(&prep)) != BRB_SYM_CBRACKET_L) break;
				BRP_fetchToken(&prep);
				BRB_DataPiece piece;
				while (BRP_getTokenSymbolId(token = BRP_peekToken(&prep)) != BRB_SYM_CBRACKET_R) {
					if ((err = getDataPiece(&prep, &builder, db_id, &piece)).type) return err;
					if ((err = BRB_addDataPiece(&builder, db_id, piece)).type) return addLoc(err, token);
				}
				BRP_fetchToken(&prep);
			} break;
			case BRB_KW_NOP:
			case BRB_KW_END:
			case BRB_KW_ADDR:
			case BRB_KW_DBADDR:
			case BRB_KW_LD:
			case BRB_KW_STR:
			case BRB_KW_SYS:
			case BRB_KW_BUILTIN:
			case BRB_KW_BYTES:
			case BRB_KW_TEXT:
			case BRB_KW_ZERO:
			case BRB_KW_ENTRY:
				return addLoc((BRB_Error){.type = BRB_ERR_INVALID_DECL}, token);
		}
	}
	BRP_delBRP(&prep);
	return BRB_extractModule(builder, dst);
}
