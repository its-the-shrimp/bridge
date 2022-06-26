// implementation for loading BRB modules from `.brb` files
#include <brb.h>
#include <errno.h>

typedef str* field;
declArray(field);
defArray(field);
typedef struct {
	Module* dst;
	FILE* src;
	long* n_fetched;
} ModuleLoader;


uint8_t loadInt8(FILE* fd, long* n_fetched)
{
	char res;
	if (!fread(&res, 1, 1, fd)) return (*n_fetched = -1);
	*n_fetched += 1;
	return res;
}

uint16_t loadInt16(FILE* fd, long* n_fetched)
{
	char res[2];
	if (!fread(res, 2, 1, fd)) return (*n_fetched = -1);
	*n_fetched += 2;
	return *(uint16_t*)BRByteOrder(&res, 2); 
}

uint32_t loadInt32(FILE* fd, long* n_fetched)
{
	char res[4];
	if (!fread(res, 4, 1, fd)) return (*n_fetched = -1);
	*n_fetched += 4;
	return *(uint32_t*)BRByteOrder(&res, 4);
}

uint64_t loadInt64(FILE* fd, long* n_fetched)
{
	char res[8];
	if (!fread(res, 8, 1, fd)) return (*n_fetched = -1);
	*n_fetched += 8;
	return *(uint64_t*)BRByteOrder(&res, 8);
}

int64_t loadInt(FILE* fd, long* n_fetched)
{
	register uint8_t size = loadInt8(fd, n_fetched) & 0b1111;
	switch (size) {
		case 8: return loadInt8(fd, n_fetched);
		case 9: return loadInt16(fd, n_fetched);
		case 10: return loadInt32(fd, n_fetched);
		case 11: return loadInt64(fd, n_fetched);
		case 12: return ~(int64_t)loadInt8(fd, n_fetched);
		case 13: return ~(int64_t)loadInt16(fd, n_fetched);
		case 14: return ~(int64_t)loadInt32(fd, n_fetched);
		case 15: return ~(int64_t)loadInt64(fd, n_fetched);
		default: return size;
	}
}

uint8_t loadHalfByte(FILE* fd, long* n_fetched)
{
	register uint8_t res = loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return 0;
	if (ungetc(res & 0b1111, fd) == EOF) {
		*n_fetched = -1;
		return 0;
	}
	return res >> 4;
}

void load2HalfBytes(FILE* fd, uint8_t* hb1, uint8_t* hb2, long* n_fetched)
{
	register uint8_t res = loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return;

	*hb1 = res >> 4;
	*hb2 = res & 0b1111;
}

void load2Ints(FILE* fd, int64_t* x, int64_t* y, long* n_fetched)
{
	uint8_t sizes = loadInt8(fd, n_fetched);

	switch (sizes >> 4) {
		case 8: *x = loadInt8(fd, n_fetched);
		case 9: *x = loadInt16(fd, n_fetched);
		case 10: *x = loadInt32(fd, n_fetched);
		case 11: *x = loadInt64(fd, n_fetched);
		case 12: *x = ~(int64_t)loadInt8(fd, n_fetched);
		case 13: *x = ~(int64_t)loadInt16(fd, n_fetched);
		case 14: *x = ~(int64_t)loadInt32(fd, n_fetched);
		case 15: *x = ~(int64_t)loadInt64(fd, n_fetched);
		default: *x = sizes >> 4;
	}

	switch (sizes & 0b1111) {
		case 8: *y = loadInt8(fd, n_fetched);
		case 9: *y = loadInt16(fd, n_fetched);
		case 10: *y = loadInt32(fd, n_fetched);
		case 11: *y = loadInt64(fd, n_fetched);
		case 12: *y = ~(int64_t)loadInt8(fd, n_fetched);
		case 13: *y = ~(int64_t)loadInt16(fd, n_fetched);
		case 14: *y = ~(int64_t)loadInt32(fd, n_fetched);
		case 15: *y = ~(int64_t)loadInt64(fd, n_fetched);
		default: *y = sizes & 0b1111;
	}
}

BRBLoadError loadDataBlock(ModuleLoader* loader, DataBlock* block)
{
	static_assert(N_PIECE_TYPES == 9, "not all data piece types are handled in `loadDataBlock`");

	block->is_mutable = loadHalfByte(loader->src, loader->n_fetched);
	block->name = (char*)loadInt(loader->src, loader->n_fetched);
	if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};

	int n_pieces = loadInt(loader->src, loader->n_fetched);
	if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
	block->pieces = DataPieceArray_new(-n_pieces);
	block->pieces.length = n_pieces;

	for (int i = 0; i < n_pieces; ++i) {
		DataPiece* piece = block->pieces.data + i;
		piece->type = loadInt8(loader->src, loader->n_fetched);
		if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};

		switch (piece->type) {
			case PIECE_BYTES:
				piece->data.length = loadInt(loader->src, loader->n_fetched);
				if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
				piece->data = smalloc(piece->data.length);

				if (!fread(piece->data.data, piece->data.length, 1, loader->src))
					return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
				*loader->n_fetched += piece->data.length;
				break;
			case PIECE_TEXT:
				piece->data = (sbuf){0};
				if (getdelim(&piece->data.data, (size_t*)&piece->data.length, '\0', loader->src) < 0)
					return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
				break;
			case PIECE_INT16:
			case PIECE_INT32:
			case PIECE_INT64:
				piece->integer = loadInt(loader->src, loader->n_fetched);
				if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
				break;
			case PIECE_DB_ADDR:
			case PIECE_MB_ADDR:
				load2Ints(loader->src, (int64_t*)&piece->module_id, (int64_t*)&piece->mark_name, loader->n_fetched);
				if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
				break;
			case PIECE_ZERO:
				piece->n_bytes = loadInt(loader->src, loader->n_fetched);
				if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
				break;
			default:
				return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
		}
	}

	return (BRBLoadError){0};
}

typedef BRBLoadError (*OpLoader) (ModuleLoader*, Op*);
 
void printLoadError(FILE* dst, BRBLoadError err)
{
	static_assert(N_BRB_ERRORS == 21, "not all BRB errors are handled\n");
	switch (err.code) {
		case BRB_ERR_OK: break;
		case BRB_ERR_NO_MEMORY:
			fprintf(dst, "BRB loading error: memory allocation failure\n");
			break;
		case BRB_ERR_NO_OPCODE:
			fprintf(dst, "BRB loading error: operation code not found\n");
			break;
		case BRB_ERR_UNRESOLVED_NAMES:
			fprintf(dst, "BRB loading error: not all names were resolved\n");
			break;
		case BRB_ERR_NO_OP_ARG:
			fprintf(dst, 
				"BRB loading error: arguments for `%.*s` operation not found\n", 
				unpack(opNames[err.opcode])
			);
			break;
		case BRB_ERR_INVALID_OPCODE:
			fprintf(dst, "BRB loading error: invalid operation code %d found\n", err.opcode);
			break;
		case BRB_ERR_INVALID_COND_ID:
			fprintf(dst, "BRB loading error: invalid condition code %hhu found\n", err.cond_id);
			break;
		case BRB_ERR_NO_STACK_SIZE:
			fprintf(dst, "BRB loading error: `stacksize` segment not found\n");
			break;
		case BRB_ERR_NO_ENTRY:
			fprintf(dst, "BRB loading error: no `main` procedure found in the module\n");
			break;
		case BRB_ERR_NO_DATA_SEGMENT:
			fprintf(dst, "BRB loading error: `data` segment not found\n");
			break;
		case BRB_ERR_NO_MEMORY_SEGMENT:
			fprintf(dst, "BRB loading error: `memory` segment not found\n");
			break;
		case BRB_ERR_NO_EXEC_SEGMENT:
			fprintf(dst, "BRB loading error: `exec` segment not found\n");
			break;
		case BRB_ERR_NO_NAME_SEGMENT:
			fprintf(dst, "BRB loading error: `name` segment not found\n");
			break;
		case BRB_ERR_NO_NAME_SPEC:
			fprintf(dst, "BRB loading error: no name specifier found\n");
			break;
		case BRB_ERR_NO_COND_ID:
			fprintf(dst, "BRB loading error: no condition code found\n");
			break;
		case BRB_ERR_NO_LOAD_SEGMENT:
			fprintf(dst, "BRB loading error: `load` segment not found\n");
			break;
		case BRB_ERR_MODULE_NOT_FOUND:
			fprintf(dst, "BRB loading error: module `%s` not found in the specified search paths\n", err.module_name);
			break;
		case BRB_ERR_INVALID_BLOCK:
			fprintf(dst, "BRB loading error: invalid block structure\n");
			break;
		case BRB_ERR_UNRESOLVED_DB_REF:
			fprintf(dst, "data block `%s` was not found in module `%s`\n", err.mark_name, err.module_name);
			break;
		case BRB_ERR_UNRESOLVED_MB_REF:
			fprintf(dst, "memory block `%s` was not found in module `%s`\n", err.mark_name, err.module_name);
			break;
		case BRB_ERR_UNRESOLVED_PROC_REF:
			fprintf(dst, "procedure `%s` was not found in module `%s`\n", err.mark_name, err.module_name);
			break;
		case N_BRB_ERRORS:
			fprintf(dst, "unreachable\n");
	}
}

BRBLoadError loadNoArgOp(ModuleLoader* loader, Op* dst)
{
	return (BRBLoadError){0};
}

BRBLoadError loadMarkOp(ModuleLoader* loader, Op* dst)
{
	dst->mark_name = (char*)loadInt(loader->src, loader->n_fetched);
	if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_NO_OP_ARG};
	return (BRBLoadError){0};
}

BRBLoadError loadRegImmOp(ModuleLoader* loader, Op* dst)
{
	dst->dst_reg = loadHalfByte(loader->src, loader->n_fetched);
	dst->value = loadInt(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError load2RegOp(ModuleLoader* loader, Op* dst)
{
	uint8_t dst_reg, src_reg;
	load2HalfBytes(loader->src, &dst_reg, &src_reg, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };

	dst->dst_reg = dst_reg;
	dst->src_reg = src_reg;
	return (BRBLoadError){0};
}

BRBLoadError loadRegSymbolIdOp(ModuleLoader* loader, Op* dst)
{
	dst->dst_reg = loadHalfByte(loader->src, loader->n_fetched);
	dst->symbol_id = loadInt(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadRegModuleIdNameOp(ModuleLoader* loader, Op* dst)
{
	dst->module_id = loadInt(loader->src, loader->n_fetched);
	dst->dst_reg = loadHalfByte(loader->src, loader->n_fetched);
	dst->mark_name = (char*)loadInt(loader->src, loader->n_fetched);
	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };

	return (BRBLoadError){0};
}

BRBLoadError loadOpSyscall(ModuleLoader* loader, Op* dst)
{
	dst->syscall_id = loadInt8(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpGoto(ModuleLoader* loader, Op* dst)
{
	dst->op_offset = loadInt(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){.code = BRB_ERR_NO_OP_ARG};
	return (BRBLoadError){0};
}

BRBLoadError loadOpCmp(ModuleLoader* loader, Op* dst)
{
	dst->src_reg = loadHalfByte(loader->src, loader->n_fetched);
	dst->value = loadInt(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpCmpr(ModuleLoader* loader, Op* dst)
{
	uint8_t src_reg, src2_reg;
	load2HalfBytes(loader->src, &src_reg, &src2_reg, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };

	dst->src_reg = src_reg;
	dst->src2_reg = src2_reg;
	return (BRBLoadError){0};
}

BRBLoadError loadOpCall(ModuleLoader* loader, Op* dst)
{
	load2Ints(loader->src, (int64_t*)&dst->module_id, (int64_t*)&dst->mark_name, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError load2RegImmOp(ModuleLoader* loader, Op* dst)
{
	uint8_t dst_reg, src_reg;
	load2HalfBytes(loader->src, &dst_reg, &src_reg, loader->n_fetched);
	dst->value = loadInt(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };

	dst->dst_reg = dst_reg;
	dst->src_reg = src_reg;
	return (BRBLoadError){0};
}

BRBLoadError load3RegOp(ModuleLoader* loader, Op* dst)
{
	uint8_t src_reg, src2_reg;
	dst->dst_reg = loadInt8(loader->src, loader->n_fetched);
	load2HalfBytes(loader->src, &src_reg, &src2_reg, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };

	dst->src_reg = src_reg;
	dst->src2_reg = src2_reg;
	return (BRBLoadError){0};
}

BRBLoadError loadOpVar(ModuleLoader* loader, Op* dst)
{
	dst->new_var_size = loadInt(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpLdv(ModuleLoader* loader, Op* dst)
{
	uint8_t dst_reg, var_size;
	load2HalfBytes(loader->src, &dst_reg, &var_size, loader->n_fetched);
	dst->symbol_id = loadInt(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };

	dst->dst_reg = dst_reg;
	dst->var_size = var_size;
	return (BRBLoadError){0};
}

BRBLoadError loadOpStrv(ModuleLoader* loader, Op* dst)
{
	uint8_t var_size, src_reg;
	dst->symbol_id = loadInt(loader->src, loader->n_fetched);
	load2HalfBytes(loader->src, &var_size, &src_reg, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };

	dst->var_size = var_size;
	dst->src_reg = src_reg;
	return (BRBLoadError){0};
}

BRBLoadError loadOpPopv(ModuleLoader* loader, Op* dst)
{
	uint8_t dst_reg, var_size;
	load2HalfBytes(loader->src, &dst_reg, &var_size, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };

	dst->dst_reg = dst_reg;
	dst->var_size = var_size;
	return (BRBLoadError){0};
}

BRBLoadError loadOpPushv(ModuleLoader* loader, Op* dst)
{
	uint8_t var_size, src_reg;
	load2HalfBytes(loader->src, &var_size, &src_reg, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };

	dst->var_size = var_size;
	dst->src_reg = src_reg;
	return (BRBLoadError){0};
}

BRBLoadError loadSymbolIdOp(ModuleLoader* loader, Op* dst)
{
	dst->symbol_id = loadInt(loader->src, loader->n_fetched);
	
	if (loader->n_fetched < 0) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpSetc(ModuleLoader* loader, Op* dst)
{
	uint8_t dst_reg, cond_arg;
	load2HalfBytes(loader->src, &dst_reg, &cond_arg, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };

	dst->dst_reg = dst_reg;
	dst->cond_arg = cond_arg;
	return (BRBLoadError){0};
}

BRBLoadError loadBitShiftOp(ModuleLoader* loader, Op* dst)
{
	uint8_t dst_reg, src_reg;
	load2HalfBytes(loader->src, &dst_reg, &src_reg, loader->n_fetched);
	dst->value = loadInt8(loader->src, loader->n_fetched);

	if (loader->n_fetched < 0) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };

	dst->dst_reg = dst_reg;
	dst->src_reg = src_reg;
	return (BRBLoadError){0};
}

OpLoader op_loaders[] = {
	[OP_NONE] = &loadNoArgOp,
	[OP_END] = &loadNoArgOp,
	[OP_MARK] = &loadNoArgOp,
	[OP_SET] = &loadRegImmOp,
	[OP_SETR] = &load2RegOp,
	[OP_SETD] = &loadRegModuleIdNameOp,
	[OP_SETB] = &loadRegSymbolIdOp,
	[OP_SETM] = &loadRegModuleIdNameOp,
	[OP_ADD] = &load2RegImmOp,
	[OP_ADDR] = &load3RegOp,
	[OP_SUB] = &load2RegImmOp,
	[OP_SUBR] = &load3RegOp,
	[OP_SYS] = &loadOpSyscall,
	[OP_GOTO] = &loadOpGoto,
	[OP_CMP] = &loadOpCmp,
	[OP_CMPR] = &loadOpCmpr,
	[OP_AND] = &load2RegImmOp,
	[OP_ANDR] = &load3RegOp,
	[OP_OR] = &load2RegImmOp,
	[OP_ORR] = &load3RegOp,
	[OP_NOT] = &load2RegOp,
	[OP_XOR] = &load2RegImmOp,
	[OP_XORR] = &load3RegOp,
	[OP_SHL] = &loadBitShiftOp,
	[OP_SHLR] = &load3RegOp,
	[OP_SHR] = &loadBitShiftOp,
	[OP_SHRR] = &load3RegOp,
	[OP_SHRS] = &loadBitShiftOp,
	[OP_SHRSR] = &load3RegOp,
	[OP_PROC] = &loadMarkOp,
	[OP_CALL] = &loadMarkOp,
	[OP_RET] = &loadNoArgOp,
	[OP_ENDPROC] = &loadNoArgOp,
	[OP_LD64] = &load2RegOp,
	[OP_STR64] = &load2RegOp,
	[OP_LD32] = &load2RegOp,
	[OP_STR32] = &load2RegOp,
	[OP_LD16] = &load2RegOp,
	[OP_STR16] = &load2RegOp,
	[OP_LD8] = &load2RegOp,
	[OP_STR8] = &load2RegOp,
	[OP_VAR] = &loadOpVar,
	[OP_SETV] = &loadRegSymbolIdOp, 
	[OP_MUL] = &load2RegImmOp,
	[OP_MULR] = &load3RegOp,
	[OP_DIV] = &load2RegImmOp,
	[OP_DIVR] = &load3RegOp,
	[OP_DIVS] = &load2RegImmOp,
	[OP_DIVSR] = &load3RegOp,
	[OP_EXTPROC] = &loadMarkOp,
	[OP_LDV] = &loadOpLdv,
	[OP_STRV] = &loadOpStrv,
	[OP_POPV] = &loadOpPopv,
	[OP_PUSHV] = &loadOpPushv,
	[OP_ATF] = &loadMarkOp,
	[OP_ATL] = &loadSymbolIdOp,
	[OP_SETC] = &loadOpSetc,
	[OP_DELNV] = &loadSymbolIdOp,
	[OP_LD64S] = &load2RegOp,
	[OP_LD32S] = &load2RegOp,
	[OP_LD16S] = &load2RegOp,
	[OP_LD8S] = &load2RegOp,
	[OP_LDVS] = &loadOpLdv,
	[OP_SX32] = &load2RegOp,
	[OP_SX16] = &load2RegOp,
	[OP_SX8] = &load2RegOp,
	[OP_MOD] = &load2RegImmOp,
	[OP_MODS] = &load2RegImmOp,
	[OP_MODR] = &load3RegOp,
	[OP_MODSR] = &load3RegOp
};
static_assert(N_OPS == sizeof(op_loaders) / sizeof(op_loaders[0]), "Some BRB operations have unmatched loaders");

fieldArray getUnresolvedNames(Module* module)
{
	fieldArray res = {0};
	Submodule root = getRootSubmodule(module, ".");
	for (
		DataBlock* block = module->seg_data.data + root.ds_offset;
		block - module->seg_data.data < root.ds_offset + root.ds_length;
		++block
	) {
		fieldArray_append(&res, &block->name);
		for (
			DataPiece* piece = block->pieces.data;
			piece - block->pieces.data < block->pieces.length;
			++piece
		) {
			if (piece->type == PIECE_DB_ADDR || piece->type == PIECE_MB_ADDR)
				fieldArray_append(&res, &piece->mark_name);
		}
	}

	for (
		MemBlock* block = module->seg_memory.data + root.ms_offset;
		block - module->seg_memory.data < root.ms_offset + root.ms_length;
		++block
	) {
		fieldArray_append(&res, &block->name);
	}

	for (
		Op* op = module->seg_exec.data + root.es_offset;
		op - module->seg_exec.data < root.es_offset + root.es_length;
		++op
	) {
		if (op_flags[op->type] & OPF_REQ_NAME_RESOLUTION)
			fieldArray_append(&res, &op->mark_name);
	}

	return res;
}

BRBLoadError preloadModule(FILE* src, Module* dst, char* module_paths[])
{
	long status = 0;
	ModuleLoader loader = {
		.dst = dst,
		.src = src,
		.n_fetched = &status
	};
	*dst = (Module){0};

// loading stack size
	dst->stack_size = loadInt(src, &status);
	if (!dst->stack_size) {
		if (status < 0) return (BRBLoadError){
			.code = BRB_ERR_NO_STACK_SIZE
		};
		dst->stack_size = DEFAULT_STACK_SIZE;
	}
	dst->stack_size *= 1024;
// loading dependencies
	int64_t n = loadInt(src, &status);
	if (status < 0) return (BRBLoadError){
		.code = BRB_ERR_NO_LOAD_SEGMENT
	};
	for (int64_t i = 0; i < n; i++) {
		char* name = NULL;
		size_t name_length = 0;

		name_length = getline(&name, &name_length, src);
		if (!name_length || !name) return (BRBLoadError){
			.code = BRB_ERR_NO_NAME_SPEC
		};
		name[--name_length] = '\0';

		FILE* module_fd = findModule(name, module_paths);
		if (!module_fd) return (BRBLoadError){
			.code = BRB_ERR_MODULE_NOT_FOUND,
			.module_name = name
		};
		Module submodule;
		BRBLoadError err = preloadModule(module_fd, &submodule, module_paths);
		if (err.code) return err;

		dst = mergeModule(&submodule, dst, name);
	}
// loading data blocks
	n = loadInt(src, &status);
	if (status < 0) return (BRBLoadError){
		.code = BRB_ERR_NO_DATA_SEGMENT
	};
	DataBlockArray_resize(&dst->seg_data, n);
	for (int64_t i = dst->seg_data.length - n; i < dst->seg_data.length; i++) {
		BRBLoadError err = loadDataBlock(&loader, dst->seg_data.data + i);
		if (err.code) return err;
	}
//  loading memory blocks
	n = loadInt(src, &status);
	if (status < 0) return (BRBLoadError){.code = BRB_ERR_NO_MEMORY_SEGMENT};
	MemBlockArray_resize(&dst->seg_memory, n);
	for (int64_t i = dst->seg_memory.length - n; i < dst->seg_memory.length; i++) {
		load2Ints(src, (int64_t*)&dst->seg_memory.data[i].name, &dst->seg_memory.data[i].size, &status);
		if (status < 0) return (BRBLoadError){.code = BRB_ERR_INVALID_BLOCK};
	}
//  loading operations
	n = loadInt(src, &status);
	if (status < 0) return (BRBLoadError){
		.code = BRB_ERR_NO_EXEC_SEGMENT
	};
	OpArray_resize(&dst->seg_exec, n);
	for (int64_t i = dst->seg_exec.length - n; i < dst->seg_exec.length; i++) {
		Op* op = dst->seg_exec.data + i;

		op->type = loadInt8(src, &status);
		if (status < 0) return (BRBLoadError){
			.code = BRB_ERR_NO_OPCODE
		};
		if (op->type < 0) {
			op->type = ~op->type;
			op->cond_id = loadInt8(src, &status);
			if (status < 0) return (BRBLoadError){
				.code = BRB_ERR_NO_COND_ID
			};
			if (!inRange(op->cond_id, 0, N_CONDS)) return (BRBLoadError){
				.code = BRB_ERR_INVALID_COND_ID,
				.cond_id = op->cond_id
			};
		}
		if (op->type >= N_OPS) return (BRBLoadError){
			.code = BRB_ERR_INVALID_OPCODE,
			.opcode = op->type
		};

		BRBLoadError err = op_loaders[op->type](&loader, op);
		if (err.code) return err;
	}
//  resolving symbol names
	n = loadInt(src, &status);
	if (status < 0) return (BRBLoadError){
		.code = BRB_ERR_NO_NAME_SEGMENT
	};

	fieldArray unresolved = getUnresolvedNames(loader.dst);

	for (int64_t i = 0; i < n; i++) {
		char* name = NULL;
		size_t name_length = 0;

		name_length = getline(&name, &name_length, src);
		if (!name_length || !name) return (BRBLoadError){
			.code = BRB_ERR_NO_NAME_SPEC
		};
		name[--name_length] = '\0';

		for (unsigned int i1 = 0; i1 < unresolved.length; ++i1) {
			if (*unresolved.data[i1] == (char*)i) {
				*unresolved.data[i1] = name;
				fieldArray_pop(&unresolved, i1);
				i1 -= 1;
			}
		}
	}
	
	if (unresolved.length) {
		fieldArray_clear(&unresolved);
		return (BRBLoadError){.code = BRB_ERR_UNRESOLVED_NAMES};
	}
	return (BRBLoadError){0};
}

BRBLoadError resolveModule(Module* dst, bool for_exec)
{
	Submodule *submodule, *root = SubmoduleArray_append(&dst->submodules, getRootSubmodule(dst, "."));

// resolving references to data and memory blocks in `data` segment
	for (
		DataBlock* block = dst->seg_data.data;
		block - dst->seg_data.data < dst->seg_data.length;
		++block
	) {
		for (
			DataPiece* piece = block->pieces.data;
			piece - block->pieces.data < block->pieces.length;
			++piece
		) {
			switch (piece->type) {
				case PIECE_DB_ADDR:
					submodule = dst->submodules.data + piece->module_id;
					for (
						DataBlock* block_iter = dst->seg_data.data + submodule->ds_offset;
						block_iter - dst->seg_data.data < submodule->ds_offset + submodule->ds_length;
						++block_iter
					) {
						if (sbufeq(block_iter->name, piece->mark_name)) {
							piece->symbol_id = block_iter - dst->seg_data.data;
							submodule = NULL;
							break;
						}
					}

					if (submodule) return (BRBLoadError){
						.code = BRB_ERR_UNRESOLVED_DB_REF,
						.module_name = submodule->name,
						.mark_name = piece->mark_name
					};
					break;
				case PIECE_MB_ADDR:
					submodule = dst->submodules.data + piece->module_id;
					for (
						MemBlock* block_iter = dst->seg_memory.data + submodule->ms_offset;
						block_iter - dst->seg_memory.data < submodule->ms_offset + submodule->ms_length;
						++block_iter
					) {
						if (sbufeq(block_iter->name, piece->mark_name)) {
							piece->symbol_id = block_iter - dst->seg_memory.data;
							submodule = NULL;
							break;
						}
					}

					if (submodule) return (BRBLoadError){
						.code = BRB_ERR_UNRESOLVED_MB_REF,
						.module_name = submodule->name,
						.mark_name = piece->mark_name
					};
					break;
				default:
					break;
			}
		}
	}
// resolving references to data blocks, memory blocks and procedures in `exec` segment
	for (int i = 0; i < dst->seg_exec.length; i++) {
		Op* op = dst->seg_exec.data + i;
		
		switch (op->type) {
			case OP_SETD:
				submodule = dst->submodules.data + op->module_id;
				for (int64_t db_i = submodule->ds_offset; db_i < submodule->ds_offset + submodule->ds_length; db_i++) {
					if (sbufeq(op->mark_name, dst->seg_data.data[db_i].name)) {
						op->symbol_id = db_i;
						submodule = NULL;
						break;
					}
				}

				if (submodule) return (BRBLoadError){
					.code = BRB_ERR_UNRESOLVED_DB_REF,
					.module_name = submodule->name,
					.mark_name = op->mark_name
				};
				break;
			case OP_SETM:
				submodule = dst->submodules.data + op->module_id;
				for (int64_t mb_i = submodule->ms_offset; mb_i < submodule->ms_offset + submodule->ms_length; mb_i++) {
					if (sbufeq(op->mark_name, dst->seg_memory.data[mb_i].name)) {
						op->symbol_id = mb_i;
						submodule = NULL;
						break;
					}
				}

				if (submodule) return (BRBLoadError){
					.code = BRB_ERR_UNRESOLVED_MB_REF,
					.module_name = submodule->name,
					.mark_name = op->mark_name
				};
				break;
			case OP_CALL:
				submodule = dst->submodules.data + op->module_id;
				for (int64_t proc_index = submodule->es_offset; proc_index < submodule->es_offset + submodule->es_length; proc_index++) {
					Op* proc = dst->seg_exec.data + proc_index;

					if (proc->type == OP_PROC || proc->type == OP_EXTPROC) {
						if (sbufeq(proc->mark_name, op->mark_name)) {
							op->symbol_id = proc_index;
							submodule = NULL;
							break;
						}
					}
				}

				if (submodule) return (BRBLoadError){
					.code = BRB_ERR_UNRESOLVED_PROC_REF,
					.module_name = submodule->name,
					.mark_name = op->mark_name
				};
				break;
			default:
				break;
		}
	}


//  resolving entry
	dst->entry_opid = -1;
	if (for_exec) {
		for (Op* op = dst->seg_exec.data + root->es_offset; op - dst->seg_exec.data < root->es_offset + root->es_length; ++op) {
			if (op->type == OP_EXTPROC) {
				if (sbufeq(op->mark_name, "main")) {
					dst->entry_opid = op - dst->seg_exec.data;
					break;
				}
			}
		}
		if (dst->entry_opid < 0) return (BRBLoadError){.code = BRB_ERR_NO_ENTRY};
	}

	return (BRBLoadError){0};
}

BRBLoadError loadModule(FILE* src, Module* dst, char* search_paths[], int flags)
{
	BRBLoadError err = preloadModule(src, dst, search_paths);
	if (err.code) return err;
	return resolveModule(dst, flags & BRB_EXECUTABLE);
}
