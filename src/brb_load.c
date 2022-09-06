// implementation for loading BRB modules from `.brb` files
#include <brb.h>
#include <errno.h>

defArray(BRB_Op);
defArray(BRB_DataBlock);
defArray(BRB_DataPiece);

typedef struct {
	BRB_ModuleBuilder builder;
	FILE* src;
	long n_fetched;
} BRB_ModuleLoader;

uint8_t BRB_loadInt8(FILE* fd, long* n_fetched)
{
	char res;
	if (!fread(&res, 1, 1, fd)) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	*n_fetched += 1;
	return res;
}

uint16_t BRB_loadInt16(FILE* fd, long* n_fetched)
{
	char res[2];
	if (!fread(res, 2, 1, fd)) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	*n_fetched += 2;
	return *(uint16_t*)BRByteOrder(&res, 2); 
}

uint32_t BRB_loadInt32(FILE* fd, long* n_fetched)
{
	char res[4];
	if (!fread(res, 4, 1, fd)) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	*n_fetched += 4;
	return *(uint32_t*)BRByteOrder(&res, 4);
}

uint64_t BRB_loadInt64(FILE* fd, long* n_fetched)
{
	char res[8];
	if (!fread(res, 8, 1, fd)) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	*n_fetched += 8;
	return *(uint64_t*)BRByteOrder(&res, 8);
}

int64_t BRB_loadInt(FILE* fd, long* n_fetched)
{
	register uint8_t size = BRB_loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return 0;
	switch (size) {
		case 8: return BRB_loadInt8(fd, n_fetched);
		case 9: return BRB_loadInt16(fd, n_fetched);
		case 10: return BRB_loadInt32(fd, n_fetched);
		case 11: return BRB_loadInt64(fd, n_fetched);
		case 12: return ~(int64_t)BRB_loadInt8(fd, n_fetched);
		case 13: return ~(int64_t)BRB_loadInt16(fd, n_fetched);
		case 14: return ~(int64_t)BRB_loadInt32(fd, n_fetched);
		case 15: return ~(int64_t)BRB_loadInt64(fd, n_fetched);
		default: return size;
	}
}

uint8_t BRB_loadHalfByte(FILE* fd, long* n_fetched)
{
	register uint8_t res = BRB_loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return 0;
	if (ungetc(res & 0xF, fd) == EOF) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	return res >> 4;
}

void BRB_load2HalfBytes(FILE* fd, uint8_t* hb1, uint8_t* hb2, long* n_fetched)
{
	register uint8_t res = BRB_loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return;

	*hb1 = res >> 4;
	*hb2 = res & 0xF;
}

void BRB_load2Ints(FILE* fd, int64_t* x, int64_t* y, long* n_fetched)
{
	uint8_t sizes = BRB_loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return;

	switch (sizes >> 4) {
		case 8: *x = BRB_loadInt8(fd, n_fetched); break;
		case 9: *x = BRB_loadInt16(fd, n_fetched); break;
		case 10: *x = BRB_loadInt32(fd, n_fetched); break;
		case 11: *x = BRB_loadInt64(fd, n_fetched); break;
		case 12: *x = ~(int64_t)BRB_loadInt8(fd, n_fetched); break;
		case 13: *x = ~(int64_t)BRB_loadInt16(fd, n_fetched); break;
		case 14: *x = ~(int64_t)BRB_loadInt32(fd, n_fetched); break;
		case 15: *x = ~(int64_t)BRB_loadInt64(fd, n_fetched); break;
		default: *x = sizes >> 4; break;
	}

	switch (sizes & 0xF) {
		case 8: *y = BRB_loadInt8(fd, n_fetched); break;
		case 9: *y = BRB_loadInt16(fd, n_fetched); break;
		case 10: *y = BRB_loadInt32(fd, n_fetched); break;
		case 11: *y = BRB_loadInt64(fd, n_fetched); break;
		case 12: *y = ~(int64_t)BRB_loadInt8(fd, n_fetched); break;
		case 13: *y = ~(int64_t)BRB_loadInt16(fd, n_fetched); break;
		case 14: *y = ~(int64_t)BRB_loadInt32(fd, n_fetched); break;
		case 15: *y = ~(int64_t)BRB_loadInt64(fd, n_fetched); break;
		default: *y = sizes & 0xF; break;
	}
}

BRB_Error BRB_loadDataBlock(BRB_ModuleLoader* loader)
{ // TODO: make errors returned by the procedure more descriptive
	bool is_mutable = BRB_loadHalfByte(loader->src, &loader->n_fetched);
	const char* name = (const char*)BRB_loadInt(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_INVALID_DB};

	uint32_t db_id, n_pieces = BRB_loadInt(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_INVALID_DB};

	BRB_Error err = BRB_addDataBlock(&loader->builder, &db_id, name, is_mutable);
	if (!(loader->builder.module.seg_data.data[db_id].pieces = BRB_DataPieceArray_new(-(int64_t)n_pieces)).data)
		return (BRB_Error){.type = BRB_ERR_NO_MEMORY};

	BRB_DataPiece piece = {0};
	while (n_pieces--) {
		piece.type = BRB_loadInt8(loader->src, &loader->n_fetched);
		if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_INVALID_DB};

		switch (piece.type) {
			case BRB_DP_BYTES:
				piece.data = smalloc(BRB_loadInt(loader->src, &loader->n_fetched));
				if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_INVALID_DB};

				if (!fread(piece.data.data, piece.data.length, 1, loader->src))
					return (BRB_Error){.type = BRB_ERR_INVALID_DB};
				loader->n_fetched += piece.data.length;
				break;
			case BRB_DP_TEXT:
				piece.data = (sbuf){0};
				if (getdelim(&piece.data.data, (size_t*)&piece.data.length, '\0', loader->src) < 0)
					return (BRB_Error){.type = BRB_ERR_INVALID_DB};
				break;
			case BRB_DP_INT16:
			case BRB_DP_INT32:
			case BRB_DP_INTPTR:
			case BRB_DP_INT64:
			case BRB_DP_DBADDR:
			case BRB_DP_ZERO:
			case BRB_DP_BUILTIN:
				piece.operand_u = BRB_loadInt(loader->src, &loader->n_fetched);
				if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_INVALID_DB};
				break;
			case BRB_DP_NONE:
			case BRB_N_DP_TYPES:
			default:
				return (BRB_Error){.type = BRB_ERR_INVALID_DB};
		}
		err = BRB_addDataPiece(&loader->builder, db_id, piece);
		if (err.type) return err;
		piece = (BRB_DataPiece){0};
	}

	return (BRB_Error){0};
}

BRB_Error BRB_loadOp(BRB_ModuleLoader* loader)
{
	BRB_Op op;
	op.type = BRB_loadInt8(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_OPCODE};
	if (op.type >= BRB_N_OPS) return (BRB_Error){.type = BRB_ERR_INVALID_OPCODE, .opcode = op.type};

	if (BRB_opFlags[op.type] & BRB_OPF_HAS_OPERAND) {
		op.operand_u = BRB_loadInt(loader->src, &loader->n_fetched);
		if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_OPERAND, .opcode = op.type};
	} 
	return BRB_addOp(&loader->builder, op);
}

BRB_Error BRB_loadModule(FILE* src, BRB_Module* dst)
{
	BRB_ModuleLoader loader = {0};
	loader.src = src;
	BRB_initModuleBuilder(&loader.builder);

	char header[BRB_HEADER_SIZE];
	if (!fread(header, sizeof(header), 1, src))
		return (BRB_Error){.type = BRB_ERR_NO_HEADER};
	loader.n_fetched += sizeof(header);

	if (*(uint64_t*)header != *(uint64_t*)BRB_V1_HEADER.data)
		return (BRB_Error){.type = BRB_ERR_INVALID_HEADER};

	int64_t seg_size = BRB_loadInt(src, &loader.n_fetched);
	if (loader.n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_DATA_SEG};

	if (!(loader.builder.module.seg_data = BRB_DataBlockArray_new(-seg_size)).data)
		return (BRB_Error){.type = BRB_ERR_NO_MEMORY};

	while (seg_size--) {
		BRB_Error err = BRB_loadDataBlock(&loader);
		if (err.type) return err;
	}

	seg_size = BRB_loadInt(src, &loader.n_fetched);
	if (loader.n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_EXEC_SEG};

	if (!(loader.builder.module.seg_exec = BRB_OpArray_new(-seg_size)).data)
		return (BRB_Error){.type = BRB_ERR_NO_MEMORY};

	while (seg_size--) {
		BRB_Error err = BRB_loadOp(&loader);
		if (err.type) return err;
	}

	fieldArray unresolved = BRB_getNameFields(&loader.builder.module);

	uint32_t resolved = 0;
	sbuf name = {0};
	for (uint32_t i = 0; true; ++i) {
		name.length = getdelim(&name.data, &name.length, '\0', src);
		if ((ssize_t)name.length < 0) {
			if (ferror(src) || errno)
				return (BRB_Error){.type = BRB_ERR_INVALID_NAME};
			break;
		}

		if (name.data[name.length - 1] != '\0')
			return (BRB_Error){.type = BRB_ERR_INVALID_NAME};

		arrayForeach (field, name_p, unresolved) {
			if ((uintptr_t)**name_p == i) {
				**name_p = strdup(name.data);
				++resolved;
			}
		}
	}
	sfree(&name);
	
	if (resolved != unresolved.length) {
		fieldArray_clear(&unresolved);
		return (BRB_Error){.type = BRB_ERR_NAMES_NOT_RESOLVED};
	}

	fieldArray_clear(&unresolved);
	return BRB_extractModule(loader.builder, dst);
}
