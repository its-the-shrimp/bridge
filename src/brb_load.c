// implementation for loading BRB modules from `.brb` files
#include <brb.h>
#include <errno.h>

typedef struct {
	BRB_ModuleBuilder builder;
	FILE* src;
	long n_fetched;
} BRB_ModuleLoader;

static uint8_t loadInt8(FILE* fd, long* n_fetched)
{
	char res;
	if (!fread(&res, 1, 1, fd)) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	*n_fetched += 1;
	return res;
}

static uint16_t loadInt16(FILE* fd, long* n_fetched)
{
	char res[2];
	if (!fread(res, 2, 1, fd)) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	*n_fetched += 2;
	return *(uint16_t*)BRByteOrder(&res, 2); 
}

static uint32_t loadInt32(FILE* fd, long* n_fetched)
{
	char res[4];
	if (!fread(res, 4, 1, fd)) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	*n_fetched += 4;
	return *(uint32_t*)BRByteOrder(&res, 4);
}

static uint64_t loadInt64(FILE* fd, long* n_fetched)
{
	char res[8];
	if (!fread(res, 8, 1, fd)) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	*n_fetched += 8;
	return *(uint64_t*)BRByteOrder(&res, 8);
}


int64_t loadInt(FILE* fd, long* n_fetched)
{
	register uint8_t size = loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return 0;
	switch (size) {
		case 8:  return loadInt8(fd, n_fetched);
		case 9:  return loadInt16(fd, n_fetched);
		case 10: return loadInt32(fd, n_fetched);
		case 11: return loadInt64(fd, n_fetched);
		case 12: return -loadInt8(fd, n_fetched);
		case 13: return -loadInt16(fd, n_fetched);
		case 14: return -loadInt32(fd, n_fetched);
		case 15: return -loadInt64(fd, n_fetched);
		default: return size;
	}
}

static uint8_t loadHalfByte(FILE* fd, long* n_fetched)
{
	register uint8_t res = loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return 0;
	if (ungetc(res & 0xF, fd) == EOF) {
		*n_fetched = -*n_fetched;
		return 0;
	}
	return res >> 4;
}

static void load2Ints(FILE* fd, uint64_t* x, uint64_t* y, long* n_fetched)
{
	uint8_t sizes = loadInt8(fd, n_fetched);
	if (*n_fetched < 0) return;

	switch (sizes >> 4) {
		case 8:  *x = loadInt8(fd, n_fetched); break;
		case 9:  *x = loadInt16(fd, n_fetched); break;
		case 10: *x = loadInt32(fd, n_fetched); break;
		case 11: *x = loadInt64(fd, n_fetched); break;
		case 12: *x = -(int64_t)loadInt8(fd, n_fetched); break;
		case 13: *x = -(int64_t)loadInt16(fd, n_fetched); break;
		case 14: *x = -(int64_t)loadInt32(fd, n_fetched); break;
		case 15: *x = -(int64_t)loadInt64(fd, n_fetched); break;
		default: *x = sizes >> 4; break;
	}

	switch (sizes & 0xF) {
		case 8:  *y = loadInt8(fd, n_fetched); break;
		case 9:  *y = loadInt16(fd, n_fetched); break;
		case 10: *y = loadInt32(fd, n_fetched); break;
		case 11: *y = loadInt64(fd, n_fetched); break;
		case 12: *y = -(int64_t)loadInt8(fd, n_fetched); break;
		case 13: *y = -(int64_t)loadInt16(fd, n_fetched); break;
		case 14: *y = -(int64_t)loadInt32(fd, n_fetched); break;
		case 15: *y = -(int64_t)loadInt64(fd, n_fetched); break;
		default: *y = sizes & 0xF; break;
	}
}

static BRB_Type loadType(FILE* src, long* n_fetched)
{
	return (BRB_Type){
		.kind = loadHalfByte(src, n_fetched),
		.n_items = loadInt(src, n_fetched)
	};
}

static BRB_Error loadDataBlockDecl(BRB_ModuleLoader* loader, uint32_t* n_pieces_p)
{
// loading the flags
	bool is_mutable = loadHalfByte(loader->src, &loader->n_fetched);
// loading the name ID
	const char* name = (const char*)loadInt(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_DB_NAME};
// loading the body size
	BRB_id_t db_id;
	*n_pieces_p = loadInt(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_DB_BODY_SIZE};
	return BRB_addDataBlock(&loader->builder, &db_id, name, is_mutable, *n_pieces_p);
}

static BRB_Error loadOp(BRB_ModuleLoader* loader, BRB_id_t proc_id)
{
	BRB_Op op;
// loading the type
	op.type = loadInt8(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_OPCODE};
	if (op.type >= BRB_N_OPS) return (BRB_Error){.type = BRB_ERR_INVALID_OPCODE, .opcode = op.type};
// loading the operand, if needed
	switch (BRB_GET_OPERAND_TYPE(op.type)) {
		case BRB_OPERAND_INT8:
			op.operand_u = loadInt8(loader->src, &loader->n_fetched);
			break;
		case BRB_OPERAND_INT:
		case BRB_OPERAND_BUILTIN:
		case BRB_OPERAND_VAR_NAME:
		case BRB_OPERAND_SYSCALL_NAME:
			op.operand_u = loadInt(loader->src, &loader->n_fetched);
			break;
		case BRB_OPERAND_DB_NAME:
			op.operand_s = ~loadInt(loader->src, &loader->n_fetched);
			break;
		case BRB_OPERAND_TYPE:
			op.operand_type = loadType(loader->src, &loader->n_fetched);
		case BRB_OPERAND_NONE:
			break;
		default:
			assert(false, "invalid operation type info");
	}
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_OPERAND, .opcode = op.type};
	return BRB_addOp(&loader->builder, proc_id, op);
}

static BRB_Error loadProcDecl(BRB_ModuleLoader* loader, uint32_t* n_ops_p)
{
// loading the return type
	BRB_Type ret_type = loadType(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_PROC_RET_TYPE};
// loading the name ID and amount of arguments
	uint64_t n_args, proc_name;
	load2Ints(loader->src, &proc_name, &n_args, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_PROC_NAME};
// loading the arguments
	BRB_Type args[n_args];
	for (size_t i = 0; i < n_args; ++i) {
		args[i] = loadType(loader->src, &loader->n_fetched);
		if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_PROC_ARG};
	}
// loading the body size
	BRB_id_t proc_id;
	*n_ops_p = loadInt(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_PROC_BODY_SIZE};
	return BRB_addProc(&loader->builder, &proc_id, (char*)proc_name, n_args, args, ret_type, *n_ops_p);
}

BRB_Error BRB_loadModule(FILE* src, BRB_Module* dst)
{
	BRB_ModuleLoader loader = {0};
	BRB_Error err;
	loader.src = src;
	BRB_initModuleBuilder(&loader.builder);
// loading the header
	char header[BRB_HEADER_SIZE];
	if (!fread(header, sizeof(header), 1, src))
		return (BRB_Error){.type = BRB_ERR_NO_HEADER};
	loader.n_fetched += sizeof(header);
	if (*(uint64_t*)header != *(uint64_t*)BRB_V1_HEADER.data)
		return (BRB_Error){.type = BRB_ERR_INVALID_HEADER};
// loading the amount of data blocks
	uint32_t n_dbs = loadInt(src, &loader.n_fetched);
	if (loader.n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_DATA_SEG};
	if ((err = BRB_preallocDataBlocks(&loader.builder, n_dbs)).type) return err;
// loading the data block declarations
	uint32_t n_pieces_per_db[n_dbs];
	for (uint32_t i = 0; i < n_dbs; ++i) {
		if ((err = loadDataBlockDecl(&loader, &n_pieces_per_db[i])).type) return err;
	}
// loading the amount of procedures
	uint32_t n_procs = loadInt(src, &loader.n_fetched);
	if (loader.n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_EXEC_SEG};
	if ((err = BRB_preallocProcs(&loader.builder, n_procs)).type) return err;
// loading the procedure declarations
	uint32_t n_ops_per_proc[n_procs];
	for (uint32_t i = 0; i < n_procs; ++i) {
		if ((err = loadProcDecl(&loader, &n_ops_per_proc[i])).type) return err;
	}
// loading the execution entry point
	loader.builder.module.exec_entry_point = loadInt(src, &loader.n_fetched);
	if (loader.n_fetched < 0) return (BRB_Error){.type = BRB_ERR_NO_ENTRY};
// loading the operations for the data blocks
	for (size_t i = 0; i < (size_t)n_dbs; ++i) {
		repeat (n_pieces_per_db[i]) {
			if ((err = loadOp(&loader, ~(BRB_id_t)i)).type) return err;
		}
	}
// loading the operations for the procedures
	for (size_t i = 0; i < (size_t)n_procs; ++i) {
		repeat (n_ops_per_proc[i]) {
			if ((err = loadOp(&loader, i)).type) return err;
		}
	}
// loading the names
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
