// implementation for loading BRB modules from `.brb` files
#include <br.h>
#include <errno.h>

typedef struct {
	BR_ModuleBuilder* builder;
	FILE* src;
	long n_fetched;
} BR_ModuleLoader;

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

static BR_Type loadType(FILE* src, long* n_fetched)
{
	uint8_t hb = loadHalfByte(src, n_fetched);
	BR_Type res = {
		.kind = 1 << hb,
		.n_items = loadInt(src, n_fetched),
	};
	if (res.kind == BR_TYPE_STRUCT)
		res.struct_id = loadInt(src, n_fetched);
	return res;
}

static BR_Error loadDataBlockDecl(BR_ModuleLoader* loader, uint32_t* n_pieces_p)
{
// loading the flags
	bool is_mutable = loadHalfByte(loader->src, &loader->n_fetched);
// loading the name ID
	const char* name = (const char*)loadInt(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_DB_NAME};
// loading the body size
	BR_id db_id;
	*n_pieces_p = loadInt(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_DB_BODY_SIZE};
	return BR_addDataBlock(loader->builder, &db_id, name, is_mutable, *n_pieces_p);
}

static BR_Error loadOp(BR_ModuleLoader* loader, BR_id proc_id)
{
	BR_Op op;
// loading the type
	op.type = loadInt8(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_OPCODE};
	if (op.type >= BR_N_OPS) return (BR_Error){.type = BR_ERR_INVALID_OPCODE, .opcode = op.type};
// loading the operand, if needed
	switch (BR_GET_OPERAND_TYPE(op.type)) {
		case BR_OPERAND_INT8:
			op.operand_u = loadInt8(loader->src, &loader->n_fetched);
			break;
		case BR_OPERAND_INT:
		case BR_OPERAND_BUILTIN:
		case BR_OPERAND_VAR_NAME:
		case BR_OPERAND_SYSCALL_NAME:
			op.operand_u = loadInt(loader->src, &loader->n_fetched);
			break;
		case BR_OPERAND_DB_NAME:
			op.operand_s = ~loadInt(loader->src, &loader->n_fetched);
			break;
		case BR_OPERAND_TYPE:
			op.operand_type = loadType(loader->src, &loader->n_fetched);
		case BR_OPERAND_NONE:
			break;
		default:
			assert(false, "invalid operation type info");
	}
	if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_OPERAND, .opcode = op.type};
	return BR_addOp(loader->builder, proc_id, op);
}

static BR_Error loadProcDecl(BR_ModuleLoader* loader, uint32_t* n_ops_p)
{
// loading the return type
	BR_Type ret_type = loadType(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_PROC_RET_TYPE};
// loading the name ID and amount of arguments
	uint64_t n_args, proc_name;
	load2Ints(loader->src, &proc_name, &n_args, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_PROC_NAME};
// loading the arguments
	BR_Type args[n_args];
	for (size_t i = 0; i < n_args; ++i) {
		args[i] = loadType(loader->src, &loader->n_fetched);
		if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_PROC_ARG};
	}
// loading the body size
	BR_id proc_id;
	*n_ops_p = loadInt(loader->src, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_PROC_BODY_SIZE};
	return BR_addProc(loader->builder, &proc_id, (char*)proc_name, n_args, args, ret_type, *n_ops_p);
}

static BR_Error loadStruct(BR_ModuleLoader* loader)
{
	uint64_t name_id, n_fields;
	load2Ints(loader->src, &name_id, &n_fields, &loader->n_fetched);
	if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_STRUCT_DECL};
	BR_Type fields[n_fields];
	for (uint64_t i = 0; i < n_fields; ++i) {
		fields[i] = loadType(loader->src, &loader->n_fetched);
		if (loader->n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_STRUCT_FIELD};
	}
	BR_id struct_id;
	return BR_addStruct(loader->builder, &struct_id, (char*)name_id, n_fields, fields);
}

defArray_as(const char**, fieldArray);

static fieldArray getNameFields(BR_Module* module)
{
	fieldArray res = fieldArray_new(-(int64_t)module->seg_data.length - module->seg_exec.length);
	arrayForeach (BR_DataBlock, block, module->seg_data) {
		fieldArray_append(&res, &block->name);
	}
	arrayForeach (BR_Proc, proc, module->seg_exec) {
		fieldArray_append(&res, &proc->name);
	}
	arrayForeach (BR_Struct, _struct, module->seg_typeinfo) {
		fieldArray_append(&res, &_struct->name);
	}
	return res;
}

BR_Error BR_loadFromBytecode(FILE* src, BR_ModuleBuilder* dst)
{
	if (dst->error.type) return dst->error;
	BR_ModuleLoader loader = {.builder = dst};
	BR_Error err;
	loader.src = src;
// loading the header
	char header[BR_HEADER_SIZE];
	if (!fread(header, sizeof(header), 1, src))
		return (BR_Error){.type = BR_ERR_NO_HEADER};
	loader.n_fetched += sizeof(header);
	if (*(uint64_t*)header != *(uint64_t*)BR_V1_HEADER.data)
		return (BR_Error){.type = BR_ERR_INVALID_HEADER};
// loading the amount of data blocks, procedures and structs
	uint64_t n_structs = loadInt(src, &loader.n_fetched),
		n_dbs, n_procs;
	load2Ints(src, &n_dbs, &n_procs, &loader.n_fetched);
	if (loader.n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_SEG_SIZES};
	if ((err = BR_preallocDataBlocks(loader.builder, n_dbs)).type) return err;
	if ((err = BR_preallocProcs(loader.builder, n_procs)).type) return err;
	if ((err = BR_preallocStructs(loader.builder, n_structs)).type) return err;
// loading the structs 
	for (uint32_t i = 0; i < n_structs; ++i) {
		if ((err = loadStruct(&loader)).type) return err;
	}
// loading the data block declarations
	uint32_t n_pieces_per_db[n_dbs];
	for (uint32_t i = 0; i < n_dbs; ++i) {
		if ((err = loadDataBlockDecl(&loader, &n_pieces_per_db[i])).type) return err;
	}
// loading the procedure declarations
	uint32_t n_ops_per_proc[n_procs];
	for (uint32_t i = 0; i < n_procs; ++i) {
		if ((err = loadProcDecl(&loader, &n_ops_per_proc[i])).type) return err;
	}
// loading the execution entry point
	loader.builder->module.exec_entry_point = loadInt(src, &loader.n_fetched);
	if (loader.n_fetched < 0) return (BR_Error){.type = BR_ERR_NO_ENTRY};
// loading the operations for the data blocks
	for (size_t i = 0; i < (size_t)n_dbs; ++i) {
		repeat (n_pieces_per_db[i]) {
			if ((err = loadOp(&loader, ~(BR_id)i)).type) return err;
		}
	}
// loading the operations for the procedures
	for (size_t i = 0; i < (size_t)n_procs; ++i) {
		repeat (n_ops_per_proc[i]) {
			if ((err = loadOp(&loader, i)).type) return err;
		}
	}
// loading the names
	fieldArray unresolved = getNameFields(&loader.builder->module);
	uint32_t resolved = 0;
	sbuf name = {0};
	for (uint32_t i = 0; true; ++i) {
		name.length = getdelim(&name.data, &name.length, '\0', src);
		if ((ssize_t)name.length < 0) {
			if (ferror(src) || errno)
				return (BR_Error){.type = BR_ERR_INVALID_NAME};
			break;
		}
		if (name.data[name.length - 1] != '\0')
			return (BR_Error){.type = BR_ERR_INVALID_NAME};
		arrayForeach_as (const char**, fieldArray, name_p, unresolved) {
			if ((uintptr_t)**name_p == i) {
				**name_p = strdup(name.data);
				++resolved;
			}
		}
	}
	sbuf_dealloc(&name);
	if (resolved != unresolved.length) {
		fieldArray_clear(&unresolved);
		return (BR_Error){.type = BR_ERR_NAMES_NOT_RESOLVED};
	}
	fieldArray_clear(&unresolved);
	return (BR_Error){0};
}
