#include "brb.h"
#include "stdio.h"
#include "errno.h"
#include "unistd.h"
#include "signal.h"
#include "stdlib.h"

defArray(Op);
defArray(DataBlock);
defArray(MemBlock);
defArray(str);

typedef struct {
	Module* src;
	FILE* dst;
	strArray consts;
} ModuleWriter;

typedef str* field;
declArray(field);
defArray(field);
typedef struct {
	Module* dst;
	FILE* src;
	fieldArray unresolved;
} ModuleLoader;

void writeInt(FILE* fd, uint64_t x)
{
	if (x) {
		if (x <= UINT8_MAX) {
			fputc(1, fd);
			int8_t x8 = (int8_t)x;
			fputc(x8, fd);
		} else if (x <= UINT16_MAX) {
			fputc(2, fd);
			int16_t x16 = (int16_t)x;
			fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
		} else if (x <= UINT32_MAX) {
			fputc(4, fd);
			int32_t x32 = (int32_t)x;
			fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
		} else {
			fputc(8, fd);
			fwrite(BRByteOrder(&x, 8), 8, 1, fd);
		}
	} else {
		fputc(0, fd);
	}
}

void writeName(ModuleWriter* writer, char* name)
{
	for (long i = 0; i < writer->consts.length; i++) {
		if (streq(name, writer->consts.data[i])) {
			writeInt(writer->dst, i);
			return;
		}
	}
	writeInt(writer->dst, writer->consts.length);
	strArray_append(&writer->consts, name);
}

void writeDataBlock(ModuleWriter* writer, char* name, sbuf obj)
{
	writeName(writer, name);
	writeInt(writer->dst, obj.length);
	fputsbuf(writer->dst, obj);
}

void writeMemoryBlock(ModuleWriter* writer, char* name, int32_t size)
{
	writeName(writer, name);
	writeInt(writer->dst, size);
}

typedef void (*OpWriter) (ModuleWriter*, Op);

void writeNoArgOp(ModuleWriter* writer, Op op) {}

void writeMarkOp(ModuleWriter* writer, Op op)
{
	writeName(writer, op.mark_name);
}

void writeRegImmOp(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	writeInt(writer->dst, op.value);
}

void write2RegOp(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	fputc(op.src_reg, writer->dst);
}

void writeRegSymbolIdOp(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	writeInt(writer->dst, op.symbol_id);
}

void writeOpSetd(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	writeName(writer, writer->src->datablocks.data[op.symbol_id].name);
}

void writeOpSetm(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	writeName(writer, writer->src->memblocks.data[op.symbol_id].name);
}

void writeOpSyscall(ModuleWriter* writer, Op op)
{
	fputc(op.syscall_id, writer->dst);
}

void writeOpGoto(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.op_offset);
}

void writeOpCall(ModuleWriter* writer, Op op)
{
	writeName(writer, writer->src->execblock.data[op.symbol_id].mark_name);
}

void writeOpCmp(ModuleWriter* writer, Op op)
{
	fputc(op.src_reg, writer->dst);
	writeInt(writer->dst, op.value);
}

void writeOpCmpr(ModuleWriter* writer, Op op)
{
	fputc(op.src_reg, writer->dst);
	fputc(op.src2_reg, writer->dst);
}

void write2RegImmOp(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	fputc(op.src_reg, writer->dst);
	writeInt(writer->dst, op.value);
}

void write3RegOp(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	fputc(op.src_reg, writer->dst);
	fputc(op.src2_reg, writer->dst);
}

void writeOpVar(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.new_var_size);
}

void writeOpLdv(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	writeInt(writer->dst, op.symbol_id);
	fputc(op.var_size, writer->dst);
}

void writeOpStrv(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.symbol_id);
	fputc(op.var_size, writer->dst);
	fputc(op.src_reg, writer->dst);
}

void writeOpPopv(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	fputc(op.var_size, writer->dst);
}

void writeOpPushv(ModuleWriter* writer, Op op)
{
	fputc(op.var_size, writer->dst);
	fputc(op.src_reg, writer->dst);
}

void writeSymbolIdOp(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.symbol_id);
}

void writeOpSetc(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	fputc(op.cond_arg, writer->dst);
}

const OpWriter op_writers[] = {
	[OP_NONE] = &writeNoArgOp,
	[OP_END] = &writeNoArgOp,
	[OP_MARK] = &writeNoArgOp,
	[OP_SET] = &writeRegImmOp,
	[OP_SETR] = &write2RegOp,
	[OP_SETD] = &writeOpSetd,
	[OP_SETB] = &writeRegSymbolIdOp,
	[OP_SETM] = &writeOpSetm,
	[OP_ADD] = &write2RegImmOp,
	[OP_ADDR] = &write3RegOp,
	[OP_SUB] = &write2RegImmOp,
	[OP_SUBR] = &write3RegOp,
	[OP_SYS] = &writeOpSyscall,
	[OP_GOTO] = &writeOpGoto,
	[OP_CMP] = &writeOpCmp,
	[OP_CMPR] = &writeOpCmpr,
	[OP_AND] = &write2RegImmOp,
	[OP_ANDR] = &write3RegOp,
	[OP_OR] = &write2RegImmOp,
	[OP_ORR] = &write3RegOp,
	[OP_NOT] = &write2RegOp,
	[OP_XOR] = &write2RegImmOp,
	[OP_XORR] = &write3RegOp,
	[OP_SHL] = &write2RegImmOp,
	[OP_SHLR] = &write3RegOp,
	[OP_SHR] = &write2RegImmOp,
	[OP_SHRR] = &write3RegOp,
	[OP_SHRS] = &write2RegImmOp,
	[OP_SHRSR] = &write3RegOp,
	[OP_PROC] = &writeMarkOp,
	[OP_CALL] = &writeOpCall,
	[OP_RET] = &writeNoArgOp,
	[OP_ENDPROC] = &writeNoArgOp,
	[OP_LD64] = &write2RegOp,
	[OP_STR64] = &write2RegOp,
	[OP_LD32] = &write2RegOp,
	[OP_STR32] = &write2RegOp,
	[OP_LD16] = &write2RegOp,
	[OP_STR16] = &write2RegOp,
	[OP_LD8] = &write2RegOp,
	[OP_STR8] = &write2RegOp,
	[OP_VAR] = &writeOpVar,
	[OP_SETV] = &writeRegSymbolIdOp,
	[OP_MUL] = &write2RegImmOp,
	[OP_MULR] = &write3RegOp,
	[OP_DIV] = &write2RegImmOp,
	[OP_DIVR] = &write3RegOp,
	[OP_DIVS] = &write2RegImmOp,
	[OP_DIVSR] = &write3RegOp,
	[OP_EXTPROC] = &writeMarkOp,
	[OP_LDV] = &writeOpLdv,
	[OP_STRV] = &writeOpStrv,
	[OP_POPV] = &writeOpPopv,
	[OP_PUSHV] = &writeOpPushv,
	[OP_ATF] = &writeMarkOp,
	[OP_ATL] = &writeSymbolIdOp,
	[OP_SETC] = &writeOpSetc,
	[OP_DELNV] = &writeSymbolIdOp,
	[OP_LD64S] = &write2RegOp,
	[OP_LD32S] = &write2RegOp,
	[OP_LD16S] = &write2RegOp,
	[OP_LD8S] = &write2RegOp,
	[OP_LDVS] = &writeOpLdv,
	[OP_SX32] = &write2RegOp,
	[OP_SX16] = &write2RegOp,
	[OP_SX8] = &write2RegOp,
	[OP_MOD] = &write2RegImmOp,
	[OP_MODS] = &write2RegImmOp,
	[OP_MODR] = &write3RegOp,
	[OP_MODSR] = &write3RegOp
};
static_assert(N_OPS == sizeof(op_writers) / sizeof(op_writers[0]), "Some BRB operations have unmatched writers");

void writeOp(ModuleWriter* writer, Op op)
{
	if (op.cond_id) {
		fputc(~op.type, writer->dst);
		fputc(op.cond_id, writer->dst);
	} else {
		fputc(op.type, writer->dst);
	}
	op_writers[op.type](writer, op);
}

void writeModule(Module* src, FILE* dst)
{
	ModuleWriter writer = {
		.consts = strArray_new(0),
		.dst = dst,
		.src = src
	};
// writing stack size
	src->stack_size /= 1024;
	writeInt(dst, src->stack_size == DEFAULT_STACK_SIZE ? 0 : src->stack_size); // dumping stack size
	src->stack_size *= 1024;
// writing dependencies
	writeInt(dst, src->submodules.length);
	array_foreach(str, name, src->submodules, 
		fputs(name, dst);
		fputc('\n', dst);
	);
//  dumping data blocks
	writeInt(dst, src->datablocks.length - src->_root_db_start); 
	for (int i = src->_root_db_start; i < src->datablocks.length; i++) {
		writeDataBlock(&writer, src->datablocks.data[i].name, src->datablocks.data[i].spec);
	}
//  dumping memory blocks
	writeInt(dst, src->memblocks.length - src->_root_mb_start);
	for (int i = src->_root_mb_start; i < src->memblocks.length; i++) {
		writeMemoryBlock(&writer, src->memblocks.data[i].name, src->memblocks.data[i].size);
	}
//  dumping operations
	writeInt(dst, src->execblock.length - src->_root_eb_start);
	for (int i = src->_root_eb_start; i < src->execblock.length; i++) {
		writeOp(&writer, src->execblock.data[i]);
	}
//  dumping constants pool
	writeInt(dst, writer.consts.length);
	array_foreach(str, constant, writer.consts, 
		fputs(constant, dst);
		fputc('\n', dst);
	);

	strArray_clear(&writer.consts);
}

uint8_t loadInt8(FILE* fd, long* n_fetched)
{
	if (feof(fd) || ferror(fd)) { *n_fetched = 0; return 0; }
	*n_fetched = 1;
	return fgetc(fd);
}

uint16_t loadInt16(FILE* fd, long* n_fetched)
{
	char res[2];
	if (fread(&res, 1, 2, fd) != 2) { *n_fetched = 0; return 0; };
	*n_fetched = 2;
	return *(uint16_t*)BRByteOrder(&res, 2); 
}

uint32_t loadInt32(FILE* fd, long* n_fetched)
{
	char res[4];
	if (fread(&res, 1, 4, fd) != 4) { *n_fetched = 0; return 0; };
	*n_fetched = 4;
	return *(uint32_t*)BRByteOrder(&res, 4);
}

uint64_t loadInt64(FILE* fd, long* n_fetched)
{
	char res[8];
	if (fread(&res, 1, 8, fd) != 8) { *n_fetched = 0; return 0; }
	*n_fetched = 8;
	return *(uint64_t*)BRByteOrder(&res, 8);
}

uint64_t loadInt(FILE* fd, long* n_fetched)
{
	if (feof(fd) || ferror(fd)) { *n_fetched = 0; return 0; }
	int8_t size = fgetc(fd);
	int64_t res;

	switch (size) {
		case 0: res = 0; break;
		case 1: res = loadInt8(fd, n_fetched); break;
		case 2: res = loadInt16(fd, n_fetched); break;
		case 4: res = loadInt32(fd, n_fetched); break;
		case 8: res = loadInt64(fd, n_fetched); break;
	}
	if (n_fetched) { *n_fetched = *n_fetched + 1; }
	return res;
}

bool loadName(ModuleLoader* loader, char** dst)
{
	long n_fetched = 0;
	int64_t index = loadInt(loader->src, &n_fetched);
	if (!n_fetched) return false;

	*dst = (char*)index;
	fieldArray_append(&loader->unresolved, dst);
	return true;
}

typedef BRBLoadError (*OpLoader) (ModuleLoader*, Op*);
 
void printLoadError(BRBLoadError err)
{
	static_assert(N_BRB_ERRORS == 20, "not all BRB errors are handled\n");
	switch (err.code) {
		case BRB_ERR_OK: break;
		case BRB_ERR_NO_MEMORY:
			eprintf("BRB loading error: memory allocation failure\n");
			break;
		case BRB_ERR_NO_BLOCK_NAME:
			eprintf("BRB loading error: block name not found\n");
			break;
		case BRB_ERR_NO_BLOCK_SIZE:
			eprintf("BRB loading error: block size not found\n");
			break;
		case BRB_ERR_NO_BLOCK_SPEC:
			eprintf("BRB loading error: data block specifier not found\n");
			break;
		case BRB_ERR_NO_OPCODE:
			eprintf("BRB loading error: operation code not found\n");
			break;
		case BRB_ERR_NO_MARK_NAME:
			eprintf("BRB loading error: mark name not found\n");
			break;
		case BRB_ERR_NO_OP_ARG:
			eprintf(
				"BRB loading error: argument for operation `"sbuf_format"` not found\n", 
				unpack(opNames[err.opcode])
			);
			break;
		case BRB_ERR_INVALID_OPCODE:
			eprintf("BRB loading error: invalid operation code %d found\n", err.opcode);
			break;
		case BRB_ERR_INVALID_COND_ID:
			eprintf("BRB loading error: invalid condition code %hhu found\n", err.cond_id);
			break;
		case BRB_ERR_NO_STACK_SIZE:
			eprintf("BRB loading error: found stack size segment identifier, but no stack size was provided\n");
			break;
		case BRB_ERR_NO_ENTRY:
			eprintf("BRB loading error: no `main` procedure found in the module\n");
			break;
		case BRB_ERR_NO_DATA_SEGMENT:
			eprintf("BRB loading error: no data segment found\n");
			break;
		case BRB_ERR_NO_MEMORY_SEGMENT:
			eprintf("BRB loading error: no memory segment found\n");
			break;
		case BRB_ERR_NO_EXEC_SEGMENT:
			eprintf("BRB loading error: no exec segment found\n");
			break;
		case BRB_ERR_NO_NAME_SEGMENT:
			eprintf("BRB loading error: no name segment found\n");
			break;
		case BRB_ERR_NO_NAME_SPEC:
			eprintf("BRB loading error: no name specifier found\n");
			break;
		case BRB_ERR_NO_COND_ID:
			eprintf("BRB loading error: no condition code found\n");
			break;
		case BRB_ERR_NO_LOAD_SEGMENT:
			eprintf("dependencies segment not found\n");
			break;
		case BRB_ERR_MODULE_NOT_FOUND:
			eprintf("module `%s` not found in the specified search paths\n", err.module_name);
			break;
		case N_BRB_ERRORS:
			eprintf("unreachable\n");
	}
}

BRBLoadError loadNoArgOp(ModuleLoader* loader, Op* dst)
{
	return (BRBLoadError){0};
}

BRBLoadError loadMarkOp(ModuleLoader* loader, Op* dst)
{
	if (!loadName(loader, &dst->mark_name)) {
		return (BRBLoadError){.code = BRB_ERR_NO_MARK_NAME};
	}
	return (BRBLoadError){0};
}

BRBLoadError loadRegImmOp(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	dst->value = loadInt(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError load2RegOp(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	dst->src_reg = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadRegSymbolIdOp(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	dst->symbol_id = loadInt(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadRegNameOp(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };

	if (!loadName(loader, &dst->mark_name)) return (BRBLoadError){.code = BRB_ERR_NO_OP_ARG};
	return (BRBLoadError){0};
}

BRBLoadError loadOpSyscall(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->syscall_id = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpGoto(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->op_offset = loadInt(loader->src, &status);

	if (!status) return (BRBLoadError){.code = BRB_ERR_NO_OP_ARG};
	return (BRBLoadError){0};
}

BRBLoadError loadOpCmp(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->src_reg = loadInt8(loader->src, &status);
	dst->value = loadInt(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpCmpr(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->src_reg = loadInt8(loader->src, &status);
	dst->src2_reg = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError load2RegImmOp(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	dst->src_reg = loadInt8(loader->src, &status);
	dst->value = loadInt(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError load3RegOp(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	dst->src_reg = loadInt8(loader->src, &status);
	dst->src2_reg = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpVar(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->new_var_size = loadInt(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpLdv(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	dst->symbol_id = loadInt(loader->src, &status);
	dst->var_size = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpStrv(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->symbol_id = loadInt(loader->src, &status);
	dst->var_size = loadInt8(loader->src, &status);
	dst->src_reg = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpPopv(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	dst->var_size = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpPushv(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->var_size = loadInt8(loader->src, &status);
	dst->src_reg = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadSymbolIdOp(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->symbol_id = loadInt(loader->src, &status);
	
	if (!status) return (BRBLoadError){ .code  = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

BRBLoadError loadOpSetc(ModuleLoader* loader, Op* dst)
{
	long status = 0;
	dst->dst_reg = loadInt8(loader->src, &status);
	dst->cond_arg = loadInt8(loader->src, &status);

	if (!status) return (BRBLoadError){ .code = BRB_ERR_NO_OP_ARG };
	return (BRBLoadError){0};
}

OpLoader op_loaders[] = {
	[OP_NONE] = &loadNoArgOp,
	[OP_END] = &loadNoArgOp,
	[OP_MARK] = &loadNoArgOp,
	[OP_SET] = &loadRegImmOp,
	[OP_SETR] = &load2RegOp,
	[OP_SETD] = &loadRegNameOp,
	[OP_SETB] = &loadRegSymbolIdOp,
	[OP_SETM] = &loadRegNameOp,
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
	[OP_SHL] = &load2RegImmOp,
	[OP_SHLR] = &load3RegOp,
	[OP_SHR] = &load2RegImmOp,
	[OP_SHRR] = &load3RegOp,
	[OP_SHRS] = &load2RegImmOp,
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

FILE* findModule(char* name, char* search_paths[])
{
	for (int i = 0; search_paths[i]; i++) {
		char path[256];
		snprintf(path, sizeof(path), "%s/%s.brb", search_paths[i], name);

		FILE* module_fd = fopen(path, "r");
		if (module_fd) return module_fd;
		errno = 0;
	}
	return NULL;
}

Module* mergeModule(Module* src, Module* dst)
{
	DataBlockArray_extend(&dst->datablocks, src->datablocks);
	MemBlockArray_extend(&dst->memblocks, src->memblocks);
	OpArray_extend(&dst->execblock, src->execblock);
	strArray_extend(&dst->submodules, src->submodules);
	dst->_root_db_start = dst->datablocks.length;
	dst->_root_mb_start = dst->memblocks.length;
	dst->_root_eb_start = dst->execblock.length;

	return dst;
}

BRBLoadError preloadModule(FILE* src, Module* dst, char* module_paths[])
{
	ModuleLoader loader = {
		.dst = dst,
		.src = src,
		.unresolved = fieldArray_new(0)
	};
	dst->datablocks = DataBlockArray_new(0);
	dst->memblocks = MemBlockArray_new(0);
	dst->execblock = OpArray_new(0);
	dst->submodules = strArray_new(0);
	dst->_root_db_start = 0;
	dst->_root_eb_start = 0;
	dst->_root_mb_start = 0;

	long status = 0;
// loading stack size
	dst->stack_size = loadInt(src, &status);
	if (!dst->stack_size) {
		if (!status) return (BRBLoadError){
			.code = BRB_ERR_NO_STACK_SIZE
		};
		dst->stack_size = DEFAULT_STACK_SIZE;
	}
	dst->stack_size *= 1024;
// loading dependencies
	int64_t n = loadInt(src, &status);
	if (!status) return (BRBLoadError){
		.code = BRB_ERR_NO_LOAD_SEGMENT
	};
	for (int64_t i = 0; i < n; i++) {
		char* name;
		size_t name_length;

		name = fgetln(src, &name_length);
		if (!name_length || !name) return (BRBLoadError){
			.code = BRB_ERR_NO_NAME_SPEC
		};
		name = tostr((sbuf){ .data = name, .length = name_length - 1 });

		FILE* module_fd = findModule(name, module_paths);
		if (!module_fd) return (BRBLoadError){
			.code = BRB_ERR_MODULE_NOT_FOUND,
			.module_name = name
		};
		Module submodule;
		BRBLoadError err = preloadModule(module_fd, &submodule, module_paths);
		if (err.code) return err;

		dst = mergeModule(&submodule, dst);
		strArray_append(&dst->submodules, name);
	}
// loading data blocks
	n = loadInt(src, &status);
	if (!status) return (BRBLoadError){
		.code = BRB_ERR_NO_DATA_SEGMENT
	};
	DataBlockArray_resize(&dst->datablocks, n);
	for (int64_t i = dst->datablocks.length - n; i < dst->datablocks.length; i++) {
		if (!loadName(&loader, &dst->datablocks.data[i].name)) return (BRBLoadError){
			.code = BRB_ERR_NO_BLOCK_NAME
		};
		dst->datablocks.data[i].spec.length = loadInt(src, &status);
		if (!status) return (BRBLoadError){
			.code = BRB_ERR_NO_BLOCK_SIZE
		};
		dst->datablocks.data[i].spec.data = malloc(dst->datablocks.data[i].spec.length);
		if (!fread(
			dst->datablocks.data[i].spec.data, 
			dst->datablocks.data[i].spec.length,
			1,
			src
		)) return (BRBLoadError){
			.code = BRB_ERR_NO_BLOCK_SPEC
		};
	}
//  loading memory blocks
	n = loadInt(src, &status);
	if (!status) return (BRBLoadError){
		.code = BRB_ERR_NO_MEMORY_SEGMENT
	};
	MemBlockArray_resize(&dst->memblocks, n);
	for (int64_t i = dst->memblocks.length - n; i < dst->memblocks.length; i++) {
		if (!loadName(&loader, &dst->memblocks.data[i].name)) return (BRBLoadError){
			.code = BRB_ERR_NO_BLOCK_NAME
		};
		
		dst->memblocks.data[i].size = loadInt(src, &status);
		if (!status) return (BRBLoadError){
			.code = BRB_ERR_NO_BLOCK_SIZE
		};
	}
//  loading operations
	n = loadInt(src, &status);
	if (!status) return (BRBLoadError){
		.code = BRB_ERR_NO_EXEC_SEGMENT
	};
	OpArray_resize(&dst->execblock, n);
	for (int64_t i = dst->execblock.length - n; i < dst->execblock.length; i++) {
		Op* op = dst->execblock.data + i;

		op->type = loadInt8(src, &status);
		if (!status) return (BRBLoadError){
			.code = BRB_ERR_NO_OPCODE
		};
		if (op->type < 0) {
			op->type = ~op->type;
			op->cond_id = loadInt8(src, &status);
			if (!status) return (BRBLoadError){
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
	if (!status) return (BRBLoadError){
		.code = BRB_ERR_NO_NAME_SEGMENT
	};
	for (int64_t i = 0; i < n; i++) {
		char* name;
		size_t name_length;

		name = fgetln(src, (size_t*)&name_length);
		if (!name_length || !name) return (BRBLoadError){
			.code = BRB_ERR_NO_NAME_SPEC
		};
		name = tostr((sbuf){ .data = name, .length = name_length - 1 });

		for (int64_t i1 = 0; i1 < loader.unresolved.length; i1++) {
			if (*loader.unresolved.data[i1] == (char*)i) {
				*loader.unresolved.data[i1] = name;
			}
		}
	}
	fieldArray_clear(&loader.unresolved);
	return (BRBLoadError){0};
}

void resolveModule(Module* dst, bool for_exec)
{
	for (int i = 0; i < dst->execblock.length; i++) {
		Op* op = dst->execblock.data + i;
		
		switch (op->type) {
			case OP_SETD:
				for (int64_t db_i = 0; db_i < dst->datablocks.length; db_i++) {
					if (streq(op->mark_name, dst->datablocks.data[db_i].name)) {
						op->symbol_id = db_i;
						break;
					}
				}
				break;
			case OP_SETM:
				for (int64_t mb_i = 0; mb_i < dst->memblocks.length; mb_i++) {
					if (streq(op->mark_name, dst->memblocks.data[mb_i].name)) {
						op->symbol_id = mb_i;
						break;
					}
				}
				break;
			case OP_CALL:
				for (int64_t proc_index = 0; proc_index < dst->execblock.length; proc_index++) {
					Op* proc = dst->execblock.data + proc_index;

					if (proc->type == OP_PROC || proc->type == OP_EXTPROC) {
						if (streq(proc->mark_name, op->mark_name)) {
							op->symbol_id = proc_index;
							break;
						}
					}
				}
				break;
			default:
				break;
		}
	}
//  resolving entry
	if (for_exec) {
		dst->entry_opid = -1;
		for (int i = dst->_root_eb_start; i < dst->execblock.length; i++) {
			Op* op = dst->execblock.data + i;
			if (op->type == OP_PROC || op->type == OP_EXTPROC) {
				if (sbufeq(fromstr(op->mark_name), fromcstr("main"))) {
					dst->entry_opid = i;
					break;
				}
			}
		}
	}
}

BRBLoadError loadModule(FILE* src, Module* dst, char* search_paths[], int flags)
{
	BRBLoadError err = preloadModule(src, dst, search_paths);
	if (err.code) return err;
	resolveModule(dst, flags & BRB_EXECUTABLE);
	return (BRBLoadError){0};
}
/*
typedef void (*OpPrinter) (Op);

void printNoArgOp(Op op, FILE* dst)
{
	fputc('\n', dst);
}

void printRegImmOp(Op op, FILE* dst)
{
	fprintf(dst, "r%hhd %lld\n", op.dst_reg, op.value);
}

void print2RegOp(Op op, FILE* dst)
{
	fprintf(dst, "r%hhd r%hhd\n", op.dst_reg, op.src_reg);
}

static OpPrinter op_printers[] = {
	[OP_NONE] = &printNoArgOp,
	[OP_END] = &printNoArgOp,
	[OP_MARK] = &printNoArgOp,
	[OP_SET] = &printRegImmOp,
	[OP_SETR] = &print2RegOp,
	OP_SETD, // uses Op::dst_reg and Op::symbol_id
	OP_SETB, // uses Op::dst_reg and Op::symbol_id
	OP_SETM, // uses Op::dst_reg and Op::symbol_id
	OP_ADD, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_ADDR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SUB, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_SUBR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SYS, // uses Op::syscall_id
	OP_GOTO, // uses Op::op_offset
	OP_CMP, // uses Op::src_reg and Op::value
	OP_CMPR, // uses Op::src_reg and Op::src2_reg
	OP_AND, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_ANDR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_OR, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_ORR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_NOT, // uses Op::dst_reg and Op::src_reg
	OP_XOR, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_XORR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SHL, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_SHLR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SHR, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_SHRR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_SHRS, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_SHRSR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_PROC, // uses Op::mark_name
	OP_CALL, // uses Op::symbol_id
	OP_RET,
	OP_ENDPROC,
	OP_LD64, // uses Op::dst_reg and Op::src_reg
	OP_STR64, // uses Op::dst_reg and Op::src_reg
	OP_LD32, // uses Op::dst_reg and Op::src_reg
	OP_STR32, // uses Op::dst_reg and Op::src_reg
	OP_LD16, // uses Op::dst_reg and Op::src_reg
	OP_STR16, // uses Op::dst_reg and Op::src_reg
	OP_LD8, // uses Op::dst_reg and Op::src_reg
	OP_STR8, // uses Op::dst_reg and Op::src_reg
	OP_VAR, // uses Op::var_size
	OP_SETV, // uses Op::dst_reg and Op::symbol_id
	OP_MUL, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_MULR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg
	OP_DIV, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_DIVR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg 
	OP_DIVS, // uses Op::dst_reg, Op::src_reg and Op::value
	OP_DIVSR, // uses Op::dst_reg, Op::src_reg and Op::src2_reg 
	OP_EXTPROC, // uses Op::mark_name
	OP_LDV, // uses Op::src_reg, Op::symbol_id and Op::var_size
	OP_STRV, // uses Op::dst_reg, Op::symbol_id and Op::var_size
	OP_POPV, // uses Op::dst_reg, Op::var_size
	OP_PUSHV, // uses Op::src_reg and Op::var_size
	OP_ATF, // uses Op::mark_name
	OP_ATL, // uses Op::symbol_id
	OP_SETC, // uses Op::cond_arg and Op::dst_reg
	OP_DELNV,
}

void printModule(Module* module, FILE* dst)
{
	if (module->datablocks.length) {
		fputs("data {\n", dst);
		array_foreach(DataBlock, block, module->datablocks, 
			fprintf(dst, "\t%s \"", block.name);
			fputsbufesc(dst, block.spec, BYTEFMT_HEX);
			fputs("\"\n", dst);
		);
		fputs("}\n", dst);
	}

	if (module->memblocks.length) {
		fputs("memory {\n", dst);
		array_foreach(MemBlock, block, module->memblocks, 
			fprintf(dst, "\t%s %lld\n", block.name, block.size);
		);
		fputs("}\n", dst);
	}

	if (module->execblock.length) {
		fputs("exec {\n", dst);
		array_foreach(Op, op, module->execblock, 
			fprintf(dst, "\t%s", opNames[op.type]);
			if (op.cond_id) fprintf(dst, ":%s ", conditionNames[op.cond_id].data);
			else fputc(' ', dst);
		);
	}
}
*/
void initExecEnv(ExecEnv* env, Module* module, char** args)
{
	env->exec_callbacks = NULL;
	env->stack_brk = malloc(module->stack_size),
	env->exitcode = 0,
	env->memblocks = sbufArray_new(module->memblocks.length * -1),
	env->op_id = module->entry_opid,
	env->registers = malloc(sizeof(int64_t) * N_REGS),
	env->prev_stack_head = env->stack_head = env->stack_brk + module->stack_size;
	memset(env->registers, 0, sizeof(int64_t) * N_REGS);

	sbuf* newblock;
	array_foreach(MemBlock, block, module->memblocks,
		newblock = sbufArray_append(&env->memblocks, smalloc(block.size));
		memset(newblock->data, 0, newblock->length);
	);

	env->exec_argc = 0;
	while (args[++env->exec_argc]) {}
	env->exec_argv = malloc(env->exec_argc * sizeof(sbuf));
	for (int i = 0; i < env->exec_argc; i++) {
		env->exec_argv[i] = fromstr(args[i]);
		env->exec_argv[i].length++;
	}
}

bool addPreCallBack(ExecEnv* env, uint8_t op_id, ExecCallback callback)
{
	if (!env->exec_callbacks) {
		env->exec_callbacks = calloc(2 * N_OPS, sizeof(env->exec_callbacks));
		if (!env->exec_callbacks) return false;
	}
	env->exec_callbacks[op_id] = callback;
	return true;
}

bool addPostCallBack(ExecEnv* env, uint8_t op_id, ExecCallback callback)
{
	if (!env->exec_callbacks) {
		env->exec_callbacks = calloc(2 * N_OPS, sizeof(env->exec_callbacks));
		if (!env->exec_callbacks) return false;
	}
	env->exec_callbacks[op_id + N_OPS] = callback;
	return true;
}

void setCurrentSrc(ExecEnv* env, Module* module)
{
	env->src_path = NULL;
	env->src_line = 0;
	for (int i = env->op_id; i >= 0; i--) {
		Op op = module->execblock.data[i];
		if (op.type == OP_ATF) {
			env->src_path = op.mark_name;
			return;
		} else if (op.type == OP_ATL && !env->src_line) env->src_line = op.symbol_id;
	}
}

bool handleInvalidSyscall(ExecEnv* env, Module* module)
{
	env->exitcode = EC_UNKNOWN_SYSCALL;
	return true;
}

bool handleExitSyscall(ExecEnv* env, Module* module)
{
	env->exitcode = env->registers[0];
	return true;
}

bool handleWriteSyscall(ExecEnv* env, Module* module)
{
	env->registers[0] = write(env->registers[0], (char*)env->registers[1], env->registers[2]);
	env->op_id++;
	return false;
}

bool handleArgcSyscall(ExecEnv* env, Module* module)
{
	env->registers[0] = env->exec_argc;
	env->op_id++;
	return false;
}

bool handleArgvSyscall(ExecEnv* env, Module* module)
{
	env->registers[0] = inRange(env->registers[0], 0, env->exec_argc) ? (uint64_t)env->exec_argv[env->registers[0]].data : 0;
	env->op_id++;
	return false;
}

bool handleReadSyscall(ExecEnv* env, Module* module)
{
    env->registers[0] = read(env->registers[0], (void*)env->registers[1], env->registers[2]);
    env->op_id++;
    return false;
}

bool handleGetErrnoSyscall(ExecEnv* env, Module* module)
{
    env->registers[0] = errno;
    env->op_id++;
    return false;
}

bool handleSetErrnoSyscall(ExecEnv* env, Module* module)
{
    errno = env->registers[0];
    env->op_id++;
    return false;
}

typedef bool (*ExecHandler) (ExecEnv*, Module*);

ExecHandler syscall_handlers[] = {
	[SYS_OP_INVALID] = &handleInvalidSyscall,
	[SYS_OP_EXIT] = &handleExitSyscall,
	[SYS_OP_WRITE] = &handleWriteSyscall,
	[SYS_OP_ARGC] = &handleArgcSyscall,
	[SYS_OP_ARGV] = &handleArgvSyscall,
    [SYS_OP_READ] = &handleReadSyscall,
    [SYS_OP_GET_ERRNO] = &handleGetErrnoSyscall,
    [SYS_OP_SET_ERRNO] = &handleSetErrnoSyscall
};
static_assert(N_SYS_OPS == sizeof(syscall_handlers) / sizeof(syscall_handlers[0]), "not all system calls have matching handlers");

bool handleCondition(ExecEnv* env, ConditionCode cond_id) {
	switch (cond_id) {
		case COND_NON: return true;
		case COND_EQU: return env->registers[CONDREG1_ID] == env->registers[CONDREG2_ID];
		case COND_NEQ: return env->registers[CONDREG1_ID] != env->registers[CONDREG2_ID];
		case COND_LTU: return env->registers[CONDREG1_ID] <  env->registers[CONDREG2_ID];
		case COND_GTU: return env->registers[CONDREG1_ID] >  env->registers[CONDREG2_ID];
		case COND_LEU: return env->registers[CONDREG1_ID] <= env->registers[CONDREG2_ID];
		case COND_GEU: return env->registers[CONDREG1_ID] >= env->registers[CONDREG2_ID];
		case COND_LTS: return (int64_t)env->registers[CONDREG1_ID] <  (int64_t)env->registers[CONDREG2_ID];
		case COND_GTS: return (int64_t)env->registers[CONDREG1_ID] >  (int64_t)env->registers[CONDREG2_ID];
		case COND_LES: return (int64_t)env->registers[CONDREG1_ID] <= (int64_t)env->registers[CONDREG2_ID];
		case COND_GES: return (int64_t)env->registers[CONDREG1_ID] >= (int64_t)env->registers[CONDREG2_ID];
		case N_CONDS: return false;
	}
}

bool handleNop(ExecEnv* env, Module* module)
{
	env->op_id++;
	return false;
}

bool handleOpEnd(ExecEnv* env, Module* module)
{
	env->exitcode = EC_OK;
	return true;
}

bool handleOpSet(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = op.value;
	env->op_id++;
	return false;
}

bool handleOpSetr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpSetd(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)module->datablocks.data[op.symbol_id].spec.data;
	env->op_id++;
	return false;
}

bool handleOpSetb(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = builtins[op.symbol_id].value;
	env->op_id++;
	return false;
}

bool handleOpSetm(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->memblocks.data[op.symbol_id].data;
	env->op_id++;
	return false;
}

bool handleOpAdd(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + op.value;
	env->op_id++;
	return false;
}

bool handleOpAddr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] + env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpSub(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] - op.value;
	env->op_id++;
	return false;
}

bool handleOpSubr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] - env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpSyscall(ExecEnv* env, Module* module)
{
	return syscall_handlers[module->execblock.data[env->op_id].syscall_id](env, module);
}

bool handleOpGoto(ExecEnv* env, Module* module)
{
	env->op_id += module->execblock.data[env->op_id].op_offset;
	return false;
}

bool handleOpCmp(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[CONDREG1_ID] = env->registers[op.src_reg];
	env->registers[CONDREG2_ID] = op.value;
	env->op_id++;
	return false;
}

bool handleOpCmpr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[CONDREG1_ID] = env->registers[op.src_reg];
	env->registers[CONDREG2_ID] = env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpAnd(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] & op.value;
	env->op_id++;
	return false;
}

bool handleOpAndr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] & env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpOr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] | op.value;
	env->op_id++;
	return false;
}

bool handleOpOrr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] | env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpNot(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = ~env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpXor(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ op.value;
	env->op_id++;
	return false;
}

bool handleOpXorr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] ^ env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpShl(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] << op.value;
	env->op_id++;
	return false;
}

bool handleOpShlr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] << env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpShr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >> op.value;
	env->op_id++;
	return false;
}

bool handleOpShrr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] >> env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpShrs(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >> op.value;
	env->op_id++;
	return false;
}

bool handleOpShrsr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] >> env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpCall(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];

	env->stack_head -= 8;
	*(int64_t*)env->stack_head = env->op_id + 1;

	env->stack_head -= 8;
	*(void**)env->stack_head = env->prev_stack_head;
	env->prev_stack_head = env->stack_head;

	env->op_id = op.symbol_id;

	return false;
}

bool handleOpRet(ExecEnv* env, Module* module)
{
	env->stack_head = env->prev_stack_head;
	env->prev_stack_head = *(void**)env->stack_head;
	env->stack_head += sizeof(env->prev_stack_head);

	env->op_id = *(int64_t*)env->stack_head;
	env->stack_head += 8;

	return false;
}

bool handleOpLd64(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = *(int64_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr64(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int64_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd32(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = *(int32_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr32(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int32_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd16(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = *(int16_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr16(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int16_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd8(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = *(int8_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpStr8(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int8_t*)env->registers[op.dst_reg] = env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpVar(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->stack_head -= op.new_var_size;
	env->op_id++;
	return false;
}

bool handleOpSetv(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->stack_head + op.symbol_id;
	env->op_id++;
	return false;
}

bool handleOpMul(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] * (uint64_t)op.value;
	env->op_id++;
	return false;
}

bool handleOpMulr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] * env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpDiv(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] / (uint64_t)op.value;
	env->op_id++;
	return false;
}

bool handleOpDivr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] / env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpDivs(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] / op.value;
	env->op_id++;
	return false;
}

bool handleOpDivsr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] / (int64_t)env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpLdv(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = 0;
	memcpy(env->registers + op.dst_reg, env->stack_head + op.symbol_id, op.var_size);
	env->op_id++;
	return false;
}

bool handleOpStrv(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	memcpy(env->stack_head + op.symbol_id, env->registers + op.src_reg, op.var_size);
	env->op_id++;
	return false;
}

bool handleOpPopv(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = 0;
	memcpy(env->registers + op.dst_reg, env->stack_head, op.var_size);
	env->stack_head += op.var_size;
	env->op_id++;
	return false;
}

bool handleOpPushv(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	memcpy((env->stack_head -= op.var_size), env->registers + op.src_reg, op.var_size);
	env->op_id++;
	return false;
}

bool handleOpAtf(ExecEnv* env, Module* module)
{
	env->src_path = module->execblock.data[env->op_id].mark_name;
	env->src_line = 0;
	env->op_id++;
	return false;
}

bool handleOpAtl(ExecEnv* env, Module* module)
{
	env->src_line = module->execblock.data[env->op_id].symbol_id;
	env->op_id++;
	return false;
}

bool handleOpSetc(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = handleCondition(env, op.cond_arg);
	env->op_id++;
	return false;
}

bool handleOpDelnv(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->stack_head += op.symbol_id;
	env->op_id++;
	return false;
}

bool handleOpLd64S(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int64_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd32S(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int32_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd16S(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int16_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLd8S(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int8_t*)env->registers[op.src_reg];
	env->op_id++;
	return false;
}

bool handleOpLdvs(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = 0;
	memcpy(env->registers + op.dst_reg, env->stack_head + op.symbol_id, op.var_size);
	if (op.var_size < 8 ? env->registers[op.dst_reg] & (1LL << (op.var_size * 8 - 1)) : false) {
		env->registers[op.dst_reg] |= ~byteMask(op.var_size);
	}
	env->op_id++;
	return false;
}

bool handleOpSx32(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int32_t*)(env->registers + op.src_reg);
	env->op_id++;
	return false;
}

bool handleOpSx16(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int16_t*)(env->registers + op.src_reg);
	env->op_id++;
	return false;
}

bool handleOpSx8(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	*(int64_t*)(env->registers + op.dst_reg) = *(int8_t*)(env->registers + op.src_reg);
	env->op_id++;
	return false;
}

bool handleOpMod(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] % op.value;
	env->op_id++;
	return false;
}

bool handleOpMods(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] % (int64_t)op.value;
	env->op_id++;
	return false;
}

bool handleOpModr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = env->registers[op.src_reg] % env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

bool handleOpModsr(ExecEnv* env, Module* module)
{
	Op op = module->execblock.data[env->op_id];
	env->registers[op.dst_reg] = (int64_t)env->registers[op.src_reg] % (int64_t)env->registers[op.src2_reg];
	env->op_id++;
	return false;
}

ExecHandler op_handlers[] = {
	[OP_NONE] = &handleNop,
	[OP_END] = &handleOpEnd,
	[OP_MARK] = &handleNop,
	[OP_SET] = &handleOpSet,
	[OP_SETR] = &handleOpSetr,
	[OP_SETD] = &handleOpSetd,
	[OP_SETB] = &handleOpSetb,
	[OP_SETM] = &handleOpSetm,
	[OP_ADD] = &handleOpAdd,
	[OP_ADDR] = &handleOpAddr,
	[OP_SUB] = &handleOpSub,
	[OP_SUBR] = &handleOpSubr,
	[OP_SYS] = &handleOpSyscall,
	[OP_GOTO] = &handleOpGoto,
	[OP_CMP] = &handleOpCmp,
	[OP_CMPR] = &handleOpCmpr,
	[OP_AND] = &handleOpAnd,
	[OP_ANDR] = &handleOpAndr,
	[OP_OR] = &handleOpOr,
	[OP_ORR] = &handleOpOrr,
	[OP_NOT] = &handleOpNot,
	[OP_XOR] = &handleOpXor,
	[OP_XORR] = &handleOpXorr,
	[OP_SHL] = &handleOpShl,
	[OP_SHLR] = &handleOpShlr,
	[OP_SHR] = &handleOpShr,
	[OP_SHRR] = &handleOpShrr,
	[OP_SHRS] = &handleOpShrs,
	[OP_SHRSR] = &handleOpShrsr,
	[OP_PROC] = &handleNop,
	[OP_CALL] = &handleOpCall,
	[OP_RET] = &handleOpRet,
	[OP_ENDPROC] = &handleNop,
	[OP_LD64] = &handleOpLd64,
	[OP_STR64] = &handleOpStr64,
	[OP_LD32] = &handleOpLd32,
	[OP_STR32] = &handleOpStr32,
	[OP_LD16] = &handleOpLd16,
	[OP_STR16] = &handleOpStr16,
	[OP_LD8] = &handleOpLd8,
	[OP_STR8] = &handleOpStr8,
	[OP_VAR] = &handleOpVar,
	[OP_SETV] = &handleOpSetv,
	[OP_MUL] = &handleOpMul,
	[OP_MULR] = &handleOpMulr,
	[OP_DIV] = &handleOpDiv,
	[OP_DIVR] = &handleOpDivr,
	[OP_DIVS] = &handleOpDivs,
	[OP_DIVSR] = &handleOpDivsr,
	[OP_EXTPROC] = &handleNop,
	[OP_LDV] = &handleOpLdv,
	[OP_STRV] = &handleOpStrv,
	[OP_POPV] = &handleOpPopv,
	[OP_PUSHV] = &handleOpPushv,
	[OP_ATF] = &handleOpAtf,
	[OP_ATL] = &handleOpAtl,
	[OP_SETC] = &handleOpSetc,
	[OP_DELNV] = &handleOpDelnv,
	[OP_LD64S] = &handleOpLd64S,
	[OP_LD32S] = &handleOpLd32S,
	[OP_LD16S] = &handleOpLd16S,
	[OP_LD8S] = &handleOpLd8S,
	[OP_LDVS] = &handleOpLdvs,
	[OP_SX32] = &handleOpSx32,
	[OP_SX16] = &handleOpSx16,
	[OP_SX8] = &handleOpSx8,
	[OP_MOD] = &handleOpMod,
	[OP_MODS] = &handleOpMods,
	[OP_MODR] = &handleOpModr,
	[OP_MODSR] = &handleOpModsr
};
static_assert(N_OPS == sizeof(op_handlers) / sizeof(op_handlers[0]), "Some BRB operations have unmatched execution handlers");

void execModule(ExecEnv* env, Module* module, volatile bool* interruptor)
{
	while (true) {
		if (module->execblock.data[env->op_id].cond_id) {
			if (!handleCondition(env, module->execblock.data[env->op_id].cond_id)) {
				env->op_id++;
				continue;
			}
		}
		Op* op = module->execblock.data + env->op_id;
		if (env->exec_callbacks)
			if (env->exec_callbacks[op->type])
				if (env->exec_callbacks[op->type](env, module)) break;

		if (op_handlers[op->type](env, module)) break;

		if (env->exec_callbacks)
			if (env->exec_callbacks[N_OPS + op->type])
				if (env->exec_callbacks[N_OPS + op->type](env, module)) break;
		if (interruptor ? *interruptor : false) break;
	}
}