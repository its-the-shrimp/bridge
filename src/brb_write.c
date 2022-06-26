// implementation for writing BRB modules into `.brb` files
#include <brb.h>

defArray(Op);
defArray(DataBlock);
defArray(DataPiece);
defArray(str);
defArray(Submodule);

typedef struct {
	Module* src;
	FILE* dst;
	strArray consts;
} ModuleWriter;

void writeInt(FILE* fd, int64_t x, uint8_t hb)
{
	if (x == (int64_t)(x & 0xFF)) {
		if (x < 8) {
			fputc(x | hb << 4, fd);
		} else {
			fputc((x < 0 ? 12 : 8) | hb << 4, fd);
			fputc((char)(x < 0 ? ~x : x), fd);
		}
	} else if (x == (int64_t)(x & 0xFFFF)) {
		fputc((x < 0 ? 13 : 9) | hb << 4, fd);
		int16_t x16 = x < 0 ? ~x : x;
		fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
	} else if (x == (int64_t)(x & 0xFFFFFFFFLL)) {
		fputc((x < 0 ? 14 : 10) | hb << 4, fd);
		int32_t x32 = x < 0 ? ~x : x;
		fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
	} else {
		fputc((x < 0 ? 15 : 11) | hb << 4, fd);
		int64_t x64 = x < 0 ? ~x : x;
		fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
	}
}

void writeInt_swapped(FILE* fd, int64_t x, uint8_t hb)
{
	if (x == (int64_t)(x & 0xFF)) {
		if (x < 8) {
			fputc(hb | x << 4, fd);
		} else {
			fputc((x < 0 ? 12 : 8) << 4 | hb, fd);
			fputc((char)(x < 0 ? ~x : x), fd);
		}
	} else if (x == (int64_t)(x & 0xFFFF)) {
		fputc((x < 0 ? 13 : 9) << 4 | hb, fd);
		int16_t x16 = x < 0 ? ~x : x;
		fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
	} else if (x == (int64_t)(x & 0xFFFFFFFFLL)) {
		fputc((x < 0 ? 14 : 10) << 4 | hb, fd);
		int32_t x32 = x < 0 ? ~x : x;
		fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
	} else {
		fputc((x < 0 ? 15 : 11) << 4 | hb, fd);
		int64_t x64 = x < 0 ? ~x : x;
		fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
	}
}

void write2HalfBytes(FILE* fd, uint8_t hb1, uint8_t hb2)
{
	fputc(((hb1 << 4) | hb2) & 255, fd);
}

void write2Ints(FILE* fd, int64_t x, int64_t y)
{
	if (x < 8) {
		if (y < 8) {
			fputc(x << 4 | y, fd);
		} else {
			writeInt(fd, y, x);
		}
	} else {
		if (y < 8) {
			writeInt_swapped(fd, x, y);
		} else {
			if (x == (int64_t)(x & 0xFF)) {
				if (y == (int64_t)(y & 0xFF)) {
					fputc((x < 0 ? 12 : 8) << 4 | (y < 0 ? 12 : 8), fd);
					fputc((char)(x < 0 ? ~x : x), fd);
					fputc((char)(y < 0 ? ~y : y), fd);
				} else if (y == (int64_t)(y & 0xFFFF)) {
					fputc((x < 0 ? 12 : 8) << 4 | (y < 0 ? 13 : 9), fd);
					int16_t y16 = y < 0 ? ~y : y;
					fputc((char)(x < 0 ? ~x : x), fd);
					fwrite(BRByteOrder(&y16, 2), 2, 1, fd);
				} else if (y == (int64_t)(y & 0xFFFFFFFFLL)) {
					fputc((x < 0 ? 12 : 8) << 4 | (y < 0 ? 14 : 10), fd);
					int32_t y32 = y < 0 ? ~y : y;
					fputc((char)(x < 0 ? ~x : x), fd);
					fwrite(BRByteOrder(&y32, 4), 4, 1, fd);
				} else {
					fputc((x < 0 ? 12 : 8) << 4 | (y < 0 ? 15 : 11), fd);
					int64_t y64 = y < 0 ? ~y : y;
					fputc((char)(x < 0 ? ~x : x), fd);
					fwrite(BRByteOrder(&y64, 8), 8, 1, fd);
				}
			} else if (x == (int64_t)(x & 0xFFFF)) {
				int16_t x16 = x < 0 ? ~x : x;
				if (y == (int64_t)(y & 0xFF)) {
					fputc((x < 0 ? 13 : 9) << 4 | (y < 0 ? 12 : 8), fd);
					fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
					fputc((char)(y < 0 ? ~y : y), fd);
				} else if (y == (int64_t)(y & 0xFFFF)) {
					fputc((x < 0 ? 13 : 9) << 4 | (y < 0 ? 13 : 9), fd);
					int16_t y16 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
					fwrite(BRByteOrder(&y16, 2), 2, 1, fd);
				} else if (y == (int64_t)(y & 0xFFFFFFFFLL)) {
					fputc((x < 0 ? 13 : 9) << 4 | (y < 0 ? 14 : 10), fd);
					int32_t y32 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
					fwrite(BRByteOrder(&y32, 4), 4, 1, fd);
				} else {
					fputc((x < 0 ? 13 : 9) << 4 | (y < 0 ? 15 : 11), fd);
					int64_t y64 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
					fwrite(BRByteOrder(&y64, 8), 8, 1, fd);
				}
			} else if (x == (int64_t)(x & 0xFFFFFFFFLL)) {
				int32_t x32 = x < 0 ? ~x : x;
				if (y == (int64_t)(y & 0xFF)) {
					fputc((x < 0 ? 14 : 10) << 4 | (y < 0 ? 12 : 8), fd);
					fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
					fputc((char)(y < 0 ? ~y : y), fd);
				} else if (y == (int64_t)(y & 0xFFFF)) {
					fputc((x < 0 ? 14 : 10) << 4 | (y < 0 ? 13 : 9), fd);
					int16_t y16 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
					fwrite(BRByteOrder(&y16, 2), 2, 1, fd);
				} else if (y == (int64_t)(y & 0xFFFFFFFFLL)) {
					fputc((x < 0 ? 14 : 10) << 4 | (y < 0 ? 14 : 10), fd);
					int32_t y32 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
					fwrite(BRByteOrder(&y32, 4), 4, 1, fd);
				} else {
					fputc((x < 0 ? 14 : 10) << 4 | (y < 0 ? 15 : 11), fd);
					int64_t y64 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
					fwrite(BRByteOrder(&y64, 8), 8, 1, fd);
				}
			} else {
				int64_t x64 = x < 0 ? ~x : x;
				fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
				if (y == (int64_t)(y & 0xFF)) {
					fputc((x < 0 ? 15 : 11) << 4 | (y < 0 ? 12 : 8), fd);
					fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
					fputc((char)(y < 0 ? ~y : y), fd);
				} else if (y == (int64_t)(y & 0xFFFF)) {
					fputc((x < 0 ? 15 : 11) << 4 | (y < 0 ? 13 : 9), fd);
					int16_t y16 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
					fwrite(BRByteOrder(&y16, 2), 2, 1, fd);
				} else if (y == (int64_t)(y & 0xFFFFFFFFLL)) {
					fputc((x < 0 ? 15 : 11) << 4 | (y < 0 ? 14 : 10), fd);
					int32_t y32 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
					fwrite(BRByteOrder(&y32, 4), 4, 1, fd);
				} else {
					fputc((x < 0 ? 15 : 11) << 4 | (y < 0 ? 15 : 11), fd);
					int64_t y64 = y < 0 ? ~y : y;
					fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
					fwrite(BRByteOrder(&y64, 8), 8, 1, fd);
				}
			}
		}
	}
}

int getNameId(ModuleWriter* writer, char* name)
{
	for (int i = 0; i < writer->consts.length; i++) {
		if (sbufeq(name, writer->consts.data[i])) {
			return i;
		}
	}
	strArray_append(&writer->consts, name);
	return writer->consts.length - 1;
}

void writeName(ModuleWriter* writer, char* name, uint8_t hb)
{
	writeInt(writer->dst, getNameId(writer, name), hb);
}

void writeDataBlock(ModuleWriter* writer, DataBlock block)
{
	static_assert(N_PIECE_TYPES == 8, "not all data piece types are handled in `writeDataBlock`");

	writeName(writer, block.name, block.is_mutable);
	writeInt(writer->dst, block.pieces.length, 0);
	for (int i = 0; i < block.pieces.length; ++i) {
		DataPiece* piece = block.pieces.data + i;
		fputc(piece->type, writer->dst);
		switch (piece->type) {
			case PIECE_BYTES:
				writeInt(writer->dst, piece->data.length, 0);
			case PIECE_TEXT:
				fputsbuf(writer->dst, piece->data);
				break;
			case PIECE_INT16:
			case PIECE_INT32:
			case PIECE_INT64:
				writeInt(writer->dst, piece->integer, 0);
				break;
			case PIECE_DB_ADDR:
				write2Ints(writer->dst, piece->module_id, getNameId(writer, writer->src->seg_data.data[piece->symbol_id].name));
				break;
			case PIECE_ZERO:
				writeInt(writer->dst, piece->n_bytes, 0);
				break;
			case PIECE_NONE:
			case N_PIECE_TYPES:
			default:
				assert(false, "invalid data piece type");
		}
	}
}

void writeMemoryBlock(ModuleWriter* writer, char* name, int32_t size)
{
	write2Ints(writer->dst, getNameId(writer, name), size);
}

typedef void (*OpWriter) (ModuleWriter*, Op);

void writeNoArgOp(ModuleWriter* writer, Op op) {}

void writeMarkOp(ModuleWriter* writer, Op op)
{
	writeName(writer, op.mark_name, 0);
}

void writeRegImmOp(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.value, op.dst_reg);
}

void write2RegOp(ModuleWriter* writer, Op op)
{
	write2HalfBytes(writer->dst, op.dst_reg, op.src_reg);
}

void writeRegSymbolIdOp(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.symbol_id, op.dst_reg);
}

void writeOpSetd(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.module_id, 0);
	writeName(writer, writer->src->seg_data.data[op.symbol_id].name, op.dst_reg);
}

void writeOpSyscall(ModuleWriter* writer, Op op)
{
	fputc(op.syscall_id, writer->dst);
}

void writeOpGoto(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.op_offset, 0);
}

void writeOpCall(ModuleWriter* writer, Op op)
{
	write2Ints(writer->dst, op.module_id, getNameId(writer, writer->src->seg_exec.data[op.symbol_id].mark_name));
}

void writeOpCmp(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.value, op.src_reg);
}

void writeOpCmpr(ModuleWriter* writer, Op op)
{
	write2HalfBytes(writer->dst, op.src_reg, op.src2_reg);
}

void write2RegImmOp(ModuleWriter* writer, Op op)
{
	write2HalfBytes(writer->dst, op.dst_reg, op.src_reg);
	writeInt(writer->dst, op.value, 0);
}

void write3RegOp(ModuleWriter* writer, Op op)
{
	fputc(op.dst_reg, writer->dst);
	write2HalfBytes(writer->dst, op.src_reg, op.src2_reg);
}

void writeOpVar(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.new_var_size, 0);
}

void writeOpLdv(ModuleWriter* writer, Op op)
{
	write2HalfBytes(writer->dst, op.dst_reg, op.var_size);
	writeInt(writer->dst, op.symbol_id, 0);
}

void writeOpStrv(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.symbol_id, 0);
	write2HalfBytes(writer->dst, op.var_size, op.src_reg);
}

void writeOpPopv(ModuleWriter* writer, Op op)
{
	write2HalfBytes(writer->dst, op.dst_reg, op.var_size);
}

void writeOpPushv(ModuleWriter* writer, Op op)
{
	write2HalfBytes(writer->dst, op.var_size, op.src_reg);
}

void writeSymbolIdOp(ModuleWriter* writer, Op op)
{
	writeInt(writer->dst, op.symbol_id, 0);
}

void writeOpSetc(ModuleWriter* writer, Op op)
{
	write2HalfBytes(writer->dst, op.dst_reg, op.cond_arg);
}

void writeBitShiftOp(ModuleWriter* writer, Op op)
{
	write2HalfBytes(writer->dst, op.dst_reg, op.src_reg);
	fputc(op.value & 255, writer->dst);
}

const OpWriter op_writers[] = {
	[OP_NONE] = &writeNoArgOp,
	[OP_END] = &writeNoArgOp,
	[OP_MARK] = &writeNoArgOp,
	[OP_SET] = &writeRegImmOp,
	[OP_SETR] = &write2RegOp,
	[OP_SETD] = &writeOpSetd,
	[OP_SETB] = &writeRegSymbolIdOp,
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
	[OP_SHL] = &writeBitShiftOp,
	[OP_SHLR] = &write3RegOp,
	[OP_SHR] = &writeBitShiftOp,
	[OP_SHRR] = &write3RegOp,
	[OP_SHRS] = &writeBitShiftOp,
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
	writeInt(dst, src->stack_size == DEFAULT_STACK_SIZE ? 0 : src->stack_size, 0); // dumping stack size
	src->stack_size *= 1024;
// writing dependencies
	int n_dependencies = 0;
	for (
		Submodule* submodule = src->submodules.data;
		submodule - src->submodules.data < src->submodules.length;
		++submodule
	) {
		if (submodule->direct && submodule - src->submodules.data < src->submodules.length - 1) n_dependencies += 1;
	}
	writeInt(dst, n_dependencies, 0);
	for (
		Submodule* submodule = src->submodules.data;
		submodule - src->submodules.data < src->submodules.length;
		++submodule
	) {
		if (submodule->direct && submodule - src->submodules.data < src->submodules.length - 1) {
			fputs(submodule->name, dst);
			fputc('\n', dst);
		}
	}

	Submodule* root = arrayhead(src->submodules);
	
//  dumping data blocks
	writeInt(dst, root->ds_length, 0);
	for (int i = root->ds_offset; i < src->seg_data.length; i++) {
		writeDataBlock(&writer, src->seg_data.data[i]);
	}
//  dumping operations
	writeInt(dst, root->es_length, 0);
	for (int i = root->es_offset; i < src->seg_exec.length; i++) {
		writeOp(&writer, src->seg_exec.data[i]);
	}
//  dumping constants pool
	writeInt(dst, writer.consts.length, 0);
	array_foreach(str, constant, writer.consts,
		fputs(constant, dst);
		fputc('\n', dst);
	);

	strArray_clear(&writer.consts);
}
