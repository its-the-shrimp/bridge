// implementation for writing BRB modules into `.brb` files
#include <brb.h>

defArray(BRB_Op);
defArray(BRB_DataPiece);
defArray(BRB_DataBlock);

typedef const char* str;
declArray(str);
defArray(str);
typedef struct {
	BRB_Module* src;
	FILE* dst;
	strArray names;
} BRB_ModuleWriter;

void BRB_writeInt(FILE* fd, uint64_t x, uint8_t hb)
{
	if (FITS_IN_8BITS(x)) {
		if (x < 8) {
			fputc(x | hb << 4, fd);
		} else {
			fputc((SIGN_BIT_SET(x) ? 12 : 8) | hb << 4, fd);
			fputc((int8_t)(SIGN_BIT_SET(x) ? ~x : x), fd);
		}
	} else if (FITS_IN_16BITS(x)) {
		fputc((SIGN_BIT_SET(x) ? 13 : 9) | hb << 4, fd);
		int16_t x16 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
	} else if (FITS_IN_32BITS(x)) {
		fputc((SIGN_BIT_SET(x) ? 14 : 10) | hb << 4, fd);
		int32_t x32 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
	} else {
		fputc((SIGN_BIT_SET(x) ? 15 : 11) | hb << 4, fd);
		int64_t x64 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
	}
}

void BRB_writeIntOnly(FILE* fd, uint64_t x)
{
	if (FITS_IN_8BITS(x)) {
		if (!inRange(x, 8, 16)) {
			fputc(x, fd);
		} else {
			fputc(SIGN_BIT_SET(x) ? 12 : 8, fd);
			fputc((int8_t)(SIGN_BIT_SET(x) ? ~x : x), fd);
		}
	} else if (FITS_IN_16BITS(x)) {
		fputc(SIGN_BIT_SET(x) ? 13 : 9, fd);
		int16_t x16 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
	} else if (FITS_IN_32BITS(x)) {
		fputc(SIGN_BIT_SET(x) ? 14 : 10, fd);
		int32_t x32 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
	} else {
		fputc(SIGN_BIT_SET(x) ? 15 : 11, fd);
		int64_t x64 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
	}
}

void BRB_writeIntSwapped(FILE* fd, uint64_t x, uint8_t hb)
{
	if (FITS_IN_8BITS(x)) {
		if (x < 8) {
			fputc(hb | x << 4, fd);
		} else {
			fputc((SIGN_BIT_SET(x) ? 12 : 8) << 4 | hb, fd);
			fputc((int8_t)(SIGN_BIT_SET(x) ? ~x : x), fd);
		}
	} else if (FITS_IN_16BITS(x)) {
		fputc((SIGN_BIT_SET(x) ? 13 : 9) << 4 | hb, fd);
		int16_t x16 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
	} else if (FITS_IN_32BITS(x)) {
		fputc((SIGN_BIT_SET(x) ? 14 : 10) << 4 | hb, fd);
		int32_t x32 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
	} else {
		fputc((SIGN_BIT_SET(x) ? 15 : 11) << 4 | hb, fd);
		int64_t x64 = SIGN_BIT_SET(x) ? ~x : x;
		fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
	}
}

void BRB_write2HalfBytes(FILE* fd, uint8_t hb1, uint8_t hb2)
{
	fputc(((hb1 << 4) | hb2) & 255, fd);
}

void BRB_write2Ints(FILE* fd, uint64_t x, uint64_t y)
{
	if (x < 8) {
		if (y < 8) {
			fputc((int8_t)(x << 4 | y), fd);
		} else BRB_writeInt(fd, y, x);
	} else if (y < 8) {
		BRB_writeIntSwapped(fd, x, y);
	} else if (FITS_IN_8BITS(x)) {
		if (FITS_IN_8BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 12 : 8) << 4 | (SIGN_BIT_SET(y) ? 12 : 8), fd);
			fputc((int8_t)(SIGN_BIT_SET(x) ? ~x : x), fd);
			fputc((int8_t)(SIGN_BIT_SET(y) ? ~y : y), fd);
		} else if (FITS_IN_16BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 12 : 8) << 4 | (SIGN_BIT_SET(y) ? 13 : 9), fd);
			uint16_t y16 = SIGN_BIT_SET(y) ? ~y : y;
			fputc((int8_t)(SIGN_BIT_SET(x) ? ~x : x), fd);
			fwrite(BRByteOrder(&y16, 2), 2, 1, fd);
		} else if (FITS_IN_32BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 12 : 8) << 4 | (SIGN_BIT_SET(y) ? 14 : 10), fd);
			uint32_t y32 = SIGN_BIT_SET(y) ? ~y : y;
			fputc((int8_t)(SIGN_BIT_SET(x) ? ~x : x), fd);
			fwrite(BRByteOrder(&y32, 4), 4, 1, fd);
		} else {
			fputc((SIGN_BIT_SET(x) ? 12 : 8) << 4 | (SIGN_BIT_SET(y) ? 15 : 11), fd);
			uint64_t y64 = SIGN_BIT_SET(y) ? ~y : y;
			fputc((int8_t)(SIGN_BIT_SET(x) ? ~x : x), fd);
			fwrite(BRByteOrder(&y64, 8), 8, 1, fd);
		}
	} else if (FITS_IN_16BITS(x)) {
		uint16_t x16 = x < 0 ? ~x : x;
		if (FITS_IN_8BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 13 : 9) << 4 | (SIGN_BIT_SET(y) ? 12 : 8), fd);
			fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
			fputc((int8_t)(y < 0 ? ~y : y), fd);
		} else if (FITS_IN_16BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 13 : 9) << 4 | (SIGN_BIT_SET(y) ? 13 : 9), fd);
			uint16_t y16 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
			fwrite(BRByteOrder(&y16, 2), 2, 1, fd);
		} else if (FITS_IN_32BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 13 : 9) << 4 | (SIGN_BIT_SET(y) ? 14 : 10), fd);
			uint32_t y32 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
			fwrite(BRByteOrder(&y32, 4), 4, 1, fd);
		} else {
			fputc((SIGN_BIT_SET(x) ? 13 : 9) << 4 | (SIGN_BIT_SET(y) ? 15 : 11), fd);
			uint64_t y64 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x16, 2), 2, 1, fd);
			fwrite(BRByteOrder(&y64, 8), 8, 1, fd);
		}
	} else if (FITS_IN_32BITS(x)) {
		uint32_t x32 = SIGN_BIT_SET(x) ? ~x : x;
		if (FITS_IN_8BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 14 : 10) << 4 | (SIGN_BIT_SET(y) ? 12 : 8), fd);
			fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
			fputc((int8_t)(y < 0 ? ~y : y), fd);
		} else if (FITS_IN_16BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 14 : 10) << 4 | (SIGN_BIT_SET(y) ? 13 : 9), fd);
			uint16_t y16 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
			fwrite(BRByteOrder(&y16, 2), 2, 1, fd);
		} else if (FITS_IN_32BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 14 : 10) << 4 | (SIGN_BIT_SET(y) ? 14 : 10), fd);
			uint32_t y32 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
			fwrite(BRByteOrder(&y32, 4), 4, 1, fd);
		} else {
			fputc((SIGN_BIT_SET(x) ? 14 : 10) << 4 | (SIGN_BIT_SET(y) ? 15 : 11), fd);
			uint64_t y64 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x32, 4), 4, 1, fd);
			fwrite(BRByteOrder(&y64, 8), 8, 1, fd);
		}
	} else {
		uint64_t x64 = SIGN_BIT_SET(x) ? ~x : x;
		if (FITS_IN_8BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 15 : 11) << 4 | (SIGN_BIT_SET(y) ? 12 : 8), fd);
			fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
			fputc((int8_t)(SIGN_BIT_SET(y) ? ~y : y), fd);
		} else if (FITS_IN_16BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 15 : 11) << 4 | (SIGN_BIT_SET(y) ? 13 : 9), fd);
			uint16_t y16 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
			fwrite(BRByteOrder(&y16, 2), 2, 1, fd);
		} else if (FITS_IN_32BITS(y)) {
			fputc((SIGN_BIT_SET(x) ? 15 : 11) << 4 | (SIGN_BIT_SET(y) ? 14 : 10), fd);
			uint32_t y32 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
			fwrite(BRByteOrder(&y32, 4), 4, 1, fd);
		} else {
			fputc((SIGN_BIT_SET(x) ? 15 : 11) << 4 | (SIGN_BIT_SET(y) ? 15 : 11), fd);
			uint64_t y64 = SIGN_BIT_SET(y) ? ~y : y;
			fwrite(BRByteOrder(&x64, 8), 8, 1, fd);
			fwrite(BRByteOrder(&y64, 8), 8, 1, fd);
		}
	}
}

uint32_t BRB_getNameId(BRB_ModuleWriter* writer, const char* name)
{
	arrayForeach (str, iter, writer->names) {
		if (streq(name, *iter)) return iter - writer->names.data;
	}
	strArray_append(&writer->names, name);
	return writer->names.length - 1;
}

void BRB_writeDataBlock(BRB_ModuleWriter* writer, BRB_DataBlock block)
{
	BRB_writeInt(writer->dst, BRB_getNameId(writer, block.name), block.is_mutable);
	BRB_writeIntOnly(writer->dst, block.pieces.length);
	arrayForeach (BRB_DataPiece, piece, block.pieces) {
		fputc(piece->type, writer->dst);
		switch (piece->type) {
			case BRB_DP_BYTES:
				BRB_writeIntOnly(writer->dst, piece->data.length);
				fputsbuf(writer->dst, piece->data);
				break;
			case BRB_DP_TEXT:
				fputsbuf(writer->dst, piece->data);
				fputc('\0', writer->dst);
				break;
			case BRB_DP_INT16:
			case BRB_DP_INT32:
			case BRB_DP_INTPTR:
			case BRB_DP_INT64:
			case BRB_DP_ZERO:
			case BRB_DP_BUILTIN:
				BRB_writeIntOnly(writer->dst, piece->operand_u);
				break;
			case BRB_DP_DBADDR:
				BRB_writeIntOnly(writer->dst, BRB_getNameId(writer, writer->src->seg_data.data[piece->operand_u].name));
				break;
			case BRB_DP_NONE:
			case BRB_N_DP_TYPES:
			default:
				assert(false, "invalid data piece type");
		}
	}
}

void BRB_writeOp(BRB_ModuleWriter* writer, BRB_Op op)
{
	fputc(op.type, writer->dst);
	if (BRB_opFlags[op.type] & BRB_OPF_HAS_OPERAND)
		BRB_writeIntOnly(writer->dst, op.operand_u);
}

void BRB_writeModule(BRB_Module src, FILE* dst)
{
	BRB_ModuleWriter writer = {
		.src = &src,
		.dst = dst
	};

	fputsbuf(dst, BRB_V1_HEADER);
	BRB_writeIntOnly(dst, src.seg_data.length);
	arrayForeach (BRB_DataBlock, block, src.seg_data) {
		BRB_writeDataBlock(&writer, *block);
	}

	BRB_writeIntOnly(dst, src.seg_exec.length);
	arrayForeach (BRB_Op, op, src.seg_exec) {
		BRB_writeOp(&writer, *op);
	}

	fieldArray names = BRB_getNameFields(&src);
	arrayForeach (field, name, names) {
		fputs(**name, dst);
		fputc('\0', dst);
	}
}
