#define BRIDGE_IMPLEMENTATION
#define _BRB_INTERNAL
#include <brb.h>
#include <unistd.h>
#include <sys/param.h>
#include <math.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <spawn.h>
#include <sys/stat.h>
#include <sys/wait.h>
#define ARENA_IMPLEMENTATION
#include <external/arena.h>
extern char** environ;

defArray(BRB_Struct);
defArray(BRB_Proc);
defArray(BRB_Type);
defArray(BRB_Op);
defArray(BRB_DataBlock);
defArray(BRB_StackNode);
defArray(BRB_StackNodeArray);

#define BRB_SNF_STACKFRAME 0x1

const sbuf BRB_opNames[]   = {
	[BRB_OP_NOP]       = fromcstr("nop"),
	[BRB_OP_END]       = fromcstr("end"),
	[BRB_OP_I8]        = fromcstr("i8"),
	[BRB_OP_I16]       = fromcstr("i16"),
	[BRB_OP_I32]       = fromcstr("i32"),
	[BRB_OP_PTR]       = fromcstr("ptr"),
	[BRB_OP_I64]       = fromcstr("i64"),
	[BRB_OP_ADDR]      = fromcstr("addr"),
	[BRB_OP_DBADDR]    = fromcstr("dbaddr"),
	[BRB_OP_LD]        = fromcstr("ld"),
	[BRB_OP_STR]       = fromcstr("str"),
	[BRB_OP_SYS]       = fromcstr("sys"),
	[BRB_OP_BUILTIN]   = fromcstr("builtin"),
	[BRB_OP_ADD]       = fromcstr("add"),
	[BRB_OP_ADDI]      = fromcstr("add-i"),
	[BRB_OP_ADDIAT8]   = fromcstr("add-i@8"),
	[BRB_OP_ADDIAT16]  = fromcstr("add-i@16"),
	[BRB_OP_ADDIAT32]  = fromcstr("add-i@32"),
	[BRB_OP_ADDIATP]   = fromcstr("add-i@p"),
	[BRB_OP_ADDIAT64]  = fromcstr("add-i@64"),
	[BRB_OP_SUB]       = fromcstr("sub"),
	[BRB_OP_SUBI]      = fromcstr("sub-i"),
	[BRB_OP_SUBIAT8]   = fromcstr("sub-i@8"),
	[BRB_OP_SUBIAT16]  = fromcstr("sub-i@16"),
	[BRB_OP_SUBIAT32]  = fromcstr("sub-i@32"),
	[BRB_OP_SUBIATP]   = fromcstr("sub-i@p"),
	[BRB_OP_SUBIAT64]  = fromcstr("sub-i@64"),
	[BRB_OP_MUL]       = fromcstr("mul"),
	[BRB_OP_MULI]      = fromcstr("mul-i"),
	[BRB_OP_MULIAT8]   = fromcstr("mul-i@8"),
	[BRB_OP_MULIAT16]  = fromcstr("mul-i@16"),
	[BRB_OP_MULIAT32]  = fromcstr("mul-i@32"),
	[BRB_OP_MULIATP]   = fromcstr("mul-i@p"),
	[BRB_OP_MULIAT64]  = fromcstr("mul-i@64"),
	[BRB_OP_DIV]       = fromcstr("div"),
	[BRB_OP_DIVI]      = fromcstr("div-i"),
	[BRB_OP_DIVIAT8]   = fromcstr("div-i@8"),
	[BRB_OP_DIVIAT16]  = fromcstr("div-i@16"),
	[BRB_OP_DIVIAT32]  = fromcstr("div-i@32"),
	[BRB_OP_DIVIATP]   = fromcstr("div-i@p"),
	[BRB_OP_DIVIAT64]  = fromcstr("div-i@64"),
	[BRB_OP_DIVS]      = fromcstr("divs"),
	[BRB_OP_DIVSI]     = fromcstr("divs-i"),
	[BRB_OP_DIVSIAT8]  = fromcstr("divs-i@8"),
	[BRB_OP_DIVSIAT16] = fromcstr("divs-i@16"),
	[BRB_OP_DIVSIAT32] = fromcstr("divs-i@32"),
	[BRB_OP_DIVSIATP]  = fromcstr("divs-i@p"),
	[BRB_OP_DIVSIAT64] = fromcstr("divs-i@64"),
	[BRB_OP_MOD]       = fromcstr("mod"),
	[BRB_OP_MODI]      = fromcstr("mod-i"),
	[BRB_OP_MODIAT8]   = fromcstr("mod-i@8"),
	[BRB_OP_MODIAT16]  = fromcstr("mod-i@16"),
	[BRB_OP_MODIAT32]  = fromcstr("mod-i@32"),
	[BRB_OP_MODIATP]   = fromcstr("mod-i@p"),
	[BRB_OP_MODIAT64]  = fromcstr("mod-i@64"),
	[BRB_OP_MODS]      = fromcstr("mods"),
	[BRB_OP_MODSI]     = fromcstr("mods-i"),
	[BRB_OP_MODSIAT8]  = fromcstr("mods-i@8"),
	[BRB_OP_MODSIAT16] = fromcstr("mods-i@16"),
	[BRB_OP_MODSIAT32] = fromcstr("mods-i@32"),
	[BRB_OP_MODSIATP]  = fromcstr("mods-i@p"),
	[BRB_OP_MODSIAT64] = fromcstr("mods-i@64"),
	[BRB_OP_AND]       = fromcstr("and"),
	[BRB_OP_ANDI]      = fromcstr("and-i"),
	[BRB_OP_ANDIAT8]   = fromcstr("and-i@8"),
	[BRB_OP_ANDIAT16]  = fromcstr("and-i@16"),
	[BRB_OP_ANDIAT32]  = fromcstr("and-i@32"),
	[BRB_OP_ANDIATP]   = fromcstr("and-i@p"),
	[BRB_OP_ANDIAT64]  = fromcstr("and-i@64"),
	[BRB_OP_OR]        = fromcstr("or"),
	[BRB_OP_ORI]       = fromcstr("or-i"),
	[BRB_OP_ORIAT8]    = fromcstr("or-i@8"),
	[BRB_OP_ORIAT16]   = fromcstr("or-i@16"),
	[BRB_OP_ORIAT32]   = fromcstr("or-i@32"),
	[BRB_OP_ORIATP]    = fromcstr("or-i@p"),
	[BRB_OP_ORIAT64]   = fromcstr("or-i@64"),
	[BRB_OP_XOR]       = fromcstr("xor"),
	[BRB_OP_XORI]      = fromcstr("xor-i"),
	[BRB_OP_XORIAT8]   = fromcstr("xor-i@8"),
	[BRB_OP_XORIAT16]  = fromcstr("xor-i@16"),
	[BRB_OP_XORIAT32]  = fromcstr("xor-i@32"),
	[BRB_OP_XORIATP]   = fromcstr("xor-i@p"),
	[BRB_OP_XORIAT64]  = fromcstr("xor-i@64"),
	[BRB_OP_SHL]       = fromcstr("shl"),
	[BRB_OP_SHLI]      = fromcstr("shl-i"),
	[BRB_OP_SHLIAT8]   = fromcstr("shl-i@8"),
	[BRB_OP_SHLIAT16]  = fromcstr("shl-i@16"),
	[BRB_OP_SHLIAT32]  = fromcstr("shl-i@32"),
	[BRB_OP_SHLIATP]   = fromcstr("shl-i@p"),
	[BRB_OP_SHLIAT64]  = fromcstr("shl-i@64"),
	[BRB_OP_SHR]       = fromcstr("shr"),
	[BRB_OP_SHRI]      = fromcstr("shr-i"),
	[BRB_OP_SHRIAT8]   = fromcstr("shr-i@8"),
	[BRB_OP_SHRIAT16]  = fromcstr("shr-i@16"),
	[BRB_OP_SHRIAT32]  = fromcstr("shr-i@32"),
	[BRB_OP_SHRIATP]   = fromcstr("shr-i@p"),
	[BRB_OP_SHRIAT64]  = fromcstr("shr-i@64"),
	[BRB_OP_SHRS]      = fromcstr("shrs"),
	[BRB_OP_SHRSI]     = fromcstr("shrs-i"),
	[BRB_OP_SHRSIAT8]  = fromcstr("shrs-i@8"),
	[BRB_OP_SHRSIAT16] = fromcstr("shrs-i@16"),
	[BRB_OP_SHRSIAT32] = fromcstr("shrs-i@32"),
	[BRB_OP_SHRSIATP]  = fromcstr("shrs-i@p"),
	[BRB_OP_SHRSIAT64] = fromcstr("shrs-i@64"),
	[BRB_OP_NOT]       = fromcstr("not"),
	[BRB_OP_NOTAT8]    = fromcstr("not-@8"),
	[BRB_OP_NOTAT16]   = fromcstr("not-@16"),
	[BRB_OP_NOTAT32]   = fromcstr("not-@32"),
	[BRB_OP_NOTATP]    = fromcstr("not-@p"),
	[BRB_OP_NOTAT64]   = fromcstr("not-@64"),
	[BRB_OP_DROP]      = fromcstr("drop"),
	[BRB_OP_NEW]       = fromcstr("new"),
	[BRB_OP_ZERO]      = fromcstr("zero"),
	[BRB_OP_COPY]      = fromcstr("copy"),
	[BRB_OP_COPYTO]    = fromcstr("copy-to")
};
static_assert(sizeof(BRB_opNames) / sizeof(BRB_opNames[0]) == BRB_N_OPS, "not all BRB operations have their names defined");

#define SET_OPERAND_TYPE(type) (BRB_OPERAND_##type << 0)
#define SET_ADDR_OP_TYPE(type) (BRB_ADDR_##type    << 3)
#define SET_BASE_OP(type)      (BRB_OP_##type      << 7)
const uint64_t BRB_opFlags[] = {
	[BRB_OP_NOP]       = SET_BASE_OP(NOP),
	[BRB_OP_END]       = SET_BASE_OP(END),
	[BRB_OP_I8]        = SET_BASE_OP(I8)      | SET_OPERAND_TYPE(INT8),
	[BRB_OP_I16]       = SET_BASE_OP(I16)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_I32]       = SET_BASE_OP(I32)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_PTR]       = SET_BASE_OP(PTR)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_I64]       = SET_BASE_OP(I64)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_ADDR]      = SET_BASE_OP(ADDR)    | SET_OPERAND_TYPE(VAR_NAME),
	[BRB_OP_DBADDR]    = SET_BASE_OP(DBADDR)  | SET_OPERAND_TYPE(DB_NAME),
	[BRB_OP_LD]        = SET_BASE_OP(LD)      | SET_OPERAND_TYPE(TYPE),
	[BRB_OP_STR]       = SET_BASE_OP(STR),
	[BRB_OP_SYS]       = SET_BASE_OP(SYS)     | SET_OPERAND_TYPE(SYSCALL_NAME),
	[BRB_OP_BUILTIN]   = SET_BASE_OP(BUILTIN) | SET_OPERAND_TYPE(BUILTIN),
	[BRB_OP_ADD]       = SET_BASE_OP(ADD),
	[BRB_OP_ADDI]      = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_ADDIAT8]   = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_ADDIAT16]  = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_ADDIAT32]  = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_ADDIATP]   = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_ADDIAT64]  = SET_BASE_OP(ADD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_SUB]       = SET_BASE_OP(SUB),
	[BRB_OP_SUBI]      = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_SUBIAT8]   = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_SUBIAT16]  = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_SUBIAT32]  = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_SUBIATP]   = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_SUBIAT64]  = SET_BASE_OP(SUB)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_MUL]       = SET_BASE_OP(MUL),
	[BRB_OP_MULI]      = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_MULIAT8]   = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_MULIAT16]  = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_MULIAT32]  = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_MULIATP]   = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_MULIAT64]  = SET_BASE_OP(MUL)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_DIV]       = SET_BASE_OP(DIV),
	[BRB_OP_DIVI]      = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_DIVIAT8]   = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_DIVIAT16]  = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_DIVIAT32]  = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_DIVIATP]   = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_DIVIAT64]  = SET_BASE_OP(DIV)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_DIVS]      = SET_BASE_OP(DIVS),
	[BRB_OP_DIVSI]     = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT),
	[BRB_OP_DIVSIAT8]  = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_DIVSIAT16] = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_DIVSIAT32] = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_DIVSIATP]  = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_DIVSIAT64] = SET_BASE_OP(DIVS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_MOD]       = SET_BASE_OP(MOD),
	[BRB_OP_MODI]      = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_MODIAT8]   = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_MODIAT16]  = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_MODIAT32]  = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_MODIATP]   = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_MODIAT64]  = SET_BASE_OP(MOD)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_MODS]      = SET_BASE_OP(MODS),
	[BRB_OP_MODSI]     = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT),
	[BRB_OP_MODSIAT8]  = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_MODSIAT16] = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_MODSIAT32] = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_MODSIATP]  = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_MODSIAT64] = SET_BASE_OP(MODS)    | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_AND]       = SET_BASE_OP(AND),
	[BRB_OP_ANDI]      = SET_BASE_OP(AND)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_ANDIAT8]   = SET_BASE_OP(AND)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_ANDIAT16]  = SET_BASE_OP(AND)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_ANDIAT32]  = SET_BASE_OP(AND)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_ANDIATP]   = SET_BASE_OP(AND)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_ANDIAT64]  = SET_BASE_OP(AND)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_OR]        = SET_BASE_OP(OR),
	[BRB_OP_ORI]       = SET_BASE_OP(OR)      | SET_OPERAND_TYPE(INT),
	[BRB_OP_ORIAT8]    = SET_BASE_OP(OR)      | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_ORIAT16]   = SET_BASE_OP(OR)      | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_ORIAT32]   = SET_BASE_OP(OR)      | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_ORIATP]    = SET_BASE_OP(OR)      | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_ORIAT64]   = SET_BASE_OP(OR)      | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_XOR]       = SET_BASE_OP(XOR),
	[BRB_OP_XORI]      = SET_BASE_OP(XOR)     | SET_OPERAND_TYPE(INT),
	[BRB_OP_XORIAT8]   = SET_BASE_OP(XOR)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_XORIAT16]  = SET_BASE_OP(XOR)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_XORIAT32]  = SET_BASE_OP(XOR)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_XORIATP]   = SET_BASE_OP(XOR)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_XORIAT64]  = SET_BASE_OP(XOR)     | SET_OPERAND_TYPE(INT) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_SHL]       = SET_BASE_OP(SHL),
	[BRB_OP_SHLI]      = SET_BASE_OP(SHL)     | SET_OPERAND_TYPE(INT8),
	[BRB_OP_SHLIAT8]   = SET_BASE_OP(SHL)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_SHLIAT16]  = SET_BASE_OP(SHL)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_SHLIAT32]  = SET_BASE_OP(SHL)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_SHLIATP]   = SET_BASE_OP(SHL)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_SHLIAT64]  = SET_BASE_OP(SHL)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_SHR]       = SET_BASE_OP(SHR),
	[BRB_OP_SHRI]      = SET_BASE_OP(SHR)     | SET_OPERAND_TYPE(INT8),
	[BRB_OP_SHRIAT8]   = SET_BASE_OP(SHR)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_SHRIAT16]  = SET_BASE_OP(SHR)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_SHRIAT32]  = SET_BASE_OP(SHR)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_SHRIATP]   = SET_BASE_OP(SHR)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_SHRIAT64]  = SET_BASE_OP(SHR)     | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_SHRS]      = SET_BASE_OP(SHRS),
	[BRB_OP_SHRSI]     = SET_BASE_OP(SHRS)    | SET_OPERAND_TYPE(INT8),
	[BRB_OP_SHRSIAT8]  = SET_BASE_OP(SHRS)    | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_SHRSIAT16] = SET_BASE_OP(SHRS)    | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_SHRSIAT32] = SET_BASE_OP(SHRS)    | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_SHRSIATP]  = SET_BASE_OP(SHRS)    | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_SHRSIAT64] = SET_BASE_OP(SHRS)    | SET_OPERAND_TYPE(INT8) | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_NOT]       = SET_BASE_OP(NOT),
	[BRB_OP_NOTAT8]    = SET_BASE_OP(NOT)     | SET_ADDR_OP_TYPE(I8),
	[BRB_OP_NOTAT16]   = SET_BASE_OP(NOT)     | SET_ADDR_OP_TYPE(I16),
	[BRB_OP_NOTAT32]   = SET_BASE_OP(NOT)     | SET_ADDR_OP_TYPE(I32),
	[BRB_OP_NOTATP]    = SET_BASE_OP(NOT)     | SET_ADDR_OP_TYPE(PTR),
	[BRB_OP_NOTAT64]   = SET_BASE_OP(NOT)     | SET_ADDR_OP_TYPE(I64),
	[BRB_OP_DROP]      = SET_BASE_OP(DROP),
	[BRB_OP_NEW]       = SET_BASE_OP(NEW)     | SET_OPERAND_TYPE(TYPE),
	[BRB_OP_ZERO]      = SET_BASE_OP(ZERO)    | SET_OPERAND_TYPE(TYPE),
	[BRB_OP_COPY]      = SET_BASE_OP(COPY)    | SET_OPERAND_TYPE(VAR_NAME),
	[BRB_OP_COPYTO]    = SET_BASE_OP(COPY),
};
static_assert(sizeof(BRB_opFlags) / sizeof(BRB_opFlags[0]) == BRB_N_OPS, "not all BRB operations have their flags defined");

const sbuf BRB_syscallNames[] = {
	[BRB_SYS_EXIT]  = fromcstr("exit"),
	[BRB_SYS_WRITE] = fromcstr("write"),
	[BRB_SYS_READ]  = fromcstr("read")
};
static_assert(sizeof(BRB_syscallNames) / sizeof(BRB_syscallNames[0]) == BRB_N_SYSCALLS, "not all BRB syscalls have their names defined");

const uintptr_t BRB_builtinValues[] = {
	[BRB_BUILTIN_NULL] = (uintptr_t)NULL,
	[BRB_BUILTIN_STDIN] = STDIN_FILENO,
	[BRB_BUILTIN_STDOUT] = STDOUT_FILENO,
	[BRB_BUILTIN_STDERR] = STDERR_FILENO,
};
static_assert(sizeof(BRB_builtinValues) / sizeof(BRB_builtinValues[0]) == BRB_N_BUILTINS, "not all BRB built-ins have their runtime values defined");

const sbuf BRB_builtinNames[] = {
	[BRB_BUILTIN_NULL] = fromcstr("NULL"),
	[BRB_BUILTIN_STDIN] = fromcstr("STDIN"),
	[BRB_BUILTIN_STDOUT] = fromcstr("STDOUT"),
	[BRB_BUILTIN_STDERR] = fromcstr("STDERR"),
};
static_assert(sizeof(BRB_builtinNames) / sizeof(BRB_builtinNames[0]) == BRB_N_BUILTINS, "not all BRB built-ins have their names defined");

const size_t BRB_syscallNArgs[] = {
	[BRB_SYS_EXIT] = 1, // [code]
	[BRB_SYS_WRITE] = 3, // [fd, addr, n_bytes]
	[BRB_SYS_READ] = 3, // [fd, addr, n_bytes]
};
static_assert(sizeof(BRB_syscallNArgs) / sizeof(BRB_syscallNArgs[0]) == BRB_N_SYSCALLS, "not all BRB syscalls have their prototype defined");

const sbuf BRB_typeNames[] = {
	[BRB_TYPE_DYNAMIC] = fromcstr("__dynamic"),
	[BRB_TYPE_I8]      = fromcstr("i8"),
	[BRB_TYPE_I16]     = fromcstr("i16"),
	[BRB_TYPE_I32]     = fromcstr("i32"),
	[BRB_TYPE_PTR]     = fromcstr("ptr"),
	[BRB_TYPE_I64]     = fromcstr("i64"),
	[BRB_TYPE_VOID]    = fromcstr("void"),
	[BRB_TYPE_STRUCT]  = fromcstr("struct"),
	[BRB_TYPE_INT]     = fromcstr("__int"),
	[BRB_TYPE_ANY]     = fromcstr("__any")
};
static_assert(sizeof(BRB_typeNames) / sizeof(BRB_typeNames[0]) == BRB_N_TYPE_KINDS, "not all BRB types have their names defined");

bool startTimerAt(struct timespec* dst)
{
	return !clock_gettime(CLOCK_MONOTONIC, dst);
}

float endTimerAt(struct timespec* src)
{
	struct timespec newtime;
	clock_gettime(CLOCK_MONOTONIC, &newtime);
	return (newtime.tv_sec - src->tv_sec) * 1000 + (newtime.tv_nsec - src->tv_nsec) / (float)1e6;
}

bool execProcess(char* command, ProcessInfo* info)
{
	pid_t pid;
	int out_pipe[2], err_pipe[2], local_errno, exit_status;
	posix_spawn_file_actions_t file_actions;

	if ((local_errno = posix_spawn_file_actions_init(&file_actions))) {
		errno = local_errno;
		return false;
	}

	if (info->in) {
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, fileno(info->in), STDIN_FILENO))) {
			errno = local_errno;
			return false;
		}
	}
	if (info->out != stdout) {
		if (pipe(out_pipe) < 0) return false;
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, out_pipe[1], STDOUT_FILENO))) {
			errno = local_errno;
			return false;
		}
	}
	if (info->err != stderr) {
		if (pipe(err_pipe) < 0) return false;
		if ((local_errno = posix_spawn_file_actions_adddup2(&file_actions, err_pipe[1], STDERR_FILENO))) {
			errno = local_errno;
			return false;
		}
	}

	char* argv[] = { "sh", "-c", command, NULL };

	if ((local_errno = posix_spawn(&pid, "/bin/sh", &file_actions, NULL, argv, environ))) {
		errno = local_errno;
		return false;
	}

	if (waitpid(pid, &exit_status, 0) != pid) return false;

	if (info->out != stdout) {
		close(out_pipe[1]);
		info->out = fdopen(out_pipe[0], "r");
	} else {
		info->out = NULL;
	}
	if (info->err != stderr) {
		close(err_pipe[1]);
		info->err = fdopen(err_pipe[0], "r");
	} else {
		info->err = NULL;
	}
	info->exited = WIFEXITED(exit_status);
	info->exitcode = info->exited ? WEXITSTATUS(exit_status) : WTERMSIG(exit_status);

	return true;
}

bool execProcess_s(sbuf command, ProcessInfo* info)
{
	char temp[command.length + 1];
	memcpy(temp, command.data, command.length);
	temp[command.length] = '\0';
	return execProcess(temp, info);
}

bool isPathDir(char* path)
{
	struct stat info;
	if (lstat(path, &info)) return false;
	return S_ISDIR(info.st_mode);
}

bool isPathDir_s(sbuf path)
{
	char temp[path.length + 1];
	memcpy(temp, path.data, path.length);
	temp[path.length] = '\0';
	return isPathDir(temp);
}

char* getFileExt(const char* path)
{
	char* dot = strrchr(path, '.');
	if (!dot || dot == path) return "";
	return strdup(dot + 1);
}

sbuf getFileExt_s(sbuf path)
{
	sbuf noext;
	if (!sbufsplitr(&path, &noext, fromcstr(".")).data) return fromcstr("");
	return path;
}

char* setFileExt(const char* path, const char* ext)
{
	sbuf src = fromstr((char*)path);
	sbuf noext;
	sbufsplitr(&src, &noext, fromcstr("."));
	return *ext ? tostr(noext, fromcstr("."), fromstr((char*)ext)) : tostr(noext);
}

sbuf setFileExt_s(sbuf path, sbuf ext)
{
	sbuf noext;
	sbufsplitr(&path, &noext, fromcstr("."));
	return ext.length ? sbufconcat(noext, fromcstr("."), ext) : sbufcopy(noext);
}

char* fileBaseName(const char* path)
{
	sbuf src = fromstr((char*)path);
	sbuf res;
	if (sbufeq(sbufsplitr(&src, &res, fromcstr("."), PATHSEP), fromcstr("."))) {
		return tostr(sbufsplitr(&res, &src, PATHSEP).length ? res : src);
	} else return tostr(src.length ? src : res);
}

sbuf fileBaseName_s(sbuf path)
{
	sbuf res;
	if (sbufeq(sbufsplitr(&path, &res, fromcstr("."), PATHSEP), fromcstr("."))) {
		return sbufsplitr(&res, &path, PATHSEP).length ? res : path;
	} else return path.length ? path : res;
}

void BRB_printErrorMsg(FILE* dst, BRB_Error err, const char* prefix)
{
	if (err.type == BRB_ERR_OK) return;
	if (err.loc) {
		BRP_fprintTokenLoc(dst, *err.loc);
		fputs(": ", dst);
	}
	if (prefix) fprintf(dst, "%s: ", prefix);
	switch (err.type) {
		case BRB_ERR_INVALID_HEADER:
			fputs("invalid module header: \"", dst);
			fputsbufesc(dst, (sbuf){ .data = err.header, .length = BRB_HEADER_SIZE }, BYTEFMT_HEX | BYTEFMT_ESC_DQUOTE);
			fputs("\"\n", dst);
			break;
		case BRB_ERR_NO_HEADER:
			fputs("no module header found\n", dst);
			break;
		case BRB_ERR_NO_MEMORY:
			fputs("memory allocation failed\n", dst);
			break;
		case BRB_ERR_NO_SEG_SIZES:
			fputs("unexpected end of input while loading segment sizes\n", dst);
			break;
		case BRB_ERR_NO_OPCODE:
			fputs("unexpected end of input while loading type of an operation\n", dst);
			break;
		case BRB_ERR_INVALID_OPCODE:
			fprintf(dst, "invalid operation type: %u\n", err.opcode);
			break;
		case BRB_ERR_NO_OPERAND:
			fprintf(dst, "unexpected end of input while loading operand of an operation `%.*s`\n", unpack(BRB_opNames[err.opcode]));
			break;
		case BRB_ERR_INVALID_NAME:
			fputs("invalid name found\n", dst);
			break;
		case BRB_ERR_NAMES_NOT_RESOLVED:
			fputs("not all symbol names were resolved from the `name` segment\n", dst);
			break;
		case BRB_ERR_STACK_UNDERFLOW:
			if (err.opcode == BRB_N_OPS) {
				fprintf(dst, "attempted to label head of an empty stack\n");
			} else fprintf(dst,
				"stack underflow for operation `%.*s`: expected %u items, instead got %u\n",
				unpack(BRB_opNames[err.opcode]),
				err.expected_stack_length,
				err.actual_stack_length
			);
			break;
		case BRB_ERR_OPERAND_OUT_OF_RANGE:
			fprintf(dst, "operand %llu is out of range for operation `%.*s`\n", err.operand, unpack(BRB_opNames[err.opcode]));
			break;
		case BRB_ERR_NO_PROC_RET_TYPE:
			fprintf(dst, "unexpected end of input while loading return type of a procedure\n");
			break;
		case BRB_ERR_NO_PROC_NAME:
			fprintf(dst, "unexpected end of input while loading name of a procedure\n");
			break;
		case BRB_ERR_NO_PROC_ARG:
			fprintf(dst, "unexpected end of input while loading arguments of a procedure\n");
			break;
		case BRB_ERR_NO_PROC_BODY_SIZE:
			fprintf(dst, "unexpected end of input while loading body size of a procedure\n");
			break;
		case BRB_ERR_NO_DB_NAME:
			fprintf(dst, "unexpected end of input while loading name of a data block\n");
			break;
		case BRB_ERR_NO_DB_BODY_SIZE:
			fprintf(dst, "unexpected end of input whole loading body size of a data block\n");
			break;
		case BRB_ERR_NO_ENTRY:
			fprintf(dst, "unexpected end of input while loading the entry point\n");
			break;
		case BRB_ERR_INVALID_ENTRY:
			fprintf(dst, "invalid entry point ID provided\n");
			break;
		case BRB_ERR_INVALID_ENTRY_PROTOTYPE:
			fprintf(dst, "the entry point procedure must not accept any arguments or return anything\n");
			break;
		case BRB_ERR_TYPE_EXPECTED:
			fprintf(dst, "expected a type specification\n");
			break;
		case BRB_ERR_OP_NAME_EXPECTED:
			fprintf(dst, "expected an operation name\n");
			break;
		case BRB_ERR_INT_OPERAND_EXPECTED:
			fprintf(dst, "expected an integer as an operand\n");
			break;
		case BRB_ERR_INT_OR_DB_NAME_EXPECTED:
			fprintf(dst, "expected an integer or a data block name as an operand\n");
			break;
		case BRB_ERR_BUILTIN_NAME_EXPECTED:
			fprintf(dst, "expected name of a built-in constant as an operand\n");
			break;
		case BRB_ERR_TEXT_OPERAND_EXPECTED:
			fprintf(dst, "expected a string literal as an operand\n");
			break;
		case BRB_ERR_INVALID_DECL:
			fprintf(dst, "top-level statements in the assembly code must start with either a return type specification, or with keyword `data`\n");
			break;
		case BRB_ERR_ARGS_EXPECTED:
			fprintf(dst, "symbol `(` expected after name of the declared procedure\n");
			break;
		case BRB_ERR_PROTOTYPE_MISMATCH:
			fprintf(dst, "procedure `%s` was already declared, but with another prototype\n", err.name);
			break;
		case BRB_ERR_SYSCALL_NAME_EXPECTED:
			fprintf(dst, "expected a syscall name as an operand\n");
			break;
		case BRB_ERR_INVALID_ARRAY_SIZE_SPEC:
			fprintf(dst, "array types must be denoted the following way: T[n]\n");
			break;
		case BRB_ERR_TYPE_MISMATCH:
			fprintf(dst, "argument #%u for a `%.*s` operation is expected to be of type `", err.arg_id, unpack(BRB_opNames[err.opcode]));
			BRB_printType(err.expected_type, dst);
			fputs("` instead got an argument of type `", dst);
			BRB_printType(err.actual_type, dst);
			fputs("`\n", dst);
			break;
		case BRB_ERR_DEL_ARGS:
			fputs("attempted to delete procedure arguments from the stack\n", dst);
			break;
		case BRB_ERR_MODULE_LOAD_INTERRUPT:
			fputs("loading the module was interrupted by user\n", dst);
			break;
		case BRB_ERR_UNKNOWN_DB:
			fprintf(dst, "unknown data block \"%s\"\n", err.name);
			break;
		case BRB_ERR_UNKNOWN_BUILTIN:
			fprintf(dst, "unknown built-in constant \"%s\"\n", err.name);
			break;
		case BRB_ERR_UNKNOWN_SYSCALL:
			fprintf(dst, "unknown syscall \"%s\"\n", err.name);
			break;
		case BRB_ERR_TOO_MANY_STRUCTS:
			fprintf(dst, "amount of declared structs exceeded the limit of %zu\n", MAX_N_STRUCTS);
			break;
		case BRB_ERR_STRUCT_NAME_EXPECTED:
			fputs("expected a word or a string as the struct name\n", dst);
			break;
		case BRB_ERR_UNKNOWN_STRUCT:
			fprintf(dst, "unknown struct \"%s\"\n", err.name);
			break;
		case BRB_ERR_RECURSIVE_TYPE:
			if (err.structs[err.struct_id].name) {
				fprintf(dst, "struct \"%s\" contains fields of the same type as itself\n", err.structs[err.struct_id].name);
			} else fprintf(dst, "struct $%u contains fields of the same type as itself\n", err.struct_id);
			break;
		case BRB_ERR_NO_STRUCT_DECL:
			fputs("unexpected end of input while loading struct declarations\n", dst);
			break;
		case BRB_ERR_NO_STRUCT_FIELD:
			fputs("unexpected end of input while loading structs' fields\n", dst);
			break;
		case BRB_ERR_STRUCT_ID_EXPECTED:
			fputs("an integer expected as the struct index after the `$` symbol\n", dst);
			break;
		case BRB_ERR_INVALID_STRUCT_ID:
			fprintf(dst, "number %llu is an invalid struct ID\n", err.operand);
			break;
		case BRB_ERR_INVALID_TYPE_KIND:
			fprintf(dst, "invalid type kind: %llu\n", err.operand);
			break;
		case BRB_ERR_OK:
		case BRB_N_ERROR_TYPES:
		default:
			fputs("undefined\n", dst);
	}
}

char* BRB_getErrorMsg(BRB_Error err, const char* prefix) {
	sbuf res = {0};
	FILE* stream = open_memstream(&res.data, &res.length);
	BRB_printErrorMsg(stream, err, prefix);
	fclose(stream);
	return res.data;
}

BRB_Error BRB_initModuleBuilder(BRB_ModuleBuilder* builder)
{
	*builder = (BRB_ModuleBuilder){0};
	builder->module.exec_entry_point = SIZE_MAX;
	return (BRB_Error){0};
}

BRB_Error BRB_analyzeModule(const BRB_Module* module, BRB_ModuleBuilder* dst)
{
	if (BRB_initModuleBuilder(dst).type) return dst->error;
	if (BRB_preallocStructs(dst, module->seg_typeinfo.length).type) return dst->error;
	if (BRB_preallocDataBlocks(dst, module->seg_data.length).type) return dst->error;
	if (BRB_preallocProcs(dst, module->seg_exec.length).type) return dst->error;
	arrayForeach (BRB_Struct, _struct, module->seg_typeinfo) {
		BRB_id _;
		if (BRB_addStruct(dst, &_, _struct->name, _struct->fields.length, _struct->fields.data).type) return dst->error;
	}
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		BRB_id _;
		if (BRB_addDataBlock(dst, &_, block->name, block->is_mutable, block->body.length).type) return dst->error;
	}
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		BRB_id _;
		if (BRB_addProc(dst, &_, proc->name, proc->args.length, proc->args.data, proc->ret_type, proc->body.length).type) return dst->error;
	}
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		arrayForeach (BRB_Op, op, block->body) {
			if (BRB_addOp(dst, ~(block - module->seg_data.data), *op).type) return dst->error;
		}
	}
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		arrayForeach (BRB_Op, op, proc->body) {
			if (BRB_addOp(dst, proc - module->seg_exec.data, *op).type) return dst->error;
		}
	}
	return BRB_setEntryPoint(dst, module->exec_entry_point);
}

BRB_Error BRB_extractModule(BRB_ModuleBuilder builder, BRB_Module* dst)
{
	if (builder.error.type) return builder.error;
	arrayForeach (BRB_StackNodeArray, proc_info, builder.procs) {
		BRB_StackNodeArray_clear(proc_info);
	}
	BRB_StackNodeArrayArray_clear(&builder.procs);
	arrayForeach (BRB_StackNodeArray, data_block_info, builder.data_blocks) {
		BRB_StackNodeArray_clear(data_block_info);
	}
	BRB_StackNodeArrayArray_clear(&builder.data_blocks);
	arena_free(&builder.arena);
	*dst = builder.module;
	return (BRB_Error){0};
}

BRB_Error BRB_setEntryPoint(BRB_ModuleBuilder* builder, size_t proc_id)
{
	if (builder->error.type) return builder->error;
	builder->module.exec_entry_point = proc_id;
	return (BRB_Error){0};
}

FILE* BRB_findModule(const char* name, const char* search_paths[])
{
	for (uint64_t i = 0; search_paths[i]; i += 1) {
		char path[MAXPATHLEN];
		snprintf(path, sizeof(path), "%s/%s.brb", search_paths[i], name);

		FILE* module_fd = fopen(path, "r");
		if (module_fd) return module_fd;
		errno = 0;
	}

	return NULL;
}

static BRB_StackNode getNthStackNode(BRB_StackNode head, size_t n)
{
	while (head && n--) head = head->prev;
	return head;
}

static BRB_StackNode getNthStackNode_nonInternal(BRB_StackNode head, size_t n)
{
	if (head && !n ? head->flags & BRB_SNF_STACKFRAME : false) {
		return head->prev;
	}
	while (head && n--) {
		if (head->flags & BRB_SNF_STACKFRAME) ++n;
		head = head->prev;
	}
	return head;
}

static size_t getStackLength(BRB_StackNode head)
{
	size_t res = 0;
	while (head) {
		head = head->prev;
		++res;
	}
	return res;
}

static bool compareTypes(BRB_Type field, BRB_Type entry)
{
	return entry.kind & field.kind
		&& (field.kind != BRB_TYPE_STRUCT || entry.struct_id == field.struct_id) && entry.n_items == field.n_items;
}

static BRB_Error validateType(BRB_Module* module, BRB_Type* type)
{
	if ((type->kind & (type->kind - 1)) != 0) // if `type->kind` is not a power of 2
		return (BRB_Error){.type = BRB_ERR_INVALID_TYPE_KIND, .operand = type->kind};
	if (type->kind == BRB_TYPE_STRUCT && type->struct_id >= module->seg_typeinfo.length)
		return (BRB_Error){.type = BRB_ERR_INVALID_STRUCT_ID, .operand = type->struct_id};
	return (BRB_Error){0};
}

static void __unused printStack(BRB_StackNode head)
{
	putchar('[');
	while (head) {
		BRB_printType(head->type, stdout);
		if (head->prev) putstr(", ");
		head = head->prev;
	}
	putchar(']');
}

static BRB_Error changeStack(BRB_StackNodeArray* stack, BRB_Op op, size_t n_in, BRB_Type* in_types, size_t n_out, BRB_Type* out_types, Arena* allocator)
{
	BRB_StackNode iter = *arrayhead(*stack);
	if (getStackLength(iter) < n_in)
		return (BRB_Error){
			.type = BRB_ERR_STACK_UNDERFLOW,
			.opcode = op.type,
			.expected_stack_length = n_in,
			.actual_stack_length = getStackLength(iter)
		};
	arrayForeach (BRB_Type, type, ((BRB_TypeArray){.data = in_types, .length = n_in})) {
		if (iter->flags & BRB_SNF_STACKFRAME)
			return (BRB_Error){.type = BRB_ERR_DEL_ARGS};
		if (!compareTypes(*type, iter->type))
			return (BRB_Error){
				.type = BRB_ERR_TYPE_MISMATCH,
				.opcode = op.type,
				.expected_type = *type,
				.actual_type = iter->type,
				.arg_id = type - in_types
			};
		iter = iter->prev;
	}

	BRB_StackNode node = getNthStackNode(*arrayhead(*stack), n_in), prev;
	arrayRevForeach (BRB_Type, type, ((BRB_TypeArray){.data = out_types, .length = n_out})) {
		prev = node;
		if (!(node = arena_alloc(allocator, sizeof(struct BRB_stacknode_t)))) return (BRB_Error){.type = BRB_ERR_NO_MEMORY};
		*node = (struct BRB_stacknode_t){0};
		node->prev = prev;
		node->type = type->kind ? *type : type->ctor(op, *arrayhead(*stack));
	}
	return (BRB_Error){.type = BRB_StackNodeArray_append(stack, node) ? 0 : BRB_ERR_NO_MEMORY};
}

BRB_Error BRB_addProc(BRB_ModuleBuilder* builder, BRB_id* proc_id_p, const char* name, size_t n_args, BRB_Type* args, BRB_Type ret_type, uint32_t n_ops_hint)
{
	if (builder->error.type) return builder->error;
	BRB_Error err;
// validating the return type
	if ((err = validateType(&builder->module, &ret_type)).type) return err;
	if (!BRB_ProcArray_append(&builder->module.seg_exec, (BRB_Proc){.name = name, .ret_type = ret_type})
		|| !BRB_StackNodeArrayArray_append(&builder->procs, (BRB_StackNodeArray){0}))
		return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
// copying the argumants
	if (!(arrayhead(builder->module.seg_exec)->args = BRB_TypeArray_copy((BRB_TypeArray){ .data = args, .length = n_args })).data)
		return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
// validating the arguments
	arrayForeach (BRB_Type, arg, arrayhead(builder->module.seg_exec)->args) {
		if ((err = validateType(&builder->module, arg)).type) return err;
	}
// pre-allocating the stack trace and the procedure body
	if (n_ops_hint)
		if (!(arrayhead(builder->module.seg_exec)->body = BRB_OpArray_new(-(int32_t)n_ops_hint)).data
			|| !(*arrayhead(builder->procs) = BRB_StackNodeArray_new(-(int32_t)n_ops_hint)).data)
			return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
// initializing the stack trace
	BRB_StackNode input = NULL, prev;
 	while (n_args--) {
		prev = input;
		if (!(input = arena_alloc(&builder->arena, sizeof(struct BRB_stacknode_t))))
			return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
		input->prev = prev;
		input->type = args[n_args];
		input->name = NULL;
		input->flags = BRB_SNF_STACKFRAME;
	}
	prev = input;
	if (!(input = arena_alloc(&builder->arena, sizeof(struct BRB_stacknode_t))))
		return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
	input->prev = prev;
	input->type = BRB_PTR_TYPE(2);
	input->name = NULL;
	input->flags = BRB_SNF_STACKFRAME;
	*proc_id_p = builder->module.seg_exec.length - 1;
	return builder->error = (BRB_Error){.type = BRB_StackNodeArray_append(arrayhead(builder->procs), input) ? 0 : BRB_ERR_NO_MEMORY};
}

BRB_Type _tctor_inputType(BRB_Op op, BRB_StackNode stack) {
	return op.operand_type;
}
BRB_Type _tctor_stackHeadType(BRB_Op op, BRB_StackNode stack) {
	return stack->type;
}

BRB_Type _tctor_typeOfInputStackItem(BRB_Op op, BRB_StackNode stack) {
	return getNthStackNode_nonInternal(stack, op.operand_u)->type;
}

BRB_Type _tctor_2ndStackItemType(BRB_Op op, BRB_StackNode stack) {
	return stack->prev->type;
}

BRB_Error BRB_addOp(BRB_ModuleBuilder* builder, BRB_id proc_id, BRB_Op op)
{
	if (builder->error.type) return builder->error;
	static uint8_t n_in[] = {
		[BRB_OP_NOP]       = 0,
		[BRB_OP_END]       = 0,
		[BRB_OP_I8]        = 0,
		[BRB_OP_I16]       = 0,
		[BRB_OP_I32]       = 0,
		[BRB_OP_PTR]       = 0,
		[BRB_OP_I64]       = 0,
		[BRB_OP_ADDR]      = 0,
		[BRB_OP_DBADDR]    = 0,
		[BRB_OP_LD]        = 1,
		[BRB_OP_STR]       = 2,
		[BRB_OP_SYS]       = 0, // needs to be set manually every time, depending on the system function in question
		[BRB_OP_BUILTIN]   = 0,
		[BRB_OP_ADD]       = 2,
		[BRB_OP_ADDI]      = 1,
		[BRB_OP_ADDIAT8]   = 1,
		[BRB_OP_ADDIAT16]  = 1,
		[BRB_OP_ADDIAT32]  = 1,
		[BRB_OP_ADDIATP]   = 1,
		[BRB_OP_ADDIAT64]  = 1,
		[BRB_OP_SUB]       = 2,
		[BRB_OP_SUBI]      = 1,
		[BRB_OP_SUBIAT8]   = 1,
		[BRB_OP_SUBIAT16]  = 1,
		[BRB_OP_SUBIAT32]  = 1,
		[BRB_OP_SUBIATP]   = 1,
		[BRB_OP_SUBIAT64]  = 1,
		[BRB_OP_MUL]       = 2,
		[BRB_OP_MULI]      = 1,
		[BRB_OP_MULIAT8]   = 1,
		[BRB_OP_MULIAT16]  = 1,
		[BRB_OP_MULIAT32]  = 1,
		[BRB_OP_MULIATP]   = 1,
		[BRB_OP_MULIAT64]  = 1,
		[BRB_OP_DIV]       = 2,
		[BRB_OP_DIVI]      = 1,
		[BRB_OP_DIVIAT8]   = 1,
		[BRB_OP_DIVIAT16]  = 1,
		[BRB_OP_DIVIAT32]  = 1,
		[BRB_OP_DIVIATP]   = 1,
		[BRB_OP_DIVIAT64]  = 1,
		[BRB_OP_DIVS]      = 2,
		[BRB_OP_DIVSI]     = 1,
		[BRB_OP_DIVSIAT8]  = 1,
		[BRB_OP_DIVSIAT16] = 1,
		[BRB_OP_DIVSIAT32] = 1,
		[BRB_OP_DIVSIATP]  = 1,
		[BRB_OP_DIVSIAT64] = 1,
		[BRB_OP_MOD]       = 2,
		[BRB_OP_MODI]      = 1,
		[BRB_OP_MODIAT8]   = 1,
		[BRB_OP_MODIAT16]  = 1,
		[BRB_OP_MODIAT32]  = 1,
		[BRB_OP_MODIATP]   = 1,
		[BRB_OP_MODIAT64]  = 1,
		[BRB_OP_MODS]      = 2,
		[BRB_OP_MODSI]     = 1,
		[BRB_OP_MODSIAT8]  = 1,
		[BRB_OP_MODSIAT16] = 1,
		[BRB_OP_MODSIAT32] = 1,
		[BRB_OP_MODSIATP]  = 1,
		[BRB_OP_MODSIAT64] = 1,
		[BRB_OP_AND]       = 2,
		[BRB_OP_ANDI]      = 1,
		[BRB_OP_ANDIAT8]   = 1,
		[BRB_OP_ANDIAT16]  = 1,
		[BRB_OP_ANDIAT32]  = 1,
		[BRB_OP_ANDIATP]   = 1,
		[BRB_OP_ANDIAT64]  = 1,
		[BRB_OP_OR]        = 2,
		[BRB_OP_ORI]       = 1,
		[BRB_OP_ORIAT8]    = 1,
		[BRB_OP_ORIAT16]   = 1,
		[BRB_OP_ORIAT32]   = 1,
		[BRB_OP_ORIATP]    = 1,
		[BRB_OP_ORIAT64]   = 1,
		[BRB_OP_XOR]       = 2,
		[BRB_OP_XORI]      = 1,
		[BRB_OP_XORIAT8]   = 1,
		[BRB_OP_XORIAT16]  = 1,
		[BRB_OP_XORIAT32]  = 1,
		[BRB_OP_XORIATP]   = 1,
		[BRB_OP_XORIAT64]  = 1,
		[BRB_OP_SHL]       = 2,
		[BRB_OP_SHLI]      = 1,
		[BRB_OP_SHLIAT8]   = 1,
		[BRB_OP_SHLIAT16]  = 1,
		[BRB_OP_SHLIAT32]  = 1,
		[BRB_OP_SHLIATP]   = 1,
		[BRB_OP_SHLIAT64]  = 1,
		[BRB_OP_SHR]       = 2,
		[BRB_OP_SHRI]      = 1,
		[BRB_OP_SHRIAT8]   = 1,
		[BRB_OP_SHRIAT16]  = 1,
		[BRB_OP_SHRIAT32]  = 1,
		[BRB_OP_SHRIATP]   = 1,
		[BRB_OP_SHRIAT64]  = 1,
		[BRB_OP_SHRS]      = 2,
		[BRB_OP_SHRSI]     = 1,
		[BRB_OP_SHRSIAT8]  = 1,
		[BRB_OP_SHRSIAT16] = 1,
		[BRB_OP_SHRSIAT32] = 1,
		[BRB_OP_SHRSIATP]  = 1,
		[BRB_OP_SHRSIAT64] = 1,
		[BRB_OP_NOT]       = 1,
		[BRB_OP_NOTAT8]    = 1,
		[BRB_OP_NOTAT16]   = 1,
		[BRB_OP_NOTAT32]   = 1,
		[BRB_OP_NOTATP]    = 1,
		[BRB_OP_NOTAT64]   = 1,
		[BRB_OP_DROP]      = 1,
		[BRB_OP_NEW]       = 0,
		[BRB_OP_ZERO]      = 0,
		[BRB_OP_COPY]      = 0,
		[BRB_OP_COPYTO]    = 2
	};
	static BRB_Type in_types[][3] = {
		[BRB_OP_NOP]       = { BRB_VOID_TYPE },
		[BRB_OP_END]       = { BRB_VOID_TYPE },
		[BRB_OP_I8]        = { BRB_VOID_TYPE },
		[BRB_OP_I16]       = { BRB_VOID_TYPE },
		[BRB_OP_I32]       = { BRB_VOID_TYPE },
		[BRB_OP_PTR]       = { BRB_VOID_TYPE },
		[BRB_OP_I64]       = { BRB_VOID_TYPE },
		[BRB_OP_ADDR]      = { BRB_VOID_TYPE },
		[BRB_OP_DBADDR]    = { BRB_VOID_TYPE },
		[BRB_OP_LD]        = { BRB_PTR_TYPE(1) },
		[BRB_OP_STR]       = { BRB_PTR_TYPE(1), BRB_ANY_TYPE(1) },
		[BRB_OP_SYS]       = { BRB_VOID_TYPE }, // needs to be set manually every time, depending on the system function in question
		[BRB_OP_BUILTIN]   = { BRB_VOID_TYPE },
		[BRB_OP_ADD]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_ADDI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_ADDIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUB]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_SUBI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_SUBIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MUL]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_MULI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_MULIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIV]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_DIVI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_DIVIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVS]      = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_DIVSI]     = { BRB_INT_TYPE(1) },
		[BRB_OP_DIVSIAT8]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIAT16] = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIAT32] = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIAT64] = { BRB_PTR_TYPE(1) },
		[BRB_OP_MOD]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_MODI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_MODIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODS]      = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_MODSI]     = { BRB_INT_TYPE(1) },
		[BRB_OP_MODSIAT8]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIAT16] = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIAT32] = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIAT64] = { BRB_PTR_TYPE(1) },
		[BRB_OP_AND]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_ANDI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_ANDIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ANDIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_ANDIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_ANDIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ANDIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_OR]        = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_ORI]       = { BRB_INT_TYPE(1) },
		[BRB_OP_ORIAT8]    = { BRB_PTR_TYPE(1) },
		[BRB_OP_ORIAT16]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ORIAT32]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ORIATP]    = { BRB_PTR_TYPE(1) },
		[BRB_OP_ORIAT64]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_XOR]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_XORI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_XORIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_XORIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_XORIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_XORIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_XORIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHL]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_SHLI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_SHLIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHLIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHLIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHLIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHLIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHR]       = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_SHRI]      = { BRB_INT_TYPE(1) },
		[BRB_OP_SHRIAT8]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRIAT16]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRIAT32]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRIAT64]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRS]      = { BRB_INT_TYPE(1), BRB_INT_TYPE(1) },
		[BRB_OP_SHRSI]     = { BRB_INT_TYPE(1) },
		[BRB_OP_SHRSIAT8]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRSIAT16] = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRSIAT32] = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRSIAT64] = { BRB_PTR_TYPE(1) },
		[BRB_OP_NOT]       = { BRB_INT_TYPE(1) },
		[BRB_OP_NOTAT8]    = { BRB_PTR_TYPE(1) },
		[BRB_OP_NOTAT16]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_NOTAT32]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_NOTATP]    = { BRB_PTR_TYPE(1) },
		[BRB_OP_NOTAT64]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_DROP]      = { BRB_ANY_TYPE(1) },
		[BRB_OP_NEW]       = { BRB_VOID_TYPE },
		[BRB_OP_ZERO]      = { BRB_VOID_TYPE },
		[BRB_OP_COPY]      = { BRB_VOID_TYPE },
		[BRB_OP_COPYTO]    = { BRB_PTR_TYPE(1), BRB_ANY_TYPE(1) }
	};
	static_assert(BRB_N_OPS == 115, "not all BRB operations have their input types defined");
	static uint8_t n_out[] = {
		[BRB_OP_NOP]       = 0,
		[BRB_OP_END]       = 0,
		[BRB_OP_I8]        = 1,
		[BRB_OP_I16]       = 1,
		[BRB_OP_I32]       = 1,
		[BRB_OP_PTR]       = 1,
		[BRB_OP_I64]       = 1,
		[BRB_OP_ADDR]      = 1,
		[BRB_OP_DBADDR]    = 1,
		[BRB_OP_LD]        = 1,
		[BRB_OP_STR]       = 0,
		[BRB_OP_SYS]       = 1,
		[BRB_OP_BUILTIN]   = 1,
		[BRB_OP_ADD]       = 1,
		[BRB_OP_ADDI]      = 1,
		[BRB_OP_ADDIAT8]   = 1,
		[BRB_OP_ADDIAT16]  = 1,
		[BRB_OP_ADDIAT32]  = 1,
		[BRB_OP_ADDIATP]   = 1,
		[BRB_OP_ADDIAT64]  = 1,
		[BRB_OP_SUB]       = 1,
		[BRB_OP_SUBI]      = 1,
		[BRB_OP_SUBIAT8]   = 1,
		[BRB_OP_SUBIAT16]  = 1,
		[BRB_OP_SUBIAT32]  = 1,
		[BRB_OP_SUBIATP]   = 1,
		[BRB_OP_SUBIAT64]  = 1,
		[BRB_OP_MUL]       = 1,
		[BRB_OP_MULI]      = 1,
		[BRB_OP_MULIAT8]   = 1,
		[BRB_OP_MULIAT16]  = 1,
		[BRB_OP_MULIAT32]  = 1,
		[BRB_OP_MULIATP]   = 1,
		[BRB_OP_MULIAT64]  = 1,
		[BRB_OP_DIV]       = 1,
		[BRB_OP_DIVI]      = 1,
		[BRB_OP_DIVIAT8]   = 1,
		[BRB_OP_DIVIAT16]  = 1,
		[BRB_OP_DIVIAT32]  = 1,
		[BRB_OP_DIVIATP]   = 1,
		[BRB_OP_DIVIAT64]  = 1,
		[BRB_OP_DIVS]      = 1,
		[BRB_OP_DIVSI]     = 1,
		[BRB_OP_DIVSIAT8]  = 1,
		[BRB_OP_DIVSIAT16] = 1,
		[BRB_OP_DIVSIAT32] = 1,
		[BRB_OP_DIVSIATP]  = 1,
		[BRB_OP_DIVSIAT64] = 1,
		[BRB_OP_MOD]       = 1,
		[BRB_OP_MODI]      = 1,
		[BRB_OP_MODIAT8]   = 1,
		[BRB_OP_MODIAT16]  = 1,
		[BRB_OP_MODIAT32]  = 1,
		[BRB_OP_MODIATP]   = 1,
		[BRB_OP_MODIAT64]  = 1,
		[BRB_OP_MODS]      = 1,
		[BRB_OP_MODSI]     = 1,
		[BRB_OP_MODSIAT8]  = 1,
		[BRB_OP_MODSIAT16] = 1,
		[BRB_OP_MODSIAT32] = 1,
		[BRB_OP_MODSIATP]  = 1,
		[BRB_OP_MODSIAT64] = 1,
		[BRB_OP_AND]       = 1,
		[BRB_OP_ANDI]      = 1,
		[BRB_OP_ANDIAT8]   = 1,
		[BRB_OP_ANDIAT16]  = 1,
		[BRB_OP_ANDIAT32]  = 1,
		[BRB_OP_ANDIATP]   = 1,
		[BRB_OP_ANDIAT64]  = 1,
		[BRB_OP_OR]        = 1,
		[BRB_OP_ORI]       = 1,
		[BRB_OP_ORIAT8]    = 1,
		[BRB_OP_ORIAT16]   = 1,
		[BRB_OP_ORIAT32]   = 1,
		[BRB_OP_ORIATP]    = 1,
		[BRB_OP_ORIAT64]   = 1,
		[BRB_OP_XOR]       = 1,
		[BRB_OP_XORI]      = 1,
		[BRB_OP_XORIAT8]   = 1,
		[BRB_OP_XORIAT16]  = 1,
		[BRB_OP_XORIAT32]  = 1,
		[BRB_OP_XORIATP]   = 1,
		[BRB_OP_XORIAT64]  = 1,
		[BRB_OP_SHL]       = 1,
		[BRB_OP_SHLI]      = 1,
		[BRB_OP_SHLIAT8]   = 1,
		[BRB_OP_SHLIAT16]  = 1,
		[BRB_OP_SHLIAT32]  = 1,
		[BRB_OP_SHLIATP]   = 1,
		[BRB_OP_SHLIAT64]  = 1,
		[BRB_OP_SHR]       = 1,
		[BRB_OP_SHRI]      = 1,
		[BRB_OP_SHRIAT8]   = 1,
		[BRB_OP_SHRIAT16]  = 1,
		[BRB_OP_SHRIAT32]  = 1,
		[BRB_OP_SHRIATP]   = 1,
		[BRB_OP_SHRIAT64]  = 1,
		[BRB_OP_SHRS]      = 1,
		[BRB_OP_SHRSI]     = 1,
		[BRB_OP_SHRSIAT8]  = 1,
		[BRB_OP_SHRSIAT16] = 1,
		[BRB_OP_SHRSIAT32] = 1,
		[BRB_OP_SHRSIATP]  = 1,
		[BRB_OP_SHRSIAT64] = 1,
		[BRB_OP_NOT]       = 1,
		[BRB_OP_NOTAT8]    = 1,
		[BRB_OP_NOTAT16]   = 1,
		[BRB_OP_NOTAT32]   = 1,
		[BRB_OP_NOTATP]    = 1,
		[BRB_OP_NOTAT64]   = 1,
		[BRB_OP_DROP]      = 0,
		[BRB_OP_NEW]       = 1,
		[BRB_OP_ZERO]      = 1,
		[BRB_OP_COPY]      = 1,
		[BRB_OP_COPYTO]    = 1
	};
	static BRB_Type out_types[][1] = {
		[BRB_OP_NOP]       = { BRB_VOID_TYPE },
		[BRB_OP_END]       = { BRB_VOID_TYPE },
		[BRB_OP_I8]        = { BRB_I8_TYPE(1) },
		[BRB_OP_I16]       = { BRB_I16_TYPE(1) },
		[BRB_OP_I32]       = { BRB_I32_TYPE(1) },
		[BRB_OP_PTR]       = { BRB_PTR_TYPE(1) },
		[BRB_OP_I64]       = { BRB_I64_TYPE(1) },
		[BRB_OP_ADDR]      = { BRB_PTR_TYPE(1) },
		[BRB_OP_DBADDR]    = { BRB_PTR_TYPE(1) },
		[BRB_OP_LD]        = { BRB_DYN_TYPE(_tctor_inputType) },
		[BRB_OP_STR]       = { BRB_VOID_TYPE },
		[BRB_OP_SYS]       = { BRB_PTR_TYPE(1) },
		[BRB_OP_BUILTIN]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADD]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_ADDI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_ADDIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_ADDIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_ADDIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_ADDIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ADDIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_SUB]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_SUBI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_SUBIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_SUBIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_SUBIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_SUBIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SUBIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_MUL]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_MULI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_MULIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_MULIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_MULIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_MULIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MULIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_DIV]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_DIVI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_DIVIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_DIVIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_DIVIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_DIVIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_DIVS]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_DIVSI]     = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_DIVSIAT8]  = { BRB_I8_TYPE(1) },
		[BRB_OP_DIVSIAT16] = { BRB_I16_TYPE(1) },
		[BRB_OP_DIVSIAT32] = { BRB_I32_TYPE(1) },
		[BRB_OP_DIVSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_DIVSIAT64] = { BRB_I64_TYPE(1) },
		[BRB_OP_MOD]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_MODI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_MODIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_MODIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_MODIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_MODIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_MODS]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_MODSI]     = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_MODSIAT8]  = { BRB_I8_TYPE(1) },
		[BRB_OP_MODSIAT16] = { BRB_I16_TYPE(1) },
		[BRB_OP_MODSIAT32] = { BRB_I32_TYPE(1) },
		[BRB_OP_MODSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_MODSIAT64] = { BRB_I64_TYPE(1) },
		[BRB_OP_AND]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_ANDI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_ANDIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_ANDIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_ANDIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_ANDIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_ANDIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_OR]        = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_ORI]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_ORIAT8]    = { BRB_I8_TYPE(1) },
		[BRB_OP_ORIAT16]   = { BRB_I16_TYPE(1) },
		[BRB_OP_ORIAT32]   = { BRB_I32_TYPE(1) },
		[BRB_OP_ORIATP]    = { BRB_PTR_TYPE(1) },
		[BRB_OP_ORIAT64]   = { BRB_I64_TYPE(1) },
		[BRB_OP_XOR]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_XORI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_XORIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_XORIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_XORIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_XORIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_XORIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_SHL]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_SHLI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_SHLIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_SHLIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_SHLIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_SHLIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHLIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_SHR]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_SHRI]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_SHRIAT8]   = { BRB_I8_TYPE(1) },
		[BRB_OP_SHRIAT16]  = { BRB_I16_TYPE(1) },
		[BRB_OP_SHRIAT32]  = { BRB_I32_TYPE(1) },
		[BRB_OP_SHRIATP]   = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRIAT64]  = { BRB_I64_TYPE(1) },
		[BRB_OP_SHRS]      = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_SHRSI]     = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_SHRSIAT8]  = { BRB_I8_TYPE(1) },
		[BRB_OP_SHRSIAT16] = { BRB_I16_TYPE(1) },
		[BRB_OP_SHRSIAT32] = { BRB_I32_TYPE(1) },
		[BRB_OP_SHRSIATP]  = { BRB_PTR_TYPE(1) },
		[BRB_OP_SHRSIAT64] = { BRB_I64_TYPE(1) },
		[BRB_OP_NOT]       = { BRB_DYN_TYPE(_tctor_stackHeadType) },
		[BRB_OP_NOTAT8]    = { BRB_I8_TYPE(1) },
		[BRB_OP_NOTAT16]   = { BRB_I16_TYPE(1) },
		[BRB_OP_NOTAT32]   = { BRB_I32_TYPE(1) },
		[BRB_OP_NOTATP]    = { BRB_PTR_TYPE(1) },
		[BRB_OP_NOTAT64]   = { BRB_I64_TYPE(1) },
		[BRB_OP_DROP]      = { BRB_VOID_TYPE },
		[BRB_OP_NEW]       = { BRB_DYN_TYPE(_tctor_inputType) },
		[BRB_OP_ZERO]      = { BRB_DYN_TYPE(_tctor_inputType) },
		[BRB_OP_COPY]      = { BRB_DYN_TYPE(_tctor_typeOfInputStackItem) },
		[BRB_OP_COPYTO]    = { BRB_DYN_TYPE(_tctor_2ndStackItemType) }
	};
	static_assert(BRB_N_OPS == 115, "not all BRB operations have their output types defined");
	static uint8_t sys_n_in[] = {
		[BRB_SYS_EXIT] = 1,
		[BRB_SYS_WRITE] = 3,
		[BRB_SYS_READ] = 3
	};
	static BRB_Type sys_in_types[][3] = {
		[BRB_SYS_EXIT] = { BRB_PTR_TYPE(1) },
		[BRB_SYS_WRITE] = { BRB_PTR_TYPE(1), BRB_PTR_TYPE(1), BRB_PTR_TYPE(1) },
		[BRB_SYS_READ] = { BRB_PTR_TYPE(1), BRB_PTR_TYPE(1), BRB_PTR_TYPE(1) }
	};
	static_assert(BRB_N_SYSCALLS == 3, "not all BRB syscalls have their input types defined");
// resolving where to add the operation
	if (builder->error.type) return builder->error;
	BRB_OpArray* body;
	BRB_StackNodeArray* vframe;
	if (proc_id < 0) {
		body = &builder->module.seg_data.data[~proc_id].body;
		vframe = &builder->data_blocks.data[~proc_id];
	} else {
		body = &builder->module.seg_exec.data[proc_id].body;
		vframe = &builder->procs.data[proc_id];
	}
// validating the type operand, if there is one
	if (BRB_GET_OPERAND_TYPE(op.type) == BRB_OPERAND_TYPE)
		if ((builder->error = validateType(&builder->module, &op.operand_type)).type) return builder->error;
// adding the operation
	if (!BRB_OpArray_append(body, op))
		return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
// checking the edge cases
	if (BRB_GET_OPERAND_TYPE(op.type) == BRB_OPERAND_VAR_NAME) {
		if (op.operand_u >= UINT32_MAX) {
			--body->length;
			return builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u};
		}
		if (op.operand_u ? !getNthStackNode_nonInternal(*arrayhead(*vframe), op.operand_u) : false) {
			--body->length;
			return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u});
		}
	} else if (BRB_GET_OPERAND_TYPE(op.type) == BRB_OPERAND_DB_NAME) {
		if (~op.operand_s >= UINT32_MAX) {
			--body->length;
			return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u});
		}
		if (~op.operand_s >= builder->module.seg_data.length) {
			--body->length;
			return (builder->error = (BRB_Error){.type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u});
		}
	} else if (op.type == BRB_OP_SYS) {
		if (op.operand_u >= BRB_N_SYSCALLS) {
			--body->length;
			return (builder->error = (BRB_Error){ .type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = BRB_OP_SYS, .operand = op.operand_u });
		}
		n_in[BRB_OP_SYS] = sys_n_in[op.operand_u];
		memcpy(in_types[BRB_OP_SYS], sys_in_types[op.operand_u], sizeof(in_types[BRB_OP_SYS]));
	} else if (op.type == BRB_OP_BUILTIN && op.operand_u >= BRB_N_BUILTINS) {
		--body->length;
		return builder->error = (BRB_Error){ .type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = BRB_OP_BUILTIN, .operand = op.operand_u };
	} else if (
			(BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_DIV
			 || BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_DIVS
			 || BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_MOD
			 || BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_MODS)
			&& BRB_GET_OPERAND_TYPE(op.type) == BRB_OPERAND_INT
			&& op.operand_u == 0
		  ) {
		--body->length;
		return builder->error = (BRB_Error){ .type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = 0 };
	} else if (
			(BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_SHL
			 || BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_SHR
			 || BRB_GET_BASE_OP_TYPE(op.type) == BRB_OP_SHRS)
			&& BRB_GET_OPERAND_TYPE(op.type) == BRB_OPERAND_INT8
			&& op.operand_u >= 64
		  ) {
		--body->length;
		return builder->error = (BRB_Error){ .type = BRB_ERR_OPERAND_OUT_OF_RANGE, .opcode = op.type, .operand = op.operand_u };
	} else if (op.type >= BRB_N_OPS) {
		--body->length;
		return builder->error = (BRB_Error){.type = BRB_ERR_INVALID_OPCODE, .opcode = op.type};
	}
// registering the changes in the stack
	if ((builder->error = changeStack(
		vframe,
		op,
		n_in[op.type], in_types[op.type],
		n_out[op.type], out_types[op.type],
		&builder->arena
	)).type) {
		--body->length;
		return builder->error;
	}
	return (BRB_Error){0};
}

BRB_Op* BRB_getOp(BRB_Module* module, BRB_id proc_id, uint32_t op_id) {
	return &(proc_id < 0 ? module->seg_data.data[~proc_id].body : module->seg_exec.data[proc_id].body).data[op_id];
}

BRB_Error BRB_labelStackItem(BRB_ModuleBuilder* builder, BRB_id proc_id, uint32_t op_id, uint32_t item_id, const char* name)
{
	if (builder->error.type) return builder->error;
	BRB_StackNode target = getNthStackNode(proc_id < 0 ? builder->data_blocks.data[~proc_id].data[op_id] : builder->procs.data[proc_id].data[op_id], item_id);
	if (!target) return (builder->error = (BRB_Error){
		.type = BRB_ERR_STACK_UNDERFLOW,
		.opcode = BRB_N_OPS,
		.expected_stack_length = 1,
		.actual_stack_length = 0
	});
	target->name = name;
	return (BRB_Error){0};
}

BRB_Error BRB_addDataBlock(BRB_ModuleBuilder* builder, BRB_id* db_id_p, const char* name, bool is_mutable, uint32_t n_ops_hint)
{
	if (builder->error.type) return builder->error;
	if (!BRB_DataBlockArray_append(&builder->module.seg_data, (BRB_DataBlock){ .name = name, .is_mutable = is_mutable })
		|| !BRB_StackNodeArrayArray_append(&builder->data_blocks, (BRB_StackNodeArray){0}))
		return (builder->error = (BRB_Error){ .type = BRB_ERR_NO_MEMORY });

	if (!(arrayhead(builder->module.seg_data)->body = BRB_OpArray_new(-(int32_t)n_ops_hint)).data)
		return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});

	if (!BRB_StackNodeArray_append(arrayhead(builder->data_blocks), NULL))
		return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});

	*db_id_p = ~((size_t)builder->module.seg_data.length - 1);
	return (BRB_Error){0};
}

BRB_id BRB_getDataBlockIdByName(BRB_Module* module, const char* name)
{
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		if (streq(block->name, name)) return ~(block - module->seg_data.data);
	}
	return BRB_INVALID_ID;
}

static size_t getTypeAlignment(BRB_Module* module, BRB_Type type)
{
	if (type.kind == BRB_TYPE_STRUCT) {
		if (type.struct_id >= module->seg_typeinfo.length) return SIZE_MAX;
		BRB_Struct* obj = &module->seg_typeinfo.data[type.struct_id];
		return obj->alignment ? obj->alignment : SIZE_MAX;
	}
	static size_t values[] = {
		[BRB_TYPE_I8] = 1,
		[BRB_TYPE_I16] = 2,
		[BRB_TYPE_I32] = 4,
		[BRB_TYPE_PTR] = sizeof(void*),
		[BRB_TYPE_I64] = 8,
		[BRB_TYPE_VOID] = 0,
	};
	return values[type.kind];
}

size_t BRB_getTypeRTSize(BRB_Module* module, BRB_Type type)
{
	if (type.kind == BRB_TYPE_STRUCT)
		return type.struct_id >= module->seg_typeinfo.length
			? SIZE_MAX
			: module->seg_typeinfo.data[type.struct_id].size;
	assert(!(type.kind & (type.kind - 1)), "invalid type kind %i", type.kind);
	static size_t coeffs[] = {
		[BRB_TYPE_I8] = 1,
		[BRB_TYPE_I16] = 2,
		[BRB_TYPE_I32] = 4,
		[BRB_TYPE_PTR] = sizeof(void*),
		[BRB_TYPE_I64] = 8,
		[BRB_TYPE_VOID] = 0,
	};
	return type.n_items * coeffs[type.kind];
}

size_t BRB_getStackItemRTOffset(BRB_ModuleBuilder* builder, BRB_id proc_id, uint32_t op_id, size_t item_id)
{
	const bool strict = item_id != SIZE_MAX;
	BRB_StackNodeArray* vframe;
	if (proc_id < 0) {
		if (~proc_id >= builder->data_blocks.length) return SIZE_MAX;
		vframe = &builder->data_blocks.data[~proc_id];
	} else {
		if (proc_id >= builder->procs.length) return SIZE_MAX;
		vframe = &builder->procs.data[proc_id];
	}
	if (++op_id >= vframe->length) return SIZE_MAX;
	// `++op_id` because the first element in the state stack is always the initial state, determined by proc args
	size_t res = 0;
	BRB_StackNode node = vframe->data[op_id];
	while (node ? node->flags & BRB_SNF_STACKFRAME : false) node = node->prev; // setting up the iterator
	while (node && item_id) {
		size_t node_size = BRB_getTypeRTSize(&builder->module, node->type);
		if (node_size == SIZE_MAX) return SIZE_MAX;
		res += node_size;
		if (!(node->flags & BRB_SNF_STACKFRAME)) --item_id;
		node = node->prev;
	}
	return item_id && strict ? SIZE_MAX : res;
}

size_t BRB_getStackRTSize(BRB_ModuleBuilder* builder, BRB_id proc_id, uint32_t op_id)
{
	return BRB_getStackItemRTOffset(builder, proc_id, op_id, SIZE_MAX);
}

size_t BRB_getMaxStackRTSize(BRB_ModuleBuilder* builder, BRB_id proc_id)
{
	size_t res = 0, i = -1;
	while (true) {
		size_t cur_size = BRB_getStackRTSize(builder, proc_id, i++);
		if (cur_size == SIZE_MAX) break;
		if (cur_size > res) res = cur_size;
	}
	return res;
}

bool BRB_getStackItemType(BRB_ModuleBuilder* builder, BRB_Type* dst, BRB_id proc_id, uint32_t op_id, uint32_t item_id)
{
	BRB_StackNodeArray* vframe;
	if (proc_id < 0) {
		if (~proc_id >= builder->data_blocks.length) return false;
		vframe = &builder->data_blocks.data[~proc_id];
	} else {
		if (proc_id >= builder->procs.length) return false;
		vframe = &builder->procs.data[proc_id];
	}
	if (++op_id >= vframe->length) return false;
	// `++op_id` because the first element in the state stack is always the initial state, determined by proc args
	BRB_StackNode node = vframe->data[op_id];
	while (node && item_id--) node = node->prev;
	if (!node) return false;
	*dst = node->type;
	return true;
}

size_t BRB_getStackItemRTSize(BRB_ModuleBuilder* builder, BRB_id proc_id, uint32_t op_id, uint32_t item_id)
{
	BRB_Type res;
	if (!BRB_getStackItemType(builder, &res, proc_id, op_id, item_id)) return SIZE_MAX;
	return BRB_getTypeRTSize(&builder->module, res);
}

BRB_id BRB_getProcIdByName(BRB_Module* module, const char* name)
{
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		if (streq(name, proc->name)) return proc - module->seg_exec.data;
	}
	return BRB_INVALID_ID;
}

size_t BRB_getStackItemIdByName(BRB_ModuleBuilder* builder, BRB_id proc_id, uint32_t op_id, const char* name)
{
	BRB_StackNodeArray* vframe;
	if (proc_id < 0) {
		if (~proc_id >= builder->data_blocks.length) return SIZE_MAX;
		vframe = &builder->data_blocks.data[~proc_id];
	} else {
		if (proc_id >= builder->procs.length) return SIZE_MAX;
		vframe = &builder->procs.data[proc_id];
	}

	if (++op_id >= vframe->length) return SIZE_MAX;
	size_t res = 0;
	for (BRB_StackNode node = vframe->data[op_id]; node; node = node->prev) {
		if (node->name ? streq(name, node->name) : false) return res;
		++res;
	}
	return SIZE_MAX;
}

BRB_Error BRB_preallocProcs(BRB_ModuleBuilder* builder, uint32_t n_procs_hint)
{
	if (builder->error.type) return builder->error;
	if (!(builder->procs = BRB_StackNodeArrayArray_new(-(int64_t)n_procs_hint)).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	if (!(builder->module.seg_exec = BRB_ProcArray_new(-(int64_t)n_procs_hint)).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	return (BRB_Error){0};
}

BRB_Error BRB_preallocDataBlocks(BRB_ModuleBuilder* builder, uint32_t n_dbs_hint)
{
	if (builder->error.type) return builder->error;
	if (!(builder->data_blocks = BRB_StackNodeArrayArray_new(-(int64_t)n_dbs_hint)).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	if (!(builder->module.seg_data = BRB_DataBlockArray_new(-(int64_t)n_dbs_hint)).data) return (builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY});
	return (BRB_Error){0};
}

void BRB_deallocProcs(BRB_Module* module)
{
	arrayForeach (BRB_Proc, proc, module->seg_exec) {
		arrayForeach (BRB_Op, op, proc->body) {
			if (BRB_opFlags[op->type] & BRB_OPERAND_ALLOCATED)
				free(op->operand_ptr);
		}
		BRB_TypeArray_clear(&proc->args);
		BRB_OpArray_clear(&proc->body);
	}
	BRB_ProcArray_clear(&module->seg_exec);
}

void BRB_deallocDataBlocks(BRB_Module* module)
{
	arrayForeach (BRB_DataBlock, block, module->seg_data) {
		arrayForeach (BRB_Op, op, block->body) {
			if (BRB_opFlags[op->type] & BRB_OPERAND_ALLOCATED)
				free(op->operand_ptr);
		}
		BRB_OpArray_clear(&block->body);
	}
	BRB_DataBlockArray_clear(&module->seg_data);
}

BRB_Error BRB_addStruct(BRB_ModuleBuilder* builder, BRB_id* struct_id_p, const char* name, uint32_t n_fields, BRB_Type* fields)
{
	if (builder->error.type) return builder->error;
// because the `struct_id` field of the `BRB_Type` type is limited to 25 bits, a module cannot have more than 33554432 structs
	if (builder->module.seg_typeinfo.length == MAX_N_STRUCTS)
		return builder->error = (BRB_Error){.type = BRB_ERR_TOO_MANY_STRUCTS};
	*struct_id_p = builder->module.seg_typeinfo.length;
	if (!BRB_StructArray_append(&builder->module.seg_typeinfo, (BRB_Struct){.name = name}))
		return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
	BRB_Struct* obj = &builder->module.seg_typeinfo.data[*struct_id_p];
// copying the fields
	if (!(obj->fields = BRB_TypeArray_copy((BRB_TypeArray){.length = n_fields, .data = fields})).data)
		return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
// validating that the type isn't recursive, as a struct that has a field of the same type as the struct itself has undefined size 
// example in C: struct x { struct x field; };
	arrayForeach (BRB_Type, field, obj->fields) {
		builder->error = validateType(&builder->module, field);
		if (builder->error.type) return builder->error;
		if (field->kind == BRB_TYPE_STRUCT && field->struct_id == *struct_id_p)
			return (BRB_Error){.type = BRB_ERR_RECURSIVE_TYPE,
				.structs = builder->module.seg_typeinfo.data,
				.struct_id = *struct_id_p};
	}
	arrayForeach (BRB_Type, field, obj->fields) {
		size_t field_align = getTypeAlignment(&builder->module, *field),
			field_size = BRB_getTypeRTSize(&builder->module, *field);
		obj->size += field_align - (obj->size - 1) % field_align - 1 + field_size; // adding the field size along with the padding for it
		obj->alignment = maxInt(obj->alignment, field_align);
	}
	obj->size += obj->alignment - (obj->size - 1) % obj->alignment - 1;
	return (BRB_Error){0};
}

BRB_Error BRB_preallocStructs(BRB_ModuleBuilder* builder, uint32_t n_structs_hint)
{
	if (builder->error.type) return builder->error;
	if (!(builder->module.seg_typeinfo = BRB_StructArray_new(-(int64_t)n_structs_hint)).data)
		return builder->error = (BRB_Error){.type = BRB_ERR_NO_MEMORY};
	return (BRB_Error){0};
}

void BRB_deallocStructs(BRB_Module* module)
{
	arrayForeach (BRB_Struct, obj, module->seg_typeinfo) {
		BRB_TypeArray_clear(&obj->fields);
	}
	BRB_StructArray_clear(&module->seg_typeinfo);
}

BRB_id BRB_getStructIdByName(BRB_Module* module, const char* name)
{
	arrayForeach (BRB_Struct, obj, module->seg_typeinfo) {
		if (streq(obj->name, name)) return obj - module->seg_typeinfo.data;
	}
	return BRB_INVALID_ID;
}
