import ctypes
from re import M
import typing
import enum

lib = ctypes.CDLL("build/lib/libbrb.dylib", use_errno=True)
c = ctypes.CDLL("libSystem.dylib", use_errno=True)

c_FILE_p = ctypes.c_void_p
CType = typing.TypeVar('CType')

def newUnion(fields: list[tuple[str, CType]]) -> ctypes.Union:
    class NewUnion(ctypes.Union):
        _fields_ = fields

    return NewUnion

def newArray(t: CType) -> ctypes.Structure:
    class NewArray(ctypes.Structure):
        _fields_ = [
            ("length", ctypes.c_int),
            ("data", ctypes.POINTER(t))
        ]

        def __iter__(self) -> typing.Iterator:
            arr_t = t * self.length
            return iter(arr_t.from_address(ctypes.cast(self.data, ctypes.c_void_p).value))

    return NewArray
    
def getCFILE(src: typing.IO, mode: str) -> c_FILE_p:
    return c.fdopen(src.fileno(), mode.encode('utf-8'))

class CEnum(enum.Enum):
    def __new__(cls):
        value = len(cls.__members__)
        obj = object.__new__(cls)
        obj._value_ = value
        return obj

class Builtin(CEnum):
    STDIN = ()
    STDOUT = ()
    STDERR = ()
    
class SyscallCode(CEnum):
	INVALID = ()
	EXIT = ()
	WRITE = ()
	ARGC = ()
	ARGV = ()
	READ = ()
	GET_ERRNO = ()
	SET_ERRNO = ()
    
class ConditionCode(CEnum):
	NON = ()
	EQU = ()
	NEQ = ()
	LTU = ()
	GTU = ()
	LEU = ()
	GEU = ()
	LTS = ()
	GTS = ()
	LES = ()
	GES = ()

class Op(ctypes.Structure):
    class Code(CEnum):
        NONE = ()
        END = ()
        MARK = ()
        SET = ()
        SETR = ()
        SETD = ()
        SETB = ()
        SETM = ()
        ADD = ()
        ADDR = ()
        SUB = ()
        SUBR = ()
        SYS = ()
        GOTO = ()
        CMP = ()
        CMPR = ()
        AND = ()
        ANDR = ()
        OR = ()
        ORR = ()
        NOT = ()
        XOR = ()
        XORR = ()
        SHL = ()
        SHLR = ()
        SHR = ()
        SHRR = ()
        SHRS = ()
        SHRSR = ()
        PROC = ()
        CALL = ()
        RET = ()
        ENDPROC = ()
        LD64 = ()
        STR64 = ()
        LD32 = ()
        STR32 = ()
        LD16 = ()
        STR16 = ()
        LD8 = ()
        STR8 = ()
        VAR = ()
        SETV = ()
        MUL = ()
        MULR = ()
        DIV = ()
        DIVR = ()
        DIVS = ()
        DIVSR = ()
        EXTPROC = ()
        LDV = ()
        STRV = ()
        POPV = ()
        PUSHV = ()
        ATF = ()
        ATL = ()
        SETC = ()
        DELNV = ()
        LD64S = ()
        LD32S = ()
        LD16S = ()
        LD8S = ()
        LDVS = ()
        SX32 = ()
        SX16 = ()
        SX8 = ()
        MOD = ()
        MODS = ()
        MODR = ()
        MODSR = ()

    _anonymous_ = ("_arg", )
    _fields_ = [
        ("c_type", ctypes.c_int8),
        ("c_dst_reg", ctypes.c_int8),
        ("c_src_reg", ctypes.c_int8),
        ("c_var_size", ctypes.c_uint8),
        ("c_cond_id", ctypes.c_uint8),
        ("c_cond_arg", ctypes.c_uint8),
        ("_arg", newUnion([
            ("c_value", ctypes.c_uint64),
            ("c_symbol_id", ctypes.c_int64),
            ("c_new_var_size", ctypes.c_int64),
            ("c_op_offset", ctypes.c_int64),
            ("c_mark_name", ctypes.c_char_p),
            ("c_syscall_id", ctypes.c_uint8),
            ("c_src2_reg", ctypes.c_int8)

        ]))
    ]
    module: typing.Any
    index: int

    @property
    def code(self) -> Code:
        return Op.Code(self.c_type)

    def get_disassembly(self) -> str:
        def opcode_str(op: Op) -> str:
            return f"{self.code.name.lower()}" + (f":{ConditionCode(op.c_cond_id).name.lower()}" if op.c_cond_id > 0 else "")

        get_no_arg_op_dis = lambda op: f"{opcode_str(op)}"
        get_reg_imm_op_dis = lambda op: f"{opcode_str(op)} r{op.c_dst_reg} {op.c_value}"
        get_2reg_op_dis = lambda op: f"{opcode_str(op)} r{op.c_dst_reg} r{op.c_src_reg}"
        get_2reg_imm_op_dis = lambda op: f"{opcode_str(op)} r{op.c_dst_reg} r{op.c_src_reg} {op.c_value}"
        get_3reg_op_dis = lambda op: f"{opcode_str(op)} r{op.c_dst_reg} r{op.c_src_reg} r{op.c_src2_reg}"
        get_mark_op_dis = lambda op: f"{opcode_str(op)} {op.c_mark_name.decode()}"
        get_ldv_op_dis = lambda op: f"{opcode_str(op)} r{op.c_src_reg} {op.c_symbol_id}%{op.c_var_size}"

        dis_f: dict[Op.Type, typing.Callable[[Op], str]] = {
            Op.Code.NONE: get_no_arg_op_dis,
            Op.Code.END: get_no_arg_op_dis,
            Op.Code.MARK: lambda op: f"{opcode_str(op)} .m{op.index}",
            Op.Code.SET: get_reg_imm_op_dis,
            Op.Code.SETR: get_2reg_op_dis,
            Op.Code.SETD: lambda op: f"{opcode_str(op)} r{op.c_dst_reg} {op.module.data_blocks[op.c_symbol_id].name}",
            Op.Code.SETB: lambda op: f"{opcode_str(op)} r{op.c_dst_reg} {Builtin(op.c_symbol_id).name}",
            Op.Code.SETM: lambda op: f"{opcode_str(op)} r{op.c_dst_reg} {op.module.memory_blocks[op.c_symbol_id].name}",
            Op.Code.ADD: get_2reg_imm_op_dis,
            Op.Code.ADDR: get_3reg_op_dis,
            Op.Code.SUB: get_2reg_imm_op_dis,
            Op.Code.SUBR: get_3reg_op_dis,
            Op.Code.SYS: lambda op: f"{opcode_str(op)} {SyscallCode(op.c_syscall_id).name.lower()}",
            Op.Code.GOTO: lambda op: f"{opcode_str(op)} .m{op.index + op.c_op_offset} // {op.c_op_offset}",
            Op.Code.CMP: lambda op: f"{opcode_str(op)} r{op.c_src_reg} {op.c_value}",
            Op.Code.CMPR: lambda op: f"{opcode_str(op)} r{op.c_src_reg} r{op.c_src2_reg}",
            Op.Code.AND: get_2reg_imm_op_dis,
            Op.Code.ANDR: get_3reg_op_dis,
            Op.Code.OR: get_2reg_imm_op_dis,
            Op.Code.ORR: get_3reg_op_dis,
            Op.Code.NOT: get_2reg_op_dis,
            Op.Code.XOR: get_2reg_imm_op_dis,
            Op.Code.XORR: get_3reg_op_dis,
            Op.Code.SHL: get_2reg_imm_op_dis,
            Op.Code.SHLR: get_3reg_op_dis,
            Op.Code.SHR: get_2reg_imm_op_dis,
            Op.Code.SHRR: get_3reg_op_dis,
            Op.Code.SHRS: get_2reg_imm_op_dis,
            Op.Code.SHRSR: get_3reg_op_dis,
            Op.Code.PROC: get_mark_op_dis,
            Op.Code.CALL: lambda op: f"{opcode_str(op)} {op.module.ops[op.c_symbol_id].c_mark_name.decode()}",
            Op.Code.RET: get_no_arg_op_dis,
            Op.Code.ENDPROC: get_no_arg_op_dis,
            Op.Code.LD64: get_2reg_op_dis,
            Op.Code.STR64: get_2reg_op_dis,
            Op.Code.LD32: get_2reg_op_dis,
            Op.Code.STR32: get_2reg_op_dis,
            Op.Code.LD16: get_2reg_op_dis,
            Op.Code.STR16: get_2reg_op_dis,
            Op.Code.LD8: get_2reg_op_dis,
            Op.Code.STR8: get_2reg_op_dis,
            Op.Code.VAR: lambda op: f"{opcode_str(op)} 0%{op.c_var_size}",
            Op.Code.SETV: lambda op: f"{opcode_str(op)} {op.c_symbol_id}%",
            Op.Code.MUL: get_2reg_imm_op_dis,
            Op.Code.MULR: get_3reg_op_dis,
            Op.Code.DIV: get_2reg_imm_op_dis,
            Op.Code.DIVR: get_2reg_imm_op_dis,
            Op.Code.DIVS: get_2reg_imm_op_dis,
            Op.Code.DIVSR: get_3reg_op_dis,
            Op.Code.EXTPROC: get_mark_op_dis,
            Op.Code.LDV: get_ldv_op_dis,
            Op.Code.STRV: lambda op: f"{opcode_str(op)} {op.c_symbol_id}%{op.c_var_size} r{op.c_src_reg}",
            Op.Code.POPV: lambda op: f"{opcode_str(op)} r{op.c_dst_reg} 0%{op.c_var_size}",
            Op.Code.PUSHV: lambda op: f"{opcode_str(op)} 0%{op.c_var_size} r{op.c_src_reg}",
            Op.Code.ATF: lambda op: f"@f {op.c_mark_name.decode()}",
            Op.Code.ATL: lambda op: f"@l {op.c_symbol_id}",
            Op.Code.SETC: lambda op: f"{opcode_str(op)} r{op.c_dst_reg} {ConditionCode(op.c_cond_arg).name.lower()}",
            Op.Code.DELNV: lambda op: f"{opcode_str(op)} %{op.c_symbol_id}",
            Op.Code.LD64S: get_2reg_op_dis,
            Op.Code.LD32S: get_2reg_op_dis,
            Op.Code.LD16S: get_2reg_op_dis,
            Op.Code.LD8S: get_2reg_op_dis,
            Op.Code.LDVS: get_ldv_op_dis,
            Op.Code.SX32: get_2reg_op_dis,
            Op.Code.SX16: get_2reg_op_dis,
            Op.Code.SX8: get_2reg_op_dis,
            Op.Code.MOD: get_2reg_imm_op_dis,
            Op.Code.MODS: get_2reg_imm_op_dis,
            Op.Code.MODR: get_3reg_op_dis,
            Op.Code.MODSR: get_3reg_op_dis
        }

        return dis_f.get(self.code, lambda op: f"<{op.c_type}>")(self)

    def __repr__(self):
        return f"<{self.__class__.__qualname__} `{self.get_disassembly()}`>"

class sbuf(ctypes.Structure):
    _fields_ = [
        ("length", ctypes.c_long),
        ("data", ctypes.c_char_p)
    ]

    def __bytes__(self):
        return ctypes.string_at(self.data, self.length)

class DataBlock(ctypes.Structure):
    _fields_ = [
        ("_name", ctypes.c_char_p),
        ("_spec", sbuf)
    ]
    
    @property
    def name(self) -> str:
        return self._name.decode('utf-8')

    @property
    def spec(self) -> bytes:
        return bytes(self._spec)

    def __repr__(self):
        return f"<{self.__class__.__qualname__} `{self.name}`: {self.spec}>"

class MemBlock(ctypes.Structure):
    _fields_ = [
        ("_name", ctypes.c_char_p),
        ("size", ctypes.c_int64)
    ]

    @property
    def name(self) -> str:
        return self._name.decode('utf-8')

    def __repr__(self):
        return f"<{self.__class__.__qualname__} `{self.name}`: {self.size} bytes>"

OpArray = newArray(Op)
DataBlockArray = newArray(DataBlock)
MemBlockArray = newArray(MemBlock)
strArray = newArray(ctypes.c_char_p)

class BRBError(Exception):
    pass

class BRBLoadError(BRBError):
    def __init__(self, error_code: int):
        super().__init__(f"Module loader failure (error code: {error_code})")

class Module(ctypes.Structure):
    class _LoadError(ctypes.Structure):
        _anonymous_ = ("_arg", )
        _fields_ = [
            ("code", ctypes.c_uint),
            ("_arg", newUnion([
                ("module_name", ctypes.c_char_p),
                ("opcode", ctypes.c_int32),
                ("cond_id", ctypes.c_uint8)

            ]))
        ]

    _fields_ = [
        ("c_execblock", OpArray),
        ("c_memblocks", MemBlockArray),
        ("c_datablocks", DataBlockArray),
        ("c_entry_opid", ctypes.c_int64),
        ("c_stack_size", ctypes.c_int64),
        ("c_submodules", strArray),
        ("c_root_db_start", ctypes.c_int),
        ("c_root_mb_start", ctypes.c_int),
        ("c_root_eb_start", ctypes.c_int)
    ]
    name: str

    class LoadFlags(enum.IntFlag):
        EXECUTABLE = 0x02

    def __init__(self, name: str):
        super().__init__()

        self.name = name
        self.ops: tuple[Op] = tuple(self.c_execblock)
        self.data_blocks: tuple[DataBlock] = tuple(self.c_datablocks)
        self.memory_blocks: tuple[MemBlock] = tuple(self.c_memblocks)

        for i, op in enumerate(self.ops):
            op.module = self
            op.index = i

    @classmethod
    def load(cls, src: str, search_paths: list[str] = ["."]):
        self: cls = cls.__new__(cls)
        
        fd: c_FILE_p = c.fopen(src.encode(), b"rb")
        if fd == None: OSError(ctypes.get_errno())
        err: self._LoadError = lib.loadModule(
            fd,
            ctypes.byref(self),
            (ctypes.c_char_p * (len(search_paths) + 1))(*[i.encode('utf-8') for i in search_paths], 0),
            Module.LoadFlags.EXECUTABLE
        )
        if err.code > 0: raise BRBLoadError(err.code)

        self.__init__(src)
        return self

    @classmethod
    def loads(cls, src: typing.ByteString, search_paths: list[str] = ["."]):
        self: cls = cls.__new__(cls)
        
        temp_fd: c_FILE_p = c.fmemopen(src, len(src), b"rb")
        if temp_fd == None: raise OSError(ctypes.get_errno())
        err: self._LoadError = lib.loadModule(
            temp_fd,
            ctypes.byref(self),
            (ctypes.c_char_p * (len(search_paths) + 1))(*[i.encode('utf-8') for i in search_paths], 0),
            Module.LoadFlags.EXECUTABLE
        )
        if err.code > 0: raise BRBLoadError(err.code)
        if c.fclose(temp_fd) < 0: raise OSError(ctypes.get_errno())

        self.__init__("")
        return self

    def __repr__(self):
        return f"<{type(self).__qualname__} `{self.name}`: {len(self.ops)} operations; {len(self.data_blocks)} data blocks; {len(self.memory_blocks)} memory blocks>"

    def get_disassembly(self) -> str:
        res: str = ""
        for op in self.ops[:-1]:
            res += f"0x{op.index:08x}\t{op.get_disassembly()}\n"
        res += f"0x{self.ops[-1].index:08x}\t{self.ops[-1].get_disassembly()}"

        return res

    def disassemble(self) -> None:
        print(self.get_disassembly())

sbufArray = newArray(sbuf)

class ExecEnv(ctypes.Structure):
    _fields_ = [
        ("c_stack_brk", ctypes.c_void_p),
        ("c_stack_head", ctypes.c_void_p),
        ("c_prev_stack_head", ctypes.c_void_p),
        ("c_memblocks", sbufArray),
        ("c_exitcode", ctypes.c_uint8),
        ("c_op_id", ctypes.c_int),
        ("c_registers", ctypes.POINTER(ctypes.c_uint64)),
        ("c_exec_argc", ctypes.c_int),
        ("c_exec_argv", ctypes.POINTER(sbuf)),
        ("c_src_path", ctypes.c_char_p),
        ("c_src_line", ctypes.c_int),
        ("c_exec_callbacks", ctypes.POINTER(ctypes.c_void_p))
    ]
    module: Module

    def __init__(self, module: Module, args: list[str] = []):
        lib.initExecEnv(
            ctypes.byref(self),
            ctypes.byref(module),
            (ctypes.c_char_p * (len(args) + 2))(module.name.encode(), *[i.encode() for i in args], 0)
        )
        self.module = module

def execute(module: Module, args: list[str] = []) -> ExecEnv:
    env: ExecEnv = ExecEnv(module, args=args)

    interruptor: ctypes.c_bool = ctypes.c_bool(False)
    lib.execModule(ctypes.byref(env), ctypes.byref(module), ctypes.byref(interruptor))

c.fdopen.argtypes = [ctypes.c_int, ctypes.c_char_p]
c.fdopen.restype = c_FILE_p

c.fopen.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
c.fopen.restype = c_FILE_p

c.fmemopen.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_char_p]
c.fmemopen.restype = c_FILE_p

c.fclose.argtypes = [c_FILE_p]
c.fclose.restype = ctypes.c_int

lib.loadModule.argtypes = [c_FILE_p, ctypes.POINTER(Module), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int]
lib.loadModule.restype = Module._LoadError

lib.initExecEnv.argtypes = [ctypes.POINTER(ExecEnv), ctypes.POINTER(Module), ctypes.POINTER(ctypes.c_char_p)]
lib.initExecEnv.restype = None

lib.execModule.argtypes = [ctypes.POINTER(ExecEnv), ctypes.POINTER(Module), ctypes.POINTER(ctypes.c_bool)]
lib.execModule.restype = None