# BRB Specification

The following is a specification of the Visual BRidge Bytecode format.\
Bytecode executable is a collection of modules, with the root module as the one from which execution starts.

## Module Structure

A module is a single file with a collection of defined segments.
Currently there are 4 segment types:
- `data`
- `memory`
- `meta`
- `exec`
They can be specified in any order. The following is the description of each of the segments:\

### `data` Segment
The `data` segment is a set of pre-defined sequences of bytes, provided as string enclosed in double quotes, with a name assigned to them.
Defined memory blocks can be referenced in the program by their name to get ther runtime address.\
The syntax of the `data` segment is the following:\
```
data {
    foo "some data as a set of UTF-8 characters.\n" block2 "некоторые данные в виде последовательности символов UTF-8.\n"
    bar "9\x05"
}
```
Hexadecimal and octal literals are supported in the definition of data blocks.
Specifying data blocks one per line isn't necessary, they can be defined one after the other.

### `memory` Segment
The `memory` segment is a set of named memory blocks, that are zero-initialized before the start of the program. Unlike memory blocks defined in the `data` segment, these cannot be pre-defined, and only need their size to be specified.\
The syntax of the `memory` segment is the following:\
```
memory {
    foo 32 baz 19
    bar 155
}
```
The size of memory blocks is provided in bytes.
Specifying data blocks one per line isn't necessary, they can be defined one after the other.

### `stacksize` Segment
Default stack size in the BRidge virtual machine is 512 KBs. This parameter can be changed using the `stacksize` segment. Unlike other segments, its syntax is much shorter:\
`stacksize 1234`\
the number after the word `stacksize`, in this case - 1234, is the specified stack size, in bytes. Numbers below or equal to 0 are invalid.\

### `exec` Segment
The `exec` segment contains operations to be executed by the virtual machine. The syntax of this segment is the following:\
```
exec {
    op1 arg1 arg2
    op2 arg1 arg2 arg3 op3 arg1
}
```
Operations and their arguments are defined one by one, without any delimiters other than spaces and newlines.
There are 3 types of arguments to the operations:\
- `value`: constant integer value, specified as either decimal, octal or hexadecimal literal. Must be in range between -2^63 and 2^63 - 1.
- `register`: register field, `r0`, `r1`, up to `r7`.
- `name`: a name, containing UTF-8 alpha-numeric characters, and other symbols except for '{', '}', and ':'. Must not be longer than 256 bytes.
The following describes all the available operations:\

#### Operations

1. `nop` (code: 0x0):\
    `nop` operation basically does nothing.

##### Operations on registers

4. `set <dst: register> <value: integer>` (code: 0x3):\
    `set` operation sets the value of register `dst` to `value`.

5. `setr <dst: register> <src: register>` (code: 0x4):\
    `setr` operation copies the value of register `src` to register `dst`.

##### Arithmetic Operations

1. `add <dst: register> <src: register> <value: integer>` (code: 0x8):\
    `add` operation writes the sum of the value of `src` register and `value` to
    register `dst`.

2. `addr <dst: register> <src: register> <src2: register>` (code: 0x9):\
    `addr` operation writes the sum of the values of registers `src` and `src2`
    to register `dst`.

3. `sub <dst: register> <src: register> <value: integer>` (code: 0xA):\
    `sub` operation writes the difference between the value of register `src`
    and `value` to register `dst`.

4. `subr <dst: register> <src: register> <src2: register>` (code: 0xB):\
   `subr` operation writes the difference between the value of register `src`
    and the value of register `src2` to register `dst`.

5. `mul <dst: register> <src: register> <value: integer>` (code: 0x2B):\
    `mul` operation saves the product of the value of register `src` and `value` to register `dst`.

6. `mulr <dst: register> <src: register> <src2: register>` (code: 0x2C):\
    `mulr` operation saves the product of the values of registers `src` and `src2` to register `dst`.

7. `div(s) <dst: register> <src: register> <value: integer>` (code: 0x2D (0x2F) ):\
    `div` operation saves the quotient of the value of register `src` and `value` to register `dst`.\
    `divs` operation is the same but performs signed division.

8. `div(s)r <dst: register> <src: register> <src2: register>` (code: 0x2E (0x30) ):\
    `divr` operation saves the quotient of the values of registers `src` and `src2` to register `dst`.\
    `divsr` operation is the same but performs signed division.

##### Bitwise operations

1. `and <dst: register> <src: register> <value: integer>` (code: 0x10):\
    `and` operation writes the result of logical conjunction of the value of
    register `src` and `value` to register `dst`.

2. `andr <dst: register> <src: register> <src2: register>` (code: 0x11):\
   `andr` operation writes the result of logical conjunction of the values of
   registers `src` and `src2` to register `dst`.

3. `or <dst: register> <src: register> <value: integer>` (code: 0x12):\
    `or` operation writes the result of logical disjunction of the value of
    register `src` and `value` to register `dst`.

4. `orr <dst: register> <src: register> <src2: register>` (code: 0x13):\
   `orr` operation writes the result of logical disjunction of the values of
   registers `src` and `src2` to register `dst`.

5. `not <dst: register> <src: register>` (code: 0x14):\
    `not` operation writes the result of logical negation of the value of
    register `src` to register `dst`.

6. `xor <dst: register> <src: register> <value: integer>` (code: 0x15):\
    `xor` operation writes the result of logical exclusive disjunction
    of the value of register `src` and `value` to register `dst`.

7. `xorr <dst: register> <src: register> <src2: register>` (code: 0x16):\
   `xorr` operation writes the result of logical exclusive  disjunction
    of the values of registers `src` and `src2` to register `dst`.

8. `shl <dst: register> <src: register> <value: integer>` (code: 0x17):\
    `shl` operation writes the value of register `src` shifted left
    by the amount of bits specified by `value` to register `dst`.

9. `shlr <dst: register> <src: register> <src2: register>` (code: 0x18):\
    `shlr` operation writes the value of register `src` shifted left
    by the amount of bits specified in register `src2` to register `dst`.

10. `shr(s) <dst: register> <src: register> <value: integer>` (code: 0x19 (0x1B) ):\
    `shr` operation writes the value of register `src` shifted right
    by the amount of bits specified by `value`, shifting in zeros, to register `dst`.
    `shrs` is the same operation, but shifts in copies of the sign bit.

11. `shr(s)r <dst: register> <src: register> <value: integer>` (code: 0x1A (0x1C) ):\
    `shrr` operation writes the value of register `src` shifted right
    by the amount of bits specified in register `src2`, shifting in zeros, to register `dst`.
    `shrsr` is the same operation, but shifts in copies of the sign bit.

##### Comparison operations

1. `cmp <src: register> <value: integer>` (code: 0xE):\
    `cmp` operation sets the arguments of a condition to the value of register `src`
    and `value`.

2. `cmpr <src: register> <src2: register>` (code: 0xF):\
    `cmpr` operation sets the arguments of a condition to the values of registers
    `src` and `src2`.

##### Control flow operations

1. `mark <mark_name: name>` (code: 0x2):\
    `mark` operation binds its index in the operation sequence to `mark_name`
    so that this index can be jumped to from other parts of the procedure.

2. `goto <mark_name: name>` (code: 0xD):\
    `goto` operation jumps to an operation index bound to `mark_name`.
    to avoid stack corruption cases, only marks in the same procedure can be accessed.

4. `proc <proc_name: name>` (code: 0x1D):\
    `proc` operation startы the definition of procedure with the name `proc_name`.
    It is a compile-time marker and is a no-op at runtime.

5. `call <mark_name: name>` (code: 0x1E):\
    `call` operation saves current execution frame
    (e.g. current operation index and stack head position) on the stack
    and jumps to a procedure bound to `mark_name`.

6. `ret` (code: 0x1F):\
    `ret` operation rewinds the execution state back to the last saved one
    on the stack, jumping back to operation index fetched from the frame
    and setting stack head to the one fetched from the frame.

7. `endproc` (code: 0x20):\
    `endproc` operation ends the procedure definition. It is only a compile-time marker,
    and is a no-op at runtime.

8. `end` (code: 0x1):\
    `end` operation gracefully stops the execution. It is effectively 
    an alias to the following sequence of operations:
    `set r0 0 sys exit`.\
    This operation is implicitly placed by the assembler at the end of the
    execution sequence, so one is not required to specify it themselves.

##### Operations on the stack

1. `var <var_size: integer>` (code: 0x29):\
    `var` operation allocates `var_size` bytes on the stack, creating a new variable on the local stack frame.

2. `setv <dst: register> <var_id: integer>` (code: 0x2A):\
    `setv` operation sets the value of register `dst` to an address of a local variable with the index specified by `var_id`.
    Variables outside of the local stack frame cannot be accessed.

##### Memory manipulation operations

1. `setd <dst: register> <block_name: name>` (code: 0x5):\
    `setd` operation copies address the of data block `block_name` to 
    register `dst`.\
    The difference between memory and data blocks is that data blocks contain predefined data, while memory blocks are simply zero-initialized blocks of memory that are free to be written to.

2. `setm <dst: register> <block_name: name>` (code: 0x7):\
    `setm` operation copies address of the memory block `block_name` to
    register `dst`.

3. `ld64 <dst: register> <src: register>` (code: 0x21):\
    `ld64` operation loads a 64-bit value from the address in register `src`
    to register `dst`.

4. `str64 <dst: register> <src: register>` (code: 0x22):\
    `str64` operation stores the value of register `src` at the address
    in register `dst`.

5. `ld32 <dst: register> <src: register>` (code: 0x23):\
    `ld32` operation loads a 32-bit value from the address in register `src`
    to register `dst`. Higher 32 bits of register `dst` are zeroed out.

6. `str32 <dst: register> <src: register>` (code: 0x24):\
    `str32` operation stores 32 lower bits of register `src` at the address
    in register `dst`.

7. `ld16 <dst: register> <src: register>` (code: 0x25):\
    `ld16` operation loads a 16-bit value from the address in register `src`
    to register `dst`. Higher 48 bits of register `dst` are zeroed out.

8. `str16 <dst: register> <src: register>` (code: 0x26):\
    `str16` operation stores 16 lower bits of register `src` at the address
    in register `dst`.

9. `ld8 <dst: register> <src: register>` (code: 0x27):\
    `ld8` operation loads a 8-bit value from the address in register `src`
    to register `dst`. Higher 56 bits of register `dst` are zeroed out.

10. `str8 <dst: register> <src: register>` (code: 0x28):\
    `str8` operation stores 8 lower bits of register `src` at the address
    in register `dst`.
    
##### System interaction and platform-specific operations

1. `setb <dst: register> <builtin_name: name>` (code: 0x6):\
    `setb` operation copies the runtime value of the built-in value `builtin_name`
    to register `dst`.
    See (Built-ins)[#built-ins] for further information and all defined built-ins.

2. `sys <sys_op_name: name>` (code: 0xC):\
    `sys` operation performs a cross-platform system call bound to `sys_op_name`.
    See (System Calls)[#system-calls] for all defined system calls.

#### Conditions

In BRidge Bytecode, all operations with a few exceptions can be executed conditionally. 
Conditions are different comparisons on 2 arguments specified by `cmp` or `cmpr` operations.
The syntax for a conditionally executed operation is the following:\
`<op>:<condition> <args>`\

#### Condition codes
1. `non` - always evaluates to true; the default condition code for all operations.\
2. `equ` - evaluates to true if the arguments are equal.\
3. `neq` - evaluates to true if the arguments are not equal.\
4. `ltu` - evaluates to true if the first argument is less than the second one; the comparison is unsigned.\
5. `gtu` - evaluates to true if the first argument is greater than the second one; the comparison is unsigned.\
6. `leu` - evaluates to true if the first argument is less than or equal to the second one; the comparison is unsigned.\
7. `geu` - evaluates to true if the first argument is greater than or equal to the second one; the comparison is unsigned.\
4. `lts` - evaluates to true if the first argument is less than the second one; the comparison is signed.\
5. `gts` - evaluates to true if the first argument is greater than the second one; the comparison is signed.\
6. `les` - evaluates to true if the first argument is less than or equal to the second one; the comparison is signed.\
7. `ges` - evaluates to true if the first argument is greater than or equal to the second one; the comparison is signed.\

##### Exceptions
The following operations cannot have a condition specified:\
- `mark`
- `proc`
- `endproc`
- `var`

#### Built-ins

**Built-ins** are a crossplatform way to get platform- and execution-specific
data, they may have different values on different systems and between different
executions, but they are bound to a name that is constant and universal.

##### All defined built-ins

1. `stdin` (index: 0x0):\
    `stdin` is used in operations on file descriptors, it is a special
    read-only file, that represents input for the program, e.g. from the terminal.

2. `stdout` (index: 0x1):\
    `stdout` is used in operations on file descriptors, it is a special
    write-only file, that represents output of the program, e.g. to the terminal.

3. `stderr` (index: 0x2):\
    `stderr` is used in operations on file descriptors, it is a special
    write-only file, that represents error output of the program, e.g. to
    the terminal.

#### System Calls

**System Calls** are a crossplatform way to interact with the current system,
they might have different call conventions and implementations on different systems,
but a BRF system call is used the same way on any system that BRF supports.

##### All defined system calls

1. `exit` (index: 0x1):\
    `exit` system call sets the exit code and stops the execution.
    Arguments:\
        `<r0>` - an 8-bit unsigned integer that specifies the exit code.

2. `write` (index: 0x2):\
    `write` system call writes data to a file descriptor.\
    Arguments:\
        `<r0>` - writable file descriptor, can be obtained using 
            `open` system call, or can be one a built-in `stdout` or `stderr`.\
        `<r1>` - pointer to the data to be written to the file descriptor.\
        `<r2>` - amount of bytes to be copied from `<r1>` to the file descriptor.\
    Result: `<r0>` - If the call succeded, the value is the amount of bytes actually written to the file descriptor.
                Otherwise, the value will be set to -1.

3. `argc` (index: 0x3):\
    `argc` system call provides the amount of arguments passed to the program, e.g. from a command line.\
    Arguments: none.\
    Result: `<r0>` - a 32-bit integer specifying the amount of provided arguments.

4. `argv` (index: 0x4):\
    `argv` system call provides a pointer to n-th argument, passed to the program.
    Note: the first argument is always the name of the program.\
    Arguments:\
        `<r0>` - index of the requested argument.\
    Result: `<r0>` - pointer to the argument, or NULL pointer if the index provided in `<r0>` is out of range.

5. `read` (index: 0x5):\
    `read` system call reads data from a file descriptor.\
    Arguments:\
        `<r0>` - readable file descriptor, can be obtained using
            `open` system call, or can be a built-in `stdin`.\
        `<r1>` - address, to which data will be read.\
        `<r2>` - amount of bytes to be read to `<r1>` from file descriptor.\
    Result:  `<r0>` - If the call succeded, the value is the amount of bytes actually read from the file descriptor.
        Otherwise, the value will be set to -1.

6. `get_errno` (index: 0x6):\
    `get_errno` system call gets the current error code. Error code is set by other system calls in case of failure, and the code itself describes the cause of failure. Alternatively, error code can be set by the application using `set_errno` system call.\
    Arguments: none.\
    Result: `<r0>` - a 32-bit integer specifying the error code.\

7. `set_errno` (index: 0x7):\
    `set_errno` system call sets the internal error code.\
    Arguments: `<r0>` - new error code to be set.\
    Result: none - the function is guaranteed to succeed.\