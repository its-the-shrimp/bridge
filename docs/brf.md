# BRF Specification

## Operations

1. `nop` (code: 0x0)
    `nop` operation basically does nothing.

### Operations on registers

4. `set <dst: register> <value: integer>` (code: 0x3)
    `set` operation sets the value of register `dst` to `value`.

5. `setr <dst: register> <src: register>` (code: 0x4)
    `setr` operation copies the value of register `src` to register `dst`.


### Arithmetic Operations

1. `add <dst: register> <src: register> <value: integer>` (code: 0x8)
    `add` operation writes the sum of the value of `src` register and `value` to
    register `dst`.

2. `addr <dst: register> <src: register> <src2: register>` (code: 0x9)
    `addr` operation writes the sum of the values of registers `src` and `src2`
    to register `dst`.

3. `sub <dst: register> <src: register> <value: integer>` (code: 0xA)
    `sub` operation writes the difference between the value of register `src`
    and `value` to register `dst`.

4. `subr <dst: register> <src: register> <src2: register>` (code: 0xB)
   `subr` operation writes the difference between the value of register `src`
    and the value of register `src2` to register `dst`.

### Bitwise operations

1. `and <dst: register> <src: register> <value: integer>` (code: 0x2B)
    `and` operation writes the result of logical conjunction of the value of
    register `src` and `value` to register `dst`.

2. `andr <dst: register> <src: register> <src2: register>` (code: 0x2C)
   `andr` operation writes the result of logical conjunction of the values of
   registers `src` and `src2` to register `dst`.

3. `or <dst: register> <src: register> <value: integer>` (code: 0x2D)
    `or` operation writes the result of logical disjunction of the value of
    register `src` and `value` to register `dst`.

4. `orr <dst: register> <src: register> <src2: register>` (code: 0x2E)
   `orr` operation writes the result of logical disjunction of the values of
   registers `src` and `src2` to register `dst`.

5. `not <dst: register> <src: register>` (code: 0x2F)
    `not` operation writes the result of logical negation of the value of
    register `src` to register `dst`.

6. `xor <dst: register> <src: register> <value: integer>` (code: 0x30)
    `xor` operation writes the result of logical exclusive disjunction
    of the value of register `src` and `value` to register `dst`.

7. `xorr <dst: register> <src: register> <src2: register>` (code: 0x31)
   `xorr` operation writes the result of logical exclusive  disjunction
    of the values of registers `src` and `src2` to register `dst`.

8. `shl <dst: register> <src: register> <value: integer>` (code: 0x32)
    `shl` operation writes the value of register `src` shifted left
    by the amount of bits specified by `value` to register `dst`.

9. `shlr <dst: register> <src: register> <src2: register>` (code: 0x33)
    `shlr` operation writes the value of register `src` shifted left
    by the amount of bits specified in register `src2` to register `dst`.

10. `shr <dst: register> <src: register> <value: integer>` (code: 0x34)
    `shr` operation writes the value of register `src` shifted right
    by the amount of bits specified by `value` to register `dst`.

11. `shrr <dst: register> <src: register> <value: integer>` (code: 0x35)
    `shrr` operation writes the value of register `src` shifted right
    by the amount of bits specified in register `src2` to register `dst`.

### Comparison operations

1. `eq <dst: register> <src: register> <value: integer>` (code: 0xF)
    `eq` operation writes 1 to register `dst` if the value in `src` is equal
    to `value`, otherwise 0 is written.

2. `eqr <dst: register> <src: register> <src2: integer>` (code: 0x10)
    `eqr` operation writes 1 to register `dst` if the values in registers
    `src` and `src2` are equal, otherwise 0 is written.

3. `neq <dst: register> <src: register> <value: integer>` (code: 0x11)
    `eq` operation writes 1 to register `dst` if the value in `src` is 
    not equal to `value`, otherwise 0 is written.

4. `neqr <dst: register> <src: register> <src2: integer>` (code: 0x12)
    `eqr` operation writes 1 to register `dst` if the values in registers
    `src` and `src2` are not equal, otherwise 0 is written.

5. `lt(s) <dst: register> <src: register> <value: integer>` (code: 0x13 (0x1B) )
    `lt` operation writes 1 to register `dst` if the value of register `src`
    is less than `value`, otherwise 0 is written.
    `lts` is the same operation, but performs a signed comparison.

6. `lt(s)r <dst: register> <src: register> <src2: register>` (code: 0x14 (0x1C) )
    `ltr` operation writes 1 to register `dst` if the value of register `src`
    is less than the value of register `src2`, otherwise 0 is written.
    `ltsr` is the same operation, but performs a signed comparison.

7. `gt(s) <dst: register> <src: register> <value: integer>` (code: 0x15 (0x1D) )
    `gt` operation writes 1 to register `dst` if the value of register `src`
    is greater than `value`, otherwise 0 is written.
    `gts` is the same operation, but performs a signed comparison.

8. `gt(s)r <dst: register> <src: register> <src2: register>` (code: 0x16 (0x1E) )
    `gtr` operation writes 1 to register `dst` if the value of register `src`
    is greater than the value of register `src2`, otherwise 0 is written.
    `gtsr` is the same operation, but performs a signed comparison.

9. `le(s) <dst: register> <src: register> <value: integer>` (code: 0x17 (0x1F) )
    `le` operation writes 1 to register `dst` if the value of register `src`
    is less than or equal to `value`, otherwise 0 is written.
    `les` is the same operation, but performs a signed comparison.

10. `le(s)r <dst: register> <src: register> <src2: register>` (code: 0x18 (0x20) )
    `ler` operation writes 1 to register `dst` if the value of register `src`
    is less than or equal to the value of register `src2`, otherwise 0 is written.
    `lesr` is the same operation, but performs a signed comparison.

11. `ge(s) <dst: register> <src: register> <value: integer>` (code: 0x19 (0x21) )
    `ge` operation writes 1 to register `dst` if the value of register `src`
    is greater than or equal to `value`, otherwise 0 is written.
    `ges` is the same operation, but performs a signed comparison.

12. `ge(s)r <dst: register> <src: register> <src2: register>` (code: 0x1A (0x22) )
    `ger` operation writes 1 to register `dst` if the value of register `src`
    is greater than or equal to the value of register `src2`, otherwise 0
    is written.
    `gesr` is the same operation, but performs a signed comparison.

### Control flow operations

1. `mark <mark_name: string>` (code: 0x2)
    `mark` operation binds its index in the operation sequence to `mark_name`
    so that this index can be jumped to from other parts of the program.

2. `goto <mark_name: string>` (code: 0xD)
    `goto` operation jumps to an operation index bound to `mark_name`.

3. `cgoto <src: register> <mark_name: string>` (code: 0xE)
    `cgoto` operation jumps to an operation index bound to `mark_name`
    only if the value of register `src` is non-zero, otherwise this operation
    is a no-op.

4. `call <mark_name: string>` (code: 0x36)
    `call` operation saves current execution frame
    (e.g. current operation index and stack head position) on the stack
    and jumps to an operation index bound to `mark_name`.

5. `ret` (code: 0x37)
    `ret` operation rewinds the execution state back to the last saved one
    on the stack, jumping back to operation index fetched from the frame
    and setting stack head to the one fetched from the frame.

6. `end` (code: 0x1)
    `end` operation gracefully stops the execution. It is effectively 
    an alias to the following sequence of operations:
    `set r0 0 sys exit`.
    This operation is implicitly placed by the assembler at the end of the
    execution sequence, so one is not required to specify it themselves.

### Operations on the stack

1. `push64 <src: register>` (code: 0x23)
    `push64` operation pushes the value of register `src` to the stack.

2. `pop64 <dst: register>` (code: 0x24)
    `pop64` operation pops 8 bytes from the stack and writes them to
    register `dst`.

3. `push32 <src: register>` (code: 0x25)
    `push32` operation pushes the 32 lower bits of register `src` to the stack.

4. `pop32 <dst: register>` (code: 0x26)
    `pop32` operation pops 4 bytes from the stack and writes them to
    register `dst`. The 32 higher bits of the register are zeroed out.

5. `push16 <src: register>` (code: 0x27)
    `push16` operation pushes the 16 lower bits of register `src` to the stack.

6. `pop16 <dst: register>` (code: 0x28)
    `pop16` operation pops 2 bytes from the stack and writes them to
    register `dst`. The 48 higher bits of the register are zeroed out.

5. `push8 <src: register>` (code: 0x29)
    `push8` operation pushes the 8 lower bits of register `src` to the stack.

6. `pop8 <dst: register>` (code: 0x2A)
    `pop8` operation pops 1 byte from the stack and writes it to
    register `dst`. The 56 higher bits of the register are zeroed out.

### Memory manipulation operations

1. `setd <dst: register> <block_name: string>` (code: 0x5)
    `setd` operation copies address the of data block `block_name` to 
    register `dst`.
    The difference between memory and data blocks is that data blocks contain predefined data, while memory blocks are simply zero-initialized blocks of memory that are free to be written to.

2. `setm <dst: register> <block_name: string>` (code: 0x7)
    `setm` operation copies address of the memory block `block_name` to
    register `dst`.
    
### System interaction and platform-specific operations

1. `setb <dst: register> <builtin_name: string>` (code: 0x6)
    `setb` operation copies the runtime value of the built-in value `builtin_name`
    to register `dst`.
    See (Built-ins)[#built-ins] for further information and all defined built-ins.

2. `sys <sys_op_name: string>` (code: 0xC)
    `sys` operation performs a cross-platform system call bound to `sys_op_name`.
    See (System Calls)[#system-calls] for all defined system calls.

## Built-ins

**Built-ins** are a crossplatform way to get platform- and execution-specific
data, they may have different values on different systems and between different
executions, but they are bound to a name that is constant and universal.

### All defined built-ins

1. `stdin` (index: 0x0)
    `stdin` is used in operations on file descriptors, it is a special
    read-only file, that represents input for the program, e.g. from the terminal.

2. `stdout` (index: 0x1)
    `stdout` is used in operations on file descriptors, it is a special
    write-only file, that represents output of the program, e.g. to the terminal.

3. `stderr` (index: 0x2)
    `stderr` is used in operations on file descriptors, it is a special
    write-only file, that represents error output of the program, e.g. to
    the terminal.

4. `argc` (index: 0x3)
    `argc` is a runtime-specific constant, which specifies how many
    command-line arguments were provided to the program.

5. `argv` (index: 0x4)
    `argv` is a runtime-specific constant, providing a pointer to the start
    of array of command-line arguments.
    Command-line arguments are provided in the form of array of pointers to
    null-terminated strings, the C type for it is `char**`.

## System Calls

**System Calls** are a crossplatform way to interact with the current system,
they might have different call conventions and implementations on different systems,
but a BRF system call is used the same way on any system that BRF supports.

### All defined system calls

1. `exit` (index: 0x1)
    `exit` system call sets the exit code and stops the execution.
    Arguments:
        `<r0>` - an 8-bit unsigned integer that specifies the exit code.

2. `write` (index: 0x2)
    `write` system call writes data to a file descriptor.
    Arguments:
        `<r0>` - file descriptor identifier, can be obtained using 
            `open` system call, or can be one a built-in `stdout` or `stderr`.
        `<r1>` - pointer to the data to be written to the file descriptor.
        `<r2>` - amount of bytes to be copied from `<r1>` to the file descriptor.
    Result: `<r0>` - If the call succeded, the value is the amount of bytes actually written to the file descriptor.
                Otherwise, the value will be set to -1.