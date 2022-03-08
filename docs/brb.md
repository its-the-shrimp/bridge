# BRB Specification

## Operations

1. `nop` (code: 0x0):\
    `nop` operation basically does nothing.

### Operations on registers

4. `set <dst: register> <value: integer>` (code: 0x3):\
    `set` operation sets the value of register `dst` to `value`.

5. `setr <dst: register> <src: register>` (code: 0x4):\
    `setr` operation copies the value of register `src` to register `dst`.


### Arithmetic Operations

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

### Bitwise operations

1. `and <dst: register> <src: register> <value: integer>` (code: 0x2B):\
    `and` operation writes the result of logical conjunction of the value of
    register `src` and `value` to register `dst`.

2. `andr <dst: register> <src: register> <src2: register>` (code: 0x2C):\
   `andr` operation writes the result of logical conjunction of the values of
   registers `src` and `src2` to register `dst`.

3. `or <dst: register> <src: register> <value: integer>` (code: 0x2D):\
    `or` operation writes the result of logical disjunction of the value of
    register `src` and `value` to register `dst`.

4. `orr <dst: register> <src: register> <src2: register>` (code: 0x2E):\
   `orr` operation writes the result of logical disjunction of the values of
   registers `src` and `src2` to register `dst`.

5. `not <dst: register> <src: register>` (code: 0x2F):\
    `not` operation writes the result of logical negation of the value of
    register `src` to register `dst`.

6. `xor <dst: register> <src: register> <value: integer>` (code: 0x30):\
    `xor` operation writes the result of logical exclusive disjunction
    of the value of register `src` and `value` to register `dst`.

7. `xorr <dst: register> <src: register> <src2: register>` (code: 0x31):\
   `xorr` operation writes the result of logical exclusive  disjunction
    of the values of registers `src` and `src2` to register `dst`.

8. `shl <dst: register> <src: register> <value: integer>` (code: 0x32):\
    `shl` operation writes the value of register `src` shifted left
    by the amount of bits specified by `value` to register `dst`.

9. `shlr <dst: register> <src: register> <src2: register>` (code: 0x33):\
    `shlr` operation writes the value of register `src` shifted left
    by the amount of bits specified in register `src2` to register `dst`.

10. `shr(s) <dst: register> <src: register> <value: integer>` (code: 0x34 (0x36) ):\
    `shr` operation writes the value of register `src` shifted right
    by the amount of bits specified by `value`, shifting in zeros, to register `dst`.
    `shrs` is the same operation, but shifts in copies of the sign bit.

11. `shr(s)r <dst: register> <src: register> <value: integer>` (code: 0x35 (0x37) ):\
    `shrr` operation writes the value of register `src` shifted right
    by the amount of bits specified in register `src2`, shifting in zeros, to register `dst`.
    `shrsr` is the same operation, but shifts in copies of the sign bit.

### Comparison operations

1. `eq <dst: register> <src: register> <value: integer>` (code: 0xF):\
    `eq` operation writes 1 to register `dst` if the value in `src` is equal
    to `value`, otherwise 0 is written.

2. `eqr <dst: register> <src: register> <src2: integer>` (code: 0x10):\
    `eqr` operation writes 1 to register `dst` if the values in registers
    `src` and `src2` are equal, otherwise 0 is written.

3. `neq <dst: register> <src: register> <value: integer>` (code: 0x11):\
    `eq` operation writes 1 to register `dst` if the value in `src` is 
    not equal to `value`, otherwise 0 is written.

4. `neqr <dst: register> <src: register> <src2: integer>` (code: 0x12):\
    `eqr` operation writes 1 to register `dst` if the values in registers
    `src` and `src2` are not equal, otherwise 0 is written.

5. `lt(s) <dst: register> <src: register> <value: integer>` (code: 0x13 (0x1B) ):\
    `lt` operation writes 1 to register `dst` if the value of register `src`
    is less than `value`, otherwise 0 is written.
    `lts` is the same operation, but performs a signed comparison.

6. `lt(s)r <dst: register> <src: register> <src2: register>` (code: 0x14 (0x1C) ):\
    `ltr` operation writes 1 to register `dst` if the value of register `src`
    is less than the value of register `src2`, otherwise 0 is written.
    `ltsr` is the same operation, but performs a signed comparison.

7. `gt(s) <dst: register> <src: register> <value: integer>` (code: 0x15 (0x1D) ):\
    `gt` operation writes 1 to register `dst` if the value of register `src`
    is greater than `value`, otherwise 0 is written.
    `gts` is the same operation, but performs a signed comparison.

8. `gt(s)r <dst: register> <src: register> <src2: register>` (code: 0x16 (0x1E) ):\
    `gtr` operation writes 1 to register `dst` if the value of register `src`
    is greater than the value of register `src2`, otherwise 0 is written.
    `gtsr` is the same operation, but performs a signed comparison.

9. `le(s) <dst: register> <src: register> <value: integer>` (code: 0x17 (0x1F) ):\
    `le` operation writes 1 to register `dst` if the value of register `src`
    is less than or equal to `value`, otherwise 0 is written.
    `les` is the same operation, but performs a signed comparison.

10. `le(s)r <dst: register> <src: register> <src2: register>` (code: 0x18 (0x20) ):\
    `ler` operation writes 1 to register `dst` if the value of register `src`
    is less than or equal to the value of register `src2`, otherwise 0 is written.
    `lesr` is the same operation, but performs a signed comparison.

11. `ge(s) <dst: register> <src: register> <value: integer>` (code: 0x19 (0x21) ):\
    `ge` operation writes 1 to register `dst` if the value of register `src`
    is greater than or equal to `value`, otherwise 0 is written.
    `ges` is the same operation, but performs a signed comparison.

12. `ge(s)r <dst: register> <src: register> <src2: register>` (code: 0x1A (0x22) ):\
    `ger` operation writes 1 to register `dst` if the value of register `src`
    is greater than or equal to the value of register `src2`, otherwise 0
    is written.
    `gesr` is the same operation, but performs a signed comparison.

### Control flow operations

1. `mark <mark_name: string>` (code: 0x2):\
    `mark` operation binds its index in the operation sequence to `mark_name`
    so that this index can be jumped to from other parts of the program.

2. `goto <mark_name: string>` (code: 0xD):\
    `goto` operation jumps to an operation index bound to `mark_name`.

3. `cgoto <src: register> <mark_name: string>` (code: 0xE):\
    `cgoto` operation jumps to an operation index bound to `mark_name`
    only if the value of register `src` is non-zero, otherwise this operation
    is a no-op.

4. `call <mark_name: string>` (code: 0x36):\
    `call` operation saves current execution frame
    (e.g. current operation index and stack head position) on the stack
    and jumps to an operation index bound to `mark_name`.

5. `ret` (code: 0x37):\
    `ret` operation rewinds the execution state back to the last saved one
    on the stack, jumping back to operation index fetched from the frame
    and setting stack head to the one fetched from the frame.

6. `end` (code: 0x1):\
    `end` operation gracefully stops the execution. It is effectively 
    an alias to the following sequence of operations:
    `set r0 0 sys exit`.\
    This operation is implicitly placed by the assembler at the end of the
    execution sequence, so one is not required to specify it themselves.

### Operations on the stack

1. `alloc <size: integer> <n: integer>` (code: 0x44):\
    `alloc` operation allocates on the stack `n` items of size defined by `size`.
    The program cannot assume anything about the values the items are initialized with.
    `size` can only be either 1, 2, 4 or 8.

2. `allocr <size: register> <src: register>` (code: 0x45):\
    `allocr` operation allocates items on the stack, their size is specified by `size`, and amount of them
    is defined by the value of register `src`. The program cannot assume anything about the values the items are initialized with.
    `size` can only be either 1, 2, 4 or 8.

3. `push64 <src: register>` (code: 0x23):\
    `push64` operation pushes the value of register `src` to the stack.

4. `pop64 <dst: register>` (code: 0x24):\
    `pop64` operation pops 8 bytes from the stack and writes them to
    register `dst`.

5. `push32 <src: register>` (code: 0x25):\
    `push32` operation pushes the 32 lower bits of register `src` to the stack.

6. `pop32 <dst: register>` (code: 0x26):\
    `pop32` operation pops 4 bytes from the stack and writes them to
    register `dst`. The 32 higher bits of the register are zeroed out.

7. `push16 <src: register>` (code: 0x27):\
    `push16` operation pushes the 16 lower bits of register `src` to the stack.

8. `pop16 <dst: register>` (code: 0x28):\
    `pop16` operation pops 2 bytes from the stack and writes them to
    register `dst`. The 48 higher bits of the register are zeroed out.

9. `push8 <src: register>` (code: 0x29):\
    `push8` operation pushes the 8 lower bits of register `src` to the stack.

10. `pop8 <dst: register>` (code: 0x2A):\
    `pop8` operation pops 1 byte from the stack and writes it to
    register `dst`. The 56 higher bits of the register are zeroed out.

### Memory manipulation operations

1. `setd <dst: register> <block_name: string>` (code: 0x5):\
    `setd` operation copies address the of data block `block_name` to 
    register `dst`.\
    The difference between memory and data blocks is that data blocks contain predefined data, while memory blocks are simply zero-initialized blocks of memory that are free to be written to.

2. `setm <dst: register> <block_name: string>` (code: 0x7):\
    `setm` operation copies address of the memory block `block_name` to
    register `dst`.

3. `ld64 <dst: register> <src: register>` (code: 0x3A):\
    `ld64` operation loads a 64-bit value from the address in register `src`
    to register `dst`.

4. `str64 <dst: register> <src: register>` (code: 0x3B):\
    `str64` operation stores the value of register `src` at the address
    in register `dst`.

5. `ld32 <dst: register> <src: register>` (code: 0x3C):\
    `ld32` operation loads a 32-bit value from the address in register `src`
    to register `dst`. Higher 32 bits of register `dst` are zeroed out.

6. `str32 <dst: register> <src: register>` (code: 0x3D):\
    `str32` operation stores 32 lower bits of register `src` at the address
    in register `dst`.

7. `ld16 <dst: register> <src: register>` (code: 0x3E):\
    `ld16` operation loads a 16-bit value from the address in register `src`
    to register `dst`. Higher 48 bits of register `dst` are zeroed out.

8. `str16 <dst: register> <src: register>` (code: 0x3F):\
    `str16` operation stores 16 lower bits of register `src` at the address
    in register `dst`.

9. `ld8 <dst: register> <src: register>` (code: 0x40):\
    `ld8` operation loads a 8-bit value from the address in register `src`
    to register `dst`. Higher 56 bits of register `dst` are zeroed out.

10. `str8 <dst: register> <src: register>` (code: 0x41):\
    `str8` operation stores 8 lower bits of register `src` at the address
    in register `dst`.

11. `sets <dst: register> <value: integer>` (code: 0x42):\
    `sets` operation writes to register `dst` the sum of an address of the head of the stack
    and `value`.

12. `setsr <dst: register> <src: register>` (code: 0x43):\
    `setsr` operation writes to register `dst` the sum of an address of the head of the stack
    and the value of register `src`.
    
### System interaction and platform-specific operations

1. `setb <dst: register> <builtin_name: string>` (code: 0x6):\
    `setb` operation copies the runtime value of the built-in value `builtin_name`
    to register `dst`.
    See (Built-ins)[#built-ins] for further information and all defined built-ins.

2. `sys <sys_op_name: string>` (code: 0xC):\
    `sys` operation performs a cross-platform system call bound to `sys_op_name`.
    See (System Calls)[#system-calls] for all defined system calls.

## Built-ins

**Built-ins** are a crossplatform way to get platform- and execution-specific
data, they may have different values on different systems and between different
executions, but they are bound to a name that is constant and universal.

### All defined built-ins

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

## System Calls

**System Calls** are a crossplatform way to interact with the current system,
they might have different call conventions and implementations on different systems,
but a BRF system call is used the same way on any system that BRF supports.

### All defined system calls

1. `exit` (index: 0x1):\
    `exit` system call sets the exit code and stops the execution.
    Arguments:\
        `<r0>` - an 8-bit unsigned integer that specifies the exit code.

2. `write` (index: 0x2):\
    `write` system call writes data to a file descriptor.\
    Arguments:\
        `<r0>` - file descriptor identifier, can be obtained using 
            `open` system call, or can be one a built-in `stdout` or `stderr`.
        `<r1>` - pointer to the data to be written to the file descriptor.
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