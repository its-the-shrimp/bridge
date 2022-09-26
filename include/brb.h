#ifndef _BRB_
#define _BRB_

#include <br.h>
#define ARENA_ASSERT(expr) assert(expr, #expr)
#include <external/arena.h>
#include <brp.h>

typedef enum {
/* 
Data Types:
 	i8 - a byte
	i16 - a 2-byte value
	i32 - a 4-byte value
	ptr - a pointer-sized value
	i64 - an 8-byte value
	int - any of the above

*/
	BRB_OP_NOP,       // [] -> nop -> []
	// do nothing
	BRB_OP_END,       // [] -> end -> []
	// stop execution of the program
	BRB_OP_I8,        // [] -> i8 <x> -> [i8]
	// push 1 byte-sized integer literal <x> onto the stack
	BRB_OP_I16,       // [] -> i16 <x> -> [i16]
	// push 2 byte-sized integer literal <x> onto the stack
	BRB_OP_I32,       // [] -> i32 <x> -> [i32]
	// push 4 byte-sized integer literal <x> onto the stack
	BRB_OP_PTR,       // [] -> ptr <x> -> [ptr]
	// pushes pointer-sized integer literal <x> onto the stack
	BRB_OP_I64,       // [] -> i64 <x> -> [i64]
	// push 8 byte-sized integer literal <x> onto the stack
	BRB_OP_ADDR,      // [A, *] -> addr <i> -> [A, *, ptr]
	// push address of the stack item at index <i> onto the stack; <i> refers to the index on the stack after pushing the address
	BRB_OP_DBADDR,    // [] -> dbaddr <i> -> [ptr]
	// push address of data block at index <i> onto the stack
	BRB_OP_LD,        // [A:ptr] -> ld <T> -> [<T>]
	// replace address A with item of type <T> loaded from it
	BRB_OP_STR,       // [A:ptr, B] -> str -> []
	// store B at the address A; same as (void)(*A = B)
	BRB_OP_SYS,
	// execute system procedure <f> with arguments from the stack
	/* System calls:
		[A:ptr] -> sys exit // exits the program with exit code A
		[A:ptr, B:ptr, C:ptr] -> sys write -> [ptr] // write C bytes from address B to file descriptor A
		[A:ptr, B:ptr, C:ptr] -> sys read -> [ptr] // read C bytes from file descriptor A to address B
	*/
	BRB_OP_BUILTIN,   // [] -> builtin <id> -> [ptr]
	// place a pointer-sized built-in constant on top of the stack

	BRB_OP_ADD,       // [A:int, B:int] -> add -> [typeof A]
	// replace A and B with their sum, that is of the same type as A
	BRB_OP_ADDI, 	  // [A:int] -> add-i <n> -> [typeof A]
	// increment A by <n>
	BRB_OP_ADDIAT8,   // [A:ptr] -> add-i@8 <n> -> [i8]
	// increment an `i8` at address A by <n>, replace A with the resulting value; like *(uint8_t*)A += n
	BRB_OP_ADDIAT16,  // [A:ptr] -> add-i@16 <n> -> [i16]
	// increment an `i16` at address A by <n>, replace A with the resulting value; like *(uint16_t*)A += n
	BRB_OP_ADDIAT32,  // [A:ptr] -> add-i@32 <n> -> [i32]
	// increment an `i32` at address A by <n>, replace A with the resulting value; like *(uint32_t*)A += n
	BRB_OP_ADDIATP,   // [A:ptr] -> add-i@p <n>  -> [ptr]
	// increment a `ptr` at address A by <n>, replace A with the resulting value; like *(uintptr_t*)A += n
	BRB_OP_ADDIAT64,  // [A:ptr] -> add-i@64 <n> -> [i64]
	// increment an `i64` at address A by <n>, replace A with the resulting value; like *(uint64_t*)A += n

	BRB_OP_SUB,       // [A:int, B:int] -> sub -> [typeof A]
	// replace A and B with their difference, that is of the same type as A
	BRB_OP_SUBI, 	  // [A:int] -> sub-i <n> -> [typeof A]
	// decrement A by <n>
	BRB_OP_SUBIAT8,   // [A:ptr] -> sub-i@8 <n> -> [i8]
	// decrement an `i8` at address A by <n>, replace A with the resulting value; like *(uint8_t*)A -= n
	BRB_OP_SUBIAT16,  // [A:ptr] -> sub-i@16 <n> -> [i16]
	// decrement an `i16` at address A by <n>, replace A with the resulting value; like *(uint16_t*)A -= n
	BRB_OP_SUBIAT32,  // [A:ptr] -> sub-i@32 <n> -> [i32]
	// decrement an `i32` at address A by <n>, replace A with the resulting value; like *(uint32_t*)A -= n
	BRB_OP_SUBIATP,   // [A:ptr] -> sub-i@p <n>  -> [ptr]
	// decrement a `ptr` at address A by <n>, replace A with the resulting value; like *(uintptr_t*)A -= n
	BRB_OP_SUBIAT64,  // [A:ptr] -> sub-i@64 <n> -> [i64]
	// decrement an `i64` at address A by <n>, replace A with the resulting value; like *(uint64_t*)A -= n

	BRB_OP_MUL,       // [A:int, B:int] -> mul -> [typeof A]
	// replace A and B with their product, that is of the same type as A
	BRB_OP_MULI, 	  // [A:int] -> mul-i <n> -> [typeof A]
	// multiply A by <n>
	BRB_OP_MULIAT8,   // [A:ptr] -> mul-i@8 <n> -> [i8]
	// multiply an `i8` at address A by <n>, replaces A with the resulting value; like *(uint8_t*)A *= n
	BRB_OP_MULIAT16,  // [A:ptr] -> mul-i@16 <n> -> [i16]
	// multiply an `i16` at address A by <n>, replaces A with the resulting value; like *(uint16_t*)A *= n
	BRB_OP_MULIAT32,  // [A:ptr] -> mul-i@32 <n> -> [i32]
	// multiply an `i32` at address A by <n>, replaces A with the resulting value; like *(uint32_t*)A *= n
	BRB_OP_MULIATP,   // [A:ptr] -> mul-i@p <n>  -> [ptr]
	// multiply a `ptr` at address A by <n>, replaces A with the resulting value; like *(uintptr_t*)A *= n
	BRB_OP_MULIAT64,  // [A:ptr] -> mul-i@64 <n> -> [i64]
	// multiply an `i64` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A *= n

	BRB_OP_DIV,       // [A:int, B:int] -> div -> [typeof A]
	// replace A and B with their quotient; division is unsigned
	BRB_OP_DIVI, 	  // [A:int] -> div-i <n> -> [typeof A]
	// divide A by <n>; division is unsigned
	BRB_OP_DIVIAT8,   // [A:ptr] -> div-i@8 <n> -> [i8]
	// divide an `i8` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned
	BRB_OP_DIVIAT16,  // [A:ptr] -> div-i@16 <n> -> [i16]
	// divide an `i16` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned
	BRB_OP_DIVIAT32,  // [A:ptr] -> div-i@32 <n> -> [i32]
	// divide an `i32` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned
	BRB_OP_DIVIATP,   // [A:ptr] -> div-i@p <n>  -> [ptr]
	// divide a `ptr` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned
	BRB_OP_DIVIAT64,  // [A:ptr] -> div-i@64 <n> -> [i64]
	// divide an `i64` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned

	BRB_OP_DIVS,      // [A:int, B:int] -> divs -> [typeof A]
	// replace A and B with their quotient; divsision is signed
	BRB_OP_DIVSI, 	  // [A:int] -> divs-i <n> -> [typeof A]
	// divide A by <n>; division is signed
	BRB_OP_DIVSIAT8,  // [A:ptr] -> divs-i@8 <n> -> [i8]
	// divide an `i8` at address A by <n>, replaces A with the resulting value; like *(int8_t*)A /= n; division is signed
	BRB_OP_DIVSIAT16, // [A:ptr] -> divs-i@16 <n> -> [i16]
	// divide an `i16` at address A by <n>, replaces A with the resulting value; like *(int16_t*)A /= n; division is signed
	BRB_OP_DIVSIAT32, // [A:ptr] -> divs-i@32 <n> -> [i32]
	// divide an `i32` at address A by <n>, replaces A with the resulting value; like *(int32_t*)A /= n; division is signed
	BRB_OP_DIVSIATP,  // [A:ptr] -> divs-i@p <n>  -> [ptr]
	// divide a `ptr` at address A by <n>, replaces A with the resulting value; like *(intptr_t*)A /= n; division is signed
	BRB_OP_DIVSIAT64, // [A:ptr] -> divs-i@64 <n> -> [i64]
	// divide an `i64` at address A by <n>, replaces A with the resulting value; like *(int64_t*)A /= n; division is signed

	BRB_OP_MOD,       // [A:int, B:int] -> mod -> [typeof A]
	// replace A and B with the remainder of A / B; division is unsigned
	BRB_OP_MODI, 	  // [A:int] -> mod-i <n> -> [typeof A]
	// replace A with the remainder of A / <n>; division is unsigned
	BRB_OP_MODIAT8,   // [A:ptr] -> mod-i@8 <n> -> [i8]
	// replace an `i8` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uint8_t*)A %= n; division is unsigned
	BRB_OP_MODIAT16,  // [A:ptr] -> mod-i@16 <n> -> [i16]
	// replace an `i16` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uint16_t*)A %= n; division is unsigned
	BRB_OP_MODIAT32,  // [A:ptr] -> mod-i@32 <n> -> [i32]
	// replace an `i32` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uint32_t*)A %= n; division is unsigned
	BRB_OP_MODIATP,   // [A:ptr] -> mod-i@p <n>  -> [ptr]
	// replace an `ptr` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uintptr_t*)A %= n; division is unsigned
	BRB_OP_MODIAT64,  // [A:ptr] -> mod-i@64 <n> -> [i64]
	// replace an `i64` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uint64_t*)A %= n; division is unsigned

	BRB_OP_MODS,      // [A:int, B:int] -> mods -> [typeof A]
	// replace A and B with the remainder of A / B; division is signed
	BRB_OP_MODSI, 	  // [A:int] -> mods-i <n> -> [typeof A]
	// replace A with the remainder of A / <n>; division is signed
	BRB_OP_MODSIAT8,  // [A:ptr] -> mods-i@8 <n> -> [i8]
	// replace an `i8` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(int8_t*)A %= n; division is signed
	BRB_OP_MODSIAT16, // [A:ptr] -> mods-i@16 <n> -> [i16]
	// replace an `i16` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(int16_t*)A %= n; division is signed
	BRB_OP_MODSIAT32, // [A:ptr] -> mods-i@32 <n> -> [i32]
	// replace an `i32` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(int32_t*)A %= n; division is signed
	BRB_OP_MODSIATP,  // [A:ptr] -> mods-i@p <n>  -> [ptr]
	// replace an `ptr` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(intptr_t*)A %= n; division is signed
	BRB_OP_MODSIAT64, // [A:ptr] -> mods-i@64 <n> -> [i64]
	// replace an `i64` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(int64_t*)A %= n; division is signed

	BRB_OP_AND,       // [A:int, B:int] -> and -> [typeof A]
	// replace A and B with the result of bitwise AND operation on A and B
	BRB_OP_ANDI,      // [A:int] -> and-i <n> -> [typeof A]
	// replace A with the result of bitwise AND operation on A and <n>
	BRB_OP_ANDIAT8,   // [A:ptr] -> and-i@8 <n> -> [i8]
	// replace an `i8` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uint8_t*)A &= n
	BRB_OP_ANDIAT16,  // [A:ptr] -> and-i@16 <n> -> [i16]
	// replace an `i16` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uint16_t*)A &= n
	BRB_OP_ANDIAT32,  // [A:ptr] -> and-i@32 <n> -> [i32]
	// replace an `i32` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uint32_t*)A &= n
	BRB_OP_ANDIATP,   // [A:ptr] -> and-i@p <n> -> [ptr]
	// replace an `ptr` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uintptr_t*)A &= n
	BRB_OP_ANDIAT64,  // [A:ptr] -> and-i@64 <n> -> [i64]
	// replace an `i64` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uint64_t*)A &= n

	BRB_OP_OR,        // [A:int, B:int] -> or -> [typeof A]
	// replace A and B with the result of bitwise OR operation on A and B
	BRB_OP_ORI,       // [A:int] -> or-i <n> -> [typeof A]
	// replace A with the result of bitwise OR operation on A and <n>
	BRB_OP_ORIAT8,    // [A:ptr] -> or-i@8 <n> -> [i8]
	// replace an `i8` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uint8_t*)A |= n
	BRB_OP_ORIAT16,   // [A:ptr] -> or-i@16 <n> -> [i16]
	// replace an `i16` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uint16_t*)A |= n
	BRB_OP_ORIAT32,   // [A:ptr] -> or-i@32 <n> -> [i32]
	// replace an `i32` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uint32_t*)A |= n
	BRB_OP_ORIATP,    // [A:ptr] -> or-i@p <n> -> [ptr]
	// replace an `ptr` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uintptr_t*)A |= n
	BRB_OP_ORIAT64,   // [A:ptr] -> or-i@64 <n> -> [i64]
	// replace an `i64` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uint64_t*)A |= n

	BRB_OP_XOR,       // [A:int, B:int] -> xor -> [typeof A]
	// replace A and B with the result of bitwise XOR operation on A and B
	BRB_OP_XORI,      // [A:int] -> xor-i <n> -> [typeof A]
	// replace A with the result of bitwise XOR operation on A and <n>
	BRB_OP_XORIAT8,   // [A:ptr] -> xor-i@8 <n> -> [i8]
	// replace an `i8` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uint8_t*)A ^= n
	BRB_OP_XORIAT16,  // [A:ptr] -> xor-i@16 <n> -> [i16]
	// replace an `i16` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uint16_t*)A ^= n
	BRB_OP_XORIAT32,  // [A:ptr] -> xor-i@32 <n> -> [i32]
	// replace an `i32` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uint32_t*)A ^= n
	BRB_OP_XORIATP,   // [A:ptr] -> xor-i@p <n> -> [ptr]
	// replace an `ptr` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uintptr_t*)A ^= n
	BRB_OP_XORIAT64,  // [A:ptr] -> xor-i@64 <n> -> [i64]
	// replace an `i64` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uint64_t*)A ^= n

	BRB_OP_SHL,      // [A:int, B:int] -> shl -> [typeof A]
	// replace A and B by A shifted by B bits to the left; B must be in range [0, 64)
	BRB_OP_SHLI,     // [A:int] -> shl-i <n> -> [typeof A]
	// shift A by <n> bits to the left; <n> must be in range [0, 64)
	BRB_OP_SHLIAT8,   // [A:ptr] -> shl-i@8 <n> -> [i8]
	// shift an `i8` at address A by <n> bits to the left, replace A with the resulting value; like *(uint8_t*)A <<= n; <n> must be in range [0, 64)
	BRB_OP_SHLIAT16,  // [A:ptr] -> shl-i@16 <n> -> [i16]
	// shift an `i16` at address A by <n> bits to the left, replace A with the resulting value; like *(uint16_t*)A <<= n; <n> must be in range [0, 64)
	BRB_OP_SHLIAT32,  // [A:ptr] -> shl-i@32 <n> -> [i32]
	// shift an `i32` at address A by <n> bits to the left, replace A with the resulting value; like *(uint32_t*)A <<= n; <n> must be in range [0, 64)
	BRB_OP_SHLIATP,   // [A:ptr] -> shl-i@p <n> -> [ptr]
	// shift an `ptr` at address A by <n> bits to the left, replace A with the resulting value; like *(uintptr_t*)A <<= n; <n> must be in range [0, 64)
	BRB_OP_SHLIAT64,  // [A:ptr] -> shl-i@64 <n> -> [i64]
	// shift an `i64` at address A by <n> bits to the left, replace A with the resulting value; like *(uint64_t*)A <<= n; <n> must be in range [0, 64)

	BRB_OP_SHR,      // [A:int, B:int] -> shr -> [typeof A]
	// replace A and B by A shifted by B bits to the right, shifting in zeros; B must be in range [0, 64)
	BRB_OP_SHRI,     // [A:int] -> shr-i <n> -> [typeof A]
	// shift A by <n> bits to the right, shifting in zeros; <n> must be in range [0, 64)
	BRB_OP_SHRIAT8,   // [A:ptr] -> shr-i@8 <n> -> [i8]
	// shift an `i8` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uint8_t*)A >>= n; <n> must be in range [0, 64)
	BRB_OP_SHRIAT16,  // [A:ptr] -> shr-i@16 <n> -> [i16]
	// shift an `i16` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uint16_t*)A >>= n; <n> must be in range [0, 64)
	BRB_OP_SHRIAT32,  // [A:ptr] -> shr-i@32 <n> -> [i32]
	// shift an `i32` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uint32_t*)A >>= n; <n> must be in range [0, 64)
	BRB_OP_SHRIATP,   // [A:ptr] -> shr-i@p <n> -> [ptr]
	// shift an `ptr` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uintptr_t*)A >>= n; <n> must be in range [0, 64)
	BRB_OP_SHRIAT64,  // [A:ptr] -> shr-i@64 <n> -> [i64]
	// shift an `i64` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uint64_t*)A >>= n; <n> must be in range [0, 64)

	BRB_OP_SHRS,     // [A:int, B:int] -> shrs -> [typeof A]
	// replace A and B by A shifted by B bits to the right, shifting in copies of the sign bit; B must be in range [0, 64)
	BRB_OP_SHRSI,    // [A:int] -> shrs-i <n> -> [typeof A]
	// shift A by <n> bits to the right, shifting in copies of the sign bit; <n> must be in range [0, 64)
	BRB_OP_SHRSIAT8,  // [A:ptr] -> shrs-i@8 <n> -> [i8]
	// shift an `i8` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(int8_t*)A >>= n; <n> must be in range [0, 64)
	BRB_OP_SHRSIAT16, // [A:ptr] -> shrs-i@16 <n> -> [i16]
	// shift an `i16` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(int16_t*)A >>= n; <n> must be in range [0, 64)
	BRB_OP_SHRSIAT32, // [A:ptr] -> shrs-i@32 <n> -> [i32]
	// shift an `i32` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(int32_t*)A >>= n; <n> must be in range [0, 64)
	BRB_OP_SHRSIATP,  // [A:ptr] -> shrs-i@p <n> -> [ptr]
	// shift an `ptr` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(intptr_t*)A >>= n; <n> must be in range [0, 64)
	BRB_OP_SHRSIAT64, // [A:ptr] -> shrs-i@64 <n> -> [i64]
	// shift an `i64` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(int64_t*)A >>= n; <n> must be in range [0, 64)

	BRB_OP_NOT,      // [A:int] -> not -> [typeof A]
	// invert the bits of A
	BRB_OP_NOTAT8,    // [A:ptr] -> not-@8 -> [i8]
	// invert the bits of an `i8` at address A, replace A with the result; like *(uint8_t*)A = ~*(uint8_t*)A
	BRB_OP_NOTAT16,   // [A:ptr] -> not-@16 -> [i16]
	// invert the bits of an `i16` at address A, replace A with the result; like *(uint16_t*)A = ~*(uint16_t*)A
	BRB_OP_NOTAT32,   // [A:ptr] -> not-@32 -> [i32]
	// invert the bits of an `i32` at address A, replace A with the result; like *(uint32_t*)A = ~*(uint32_t*)A
	BRB_OP_NOTATP,    // [A:ptr] -> not-@p -> [ptr]
	// invert the bits of an `ptr` at address A, replace A with the result; like *(uintptr_t*)A = ~*(uintptr_t*)A
	BRB_OP_NOTAT64,   // [A:ptr] -> not-@64 -> [i64]
	// invert the bits of an `i64` at address A, replace A with the result; like *(uint64_t*)A = ~*(uint64_t*)A

	BRB_OP_DROP,     // [A:any] -> drop -> []
	// deletes A from the stack
/* TODO:
	BRB_OP_NEW,      // [] -> new <T> -> [<T>]
	// create new item of type <T> on top of the stack; contents of the item is undefined
	BRB_OP_ZERO,     // [] -> zero <T> -> [<T>]
	// create new item of type <T> on top of the stack, with every byte initialized to 0
	BRB_OP_DUP,      // [A:x...] -> dup <i> -> [A:x...A:x]
	// duplicates stack item at index <i> on top of the stack
	BRB_OP_SWAP,     // [A:x...B:x] -> swap <i> -> [B:x...A:x]
	// swaps the contents of A and B, A being the stack head and B being at index <i>
	BRB_OP_SLICE,    // [A] -> slice <n, offset> -> [A]
	// replaces A with its slice of <n> bytes, starting from <offset>
	BRB_OP_JOIN,     // [...] -> join <n> -> [A]
	// combine <n> items on top of the stack into a single item

	BRB_OP_EQU,      // [A:x, B:x] -> equ -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if they are equal, and 0 otherwise
	BRB_OP_NEQ,      // [A:x, B:x] -> neq -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if they are equal, and 0 otherwise
	BRB_OP_LTU,      // [A:int, B:int] -> ltu -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A < B, and 0 otherwise; comparison is unsigned
	BRB_OP_LTS,      // [A:int, B:int] -> lts -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A < B, and 0 otherwise; comparison is signed
	BRB_OP_GTU,      // [A:int, B:int] -> gtu -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A > B, and 0 otherwise; comparison is unsigned
	BRB_OP_GTS,      // [A:int, B:int] -> gts -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A > B, and 0 otherwise; comparison is signed
	BRB_OP_LEU,      // [A:int, B:int] -> leu -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A <= B, and 0 otherwise; comparison is unsigned
	BRB_OP_LES,      // [A:int, B:int] -> les -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A <= B, and 0 otherwise; comparison is signed
	BRB_OP_GEU,      // [A:int, B:int] -> geu -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A >= B, and 0 otherwise; comparison is unsigned
	BRB_OP_GES,      // [A:int, B:int] -> ges -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A >= B, and 0 otherwise; comparison is signed
	BRB_OP_GOTO,     // [] -> goto <i> -> []
	// jump to operation at index <i>
	BRB_OP_GOTOIF,   // [A:int] -> gotoif <i> -> []
	// jump to operation at index <i> if A is not zero
	BRB_OP_GOTOIFN,  // [A:int] -> gotoifn <i> -> []
	// jump to operation at index <i> if A is zero

	BRB_OP_CM,       // [A:ptr, B:int] -> cm <n> -> [A:ptr]
	// copy <n> bytes from address B to address A, pop B from the stack; the addresses must not overlap
	BRB_OP_CMV,      // [A;ptr, B:ptr, C:int] -> cm-v -> [A:ptr]
	// copy C bytes from address B to address A, pop B and C from the stack; the address must not overlap
	BRB_OP_CMO,      // [A:ptr, B:int] -> cm-o <n> -> [A:ptr]
	// copy <n> bytes from address B to address A, pop B from the stack; the addresses may overlap
	BRB_OP_CMOV,     // [A;ptr, B:ptr, C:int] -> cm-ov -> [A:ptr]
	// copy C bytes from address B to address A, pop B and C from the stack; the address may overlap
	BRB_OP_ZM,       // [A:ptr] -> zm <n> -> [A:ptr]
	// set <n> bytes at address A to zero
	BRB_OP_ZMV,      // [A:ptr, B:int] -> zm-v -> [A:ptr]
	// sets B bytes at address A to zero, pop B from the stack
	BRB_OP_FM,       // [A:ptr, B] -> fm <n> -> [A:ptr]
	// fill memory at address A with <n> copies of B, pop B from the stack
	BRB_OP_FMV,      // [A:ptr, B, C:int] -> fm-v -> [A:ptr]
	// fill memory at address A with C copies of B, pop B and C from the stack
	BRB_OP_LDI,	 // [A:ptr, B:int] -> ld-i <T> -> [<T>]
	// load item of type <T> from address A at index B
	BRB_OP_STRI,	 // [A:ptr, B:int, C] -> str-i -> []
	// write C to address A at index B

	BRB_OP_CI8,      // [A:int] -> ci8 -> [A:i8]
	// convert A to an `i8`
	BRB_OP_CI16,     // [A:int] -> ci16 -> [A:i16]
	// convert A to an `i16`, zero-extending if the resulting value is larger
	BRB_OP_CI16S,     // [A:int] -> ci16-s -> [A:i16]
	// convert A to an `i16`, sign-extending if the resulting value is larger
	BRB_OP_CI32,     // [A:int] -> ci32 -> [A:i32]
	// convert A to an `i32`, zero-extending if the resulting value is larger
	BRB_OP_CI32S,     // [A:int] -> ci32-s -> [A:i32]
	// convert A to an `i32`, sign-extending if the resulting value is larger
	BRB_OP_CIP,      // [A:int] -> cip ->  [A:ptr]
	// convert A to an `ptr`, zero-extending if the resulting value is larger
	BRB_OP_CIPS,      // [A:int] -> cip-s ->  [A:ptr]
	// convert A to an `ptr`, sign-extending if the resulting value is larger
	BRB_OP_CI64,     // [A:int] -> ci64 -> [A:i64]
	// convert A to an `i64`, zero-extending if the resulting value is larger
	BRB_OP_CI64S,     // [A:int] -> ci64-s -> [A:i64]
	// convert A to an `i64`, sign-extending if the resulting value is larger
*/
	BRB_N_OPS
} BRB_OpType;

extern const sbuf BRB_opNames[];
extern const uint64_t BRB_opFlags[];

typedef enum {
	BRB_OPERAND_NONE,
	BRB_OPERAND_INT8,
	BRB_OPERAND_INT,
	BRB_OPERAND_TYPE,
	BRB_OPERAND_VAR_NAME,
	BRB_OPERAND_DB_NAME,
	BRB_OPERAND_SYSCALL_NAME,
	BRB_OPERAND_BUILTIN,
} BRB_OperandType;
#define BRB_GET_OPERAND_TYPE(type) ((BRB_OperandType)(BRB_opFlags[type] & 7))

typedef enum {
	BRB_ADDR_NONE,
	BRB_ADDR_I8,
	BRB_ADDR_I16,
	BRB_ADDR_I32,
	BRB_ADDR_PTR,
	BRB_ADDR_I64,
} BRB_AddrOperandType;
#define BRB_GET_ADDR_OP_TYPE(type) ((BRB_AddrOperandType)((BRB_opFlags[type] >> 3) & 7))
#define BRB_OPERAND_ALLOCATED 64
#define BRB_GET_BASE_OP_TYPE(type) ((BRB_OpType)(BRB_opFlags[type] >> 7))

typedef enum {
	BRB_TYPE_I8,
	BRB_TYPE_I16,	
	BRB_TYPE_I32,
	BRB_TYPE_PTR,
	BRB_TYPE_I64,	
	BRB_TYPE_VOID,
	BRB_N_TYPE_KINDS
} BRB_TypeKind;

#ifdef _BRB_INTERNAL
#	define BRB_TYPE_ANY   1
#	define BRB_TYPE_INT   2
#	define BRB_TYPE_OF    3
#	define BRB_TYPE_INPUT 4
#	define BRB_ANY_TYPE  ((BRB_Type){.internal_kind = BRB_TYPE_ANY})
#	define BRB_INT_TYPE  ((BRB_Type){.internal_kind = BRB_TYPE_INT})
#	define BRB_TYPEOF(i) ((BRB_Type){.internal_kind = BRB_TYPE_OF, .n_items = i })
#	define BRB_INPUT_TYPE ((BRB_Type){.internal_kind = BRB_TYPE_INPUT})
// the `BRB_TYPE_OF` and `BRB_TYPE_INPUT` internal type kinds are only checked in the output types (those that are provided in `out_types`), in other cases they are ignored
#endif // _BRB_INTERNAL

extern const sbuf BRB_typeNames[];

#define BRB_I8_TYPE(n)  ((BRB_Type){.kind = BRB_TYPE_I8,  .n_items = n})
#define BRB_I16_TYPE(n) ((BRB_Type){.kind = BRB_TYPE_I16, .n_items = n})
#define BRB_I32_TYPE(n) ((BRB_Type){.kind = BRB_TYPE_I32, .n_items = n})
#define BRB_PTR_TYPE(n) ((BRB_Type){.kind = BRB_TYPE_PTR, .n_items = n})
#define BRB_I64_TYPE(n) ((BRB_Type){.kind = BRB_TYPE_I64, .n_items = n})
#define BRB_VOID_TYPE   ((BRB_Type){.kind = BRB_TYPE_VOID             })
typedef struct {
	BRB_TypeKind kind:8;
	uint8_t internal_kind;
	uint32_t n_items;
} BRB_Type;
static_assert(sizeof(BRB_Type) <= sizeof(uint64_t), "just for compactness");
declArray(BRB_Type);

typedef struct {
	BRB_OpType type:8;
	uint8_t x_op1_size; // only for the `add` operation and only during execution
	uint8_t x_op2_size;
	union {
		uint64_t operand_u;	
		int64_t operand_s;
		BRB_Type operand_type;
		void* operand_ptr;
	};
} BRB_Op;
static_assert(sizeof(BRB_Op) <= 16, "`sizeof(BRB_Op) > 16`, that's no good, gotta save up on that precious memory");
declArray(BRB_Op);

typedef struct {
	const char* name;
	BRB_OpArray body;
	BRB_Type ret_type;
	BRB_TypeArray args;
} BRB_Proc;
declArray(BRB_Proc);

// System Calls: crossplatform way to use system-dependent functionality
typedef enum {
	BRB_SYS_EXIT,
	BRB_SYS_WRITE,
	BRB_SYS_READ,
	BRB_N_SYSCALLS
} BRB_Syscall;

extern const sbuf BRB_syscallNames[];

extern const size_t BRB_syscallNArgs[];

// Built-ins: crossplatform way to get system-dependent constants
// all built-ins are of pointer size
typedef enum {
	BRB_BUILTIN_NULL,
	BRB_BUILTIN_STDIN,
	BRB_BUILTIN_STDOUT,
	BRB_BUILTIN_STDERR,
	BRB_N_BUILTINS
} BRB_Builtin;

extern const uintptr_t BRB_builtinValues[];
extern const sbuf BRB_builtinNames[];

typedef struct {
	const char* name;
	union {
		BRB_OpArray body;
		sbuf data;
	};
	bool is_mutable;
} BRB_DataBlock;
declArray(BRB_DataBlock);

typedef struct {
	BRB_ProcArray seg_exec;
	BRB_DataBlockArray seg_data;
	size_t exec_entry_point;
} BRB_Module;

#define BRB_HEADER_SIZE 8
#define BRB_V1_HEADER fromcstr("BRBv1\0\0\0")

typedef enum {
	BRB_ERR_OK,
	BRB_ERR_INVALID_HEADER,
	BRB_ERR_NO_HEADER,
	BRB_ERR_NO_DATA_SEG,
	BRB_ERR_NO_MEMORY,
	BRB_ERR_NO_EXEC_SEG,
	BRB_ERR_NO_OPCODE,
	BRB_ERR_INVALID_OPCODE,
	BRB_ERR_NO_OPERAND,
	BRB_ERR_INVALID_NAME,
	BRB_ERR_NAMES_NOT_RESOLVED,
	BRB_ERR_STACK_UNDERFLOW,
	BRB_ERR_OPERAND_OUT_OF_RANGE,
	BRB_ERR_NO_PROC_RET_TYPE,
	BRB_ERR_NO_PROC_NAME,
	BRB_ERR_NO_PROC_ARG,
	BRB_ERR_NO_PROC_BODY_SIZE,
	BRB_ERR_NO_DB_NAME,
	BRB_ERR_NO_DB_BODY_SIZE,
	BRB_ERR_NO_ENTRY,
	BRB_ERR_INVALID_ENTRY,
	BRB_ERR_INVALID_ENTRY_PROTOTYPE,
	BRB_ERR_TYPE_EXPECTED,
	BRB_ERR_OP_NAME_EXPECTED,
	BRB_ERR_INT_OPERAND_EXPECTED,
	BRB_ERR_INT_OR_NAME_OPERAND_EXPECTED,
	BRB_ERR_BUILTIN_OPERAND_EXPECTED,
	BRB_ERR_TEXT_OPERAND_EXPECTED,
	BRB_ERR_INVALID_DECL,
	BRB_ERR_ARGS_EXPECTED,
	BRB_ERR_PROTOTYPE_MISMATCH,
	BRB_ERR_SYSCALL_NAME_EXPECTED,
	BRB_ERR_INVALID_ARRAY_SIZE_SPEC,
	BRB_ERR_TYPE_MISMATCH,
	BRB_ERR_DEL_ARGS,
	BRB_ERR_MODULE_LOAD_INTERRUPT,
	BRB_N_ERROR_TYPES
} BRB_ErrorType;

typedef struct {
	BRB_ErrorType type;
	uint8_t opcode;
	BRP_TokenLoc* loc;
	union {
		const char* name;
		char header[BRB_HEADER_SIZE];
		BRB_Builtin builtin_id;
		BRB_Syscall syscall_id;
		struct {
			uint32_t expected_stack_length;
			uint32_t actual_stack_length;
		};
		struct {
			BRB_Type expected_type;
			BRB_Type actual_type;
			uint32_t arg_id;
		};
		uint64_t operand;
	};
} BRB_Error;

typedef struct BRB_stacknode_t* BRB_StackNode;
struct BRB_stacknode_t {
	BRB_StackNode prev;
	const char* name;
	BRB_Type type;
	uint8_t flags;
};
declArray(BRB_StackNode);
declArray(BRB_StackNodeArray);

typedef struct {
	BRB_Module module;
	BRB_StackNodeArrayArray procs;
	BRB_StackNodeArrayArray data_blocks;
	BRB_Error error;
	Arena arena;
} BRB_ModuleBuilder;

typedef enum {
	BRB_EXC_CONTINUE,
	BRB_EXC_EXIT,
	BRB_EXC_END,
	BRB_EXC_INTERRUPT,
	BRB_EXC_UNKNOWN_OP,
	BRB_EXC_STACK_OVERFLOW,
	N_BRB_EXCS
} BRB_ExecStatusType;

typedef struct {
	BRB_ExecStatusType type;
	uint8_t exit_code; // for BRB_EXC_EXIT
} BRB_ExecStatus;

declArray(sbuf);
#define BRB_DEFAULT_STACK_SIZE (512 * 1024) /* 512 KBs, just like in JVM */
typedef struct {
	sbufArray seg_data;
	BRB_ProcArray seg_exec;
	const BRB_Op* cur_proc;
	sbuf stack;
	char* stack_head;
	uint32_t exec_index;
	BRB_ExecStatus exec_status;
	sbuf* exec_argv;
	uint32_t exec_argc;
	uint32_t entry_point;
} BRB_ExecEnv;

typedef int64_t BRB_id_t; // an ID of either a procedure or a data block
#define BRB_INVALID_ID INT64_MIN

typedef const char** field;
declArray(field);
defArray(field);
// implemented in `src/brb_core.c`
void       BRB_printErrorMsg(FILE* dst, BRB_Error err, const char* prefix);
char*      BRB_getErrorMsg(BRB_Error err, const char* prefix);

fieldArray BRB_getNameFields(BRB_Module* module);
FILE*      BRB_findModule(const char* module_name, const char* search_paths[]);

BRB_Error  BRB_initModuleBuilder(BRB_ModuleBuilder* builder);
BRB_Error  BRB_analyzeModule(const BRB_Module* module, BRB_ModuleBuilder* dst);
BRB_Error  BRB_extractModule(BRB_ModuleBuilder builder, BRB_Module* dst);
BRB_Error  BRB_setEntryPoint(BRB_ModuleBuilder* builder, size_t proc_id);
BRB_Error  BRB_addOp(BRB_ModuleBuilder* builder, BRB_id_t proc_id, BRB_Op op);

BRB_Error  BRB_preallocProcs(BRB_ModuleBuilder* builder, uint32_t n_procs_hint);
void       BRB_deallocProcs(BRB_Module* module);
BRB_Error  BRB_addProc(BRB_ModuleBuilder* builder, BRB_id_t* proc_id_p, const char* name, size_t n_args, BRB_Type* args, BRB_Type ret_type, uint32_t n_ops_hint);
BRB_id_t   BRB_getProcIdByName(BRB_Module* module, const char* name); // returns BRB_INVALID_ID on error

BRB_Error  BRB_preallocDataBlocks(BRB_ModuleBuilder* builder, uint32_t n_dbs_hint);
void       BRB_deallocDataBlocks(BRB_Module* module);
BRB_Error  BRB_addDataBlock(BRB_ModuleBuilder* builder, BRB_id_t* db_id_p, const char* name, bool is_mutable, uint32_t n_pieces_hint);
BRB_id_t   BRB_getDataBlockIdByName(BRB_Module* module, const char* name); // returns BRB_INVALID_ID on error

BRB_Op*    BRB_getOp(BRB_Module* module, BRB_id_t proc_id, uint32_t op_id);
BRB_Error  BRB_labelStackItem(BRB_ModuleBuilder* builder, BRB_id_t proc_id, uint32_t op_id, uint32_t item_id, const char* name);
bool       BRB_getStackItemType(BRB_ModuleBuilder* builder, BRB_Type* dst, BRB_id_t proc_id, uint32_t op_id, uint32_t item_id);
size_t     BRB_getStackItemRTOffset(BRB_ModuleBuilder* builder, BRB_id_t proc_id, uint32_t op_id, size_t item_id); // if `item_id` is not SIZE_MAX, returns SIZE_MAX on error
size_t     BRB_getStackRTSize(BRB_ModuleBuilder* builder, BRB_id_t proc_id, uint32_t op_id);
size_t     BRB_getMaxStackRTSize(BRB_ModuleBuilder* builder, BRB_id_t proc_id);
size_t     BRB_getStackItemRTSize(BRB_ModuleBuilder* builder, BRB_id_t proc_id, uint32_t op_id, uint32_t item_id); // returns SIZE_MAX on error
ssize_t    BRB_getStackRTSizeDiff(BRB_ModuleBuilder* builder, BRB_id_t proc_id, uint32_t op_id);
size_t     BRB_getStackItemIdByName(BRB_ModuleBuilder* builder, BRB_id_t proc_id, uint32_t op_id, const char* name); // returns SIZE_MAX on error
size_t     BRB_getTypeRTSize(BRB_Type type);

// implemented in `src/brb_write.c`
long       BRB_writeModule(BRB_Module src, FILE* dst);

// implemented in `src/brb_load.c`
BRB_Error  BRB_loadModule(FILE* src, BRB_Module* dst);

// implemented in `src/brb_exec.c`
BRB_Error  BRB_execModule(BRB_Module module, BRB_ExecEnv* env, char* args[], size_t stack_size, const volatile bool* interruptor);
void       BRB_delExecEnv(BRB_ExecEnv* env);

// implemented in `src/brb_asm.c`
BRB_Error  BRB_assembleModule(FILE* input, const char* input_name, BRB_Module* dst);

// implemented in `src/brb_dis.c`
long       BRB_printType(BRB_Type type, FILE* dst);
long       BRB_disassembleModule(const BRB_Module* module, FILE* dst);

// implemented in `src/brb_compile.c`
long       BRB_compileModule_darwin_arm64(const BRB_Module* module, FILE* dst);

#endif // _BRB_
