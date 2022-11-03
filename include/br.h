#ifndef _BRIDGE_
#define _BRIDGE_

#ifdef BRIDGE_IMPLEMENTATION
#define SBUF_IMPLEMENTATION
#define BR_UTILS_IMPLEMENTATION
#endif

#include <br_utils.h>
#include <datasets.h>
#include <sbuf.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>
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
	BR_OP_NOP,       // [] -> nop -> []
	// do nothing
	BR_OP_END,       // [] -> end -> []
	// stop execution of the program
	BR_OP_I8,        // [] -> i8 <x> -> [i8]
	// push 1 byte-sized integer literal <x> onto the stack
	BR_OP_I16,       // [] -> i16 <x> -> [i16]
	// push 2 byte-sized integer literal <x> onto the stack
	BR_OP_I32,       // [] -> i32 <x> -> [i32]
	// push 4 byte-sized integer literal <x> onto the stack
	BR_OP_PTR,       // [] -> ptr <x> -> [ptr]
	// pushes pointer-sized integer literal <x> onto the stack
	BR_OP_I64,       // [] -> i64 <x> -> [i64]
	// push 8 byte-sized integer literal <x> onto the stack
	BR_OP_ADDR,      // [A, *] -> addr <i> -> [A, *, ptr]
	// push address of the stack item at index <i> onto the stack; <i> refers to the index on the stack after pushing the address
	BR_OP_DBADDR,    // [] -> dbaddr <i> -> [ptr]
	// push address of data block at index <i> onto the stack
	BR_OP_SYS,
	// execute system procedure <f> with arguments from the stack
	/* System calls:
		[A:ptr] -> sys exit // exits the program with exit code A
		[A:ptr, B:ptr, C:ptr] -> sys write -> [ptr] // write C bytes from address B to file descriptor A
		[A:ptr, B:ptr, C:ptr] -> sys read -> [ptr] // read C bytes from file descriptor A to address B
	*/
	BR_OP_BUILTIN,   // [] -> builtin <id> -> [ptr]
	// place a pointer-sized built-in constant on top of the stack

	BR_OP_ADD,       // [A:int, B:int] -> add -> [typeof A]
	// replace A and B with their sum, that is of the same type as A
	BR_OP_ADDI, 	  // [A:int] -> add-i <n> -> [typeof A]
	// increment A by <n>
	BR_OP_ADDIAT8,   // [A:ptr] -> add-i@8 <n> -> [i8]
	// increment an `i8` at address A by <n>, replace A with the resulting value; like *(uint8_t*)A += n
	BR_OP_ADDIAT16,  // [A:ptr] -> add-i@16 <n> -> [i16]
	// increment an `i16` at address A by <n>, replace A with the resulting value; like *(uint16_t*)A += n
	BR_OP_ADDIAT32,  // [A:ptr] -> add-i@32 <n> -> [i32]
	// increment an `i32` at address A by <n>, replace A with the resulting value; like *(uint32_t*)A += n
	BR_OP_ADDIATP,   // [A:ptr] -> add-i@p <n>  -> [ptr]
	// increment a `ptr` at address A by <n>, replace A with the resulting value; like *(uintptr_t*)A += n
	BR_OP_ADDIAT64,  // [A:ptr] -> add-i@64 <n> -> [i64]
	// increment an `i64` at address A by <n>, replace A with the resulting value; like *(uint64_t*)A += n

	BR_OP_SUB,       // [A:int, B:int] -> sub -> [typeof A]
	// replace A and B with their difference, that is of the same type as A
	BR_OP_SUBI, 	  // [A:int] -> sub-i <n> -> [typeof A]
	// decrement A by <n>
	BR_OP_SUBIAT8,   // [A:ptr] -> sub-i@8 <n> -> [i8]
	// decrement an `i8` at address A by <n>, replace A with the resulting value; like *(uint8_t*)A -= n
	BR_OP_SUBIAT16,  // [A:ptr] -> sub-i@16 <n> -> [i16]
	// decrement an `i16` at address A by <n>, replace A with the resulting value; like *(uint16_t*)A -= n
	BR_OP_SUBIAT32,  // [A:ptr] -> sub-i@32 <n> -> [i32]
	// decrement an `i32` at address A by <n>, replace A with the resulting value; like *(uint32_t*)A -= n
	BR_OP_SUBIATP,   // [A:ptr] -> sub-i@p <n>  -> [ptr]
	// decrement a `ptr` at address A by <n>, replace A with the resulting value; like *(uintptr_t*)A -= n
	BR_OP_SUBIAT64,  // [A:ptr] -> sub-i@64 <n> -> [i64]
	// decrement an `i64` at address A by <n>, replace A with the resulting value; like *(uint64_t*)A -= n

	BR_OP_MUL,       // [A:int, B:int] -> mul -> [typeof A]
	// replace A and B with their product, that is of the same type as A
	BR_OP_MULI, 	  // [A:int] -> mul-i <n> -> [typeof A]
	// multiply A by <n>
	BR_OP_MULIAT8,   // [A:ptr] -> mul-i@8 <n> -> [i8]
	// multiply an `i8` at address A by <n>, replaces A with the resulting value; like *(uint8_t*)A *= n
	BR_OP_MULIAT16,  // [A:ptr] -> mul-i@16 <n> -> [i16]
	// multiply an `i16` at address A by <n>, replaces A with the resulting value; like *(uint16_t*)A *= n
	BR_OP_MULIAT32,  // [A:ptr] -> mul-i@32 <n> -> [i32]
	// multiply an `i32` at address A by <n>, replaces A with the resulting value; like *(uint32_t*)A *= n
	BR_OP_MULIATP,   // [A:ptr] -> mul-i@p <n>  -> [ptr]
	// multiply a `ptr` at address A by <n>, replaces A with the resulting value; like *(uintptr_t*)A *= n
	BR_OP_MULIAT64,  // [A:ptr] -> mul-i@64 <n> -> [i64]
	// multiply an `i64` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A *= n

	BR_OP_DIV,       // [A:int, B:int] -> div -> [typeof A]
	// replace A and B with their quotient; division is unsigned
	BR_OP_DIVI, 	  // [A:int] -> div-i <n> -> [typeof A]
	// divide A by <n>; division is unsigned
	BR_OP_DIVIAT8,   // [A:ptr] -> div-i@8 <n> -> [i8]
	// divide an `i8` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned
	BR_OP_DIVIAT16,  // [A:ptr] -> div-i@16 <n> -> [i16]
	// divide an `i16` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned
	BR_OP_DIVIAT32,  // [A:ptr] -> div-i@32 <n> -> [i32]
	// divide an `i32` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned
	BR_OP_DIVIATP,   // [A:ptr] -> div-i@p <n>  -> [ptr]
	// divide a `ptr` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned
	BR_OP_DIVIAT64,  // [A:ptr] -> div-i@64 <n> -> [i64]
	// divide an `i64` at address A by <n>, replaces A with the resulting value; like *(uint64_t*)A /= n; division is unsigned

	BR_OP_DIVS,      // [A:int, B:int] -> divs -> [typeof A]
	// replace A and B with their quotient; divsision is signed
	BR_OP_DIVSI, 	  // [A:int] -> divs-i <n> -> [typeof A]
	// divide A by <n>; division is signed
	BR_OP_DIVSIAT8,  // [A:ptr] -> divs-i@8 <n> -> [i8]
	// divide an `i8` at address A by <n>, replaces A with the resulting value; like *(int8_t*)A /= n; division is signed
	BR_OP_DIVSIAT16, // [A:ptr] -> divs-i@16 <n> -> [i16]
	// divide an `i16` at address A by <n>, replaces A with the resulting value; like *(int16_t*)A /= n; division is signed
	BR_OP_DIVSIAT32, // [A:ptr] -> divs-i@32 <n> -> [i32]
	// divide an `i32` at address A by <n>, replaces A with the resulting value; like *(int32_t*)A /= n; division is signed
	BR_OP_DIVSIATP,  // [A:ptr] -> divs-i@p <n>  -> [ptr]
	// divide a `ptr` at address A by <n>, replaces A with the resulting value; like *(intptr_t*)A /= n; division is signed
	BR_OP_DIVSIAT64, // [A:ptr] -> divs-i@64 <n> -> [i64]
	// divide an `i64` at address A by <n>, replaces A with the resulting value; like *(int64_t*)A /= n; division is signed

	BR_OP_MOD,       // [A:int, B:int] -> mod -> [typeof A]
	// replace A and B with the remainder of A / B; division is unsigned
	BR_OP_MODI, 	  // [A:int] -> mod-i <n> -> [typeof A]
	// replace A with the remainder of A / <n>; division is unsigned
	BR_OP_MODIAT8,   // [A:ptr] -> mod-i@8 <n> -> [i8]
	// replace an `i8` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uint8_t*)A %= n; division is unsigned
	BR_OP_MODIAT16,  // [A:ptr] -> mod-i@16 <n> -> [i16]
	// replace an `i16` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uint16_t*)A %= n; division is unsigned
	BR_OP_MODIAT32,  // [A:ptr] -> mod-i@32 <n> -> [i32]
	// replace an `i32` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uint32_t*)A %= n; division is unsigned
	BR_OP_MODIATP,   // [A:ptr] -> mod-i@p <n>  -> [ptr]
	// replace an `ptr` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uintptr_t*)A %= n; division is unsigned
	BR_OP_MODIAT64,  // [A:ptr] -> mod-i@64 <n> -> [i64]
	// replace an `i64` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(uint64_t*)A %= n; division is unsigned

	BR_OP_MODS,      // [A:int, B:int] -> mods -> [typeof A]
	// replace A and B with the remainder of A / B; division is signed
	BR_OP_MODSI, 	  // [A:int] -> mods-i <n> -> [typeof A]
	// replace A with the remainder of A / <n>; division is signed
	BR_OP_MODSIAT8,  // [A:ptr] -> mods-i@8 <n> -> [i8]
	// replace an `i8` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(int8_t*)A %= n; division is signed
	BR_OP_MODSIAT16, // [A:ptr] -> mods-i@16 <n> -> [i16]
	// replace an `i16` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(int16_t*)A %= n; division is signed
	BR_OP_MODSIAT32, // [A:ptr] -> mods-i@32 <n> -> [i32]
	// replace an `i32` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(int32_t*)A %= n; division is signed
	BR_OP_MODSIATP,  // [A:ptr] -> mods-i@p <n>  -> [ptr]
	// replace an `ptr` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(intptr_t*)A %= n; division is signed
	BR_OP_MODSIAT64, // [A:ptr] -> mods-i@64 <n> -> [i64]
	// replace an `i64` at address A with the remainder of it divided by <n>, replaces A with the resulting value; like *(int64_t*)A %= n; division is signed

	BR_OP_AND,       // [A:int, B:int] -> and -> [typeof A]
	// replace A and B with the result of bitwise AND operation on A and B
	BR_OP_ANDI,      // [A:int] -> and-i <n> -> [typeof A]
	// replace A with the result of bitwise AND operation on A and <n>
	BR_OP_ANDIAT8,   // [A:ptr] -> and-i@8 <n> -> [i8]
	// replace an `i8` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uint8_t*)A &= n
	BR_OP_ANDIAT16,  // [A:ptr] -> and-i@16 <n> -> [i16]
	// replace an `i16` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uint16_t*)A &= n
	BR_OP_ANDIAT32,  // [A:ptr] -> and-i@32 <n> -> [i32]
	// replace an `i32` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uint32_t*)A &= n
	BR_OP_ANDIATP,   // [A:ptr] -> and-i@p <n> -> [ptr]
	// replace an `ptr` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uintptr_t*)A &= n
	BR_OP_ANDIAT64,  // [A:ptr] -> and-i@64 <n> -> [i64]
	// replace an `i64` at address A with the result of bitwise AND operation on the value and <n>, replace A with the resulting value; like *(uint64_t*)A &= n

	BR_OP_OR,        // [A:int, B:int] -> or -> [typeof A]
	// replace A and B with the result of bitwise OR operation on A and B
	BR_OP_ORI,       // [A:int] -> or-i <n> -> [typeof A]
	// replace A with the result of bitwise OR operation on A and <n>
	BR_OP_ORIAT8,    // [A:ptr] -> or-i@8 <n> -> [i8]
	// replace an `i8` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uint8_t*)A |= n
	BR_OP_ORIAT16,   // [A:ptr] -> or-i@16 <n> -> [i16]
	// replace an `i16` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uint16_t*)A |= n
	BR_OP_ORIAT32,   // [A:ptr] -> or-i@32 <n> -> [i32]
	// replace an `i32` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uint32_t*)A |= n
	BR_OP_ORIATP,    // [A:ptr] -> or-i@p <n> -> [ptr]
	// replace an `ptr` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uintptr_t*)A |= n
	BR_OP_ORIAT64,   // [A:ptr] -> or-i@64 <n> -> [i64]
	// replace an `i64` at address A with the result of bitwise OR operation on the value and <n>, replace A with the resulting value; like *(uint64_t*)A |= n

	BR_OP_XOR,       // [A:int, B:int] -> xor -> [typeof A]
	// replace A and B with the result of bitwise XOR operation on A and B
	BR_OP_XORI,      // [A:int] -> xor-i <n> -> [typeof A]
	// replace A with the result of bitwise XOR operation on A and <n>
	BR_OP_XORIAT8,   // [A:ptr] -> xor-i@8 <n> -> [i8]
	// replace an `i8` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uint8_t*)A ^= n
	BR_OP_XORIAT16,  // [A:ptr] -> xor-i@16 <n> -> [i16]
	// replace an `i16` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uint16_t*)A ^= n
	BR_OP_XORIAT32,  // [A:ptr] -> xor-i@32 <n> -> [i32]
	// replace an `i32` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uint32_t*)A ^= n
	BR_OP_XORIATP,   // [A:ptr] -> xor-i@p <n> -> [ptr]
	// replace an `ptr` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uintptr_t*)A ^= n
	BR_OP_XORIAT64,  // [A:ptr] -> xor-i@64 <n> -> [i64]
	// replace an `i64` at address A with the result of bitwise XOR operation on the value and <n>, replace A with the resulting value; like *(uint64_t*)A ^= n

	BR_OP_SHL,      // [A:int, B:int] -> shl -> [typeof A]
	// replace A and B by A shifted by B bits to the left; B must be in range [0, 64)
	BR_OP_SHLI,     // [A:int] -> shl-i <n> -> [typeof A]
	// shift A by <n> bits to the left; <n> must be in range [0, 64)
	BR_OP_SHLIAT8,   // [A:ptr] -> shl-i@8 <n> -> [i8]
	// shift an `i8` at address A by <n> bits to the left, replace A with the resulting value; like *(uint8_t*)A <<= n; <n> must be in range [0, 64)
	BR_OP_SHLIAT16,  // [A:ptr] -> shl-i@16 <n> -> [i16]
	// shift an `i16` at address A by <n> bits to the left, replace A with the resulting value; like *(uint16_t*)A <<= n; <n> must be in range [0, 64)
	BR_OP_SHLIAT32,  // [A:ptr] -> shl-i@32 <n> -> [i32]
	// shift an `i32` at address A by <n> bits to the left, replace A with the resulting value; like *(uint32_t*)A <<= n; <n> must be in range [0, 64)
	BR_OP_SHLIATP,   // [A:ptr] -> shl-i@p <n> -> [ptr]
	// shift an `ptr` at address A by <n> bits to the left, replace A with the resulting value; like *(uintptr_t*)A <<= n; <n> must be in range [0, 64)
	BR_OP_SHLIAT64,  // [A:ptr] -> shl-i@64 <n> -> [i64]
	// shift an `i64` at address A by <n> bits to the left, replace A with the resulting value; like *(uint64_t*)A <<= n; <n> must be in range [0, 64)

	BR_OP_SHR,      // [A:int, B:int] -> shr -> [typeof A]
	// replace A and B by A shifted by B bits to the right, shifting in zeros; B must be in range [0, 64)
	BR_OP_SHRI,     // [A:int] -> shr-i <n> -> [typeof A]
	// shift A by <n> bits to the right, shifting in zeros; <n> must be in range [0, 64)
	BR_OP_SHRIAT8,   // [A:ptr] -> shr-i@8 <n> -> [i8]
	// shift an `i8` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uint8_t*)A >>= n; <n> must be in range [0, 64)
	BR_OP_SHRIAT16,  // [A:ptr] -> shr-i@16 <n> -> [i16]
	// shift an `i16` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uint16_t*)A >>= n; <n> must be in range [0, 64)
	BR_OP_SHRIAT32,  // [A:ptr] -> shr-i@32 <n> -> [i32]
	// shift an `i32` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uint32_t*)A >>= n; <n> must be in range [0, 64)
	BR_OP_SHRIATP,   // [A:ptr] -> shr-i@p <n> -> [ptr]
	// shift an `ptr` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uintptr_t*)A >>= n; <n> must be in range [0, 64)
	BR_OP_SHRIAT64,  // [A:ptr] -> shr-i@64 <n> -> [i64]
	// shift an `i64` at address A by <n> bits to the right, shifting in zeros, replace A with the result; like *(uint64_t*)A >>= n; <n> must be in range [0, 64)

	BR_OP_SHRS,     // [A:int, B:int] -> shrs -> [typeof A]
	// replace A and B by A shifted by B bits to the right, shifting in copies of the sign bit; B must be in range [0, 64)
	BR_OP_SHRSI,    // [A:int] -> shrs-i <n> -> [typeof A]
	// shift A by <n> bits to the right, shifting in copies of the sign bit; <n> must be in range [0, 64)
	BR_OP_SHRSIAT8,  // [A:ptr] -> shrs-i@8 <n> -> [i8]
	// shift an `i8` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(int8_t*)A >>= n; <n> must be in range [0, 64)
	BR_OP_SHRSIAT16, // [A:ptr] -> shrs-i@16 <n> -> [i16]
	// shift an `i16` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(int16_t*)A >>= n; <n> must be in range [0, 64)
	BR_OP_SHRSIAT32, // [A:ptr] -> shrs-i@32 <n> -> [i32]
	// shift an `i32` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(int32_t*)A >>= n; <n> must be in range [0, 64)
	BR_OP_SHRSIATP,  // [A:ptr] -> shrs-i@p <n> -> [ptr]
	// shift an `ptr` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(intptr_t*)A >>= n; <n> must be in range [0, 64)
	BR_OP_SHRSIAT64, // [A:ptr] -> shrs-i@64 <n> -> [i64]
	// shift an `i64` at address A by <n> bits to the right, shifting in copies of the sign bit, replace A with the result; like *(int64_t*)A >>= n; <n> must be in range [0, 64)

	BR_OP_NOT,      // [A:int] -> not -> [typeof A]
	// invert the bits of A
	BR_OP_NOTAT8,    // [A:ptr] -> not-@8 -> [i8]
	// invert the bits of an `i8` at address A, replace A with the result; like *(uint8_t*)A = ~*(uint8_t*)A
	BR_OP_NOTAT16,   // [A:ptr] -> not-@16 -> [i16]
	// invert the bits of an `i16` at address A, replace A with the result; like *(uint16_t*)A = ~*(uint16_t*)A
	BR_OP_NOTAT32,   // [A:ptr] -> not-@32 -> [i32]
	// invert the bits of an `i32` at address A, replace A with the result; like *(uint32_t*)A = ~*(uint32_t*)A
	BR_OP_NOTATP,    // [A:ptr] -> not-@p -> [ptr]
	// invert the bits of an `ptr` at address A, replace A with the result; like *(uintptr_t*)A = ~*(uintptr_t*)A
	BR_OP_NOTAT64,   // [A:ptr] -> not-@64 -> [i64]
	// invert the bits of an `i64` at address A, replace A with the result; like *(uint64_t*)A = ~*(uint64_t*)A

	BR_OP_DROP,      // [A:any] -> drop -> []
	// deletes A from the stack
	BR_OP_NEW,       // [] -> new <T> -> [<T>]
	// create new item of type <T> on top of the stack; contents of the item is undefined
	BR_OP_ZERO,      // [] -> zero <T> -> [<T>]
	// create new item of type <T> on top of the stack, with every byte initialized to 0
	BR_OP_GET,       // [any[<i>], A:x] -> get <i> -> [A:x, any[<i>], A:x]
	// duplicates stack item at index <i> on top of the stack
	BR_OP_SETAT,     // [A:ptr, B] -> set-at -> [B]
	// store B at address A, pop A from the stack; A must not overlap with the address of B; same as `*A = B`
	BR_OP_GETFROM,   // [A:ptr] -> get-from <T> -> [<T>]
	// load an object of type <T> from address A, pop A from the stack; A must not overlap with its own address; same as `*(T*)A`
	BR_OP_COPY,      // [A:ptr, B:ptr] -> copy <T> -> [A:ptr]
	// copy an object of type <T> from address B to address A, pop B from the stack, addresses must not overlap; same as `memcpy(A, B, sizeof(T))`
/* TODO:
	BR_OP_SET,       // [A:x, any[<i> - 1], B:x] -> set -> [A:x, any[<i> - 1], A:x]
	// set the contents of A to that of B; A and B must be of the same type; <i> must not be 0
	BR_OP_COPYO,     // [A:ptr, B:ptr] -> copy-o <T> -> [A:ptr]
	// copy an object of type <T> from address B to address A, pop B from the stack, addresses may overlap; same as `memmove(A, B, sizeof(T))`
	BR_OP_GETITEM,   // [any[<i1>], [any[<i2>], A]] -> get-item <i1>[<i2>] -> [typeof A]
	// copies an subitem <i2> of stack item <i1> on top of the stack
	BR_OP_SETITEM,   // [A:x, any[<i1> - 1], [any[<i2>], B:x]] -> set-item <i1>[<i2>] -> [A:x, any[<i1> - 1], [any[<i2>], A:x]]
	

	BR_OP_EQU,       // [A:x, B:x] -> equ -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if they are equal, and 0 otherwise
	BR_OP_NEQ,       // [A:x, B:x] -> neq -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if they are equal, and 0 otherwise
	BR_OP_LTU,      // [A:int, B:int] -> ltu -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A < B, and 0 otherwise; comparison is unsigned
	BR_OP_LTS,      // [A:int, B:int] -> lts -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A < B, and 0 otherwise; comparison is signed
	BR_OP_GTU,      // [A:int, B:int] -> gtu -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A > B, and 0 otherwise; comparison is unsigned
	BR_OP_GTS,      // [A:int, B:int] -> gts -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A > B, and 0 otherwise; comparison is signed
	BR_OP_LEU,      // [A:int, B:int] -> leu -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A <= B, and 0 otherwise; comparison is unsigned
	BR_OP_LES,      // [A:int, B:int] -> les -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A <= B, and 0 otherwise; comparison is signed
	BR_OP_GEU,      // [A:int, B:int] -> geu -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A >= B, and 0 otherwise; comparison is unsigned
	BR_OP_GES,      // [A:int, B:int] -> ges -> i8
	// compare A and B and replace them with a byte, the value of which will be 1 if A >= B, and 0 otherwise; comparison is signed
	BR_OP_GOTO,     // [] -> goto <i> -> []
	// jump to operation at index <i>
	BR_OP_GOTOIF,   // [A:int] -> gotoif <i> -> []
	// jump to operation at index <i> if A is not zero
	BR_OP_GOTOIFN,  // [A:int] -> gotoifn <i> -> []
	// jump to operation at index <i> if A is zero

	BR_OP_CM,       // [A:ptr, B:int] -> cm <n> -> [A:ptr]
	// copy <n> bytes from address B to address A, pop B from the stack; the addresses must not overlap
	BR_OP_CMV,      // [A;ptr, B:ptr, C:int] -> cm-v -> [A:ptr]
	// copy C bytes from address B to address A, pop B and C from the stack; the address must not overlap
	BR_OP_CMO,      // [A:ptr, B:int] -> cm-o <n> -> [A:ptr]
	// copy <n> bytes from address B to address A, pop B from the stack; the addresses may overlap
	BR_OP_CMOV,     // [A;ptr, B:ptr, C:int] -> cm-ov -> [A:ptr]
	// copy C bytes from address B to address A, pop B and C from the stack; the address may overlap
	BR_OP_ZM,       // [A:ptr] -> zm <n> -> [A:ptr]
	// set <n> bytes at address A to zero
	BR_OP_ZMV,      // [A:ptr, B:int] -> zm-v -> [A:ptr]
	// sets B bytes at address A to zero, pop B from the stack
	BR_OP_FM,       // [A:ptr, B] -> fm <n> -> [A:ptr]
	// fill memory at address A with <n> copies of B, pop B from the stack
	BR_OP_FMV,      // [A:ptr, B, C:int] -> fm-v -> [A:ptr]
	// fill memory at address A with C copies of B, pop B and C from the stack

	BR_OP_CI8,      // [A:int] -> ci8 -> [A:i8]
	// convert A to an `i8`
	BR_OP_CI16,     // [A:int] -> ci16 -> [A:i16]
	// convert A to an `i16`, zero-extending if the resulting value is larger
	BR_OP_CI16S,     // [A:int] -> ci16-s -> [A:i16]
	// convert A to an `i16`, sign-extending if the resulting value is larger
	BR_OP_CI32,     // [A:int] -> ci32 -> [A:i32]
	// convert A to an `i32`, zero-extending if the resulting value is larger
	BR_OP_CI32S,     // [A:int] -> ci32-s -> [A:i32]
	// convert A to an `i32`, sign-extending if the resulting value is larger
	BR_OP_CIP,      // [A:int] -> cip ->  [A:ptr]
	// convert A to an `ptr`, zero-extending if the resulting value is larger
	BR_OP_CIPS,      // [A:int] -> cip-s ->  [A:ptr]
	// convert A to an `ptr`, sign-extending if the resulting value is larger
	BR_OP_CI64,     // [A:int] -> ci64 -> [A:i64]
	// convert A to an `i64`, zero-extending if the resulting value is larger
	BR_OP_CI64S,     // [A:int] -> ci64-s -> [A:i64]
	// convert A to an `i64`, sign-extending if the resulting value is larger
*/
	BR_N_OPS
} BR_OpType;
static_assert(BR_N_OPS <= 256, "time for some drastic changes in the encoding");

extern const sbuf BR_opNames[];
extern const uint64_t BR_opFlags[];

typedef enum {
	BR_OPERAND_NONE,
	BR_OPERAND_INT8,
	BR_OPERAND_INT,
	BR_OPERAND_TYPE,
	BR_OPERAND_VAR_NAME,
	BR_OPERAND_DB_NAME,
	BR_OPERAND_SYSCALL_NAME,
	BR_OPERAND_BUILTIN,
} BR_OperandType;
#define BR_GET_OPERAND_TYPE(type) ((BR_OperandType)(BR_opFlags[type] & 7))

typedef enum {
	BR_ADDR_NONE,
	BR_ADDR_I8,
	BR_ADDR_I16,
	BR_ADDR_I32,
	BR_ADDR_PTR,
	BR_ADDR_I64,
} BR_AddrOperandType;
#define BR_GET_ADDR_OP_TYPE(type) ((BR_AddrOperandType)((BR_opFlags[type] >> 3) & 7))
#define BR_OPERAND_ALLOCATED 64

typedef enum {
	BR_TYPE_DYNAMIC = 0,
	BR_TYPE_I8      = 1,
	BR_TYPE_I16     = 2,	
	BR_TYPE_I32     = 4,
	BR_TYPE_PTR     = 8,
	BR_TYPE_I64     = 16,
	BR_TYPE_VOID    = 32,
	BR_TYPE_STRUCT  = 64,
	BR_TYPE_INT     = BR_TYPE_I8 | BR_TYPE_I16 | BR_TYPE_I32 | BR_TYPE_I64 | BR_TYPE_PTR,
	BR_TYPE_ANY     = BR_TYPE_INT | BR_TYPE_STRUCT,
	BR_N_TYPE_KINDS
} BR_TypeKind;
extern const sbuf BR_typeNames[];

typedef union  BR_Type BR_Type;
typedef struct BR_Op BR_Op;
typedef struct BR_stacknode_t* BR_StackNode;
union BR_Type {
	struct {
		uint32_t n_items;
		uint32_t struct_id:25;
		BR_TypeKind kind:7;
	};
	BR_Type (*ctor)(BR_Op, BR_StackNode);
};
static_assert(sizeof(BR_Type) <= sizeof(uint64_t), "just for compactness");
declArray(BR_Type);
#define MAX_N_STRUCTS          (size_t)(1UL << 25)
#define BR_I8_TYPE(n)         ((BR_Type){.kind = BR_TYPE_I8,  .n_items = n})
#define BR_I16_TYPE(n)        ((BR_Type){.kind = BR_TYPE_I16, .n_items = n})
#define BR_I32_TYPE(n)        ((BR_Type){.kind = BR_TYPE_I32, .n_items = n})
#define BR_PTR_TYPE(n)        ((BR_Type){.kind = BR_TYPE_PTR, .n_items = n})
#define BR_I64_TYPE(n)        ((BR_Type){.kind = BR_TYPE_I64, .n_items = n})
#define BR_INT_TYPE(n)        ((BR_Type){.kind = BR_TYPE_INT, .n_items = n})
#define BR_ANY_TYPE(n)        ((BR_Type){.kind = BR_TYPE_ANY, .n_items = n})
#define BR_VOID_TYPE          ((BR_Type){.kind = BR_TYPE_VOID             })
#define BR_STRUCT_TYPE(id, n) ((BR_Type){.kind = BR_TYPE_STRUCT, .struct_id = id, .n_items = n})
#define BR_DYN_TYPE(_ctor)    ((BR_Type){.ctor = _ctor})

struct BR_Op {
	BR_OpType type:8;
	uint8_t x_op2_size;
	uint32_t x_op1_size; // only for the `add` operation and only during execution
	union {
		uint64_t operand_u;	
		int64_t operand_s;
		BR_Type operand_type;
		void* operand_ptr;
	};
};
static_assert(sizeof(BR_Op) <= 16, "`sizeof(BR_Op) > 16`, that's no good, gotta save up on that precious memory");
declArray(BR_Op);

typedef struct {
	const char* name;
	BR_OpArray body;
	BR_Type ret_type;
	BR_TypeArray args;
} BR_Proc;
declArray(BR_Proc);
#define BR_MAX_N_PROCS UINT32_MAX

// System Calls: crossplatform way to use system-dependent functionality
typedef enum {
	BR_SYS_EXIT,
	BR_SYS_WRITE,
	BR_SYS_READ,
	BR_N_SYSCALLS
} BR_Syscall;

extern const sbuf BR_syscallNames[];
extern const size_t BR_syscallNArgs[];

// Built-ins: crossplatform way to get system-dependent constants
// all built-ins are of pointer size
typedef enum {
	BR_BUILTIN_NULL,
	BR_BUILTIN_STDIN,
	BR_BUILTIN_STDOUT,
	BR_BUILTIN_STDERR,
	BR_N_BUILTINS
} BR_Builtin;

extern const uintptr_t BR_builtinValues[];
extern const sbuf BR_builtinNames[];

typedef struct {
	const char* name;
	union {
		BR_OpArray body;
		sbuf data;
	};
	bool is_mutable;
} BR_DataBlock;
declArray(BR_DataBlock);
#define BR_MAX_N_DBS UINT32_MAX

typedef struct {
	BR_TypeArray fields;
	const char* name;
	size_t size;
	uint8_t alignment;
} BR_Struct;
declArray(BR_Struct);

typedef struct {
	BR_StructArray seg_typeinfo;
	BR_ProcArray seg_exec;
	BR_DataBlockArray seg_data;
	size_t exec_entry_point;
} BR_Module;

#define BR_HEADER_SIZE 8
#define BR_V1_HEADER sbuf_fromcstr("BRBv1\0\0\0")

typedef enum {
	BR_ERR_OK,
	BR_ERR_INVALID_HEADER,
	BR_ERR_NO_HEADER,
	BR_ERR_NO_MEMORY,
	BR_ERR_NO_SEG_SIZES,
	BR_ERR_NO_OPCODE,
	BR_ERR_INVALID_OPCODE,
	BR_ERR_NO_OPERAND,
	BR_ERR_INVALID_NAME,
	BR_ERR_NAMES_NOT_RESOLVED,
	BR_ERR_STACK_UNDERFLOW,
	BR_ERR_OPERAND_OUT_OF_RANGE,
	BR_ERR_NO_PROC_RET_TYPE,
	BR_ERR_NO_PROC_NAME,
	BR_ERR_NO_PROC_ARG,
	BR_ERR_NO_PROC_BODY_SIZE,
	BR_ERR_NO_DB_NAME,
	BR_ERR_NO_DB_BODY_SIZE,
	BR_ERR_NO_ENTRY,
	BR_ERR_INVALID_ENTRY,
	BR_ERR_INVALID_ENTRY_PROTOTYPE,
	BR_ERR_TYPE_EXPECTED,
	BR_ERR_OP_NAME_EXPECTED,
	BR_ERR_INT_OPERAND_EXPECTED,
	BR_ERR_INT_OR_DB_NAME_EXPECTED,
	BR_ERR_UNKNOWN_DB,
	BR_ERR_BUILTIN_NAME_EXPECTED,
	BR_ERR_UNKNOWN_BUILTIN,
	BR_ERR_TEXT_OPERAND_EXPECTED,
	BR_ERR_INVALID_DECL,
	BR_ERR_ARGS_EXPECTED,
	BR_ERR_PROTOTYPE_MISMATCH,
	BR_ERR_SYSCALL_NAME_EXPECTED,
	BR_ERR_UNKNOWN_SYSCALL,
	BR_ERR_INVALID_ARRAY_SIZE_SPEC,
	BR_ERR_TYPE_MISMATCH,
	BR_ERR_DEL_ARGS,
	BR_ERR_MODULE_LOAD_INTERRUPT,
	BR_ERR_TOO_MANY_STRUCTS,
	BR_ERR_STRUCT_NAME_EXPECTED,
	BR_ERR_UNKNOWN_STRUCT,
	BR_ERR_RECURSIVE_TYPE,
	BR_ERR_NO_STRUCT_DECL,
	BR_ERR_NO_STRUCT_FIELD,
	BR_ERR_STRUCT_ID_EXPECTED,
	BR_ERR_INVALID_STRUCT_ID,
	BR_ERR_INVALID_TYPE_KIND,
	BR_N_ERROR_TYPES
} BR_ErrorType;

typedef struct {
	BR_ErrorType type;
	uint8_t opcode;
	BRP_TokenLoc* loc;
	union {
		const char* name;
		char header[BR_HEADER_SIZE];
		struct {
			uint32_t expected_stack_length;
			uint32_t actual_stack_length;
		};
		struct {
			BR_Type expected_type;
			BR_Type actual_type;
			uint32_t arg_id;
		};
		uint64_t operand;
		struct {
			BR_Struct* structs;
			uint32_t struct_id;
		};
	};
} BR_Error;

struct BR_stacknode_t {
	BR_StackNode prev;
	const char* name;
	BR_Type type;
	uint8_t flags;
};
declArray(BR_StackNode);
declArray(BR_StackNodeArray);

typedef struct {
	BR_Module module;
	BR_StackNodeArrayArray procs;
	BR_StackNodeArrayArray data_blocks;
	BR_Error error;
	Arena arena;
} BR_ModuleBuilder;

typedef enum {
	BR_EXC_CONTINUE,
	BR_EXC_EXIT,
	BR_EXC_END,
	BR_EXC_INTERRUPT,
	BR_EXC_UNKNOWN_OP,
	BR_EXC_STACK_OVERFLOW,
	N_BR_EXCS
} BR_ExecStatusType;

typedef struct {
	BR_ExecStatusType type;
	uint8_t exit_code; // for BR_EXC_EXIT
} BR_ExecStatus;

#define UINT48_MAX 0xFFFFFFFFFFFFULL
#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define eputc(ch) fputc(ch, stderr)
#define eputs(str) fputs(str, stderr)

#define BRB_EXT "brb"
#define VBR_EXT "vbrb"
#define BR_EXT "br"

static struct timespec TIME;

bool BR_startTimerAt(struct timespec* dst);
#define startTimer() startTimerAt(&TIME)

float BR_endTimerAt(struct timespec* src);
#define endTimer() endTimerAt(&TIME)

typedef struct {
	u8 exitcode;
	bool exited;
	FILE* in;
	FILE* out;
	FILE* err;
} BR_ProcessInfo;

// exEcutes the command in a subshell and returns the IO descriptors and the exitcode of the process
bool BR_execProcess(char* argv[], BR_ProcessInfo* info);
// same as execProcess(), but command is provided in the form of a sized buffer
bool BR_execProcess_s(sbuf command, BR_ProcessInfo* info);

// returns `true` if the path points to a directory, otherwise returns `false`
bool BR_isPathDir(char* path);
// same as isPathDir(), but path is provided in the form of a sized buffer
bool BR_isPathDir_s(sbuf path);

// returns a null-terminated string that is a copy of the file extension of the null-terminated `path`
char* BR_getFileExt(const char* path);
// same as `getFileExt`, but operates on and returns a sized buffer, without additional allocations
sbuf BR_getFileExt_s(sbuf path);

// string function that changes the extension of the file path `path` to `ext`
char* BR_setFileExt(const char* path, const char* ext);
// same as setFileExt(), but operates on and returns a sized buffer instead of a string
sbuf BR_setFileExt_s(sbuf path, sbuf ext);

// returns the base file name extracted from a file path, e.g. "dir/y.txt" -> "y"
char* BR_fileBaseName(const char* path);
// same as fileBaseName, but operates on and returns a sized buffer
sbuf BR_fileBaseName_s(sbuf path);

#define IS_TEMPPATH(path) sbufstartswith(path, fromcstr("/tmp")) 
#define inRange(x, start, end) ((int64_t)(x) >= (int64_t)(start) && (int64_t)(x) < (int64_t)(end))
#define isSlice(sub_start, sub_size, start, size) \
	((sub_start) >= (start) && (sub_start) + (sub_size) <= (start) + (size)) 
// assumes that sub_size > 0 and size > 0
#define maxInt(a, b) ( a > b ? a : b )
#define minInt(a, b) ( a < b ? a : b )
#define absInt(x) (SIGN_BIT_SET(x) ? -x : x)
#define absNegInt(x) (SIGN_BIT_SET(x) ? x : -x)
#define swap(v1, v2, tmp_t) { tmp_t TEMPVAR = v1; v1 = v2; v2 = TEMPVAR; }
#define byteMask(n_bytes, offset) ((1ULL << ((n_bytes) * 8)) - 1) << ((offset) * 8)


declArray(sbuf);
#define BR_DEFAULT_STACK_SIZE (512 * 1024) /* 512 KBs, just like in JVM */
typedef struct {
	sbufArray seg_data;
	BR_ProcArray seg_exec;
	const BR_Op* cur_proc;
	sbuf stack;
	char* stack_head;
	uint32_t exec_index;
	BR_ExecStatus exec_status;
	sbuf* exec_argv;
	uint32_t exec_argc;
	uint32_t entry_point;
} BR_ExecEnv;

typedef int64_t BR_id; // an ID of either a procedure or a data block
#define BR_INVALID_ID INT64_MIN

typedef enum {
	BR_CERR_OK,
	BR_CERR_OPERATOR_EXPECTED,
	BR_CERR_OPERAND_EXPECTED,
	BR_CERR_EXTRA_BRACKET,
	BR_CERR_PREP_FAILURE,
	BR_CERR_UNCLOSED_BRACKET,
	BR_CERR_MISMATCHED_BRACKET,
	BR_CERR_MULTIPLE_EXPRS_IN_BRACKETS
} BR_CompilationErrorType;

typedef struct {
	BR_CompilationErrorType type;
	BRP_Token loc;
	BRP_Token other_loc;
	BRP* prep;
} BR_CompilationError;

// implemented in `src/libbr_core.c`
void     BR_printErrorMsg(FILE* dst, BR_Error err, const char* prefix);
FILE*    BR_findModule(const char* module_name, const char* search_paths[]);

BR_Error BR_initModuleBuilder(BR_ModuleBuilder* builder);
BR_Error BR_analyzeModule(const BR_Module* module, BR_ModuleBuilder* dst);
void     BR_delModuleBuilder(BR_ModuleBuilder builder);
void     BR_delModule(BR_Module module);
BR_Error BR_extractModule(BR_ModuleBuilder builder, BR_Module* dst);
BR_Error BR_setEntryPoint(BR_ModuleBuilder* builder, size_t proc_id);
BR_Error BR_addOp(BR_ModuleBuilder* builder, BR_id proc_id, BR_Op op);

BR_Error BR_preallocStructs(BR_ModuleBuilder* builder, uint32_t n_structs_hint);
void     BR_deallocStructs(BR_Module* module);
BR_Error BR_addStruct(BR_ModuleBuilder* builder, BR_id* struct_id_p, const char* name, uint32_t n_fields, BR_Type* fields);
BR_id    BR_getStructIdByName(const BR_Module* module, const char* name);

BR_Error BR_preallocProcs(BR_ModuleBuilder* builder, uint32_t n_procs_hint);
void     BR_deallocProcs(BR_Module* module);
BR_Error BR_addProc(BR_ModuleBuilder* builder, BR_id* proc_id_p, const char* name, size_t n_args, const BR_Type* args, BR_Type ret_type, uint32_t n_ops_hint);
BR_id    BR_getProcIdByName(const BR_Module* module, const char* name); // returns BR_INVALID_ID on error

BR_Error BR_preallocDataBlocks(BR_ModuleBuilder* builder, uint32_t n_dbs_hint);
void     BR_deallocDataBlocks(BR_Module* module);
BR_Error BR_addDataBlock(BR_ModuleBuilder* builder, BR_id* db_id_p, const char* name, bool is_mutable, uint32_t n_pieces_hint);
BR_id    BR_getDataBlockIdByName(const BR_Module* module, const char* name); // returns BR_INVALID_ID on error

BR_Op*   BR_getOp(BR_Module* module, BR_id proc_id, uint32_t op_id);
BR_Error BR_labelStackItem(BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id, uint32_t item_id, const char* name);
bool     BR_getStackItemType(const BR_ModuleBuilder* builder, BR_Type* dst, BR_id proc_id, uint32_t op_id, uint32_t item_id);
size_t   BR_getStackItemRTOffset(const BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id, size_t item_id); // if `item_id` is not SIZE_MAX, returns SIZE_MAX on error
size_t   BR_getStackRTSize(const BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id);
size_t   BR_getMaxStackRTSize(const BR_ModuleBuilder* builder, BR_id proc_id);
size_t   BR_getStackItemRTSize(const BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id, uint32_t item_id); // returns SIZE_MAX on error
size_t   BR_getStackItemIdByName(const BR_ModuleBuilder* builder, BR_id proc_id, uint32_t op_id, const char* name); // returns SIZE_MAX on error
size_t   BR_getTypeRTSize(const BR_Module* module, BR_Type type);

// implemented in `src/libbr_write.c`
long     BR_writeModule(BR_Module src, FILE* dst);

// implemented in `src/libbr_load.c`
BR_Error BR_loadFromBytecode(FILE* src, BR_ModuleBuilder* dst);

// implemented in `src/libbr_exec.c`
BR_Error BR_execModule(BR_Module module, BR_ExecEnv* env, char* args[], size_t stack_size, const volatile bool* interruptor);
void     BR_delExecEnv(BR_ExecEnv* env);

// implemented in `src/libbr_asm.c`
BR_Error BR_loadFromAssembly(FILE* input, const char* input_name, BR_ModuleBuilder* dst);

// implemented in `src/libbr_dis.c`
long     BR_printType(BR_Type type, FILE* dst);
long     BR_disassembleModule(const BR_Module* module, FILE* dst);

// implemented in `src/libbr_native_codegen.c`
long     BR_compileModule_darwin_arm64(const BR_Module* module, FILE* dst, char** entry_point_name);

// implemented in `src/libbr_compiler.c`
BR_CompilationError BR_loadFromSource(FILE* input, const char* input_name, BR_ModuleBuilder* dst);
void                BR_printCompilationErrorMsg(FILE* dst, BR_CompilationError err, const char* prefix);

// TODO: add the `const`s wherever possible
#endif // _BRIDGE_
