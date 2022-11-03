// compatibility definitions and declarations
#ifndef _BR_UTILS_H
#define _BR_UTILS_H

#include <stdint.h>
#include <string.h>

#if defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__) && defined(__ORDER_LITTLE_ENDIAN__)
#define IS_BIG_ENDIAN (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#define IS_LITTLE_ENDIAN (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#elif defined(IS_BIG_ENDIAN) && IS_BIG_ENDIAN == 1
#define IS_LITTLE_ENDIAN 0
#elif defined(IS_LITTLE_ENDIAN) && IS_LITTLE_ENDIAN == 1
#define IS_BIG_ENDIAN 0
#else
#error "could not automatically detect system endianness; if your system is big-endian, put `#define IS_BIG_ENDIAN 1` before importing `br.h`, if your system is little-endian, put `#define IS_LITTLE_ENDIAN 1` before importing `br.h`"
#endif

// determining CPU type
#if defined(__x86_64__) || defined(__aarch64__)
#define IS_32BIT 0
#define IS_64BIT 1
#else
#define IS_32BIT 1
#define IS_64BIT 0
#endif

typedef uint8_t   u8;
typedef int8_t    s8;
typedef uint16_t  u16;
typedef int16_t   s16;
typedef uint32_t  u32;
typedef int32_t   s32;
typedef uintptr_t uptr;
typedef intptr_t  sptr;
typedef uint64_t  u64;
typedef int64_t   s64;

void* reverseByteOrder(void* src, u64 length);
#if IS_LITTLE_ENDIAN
#define BRByteOrder(src, length) reverseByteOrder(src, length)
#else
#define BRByteOrder(src, length) (src)
#endif

// DEF_WITH_ATTRS macro creates a function definition with attributes, which is normally prohibited by the C standard
#define DEF_WITH_ATTRS(prototype, attrs) prototype attrs; prototype
#define alignby(value, step) ((step) - (uint64_t)((value) - 1) % (step) + (value) - 1)

#define _s(x) #x
#define _CONCAT(x, y) x##y
#define CONCAT(x, y) _CONCAT(x, y)
#define TEMPVAR CONCAT(_lc, __LINE__)

// variadic macros! (only up to 8 arguments, but we can expand it if needed)

#define _NARGS(x,x8,x7,x6,x5,x4,x3,x2,x1,n,...) n
#define NARGS(...) _NARGS(0,__VA_ARGS__,8,7,6,5,4,3,2,1,0)

#define LAZY_AND_1(x) (x)
#define LAZY_AND_2(x, y) ((x) ? (y) : false)
#define LAZY_AND_3(x, ...) LAZY_AND_2(x, LAZY_AND_2(__VA_ARGS__))
#define LAZY_AND_4(x, ...) LAZY_AND_2(x, LAZY_AND_3(__VA_ARGS__))
#define LAZY_AND_5(x, ...) LAZY_AND_2(x, LAZY_AND_4(__VA_ARGS__))
#define LAZY_AND_6(x, ...) LAZY_AND_2(x, LAZY_AND_5(__VA_ARGS__))
#define LAZY_AND_7(x, ...) LAZY_AND_2(x, LAZY_AND_6(__VA_ARGS__))
#define LAZY_AND_8(x, ...) LAZY_AND_2(x, LAZY_AND_7(__VA_ARGS__))
#define _LAZY_AND_N(n, ...) LAZY_AND_##n(__VA_ARGS__)
#define LAZY_AND_N(n, ...) _LAZY_AND_N(n, __VA_ARGS__)
#define LAZY_AND(...) LAZY_AND_N(NARGS(__VA_ARGS__), __VA_ARGS__)

#define LAZY_OR_1(x) (x)
#define LAZY_OR_2(x, y) ((bool)(x) ? true : (bool)(y))
#define LAZY_OR_3(x, ...) LAZY_OR_2(x, LAZY_OR_2(__VA_ARGS__))
#define LAZY_OR_4(x, ...) LAZY_OR_2(x, LAZY_OR_3(__VA_ARGS__))
#define LAZY_OR_5(x, ...) LAZY_OR_2(x, LAZY_OR_4(__VA_ARGS__))
#define LAZY_OR_6(x, ...) LAZY_OR_2(x, LAZY_OR_5(__VA_ARGS__))
#define LAZY_OR_7(x, ...) LAZY_OR_2(x, LAZY_OR_6(__VA_ARGS__))
#define LAZY_OR_8(x, ...) LAZY_OR_2(x, LAZY_OR_7(__VA_ARGS__))
#define _LAZY_OR_N(n, ...) LAZY_OR_##n(__VA_ARGS__)
#define LAZY_OR_N(n, ...) _LAZY_OR_N(n, __VA_ARGS__)
#define LAZY_OR(...) LAZY_OR_N(NARGS(__VA_ARGS__), __VA_ARGS__)

// bit manipulation functions
#define BIT(n) (1ULL << (n))
#define BIT_MASK(start, length) ((BIT(length) - 1) << (start))
int LOWEST_SET_BIT(uint64_t bitset);
#define LOWEST_CLEAR_BIT(bitset) LOWEST_SET_BIT(~(bitset))

#define CLEAR_BIT(bitset, n) ((bitset) & ~BIT(n))
#define SET_BIT(bitset, n) ((bitset) | BIT(n))

#define BIT_CLEAR(bitset, n) (((bitset) & BIT(n)) == 0)
#define BIT_SET(bitset, n) (!BIT_CLEAR(bitset, n))
#define SIGN_BIT_SET(x) ((int64_t)(x) < 0)

#define FITS_IN_8BITS(x)  (SIGN_BIT_SET(x) ? (int64_t)(x) >> 8  == -1 : (int64_t)(x) >> 8  == 0)
#define FITS_IN_16BITS(x) (SIGN_BIT_SET(x) ? (int64_t)(x) >> 16 == -1 : (int64_t)(x) >> 16 == 0)
#define FITS_IN_32BITS(x) (SIGN_BIT_SET(x) ? (int64_t)(x) >> 32 == -1 : (int64_t)(x) >> 32 == 0)

// custom replacement for <assert.h>
#define _assert(expr, file, line, f_name, ...) { \
	if (!(expr)) { \
		fprintf(stderr, "Asssertion failed at "_s(file)":"_s(line)", in function `%s`:\n\t", f_name); \
		fprintf(stderr, __VA_ARGS__); \
		fputc('\n', stderr); \
		abort(); \
	} \
}
#define assert(expr, ...) _assert(expr, __FILE__, __LINE__, __func__, __VA_ARGS__)
#define static_assert _Static_assert
#define unreachable() assert(false, "congratulations, unreachable code is reached")

void* memdup(const void* ptr, size_t size);
#define objdup(T, obj) ((T*)memdup(&obj, sizeof(T)))

#endif // _BR_UTILS_H

#if defined(BR_UTILS_IMPLEMENTATION) && !defined(_BR_UTILS_IMPL_LOCK)
#define _BR_UTILS_IMPL_LOCK

#include <math.h>
#include <stdlib.h>

void* memdup(const void* ptr, size_t size) {
	return memcpy(malloc(size), ptr, size);
}

void* reverseByteOrder(void *const src, uint64_t length)
{
	uint8_t *const _src = src;
	for (uint64_t i = 0; i < length / 2; i += 1) {
		uint8_t tmp = _src[i];
		_src[i] = _src[length - i - 1];
		_src[length - i - 1] = tmp;
	}
	return src;
}

int LOWEST_SET_BIT(uint64_t bitset)
{
	return lroundf(log2f(bitset & -bitset));
}

#endif // BR_UTILS_IMPLEMENTATION
