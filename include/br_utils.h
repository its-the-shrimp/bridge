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

void* reverseByteOrder(void* src, uint64_t length);
#if IS_LITTLE_ENDIAN
#define BRByteOrder(src, length) reverseByteOrder(src, length)
#else
#define BRByteOrder(src, length) (src)
#endif

// DEF_WITH_ATTRS macro creates a function definition with attributes, which is normally prohibited by the C standard
#define DEF_WITH_ATTRS(prototype, attrs) prototype attrs; prototype
#define alignby(value, step) ((step) - (unsigned)((value) - 1) % (step) + (value) - 1)

#define _s(x) #x
#define _CONCAT(x, y) x##y
#define CONCAT(x, y) _CONCAT(x, y)
#define TEMPVAR CONCAT(_lc, __LINE__)

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

#define FITS_IN_8BITS(x)  ((uint64_t)x <= UINT8_MAX)
#define FITS_IN_16BITS(x) ((uint64_t)x <= UINT16_MAX)
#define FITS_IN_32BITS(x) ((uint64_t)x <= UINT32_MAX)

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
