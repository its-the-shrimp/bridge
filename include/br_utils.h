// compatibility definitions and declarations
#ifndef _BR_UTILS_H
#define _BR_UTILS_H

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

void* reverseByteOrder(void* src, long length);
#if IS_LITTLE_ENDIAN
#define BRByteOrder reverseByteOrder
#else
#define BRByteOrder(src, length) (src)
#endif

// DEF_WITH_ATTRS macro creates a function definition with attributes, which is normally prohibited by the C standard
#define DEF_WITH_ATTRS(prototype, attrs) prototype attrs; prototype
#define alignby(value, step) ((step) - (unsigned)((value) - 1) % (step) + (value) - 1)

#if IS_BIG_ENDIAN
#define CC16(src) (src[0] << 8 | src[1])
#define CC32(src) (CC16(src) << 16 | CC16((char*)((void*)src + 2)))
#else
#define CC16(src) ((src)[1] << 8 | (src)[0])
#define CC32(src) (CC16((char*)((void*)src + 2)) << 16 | CC16(src))
#define CC64(src) ((long)CC32((char*)((void*)src + 4)) << 32 | CC32(src))
#endif // IS_BIG_ENDIAN

#define _s(x) #x

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

#endif // _BR_UTILS_H

#if defined(BR_UTILS_IMPLEMENTATION) && !defined(_BR_UTILS_IMPL_LOCK)
#define _BR_UTILS_IMPL_LOCK

void* reverseByteOrder(void* src, long length) {
	char* _src = src;
	for (long i = 0; i < length / 2; i++) {
		char tmp = _src[i];
		_src[i] = _src[length - i - 1];
		_src[length - i - 1] = tmp;
	}
	return src;
}

#endif // BR_UTILS_IMPLEMENTATION
