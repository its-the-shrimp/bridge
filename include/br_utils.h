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