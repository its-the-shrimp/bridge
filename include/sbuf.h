// sbuf - Sized Buffer Implementation
// sized buffers provide utilities for parsing and manipulating text
#ifndef _SBUF_
#define _SBUF_

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdlib.h>

#define sbuf_format "%.*s"
#define unpack(array) (int)array.length, array.data

#define sbufshift(obj, offset) { obj.length -= offset; obj.data += offset; }
#define sbufpshift(obj, offset) { obj->length -= offset; obj->data += offset; }

static char _CHARSET[UINT8_MAX + 1] = {
	0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  14,  15,
	16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
	32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
	48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
	64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
	80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
	96,  97,  98,  99,  100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
	112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
	128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
	144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
	160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
	176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
	192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
	208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
	224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
	240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255
};

#define fromcstr(src) ((sbuf){ .data = src, .length = sizeof(src) - 1 })
#define fromstr(src) ((sbuf){ .data = src, .length = strlen(src) })
#define sbufslice(obj, start, end) ((sbuf){ .data = (obj).data + start, .length = (end < 0 || end > (obj).length ? (obj).length : end) - start })

#define _FORCECAST(x, T) _Generic((x), T: (x), default: (T){0})
#define _s(x) #x
#define IS_CSTR(literal) (_s(literal)[0] == '"')
#define _STATIC_IF(condition, on_true, on_false) _Generic(&(char[(condition) + 1]){0}, char(*)[2]: (on_true), char(*)[1]: (on_false))

#define SBUF(x) _Generic((x), \
    sbuf: (x), \
    char*: _STATIC_IF(IS_CSTR(_FORCECAST((x), char*)), fromcstr(_FORCECAST((x), char*)), fromstr(_FORCECAST((x), char*))), \
    int: ((sbuf){ .data = &_CHARSET[_FORCECAST((x), int)], .length = 1 }), \
    char: ((sbuf){ .data = &_CHARSET[_FORCECAST((x), int)], .length = 1 }) \
)

#define CSBUF(x) _Generic((x), \
    sbuf: (x), \
    char*: fromcstr(_FORCECAST((x), char*)), \
    int: ((sbuf){ .data = &_CHARSET[_FORCECAST((x), int)], .length = 1 }) \
)

#define BYTEFMT_OCT         0b00000001
#define BYTEFMT_HEX         0b00000010
#define BYTEFMT_HEX_LITERAL 0b00000100
#define BYTEFMT_RAW         0b00001000
//                          0b00010000
//                          0b00100000
#define BYTEFMT_ESC_DQUOTE  0b01000000
#define BYTEFMT_ESC_QUOTE   0b10000000

typedef long sbuf_size_t;

typedef struct sbuf {
	sbuf_size_t length;
	char* data;
} sbuf;

#define CSTRTERM ((sbuf){ .data = "\0", .length = 1 })

sbuf filecontent(FILE* fd);
sbuf _sbufunesc(sbuf src, sbuf* dst);
#define sbufunesc(src, dst) _sbufunesc(SBUF(src), dst)

sbuf_size_t sbufutf8len(sbuf obj);
bool sbufascii(sbuf obj);

bool _sbufstartswith(sbuf obj, sbuf sub);
#define sbufstartswith(obj, sub) _sbufstartswith(SBUF(obj), SBUF(sub))

bool _sbufendswith(sbuf obj, sbuf sub);
#define sbufendswith(obj, sub) _sbufendswith(SBUF(obj), SBUF(sub))

bool _sbufspace(sbuf obj);
#define sbufspace(obj) _sbufspace(SBUF(obj))

int _sbufsub(sbuf src, sbuf* dst, sbuf sub_src, sbuf sub_dst);
#define sbufsub(src, dst, sub_src, sub_dst) _sbufsub(SBUF(src), dst, SBUF(sub_src), SBUF(sub_dst))

sbuf _sbufconcat(int _, ...);
#define sbufconcat(...) _sbufconcat(0, __VA_ARGS__, (sbuf){0})
#define tostr(...) (sbufconcat(__VA_ARGS__, CSTRTERM).data)
#define strcopy(ptr) (sbufconcat(fromstr(ptr), CSTRTERM).data)

int sbufsplitescv(sbuf* src, sbuf* dst, sbuf delims[]);
sbuf _sbufsplitesc(sbuf* src, sbuf* dst, ...);
#define sbufsplitesc(src, dst, ...) _sbufsplitesc(src, dst, __VA_ARGS__, (sbuf){0})

int sbufsplitv(sbuf* src, sbuf* dst, sbuf delims[]);
sbuf _sbufsplit(sbuf* src, sbuf* dst, ...);
#define sbufsplit(src, dst, ...) _sbufsplit(src, dst, __VA_ARGS__, (sbuf){0})

int sbufsplitrv(sbuf* src, sbuf* dst, sbuf delims[]);
sbuf _sbufsplitr(sbuf* src, sbuf* dst, ...);
#define sbufsplitr(src, dst, ...) _sbufsplitr(src, dst, __VA_ARGS__, (sbuf){0})

bool _sbufeq(sbuf item1, sbuf item2);
#define sbufeq(item1, item2) _sbufeq(SBUF(item1), SBUF(item2))

bool _sbufint(sbuf src);
#define sbufint(src) _sbufint(SBUF(src))

long _sbuftoint(sbuf obj);
#define sbuftoint(obj) _sbuftoint(SBUF(obj))

sbuf _sbufstriplc(sbuf* src, const sbuf set);
#define sbufstriplc(src, set) _sbufstriplc(src, SBUF(set))

sbuf sbufstriplv(sbuf* src, const sbuf items[]);
sbuf sbufstriplva(sbuf* src, va_list args);
sbuf _sbufstripl(sbuf* src, ...);
#define sbufstripl(src, ...) _sbufstripl(src, __VA_ARGS__, (sbuf){0})

sbuf _sbufstriprc(sbuf* src, const sbuf set);
#define sbufstriprc(src, set) _sbufstriprc(src, SBUF(set))

sbuf sbufstriprv(sbuf* src, const sbuf items[]);
sbuf sbufstriprva(sbuf* src, va_list args);
sbuf _sbufstripr(sbuf* src, ...);
#define sbufstripr(src, ...) _sbufstripr(src, __VA_ARGS__, (sbuf){0})

sbuf_size_t _sbufstripc(sbuf* src, sbuf* ldst, sbuf* rdst, const sbuf set);
#define sbufstripc(src, ldst, rdst, set) _sbufstripc(src, ldst, rdst, SBUF(set))

sbuf_size_t sbufstripv(sbuf* src, sbuf* ldst, sbuf* rdst, const sbuf items[]);
sbuf_size_t _sbufstrip(sbuf* src, sbuf* ldst, sbuf* rdst, ...);
#define sbufstrip(src, ldst, rdst, ...) _sbufstrip(src, ldst, rdst, __VA_ARGS__, (sbuf){0})


char fputcesc(FILE* fd, unsigned char obj, unsigned char format);
#define putcharesc(obj, format) fputcesc(stdout, obj, format)

sbuf_size_t fputsbufesc(FILE* fd, sbuf obj, unsigned char format);
#define fputsbuflnesc(fd, obj, format) fputsbufesc(fd, obj, format); fputc('\n', fd)
#define fputsbuf(fd, obj) fputsbufesc(fd, obj, BYTEFMT_RAW)
#define fputsbufln(fd, obj) fputsbuf(fd, obj); fputc('\n', fd)
#define putsbufesc(obj, format) fputsbufesc(stdout, obj, format)
#define putsbuflnesc(obj, format) putsbufesc(obj, format); putchar('\n')
#define putsbuf(obj) fputsbuf(stdout, obj)
#define putsbufln(obj) fputsbufln(stdout, obj)

sbuf_size_t sbufindex(sbuf obj, sbuf sub);
sbuf sbufcopy(sbuf obj);

sbuf_size_t _sbufcount_c(sbuf obj, const sbuf set);
#define sbufcount_c(obj, set) _sbufcount_c(obj, SBUF(set))

sbuf_size_t sbufcount_v(sbuf obj, const sbuf items[]);
sbuf_size_t sbufcount_va(sbuf obj, va_list);
sbuf_size_t _sbufcount(sbuf obj, ...);
#define sbufcount(src, ...) _sbufcount(src, __VA_ARGS__, (sbuf){0})

char _sbufcutc(sbuf* src, const sbuf set);
#define sbufcutc(src, set) _sbufcutc(src, SBUF(set))

int sbufcutv(sbuf* src, const sbuf items[]);
sbuf sbufcutva(sbuf* src, va_list args);
sbuf _sbufcut(sbuf* src, ...);
#define sbufcut(src, ...) _sbufcut(src, __VA_ARGS__, (sbuf){0})

char _sbufcutrc(sbuf* src, const sbuf set);
#define sbufcutrc(src, set) _sbufcutrc(src, SBUF(set))

int sbufcutrv(sbuf* src, const sbuf items[]);
sbuf sbufcutrva(sbuf* src, va_list args);
sbuf _sbufcutr(sbuf* src, ...);
#define sbufcutr(src, ...) _sbufcutr(src, __VA_ARGS__, (sbuf){0})

sbuf smalloc(sbuf_size_t size);
sbuf scalloc(sbuf_size_t size);
bool srealloc(sbuf* obj, sbuf_size_t size);
void sfree(sbuf* obj);

#endif  // _SBUF_

#if defined(SBUF_IMPLEMENTATION) && !defined(_SBUF_IMPL_LOCK)
#define _SBUF_IMPL_LOCK

// concatenates the buffers provided as variadic arguments in a newly allocated buffer, returns the resulting buffer
sbuf _sbufconcat(int _, ...)
{
	va_list args;
	va_start(args, _);
	sbuf res = smalloc(0);
	sbuf new;
	while ((new = va_arg(args, sbuf)).data) {
		if (!srealloc(&res, res.length + new.length)) {
			sfree(&res);
			return (sbuf){0};
		}
		memcpy(res.data + res.length - new.length, new.data, new.length);
	}
	va_end(args);
	return res;
}


//returns a sized buffer created containing output fetched from the file descriptor `fd`.
// if an error occures, a zero-initialized sized buffer is returned.
sbuf filecontent(FILE* fd)
{
	if (fd == NULL || ferror(fd) || feof(fd)) return (sbuf){0};
	char temp[1024];
	sbuf res = smalloc(0);

	while (feof(fd) == 0 && ferror(fd) == 0) {
		int chunk_size = fread(temp, 1, sizeof(temp), fd);
		if (!srealloc(&res, res.length + chunk_size)) {
			sfree(&res);
			return (sbuf){0};
		}
		memcpy(res.data + res.length - chunk_size, temp, chunk_size);
	}

	return res;
}

// returns a bool, indicating whether `item` is in sbuf object `src`
bool sbufcontains(sbuf src, char item)
{
	for (int i = 0; i < src.length; i++) {
		if (src.data[i] == item) return true;
	}
	return false;
}

sbuf _sbufsplit(sbuf* src, sbuf* dst, ...)
{
	va_list args;
	dst->length = 0;
	dst->data = src->data;
	while (src->length > 0) {
		va_start(args, dst);
		sbuf delim = sbufcutva(src, args);
		va_end(args);
		if (delim.data) return delim;
		sbufpshift(src, 1);
		dst->length++;
	}
	return (sbuf){0};
}

sbuf _sbufsplitr(sbuf* src, sbuf* dst, ...)
{
	va_list args;
	*dst = *src;
	src->data += src->length;
	src->length = 0;
	while (dst->length > 0) {
		va_start(args, dst);
		sbuf delim = sbufcutrva(dst, args);
		va_end(args);
		if (delim.data) return delim;
		sbufpshift(src, -1);
		dst->length--;
	}
	*dst = *src;
	src->length = 0;
	return (sbuf){0};
}

int sbufsplitv(sbuf* src, sbuf* dst, sbuf delims[])
{
	dst->length = 0;
	dst->data = src->data;
	while (src->length > 0) {
		int delim_id = sbufcutv(src, delims);
		if (delim_id >= 0) return delim_id;
		sbufpshift(src, 1);
		dst->length++;
	}
	return -1;
}

int sbufsplitrv(sbuf* src, sbuf* dst, sbuf delims[])
{
	*dst = *src;
	src->data += src->length;
	src->length = 0;
	while (dst->length > 0) {
		int delim_id = sbufcutrv(dst, delims);
		if (delim_id >= 0) return delim_id;
		sbufpshift(src, -1);
		dst->length--;
	}
	return -1;
}

// works like sbufsplit, but ignores the delimeters that are preceded by the '\' escape character
sbuf _sbufsplitesc(sbuf* src, sbuf* dst, ...)
{
	va_list args;
	dst->length = 0;
	dst->data = src->data;
	char prevchar = '\0';
	while (src->length > 0)
	{
		va_start(args, dst);
		for (sbuf delim = va_arg(args, sbuf); delim.data != NULL; delim = va_arg(args, sbuf)) {
			if (sbufstartswith(*src, delim) && prevchar != '\\') {
				sbufpshift(src, delim.length);
				va_end(args);
				return (sbuf){ .data = src->data - delim.length, .length = delim.length };
			}
		}
		va_end(args);
		prevchar = *src->data;
		sbufpshift(src, 1);
		dst->length++;
	}
	return (sbuf){0};
}

int sbufsplitescv(sbuf* src, sbuf* dst, sbuf delims[])
{
	dst->length = 0;
	dst->data = src->data;
	char prevchar = '\0';
	while (src->length > 0) {
		for (int i = 0; delims[i].data; i++) {
			if (sbufstartswith(*src, delims[i]) && prevchar != '\\') {
				sbufpshift(src, delims[i].length);
				return i;
			}
		}
		prevchar = *src->data;
		sbufpshift(src, 1);
		dst->length++;
	}
	return -1;
}

#define lowerchar(ch) ( ch >= 'A' ? ch | 32 : ch )
// unescapes the characters in `src` and writes them to `dst`; if `dst` is NULL, a new buffer is allocated and returned
// `dst` buffer must be at least the size of `src`; the function guarantees that the output will not be larger than the input
sbuf _sbufunesc(sbuf src, sbuf* dst)
{
	sbuf stub_dst;
	if (!dst) {
		stub_dst = smalloc(src.length);
		dst = &stub_dst;
	}
	dst->length = 0;

	for (int i = 0; src.length > 0; i++) {
		if (*src.data == '\\') {
			sbufshift(src, 1);
			switch (*src.data) {
				case '0':  dst->data[i] = '\0'; sbufshift(src, 1); break;
				case 'n':  dst->data[i] = '\n'; sbufshift(src, 1); break;
				case 'r':  dst->data[i] = '\r'; sbufshift(src, 1); break;
				case 't':  dst->data[i] = '\t'; sbufshift(src, 1); break;
				case '\\': dst->data[i] = '\\'; sbufshift(src, 1); break;
				case 'x':
					sbufshift(src, 1);
					if (*src.data == '\0') { dst->data[i] = 'x'; break; }
					dst->data[i] = (*src.data >= 65 ? lowerchar(*src.data) - 87 : *src.data - 48) << 4;

					sbufshift(src, 1);
					if (*src.data == '\0') { dst->data[i] = (unsigned char)dst->data[i] >> 4; break; }
					dst->data[i] |= *src.data >= 65 ? lowerchar(*src.data) - 87 : *src.data - 48;

					sbufshift(src, 1);
					break;
				default: dst->data[i] = *src.data; break;
			}
		} else { dst->data[i] = *src.data; sbufshift(src, 1); }
		dst->length++;
	}
	return *dst;
}

// returns amounts of UTF-8 characters in the sized string.
sbuf_size_t sbufutf8len(sbuf obj)
{
	sbuf_size_t res = 0;
	for (long i = 0; i < obj.length; i++) {
		if (!(obj.data[i] & 128 && !(obj.data[i] & 64))) res++;
	}
	return res;
}

// returns `true` if the sized string `obj` only consists of ASCII characters, otherwise returns `false`
bool sbufascii(sbuf obj)
{
	for (size_t i = 0; i < obj.length; i++) {
		if (obj.data[i] < 0) return false;
	}
	return true;
}

// returns `true` if the sized string `obj` starts with the sequence, equal to the sequence in the sized string `sub`,
// otherwise returns `false`
bool _sbufstartswith(sbuf obj, sbuf sub)
{
	if (sub.length > obj.length) return false;
	for (size_t i = 0; i < sub.length; i++) {
		if (obj.data[i] != sub.data[i]) return false;
	}
	return true;
}

// returns `true` if the sized string `obj` ends with the sequence, equal to the sequence in the sized string `sub`,
// otherwise returns `false`
bool _sbufendswith(sbuf obj, sbuf sub)
{
	if (sub.length > obj.length) return false;
	size_t offset = obj.length - sub.length;
	for (sbuf_size_t i = offset; i < obj.length; i++) {
		if (obj.data[i] != sub.data[i - offset]) return false;
	}
	return true;
}

// returns `true` if the sized string `obj` only consists of tabs, newlines, spaces or other control characters,
// otherwise returns `false` 
bool _sbufspace(sbuf obj)
{
	for (size_t i = 0; i < obj.length; i++) {
		if (obj.data[i] > 32) return false;
	}
	return true;
}

// inserts buffer `sub_dst` in place of every occurence of `sub_src` in buffer `src`. Allocates a new buffer with the substituted contents and writes it to `dst`
// returns the number of substitutions made
int _sbufsub(sbuf src, sbuf* dst, sbuf sub_src, sbuf sub_dst)
{
	sbuf sub_src_array[2] = { sub_src };
	int src_count = sbufcount_v(src, sub_src_array);
	if (src_count == 0) {
		*dst = smalloc(src.length);
		memcpy(dst->data, src.data, src.length);
		return 0;
	}

	*dst = smalloc(src.length + src_count * (sub_dst.length - sub_src.length));
	sbuf_size_t offset = 0;
	while (true) {
		sbuf piece;
		if (sbufsplitv(&src, &piece, sub_src_array) < 0) {
			memcpy(dst->data + offset, piece.data, piece.length);
			return src_count;
		}
		memcpy(dst->data + offset, piece.data, piece.length);
		offset += piece.length;
		memcpy(dst->data + offset, sub_dst.data, sub_dst.length);
		offset += sub_dst.length;
	}

}

// returns `true` if the sized strings `item1` and `item2` are equal byte-by-byte, otherwise returns `false`
bool _sbufeq(sbuf item1, sbuf item2)
{
	if ( item1.length != item2.length ) return false;
	for (int i = 0; i < item1.length; i++) {
		if (item1.data[i] != item2.data[i]) return false;
	}
	return true;
}

// returns true if the sized string `src` can be interpreted as an integer literal, otherwise returns `false`.
// the function is intended to be used in conjunction with the `sbuftoint` function.
bool _sbufint(sbuf src)
{
	if (src.length == 0) { return false; }
	sbuf charset;
	if ( *src.data == 'u' || *src.data == '-' ) { sbufshift(src, 1); }
	if (sbufcut(&src, fromcstr("0x")).data) {
		charset = fromcstr("0123456789abcdefABCDEF");
	} else if (sbufcut(&src, fromcstr("0o")).data) {
		charset = fromcstr("01234567");
	} else if (sbufcut(&src, fromcstr("0b")).data) {
		charset = fromcstr("01");
	} else {
		charset = fromcstr("0123456789");
	}

	for (size_t i = 0; i < src.length; i++)
	{
		if ( !sbufcontains(charset, src.data[i]) ) { return false; }
	}

	return true;
}

// interprets the sized string `obj` as an integer literal and returns the corresponding 64 bit integer.
// supports binary, octal, decimal and hexadecimal integer literals.
// if `obj` is an invalid integer literal, 0 is returned; to check if a sized string is a valid integer literal,
// use the `sbufint` function
long _sbuftoint(sbuf obj)
{
	if (!obj.length) return 0;
	bool is_negative = obj.data[0] == '-';
	if (is_negative) { sbufshift(obj, 1); }
	long res = 0;
	char base = 10;
	if (obj.length > 2 && *obj.data == '0') {
		switch (obj.data[1]) {
			case 'b': base = 2;  sbufshift(obj, 2); break;
			case 'o': base = 8;  sbufshift(obj, 2); break;
			case 'x': base = 16; sbufshift(obj, 2); break;
		}
	}

	char cur_char;
	sbuf_size_t coef = 1;
	for (sbuf_size_t i = obj.length - 1; i >= 0 ; i--) {
		cur_char = obj.data[i] >= 'A' ? obj.data[i] | 0b00100000 : obj.data[i];
		res += ((cur_char > '9' ? cur_char - ('a' - '9' - 1) : cur_char) - '0') * coef;
		coef *= base;
	}

	if (is_negative) res *= -1;
	return res;
}

sbuf _sbufstriplc(sbuf* src, sbuf set)
{
	sbuf res = { .data = src->data, .length = 0 };
	while (true) {
		char item = sbufcutc(src, set);
		if (!item) break;
		res.length++;
	}
	return res;
}
// strips buffers in `items` from `src`, returns the stripped buffer
sbuf sbufstriplv(sbuf* src, const sbuf items[])
{
	sbuf res = { .data = src->data, .length = 0 };
	while (true) {
		int item = sbufcutv(src, items);
		if (item < 0) break;
		res.length += items[item].length;
	}
	return res;
}

sbuf sbufstriplva(sbuf* src, va_list args)
{
	sbuf res = { .data = src->data, .length = 0 };
	va_list args_l;
	while (true) {
		va_copy(args_l, args);
		sbuf item = sbufcutva(src, args_l);
		va_end(args);
		if (!item.length) break;
		res.length += item.length;
	}
	return res;
}

sbuf _sbufstripl(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	sbuf res = sbufstriplva(src, args);
	va_end(args);
	return res;
}

sbuf _sbufstriprc(sbuf* src, sbuf set)
{
	sbuf res = { .data = src->data, .length = 0 };
	while (true) {
		char item = sbufcutrc(src, set);
		if (!item) break;
		res.length++;
	}
	return res;
}

sbuf sbufstriprv(sbuf* src, const sbuf items[])
{
	sbuf res = { .data = src->data + src->length, .length = 0 };
	while (true) {
		int item = sbufcutrv(src, items);
		if (item < 0) break;
		sbufshift(res, -items[item].length);
	}
	return res;
}

sbuf sbufstriprva(sbuf* src, va_list args)
{
	sbuf res = { .data = src->data + src->length, .length = 0 };
	va_list args_l;
	while (true) {
		va_copy(args_l, args);
		sbuf item = sbufcutrva(src, args_l);
		va_end(args);
		if (!item.length) break;
		sbufshift(res, -item.length);
	}
	return res;
}

sbuf _sbufstripr(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	sbuf res = sbufstriprva(src, args);
	va_end(args);
	return res;
}

sbuf_size_t _sbufstripc(sbuf* src, sbuf* ldst, sbuf* rdst, sbuf set)
{
	sbuf ldst_l = sbufstriplc(src, set);
	if (ldst) *ldst = ldst_l;
	sbuf rdst_l = sbufstriprc(src, set);
	if (ldst) *rdst = rdst_l;

	return rdst_l.length + ldst_l.length;
}

sbuf_size_t sbufstripv(sbuf* src, sbuf* ldst, sbuf* rdst, const sbuf items[])
{
	sbuf ldst_l = sbufstriplv(src, items);
	if (ldst) *ldst = ldst_l;
	sbuf rdst_l = sbufstriprv(src, items);
	if (ldst) *rdst = rdst_l;

	return rdst_l.length + ldst_l.length;
}

sbuf_size_t sbufstripva(sbuf* src, sbuf* ldst, sbuf* rdst, va_list args)
{
	sbuf ldst_l = sbufstriplva(src, args);
	if (ldst) *ldst = ldst_l;
	sbuf rdst_l = sbufstriprva(src, args);
	if (ldst) *rdst = rdst_l;

	return rdst_l.length + ldst_l.length;
}

// writes the character `obj` with escaping specified by a set of flags `format` to the file descriptor `fd`
// supported formatting flags:
//	BYTEFMT_HEX - write unprintable characters in the form of "\x<hexadecimal value of the character>"
//	BYTEFMT_OCT - write unprintable characters in the form of "\<octal value of the character>"
//      BYTEFMT_HEX_LITERAL - write all characters in the form of "\x<hexadecimal value of the character>"
//      BYTEFMT_RAW - write all characters as is, equivalent to fputc(obj, fd)
//	BYTEFMT_ESC_QUOTE - escape single quote character ("\'" instead of "'")
//	BYTEFMT_ESC_DQUOTE = escape double quote character ( "\"" instead of """)
char fputcesc(FILE* fd, unsigned char obj, unsigned char format)
{
	char n = 0;
	if (format == BYTEFMT_HEX_LITERAL) { 
		n += fwrite("\\x", 2, 1, fd);
		char temp = (obj & 0xf0) >> 4; 
		n += fputc(temp + (temp > 9 ? 'A' - 10 : '0'), fd);
		temp = obj & 0xf;
		n += fputc(temp + (temp > 9 ? 'A' - 10 : '0'), fd);
	}
	else if (obj == '\0')				    { n += fwrite("\\0",  1, 2, fd); }
	else if (obj == '\t')				    { n += fwrite("\\t",  1, 2, fd); }
	else if (obj == '\n')				    { n += fwrite("\\n",  1, 2, fd); }
	else if (obj == '\r')				    { n += fwrite("\\r",  1, 2, fd); }
	else if (obj == '\\')				    { n += fwrite("\\\\", 1, 2, fd); }
	else if (obj == '\'' && format & BYTEFMT_ESC_QUOTE) { n += fwrite("\\'",  1, 2, fd); }
	else if (obj == '"' && format & BYTEFMT_ESC_DQUOTE) { n += fwrite("\\\"", 1, 2, fd); }
	else if (obj > 0x1F && obj < 0x7F)                  { n += fputc(obj,		fd); }
	else if (format & BYTEFMT_OCT) {
		n += fputc('\\', fd);
		n += fputc(obj & 0b00000111 + '0', fd);
		n += fputc((obj & 0b00111000 >> 3) + '0', fd);
		n += fputc((obj & 0b11000000 >> 6) + '0', fd);
	} else if (format & BYTEFMT_HEX) {
		n += fwrite("\\x", 2, 1, fd);
		char temp = obj >> 4;
		n += fputc(temp + (temp > 9 ? 'A' - 10 : '0'), fd);
		temp = obj & 0xf;
		n += fputc(temp + (temp > 9 ? 'A' - 10 : '0'), fd);
	}
	return n;
}

// performs the `fputcesc` function on all the characters in the sized string `obj`.
sbuf_size_t fputsbufesc(FILE* fd, sbuf obj, unsigned char format)
{
	if (format & BYTEFMT_RAW) return fwrite(obj.data, 1, obj.length, fd);
	sbuf_size_t n = 0;
	for (size_t i = 0; i < obj.length; i++) { 
		n += fputcesc(fd, obj.data[i], format);
	}
	return n;
}

// returns the index in the sized string `obj` where one of the characters in the sized string `sub` first occured.
// if none are found, -1 is returned.
sbuf_size_t sbufindex(sbuf obj, sbuf sub)
{
	sbuf_size_t res = 0;
	while (obj.length >= sub.length) {
		if (sbufstartswith(obj, sub)) return res;
		sbufshift(obj, 1);
		res++;
	}
	return -1;
}

sbuf_size_t _sbufcount_c(sbuf obj, sbuf set)
{
	sbuf_size_t res = 0;
	while (obj.length > 0) {
		if (memchr(set.data, *obj.data, set.length) != NULL) {
			res++;
		} else {
			sbufshift(obj, 1);
		}
	}
	return res;
}

sbuf_size_t sbufcount_v(sbuf obj, const sbuf items[])
{
	sbuf_size_t res = 0;
	while (obj.length > 0) {
		if (sbufcutv(&obj, items) >= 0) {
			res++;
		} else {
			sbufshift(obj, 1);
		}
	}

	return res;
}

sbuf_size_t sbufcount_va(sbuf obj, va_list args)
{
	sbuf_size_t res = 0;
	while (obj.length > 0) {
		if (sbufcutva(&obj, args).data) {
			res++;
		} else {
			sbufshift(obj, 1);
		}
	}

	return res;
}

sbuf_size_t _sbufcount(sbuf obj, ...)
{
	va_list args;
	va_start(args, obj);
	sbuf_size_t res = sbufcount_va(obj, args);
	va_end(args);
	return res;
}

// copies data of the sized buffer `obj` to the heap and returns sized string with the copied data 
sbuf sbufcopy(sbuf obj)
{
	sbuf res = smalloc(obj.length);
	memcpy(res.data, obj.data, obj.length);
	return res;
}

char _sbufcutc(sbuf* src, sbuf set)
{
	if (src->length == 0) return '\0';
	for (int i = 0; i < set.length; i++) {
		if (*src->data == set.data[i]) {
			sbufpshift(src, 1);
			return set.data[i];
		}
	}

	return '\0';
}

int sbufcutv(sbuf* src, const sbuf items[])
{
	for (int i = 0; items[i].data; i++) {
		if (sbufstartswith(*src, items[i])) {
			sbufpshift(src, items[i].length);
			return i;
		}
	}
	return -1;
}

sbuf sbufcutva(sbuf* src, va_list args)
{
	sbuf new;
	while ((new = va_arg(args, sbuf)).data) {
		if (sbufstartswith(*src, new)) {
			sbufpshift(src, new.length);
			return new;
		}
	}
	return (sbuf){0};
}

sbuf _sbufcut(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	sbuf res = sbufcutva(src, args);
	va_end(args);
	return res;
}

char _sbufcutrc(sbuf* src, sbuf set)
{
	if (src->length == 0) return '\0';
	char src_chr = src->data[src->length - 1];
	for (int i = 0; i < set.length; i++) {
		if (src_chr == set.data[i]) return src->data[--src->length];
	}

	return '\0';
}

int sbufcutrv(sbuf* src, const sbuf items[])
{
	for (int i = 0; items[i].data; i++) {
		if (sbufendswith(*src, items[i])) {
			src->length -= items[i].length;
			return i;
		}
	}
	return -1;
}
sbuf sbufcutrva(sbuf* src, va_list args)
{
	sbuf new;
	while ((new = va_arg(args, sbuf)).data) {
		if (sbufendswith(*src, new)) {
			src->length -= new.length;
			return new;
		}
	}
	return (sbuf){0};
}

sbuf _sbufcutr(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	sbuf res = sbufcutrva(src, args);
	va_end(args);
	return res;
}

sbuf smalloc(long length)
{
	char* temp = malloc(length);
	return (sbuf){ .data = temp, .length = temp ? length : 0 };
}

sbuf scalloc(long length)
{
	char* temp = calloc(1, length);
	return (sbuf){ .data = temp, .length = temp ? length : 0 };
}

bool srealloc(sbuf* obj, sbuf_size_t new_length)
{
	sbuf_size_t prevlen = obj->length;
	void* new;
	if ((new = realloc(obj->data, new_length))) {
		obj->length = new_length;
		obj->data = new;
		return true;
	}
	return false;
}

void sfree(sbuf* obj)
{
	free(obj->data);
}

#endif // SBUF_IMPLEMENTATION
