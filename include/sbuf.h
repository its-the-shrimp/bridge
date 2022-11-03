// sbuf - Sized Buffer Implementation
// sized buffers provide utilities for parsing and manipulating text
#ifndef _SBUF_
#define _SBUF_

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdlib.h>

#define sbuf_fmt "%.*s"
#define sbuf_unpack(array) (int)(array).length, (array).data

#define sbuf_shift(obj, offset) do { (obj).length -= (offset); (obj).data += (offset); } while (0);

#define sbuf_fromcstr(src) ((sbuf){ .data = src, .length = sizeof(src) - 1 })
#define sbuf_fromstr(src) ((sbuf){ .data = src, .length = strlen(src) })
#define sbuf_slice(obj, start, end) ((sbuf){ .data = (obj).data + start, .length = (end < 0 || end > (obj).length ? (obj).length : end) - start })

#define SBUF_TEMPVAR _sbuf_temp##__LINE__
#define sbuf_foreach_char(name, obj) \
const sbuf SBUF_TEMPVAR=(obj);for(char* name=SBUF_TEMPVAR.data;(size_t)(name-SBUF_TEMPVAR.data)<SBUF_TEMPVAR.length;name+=1)

typedef struct sbuf {
	size_t length;
	char* data;
} sbuf;

#define SBUF_CSTRTERM ((sbuf){ .data = "\0", .length = 1 })
#define SBUF_DQUOTE sbuf_fromcstr("\"")
#define SBUF_QUOTE sbuf_fromcstr("'")
#define SBUF_NT_PATHSEP sbuf_fromcstr("\\")
#define SBUF_POSIX_PATHSEP sbuf_fromcstr("/")
#define SBUF_NT_NEWLINE sbuf_fromcstr("\r\n")
#define SBUF_POSIX_NEWLINE sbuf_fromcstr("\n")
#define SBUF_SPACE sbuf_fromcstr(" ")
#define SBUF_TAB sbuf_fromcstr("\t")

#ifdef _WIN32_
#define SBUF_PATHSEP SBUF_NT_PATHSEP
#define SBUF_NEWLINE SBUF_NT_NEWLINE
#else
#define SBUF_PATHSEP SBUF_POSIX_PATHSEP
#define SBUF_NEWLINE SBUF_POSIX_NEWLINE
#endif

// Returns a buffer, containing output fetched from the file descriptor `fd`.
// If an error occures, an empty buffer is returned.
sbuf    sbuf_fromfile(FILE* fd);
// Unescapes the characters in `src` writes them to `dst`. If `dst` is NULL, a new buffer is allocated. The function returns `dst` of the new buffer.
// `dst` must point to a buffer at least of the size of `src` or be NULL. The function guarantees that the output will not be larger than the input.
sbuf    sbuf_unesc(sbuf src, sbuf* dst);
// Returns the length of `obj` in UTF-8 characters.
size_t  sbuf_utf8len(sbuf obj);
// Tests whether `obj` begins with `sub`.
bool    sbuf_startswith(sbuf obj, sbuf sub);
// Tests whether `obj` ends with `sub`.
bool    sbuf_endswith(sbuf obj, sbuf sub);
// Tests whether `obj` only consists of tabs, newlines, spaces or other control characters.
bool    sbuf_space(sbuf obj);
// Allocates a new buffer to which `src` is copied, with every occurence of `sub_src` replaced by `sub_dst`, the new buffer is then returned into `dst`.
// Returns the number of substitutions made.
int     sbuf_sub(sbuf src, sbuf* dst, sbuf sub_src, sbuf sub_dst);
sbuf    sbuf__concat(int _, ...);
// concatenates the buffers provided as variadic arguments in a newly allocated buffer, returns the resulting buffer
#define sbuf_concat(...) sbuf__concat(0, __VA_ARGS__, (sbuf){0})
#define sbuf_tostr(...) (sbuf_concat(__VA_ARGS__, SBUF_CSTRTERM).data)
// Splits buffer at `src` by one of `delims`, whichever is first found in `src`.
// The part before the delimiter (excluding it) is returned into `dst`, and the part after the delimiter - back into `src`.
// returns the index of the delimiter in `delims`.
// If none of `delims` were found in `src`, the whole `src` is transferred into `dst`, zeroed out and -1 is returned.
int     sbuf_split_v(sbuf* src, sbuf* dst, sbuf delims[]);
sbuf    sbuf__split(sbuf* src, sbuf* dst, ...);
// same as `sbuf_split_v`, but `delims` are provided as variadic arguments
#define sbuf_split(src, dst, ...) sbuf__split(src, dst, __VA_ARGS__, (sbuf){0})
// same as `sbuf_split_v`, but if the delimiter found in `src` is prefixed by a back-slash, it is ignored.
int     sbuf_splitesc_v(sbuf* src, sbuf* dst, sbuf delims[]); // TODO: the function doesn't recognise double back-slash as an escape sequence
sbuf    sbuf__splitesc(sbuf* src, sbuf* dst, ...);
// same as `sbuf_splitesc_v`, but `delims` are provided as variadic arguments
#define sbuf_splitesc(src, dst, ...) sbuf__splitesc(src, dst, __VA_ARGS__, (sbuf){0})
// same as `sbuf_rsplit_v`, but `delims` are searched in reverse order, from the end of `src` to the beginning
int     sbuf_rsplit_v(sbuf* src, sbuf* dst, sbuf delims[]);
sbuf    sbuf__rsplit(sbuf* src, sbuf* dst, ...);
// same as `sbuf_rsplit_v`, but `delims` are provided as variadic arguments
#define sbuf_rsplit(src, dst, ...) sbuf__rsplit(src, dst, __VA_ARGS__, (sbuf){0})
// tests whether `src` can be interpreted as an integer (binary, octal, decimal or hexadecimal) literal.
bool    sbuf_int(sbuf src);
// Interprets `obj` as an integer literal and returns the corresponding 64 bit integer.
// Supports binary, octal, decimal and hexadecimal integer literals.
// If `obj` is an invalid integer literal, 0 is returned; to check if a sized string is a valid integer literal, use the `sbuf_int` function.
long    sbuf_toint(sbuf obj);
// Tests whether the contents of `item1` and `item2` are equal.
bool    sbuf_eq(sbuf item1, sbuf item2);
// Tests whether 2 strings `str1` and `str2` are equal.
#define str_eq(str1, str2) (!(bool)strcmp(str1, str2))
// strips all the occurences of any character in `set` from the beginning of `src`; returns the stripped buffer.
sbuf    sbuf_lstrip_c(sbuf* src, const sbuf set);
// strips all the occurences of any of the `items` from the beginning of `src`; returns the stripped buffer.
sbuf    sbuf_lstrip_v(sbuf* src, const sbuf items[]);
// Like `sbuf_lstrip_v`, but `items` are provided in the variadic list `args`.
sbuf    sbuf_lstrip_va(sbuf* src, va_list args);
sbuf    sbuf__lstrip(sbuf* src, ...);
// Like `sbuf_strip_v`, but `items` are provided as variadic arguments.
#define sbuf_lstrip(src, ...) sbuf__lstrip(src, __VA_ARGS__, (sbuf){0})
// Like `sbuf_lstrip_c`, but characters are stripped from the end of `src`.
sbuf    sbuf_rstrip_c(sbuf* src, const sbuf set);
// Like `sbuf_lstrip_v`, but `items` are stripped from the end of `src`.
sbuf    sbuf_rstrip_v(sbuf* src, const sbuf items[]);
// Like `sbuf_lstrip_va`, but the arguments are stripped from the end of `src`.
sbuf    sbuf_rstrip_va(sbuf* src, va_list args);
sbuf    sbuf__rstrip(sbuf* src, ...);
// Like `sbuf_lstrip`, but the arguments are stripped from the end of `src`.
#define sbuf_rstrip(src, ...) sbuf__rstrip(src, __VA_ARGS__, (sbuf){0})
// Print the contents of `obj` with unprintable characters escaped.
// Flags, that can be arbitrarily combined and provided as `format`:
#define SBUF_BFMT_OCT         0x1
//	Print unknown unprintable characters as octal literals.
#define SBUF_BFMT_HEX         0x2
//	Print unknown unprintable characters as hexadecimal literals.
#define SBUF_BFMT_HEX_LITERAL 0x4
//	Print all the characters as hexadecimal literals, regardless of whether they're printable or not
#define SBUF_BFMT_RAW         0x8
//	Do not apply any escaping when printing.
#define SBUF_BFMT_DQUOTED     0x10
//	Put double quotes between the printed string.
#define SBUF_BFMT_QUOTED      0x20
//	Put single quotes between the printed string.
#define SBUF_BFMT_ESC_DQUOTE  0x40
//	Print double quotes escaped.
#define SBUF_BFMT_ESC_QUOTE   0x80
//	Print single quotes escaped.
size_t  sbuf_fputesc(FILE* fd, sbuf obj, unsigned char format);
#define sbuf_fputlnesc(fd, obj, format) (sbuf_fputesc(fd, obj, format) + (fputc('\n', fd) != EOF))
#define sbuf_fput(fd, obj)               sbuf_fputesc(fd, obj, SBUF_BFMT_RAW)
#define sbuf_fputln(fd, obj)            (sbuf_fput(fd, obj) + (fputc('\n', fd) != EOF))
#define sbuf_putesc(obj, format)         sbuf_fputesc(stdout, obj, format)
#define sbuf_putlnesc(obj, format)      (sbuf_putesc(obj, format) + (putchar('\n') != EOF))
#define sbuf_put(obj)                    sbuf_fput(stdout, obj)
#define sbuf_putln(obj)                  sbuf_fputln(stdout, obj)
// Same as `sbuf_fputesc`, but `obj` is a null-terminated string instead of a buffer.
size_t  str_fputesc(FILE* fd, const char* obj, unsigned char format);
#define str_fputlnesc(fd, obj, format) (str_fputesc(fd, obj, format) + (fputc('\n', fd) != EOF))
#define str_fput(fd, obj)               str_fputesc(fd, obj, SBUF_BFMT_RAW)
#define str_fputln(fd, obj)            (str_fput(fd, obj) + (fputc('\n', fd) != EOF))
#define str_putesc(obj, format)         str_fputesc(stdout, obj, format)
#define str_putlnesc(obj, format)      (str_putesc(obj, format) + (putchar('\n') != EOF))
#define str_put(obj)                    str_fput(stdout, obj)
#define str_putln(obj)                  str_fputln(stdout, obj)
// Copies the contents of `obj` into a newly allocated buffer, returns the buffer.
sbuf    sbuf_copy(sbuf obj);
#define str_copy(obj) strdup(obj)
// Returns the number of occurences of any character of `set` in `obj`.
size_t  sbuf_count_c(sbuf obj, const sbuf set);
// Returns the number of occurences of any of `items` in `obj`.
size_t  sbuf_count_v(sbuf obj, const sbuf items[]);
// Like `sbuf_count_v`, but `items` are provided in a variadic list `args`.
size_t  sbuf_count_va(sbuf obj, va_list args);
size_t  sbuf__count(sbuf obj, ...);
// Like `sbuf_count_v`, but `items` are provided as variadic arguments.
#define sbuf_count(src, ...) sbuf__count(src, __VA_ARGS__, (sbuf){0})
// If any one of the characters in `set` is found at the beginning of `src`, it's removed from `src` and is returned.
// If none are found, null byte is returned.
char    sbuf_cut_c(sbuf* src, const sbuf set);
// If any one of `items` is found at the beginning of `src`, it's removed from `src` and it's index in `items` is retuerned.
// If none are found, -1 is returned.
int     sbuf_cut_v(sbuf* src, const sbuf items[]);
// Like `sbuf_cut_v`, but `items` are provided in a variadic list `args`.
sbuf    sbuf_cut_va(sbuf* src, va_list args);
sbuf    sbuf__cut(sbuf* src, ...);
// Like `sbuf_cut_v`, but `items` are provided as variadic arguments.
#define sbuf_cut(src, ...) sbuf__cut(src, __VA_ARGS__, (sbuf){0})
// Like `sbuf_cut_c`, but occurences are searched at the end of `src`.
char    sbuf_rcut_c(sbuf* src, const sbuf set);
// Like `sbuf_cut_v`, but occurences are searched at the end of `src`.
int     sbuf_rcut_v(sbuf* src, const sbuf items[]);
// Like `sbuf_cut_va`, but occurences are searched at the end of `src`.
sbuf    sbuf_rcut_va(sbuf* src, va_list args);
sbuf    sbuf__rcut(sbuf* src, ...);
// Like `sbuf_cut`, but occurences are searched at the end of `src`.
#define sbuf_rcut(src, ...) sbuf__rcut(src, __VA_ARGS__, (sbuf){0})
// Returns a dynamically allocated buffer of size `size`. Its contents are undefined.
sbuf    sbuf_alloc(size_t size);
// Returns a dynamically allocated buffer of size `size`. Its contents are all zeros.
sbuf    sbuf_alloc_z(size_t size);
// Resizes a dynamically allocated buffer to `size`.
bool    sbuf_realloc(sbuf* obj, size_t size);
// Deallocates a dynamically allocated `obj` and sets it to NULL.
void    sbuf_dealloc(sbuf* obj);

#endif  // _SBUF_

#if defined(SBUF_IMPLEMENTATION) && !defined(_SBUF_IMPL_LOCK)
#define _SBUF_IMPL_LOCK
#include <errno.h>

sbuf sbuf_fromfile(FILE* fd)
{
	if (!fd) return (sbuf){0};
	char temp[1024];
	sbuf res = {0};
	while (feof(fd) == 0 && ferror(fd) == 0) {
		int chunk_size = fread(temp, 1, sizeof(temp), fd);
		if (!sbuf_realloc(&res, res.length + chunk_size)) {
			sbuf_dealloc(&res);
			return (sbuf){0};
		}
		memcpy(res.data + res.length - chunk_size, temp, chunk_size);
	}
	return res;
}

static const uint8_t char2digit[UINT8_MAX] = {
	['0'] = 0,   ['1'] = 1,
	['2'] = 2,   ['3'] = 3,
	['4'] = 4,   ['5'] = 5,
	['6'] = 6,   ['7'] = 7,
	['8'] = 8,   ['9'] = 9,
	['a'] = 0xa, ['A'] = 0xA,
	['b'] = 0xb, ['B'] = 0xB,
	['c'] = 0xc, ['C'] = 0xC,
	['d'] = 0xd, ['D'] = 0xD,
	['e'] = 0xe, ['E'] = 0xE,
	['f'] = 0xf, ['F'] = 0xF 
};

sbuf sbuf_unesc(sbuf src, sbuf* dst)
{
	sbuf stub_dst;
	if (!dst) {
		stub_dst = sbuf_alloc_z(src.length);
		dst = &stub_dst;
	}
	if (dst->length < src.length) return (sbuf){0};
	dst->length = 0;
	while (src.length) {
		if (src.data[0] == '\\' && src.length > 1) {
			switch (src.data[1]) {
				case '0': 
					dst->data[dst->length] = '\0';
					sbuf_shift(src, 2);
					break;
				case 'n':
					dst->data[dst->length] = '\n';
					sbuf_shift(src, 2);
					break;
				case 'r':
					dst->data[dst->length] = '\r';
					sbuf_shift(src, 2);
					break;
				case 't':
					dst->data[dst->length] = '\t';
					sbuf_shift(src, 2);
					break;
				case '\\':
					dst->data[dst->length] = '\\';
					sbuf_shift(src, 2);
					break;
				case 'x':
					if (src.length == 2) {
						dst->data[dst->length] = 'x';
						src.length = 0;
						break;
					}
					dst->data[dst->length] = char2digit[(unsigned)src.data[2]];
					if (src.length == 3) {
						src.length = 0;
						break;
					}
					dst->data[dst->length] = (dst->data[dst->length] << 4) | char2digit[(unsigned)src.data[3]];
					sbuf_shift(src, 4);
					break;
				default:
					dst->data[dst->length] = *src.data;
					sbuf_shift(src, 1);
			}
		} else {
			dst->data[dst->length] = *src.data;
			sbuf_shift(src, 1);
		}
		++dst->length;
	}
	return *dst;
}

size_t sbuf_utf8len(sbuf obj)
{
	size_t res = 0;
	sbuf_foreach_char (iter, obj)
		if (!(*iter & 128 && !(*iter & 64))) ++res;
	return res;
}

bool sbuf_startswith(sbuf obj, sbuf sub)
{
	if (sub.length > obj.length) return false;
	return memcmp(sub.data, obj.data, sub.length) == 0;
}

bool sbuf_endswith(sbuf obj, sbuf sub)
{
	if (sub.length > obj.length) return false;
	return memcmp(sub.data, obj.data + (obj.length - sub.length), sub.length) == 0;
}

bool sbuf_space(sbuf obj)
{
	sbuf_foreach_char (iter, obj)
		if (*iter > 32) return false;
	return true;
}

int sbuf_sub(sbuf src, sbuf* dst, sbuf sub_src, sbuf sub_dst)
{
	sbuf sub_src_array[2] = {sub_src};
	int src_count = sbuf_count_v(src, sub_src_array);
	*dst = sbuf_alloc(src.length + src_count * (sub_dst.length - sub_src.length));
	size_t offset = 0;
	while (true) {
		sbuf piece;
		if (sbuf_split_v(&src, &piece, sub_src_array) < 0) {
			memcpy(dst->data + offset, piece.data, piece.length);
			return src_count;
		}
		memcpy(dst->data + offset, piece.data, piece.length);
		offset += piece.length;
		memcpy(dst->data + offset, sub_dst.data, sub_dst.length);
		offset += sub_dst.length;
	}
}

sbuf sbuf__concat(int _, ...)
{
	va_list args;
	va_start(args, _);
	sbuf new, res = {0};
	while ((new = va_arg(args, sbuf)).data) {
		if (!sbuf_realloc(&res, res.length + new.length)) {
			sbuf_dealloc(&res);
			return (sbuf){0};
		}
		memcpy(res.data + res.length - new.length, new.data, new.length);
	}
	va_end(args);
	return res;
}

int sbuf_split_v(sbuf* src, sbuf* dst, sbuf delims[])
{
	dst->length = 0;
	dst->data = src->data;
	while (src->length) {
		int delim_id = sbuf_cut_v(src, delims);
		if (delim_id >= 0) return delim_id;
		sbuf_shift(*src, 1);
		dst->length++;
	}
	return -1;
}

sbuf sbuf__split(sbuf* src, sbuf* dst, ...)
{
	va_list args;
	dst->length = 0;
	dst->data = src->data;
	while (src->length) {
		va_start(args, dst);
		sbuf delim = sbuf_cut_va(src, args);
		if (delim.data) return delim;
		sbuf_shift(*src, 1);
		dst->length++;
	}
	return (sbuf){0};
}


int sbuf_rsplit_v(sbuf* src, sbuf* dst, sbuf delims[])
{
	*dst = *src;
	src->data += src->length;
	src->length = 0;
	while (dst->length > 0) {
		int delim_id = sbuf_rcut_v(dst, delims);
		if (delim_id >= 0) return delim_id;
		sbuf_shift(*src, -1);
		dst->length--;
	}
	return -1;
}

sbuf sbuf__rsplit(sbuf* src, sbuf* dst, ...)
{
	va_list args;
	*dst = *src;
	src->data += src->length;
	src->length = 0;
	while (dst->length > 0) {
		va_start(args, dst);
		sbuf delim = sbuf_rcut_va(dst, args);
		if (delim.data) return delim;
		sbuf_shift(*src, -1);
		--dst->length;
	}
	*dst = *src;
	src->length = 0;
	return (sbuf){0};
}

int sbuf_splitesc_v(sbuf* src, sbuf* dst, sbuf delims[])
{
	dst->length = 0;
	dst->data = src->data;
	char prevchar = '\0';
	while (src->length > 0) {
		for (int i = 0; delims[i].data; ++i)
			if (sbuf_startswith(*(sbuf*)src, delims[i]) && prevchar != '\\') {
				sbuf_shift(*src, delims[i].length);
				return i;
			}
		prevchar = *src->data;
		sbuf_shift(*src, 1);
		++dst->length;
	}
	return -1;
}


sbuf sbuf__splitesc(sbuf* src, sbuf* dst, ...)
{
	va_list args;
	dst->length = 0;
	dst->data = src->data;
	char prevchar = '\0';
	while (src->length > 0) {
		va_start(args, dst);
		for (sbuf delim = va_arg(args, sbuf); delim.data != NULL; delim = va_arg(args, sbuf))
			if (sbuf_startswith(*(sbuf*)src, delim) && prevchar != '\\') {
				sbuf_shift(*src, delim.length);
				va_end(args);
				return (sbuf){ .data = (char*)src->data - delim.length, .length = delim.length };
			}
		va_end(args);
		prevchar = *src->data;
		sbuf_shift(*src, 1);
		++dst->length;
	}
	return (sbuf){0};
}

bool sbuf_eq(sbuf item1, sbuf item2)
{
	if (item1.length != item2.length) return false;
	return memcmp(item1.data, item2.data, item1.length) == 0;
}

bool sbuf_int(sbuf src)
{
	if (!src.length) return false;
	sbuf charset = sbuf_fromcstr("0123456789");
	sbuf_cut(&src, sbuf_fromcstr("-"));
	if (sbuf_cut(&src, sbuf_fromcstr("0x")).data) {
		charset = sbuf_fromcstr("0123456789abcdefABCDEF");
	} else if (sbuf_cut(&src, sbuf_fromcstr("0o")).data) {
		charset = sbuf_fromcstr("01234567");
	} else if (sbuf_cut(&src, sbuf_fromcstr("0b")).data)
		charset = sbuf_fromcstr("01");
	sbuf_foreach_char(iter, src)
		if (memchr(charset.data, *iter, charset.length) == NULL) return false;
	return true;
}

long sbuf_toint(sbuf obj)
{
	if (!obj.length) return 0;
	bool is_negative = obj.data[0] == '-';
	if (is_negative)
		sbuf_shift(obj, 1);
	long res = 0;
	char base = 10;
	if (obj.length > 2 && *obj.data == '0') {
		switch (obj.data[1]) {
			case 'b': base = 2;  sbuf_shift(obj, 2); break;
			case 'o': base = 8;  sbuf_shift(obj, 2); break;
			case 'x': base = 16; sbuf_shift(obj, 2); break;
		}
	}
	size_t coef = 1;
	for (char* iter = &obj.data[obj.length - 1]; iter >= obj.data; --iter) {
		res += char2digit[*(uint8_t*)iter] * coef;
		coef *= base;
	}
	if (is_negative) res *= -1;
	return res;
}

sbuf sbuf_lstrip_c(sbuf* src, sbuf set)
{
	sbuf res = {.data = (char*)src->data, .length = 0};
	while (sbuf_cut_c(src, set)) ++res.length;
	return res;
}

sbuf sbuf_lstrip_v(sbuf* src, const sbuf items[])
{
	sbuf res = {.data = src->data, .length = 0};
	while (true) {
		int item = sbuf_cut_v(src, items);
		if (item < 0) break;
		res.length += items[item].length;
	}
	return res;
}

sbuf sbuf_lstrip_va(sbuf* src, va_list args)
{
	sbuf res = { .data = (char*)src->data, .length = 0 };
	va_list args_l;
	while (true) {
		va_copy(args_l, args);
		sbuf item = sbuf_cut_va(src, args_l);
		if (!item.length) break;
		res.length += item.length;
	}
	va_end(args);
	return res;
}

sbuf sbuf__lstrip(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	sbuf res = sbuf_lstrip_va(src, args);
	return res;
}

sbuf sbuf_rstrip_c(sbuf* src, sbuf set)
{
	sbuf res = { .data = (char*)src->data, .length = 0 };
	while (true) {
		char item = sbuf_cut_c(src, set);
		if (!item) break;
		res.length += 1;
	}
	return res;
}

sbuf sbuf_rstrip_v(sbuf* src, const sbuf items[])
{
	sbuf res = {.data = (char*)src->data + src->length, .length = 0};
	while (true) {
		int item = sbuf_cut_v(src, items);
		if (item < 0) break;
		sbuf_shift(res, -items[item].length);
	}
	return res;
}

sbuf sbuf_rstrip_va(sbuf* src, va_list args)
{
	sbuf res = {.data = (char*)src->data + src->length, .length = 0};
	va_list args_l;
	while (true) {
		va_copy(args_l, args);
		sbuf item = sbuf_cut_va(src, args_l);
		if (!item.length) break;
		sbuf_shift(res, -item.length);
	}
	va_end(args);
	return res;
}

sbuf sbuf__rstrip(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	sbuf res = sbuf_rstrip_va(src, args);
	return res;
}

static inline char fputcesc(FILE* fd, unsigned char obj, unsigned char format)
{
	if (format == SBUF_BFMT_HEX_LITERAL)
		return fprintf(fd, "\\x%02hhX", obj);
	switch (obj) {
		case '\0':
			return fwrite("\\0", 1, 2, fd);
		case '\t':
			return fwrite("\\t", 1, 2, fd);
		case '\n':
			return fwrite("\\n", 1, 2, fd);
		case '\r':
			return fwrite("\\r", 1, 2, fd);
		case '\\':
			return fwrite("\\\\", 1, 2, fd);
		case '\'':
			return format & SBUF_BFMT_ESC_QUOTE ? fwrite("\\'", 1, 2, fd) : fputc('\'', fd) != EOF;
		case '"':
			return format & SBUF_BFMT_ESC_DQUOTE ? fwrite("\\\"", 1, 2, fd) : fputc('"', fd) != EOF;
		default:
			if (obj > 0x1F && obj < 0x7F) {
				return fputc(obj, fd) != EOF;
			} else if (format & SBUF_BFMT_OCT) {
				return (fputc('\\', fd) != EOF)
					+ (fputc( obj & 7  /*0b00000111*/ + '0', fd) != EOF)
					+ (fputc((obj & 56 /*0b00111000*/ >> 3) + '0', fd) != EOF)
					+ (fputc((obj & 192/*0b11000000*/ >> 6) + '0', fd) != EOF);
			} else if (format & SBUF_BFMT_HEX) return fprintf(fd, "\\x%02hhX", obj);
			assert(false, "invalid escape format specified");
	}
}

size_t sbuf_fputesc(FILE* fd, sbuf obj, unsigned char format)
{
	register size_t n = 0;
	if (format & SBUF_BFMT_QUOTED) n += fputc('\'', fd) != EOF;
	if (format & SBUF_BFMT_DQUOTED) n += fputc('"', fd) != EOF;
	if (format & SBUF_BFMT_RAW) return fwrite(obj.data, 1, obj.length, fd);
	sbuf_foreach_char (iter, obj) {
		n += fputcesc(fd, *iter, format);
	}
	if (format & SBUF_BFMT_QUOTED) n += fputc('\'', fd) != EOF;
	if (format & SBUF_BFMT_DQUOTED) n += fputc('"', fd) != EOF;
	return n;
}

size_t str_fputesc(FILE* fd, const char* obj, unsigned char format)
{
	return sbuf_fputesc(fd, sbuf_fromstr((char*)obj), format);
}

size_t sbuf_count_c(sbuf obj, sbuf set)
{
	size_t res = 0;
	while (obj.length > 0) {
		if (memchr(set.data, *obj.data, set.length) != NULL) {
			res++;
		} else sbuf_shift(obj, 1);
	}
	return res;
}

size_t sbuf_count_v(sbuf obj, const sbuf items[])
{
	size_t res = 0;
	while (obj.length > 0) {
		if (sbuf_cut_v(&obj, items) >= 0) {
			res++;
		} else sbuf_shift(obj, 1);
	}
	return res;
}

size_t sbuf_count_va(sbuf obj, va_list args)
{
	size_t res = 0;
	va_list args_l;
	while (obj.length > 0) {
		va_copy(args_l, args);
		if (sbuf_cut_va(&obj, args_l).data) {
			res += 1;
		} else sbuf_shift(obj, 1);
	}
	va_end(args);

	return res;
}

size_t sbuf__count(sbuf obj, ...)
{
	va_list args;
	va_start(args, obj);
	size_t res = sbuf_count_va(obj, args);
	return res;
}

sbuf sbuf_copy(sbuf obj)
{
	return (sbuf){.data = memcpy(malloc(obj.length), obj.data, obj.length), .length = obj.length};
}

char sbuf_cut_c(sbuf* src, sbuf set)
{
	if (src->length == 0) return '\0';
	sbuf_foreach_char (iter, set)
		if (*src->data == *iter) {
			sbuf_shift(*src, 1);
			return *iter;
		}
	return '\0';
}

int sbuf_cut_v(sbuf* src, const sbuf items[])
{
	for (int i = 0; items[i].data; ++i)
		if (sbuf_startswith(*src, items[i])) {
			sbuf_shift(*src, items[i].length);
			return i;
		}
	return -1;
}

sbuf sbuf_cut_va(sbuf* src, va_list args)
{
	sbuf new;
	while ((new = va_arg(args, sbuf)).data)
		if (sbuf_startswith(*src, *(sbuf*)&new)) {
			sbuf_shift(*src, new.length);
			va_end(args);
			return new;
		}
	return (sbuf){0};
}

sbuf sbuf__cut(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	sbuf res = sbuf_cut_va(src, args);
	return res;
}

char sbuf_rcut_c(sbuf* src, sbuf set)
{
	if (src->length == 0) return '\0';
	char src_chr = src->data[src->length - 1];
	sbuf_foreach_char(iter, set)
		if (src_chr == *iter) return src->data[--src->length];
	return '\0';
}

int sbuf_rcut_v(sbuf* src, const sbuf items[])
{
	for (int i = 0; items[i].data; ++i)
		if (sbuf_endswith(*src, items[i])) {
			src->length -= items[i].length;
			return i;
		}
	return -1;
}

sbuf sbuf_rcut_va(sbuf* src, va_list args)
{
	sbuf new;
	while ((new = va_arg(args, sbuf)).data)
		if (sbuf_endswith(*src, new)) {
			src->length -= new.length;
			va_end(args);
			return new;
		}
	return (sbuf){0};
}

sbuf sbuf__rcut(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	return sbuf_rcut_va(src, args);
}

sbuf sbuf_alloc(size_t length)
{
	char* temp = malloc(length);
	return (sbuf){ .data = temp, .length = temp ? length : 0 };
}

sbuf sbuf_alloc_z(size_t length)
{
	char* temp = calloc(1, length);
	return (sbuf){ .data = temp, .length = temp ? length : 0 };
}

bool sbuf_realloc(sbuf* obj, size_t new_length)
{
	void* new;
	if ((new = realloc(obj->data, new_length)) || new_length == 0) {
		obj->length = new_length;
		obj->data = new;
		return true;
	}
	return false;
}

void sbuf_dealloc(sbuf* obj)
{
	free(obj->data);
	*obj = (sbuf){0};
}

#endif // SBUF_IMPLEMENTATION
