// sbuf - Sized Buffer Implementation
// sized buffers provide utilities for parsing and manipulating text
#ifndef _SBUF_
#define _SBUF_

#include "stdio.h"
#include "string.h"
#include "stdbool.h"
#include "stdarg.h"
#include "ctxalloc.h"

#define sbuf_format "%.*s"
#define unpack(array) (int)array.length, array.data

#define fileloc_format sbuf_format":%u:%u"
#define fileloc_arg(obj) unpack(obj.filename), obj.lineno, obj.colno

#define sbufshift(obj, offset) obj.length -= offset; obj.data += offset;
#define sbufpshift(obj, offset) obj->length -= offset; obj->data += offset;

#define fromcstr(src) (sbuf){ .data = src, .length = sizeof(src) - 1 }
#define fromstr(src) (sbuf){ .data = src, .length = strlen(src) }
#define sbufslice(obj, start, end) (sbuf){ .data = obj.data + start, .length = (end < 0 ? obj.length : end) - start }

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

typedef struct sfile {
	sbuf name;
	sbuf content;
} sfile;

const sbuf CSTRTERM = { .data = "\0", .length = 1 };

sbuf _sbufconcat(heapctx_t ctx, ...);
sbuf filecontent(FILE* fd, heapctx_t ctx);
sfile openfile(sbuf filename, heapctx_t ctx);
char lowerchar(char src);
sbuf _sbufsplitesc(sbuf* src, sbuf* dst, ...);
int sbufsplitescv(sbuf* src, sbuf* dst, sbuf delims[]);
sbuf _sbufsplit(sbuf* src, sbuf* dst, ...);
int sbufsplitv(sbuf* src, sbuf* dst, sbuf delims[]);
sbuf sbufunesc(sbuf src, heapctx_t ctx);
sbuf_size_t sbufutf8len(sbuf obj);
bool sbufascii(sbuf obj);
bool sbufspace(sbuf obj);
bool sbufstartswith(sbuf obj, sbuf sub);
bool sbufendswith(sbuf obj, sbuf sub);
bool sbufeq(sbuf item1, sbuf item2);
bool sbufint(sbuf src);
long sbuftoint(sbuf obj);
sbuf_size_t sbufstripl(sbuf* src, sbuf items);
char fputcesc(FILE* fd, char obj, unsigned char format);
sbuf_size_t fputsbufesc(FILE* fd, sbuf obj, unsigned char format);
sbuf_size_t sbufindex(sbuf obj, sbuf sub);
sbuf_size_t sbufcount(sbuf obj, sbuf items);
sbuf sbufcopy(sbuf obj, heapctx_t ctx);
sbuf _sbufcut(sbuf* src, ...);
sbuf sbufwrite(sbuf dst, sbuf src, sbuf_size_t offset);
// Sized Contextual Allocator - context-based memory allocator using sized buffers
sbuf sctxalloc_new(long length, heapctx_t ctx);
long sctxalloc_resize(sbuf* obj, long new_length);
long sctxalloc_free(sbuf* obj);

#define streq(str1, str2) sbufeq(fromstr(str1), fromstr(str2))
#define sbufconcat(ctx, ...) _sbufconcat(ctx, __VA_ARGS__, (sbuf){0})
#define tostr(ctx, ...) (sbufconcat(ctx, __VA_ARGS__, CSTRTERM).data)
#define sbufsplit(src, dst, ...) _sbufsplit(src, dst, __VA_ARGS__, (sbuf){0})
#define sbufsplitesc(src, dst, ...) _sbufsplitesc(src, dst, __VA_ARGS__, (sbuf){0})
#define sbufcut(src, ...) _sbufcut(src, __VA_ARGS__, (sbuf){0})

#define fputsbuflnesc(fd, obj, format) fputsbufesc(fd, obj, format); fputc('\n', fd)
#define putsbufesc(obj, format) fputsbufesc(stdout, obj, format)
#define putsbuflnesc(obj, format) putsbufesc(obj, format); putchar('\n')
#define putcharesc(obj, format) fputcesc(stdout, obj, format)

#define fputsbuf(fd, obj) fputsbufesc(fd, obj, BYTEFMT_RAW)
#define putsbuf(obj) fputsbuf(stdout, obj)
#define fputsbufln(fd, obj) fputsbuf(fd, obj); fputc('\n', fd)
#define putsbufln(obj) fputsbufln(stdout, obj)

#endif  // _SBUF_

#ifdef SBUF_IMPLEMENTATION
#undef SBUF_IMPLEMENTATION

#define cast(type, expr) (type)(expr)

sbuf _sbufconcat(heapctx_t ctx, ...)
{
	va_list args;
	va_start(args, ctx);
	sbuf res = sctxalloc_new(0, ctx);
	sbuf new;
	while ((new = va_arg(args, sbuf)).data) {
		if (!sctxalloc_resize(&res, res.length + new.length)) {
			sctxalloc_free(&res);
			return (sbuf){0};
		}
		memcpy(res.data + res.length - new.length, new.data, new.length);
	}
	va_end(args);
	return res;
}


//returns a sized buffer created containing output fetched from the file descriptor `fd`.
// if an error occures, a zero-initialized sized string is returned.
sbuf filecontent(FILE* fd, heapctx_t ctx)
{
	char temp[1024];
	sbuf res = sctxalloc_new(0, ctx);
	int chunk_size;

	if (fd == NULL || ferror(fd) != 0 || feof(fd)) { return (sbuf){0}; }

	while (feof(fd) == 0 && ferror(fd) == 0)
	{
		int chunk_size = fread(temp, 1, sizeof(temp), fd);
		if (!sctxalloc_resize(&res, res.length + chunk_size)) {
			sctxalloc_free(&res);
			return (sbuf){0};
		}
		memcpy(res.data + res.length - chunk_size, temp, chunk_size);
	}

	fclose(fd);
	return res;
}

// utility wrapper for `filecontent` function, it opens a file, checks for errors, fetches its content
// closes the file descriptor and returns a sized file object.
// if an error occurs, a zero-initialized file is returned.
sfile openfile(sbuf filename, heapctx_t ctx) {
	enter_tempctx(funcctx, 0);
	char* temp = tostr(TEMP_CTX, filename); 
	FILE* fd = fopen(temp, "r");

	if (fd == NULL)
	{
		exit_tempctx(funcctx);
		return (sfile){0};
	}
	exit_tempctx(funcctx);
	return (sfile){
		.name = filename,
		.content = filecontent(fd, ctx)
	};
}

char lowerchar(char src) { return src >= 65 && src <= 90 ? src + 32 : src; }

// returns a bool, indicating whether `item` is in sbuf object `src`
bool sbufcontains(sbuf src, char item)
{
	for (int i = 0; i < src.length; i++)
	{
		if (src.data[i] == item) { return true; }
	}
	return false;
}

sbuf _sbufsplit(sbuf* src, sbuf* dst, ...)
{
	va_list args;
	dst->length = 0;
	dst->data = src->data;
	while (src->length > 0)
	{
		va_start(args, dst);
		for (sbuf delim = va_arg(args, sbuf); delim.data != NULL; delim = va_arg(args, sbuf))
		{
			if (sbufstartswith(*src, delim))
			{
				sbufpshift(src, delim.length);
				va_end(args);
				return (sbuf){ .data = src->data - delim.length, .length = delim.length };
			}
		}
		va_end(args);
		sbufpshift(src, 1);
		dst->length++;
	}
	return (sbuf){0};
}

int sbufsplitv(sbuf* src, sbuf* dst, sbuf delims[])
{
	dst->length = 0;
	dst->data = src->data;
	while (src->length > 0)
	{
		for (int i = 0; delims[i].data; i++)
		{
			if (sbufstartswith(*src, delims[i]))
			{
				sbufpshift(src, delims[i].length);
				return i;
			}
		}
		sbufpshift(src, 1);
		dst->length++;
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
		for (sbuf delim = va_arg(args, sbuf); delim.data != NULL; delim = va_arg(args, sbuf))
		{
			if (sbufstartswith(*src, delim) && prevchar != '\\')
			{
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
	while (src->length > 0)
	{
		for (int i = 0; delims[i].data; i++)
		{
			if (sbufstartswith(*src, delims[i]) && prevchar != '\\')
			{
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

// returns a sized string with data from the sized string `src` unescaped in-place.
sbuf sbufunesc(sbuf src, heapctx_t ctx)
{
	sbuf res = sctxalloc_new(src.length, ctx);
	res.length = 0;

	for (int i = 0; src.length > 0; i++)
	{
		if (*src.data == '\\')
		{
			sbufshift(src, 1);
			if (i == src.length)
			{
				break;
			}
			switch (*src.data)
			{
				case 'n':  res.data[i] = '\n'; sbufshift(src, 1); break;
				case 'r':  res.data[i] = '\r'; sbufshift(src, 1); break;
				case 't':  res.data[i] = '\t'; sbufshift(src, 1); break;
				case '\\': res.data[i] = '\\'; sbufshift(src, 1); break;
				case 'x':
					sbufshift(src, 1);
					if (*src.data == '\0') { res.data[i] = 'x'; break; }
					res.data[i] = (lowerchar(*src.data) >= 97 ? *src.data - 97 : *src.data - 48) * 16;

					sbufshift(src, 1);
					if (*src.data == '\0') { res.data[i] /= 16; break; }
					res.data[i] /= lowerchar(*src.data) >= 97 ? *src.data - 97 : *src.data - 48;
					break;
				default: res.data[i] = *src.data; break;
			}
		} else { res.data[i] = *src.data; sbufshift(src, 1); }
		res.length++;
	}
	return res;
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

// returns `true` if the sized string `obj` only consists of tabs, newlines, spaces or other control characters,
// otherwise returns `false` 
bool sbufspace(sbuf obj)
{
	for (size_t i = 0; i < obj.length; i++) {
		if (obj.data[i] > 32) return false;
	}
	return true;
}

// returns `true` if the sized string `obj` starts with the sequence, equal to the sequence in the sized string `sub`,
// otherwise returns `false`
bool sbufstartswith(sbuf obj, sbuf sub)
{
	if (sub.length > obj.length) return false;
	for (size_t i = 0; i < sub.length; i++) {
		if (obj.data[i] != sub.data[i]) return false;
	}
	return true;
}

// returns `true` if the sized string `obj` ends with the sequence, equal to the sequence in the sized string `sub`,
// otherwise returns `false`
bool sbufendswith(sbuf obj, sbuf sub)
{
	size_t offset = obj.length - sub.length;
	if (sub.length > obj.length) { return false; }
	for (sbuf_size_t i = offset; i < obj.length; i++) {
		if (obj.data[i] != sub.data[i - offset]) return false;
	}
	return true;
}

// returns `true` if the sized strings `item1` and `item2` are equal byte-by-byte, otherwise returns `false`
bool sbufeq(sbuf item1, sbuf item2)
{
	if ( item1.length != item2.length ) return false;
	for (int i = 0; i < item1.length; i++) {
		if (item1.data[i] != item2.data[i]) return false;
	}
	return true;
}

// returns true if the sized string `src` can be interpreted as an integer literal, otherwise returns `false`.
// the function is intended to be used in conjunction with the `sbuftoint` function.
bool sbufint(sbuf src)
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
long sbuftoint(sbuf obj)
{
	long res = 0;
	char base = 10;
	if (obj.length > 2 && *obj.data == '0')
	{
		switch (obj.data[1])
		{
			case 'b': base = 2;  obj.length -= 2; obj.data += 2; break;
			case 'o': base = 8;  obj.length -= 2; obj.data += 2; break;
			case 'x': base = 16; obj.length -= 2; obj.data += 2; break;
		}
	}

	char cur_char;
	sbuf_size_t coef = 1;
	for (sbuf_size_t i = obj.length - 1; i >= 0 ; i--){
		cur_char = lowerchar(obj.data[i]);
		res += ((cur_char > '9' ? cur_char - ('a' - '9' - 1) : cur_char) - '0') * coef;
		coef *= base;
	}

	return res;
}

// strips the sbuf object `src` of the characters in an sbuf object `items` from the left, 
// and returns the number of characters stripped
sbuf_size_t sbufstripl(sbuf* src, sbuf items)
{
	sbuf_size_t orig_length = src->length;
	while (src->length > 0) if (memmem(items.data, items.length, src->data, 1) != NULL)
	{
		sbufpshift(src, 1);
	} else break;
	return orig_length - src->length;
}

// writes the character `obj` with escaping specified by a set of flags `format` to the file descriptor `fd`
// supported formatting flags:
//	BYTEFMT_HEX - write unprintable characters in the form of "\x<hexadecimal value of the character>"
//	BYTEFMT_OCT - write unprintable characters in the form of "\<octal value of the character>"
//      BYTEFMT_HEX_LITERAL - write all characters in the form of "\x<hexadecimal value of the character>"
//      BYTEFMT_RAW - write all characters as is, equivalent to fputc(obj, fd)
//	BYTEFMT_ESC_QUOTE - escape single quote character ("\'" instead of "'")
//	BYTEFMT_ESC_DQUOTE = escape double quote character ( "\"" instead of """)
char fputcesc(FILE* fd, char obj, unsigned char format)
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
	else if (obj == '\a')				    { n += fwrite("\\a",  1, 2, fd); }
	else if (obj == '\b')				    { n += fwrite("\\b",  1, 2, fd); }
	else if (obj == '\t')				    { n += fwrite("\\t",  1, 2, fd); }
	else if (obj == '\n')				    { n += fwrite("\\n",  1, 2, fd); }
	else if (obj == '\v')				    { n += fwrite("\\v",  1, 2, fd); }
	else if (obj == '\f')				    { n += fwrite("\\f",  1, 2, fd); }
	else if (obj == '\r')				    { n += fwrite("\\r",  1, 2, fd); }
	else if (obj == '\e')				    { n += fwrite("\\e",  1, 2, fd); }
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
		char temp = obj & 0xf0 >> 8; 
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

// returns the amount of occurences of characters from sized string `items` in sized string `obj`
sbuf_size_t sbufcount(sbuf obj, sbuf items)
{
	sbuf_size_t res = 0;
	while (obj.length > 0) {
		if (memmem(items.data, items.length, obj.data, 1) != NULL) res++;
		sbufshift(obj, 1);
	}
	return res;
}

// copies data of the sized buffer `obj` to the heap and returns sized string with the copied data 
sbuf sbufcopy(sbuf obj, heapctx_t ctx)
{
	sbuf res = sctxalloc_new(obj.length, ctx);
	memcpy(res.data, obj.data, obj.length);
	return res;
}

sbuf _sbufcut(sbuf* src, ...)
{
	va_list args;
	va_start(args, src);
	sbuf new;
	while ((new = va_arg(args, sbuf)).data != NULL) {
		if (sbufstartswith(*src, new)) {
			va_end(args);
			sbufpshift(src, new.length);
			return new;
		}
	}
	va_end(args);
	return (sbuf){0};
}

sbuf sbufwrite(sbuf dst, sbuf src, sbuf_size_t offset)
{
	if (dst.data && src.data && dst.length - offset >= src.length) {
		memcpy(dst.data + offset, src.data, src.length);
		sbufshift(dst, offset + src.length);
	}
	return dst;
}

sbuf sctxalloc_new(long length, heapctx_t ctx)
{
	char* temp = ctxalloc_new(length, ctx);
	return (sbuf){ .data = temp, .length = temp ? length : 0 };
}

long sctxalloc_resize(sbuf* obj, long new_length)
{
	long prevlen = obj->length;
	void* new;
	if ((new = ctxalloc_resize(obj->data, new_length))) {
		obj->length = new_length;
		obj->data = new;
		return new_length - prevlen;
	}
	return 0;
}
long sctxalloc_free(sbuf* obj)
{
	long prevsize = obj->length;
	if (!ctxalloc_free(obj->data)) {
		obj->length = 0;
		return 0 - prevsize;
	}
	return 0;
}

#endif // SBUF_IMPLEMENTATION
