#include "common/utilities.h"
#include <string.h>
#include <glib.h>

// This fixes a bug in Open C MR.
//
// xxx This bug might no longer apply for newer versions of Open C.
#ifdef __SYMBIAN32__
#undef g_utf8_next_char
#define g_utf8_next_char(p) (char *)((p) + *(*_g_utf8_skip() + *(guchar *)(p)))
#endif

// Forms a JSON String text out of the given UTF-8 text. The string
// will be quoted, and still UTF-8.
// 
// "JSON text SHALL be encoded in Unicode. The default encoding is
// UTF-8. All Unicode characters may be placed within the quotation
// marks except for the characters that must be escaped: quotation
// mark, reverse solidus, and the control characters (U+0000 through
// U+001F)."
//
// The caller is responsible for freeing the result buffer.
EXTERN_C char* utf8ToJsonString(const char* text)
{
    GString* gs = g_string_sized_new(strlen(text) + 8);

    gunichar ch;
    g_string_append_c(gs, '\"');
    while (*text) {
	ch = g_utf8_get_char(text);

	if (ch == '"') {
	    g_string_append(gs, "\\\"");
	} else if (ch == '\\') {
	    g_string_append(gs, "\\\\");
	} else if (ch > 0 && ch < 0x20) {
	    g_string_append_printf(gs, "\\u%04x", ch);
	} else {
	    g_string_append_unichar(gs, ch);
	}

	text = g_utf8_next_char(text);
    }
    g_string_append_c(gs, '\"');

    char* res = gs->str;
    g_string_free(gs, FALSE);
    return res;
}

/**

utilities.c

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

 **/
