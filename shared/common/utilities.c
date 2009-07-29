#include "common/utilities.h"
#include <string.h>
#include <glib.h>

// This fixes a bug in Open C MR.
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
