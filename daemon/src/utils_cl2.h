// These are CL2 specific utilities.

#ifndef __utils_cl2_h__
#define __utils_cl2_h__

#include <glib.h>
#include "common/platform_config.h"

G_BEGIN_DECLS

gboolean mkdir_p(const gchar* pathname, GError** error);

gboolean rm_file(const gchar* pathname, GError** error);

gboolean is_ascii_ident(const gchar* s);

#define DECREF(x) { g_object_unref(x); x = NULL; }
#define XDECREF(x) { if (x) DECREF(x) }

#ifdef __cplusplus
#define DELETE_Z(x) { delete x; x = NULL; }
#endif // __cplusplus

#define FREE_Z(_x,_f) { if (_x) { _f(_x); (_x) = NULL; } }

#if !GLIB_CHECK_VERSION(2,14,6)
void g_string_vprintf(GString *string,
		      const gchar *format,
		      va_list args);
#endif

#if PRINTF_DOUBLE_BUGGY
// This may produce a GLib OOM error on Symbian.
void g_string_append_printf_fix(GString *gs,
				const gchar *fmt,
				...);
gint g_snprintf_fix(gchar *string,
		    gulong n,
		    gchar const *format,
		    ...);
#else
#define g_string_append_printf_fix(arg...) g_string_append_printf(arg)
#define g_snprintf_fix(arg...) g_snprintf(arg)
#endif // PRINTF_DOUBLE_BUGGY

G_END_DECLS

#ifdef __EPOC32__
#ifdef __cplusplus

/*
 !concept {:name => "Double byte string / UTF-8 conversion utilities",
   :desc => "A number of different utility functions for conveniently converting between UTF-8 and the native UTF-16 encoding on Symbian."}
*/

#include <e32std.h>

HBufC8* ConvToUtf8ZL(const TDesC& name16);

HBufC8* ConvToUtf8Z(const TDesC& name16);

gchar* ConvToUtf8CStringL(const TDesC& name16);

gchar* ConvToUtf8CString(const TDesC& name16);

int ConvToUtf8CString(gchar* buf, int bufLen, const TDesC& name16);

HBufC* ConvFromUtf8L(const TDesC8& name8);

HBufC* ConvFromUtf8CStringL(const char* s);

#include "common/epoc-session.hpp"

#endif // __cplusplus
#endif // __EPOC32__

#endif /* __utils_cl2_h__ */

/**

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
