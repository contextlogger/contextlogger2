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

#if !GLIB_CHECK_VERSION(2,14,6)
void g_string_vprintf(GString *string,
		      const gchar *format,
		      va_list args);
#endif

#if PRINTF_DOUBLE_BUGGY
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

#include <e32std.h>

HBufC8* ConvToUtf8ZL(const TDesC& name16);

HBufC8* ConvToUtf8Z(const TDesC& name16);

gchar* ConvToUtf8CStringL(const TDesC& name16);

gchar* ConvToUtf8CString(const TDesC& name16);

int ConvToUtf8CString(gchar* buf, int bufLen, const TDesC& name16);

HBufC* ConvFromUtf8L(const TDesC8& name8);

#include "common/epoc-session.hpp"

#endif // __cplusplus
#endif // __EPOC32__

#endif /* __utils_cl2_h__ */
