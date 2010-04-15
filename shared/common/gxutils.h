#ifndef __gxutils_h__
#define __gxutils_h__

// Utilities that require a GLib include in the header.

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  gchar* gx_strdup (const gchar *str);

  //GQuark gx_quark_from_static_string (const gchar *string);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __gxutils_h__ */

