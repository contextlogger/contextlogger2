#ifndef __cf_query_h__
#define __cf_query_h__

#include "application_config.h"

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  // Returns FALSE iff there is an error. In the "not found" case TRUE
  // is returned, and "value" is not set; "found" is set to indicate
  // if something was found, if it is non-NULL. "found" is also set to
  // FALSE if the config setting is "nil" rather than an integer.
  gboolean try_get_ConfigDb_int(const gchar* name, int* value, 
				gboolean* found, GError** error);

  gboolean get_ConfigDb_int(const gchar* name, int* value, 
			    int default_value, GError** error);

  gboolean try_get_ConfigDb_bool(const gchar* name, gboolean* value, 
				 gboolean* found, GError** error);

  gboolean get_ConfigDb_bool(const gchar* name, gboolean* value, 
			     gboolean default_value, GError** error);

  // Caller takes ownership of the string set to "s". "s" is set to
  // "default_s" (which may be NULL) if no config entry found, or if
  // its value evaluates to a nil value.
  gboolean get_ConfigDb_str(const gchar* name, gchar** s, 
			    const gchar* default_s, GError** error);

#define try_get_ConfigDb_str(n,s,e) \
  get_ConfigDb_str(n, s, NULL, e)

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __cf_query_h__ */
