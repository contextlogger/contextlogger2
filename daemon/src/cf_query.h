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

  int force_get_ConfigDb_int(const gchar* name, int default_value);

  int force_lua_eval_int(const gchar* luaStr, int default_value);

  gboolean try_get_ConfigDb_bool(const gchar* name, gboolean* value, 
				 gboolean* found, GError** error);

  gboolean get_ConfigDb_bool(const gchar* name, gboolean* value, 
			     gboolean default_value, GError** error);

  gboolean force_get_ConfigDb_bool(const gchar* name, gboolean default_value);

  gboolean force_lua_eval_bool(const gchar* luaStr, gboolean default_value);

  // Caller takes ownership of the string set to "s". "s" is set to
  // NULL if no config entry by "name" is found, or if the entry has a
  // nil value.
  gboolean try_get_ConfigDb_str(const gchar* name, gchar** s, 
				GError** error);

  // Like try_get_ConfigDb_str, but if *s would get set to NULL, then
  // instead sets it to strdup'ed copy of "default_s" (except if
  // "default_s" itself is NULL).
  gboolean get_ConfigDb_str(const gchar* name, gchar** s, 
			    gchar* default_s, GError** error);

  int get_config_iap_id();

  // The returned buffer is static, and not to be freed.
  gchar* get_config_username();

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __cf_query_h__ */
