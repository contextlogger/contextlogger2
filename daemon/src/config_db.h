// This component takes care of the initialization and cleanup of the
// configuration database (ConfigDb), and provides an API for
// modifying and querying it. The NO_THREAD_SAFETY option controls
// whether the query/modification API is thread safe.

#ifndef __config_db_h__
#define __config_db_h__

#include "er_errors.h" // is_not_found_error

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _ConfigDb ConfigDb;

  ConfigDb* ConfigDb_new(GError** error);
  
  void ConfigDb_destroy(ConfigDb* self);

  // If a setter returns an error, that typically means that there has
  // been a validation error, i.e. that the setting was not changed
  // due to the specified (new) value failing to validate. That said,
  // the only validation that really can be done is to check the (Lua)
  // syntax of the specified value.

  // The caller takes ownership of the returned UTF-8 string. NULL is
  // returned if a matching value is not found or for some other
  // reason cannot be retrieved. Any error can optionally be set to
  // "error". You may use is_not_found_error to check for the not
  // found error case.
  gchar* ConfigDb_get_generic(ConfigDb* self, 
			      const gchar* name,
			      GError** error);

  // The "value" must be a UTF-8 string, and it should be a valid Lua
  // expression.
  gboolean ConfigDb_set_generic(ConfigDb* self, 
				const gchar* name, 
				const gchar* value, 
				GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __config_db_h__ */
