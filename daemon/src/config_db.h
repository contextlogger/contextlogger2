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

/**

config_db.h

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
