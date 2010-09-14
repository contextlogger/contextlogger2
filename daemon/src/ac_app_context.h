// The idea behind this API is that it decides what parts of the
// internal application state (that in our case resides in the
// controller, for the most part) is exposed to other components
// within this application. This increases API stability, as we can
// just pass a single argument for access to app state when creating a
// component, or most components anyway, ones that are not tightly
// coupled with the controller. That is, the purpose of this API is
// encapsulation. It is also a good place to maintain shared state
// required by multiple components; an example of such state would be
// Symbian client/server session handles.

#ifndef __ac_app_context_h__
#define __ac_app_context_h__

// --------------------------------------------------
// common API
// --------------------------------------------------

#include "bb_blackboard.h"
#include "config_db.h"
#include "cf_rcfile.h"
#include "kr_controller.h"
#include "log-db.h"

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _ac_AppContext ac_AppContext;

  ac_AppContext* ac_get_global_AppContext();

  typedef struct {
    time_t last_upload_time;
  } ac_Registry;

  ac_Registry* ac_get_Registry(ac_AppContext* self);

  bb_Blackboard* ac_get_Blackboard(ac_AppContext* self);

  const char* ac_get_logdb_file(ac_AppContext* self);
  const char* ac_get_logdb_dir(ac_AppContext* self);
  const char* ac_get_log_uploads_dir(ac_AppContext* self);

  kr_Controller* ac_Controller(ac_AppContext* self);

  LogDb* ac_LogDb(ac_AppContext* self);

  cf_RcFile* ac_RcFile(ac_AppContext* self);

  ConfigDb* ac_ConfigDb(ac_AppContext* self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#define ac_global_Controller ac_Controller(ac_get_global_AppContext())
#define ac_global_LogDb ac_LogDb(ac_get_global_AppContext())
#define ac_global_RcFile ac_RcFile(ac_get_global_AppContext())
#define ac_global_ConfigDb ac_ConfigDb(ac_get_global_AppContext())
#define ac_global_Registry ac_get_Registry(ac_get_global_AppContext())
#define ac_global_Blackboard ac_get_Blackboard(ac_get_global_AppContext())
#define LOGDB_FILE ac_get_logdb_file(ac_get_global_AppContext())
#define LOGDB_DIR ac_get_logdb_dir(ac_get_global_AppContext())
#define LOG_UPLOADS_DIR ac_get_log_uploads_dir(ac_get_global_AppContext())

#if defined(__SYMBIAN32__)
#define DATABASE_DRIVE_LETTER (LOGDB_FILE [0])
#endif /* __SYMBIAN32__ */

// --------------------------------------------------
// backwards compatibility
// --------------------------------------------------

#define ac_STATIC_GET(key) \
  (cf_RcFile_get_##key(ac_global_RcFile))

  // The caller must free any returned value.
#define ac_DYNAMIC_GET(_key) \
  (ConfigDb_get_generic(ac_global_ConfigDb, _key, NULL))

  // The caller must free any returned value, and free any set GError
  // value.
#define ac_DYNAMIC_GET_ERR(_key,_errorpp) \
  (ConfigDb_get_generic(ac_global_ConfigDb, _key, _errorpp))

  // The caller must free any set GError value.
#define ac_DYNAMIC_SET_ERR(_key,_value,_errorpp) \
  (ConfigDb_set_generic(ac_global_ConfigDb, _key, _value, _errorpp))

// --------------------------------------------------
// Symbian API
// --------------------------------------------------

#ifdef __cplusplus
#if defined(__SYMBIAN32__)

class RFs;
RFs& ac_Fs(ac_AppContext* self);

class CTelephony;
CTelephony& ac_Telephony(ac_AppContext* self);

class CContactDatabase;
CContactDatabase& ac_ContactDatabase(ac_AppContext* self);

#endif /* __SYMBIAN32__ */
#endif

#endif /* __ac_app_context_h__ */

/**

ac_app_context.h

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
