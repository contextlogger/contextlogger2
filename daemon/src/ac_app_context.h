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

#include "config_db.h"
#include "cf_rcfile.h"
#include "log-db.h"

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _ac_AppContext ac_AppContext;

  // xxx eventually we will deprecate getGlobalContext
  ac_AppContext* ac_get_global_AppContext();

  LogDb* ac_LogDb(ac_AppContext* self);

  cf_RcFile* ac_RcFile(ac_AppContext* self);

  ConfigDb* ac_ConfigDb(ac_AppContext* self);

#if __NEED_IMEI__
  // Returned buffer must not be freed.
  const char* ac_Imei(ac_AppContext* self);
#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#define ac_global_LogDb ac_LogDb(ac_get_global_AppContext())
#define ac_global_RcFile ac_RcFile(ac_get_global_AppContext())
#define ac_global_ConfigDb ac_ConfigDb(ac_get_global_AppContext())

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

#endif /* __SYMBIAN32__ */
#endif

#endif /* __ac_app_context_h__ */
