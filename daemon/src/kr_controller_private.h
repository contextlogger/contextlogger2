#ifndef __kr_controller_private_h__
#define __kr_controller_private_h__

#include "kr_controller.h"

#include "application_config.h"

#include "cf_rcfile.h"
#include "config_db.h"
#include "local_server.h"
#include "log-db.h"
#include "rk_remokon.h"
#include "sa_array.h"
#include "up_uploader.h"

#ifdef __cplusplus
extern "C" {
#endif

  kr_Controller* getGlobalClient();

  struct _kr_Controller {
    LogDb* log; // owned

    ConfigDb* configDb; // owned

    cf_RcFile* rcFile; // owned

    sa_Array* scanner; // owned

    LocalServer* localServer; // owned

#if __FEATURE_REMOKON__
    rk_Remokon* remokon; // owned
#endif

#if __FEATURE_UPLOADER__
    up_Uploader* uploader; // owned
#endif
  };

#ifdef __cplusplus
} /* extern "C" */
#endif

#define cf_STATIC_GET(key) \
  (cf_RcFile_get_##key(getGlobalClient()->rcFile))

  // The caller must free any returned value.
#define cf_DYNAMIC_GET(_key) \
  (ConfigDb_get_generic(getGlobalClient()->configDb, _key, NULL))

  // The caller must free any returned value, and free any set GError
  // value.
#define cf_DYNAMIC_GET_ERR(_key,_errorpp) \
  (ConfigDb_get_generic(getGlobalClient()->configDb, _key, _errorpp))

  // The caller must free any set GError value.
#define cf_DYNAMIC_SET_ERR(_key,_value,_errorpp) \
  (ConfigDb_set_generic(getGlobalClient()->configDb, _key, _value, _errorpp))

#endif /* __kr_controller_private_h__ */
