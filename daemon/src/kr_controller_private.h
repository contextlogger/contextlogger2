#ifndef __kr_controller_private_h__
#define __kr_controller_private_h__

#include "kr_controller.h"

#include "application_config.h"

#include "ac_app_context_private.h"
#include "cf_rcfile.h"
#include "config_db.h"
#include "local_server.h"
#include "log-db.h"
#include "kr_plat_ao.h"
#include "rk_remokon.h"
#include "sa_array.h"
#include "up_uploader.h"

#ifdef __cplusplus
extern "C" {
#endif

  // Deprecated. xxx
  kr_Controller* getGlobalClient();

  struct _kr_Controller {
#if HAVE_PLAT_AO
    kr_PlatAo* platAo;
#endif

    ac_AppContext* appContext;

    LogDb* log; // owned

    ConfigDb* configDb; // owned

    cf_RcFile* rcFile; // owned

    sa_Array* scanner; // owned

#if __FEATURE_LOCALSERVER__
    LocalServer* localServer; // owned
#endif

#if __FEATURE_REMOKON__
    rk_Remokon* remokon; // owned
#endif

#if __FEATURE_UPLOADER__
    up_Uploader* uploader; // owned
#endif

    gboolean are_uploads_allowed;
    gboolean is_cellular_ap;
    int non_roaming_mcc; // -1 for none configured
    int current_mcc; // -1 for no network
    int current_signal_strength; // +1 for no network
  };

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __kr_controller_private_h__ */

/**

kr_controller_private.h

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
