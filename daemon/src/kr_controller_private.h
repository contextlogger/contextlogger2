#ifndef __kr_controller_private_h__
#define __kr_controller_private_h__

#include "kr_controller.h"

#include "cf_rcfile.h"
#include "config_db.h"
#include "local_server.h"
#include "log-db.h"
#include "sa_array.h"
#include "up_uploader.h"

#include "common/evq_event.h"

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

    up_Uploader* uploader; // owned

    // This queue is passed to the subcomponents of this component, to
    // enable them to do event delivery.
    // 
    // The caller may interact directly with the event loop by calling
    // event_loop and event_loop_stop as desired. Of course, the
    // event_loop_stop will have to originate from a different thread,
    // since event_loop does not return before a call to
    // event_loop_stop. There may also be alternative ways of
    // interacting with the event loop; particularly on Symbian there is
    // platform-provided machinery for doing that.
    // 
    // The event queue is owned by this component due to a shortcoming
    // in our event system. Basically, we must ensure that there are no
    // events originated by CL2 after the ClientCl2 instance has been
    // deleted. However, currently the only mechanism for doing this is
    // to invoke event_close, and this means that no one else will be
    // able to use the queue either any longer after that. Us owning the
    // queue here reflects this reality, but ideally the event queue
    // would encompass the entire application, and be passed to
    // ClientCl2 upon initialization. This anyway kind of is the
    // situation with the Symbian built in event mechanism, and
    // reflecting that design here would be more ideal.
    EventQueue eventQueue;
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
