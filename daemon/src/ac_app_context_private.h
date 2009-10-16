#ifndef __ac_app_context_private_h__
#define __ac_app_context_private_h__

#include "ac_app_context.h"

#include "kr_controller.h"

//
// This internal API is only for controller and application context
// implementation.
//

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  ac_AppContext* ac_AppContext_new(GError** error);

  void ac_AppContext_set_controller(ac_AppContext* self, kr_Controller* kr);

  void ac_AppContext_destroy(ac_AppContext* self);

  void ac_set_global_AppContext(ac_AppContext* ac);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __ac_app_context_private_h__ */
