#ifndef __ac_app_context_private_h__
#define __ac_app_context_private_h__

#include "ac_app_context.h"

#include "kr_controller.h"

#include "common/platform_config.h"

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

  // Refreshes settings from the configuration file.
  // Any previous settings are lost.
  // Should be called once the configuration file has been read.
  gboolean ac_AppContext_configure(ac_AppContext* self, GError** error);

  void ac_AppContext_destroy(ac_AppContext* self);

  void ac_set_global_AppContext(ac_AppContext* ac);

#ifdef __cplusplus
} /* extern "C" */
#endif

#if __SYMBIAN_CXX__

class MAppContextInitObserver
{
public:
  virtual void AppContextReady(TInt aError) = 0;
};

class CAppContextImpl;

NONSHARABLE_CLASS(CAppContext) : public CBase
{
 public:
  static CAppContext* NewL(ac_AppContext* ac,
			   MAppContextInitObserver& obs);
  ~CAppContext();
 public:
  CAppContextImpl* iImpl;
};

// Creates a platform-specific CAppContext object, and sets it as
// property of the specified ac_AppContext, provided that at least
// initial allocation succeeds. Notification of asynchronous
// initialization completion is provided via MAppContextInitObserver.
void ac_AppContext_PlatInitAsyncL(ac_AppContext* ac,
				  MAppContextInitObserver& obs);

// Returns a reference to a Symbian-specific app context object, via
// which additional services are available.
CAppContext& ac_AppContext_plat(ac_AppContext* ac);
#endif

#endif /* __ac_app_context_private_h__ */

/**

ac_app_context_private.h

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
