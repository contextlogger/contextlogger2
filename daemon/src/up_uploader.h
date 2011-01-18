#ifndef __up_uploader_h__
#define __up_uploader_h__

#include "ac_app_context.h"
#include "application_config.h"

#include "common/error_list.h"

#include <glib.h>

#define up_DOMAIN domain_cl2app

#define up_ERR_GENERAL 1

#ifdef __cplusplus
extern "C" {
#endif

  // Does global initialization.
  //
  // Additionally, you will probably want to invoke signal(SIGPIPE,
  // SIG_IGN) as part of the global initialization.
  gboolean up_global_init(GError** error);

  // Does global cleanup.
  //
  void up_global_cleanup();

  typedef struct _up_Uploader up_Uploader;

  up_Uploader* up_Uploader_new(ac_AppContext* aAppContext, GError** error);

  void up_Uploader_destroy(up_Uploader* object);

  // Overrides any timeout to cause a new LogDb snapshot to be taken
  // immediately.
  gboolean up_Uploader_upload_now(up_Uploader* object,
				  GError** error);

  // Modifies the up_Uploader runtime configuration. Any upload that is
  // in progress is not interrupted.
  gboolean up_Uploader_reconfigure(up_Uploader* object,
				   const gchar* key,
				   const gchar* value,
				   GError** error);

  up_Uploader* ac_get_Uploader(ac_AppContext* self);
#define ac_global_Uploader ac_get_Uploader(ac_get_global_AppContext())

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __up_uploader_h__ */

/**

up_uploader.h

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
