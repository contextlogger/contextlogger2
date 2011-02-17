#ifndef __rk_remokon_h__
#define __rk_remokon_h__

#include "application_config.h"

#include "ac_app_context.h"

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _rk_Remokon rk_Remokon;
  
  rk_Remokon* rk_Remokon_new(GError** error);

  void rk_Remokon_destroy(rk_Remokon* self);

  // Attempts to establish a connection to the server, and keeps
  // attempting. Does nothing if already started.
  gboolean rk_Remokon_start(rk_Remokon* self,
			    GError** error);
  
  // Breaks any existing connection. Does nothing if already stopped.
  void rk_Remokon_stop(rk_Remokon* self);

  // Runs remokon for the specified amount of time, and after that
  // stops it. The timer is reset whenever there are actual requests
  // coming in.
  gboolean rk_Remokon_start_timed(rk_Remokon* self,
				  int secs,
				  GError** error);

  // Modifies the runtime configuration. Any existing connection is
  // not broken. (Use this for instance when the IAP parameter changes.)
  gboolean rk_Remokon_reconfigure(rk_Remokon* object,
				  const gchar* key,
				  const gchar* value,
				  GError** error);

  // We might implement the following for local control purposes, but
  // they of course will not be useful for remote queries.
  
  gboolean rk_Remokon_is_started(rk_Remokon* self);
  
  gboolean rk_Remokon_is_autostart_enabled(rk_Remokon* self);
  
  // If connected with an established session, ready for messaging.
  gboolean rk_Remokon_is_connected(rk_Remokon* self);

  // Sends a CL2 initiated message. Normally messaging is done
  // internally within Remokon, driven by externally originating
  // messages, but Jabber messages might serve as a useful event
  // notification mechanism, say triggered by a database modification.
  gboolean rk_Remokon_send(rk_Remokon* self,
			   const char* toJid,
			   const char* msgText,
			   GError** error);

  rk_Remokon* ac_get_Remokon(ac_AppContext* self);
#define ac_global_Remokon ac_get_Remokon(ac_get_global_AppContext())

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __rk_remokon_h__ */

/**

rk_remokon.h

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
