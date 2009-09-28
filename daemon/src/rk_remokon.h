#ifndef __rk_remokon_h__
#define __rk_remokon_h__

#include "application_config.h"

#if __FEATURE_REMOKON__

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

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __FEATURE_REMOKON__ */

#endif /* __rk_remokon_h__ */
