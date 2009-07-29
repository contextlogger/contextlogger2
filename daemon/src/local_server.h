#ifndef __local_server_h__
#define __local_server_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  // This actually allows us to do data hiding in C.
typedef struct _LocalServer LocalServer;

LocalServer * 	LocalServer_new	(GError ** error);

  // Does nothing if already started.
gboolean 	LocalServer_start	(LocalServer * self,
					GError ** error);

  // Does nothing if already stopped.
void 	LocalServer_stop	(LocalServer * self);

void	LocalServer_destroy	(LocalServer * self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __local_server_h__ */
