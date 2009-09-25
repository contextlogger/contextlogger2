#ifndef __sa_array_h__
#define __sa_array_h__

#include "log-db.h"

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _sa_Array sa_Array;

  /** Instantiates a sensor array consisting of all supported sensors.
      Does not yet start the sensors. */
  sa_Array* 	sa_Array_new	(LogDb* log,
				 GError** error);
  
  /** Starts all supported sensors. */
  void sa_Array_start(sa_Array* self);
  
  /** Stops all supported sensors. */
  void sa_Array_stop(sa_Array* self);
  
  /** Destroys a sensor array. Naturally all the sensors in it are stopped. */
  void 	sa_Array_destroy	(sa_Array* self);
  
  /** Returns FALSE for unknown names. */
  gboolean sa_sensor_is_supported(const gchar* name);

  gboolean sa_Array_sensor_is_running(sa_Array* self, const gchar* name);

  void sa_Array_sensor_stop(sa_Array* self, const gchar* name);

  gboolean sa_Array_sensor_start(sa_Array* self, const gchar* name, GError** error);

  gboolean sa_Array_reconfigure(sa_Array* self, const gchar* key, const gchar* value, GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __sa_array_h__ */
