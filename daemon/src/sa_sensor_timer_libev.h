#ifndef __sa_sensor_timer_libev_h__
#define __sa_sensor_timer_libev_h__

#include "application_config.h"

#if __TIMER_ENABLED__

#include "log-db.h"

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _sa_Sensor_timer sa_Sensor_timer;

  sa_Sensor_timer* sa_Sensor_timer_new(LogDb* log, GError** error);

  void sa_Sensor_timer_destroy(sa_Sensor_timer* self);

  gboolean sa_Sensor_timer_start(sa_Sensor_timer* self, GError** error);

  void sa_Sensor_timer_stop(sa_Sensor_timer* self);

  gboolean sa_Sensor_timer_is_active(sa_Sensor_timer* self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __TIMER_ENABLED__ */

#endif /* __sa_sensor_timer_libev_h__ */
