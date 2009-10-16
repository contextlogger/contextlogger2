#ifndef __sa_sensor_mark_h__
#define __sa_sensor_mark_h__

#include "application_config.h"

#if __MARK_ENABLED__

#include "log-db.h"

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _sa_Sensor_mark sa_Sensor_mark;

  sa_Sensor_mark* sa_Sensor_mark_new(LogDb* log, GError** error);

  void sa_Sensor_mark_destroy(sa_Sensor_mark* self);

  gboolean sa_Sensor_mark_start(sa_Sensor_mark* self, GError** error);

  void sa_Sensor_mark_stop(sa_Sensor_mark* self);

  gboolean sa_Sensor_mark_is_active(sa_Sensor_mark* self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __MARK_ENABLED__ */

#endif /* __sa_sensor_mark_h__ */
