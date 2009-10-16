#include "sa_sensor_mark.h"

#if __MARK_ENABLED__

#include "er_errors.h"
#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"
#include "ut_timer.h"

#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

struct _sa_Sensor_mark {
  ut_Timer* timer;
  LogDb* log; // not owned, no refcounting
};

static void timerCallback(void* userdata, GError* timerError);

static gboolean setTimer(sa_Sensor_mark* self, GError** error)
{
  int wait_secs = 10 * 60;
  return ut_Timer_set_after(self->timer, wait_secs, error);
}

// Takes ownership of any timerError.
static void timerCallback(void* userdata, GError* timerError)
{
  sa_Sensor_mark* self = (sa_Sensor_mark*)userdata;

  if (timerError)
    goto fail;

  // Log "sensor" event.
  {
    logt("MARK");
    GError* localError = NULL;
    if (!log_db_log_mark(self->log, &localError)) {
      gx_log_free_fatal_error(localError);
      return;
    }
  }

  // Schedule next timer event.
  if (!setTimer(self, &timerError)) {
    goto fail;
  }
  return;

 fail:
  {
    gx_db_log_free_fatal_error(self->log, timerError);
  }
}

EXTERN_C sa_Sensor_mark* sa_Sensor_mark_new(LogDb* log, GError** error)
{
  sa_Sensor_mark* self = g_try_new0(sa_Sensor_mark, 1);
  if (!self) {
    if (error) *error = NULL;
    return NULL;
  }
  self->log = log;
  self->timer = ut_Timer_new(self, &timerCallback, error);
  if (!self->timer) {
    sa_Sensor_mark_destroy(self);
    return NULL;
  }
  return self;
}

EXTERN_C void sa_Sensor_mark_destroy(sa_Sensor_mark* self)
{
  if (self) {
    sa_Sensor_mark_stop(self);
    ut_Timer_destroy(self->timer);
    g_free(self);
  }
}

EXTERN_C gboolean sa_Sensor_mark_start(sa_Sensor_mark* self, GError** error)
{
  if (!sa_Sensor_mark_is_active(self)) {
    logt("START MARK");
    if (!log_db_log_mark(self->log, error)) // begin mark
      return FALSE;

    if (!setTimer(self, error))
      return FALSE;
  }
  return TRUE;
}

EXTERN_C void sa_Sensor_mark_stop(sa_Sensor_mark* self)
{
  if (sa_Sensor_mark_is_active(self)) {
    ut_Timer_cancel(self->timer);
    logt("END MARK");
    log_db_log_mark(self->log, NULL); // end mark
  }
}

EXTERN_C gboolean sa_Sensor_mark_is_active(sa_Sensor_mark* self)
{
  return ut_Timer_is_active(self->timer);
}

#endif /* __MARK_ENABLED__ */
