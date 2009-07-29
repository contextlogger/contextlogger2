// This implementation is presently only intended for testing. It is a
// simple timer sensor for producing "dummy" sensor events at random
// time intervals. Event delivery is specific to our own custom event
// loop. Alternative implementations for other event loops ought to be
// simple enough to device.

#include "sa_sensor_timer_posix.h"

#if __TIMER_ENABLED__

#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"

#include "common/error_list.h"
#include "common/evq_event.h"
#include "common/evq_timer.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

struct _sa_Sensor_timer {
  LogDb* log; // not owned, no refcounting
  EventQueue* eventQueue; // not owned
  Timer timer;
  struct timespec ts;
  gboolean isActive;
};

static gboolean timerCallback(int errCode, void* user_data, GError** error);

static void logTimerError(sa_Sensor_timer* self, int errCode)
{
  const char* fmt = "ERROR: timer error: %s (%d)";
  const char* platErr = plat_error_strerror(errCode);
  logf(fmt, platErr, errCode);
  GError* error;
  if (!log_db_log_status(self->log, &error, fmt, platErr, errCode)) {
    logt(error->message);
    g_error_free(error);
  }
}

static void setTimer(sa_Sensor_timer* self)
{
  struct timeval now;
  if (gettimeofday(&now, NULL)) {
    logTimerError(self, errno);
    return;
  }
  int wait_secs = (rand() % 10);
  logf("waiting for %d secs", wait_secs);
  self->ts.tv_sec = now.tv_sec + wait_secs;
  self->ts.tv_nsec = now.tv_usec * 1000;
  timer_at(&self->timer, &self->ts, &timerCallback, self);
  self->isActive = TRUE;
}

static gboolean timerCallback(int errCode, void* user_data, GError** error)
{
  sa_Sensor_timer* self = (sa_Sensor_timer*)user_data;
  self->isActive = FALSE;
  if (errCode) {
    // Log error.
    logTimerError(self, errCode);
  } else {
    // Log "sensor" event.
    if (!log_db_log_timer(self->log, error)) {
      // If a context logger fails to log, that is a pretty severe error.
      return FALSE;
    } else {
      // Schedule next timer event.
      setTimer(self);
    }
  }
  return TRUE;
}

EXTERN_C sa_Sensor_timer* sa_Sensor_timer_new(EventQueue* evQueue, LogDb* log, GError** error)
{
  sa_Sensor_timer* self = g_try_new0(sa_Sensor_timer, 1);
  if (!self) {
    if (error) *error = NULL;
    return NULL;
  }
  self->log = log;
  self->eventQueue = evQueue;
  timer_init(self->eventQueue, &self->timer);
  return self;
}

EXTERN_C void sa_Sensor_timer_destroy(sa_Sensor_timer* self)
{
  timer_close(&self->timer);
  g_free(self);
}

EXTERN_C gboolean sa_Sensor_timer_start(sa_Sensor_timer* self, GError** error)
{
  setTimer(self);
  return TRUE;
}

EXTERN_C void sa_Sensor_timer_stop(sa_Sensor_timer* self)
{
  timer_cancel(&self->timer);
  self->isActive = FALSE;
}

EXTERN_C gboolean sa_Sensor_timer_is_active(sa_Sensor_timer* self)
{
  return self->isActive;
}

#endif /* __TIMER_ENABLED__ */
