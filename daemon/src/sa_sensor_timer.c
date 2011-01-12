// This implementation is presently only intended for testing. It is a
// simple timer sensor for producing "dummy" sensor events at random
// time intervals. It is implemented based on the internal,
// platform-independent ut_Timer API.

#include "sa_sensor_timer.h"

#if __TIMER_ENABLED__

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"
#include "ut_timer.h"

#include "common/utilities.h"

#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

struct _sa_Sensor_timer {
  ut_Timer* timer;
  LogDb* log; // not owned, no refcounting
};

static void timerCallback(void* userdata, GError* timerError);

static void setTimer(sa_Sensor_timer* self)
{
  int wait_secs = ((rand() % 30) + 1);
  logg("waiting for %d secs", wait_secs);
  
  GError* timerError = NULL;
  if (!ut_Timer_set_after(self->timer, wait_secs, &timerError)) {
    er_log_gerror(er_FATAL|er_FREE, timerError, 
		  "error setting timer in timer sensor");
    return;
  }
}

// A non-NULL timerError indicates an error.
// Caller takes ownership of any timerError.
static void timerCallback(void* userdata, GError* timerError)
{
  sa_Sensor_timer* self = (sa_Sensor_timer*)userdata;

  if (timerError) {
    er_log_gerror(er_FATAL|er_FREE, timerError, 
		  "timer error event in timer sensor");
    return;
  }

  // Log "sensor" event.
  GError* localError = NULL;
  if (!log_db_log_timer(self->log, &localError)) {
    er_log_gerror(er_FATAL|er_FREE, localError, 
		  "logging error in timer sensor");
    return;
  }

  guilogf("timer: event");

  // Schedule next timer event.
  setTimer(self);
}

EXTERN_C sa_Sensor_timer* sa_Sensor_timer_new(LogDb* log, GError** error)
{
  sa_Sensor_timer* self = g_try_new0(sa_Sensor_timer, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }
  self->log = log;
  self->timer = ut_Timer_new(self, timerCallback /*xxx*/, error);
  if (G_UNLIKELY(!(self->timer))) {
    g_free(self);
    return NULL;
  }
  return self;
}

EXTERN_C void sa_Sensor_timer_destroy(sa_Sensor_timer* self)
{
  if (self) {
    ut_Timer_destroy(self->timer);
    g_free(self);
  }
}

EXTERN_C gboolean sa_Sensor_timer_start(sa_Sensor_timer* self, GError** error)
{
  (void)error;
  if (!sa_Sensor_timer_is_active(self)) {
    setTimer(self);
  }
  return TRUE;
}

EXTERN_C void sa_Sensor_timer_stop(sa_Sensor_timer* self)
{
  ut_Timer_cancel(self->timer);
}

EXTERN_C gboolean sa_Sensor_timer_is_active(sa_Sensor_timer* self)
{
  return ut_Timer_is_active(self->timer);
}

#endif /* __TIMER_ENABLED__ */

/**

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
