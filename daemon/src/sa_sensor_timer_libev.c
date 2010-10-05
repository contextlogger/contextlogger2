// This implementation is presently only intended for testing. It is a
// simple timer sensor for producing "dummy" sensor events at random
// time intervals. Event delivery is specific to the libev event loop.
// Alternative implementations for other event loops ought to be
// simple enough to devise.

#include "sa_sensor_timer_libev.h"

#if __TIMER_ENABLED__

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"

#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <ev.h>

#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

struct _sa_Sensor_timer {
  ev_timer timer; // must be the first member
  LogDb* log; // not owned, no refcounting
};

static void timerCallback(EV_P_ ev_timer* w, int revents);

static void setTimer(sa_Sensor_timer* self)
{
  int wait_secs = ((rand() % 30) + 1);
  logf("waiting for %d secs", wait_secs);
  
  ev_tstamp after = wait_secs; // coerce to float type of some kind
  // Note that ev_TYPE_set may not be called when active.
  ev_timer_set(&self->timer, after, 0); // no repeat
  ev_timer_start(EV_DEFAULT, &self->timer);
}

static void timerCallback(EV_P_ ev_timer* w, int revents)
{
  sa_Sensor_timer* self = (sa_Sensor_timer*)w;

  // Log "sensor" event.
  GError* localError = NULL;
  if (!log_db_log_timer(self->log, &localError)) {
    gx_txtlog_error_free(localError);
    EXIT_APPLICATION;
    return;
  }

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
  ev_init(&self->timer, timerCallback);
  return self;
}

EXTERN_C void sa_Sensor_timer_destroy(sa_Sensor_timer* self)
{
  if (self) {
    sa_Sensor_timer_stop(self);
    g_free(self);
  }
}

EXTERN_C gboolean sa_Sensor_timer_start(sa_Sensor_timer* self, GError** error)
{
  if (!sa_Sensor_timer_is_active(self)) {
    setTimer(self);
  }
  return TRUE;
}

EXTERN_C void sa_Sensor_timer_stop(sa_Sensor_timer* self)
{
  // Harmless if inactive.
  // Frees any timer resources as well.
  ev_timer_stop(EV_DEFAULT, &self->timer);
}

EXTERN_C gboolean sa_Sensor_timer_is_active(sa_Sensor_timer* self)
{
  return (ev_is_active(&self->timer));
}

#endif /* __TIMER_ENABLED__ */

/**

sa_sensor_timer_libev.c

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
