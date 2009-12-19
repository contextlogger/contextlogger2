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
    if (!log_db_log_mark(self->log, "mark", &localError)) {
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
    if (!log_db_log_mark(self->log, "start", error)) // begin mark
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
    log_db_log_mark(self->log, "stop", NULL); // end mark
  }
}

EXTERN_C gboolean sa_Sensor_mark_is_active(sa_Sensor_mark* self)
{
  return ut_Timer_is_active(self->timer);
}

#endif /* __MARK_ENABLED__ */

/**

sa_sensor_mark.c

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
