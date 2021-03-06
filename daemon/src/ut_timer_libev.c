#include "ut_timer.h"

#include "er_errors.h"

#include <ev.h>

#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

struct _ut_Timer {
  ev_timer timer; // must be the first member
  void* userdata; 
  ut_TimerCallback* cb;
};

static void evCallback(EV_P_ ev_timer* w, int revents);

static void setTimer(ut_Timer* self, int wait_secs)
{
  ev_tstamp after = wait_secs; // coerce to float type of some kind
  // Note that ev_TYPE_set may not be called when active.
  ev_timer_set(&self->timer, after, 0); // no repeat
  ev_timer_start(EV_DEFAULT, &self->timer);
}

static void evCallback(EV_P_ ev_timer* w, int revents)
{
  ut_Timer* self = (ut_Timer*)w;
  (*(self->cb))(self->userdata, NULL);
}

ut_Timer* ut_Timer_new(void* userdata, ut_TimerCallback* cb, GError** error)
{
  ut_Timer* self = g_try_new0(ut_Timer, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }
  self->userdata = userdata;
  self->cb = cb;
  ev_init(&self->timer, evCallback);
  return self;
}
  
void ut_Timer_destroy(ut_Timer* self)
{
  if (self) {
    ut_Timer_cancel(self);
    g_free(self);
  }
}

gboolean ut_Timer_set_after(ut_Timer* self, int secs, GError** error)
{
  if (!ut_Timer_is_active(self)) {
    setTimer(self, secs);
  }
  return TRUE;
}

void ut_Timer_cancel(ut_Timer* self)
{
  // Harmless if inactive.
  // Frees any timer resources as well.
  ev_timer_stop(EV_DEFAULT, &self->timer);
}

gboolean ut_Timer_is_active(ut_Timer* self)
{
  return (ev_is_active(&self->timer));
}

/**

ut_timer_libev.c

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
