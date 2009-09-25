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
  if (!self) {
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
