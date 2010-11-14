#include "ut_timer_qt_private.hpp"

#include "er_errors.h"

#include "common/platform_config.h"

struct _ut_Timer {
  MyTimer* timer;
};

void MyTimer::handleTimeout()
{
  (*cb)(userdata, NULL);
}

extern "C"
ut_Timer* ut_Timer_new(void* userdata, ut_TimerCallback* cb, GError** error)
{
  ut_Timer* self = g_try_new0(ut_Timer, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }
  self->timer = new_nothrow MyTimer(userdata, cb);
  if (G_UNLIKELY(!(self->timer))) {
    g_free(self);
    if (error) *error = gx_error_no_memory;
    return NULL;
  }
  return self;
}
  
extern "C"
void ut_Timer_destroy(ut_Timer* self)
{
  if (self) {
    delete self->timer;
    g_free(self);
  }
}

extern "C"
gboolean ut_Timer_set_after(ut_Timer* self, int secs, GError** error)
{
  (void)error;
  if (!ut_Timer_is_active(self)) {
    self->timer->start(1000 * secs);
  }
  return TRUE;
}

extern "C"
void ut_Timer_cancel(ut_Timer* self)
{
  self->timer->stop();
}

extern "C"
gboolean ut_Timer_is_active(ut_Timer* self)
{
  return self->timer->isActive();
}

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
