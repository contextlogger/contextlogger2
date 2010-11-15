#include "ut_timer.h"

#include "er_errors.h"

#include "common/epoc-time.h"

#include <e32base.h>

class CUtTimer;

struct _ut_Timer {
  CUtTimer* timer;
  void* userdata; 
  ut_TimerCallback* cb;
};

NONSHARABLE_CLASS(CUtTimer) :
  public CTimer
{
public:

  static CUtTimer *NewLC(ut_Timer &aInterface, TInt aPriority);

  static CUtTimer *NewL(ut_Timer &aInterface, TInt aPriority);

private:

  CUtTimer(ut_Timer &aInterface, TInt aPriority);

  virtual void RunL();

  ut_Timer &iInterface;
};

CUtTimer *CUtTimer::NewLC(ut_Timer &aInterface, TInt aPriority)
{
  CUtTimer *object = new (ELeave) CUtTimer(aInterface, aPriority);
  CleanupStack::PushL(object);
  object->ConstructL();
  return object;
}

CUtTimer *CUtTimer::NewL(ut_Timer &aInterface, TInt aPriority)
{
  CUtTimer *object = NewLC(aInterface, aPriority);
  CleanupStack::Pop();
  return object;
}

CUtTimer::CUtTimer(ut_Timer &aInterface, TInt aPriority) : 
  CTimer(aPriority), iInterface(aInterface)
{
  CActiveScheduler::Add(this);
}

#define make_error(_errCode, _msg)					\
  gx_error_new(domain_symbian, _errCode, _msg ": %s (%d)",		\
	       plat_error_strerror(_errCode), _errCode);

void CUtTimer::RunL()
{
  GError* error = NULL;
  TInt errCode = iStatus.Int();
  if (errCode) {
    error = make_error(errCode, "timer error");
  }
  (*(iInterface.cb))(iInterface.userdata, error);
}

extern "C" ut_Timer* ut_Timer_new(void* userdata, ut_TimerCallback* cb, GError** error)
{
  ut_Timer* self = g_try_new0(ut_Timer, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }
  self->userdata = userdata;
  self->cb = cb;

  TRAPD(errCode, self->timer = CUtTimer::NewL(*self, CActive::EPriorityStandard));
  if (G_UNLIKELY(errCode)) {
    g_free(self);
    if (error) *error = make_error(errCode, "failed to create native timer instance");
    return NULL;
  }

  return self;
}
  
extern "C" void ut_Timer_destroy(ut_Timer* self)
{
  if (self) {
    delete self->timer;
    g_free(self);
  }
}

extern "C" gboolean ut_Timer_set_after(ut_Timer* self, int secs, GError** error)
{
  ut_Timer_cancel(self);

  TTimeIntervalMicroSeconds32 interval = SecsToUsecs(secs);
  //logg("interval timer set to %d secs / %d usecs", secs, interval.Int());

  // Note that these timers should not complete with KErrAbort, since
  // a wait for an interval should not be affected by a system time
  // change.
  self->timer->After(interval);
  
  return TRUE;
}

extern "C" void ut_Timer_cancel(ut_Timer* self)
{
  self->timer->Cancel();
}

extern "C" gboolean ut_Timer_is_active(ut_Timer* self)
{
  return self->timer->IsActive();
}

/**

ut_timer_epoc.cpp

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
