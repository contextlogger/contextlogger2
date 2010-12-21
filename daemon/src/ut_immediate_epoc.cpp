#include "ut_immediate_epoc.hpp"
#include "ut_immediate.h"

#include "er_errors.h"

// --------------------------------------------------
// active object
// --------------------------------------------------

TInt MImmediateObserver::HandleImmediateError(TInt errCode)
{
  assert(0 && "error in CImmediateAo::RunL()");
  return errCode;
}

CImmediateAo *CImmediateAo::NewL(MImmediateObserver& aClient)
{
  CImmediateAo *object = new (ELeave) CImmediateAo(aClient);
  /*
  CleanupStack::PushL(object);
  object->ConstructL();
  CleanupStack::Pop();
  */
  return object;
}

CImmediateAo::~CImmediateAo()
{
  Cancel();
}

CImmediateAo::CImmediateAo(MImmediateObserver& aObserver) : 
  CActive(EPriorityStandard), iObserver(aObserver)
{
  CActiveScheduler::Add(this);
}

void CImmediateAo::Complete()
{
  Cancel();

  iStatus = KRequestPending;
  SetActive();

  TRequestStatus* status = &iStatus;
  User::RequestComplete(status, KErrNone);
}

void CImmediateAo::DoCancel()
{
  if (iStatus == KRequestPending)
    {
      assert(0 && "CImmediateAo::DoCancel() unexpectedly called");
    }
}

TInt CImmediateAo::RunError(TInt aError)
{
  return iObserver.HandleImmediateError(aError);
}

void CImmediateAo::RunL()
{
  TInt errCode = iStatus.Int();
  if (G_UNLIKELY(errCode))
    iObserver.HandleImmediateError(errCode);
  else
    iObserver.HandleImmediateEvent();
}

// --------------------------------------------------
// opaque object
// --------------------------------------------------

#define make_error(_errCode, _msg)					\
  gx_error_new(domain_symbian, _errCode, _msg ": %s (%d)",		\
	       plat_error_strerror(_errCode), _errCode)

struct _ut_Immediate : public MImmediateObserver 
{
  CImmediateAo* ao;
  void* userdata; 
  ut_ImmediateCallback* cb;

  virtual void HandleImmediateEvent() 
  {
    (*cb)(userdata, NULL);
  }

  virtual TInt HandleImmediateError(TInt errCode)
  {
    (*cb)(userdata, make_error(errCode, "immediate error"));
    return 0;
  }
};

// --------------------------------------------------
// API
// --------------------------------------------------

extern "C" ut_Immediate* ut_Immediate_new(void* userdata, ut_ImmediateCallback* cb, 
					  GError** error)
{
  ut_Immediate* self = g_try_new0(ut_Immediate, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }
  self->userdata = userdata;
  self->cb = cb;

  TRAPD(errCode, self->ao = CImmediateAo::NewL(*self));
  if (G_UNLIKELY(errCode)) {
    g_free(self);
    if (error) 
      *error = make_error(errCode, "failed to create native immediate instance");
    return NULL;
  }

  return self;
}
  
extern "C" void ut_Immediate_destroy(ut_Immediate* self)
{
  if (self) {
    delete self->ao;
    g_free(self);
  }
}

extern "C" gboolean ut_Immediate_complete(ut_Immediate* self, GError** error)
{
  (void)error;
  ut_Immediate_cancel(self);
  self->ao->Complete();
  return TRUE;
}

extern "C" void ut_Immediate_cancel(ut_Immediate* self)
{
  self->ao->Cancel();
}

extern "C" gboolean ut_Immediate_is_active(ut_Immediate* self)
{
  return self->ao->IsActive();
}

/**

ut_immediate_epoc.cpp

Copyright 2009-2010 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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
