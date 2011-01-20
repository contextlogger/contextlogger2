#include "epoc-gps-positioner.hpp"

_LIT_W(KRequestor, COMPONENT_NAME_W);

CTOR_IMPL_CPositioner_gps;

void CPositioner_gps::ConstructL()
{
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iPositioner, iPositioner.Open(iPositionServer, iModuleId));
  User::LeaveIfError(iPositioner.SetRequestor(CRequestor::ERequestorService,
					      CRequestor::EFormatApplication, 
					      KRequestor));
  
  iUpdateOptions.SetAcceptPartialUpdates(EFalse);

  TInt64 usecs = iUpdateIntervalSecs * 1000000LL;
  iUpdateOptions.SetUpdateInterval(TTimeIntervalMicroSeconds(usecs));

  usecs = iUpdateTimeoutSecs * 1000000LL;
  iUpdateOptions.SetUpdateTimeOut(TTimeIntervalMicroSeconds(usecs));

  /*
    If you set this to a non-zero value, expect error completions for
    positioning requests. Without setting this presumably there is no
    timeout. Set to zero for no timeout.
  */
  User::LeaveIfError(iPositioner.SetUpdateOptions(iUpdateOptions));

  dblogg("gps scan interval set to %d secs", iUpdateIntervalSecs);
  guilogf("gps: interval %d secs", iUpdateIntervalSecs);
}

CPositioner_gps::~CPositioner_gps()
{
  Cancel(); // safe when AO inactive as DoCancel not called
  SESSION_CLOSE_IF_OPEN(iPositioner);
}

void CPositioner_gps::MakeRequest() 
{
  iPositioner.NotifyPositionUpdate(iPositionInfo, iStatus);
  SetActive();
}
  
gboolean CPositioner_gps::RunGL(GError** error) 
{
  return iObserver.PositionerEventL(error);
}
  
const char* CPositioner_gps::Description() 
{
  return "gps_positioner";
}
  
void CPositioner_gps::DoCancel() 
{
  assert(IS_SESSION_OPEN(iPositioner));
  // Ignoring return value. The only possible error should never
  // happen in this case.
  iPositioner.CancelRequest(EPositionerNotifyPositionUpdate);
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
