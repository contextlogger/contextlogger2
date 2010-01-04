#include "ut_retry_epoc.hpp"

#include "er_errors.h"

#include "common/epoc-time.h"

#include <emisc.h> // OR_LEAVE

#include <stdlib.h> // rand

// --------------------------------------------------
// CRetryAo
// --------------------------------------------------

CRetryAo::CRetryAo(MRetryAoObserver& aObserver,
		   TInt aMaxNumRetries,
		   TInt aBaseInterval /* secs */) :
  CActive(EPriorityLow), 
  iObserver(aObserver),
  iMaxNumRetries(aMaxNumRetries),
  iBaseInterval(aBaseInterval)
  
{
  iTimer->CreateLocal() OR_LEAVE;
  CActiveScheduler::Add(this);
}

CRetryAo::~CRetryAo()
{
  Cancel();
}

void CRetryAo::Retry()
{
  Cancel();
  iNumScanFailures++;
  TTimeIntervalMicroSeconds32 interval = WaitInterval();
  iTimer->After(iStatus, interval);
  SetActive();
}

void CRetryAo::RunL()
{
  iObserver.RetryTimerExpired(this, iStatus.Int());
}

void CRetryAo::DoCancel()
{
  iTimer->Cancel();
}

TTimeIntervalMicroSeconds32 CRetryAo::WaitInterval()
{
  int secs = (iBaseInterval * iNumScanFailures) + (rand() % 10);
  TTimeIntervalMicroSeconds32 interval = SecsToUsecs(secs);
  return interval;
}

void CRetryAo::ResetFailures()
{ 
  iNumScanFailures = 0; 
}

/**

ut_retry_epoc.cpp

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
