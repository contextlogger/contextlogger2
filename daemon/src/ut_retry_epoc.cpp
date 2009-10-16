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
