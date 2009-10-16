#ifndef __sa_sensor_util_epoc_hpp__
#define __sa_sensor_util_epoc_hpp__

#include "utils_cl2.h"

#include <e32base.h>

#include <emanaged.h>

// --------------------------------------------------
// CRetryAo
// --------------------------------------------------

class CRetryAo;

class MRetryAoObserver {
public:
  virtual void RetryTimerExpired(CRetryAo* src, TInt errCode) = 0;
};

NONSHARABLE_CLASS(CRetryAo) :
  public CActive
{
 public:
  CONSTRUCTORS_MAY_LEAVE;

  CRetryAo(MRetryAoObserver& aObserver,
	   TInt aMaxNumRetries,
	   TInt aBaseInterval /* secs */);

  ~CRetryAo();

  void Retry();

  void ResetFailures();

 private: // CActive

  virtual void RunL();
  
  virtual void DoCancel();
  
 protected: // property

  MRetryAoObserver& iObserver;
  TInt iMaxNumRetries;
  TInt iBaseInterval;
  
  LManagedHandle<RTimer> iTimer;

  TInt iNumScanFailures;

 protected: // for possible overriding

  virtual TTimeIntervalMicroSeconds32 WaitInterval();

};

#endif /* __sa_sensor_util_epoc_hpp__ */
