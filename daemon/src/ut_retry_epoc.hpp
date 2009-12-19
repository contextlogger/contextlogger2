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

/**

ut_retry_epoc.hpp

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
