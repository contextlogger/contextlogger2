#include "epoc-indicator.hpp"

#if __INDICATOR_ENABLED__

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#include "common/assertions.h"
#include "common/epoc-time.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <stdlib.h> // rand

// -------------------------------------------------------------------
// the sensor object implementation...

CTOR_IMPL_CSensor_indicator;

void CSensor_indicator::ConstructL()
{
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iTimer, iTimer.CreateLocal()); 
}

CSensor_indicator::~CSensor_indicator()
{
  Cancel();
  SESSION_CLOSE_IF_OPEN(iTimer);
}

#define iLogDb ac_LogDb(iAppContext)
#define iTelephony ac_Telephony(iAppContext)

gboolean CSensor_indicator::StartL(GError** error)
{
  iNumScanFailures = 0;
  if (!IsActive()) {
    MakeRequest();
    log_db_log_status(iLogDb, NULL, "indicator sensor started");
  }
  return TRUE;
}

void CSensor_indicator::Stop()
{
  if ((IsActive())) {
    Cancel();
    log_db_log_status(iLogDb, NULL, "indicator sensor stopped");
  }
}

void CSensor_indicator::MakeRequest()
{
  iTelephony.NotifyChange(iStatus, CTelephony::EIndicatorChange, iIndicatorDes);
  SetActive();
  iState = EQuerying;
  //logt("indicator sensor observing indicator");
}

void CSensor_indicator::SetTimer() 
{
  int secs = 5 * (1 + iNumScanFailures) + (rand() % 10);
  TTimeIntervalMicroSeconds32 interval = SecsToUsecs(secs);
  logf("indicator timer set to %d secs / %d usecs", secs, interval.Int());
  iTimer.After(iStatus, interval);
  SetActive();
  iState = ERetryWaiting;
}

// Retry timer expired.
void CSensor_indicator::HandleTimer()
{
  int errCode = iStatus.Int();
  if (errCode) {
    // unexpected with an interval timer
    er_log_symbian(er_FATAL, errCode, "retry timer error in indicator");
    return;
  }
  MakeRequest();
}

static char ind2ch(CTelephony::TPhoneIndicators t,
		   CTelephony::TIndicatorV1 d)
{
  if ((d.iCapabilities & t) == 0)
    return 'N';
  if ((d.iIndicator & t) == 0)
    return '0';
  return '1';
}

#define IND2CH(t) (ind2ch(t, iIndicator))

#define INDICATOR_EQ(x,y) \
  (((x).iIndicator == (y).iIndicator) && \
   ((x).iCapabilities == (y).iCapabilities))

void CSensor_indicator::HandleRead()
{
  int errCode = iStatus.Int();

  LogDb* logDb = ac_LogDb(iAppContext);
  GError* localError = NULL;

  if (errCode) {
    iNumScanFailures++;
    logf("%dth consecutive failure in indicator", iNumScanFailures);

    if (iNumScanFailures < 100) {
      logf("ERROR: %dth consecutive failure reading indicator sensor: %s (%d)", 
	   iNumScanFailures, plat_error_strerror(errCode), errCode);

      SetTimer();
    } else {
      log_db_log_status(logDb, NULL, "INACTIVATE: indicator: stopping indicator scanning due to too many consecutive errors");
    }

    return;
  }

  {
    iNumScanFailures = 0;

    if (!(INDICATOR_EQ(iIndicator, iOldIndicator))) {
      logf("new indicator value: %u/%u", 
	   iIndicator.iIndicator,
	   iIndicator.iCapabilities);
      logf("indicators: charger=%c network=%c call=%c", 
	   IND2CH(CTelephony::KIndChargerConnected),
	   IND2CH(CTelephony::KIndNetworkAvailable),
	   IND2CH(CTelephony::KIndCallInProgress));

      if (!log_db_log_indicator(logDb, iIndicator.iIndicator, 
				iIndicator.iCapabilities,
				&localError)) {
	gx_txtlog_fatal_error_free(localError);
	return;
      }
      
      iOldIndicator = iIndicator;
    }

    MakeRequest();
  }
}

void CSensor_indicator::RunL()
{
  //logt("CSensor_indicator::RunL()");

  TState oldState = iState;
  iState = EInactive;

  switch (oldState)
    {
    case EQuerying:
      {
	HandleRead();
	break;
      }
    case ERetryWaiting:
      {
	HandleTimer();
        break;
      }
    default:
      {
        assert(0 && "unexpected state");
        break;
      }
    }
}

void CSensor_indicator::DoCancel()
{
  switch (iState)
    {
    case EQuerying:
      {
	iTelephony.CancelAsync(CTelephony::EIndicatorChangeCancel);
        break;
      }
    case ERetryWaiting:
      {
	iTimer.Cancel();
        break;
      }
    default:
      {
        assert(0 && "unexpected state");
        break;
      }
    }

  // Note that the state must never become anything else without
  // invoking SetActive at the same time.
  iState = EInactive;
}

#endif // __INDICATOR_ENABLED__

/**

epoc-indicator.cpp

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
