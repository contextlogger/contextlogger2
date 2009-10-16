#include "epoc-indicator.hpp"

#if __INDICATOR_ENABLED__

#include "er_errors.h"
#include "log-db-logging.h"
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
  iTelephony = CTelephony::NewL();
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iTimer, iTimer.CreateLocal()); 
}

CSensor_indicator::~CSensor_indicator()
{
  Cancel();
  SESSION_CLOSE_IF_OPEN(iTimer);
  delete iTelephony;
}

gboolean CSensor_indicator::StartL(GError** error)
{
  iNumScanFailures = 0;
  if (!IsActive()) {
    MakeRequest();
    logt("indicator sensor started");
  }
  return TRUE;
}

void CSensor_indicator::Stop()
{
  if ((IsActive())) {
    Cancel();
    logt("indicator sensor stopped");
  }
}

void CSensor_indicator::MakeRequest()
{
  iTelephony->NotifyChange(iStatus, CTelephony::EIndicatorChange, iIndicatorDes);
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
    logt("retry timer error in indicator");
    ex_log_fatal_error(errCode);
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
      if (!log_db_log_status(logDb, &localError, 
			     "ERROR: failure reading indicator sensor: %s (%d)", 
			     plat_error_strerror(errCode), errCode)) {
	// Logging failed.
	gx_log_free_fatal_error(localError);
	return;
      }

      SetTimer();
    } else {
      logt("stopping indicator scanning due to too many errors");
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
	gx_log_free_fatal_error(localError);
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
	iTelephony->CancelAsync(CTelephony::EIndicatorChangeCancel);
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
