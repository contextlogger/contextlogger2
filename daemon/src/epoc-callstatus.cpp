#include "epoc-callstatus.hpp"

#if __CALLSTATUS_ENABLED__

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

// http://www.forum.nokia.com/infocenter/index.jsp?topic=/S60_5th_Edition_Cpp_Developers_Library/GUID-35228542-8C95-4849-A73F-2B4F082F0C44/sdk/doc_source/guide/Telephony-subsystem-guide/ThirdPartyTelephony/info_calls.html

// -------------------------------------------------------------------
// utilities...

static gboolean TFlightModeV1ToBoolean(const CTelephony::TFlightModeV1& data)
{
  if (data.iFlightModeStatus == CTelephony::EFlightModeOn)
    return ETrue;
  else if (data.iFlightModeStatus == CTelephony::EFlightModeOff)
    return EFalse;
  else
    assert(0 && "unexpected TFlightModeStatus value");
  return EFalse;
}

// -------------------------------------------------------------------
// the sensor object implementation...

CTOR_IMPL_CSensor_callstatus;

void CSensor_callstatus::ConstructL()
{
  iTelephony = CTelephony::NewL();
  iRetryAo = new (ELeave) CRetryAo(*this, 100, 10);
  iFlightModeGetter = new (ELeave) CFlightModeGetter(*iTelephony, *this);
  iFlightModeNotifier = new (ELeave) CFlightModeNotifier(*iTelephony, *this);
  iCallStatusNotifier = new (ELeave) CCallStatusNotifier(*iTelephony, *this);
}

CSensor_callstatus::~CSensor_callstatus()
{
  delete iRetryAo;
  delete iFlightModeGetter;
  delete iFlightModeNotifier;
  delete iCallStatusNotifier;
  delete iTelephony;
}

void CSensor_callstatus::Cancel()
{
  iFlightModeGetter->Cancel();
  iFlightModeNotifier->Cancel();
  iCallStatusNotifier->Cancel();
  iRetryAo->Cancel();
  iState = EInactive;
}

gboolean CSensor_callstatus::StartL(GError** error)
{
  if (!IsActive()) {
    iRetryAo->ResetFailures();
    iFlightModeGetter->MakeRequest();
    iState = EQueryingFlightMode;
    logt("callstatus sensor started");
  }
  return TRUE;
}

void CSensor_callstatus::Stop()
{
  if ((IsActive())) {
    Cancel();
    logt("callstatus sensor stopped");
  }
}

// We expect success here.
void CSensor_callstatus::HandleGotFlightMode(TInt errCode)
{
  if (errCode) {
    log_db_log_status(GetLogDb(), NULL,
		      "ERROR: failure getting flight mode in callstatus sensor: %s (%d)", 
		      plat_error_strerror(errCode), errCode);
    Cancel();
  } else {
    gboolean value = TFlightModeV1ToBoolean(iFlightModeGetter->Data());
    logf("current flight mode setting: %d", value);
    if (value) {
      iState = EInFlightMode;
    } else {
      iCallStatusNotifier->MakeRequest();
      iState = EQueryingCallStatus;
    }
    iFlightModeNotifier->MakeRequest();
  }
}

// We expect success here.
void CSensor_callstatus::HandleFlightModeChange(TInt errCode)
{
  if (errCode) {
    log_db_log_status(GetLogDb(), NULL,
		      "ERROR: flight mode notification error in callstatus sensor: %s (%d)", 
		      plat_error_strerror(errCode), errCode);
    Cancel();
  } else {
    gboolean value = TFlightModeV1ToBoolean(iFlightModeNotifier->Data());

    // Log this as well since we are observing this anyway.
    log_db_log_flightmode(GetLogDb(), value, NULL);

    if (value) {
      Cancel();
      iState = EInFlightMode;
    } else {
      iRetryAo->Cancel(); // to make sure
      iCallStatusNotifier->MakeRequest(); // harmless if active
      iState = EQueryingCallStatus;
    }
    iFlightModeNotifier->MakeRequest();
  }
}

void CSensor_callstatus::HandleCallStatusChange(TInt errCode)
{
  if (errCode) {
    log_db_log_status(GetLogDb(), NULL,
		      "NOTICE: call status notification error in callstatus sensor: %s (%d)", 
		      plat_error_strerror(errCode), errCode);
    iRetryAo->Retry();
    iState = ERetryWaiting;
  } else {
    iRetryAo->ResetFailures();
    {
      GError* localError = NULL;
      // Here iStatus is of the enum type TCallStatus which has at
      // least 10 or so different possible values.
      if (!log_db_log_callstatus(GetLogDb(), iCallStatusNotifier->Data().iStatus,
				&localError)) {
	gx_log_free_fatal_error(localError);
	return;
      }
    }
    iCallStatusNotifier->MakeRequest();
    iState = EQueryingCallStatus;
  }
}

// We expect success here.
void CSensor_callstatus::RetryTimerExpired(CRetryAo* src, TInt errCode)
{
  if (errCode) {
    log_db_log_status(GetLogDb(), NULL,
		      "ERROR: stopping callstatus sensor: timer error: %s (%d)", 
		      plat_error_strerror(errCode), errCode);
    Cancel();
  } else {
    iCallStatusNotifier->MakeRequest();
    iState = EQueryingCallStatus;
  }
}

#endif // __CALLSTATUS_ENABLED__
