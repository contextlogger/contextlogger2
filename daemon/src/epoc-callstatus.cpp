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

// http://library.forum.nokia.com/topic/S60_3rd_Edition_Cpp_Developers_Library/GUID-35228542-8C95-4849-A73F-2B4F082F0C44/html/SDL_93/doc_source/guide/Telephony-subsystem-guide/ThirdPartyTelephony/info_calls.html

// xxx We might also want to log the remote party phone number when
// the remote party information changes. This would involve a separate
// active object for requesting notification of remote party info
// change (with a separate ``NotifyChange`` request).

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

// ``GetCallInfo`` should be used after a notification of an incoming
// call (``EStatusRinging``?), notification of line status change to
// dialling (``EStatusDialling``), or notification of remote party
// info change (with a separate ``NotifyChange`` request).

static TInt GetRemotePartyInfo(CTelephony& aTelephony,
			       CTelephony::TCallInfoV1& callInfo,
			       CTelephony::TRemotePartyInfoV1& remoteInfo)
{
  CTelephony::TCallSelectionV1 callSelect;
  CTelephony::TCallSelectionV1Pckg callSelectPckg(callSelect);
  callSelect.iLine = CTelephony::EVoiceLine;
  // Call during setup/disconnecting process, not active.
  callSelect.iSelect = CTelephony::EInProgressCall;

  CTelephony::TCallInfoV1Pckg callInfoPckg(callInfo);
  CTelephony::TRemotePartyInfoV1Pckg remoteInfoPckg(remoteInfo);

  // "If a call with the appropriate status is not available, then
  // KErrNotFound is returned."
  TInt errCode = aTelephony.GetCallInfo(callSelectPckg, callInfoPckg, remoteInfoPckg);
  return errCode;
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

    // Here iStatus is of the enum type TCallStatus which has at
    // least 10 or so different possible values.
    CTelephony::TCallStatus callStatus = iCallStatusNotifier->Data().iStatus;

    TBuf8<CTelephony::KMaxTelNumberSize + 1> numberBuf;
    const char* number = NULL;

    // Get remote party phone number if possible.
    {
      switch (callStatus)
        {
        case CTelephony::EStatusRinging: // use TRemotePartyInfoV1
        case CTelephony::EStatusDialling: // use TCallInfoV1
          {
	    CTelephony::TCallInfoV1 callInfo;
	    CTelephony::TRemotePartyInfoV1 remoteInfo;
	    errCode = GetRemotePartyInfo(*iTelephony, callInfo, remoteInfo);
	    if (errCode) {
	      logf("failed to get remote party info: %s (%d)", 
		   plat_error_strerror(errCode), errCode);
	    } else {
	      if (callStatus == CTelephony::EStatusRinging) {
		switch (remoteInfo.iRemoteIdStatus)
		  {
		  case CTelephony::ERemoteIdentityUnknown:
		    {
		      number = "(unknown)";
		      break;
		    }
		  case CTelephony::ERemoteIdentitySuppressed:
		    {
		      number = "(suppressed)";
		      break;
		    }
		  case CTelephony::ERemoteIdentityAvailable:
		    {
		      TDesC& numberW = remoteInfo.iRemoteNumber.iTelNumber; // TBuf<KMaxTelNumberSize>
		      numberBuf.Copy(numberW);
		      number = (const char*)(numberBuf.PtrZ());
		      break;
		    }
		  default:
		    {
		      logt("unknown remoteInfo.iRemoteIdStatus");
		      break;
		    }
		  }
	      } else { // callStatus == CTelephony::EStatusDialling
		TDesC& numberW = callInfo.iDialledParty.iTelNumber; // TBuf<KMaxTelNumberSize>
		numberBuf.Copy(numberW);
		number = (const char*)(numberBuf.PtrZ());
	      }
	    }
            break;
          }

	default:
	  {
	    // Number may not be available in this state.
	  }
        }
    }
      
    // Log.
    {
      //if (number) logf("remote party number is '%s'", number);

      GError* localError = NULL;
      if (!log_db_log_callstatus(GetLogDb(), callStatus, number, &localError)) {
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
