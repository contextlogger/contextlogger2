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

#include <glib/gprintf.h>

// http://library.forum.nokia.com/topic/S60_3rd_Edition_Cpp_Developers_Library/GUID-35228542-8C95-4849-A73F-2B4F082F0C44/html/SDL_93/doc_source/guide/Telephony-subsystem-guide/ThirdPartyTelephony/info_calls.html

// xxx We might also want to log the remote party phone number when
// the remote party information changes. This would involve a separate
// active object for requesting notification of remote party info
// change (with a separate ``NotifyChange`` request).

// TCallInfoV1::iExitCode is not documented in API docs, but see http://wiki.forum.nokia.com/index.php/TSS001320_-_Interpreting_TCallInfoV1::iExitCode_value -- supposedly "gives the reason for the termination of a finished call"

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

static TInt DoGetCallInfo(CTelephony& aTelephony,
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

    // Set to remote party phone number if possible. Will be left as
    // NULL otherwise.
    const char* number = NULL;

    // Any additional information encoded as a string. May be left as NULL.
    const char* extra = NULL;

    // Call start time. May be left as zero, which means no start time.
    time_t startTime = 0;

    // Call termination reason codes. May be left as 1, which means no
    // reason.
    TInt etelCode = 1;
    TInt netCode = 1;

    CTelephony::TCallInfoV1 callInfo;
    CTelephony::TRemotePartyInfoV1 remoteInfo;
    errCode = DoGetCallInfo(*iTelephony, callInfo, remoteInfo);

    if (errCode) {
      // Certainly this is true for idle line (1), and probably for
      // unknown status (0) as well.
      logf("failed to get call info for status %d: %s (%d)", 
	   callStatus, plat_error_strerror(errCode), errCode);
    } else {
      logf("got call info for status %d", callStatus);
    }
    TBool gotCallInfo = (errCode == 0);

    if (gotCallInfo) {
      // We would like to use call start time as the "call ID", as it
      // ought to be unique enough, but it seems to always be 0.
      {
	const TDateTime& dt = callInfo.iStartTime;
	TTime epocStartTime(dt);
	if (epocStartTime.Int64() != 0) {
	  logf("call start datetime is %d.%d.%d %d:%d:%d.%d",
	       dt.Year(), dt.Month(), dt.Day(),
	       dt.Hour(), dt.Minute(), dt.Second(), dt.MicroSecond());
	  startTime = LocalEpocTimeToUnixTime(epocStartTime);
	  logf("call start time is %d", startTime);
	  logf("time now is %d", time(NULL));
	}
      }

      // For some cases we can get some additional useful information.
      switch (callStatus)
	{
	  // May be able to get remote party phone number in these cases.
	case CTelephony::EStatusRinging: // use TRemotePartyInfoV1
	case CTelephony::EStatusDialling: // use TCallInfoV1
	  {
	    TBuf8<CTelephony::KMaxTelNumberSize + 1> numberBuf;

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
	    break;
	  }
	  
	  // May be able to get disconnect reason in this case. The
	  // information is supplied by the operator, though, which
	  // means we depend on what they choose to tell us.
	case CTelephony::EStatusDisconnecting:
	  {
	    TInt& exitCode = callInfo.iExitCode;
	    etelCode = (exitCode | 0xFFFF0000);
	    netCode = (exitCode >> 16); 
	    logf("disconnect reason: etel=%d, net=%d", etelCode, netCode);
	    char extraBuf[50];
	    g_sprintf(extraBuf, "os=%d/net=%d", etelCode, netCode);
	    extra = extraBuf;
	    break;
	  }
	  
	default:
	  {
	    // No additional information to record in this state.
	  }
	} // end switch
    }
      
    // Log.
    {
      //if (number) logf("remote party number is '%s'", number);

      GError* localError = NULL;
      if (!log_db_log_callstatus(GetLogDb(), callStatus, number, startTime, etelCode, netCode, &localError)) {
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
