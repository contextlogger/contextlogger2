#include "epoc-callstatus.hpp"

#if __CALLSTATUS_ENABLED__

#include "er_errors.h"
#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"
#include "ut_telno_epoc.hpp"
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
    log_db_log_status(GetLogDb(), NULL, "callstatus sensor started");
  }
  return TRUE;
}

void CSensor_callstatus::Stop()
{
  if ((IsActive())) {
    Cancel();
    log_db_log_status(GetLogDb(), NULL, "callstatus sensor stopped");
  }
}

// We expect success here.
void CSensor_callstatus::HandleGotFlightMode(TInt errCode)
{
  if (errCode) {
    log_db_log_status(GetLogDb(), NULL,
		      "INACTIVATE: callstatus: failure getting flight mode: %s (%d)", 
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
		      "INACTIVATE: callstatus: flight mode notification error: %s (%d)", 
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
    if (iRetryAo->Retry()) {
      iState = ERetryWaiting;
    } else {
      log_db_log_status(GetLogDb(), NULL,
			"INACTIVATE: callstatus: max num of retries");
      Cancel();
    }
  } else {
    iRetryAo->ResetFailures();

    // Here iStatus is of the enum type TCallStatus which has at
    // least 10 or so different possible values.
    CTelephony::TCallStatus callStatus = iCallStatusNotifier->Data().iStatus;

    // Set to remote party phone number if possible. Will be left as
    // NULL otherwise.
    const char* number = NULL;

    // Any contact name as a string. May be left as NULL. Must be
    // freed if non-NULL.
    gchar* contactName = NULL;

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

#define SET_NUMBER_AND_CONTACT { \
	if (numberW.Length() > 0) { \
	  numberBuf.Copy(numberW); \
	  number = (const char*)(numberBuf.PtrZ()); \
	  logf("call remote party number is '%s'", number); \
	  contactName = GetContactNameByPhoneNo(numberW); \
	  if (contactName) { \
	    logf("call remote party name is '%s'", contactName); \
	  } else { \
	    logt("could not get call remote party name"); \
	  } \
	} else { \
	  logt("could not get call remote party phone number"); \
	} \
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
		    SET_NUMBER_AND_CONTACT;
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
	      SET_NUMBER_AND_CONTACT;
	    }
	    break;
	  }
	  
	  // May be able to get disconnect reason in this case. The
	  // information is supplied by the operator, though, which
	  // means we depend on what they choose to tell us.
	case CTelephony::EStatusDisconnecting:
	  {
	    // KErrNone if the call ended normally.
	    TInt& exitCode = callInfo.iExitCode;
#if 0
            // "The result of a right shift of a signed negative
            // quantity is implementation dependent." Do not know of
            // any C++ compilers that implement right shift as a
            // logical rather than arithmetic shift, but still, we can
            // rely on an unsigned shift instead. And with casting
            // rather than logical ORing we can also make these macros
            // work for both positive and negative arguments. And
            // note, these macros produce a 32-bit result, not 16-bit
            // (even though they would fit).
#if sizeof(exitCode) != 4
#error unexpected exitCode size
#endif
#define INT32LEFTWORD(x) ((TInt32)((TInt16)((TUint16)(((TUint32)(x)) >> 16))))
#define INT32RIGHTWORD(x) ((TInt32)((TInt16)((TUint16)(((TUint32)(x))))))

	    // This is KErrNotFound (-1) if no call is in progress,
	    // and KErrGeneral (-2) unless there is an appropriate
	    // E32 or core ETel error code.
	    etelCode = INT32RIGTHWORD(exitCode);

	    // This code is network-specific, but it may be that in
	    // the event of the call terminating due to a network
	    // error or the remote party's actions, the code should
	    // be documented in the "exterror.h" Symbian header
	    // file, available in some SDKs.
	    netCode = INT32LEFTWORD(exitCode);

            // Left shift is well defined, so let us use it to verify
            // the results.
	    if (exitCode != ((netCode << 16) | (etelCode & 0xffff))) {
	      logt("WARNING: exitCode consistency check failed");
            }
#else
            // xxx Only works right for negative values, for zero this
            // gives -65536.
	    etelCode = (exitCode | 0xFFFF0000);
            // xxx Dangerous. Compiler-specific behavior.
	    netCode = (exitCode >> 16);
#endif
	    logf("disconnect reason: code=%d, etel=%d, net=%d", exitCode, etelCode, netCode);
	    /*
	    char extraBuf[50];
	    g_sprintf(extraBuf, "os=%d/net=%d", etelCode, netCode);
	    */
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
      GError* localError = NULL;
      if (!log_db_log_callstatus(GetLogDb(), callStatus, number, contactName, startTime, etelCode, netCode, &localError)) {
	gx_txtlog_fatal_error_free(localError);
	goto cleanup;
      }
    }

    iCallStatusNotifier->MakeRequest();
    iState = EQueryingCallStatus;

  cleanup:
    g_free(contactName);
  }
}

// We expect success here.
void CSensor_callstatus::RetryTimerExpired(CRetryAo* src, TInt errCode)
{
  (void)src;
  if (errCode) {
    log_db_log_status(GetLogDb(), NULL,
		      "INACTIVATE: callstatus: timer error: %s (%d)", 
		      plat_error_strerror(errCode), errCode);
    Cancel();
  } else {
    iCallStatusNotifier->MakeRequest();
    iState = EQueryingCallStatus;
  }
}

#endif // __CALLSTATUS_ENABLED__

/**

epoc-callstatus.cpp

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
