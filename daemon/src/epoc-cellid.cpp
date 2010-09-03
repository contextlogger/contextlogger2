#include "epoc-cellid.hpp"

#if __CELLID_ENABLED__

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

CTOR_IMPL_CSensor_cellid;

void CSensor_cellid::ConstructL()
{
  iTelephony = CTelephony::NewL();
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iTimer, iTimer.CreateLocal()); 
}

CSensor_cellid::~CSensor_cellid()
{
  Cancel();
  SESSION_CLOSE_IF_OPEN(iTimer);
  delete iTelephony;
}

gboolean CSensor_cellid::StartL(GError** error)
{
  iNumScanFailures = 0;
  if (!IsActive()) {
    MakeRequest();
    log_db_log_status(iLogDb, NULL, "cellid sensor started");
  }
  return TRUE;
}

void CSensor_cellid::Stop()
{
  if ((IsActive())) {
    Cancel();
    log_db_log_status(iLogDb, NULL, "cellid sensor stopped");
  }
}

void CSensor_cellid::MakeRequest()
{
  iTelephony->NotifyChange(iStatus, CTelephony::ECurrentNetworkInfoChange, iDataDes);
  SetActive();
  iState = EQuerying;
}

void CSensor_cellid::SetTimer() 
{
  int secs = 5 * (1 + iNumScanFailures) + (rand() % 10);
  TTimeIntervalMicroSeconds32 interval = SecsToUsecs(secs);
  logf("cellid timer set to %d secs / %d usecs", secs, interval.Int());
  iTimer.After(iStatus, interval);
  SetActive();
  iState = ERetryWaiting;
}

// Retry timer expired.
void CSensor_cellid::HandleTimerL()
{
  int errCode = iStatus.Int();
  User::LeaveIfError(errCode); // unexpected with an interval timer
  MakeRequest();
}

gboolean CSensor_cellid::HandleReadGL(GError** error)
{
  int errCode = iStatus.Int();

  if (errCode) {
    // Sensor read error. Log it and issue a new request if there have
    // not been all that many consecutive errors. (It is particularly
    // important to avoid a situation in which we would "busy loop" by
    // repeatedly making immediately failing requests, and this
    // approach certainly avoids that.) Read errors should not be all
    // that unusual here if one disables the network or something, but
    // hopefully network becoming unavailable is just one event among
    // others. Have seen at least KErrOverflow here upon turning on
    // flight mode.
    iNumScanFailures++;
    logf("%dth consecutive failure in cellid", iNumScanFailures);

    if (iNumScanFailures < 100) {
      if (!log_db_log_status(iLogDb, error, "ERROR: %dth consecutive failure reading cellid sensor: %s (%d)", iNumScanFailures, plat_error_strerror(errCode), errCode)) {
	// Logging failed.
	return FALSE;
      }
      SetTimer();
    } else {
      log_db_log_status(iLogDb, NULL, "INACTIVATE: cellid: stopping scanning due to too many errors");
    }

    return TRUE;
  }

  if (errCode == KErrNone) {
    iNumScanFailures = 0;

    // Logging is not all that straightforward here, as some readings
    // can for instance indicate that some or all of the usual
    // (country_code, network_code, area_code, cell_id) information is
    // not available or has just become unavailable. Our present
    // solution is to log nothing unless all of that information is
    // available.
    if (!iData.iAccess) {
      logf("cellid info: no network access: iAreaKnown=%d", (int)iData.iAreaKnown);
      MakeRequest();
    } else {
      // Here we are assuming that the initial "zero" iOldData is not
      // a valid reading, and likely this is true as it would mean no
      // country code or network ID. And duplicates can still occur,
      // across sensor restarts. This should nonetheless help reduce
      // the amount of logged data quite a bit, as it seems that in
      // practice one gets around 2-4 cell ID events per minute even
      // when there is no cell change.
      if ((iOldData.iCountryCode == iData.iCountryCode) &&
	  (iOldData.iNetworkId == iData.iNetworkId) &&
	  (iOldData.iLocationAreaCode == iData.iLocationAreaCode) &&
	  (iOldData.iCellId == iData.iCellId)) {
	// Same reading as previously.
	//logt("duplicate cell ID reading");
	MakeRequest();
      } else {
	//logt("new cell ID reading");
	iOldData = iData;

	// In practice it seems that countryCode and networkCode are
	// decimal strings, but this may only apply to GSM networks, and
	// hence we are treating them as strings. Whoever parses the
	// database content may decide to do something different if they
	// see that all the data indeed is decimal strings.
	// http://en.wikipedia.org/wiki/List_of_mobile_country_codes
	// http://en.wikipedia.org/wiki/Mobile_Network_Code
	HBufC8* countryCode = ConvToUtf8ZL(iData.iCountryCode);
	CleanupStack::PushL(countryCode);
	HBufC8* networkCode = ConvToUtf8ZL(iData.iNetworkId);
	CleanupStack::PushL(networkCode);
	int areaCode = iData.iLocationAreaCode; // valid if iAccess is true
	int cellId = iData.iCellId; // valid if iAccess is true

	gboolean ok = log_db_log_cellid(iLogDb, (char*)(countryCode->Ptr()), (char*)(networkCode->Ptr()), areaCode, cellId, error);
	CleanupStack::PopAndDestroy(2); // networkCode, countryCode
	if (!ok) return FALSE;
	MakeRequest();
      }
    }
  }

  return TRUE;
}

// To display readable logged times, do something like
//
//   select datetime(unixtime, 'unixepoch', 'localtime') from cellid_scan;
gboolean CSensor_cellid::RunGL(GError** error)
{
  assert_error_unset(error);
  TState oldState = iState;
  iState = EInactive;

  switch (oldState)
    {
    case EQuerying:
      {
	return HandleReadGL(error);
      }
    case ERetryWaiting:
      {
	HandleTimerL();
        break;
      }
    default:
      {
        assert(0 && "unexpected state");
        break;
      }
    }

  return TRUE;
}

void CSensor_cellid::DoCancel()
{
  switch (iState)
    {
    case EQuerying:
      {
	iTelephony->CancelAsync(CTelephony::ECurrentNetworkInfoChangeCancel);
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

const char* CSensor_cellid::Description()
{
  return "cellid";
}

#endif // __CELLID_ENABLED__

/**

epoc-cellid.cpp

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
