#include "epoc-flightmode.hpp"

#if __FLIGHTMODE_ENABLED__

#include "common/assertions.h"
#include "common/utilities.h"
#include "common/error_list.h"
#include "common/platform_error.h"
#include "common/logging.h"
#include "er_errors.h"
#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"

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

CTOR_IMPL_CSensor_flightmode;

void CSensor_flightmode::ConstructL()
{
  iTelephony = CTelephony::NewL();
}

CSensor_flightmode::~CSensor_flightmode()
{
  Cancel();
  delete iTelephony;
}

gboolean CSensor_flightmode::StartL(GError** error)
{
  if (!IsActive()) {
    MakeRequest();
    log_db_log_status(iLogDb, NULL, "flightmode sensor started");
  }
  return TRUE;
}

void CSensor_flightmode::MakeRequest()
{
  iTelephony->NotifyChange(iStatus, CTelephony::EFlightModeChange, iDataDes);
  SetActive();
}

void CSensor_flightmode::Stop()
{
  if ((IsActive())) {
    Cancel();
    log_db_log_status(iLogDb, NULL, "flightmode sensor stopped");
  }
}

void CSensor_flightmode::RunL()
{
#if 0
  TInt errCode = KErrGeneral;
  Leave(g_error_new(domain_symbian, errCode,
		    "dummy failure: %s (%d)", 
		    plat_error_strerror(errCode), errCode));
#endif

  GError** error = &iError;
  assert_error_unset(error);

  if (iStatus.Int() == KErrNone) {
    gboolean value = TFlightModeV1ToBoolean(iData);

    if (!log_db_log_flightmode(iLogDb, value, error)) {
      LeaveWithError();
    }

    MakeRequest();
  } else {
    // Sensor read error. Log it and do nothing further. (It is
    // particularly important to avoid a situation in which we would
    // "busy loop" by repeatedly making immediately failing requests,
    // and this approach certainly avoids that.)
    logt("flightmode sensor read error");

    int errCode = iStatus.Int();
    if (!log_db_log_status(iLogDb, error, "INACTIVATE: flightmode: failure reading sensor: %s (%d)", plat_error_strerror(errCode), errCode)) {
      LeaveWithError();
    }
  }
}

void CSensor_flightmode::DoCancel()
{
  iTelephony->CancelAsync(CTelephony::EFlightModeChangeCancel);
}

const char* CSensor_flightmode::Description()
{
  return "flightmode";
}

#endif // __FLIGHTMODE_ENABLED__

/**

epoc-flightmode.cpp

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
