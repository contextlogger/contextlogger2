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
  if ((IsActive())) {
    return TRUE;
  }

  MakeRequest();
  logt("flightmode sensor started");
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
    logt("flightmode sensor stopped");
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
    if (!log_db_log_status(iLogDb, error, "ERROR: failure reading flightmode sensor: %s (%d)", plat_error_strerror(errCode), errCode)) {
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
