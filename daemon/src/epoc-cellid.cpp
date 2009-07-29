#include "epoc-cellid.hpp"

#if __CELLID_ENABLED__

#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#include "common/assertions.h"
#include "er_errors.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

// -------------------------------------------------------------------
// the sensor object implementation...

CTOR_IMPL_CSensor_cellid;

void CSensor_cellid::ConstructL()
{
  iTelephony = CTelephony::NewL();
}

CSensor_cellid::~CSensor_cellid()
{
  Cancel();
  delete iTelephony;
}

gboolean CSensor_cellid::StartL(GError** error)
{
  iNumScanFailures = 0;
  if (!IsActive()) {
    MakeRequest();
    logt("cellid sensor started");
  }
  return TRUE;
}

void CSensor_cellid::MakeRequest()
{
  iTelephony->NotifyChange(iStatus, CTelephony::ECurrentNetworkInfoChange, iDataDes);
  SetActive();
}

void CSensor_cellid::Stop()
{
  if ((IsActive())) {
    Cancel();
    logt("cellid sensor stopped");
  }
}

// To display readable logged times, do something like
//
//   select datetime(unixtime, 'unixepoch', 'localtime') from cellid_scan;
gboolean CSensor_cellid::RunGL(GError** error)
{
  assert_error_unset(error);

#if 0
  { // test code...
    TInt errCode = KErrGeneral;
    Leave(g_error_new(domain_symbian, errCode,
		      "dummy failure: %s (%d)", 
		      plat_error_strerror(errCode), errCode));
    assert(0);
  }
#endif

  int errCode = iStatus.Int();

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
	logt("duplicate cell ID reading");
	MakeRequest();
      } else {
	logt("new cell ID reading");
	iOldData = iData;

	// In practice it seems that countryCode and networkCode are
	// decimal strings, but this may only apply to GSM networks, and
	// hence we are treating them as strings. Whoever parses the
	// database content may decide to do something different if they
	// see that all the data indeed is decimal strings.
	HBufC8* countryCode = ConvToUtf8ZL(iData.iCountryCode);
	CleanupStack::PushL(countryCode);
	HBufC8* networkCode = ConvToUtf8ZL(iData.iNetworkId);
	CleanupStack::PushL(networkCode);
	int areaCode = iData.iLocationAreaCode; // valid if iAccess is true
	int cellId = iData.iCellId; // valid if iAccess is true

	gboolean ok = log_db_log_cellid(iLogDb, (char*)(countryCode->Ptr()), (char*)(networkCode->Ptr()), areaCode, cellId, error);
	CleanupStack::PopAndDestroy(2); // networkCode, countryCode

	if (ok) {
	  MakeRequest();
	} else {
	  // Logging failed. This is bad. What is the point of running a
	  // context logger that cannot log.
	  assert_error_set(error);
	  return FALSE;
	}
      }
    }
  } else {
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
      if (!log_db_log_status(iLogDb, error, "ERROR: failure reading cellid sensor: %s (%d)", plat_error_strerror(errCode), errCode)) {
	// Logging failed. This is bad. What is the point of running a
	// context logger that cannot log.
	assert_error_set(error);
	return FALSE;
      }
      MakeRequest();
    } else {
      logt("stopping cellid scanning due to too many errors");
    }
  }

  return TRUE;
}

void CSensor_cellid::DoCancel()
{
  iTelephony->CancelAsync(CTelephony::ECurrentNetworkInfoChangeCancel);
}

const char* CSensor_cellid::Description()
{
  return "cellid";
}

#endif // __CELLID_ENABLED__
