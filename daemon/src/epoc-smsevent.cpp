#include "epoc-smsevent.hpp"

#if __SMSEVENT_ENABLED__

#include "er_errors.h"
#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

// -------------------------------------------------------------------
// the sensor object implementation...

CTOR_IMPL_CSensor_smsevent;

void CSensor_smsevent::ConstructL()
{
  // nothing to do
}

CSensor_smsevent::~CSensor_smsevent()
{
  delete iSmsEventNotifier;
}

gboolean CSensor_smsevent::StartL(GError** error)
{
  if (!IsActive()) {
    ActivateL();
    logt("smsevent sensor started");
  }
  return TRUE;
}

void CSensor_smsevent::Stop()
{
  if ((IsActive())) {
    Disactivate();
    logt("smsevent sensor stopped");
  }
}

void CSensor_smsevent::ActivateL()
{
  iSmsEventNotifier = CSmsEventNotifier::NewL();
  iSmsEventNotifier->SetHandler(this);
}

void CSensor_smsevent::Disactivate()
{
  DELETE_Z(iSmsEventNotifier);
}

/*
void CSensor_smsevent::HandleRead()
{
  int errCode = iStatus.Int();

  LogDb* logDb = ac_LogDb(iAppContext);
  GError* localError = NULL;

  if (errCode) {
    iNumScanFailures++;
    logf("%dth consecutive failure in smsevent", iNumScanFailures);

    if (iNumScanFailures < 100) {
      if (!log_db_log_status(logDb, &localError, 
			     "ERROR: failure reading smsevent sensor: %s (%d)", 
			     plat_error_strerror(errCode), errCode)) {
	// Logging failed.
	gx_log_free_fatal_error(localError);
	return;
      }

      SetTimer();
    } else {
      logt("stopping smsevent scanning due to too many errors");
    }

    return;
  }

  {
    iNumScanFailures = 0;

    if (!(SMSEVENT_EQ(iSmsevent, iOldSmsevent))) {
      logf("new smsevent value: %u/%u", 
	   iSmsevent.iSmsevent,
	   iSmsevent.iCapabilities);
      logf("smsevents: charger=%c network=%c call=%c", 
	   IND2CH(CTelephony::KIndChargerConnected),
	   IND2CH(CTelephony::KIndNetworkAvailable),
	   IND2CH(CTelephony::KIndCallInProgress));

      if (!log_db_log_smsevent(logDb, iSmsevent.iSmsevent, 
				iSmsevent.iCapabilities,
				&localError)) {
	gx_log_free_fatal_error(localError);
	return;
      }
      
      iOldSmsevent = iSmsevent;
    }

    MakeRequest();
  }
}
*/

void CSensor_smsevent::LogEvent(const char* evType, const TDesC& aTelNoDes)
{
  gchar* telNo = ConvToUtf8CString(aTelNoDes);
  if (!telNo) {
    ex_log_fatal_error(KErrNoMemory);
    return;
  }

  logf("smsevent %s, number '%s'", evType, telNo);

  LogDb* logDb = GetLogDb();
  GError* localError = NULL;
  gboolean ok = log_db_log_smsevent(logDb, evType, telNo, &localError);
  g_free(telNo);

  if (!ok) {
    gx_log_free_fatal_error(localError);
    return;
  }
}

void CSensor_smsevent::handle_reception(const TMsvId& entry_id, 
					const TMsvId& folder_id, 
					const TDesC& senderDes, 
					const TDesC& body)
{
  //logt("smsevent receive");
  LogEvent("recv", senderDes);
} 

void CSensor_smsevent::handle_sending(const TMsvId& entry_id, 
				      const TDesC& senderDes, 
				      const TDesC& body)
{
  //logt("smsevent send");
  LogEvent("send", senderDes);
}

void CSensor_smsevent::handle_error(TInt errCode)
{
  assert(errCode != 0);

  Stop();

  LogDb* logDb = GetLogDb();
  GError* localError = NULL;
  if (!log_db_log_status(logDb, &localError, 
			 "ERROR: stopped smsevent sensor due to error: %s (%d)", 
			 plat_error_strerror(errCode), errCode)) {
    gx_log_free_fatal_error(localError);
    return;
  }
}

// We must stop using the notifier session.
void CSensor_smsevent::handle_close()
{
  Stop();

  LogDb* logDb = GetLogDb();
  GError* localError = NULL;
  if (!log_db_log_status(logDb, &localError, 
			 "ERROR: stopped smsevent sensor "
			 "due to session termination")) {
    gx_log_free_fatal_error(localError);
    return;
  }
}

#endif // __SMSEVENT_ENABLED__
