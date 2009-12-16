#include "epoc-smsevent.hpp"

#if __SMSEVENT_ENABLED__

#include "er_errors.h"
#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"
#include "ut_telno_epoc.hpp"
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

void CSensor_smsevent::LogEvent(const char* evType, const TDesC& aTelNoDes)
{
  gchar* telNo = ConvToUtf8CString(aTelNoDes);
  if (!telNo) {
    ex_log_fatal_error(KErrNoMemory);
    return;
  }
  logf("smsevent %s, number '%s'", evType, telNo);

  gchar* contactName = GetContactNameByPhoneNo(aTelNoDes);
  logf("smsevent contact name '%s'", contactName);

  LogDb* logDb = GetLogDb();
  GError* localError = NULL;
  gboolean ok = log_db_log_smsevent(logDb, evType, telNo, contactName, &localError);
  g_free(telNo);
  g_free(contactName);

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

/**

epoc-smsevent.cpp

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
