/*
 !concept {:name => "Observing SMS events",
   :desc => "Detecting and getting information about incoming and outgoing SMS messages."}
*/

#include "epoc-smsevent.hpp"

#if __SMSEVENT_ENABLED__

#include "er_errors.h"
#include "ld_logging.h"
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

#define iLogDb ac_LogDb(iAppContext)

gboolean CSensor_smsevent::StartL(GError** error)
{
  if (!IsActive()) {
    ActivateL();
    //log_db_log_status(iLogDb, NULL, "smsevent sensor started");
  }
  return TRUE;
}

void CSensor_smsevent::Stop()
{
  if ((IsActive())) {
    Disactivate();
    //log_db_log_status(iLogDb, NULL, "smsevent sensor stopped");
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

// Caller must free non-NULL result.
static gchar* GetBodyL(CRichText& richText)
{
  if (richText.HasMarkupData())
    return NULL; // unexpected

  TInt bodyLen = richText.DocumentLength();
  HBufC* hbuf16 = HBufC::NewLC(bodyLen);
  TPtr bodyDes(hbuf16->Des());
  richText.Extract(bodyDes);
  gchar* bodyStr = ConvToUtf8CString(bodyDes);
  CleanupStack::PopAndDestroy(); // hbuf16

  if (G_UNLIKELY(!bodyStr)) {
    er_fatal_oom;
    return NULL;;
  }

  //guilogf("smsevent: body '%s'", bodyStr);
  
  return bodyStr;
  //g_free(bodyStr);
}

static gchar* GetBody(CRichText& richText)
{
  gchar* result = NULL;
  TRAPD(error, result = GetBodyL(richText));
  if (error) {
    er_log_symbian(0, error, "smsevent: failure getting body content");
  }
  return result;
}

void CSensor_smsevent::LogEvent(const char* evType, 
				const TDesC& aTelNoDes,
				CRichText& aBody)
{
  //logg("sms event type: '%s'", evType);

  gchar* telNo = NULL;
  gchar* contactName = NULL;

  if (aTelNoDes.Length() > 0) {
    telNo = ConvToUtf8CString(aTelNoDes);
    if (G_UNLIKELY(!telNo)) {
      er_fatal_oom;
      return;
    }
    //logg("sms remote party number is '%s'", telNo);
    contactName = GetContactNameByPhoneNo(aTelNoDes);
    if (contactName) {
      //logg("sms remote party name is '%s'", contactName);
    } else {
      //logt("could not get sms remote party name");
    }
  } else {
    logt("could not get sms remote party phone number");
  }

  gchar* bodyText = NULL;
  if (ac_STATIC_GET(log_sms_body)) {
    bodyText = GetBody(aBody);
  }

  LogDb* logDb = GetLogDb();
  GError* localError = NULL;
  gboolean ok = log_db_log_smsevent(logDb, evType, telNo, contactName, bodyText, &localError);
  guilogf("smsevent: %s '%s' name='%s' body='%s'",
	  evType, telNo, 
	  contactName ? contactName : "(N/A)",
	  bodyText ? bodyText : "");
  g_free(telNo);
  g_free(contactName);
  g_free(bodyText);

  if (G_UNLIKELY(!ok)) {
    gx_txtlog_fatal_error_free(localError);
    return;
  }
}

void CSensor_smsevent::handle_reception(const TMsvId& entry_id,
					const TMsvId& folder_id,
					const TDesC& senderDes,
					CRichText& body)
{
  //logt("smsevent receive");
  LogEvent("recv", senderDes, body);
}

void CSensor_smsevent::handle_sending(const TMsvId& entry_id,
				      const TDesC& senderDes,
				      CRichText& body)
{
  //logt("smsevent send");
  LogEvent("send", senderDes, body);
}

void CSensor_smsevent::handle_error(TInt errCode)
{
  assert(errCode != 0);

  Stop();

  LogDb* logDb = GetLogDb();
  GError* localError = NULL;
  if (!log_db_log_status(logDb, &localError,
			 "INACTIVATE: smsevent: error: %s (%d)",
			 plat_error_strerror(errCode), errCode)) {
    gx_txtlog_fatal_error_free(localError);
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
			 "INACTIVATE: smsevent: session termination")) {
    gx_txtlog_fatal_error_free(localError);
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
