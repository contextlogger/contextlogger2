#include "epoc-httpurl.hpp"

/*
 Implements **URL observing**. Should work on S60 v3.0-5.2 at least, and maybe even older systems. The [AHLE API http://wiki.forum.nokia.com/index.php/Adaptive_History_List_API_for_5th_Edition] now appears to be intended only for use internally by apps, and not to access Web history. Writing an [HTTP filter plugin http://wiki.forum.nokia.com/index.php/Writing_an_HTTP_filter_plugin] apparently does the trick and does it better (should be able to get all URLs, even ones in other applications that use ``RHTTPSession``), [on older devices at least http://discussion.forum.nokia.com/forum/showthread.php?128548-How-to-monitor-S60-browser-to-get-the-details-of-URLs-visited-by-the-user]. Requires the ``NetworkControl`` capability, meaning that only publishers can sign.
*/

#if __HTTPURL_ENABLED__

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#include "cl2webfilter.h"

#include "common/utilities.h"

// -------------------------------------------------------------------
// the sensor object implementation...

CSensor_httpurl* CSensor_httpurl::NewLC(ac_AppContext* aAppContext)
{
  CSensor_httpurl* obj = new (ELeave) CSensor_httpurl(aAppContext);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  return obj;
}

CSensor_httpurl* CSensor_httpurl::NewL(ac_AppContext* aAppContext)
{
  CSensor_httpurl* obj = CSensor_httpurl::NewLC(aAppContext);
  CleanupStack::Pop(obj);
  return obj;
}

CSensor_httpurl::~CSensor_httpurl()
{
  Cancel();
  SESSION_CLOSE_IF_OPEN(iProperty);
}

CSensor_httpurl::CSensor_httpurl(ac_AppContext* aAppContext) : 
  CActive(CActive::EPriorityStandard), 
  iAppContext(aAppContext)
{
  CActiveScheduler::Add(this);
}

void CSensor_httpurl::ConstructL()
{
  // We have both the "server" and "client" try to define the key. We
  // do open a session to the property, after all, and than cannot be
  // done unless the property exists.
  TInt errCode = RProperty::Define(TUid::Uid(KCl2WebFilterCat), 
				   KCl2WebFilterKey, 
				   RProperty::EByteArray,
				   256);
  if (errCode != KErrNone && errCode != KErrAlreadyExists) 
    User::Leave(errCode);

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iProperty, iProperty.Attach(TUid::Uid(KCl2WebFilterCat), KCl2WebFilterKey, EOwnerThread));
  MakeRequest();
}

void CSensor_httpurl::MakeRequest()
{
  iProperty.Subscribe(iStatus);
  SetActive();
}

void CSensor_httpurl::DoCancel()
{
  assert(IS_SESSION_OPEN(iProperty));
  iProperty.Cancel();
}

TInt CSensor_httpurl::RunError(TInt errCode)
{
  er_log_symbian(0, errCode, "INACTIVATE: leave in httpurl sensor event handler");
  return 0;
}

void CSensor_httpurl::RunL()
{
  TInt errCode = iStatus.Int();
  if (errCode) {
    er_log_symbian(0, errCode, "INACTIVATE: httpurl sensor request error");
    return;
  }

  TBuf8<RProperty::KMaxPropertySize + 1> buf;
  errCode = iProperty.Get(buf);
  if (errCode) {
    er_log_symbian(0, errCode, "INACTIVATE: httpurl data fetch error");
    return;
  }

  MakeRequest();

  const char* url = (const char*)(buf.PtrZ());
  //logg("httpurl URL is '%s'", url);
  log_db_log_httpurl(GetLogDb(), url, NULL);
}

#endif // __HTTPURL_ENABLED__

/**

epoc-httpurl.cpp

Copyright 2009-2010 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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
