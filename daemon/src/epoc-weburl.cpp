#include "epoc-weburl.hpp"

#if __WEBURL_ENABLED__

#include "er_errors.h"
#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#include "common/utilities.h"

// -------------------------------------------------------------------
// the sensor object implementation...

CTOR_IMPL_CSensor_weburl;

void CSensor_weburl::ConstructL()
{
  iAhle = CAHLE::NewL();
  iAhle->SetObserverL(this);
}

CSensor_weburl::~CSensor_weburl()
{
  delete iAhle;
  delete iOldUrlArray;
}

void CSensor_weburl::AdaptiveListChanged(TInt errCode)
{
  //logf("AdaptiveListChanged(%d)", errCode);
  if (errCode) {
    er_log_symbian(0, errCode,
		   "URL change notification failure in weburl sensor");
  } else {
    TRAP(errCode, LogDataL());
    if (errCode) {
      er_log_symbian(0, errCode,
		     "URL data logging in weburl sensor");
    }
  }
}

#define MAX_URLS 100

void CSensor_weburl::LogDataL()
{
  CDesCArray* urlArray = new (ELeave) CDesCArrayFlat(MAX_URLS);
  CleanupStack::PushL(urlArray);

  CDesCArray* nameArray = new (ELeave) CDesCArrayFlat(MAX_URLS);
  CleanupStack::PushL(nameArray);

  // This just appears to give us the first items in the adaptive
  // list. There does not seem to be a way to request the latest
  // addition to the list. We may have to keep any previous copy of
  // urlArray, and log the new ones only. Difficult to analyze, but
  // better than nothing, I guess.
  iAhle->AdaptiveListL(*urlArray, *nameArray, MAX_URLS, 
		       KNullDesC, 
		       //EAHLEAdaptiveSiteList // (short url, short url) 
		       EAHLEAdaptiveSiteDetails // (caption, long url)
		       //EAHLEAdaptiveAutoComplete // (caption, long url)
		       );

  TInt numItems = urlArray->Count();
  logf("read total of %d adaptive history items", numItems);

  for (TInt i = 0; i < numItems; i++) {
    TPtrC url16(urlArray->MdcaPoint(i));
    TInt dummyPos;
    if ((!iOldUrlArray) ||
	// This returns zero if a match is found.
	// iOldUrlArray must be sorted for this to work.
	iOldUrlArray->FindIsq(url16, dummyPos, ECmpNormal)) {

      HBufC8* name8 = ConvToUtf8ZL(nameArray->MdcaPoint(i));
      CleanupStack::PushL(name8);
      
      HBufC8* url8 = ConvToUtf8ZL(url16);

      const char* name = (const char*)(name8->Ptr());
      const char* url = (const char*)(url8->Ptr());
      //logf("name is '%s', URL is '%s'", name, url);
      log_db_log_weburl(GetLogDb(), name, url, NULL);

      delete url8;
      CleanupStack::PopAndDestroy(name8);
    }
  }

  CleanupStack::PopAndDestroy(nameArray);
  CleanupStack::Pop(urlArray);

  urlArray->Sort(ECmpNormal);
  delete iOldUrlArray;
  iOldUrlArray = urlArray;
}

#endif // __WEBURL_ENABLED__

/**

epoc-weburl.cpp

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
