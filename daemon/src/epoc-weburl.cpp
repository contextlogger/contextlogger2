#include "epoc-weburl.hpp"

#if __WEBURL_ENABLED__

/*
http://wiki.forum.nokia.com/index.php/Adaptive_History_List_API
http://wiki.forum.nokia.com/index.php/Adaptive_History_List_API_for_5th_Edition
*/

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#include "common/utilities.h"

using namespace epocxplat;

// -------------------------------------------------------------------
// the sensor object implementation...

CTOR_IMPL_CSensor_weburl;

void CSensor_weburl::ConstructL()
{
  if (epocxplat::HasFeature(EFeatureAhleBrowser)) {
    iAhle = AhleBrowser::NewNotifierL(*this);
  }
}

CSensor_weburl::~CSensor_weburl()
{
  delete iAhle;
  delete iOldUrlArray;
}

void CSensor_weburl::AhleBrowserError(TInt errCode)
{
  er_log_symbian(0, errCode, "failure in weburl sensor");
}

// If this leaves, we get an AhleBrowserError callback.
void CSensor_weburl::AhleBrowserDataL(const TDesC& aName, const TDesC& aUrl)
{
  HBufC8* name8 = ConvToUtf8ZL(aName);
  CleanupStack::PushL(name8);
      
  HBufC8* url8 = ConvToUtf8ZL(aUrl);

  const char* name = (const char*)(name8->Ptr());
  const char* url = (const char*)(url8->Ptr());
  //logf("name is '%s', URL is '%s'", name, url);
  log_db_log_weburl(GetLogDb(), name, url, NULL);

  delete url8;
  CleanupStack::PopAndDestroy(name8);
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
