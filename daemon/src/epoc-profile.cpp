#include "epoc-profile.hpp"

#if __PROFILE_ENABLED__ && !__HAVE_PROFILEENGINE_LIB__

#include "epoc-ao-gerror.hpp"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#include "common/assertions.h"
#include "er_errors.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <mprofile.h>
#include <mprofilename.h>

// The caller must free the returned buffer. The buffer is guaranteed
// to have a zero terminator.
HBufC8* CSensor_profile::GetCurrentProfileNameL()
{
  MProfile* profile = iProfileEngine->ActiveProfileL();
  CleanupReleasePushL(*profile);

  const MProfileName& profileName = profile->ProfileName();
  const TDesC& name16 = profileName.Name();

  HBufC8* name8 = ConvToUtf8ZL(name16);

  CleanupStack::PopAndDestroy(); // profile

  return name8;
}

CSensor_profile* CSensor_profile::NewL(LogDb* aLogDb)
{
  CSensor_profile* obj = new (ELeave) CSensor_profile(aLogDb);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop();
  return obj;
}

CSensor_profile::CSensor_profile(LogDb* aLogDb)
{
  iLogDb = aLogDb;
}

void CSensor_profile::ConstructL()
{
  // xxx use the RFs* variant instead, as soon as other sensors need a handle
  iProfileEngine = CreateProfileEngineL();
}

CSensor_profile::~CSensor_profile()
{
  delete iReader;
  if (iProfileEngine)
    {
      iProfileEngine->Release();
    }
}

gboolean CSensor_profile::StartL(GError** error)
{
  if (!iReader) {
    // This starts observing right away, and we cannot cancel our
    // "subscription" without destroying the object.
    iReader = CProfileChangeNotifyHandler::NewL(this);
    log_db_log_status(iLogDb, NULL, "profile sensor started");
  }
  return TRUE;
}

void CSensor_profile::Stop()
{
  delete iReader;
  iReader = NULL;
  log_db_log_status(iLogDb, NULL, "profile sensor stopped");
}

void CSensor_profile::HandleActiveProfileEventL(TProfileEvent aProfileEvent,
						TInt aProfileId)
{
#if 0
  { // test code...
    ex_txtlog_fatal_error(KErrGeneral);
  }
#endif

  if (aProfileEvent == EProfileNewActiveProfile) {
    // We shall log 'aProfileId'.
    GError* error = NULL;

    // And we shall also log the current profile name, which hopefully
    // still corresponds to the ID. I guess in theory it might not, so
    // we might consider using and probably caching a
    // 'ProfilesNamesArrayLC' returned value instead.        xxx
    HBufC8* pnDes = NULL;
    TRAPD(errCode, pnDes = GetCurrentProfileNameL());
    char* profileName;
    if (errCode) {
      profileName = "";
      logf("could not get profile name for profile %d: %s (%d)", aProfileId, plat_error_strerror(errCode), errCode);
    } else {
      profileName = (char*)pnDes->Ptr();
    }

    gboolean ok = log_db_log_profile(iLogDb, aProfileId, profileName, &error);
    delete pnDes;
    if (!ok) {
      gx_txtlog_error_clear(&error);
      User::Leave(KErrWrite);
    }
  }
}

#endif // __PROFILE_ENABLED__

/**

epoc-profile.cpp

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
