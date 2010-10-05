#include "epoc-profile-31.hpp"

#if __PROFILE_ENABLED__ && __HAVE_PROFILEENGINE_LIB__

#include "epoc-ao-gerror.hpp"
#include "er_errors.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <proengfactory.h> // profileengine.lib
#include <mproengprofile.h>
#include <mproengprofilename.h>

// The caller must free the returned buffer. The buffer is guaranteed
// to have a zero terminator.
HBufC8* CSensor_profile::GetCurrentProfileNameL()
{
  MProEngProfile* profile = iProfileEngine->ActiveProfileLC();

  MProEngProfileName& profileName = profile->ProfileName();
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
  iProfileEngine = ProEngFactory::NewEngineL();
  iNotifyHandler = ProEngFactory::NewNotifyHandlerL();
}

gboolean CSensor_profile::StartL(GError** error)
{
  if (!iIsActive) {
    iNotifyHandler->RequestProfileActivationNotificationsL(*this);
    iIsActive = ETrue;
    log_db_log_status(iLogDb, NULL, "profile sensor started");
  }
  return TRUE;
}

void CSensor_profile::Stop()
{
  if (iIsActive) {
    iNotifyHandler->CancelActiveProfileNotifications();
    iIsActive = EFalse;
    log_db_log_status(iLogDb, NULL, "profile sensor stopped");
  }
}

CSensor_profile::~CSensor_profile()
{
  delete iNotifyHandler; // surely cancels requests as well
  if( iProfileEngine )
    {
      iProfileEngine->Release();
    }
}

// This implementation never leaves.
void CSensor_profile::HandleProfileActivatedL(TInt aProfileId)
{
  // We shall log 'aProfileId'. And we shall also log the current
  // profile name, which hopefully still corresponds to the ID. I
  // guess in theory it might not, so we might consider using and
  // probably caching a 'ProfilesNamesArrayLC' returned value instead.
  // xxx
  HBufC8* pnDes = NULL;
  TRAPD(errCode, pnDes = GetCurrentProfileNameL());
  char* profileName;
  if (errCode) {
    profileName = "";
    dblogf("could not get profile name for profile %d: %s (%d)", 
	   aProfileId, plat_error_strerror(errCode), errCode);
  } else {
    profileName = (char*)pnDes->Ptr();
  }

  GError* localError = NULL;
  gboolean ok = log_db_log_profile(iLogDb, aProfileId, profileName, &localError);
  delete pnDes;
  if (!ok) {
    gx_dblog_fatal_error_free(iLogDb, localError);
    return;
  }
}

// The default empty implementation suits us fine.
/*
void CSensor_profile::HandleProfileActivationNotificationError(TInt)
{
}
*/

#endif // __PROFILE_ENABLED__

/**

epoc-profile-31.cpp

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
