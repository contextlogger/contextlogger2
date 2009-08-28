#ifndef __epoc_profile_hpp__
#define __epoc_profile_hpp__

#include "application_config.h"

#if __PROFILE_ENABLED__ && !__HAVE_PROFILEENGINE_LIB__

#include <glib.h>

#include <e32base.h>
#include <cprofilechangenotifyhandler.h>
#include <mprofilechangeobserver.h>

// For S60 v3.0 there is ProfilesEngine.ZIP SDK API plugin, but not
// for later platform versions. (Mind you though that at least v3.1
// maintains binary compatibility with the ProfilesEngine.ZIP defined
// API. So for now it seems to be okay to just install the v3.0
// ProfilesEngine.ZIP plugin to a v3.1 SDK.)
//
// We require a separate implementation for later platform versions.
#include <mprofileengine.h>

#include "log-db.h"

NONSHARABLE_CLASS(CSensor_profile) : public CBase, 
				     public MProfileChangeObserver
{
 public:

  static CSensor_profile* NewL(LogDb* aLogDb);

  virtual ~CSensor_profile();

  gboolean StartL(GError** error);

  void Stop();

  gboolean IsActive() { return (iReader != NULL); }

 private:
 
  CSensor_profile(LogDb* aLogDb);

  void ConstructL();

 private:

  LogDb* iLogDb; // not owned

 private: // MProfileChangeObserver

  virtual void HandleActiveProfileEventL(TProfileEvent aProfileEvent,
					 TInt aProfileId);

 private:

  // Provides a subscription to notification, i.e. we need not keep
  // repeatedly making new requests.
  CProfileChangeNotifyHandler* iReader;

 private: // for profile name getting

  HBufC8* GetCurrentProfileNameL();
  MProfileEngine* iProfileEngine;
};

#endif // __PROFILE_ENABLED__

#endif /* __epoc_profile_hpp__ */
