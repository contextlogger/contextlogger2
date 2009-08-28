#ifndef __epoc_profile_31_hpp__
#define __epoc_profile_31_hpp__

#include "application_config.h"

#if __PROFILE_ENABLED__ && __HAVE_PROFILEENGINE_LIB__

#include "log-db.h"

#include <glib.h>

#include <e32base.h>

// S60 v3.1, v3.2 and v5.0 SDKs include ``mproengprofile.h``, and
// hence we are using that in all but v3.0 builds. Can link against
// profileengine.lib, use the API in
// ``mproengactiveprofileobserver.h``, WriteDeviceData required only
// for changing profile.
#include <mproengengine.h> // profileengine.lib
#include <mproengnotifyhandler.h>
#include <mproengprofileactivationobserver.h>

NONSHARABLE_CLASS(CSensor_profile) : 
  public CBase, 
  public MProEngProfileActivationObserver
{
 public:

  // For simplicity, we make sure to implement the same interface here
  // as the other CSensor_profile variant(s).

  static CSensor_profile* NewL(LogDb* aLogDb);

  virtual ~CSensor_profile();

  gboolean StartL(GError** error);

  void Stop();

  gboolean IsActive() { return iNotifyHandler && iIsActive; }

 private:
 
  CSensor_profile(LogDb* aLogDb);

  void ConstructL();

 private: // MProEngProfileActivationObserver

  virtual void HandleProfileActivatedL(TInt aProfileId);

  // The default empty implementation suits us fine.
  //virtual void HandleProfileActivationNotificationError(TInt);

 private: // utils

  HBufC8* GetCurrentProfileNameL();

 private: // property

  LogDb* iLogDb; // not owned

  MProEngEngine* iProfileEngine;

  MProEngNotifyHandler* iNotifyHandler;

  TBool iIsActive;

};

#endif // __PROFILE_ENABLED__

#endif /* __epoc_profile_31_hpp__ */
