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

// For info on accessing additional profile info on 3rd,
// see http://mikie.iki.fi/wordpress/?p=31

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

/**

epoc-profile.hpp

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
