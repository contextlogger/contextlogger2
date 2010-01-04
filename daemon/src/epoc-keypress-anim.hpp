// This is a variant implementation of the "keypress" sensor,
// utilizing the Jaiku "keyevents" component. Unfortunately the Jaiku
// codebase does not actually appear to have any code that uses the
// component, but hopefully we manage it without an example, and
// hopefully it actually does work.

#ifndef __epoc_keypress_anim_hpp__
#define __epoc_keypress_anim_hpp__

#include "application_config.h"

#if __KEYPRESS_ENABLED__ && __HAVE_ANIM__

#include "log-db.h"
#include "utils_cl2.h"

#include "keyevents/client.h" // third-party library

#include <apgwgnam.h>
#include <coedef.h>
#include <e32base.h>
#include <e32property.h>
#include <w32std.h> // link against: ws32.lib

#include <glib.h>

// The implementation bears some similarity to that of the appfocus
// sensor, since we must deal with window groups and the like.
NONSHARABLE_CLASS(CSensor_keypress) : public CActive
{
 public:
  static CSensor_keypress* NewL(LogDb* aLogDb);

  virtual ~CSensor_keypress();

  gboolean StartL(GError** error);

  void Stop();

 private:
  CSensor_keypress(LogDb* aLogDb);

  void ConstructL();

 private:

  // Makes next capture request.
  void MakeRequest();

  virtual void RunL();

  virtual void DoCancel();

  gboolean LogAndClear(GError** error);

 private:

  LogDb* iLogDb; // not owned

  RWsSession* iSession;
  RWindowGroup* iWinGroup;
  CApaWindowGroupName* iWinGroupName;

  DEF_SESSION(RProperty, iProperty);

  CKeyEventsClient* iKeyEventsClient;

  TInt32 iCaptureHandles[350];
  int iNumCaptureHandles;

#define MAX_NUM_CAPTURED_KEYS 100
  // We should log and clear when the buffer is full, or when the
  // logger is stopped, whichever comes first.
  time_t iCapturedKeys[MAX_NUM_CAPTURED_KEYS];
  int iNumCapturedKeys;
  GString* iKeysText; // buffer for text to log
};

#endif // __KEYPRESS_ENABLED__ && __HAVE_ANIM__

#endif /* __epoc_keypress_anim_hpp__ */

/**

epoc-keypress-anim.hpp

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
