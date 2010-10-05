/**
 * ====================================================================
 * capturer.h
 * Copyright (c) 2006 Nokia Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ====================================================================
 */

// A small amount of the code is derived from CCapturer in PyS60,
// hence the above license applies. Any changes made are Copyright
// 2009 Helsinki Institute for Information Technology (HIIT) and Tero
// Hasu <tero.hasu@hut.fi>, and are made available under the original
// license.

// Note that this keylogging implementation may interact badly with
// some built-in functions on some devices. If you are going to have
// the Jaiku keyevents anim DLL available, you may use the alternative
// implementation, just define __HAVE_ANIM__ as true.

#ifndef __epoc_keypress_hpp__
#define __epoc_keypress_hpp__

#include "application_config.h"

#if __KEYPRESS_ENABLED__ && !__HAVE_ANIM__

#include "epoc-ao-gerror.hpp"
#include "ld_log_db.h"
#include "utils_cl2.h"

#include <w32std.h> // link against: ws32.lib
#include <e32base.h>
#include <apgwgnam.h>
#include <coedef.h>

#include <glib.h>

// The implementation bears some similarity to that of the appfocus
// sensor, since we must deal with window groups and the like.
NONSHARABLE_CLASS(CSensor_keypress) : public CActiveRunG
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
  void MakeRequest(); // was StartCapturing

  void RequestAllKeys();
  void CancelAllKeys();

  // we want to capture all, do we need these?
  TInt32 SetKeyToBeCaptured(TInt32 keyCode);
  void RemoveKey(TInt32 keyId);
  
  virtual gboolean RunGL(GError** error);

  void DoCancel();

  virtual const char* Description();

  gboolean LogAndClear(GError** error);

 private:

  LogDb* iLogDb; // not owned

  // Not quite sure if we could share some of these with the appfocus
  // sensor. The issue of course being that it may not be possible to
  // make multiple simultaneous async requests via the same session
  // object.
  RWsSession* iSession;
  RWindowGroup* iWinGroup;
  CApaWindowGroupName* iWinGroupName;

  TInt32 iCaptureHandles[350];
  int iNumCaptureHandles;

#define MAX_NUM_CAPTURED_KEYS 100
  // We should log and clear when the buffer is full, or when the
  // logger is stopped, whichever comes first.
  time_t iCapturedKeys[MAX_NUM_CAPTURED_KEYS];
  int iNumCapturedKeys;
  GString* iKeysText; // buffer for text to log
};

#endif // __KEYPRESS_ENABLED__ && !__HAVE_ANIM__

#endif /* __epoc_keypress_hpp__ */
