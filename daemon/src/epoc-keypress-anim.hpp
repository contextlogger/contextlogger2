// This is a variant implementation of the "keypress" sensor,
// utilizing the Jaiku "keyevents" component. Unfortunately the Jaiku
// codebase does not actually appeat to have any code that uses the
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
