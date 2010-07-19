#include "epoc-keypress-anim.hpp"

#if __KEYPRESS_ENABLED__ && __HAVE_ANIM__

#include "log-db-logging.h"
#include "sa_sensor_list_log_db.h"

#include "common/assertions.h"
#include "er_errors.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <errno.h>

#define KPropertyCategory TUid::Uid(__UID__)
#define KPropertyKey 1

CSensor_keypress* CSensor_keypress::NewL(LogDb* aLogDb)
{
  CSensor_keypress* self = new (ELeave) CSensor_keypress(aLogDb);
  CleanupStack::PushL(self);
  self->ConstructL();
  CleanupStack::Pop(self);
  return self;
};

CSensor_keypress::CSensor_keypress(LogDb* aLogDb) :
  CActive(CActive::EPriorityLow)
{
  iLogDb = aLogDb;
  CActiveScheduler::Add(this);
};

void CSensor_keypress::ConstructL()
{
  // The size chosen so that we are not wasteful with space, but
  // neither should require too many reallocations.
  SET_TRAP_OOM(User::LeaveNoMemory());
  iKeysText = g_string_sized_new(20 + MAX_NUM_CAPTURED_KEYS * 2);
  UNSET_TRAP_OOM();
  assert(iKeysText != NULL);

  iSession = new (ELeave) RWsSession();
  
  User::LeaveIfError(iSession->Connect());
  iWinGroup = new (ELeave) RWindowGroup(*iSession);
  iWinGroup->Construct((TUint32)iWinGroup, EFalse);
  
  iWinGroup->SetOrdinalPosition(-1, ECoeWinPriorityNeverAtFront); 
  iWinGroup->EnableReceiptOfFocus(EFalse);

  iWinGroupName = CApaWindowGroupName::NewL(*iSession);
  iWinGroupName->SetHidden(ETrue); // Hide from tasklist.
  iWinGroupName->SetWindowGroupName(*iWinGroup);

  TUid cat(KPropertyCategory);
  iKeyEventsClient = CKeyEventsClient::NewL(cat, KPropertyKey,
					    iSession,
					    iWinGroup);

  // It does not look like iKeyEventsClient owns the property, so we
  // shall be the ones to close it.
  iKeyEventsClient->OpenNotificationPropertyL(&iProperty);
  SET_SESSION_OPEN(iProperty);
};

CSensor_keypress::~CSensor_keypress()
{
  Cancel();
  LogAndClear(NULL);
  delete iKeyEventsClient;
  SESSION_CLOSE_IF_OPEN(iProperty);
  delete iWinGroupName;
  if(iWinGroup){
    iWinGroup->Close();
    delete iWinGroup;
  }
  if(iSession){
    iSession->Close();
    delete iSession;
  }
  if (iKeysText) {
    g_string_free(iKeysText, TRUE);
  }
};

gboolean CSensor_keypress::StartL(GError** error)
{
  if (!IsActive()) {
    MakeRequest();
    log_db_log_status(iLogDb, NULL, "keypress sensor started");
  }
  return TRUE;
}

// Must be callable from the dtor, without ContructL necessarily
// having succeeded.
void CSensor_keypress::Stop()
{
  if (IsActive()) {
    Cancel();
    log_db_log_status(iLogDb, NULL, "keypress sensor stopped");
  }
  LogAndClear(NULL); // best effort
}

void CSensor_keypress::MakeRequest()
{
  iProperty.Subscribe(iStatus);
  SetActive();
}

void CSensor_keypress::DoCancel()
{
  assert(IS_SESSION_OPEN(iProperty));
  iProperty.Cancel();
}

void CSensor_keypress::RunL()
{  
  TInt errCode = iStatus.Int();
  //logf("keypress (anim) RunL error %d", errCode);

  if (errCode) {
    // This error really should not occur, but since it has, we will
    // simply log the error and inactivate this scanner.
    log_db_log_status(iLogDb, NULL, "INACTIVATE: keypress: failure reading sensor: %s (%d)", plat_error_strerror(errCode), errCode);
    Stop();
  } else {
    TInt eventCount;
    errCode = iProperty.Get(KPropertyCategory, KPropertyKey, eventCount);
    if (errCode) {
      log_db_log_status(iLogDb, NULL, "INACTIVATE: keypress: RProperty access error: %s (%d)", plat_error_strerror(errCode), errCode);
      Stop();
    } else {
      //logf("keypress: eventCount is %d", eventCount);
      { // record event time
	time_t now = time(NULL);
	if (now == -1) {
	  px_dblog_fatal_errno(iLogDb);
	  return;
	} else {
	  iCapturedKeys[iNumCapturedKeys] = now;
	  iNumCapturedKeys++;
	  if (iNumCapturedKeys == MAX_NUM_CAPTURED_KEYS) {
	    GError* localError = NULL;
	    if (!LogAndClear(&localError)) {
	      gx_dblog_fatal_error_clear(iLogDb, &localError);
	      return;
	    }
	  }
	}
      }
      MakeRequest();
    }
  }
}

// Must be callable from the dtor, without ContructL necessarily
// having succeeded.
gboolean CSensor_keypress::LogAndClear(GError** error)
{
  if (iNumCapturedKeys > 0) {
    assert(iKeysText);
    time_t base = iCapturedKeys[0];
    SET_TRAP_OOM(goto nomemory);
    g_string_set_size(iKeysText, 0);
    g_string_append_printf(iKeysText, "{base: %d, times: [", base);
    int i = 0;
    while (i < iNumCapturedKeys) {
      if (i != 0)
	g_string_append(iKeysText, ", ");
      g_string_append_printf(iKeysText, "%d", iCapturedKeys[i] - base);
      i++;
    }
    g_string_append(iKeysText, "]}");
    UNSET_TRAP_OOM();
    iNumCapturedKeys = 0;

    if (!log_db_log_keypress(iLogDb, iKeysText->str, error)) {
      return FALSE;
    }
  }
  return TRUE;

 nomemory:
  if (error) *error = gx_error_no_memory;
  return FALSE;
}

#endif // __KEYPRESS_ENABLED__ && __HAVE_ANIM__

/**

epoc-keypress-anim.cpp

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
