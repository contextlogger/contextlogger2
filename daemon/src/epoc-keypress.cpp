/**
 * ====================================================================
 * capturer.cpp
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

/*
 References:

 * http://mikie.iki.fi/symbian/keycapture.html

 * http://stackoverflow.com/questions/243504/capture-keystrokes-on-symbian-os

 */
 
#include "epoc-keypress.hpp"

#if __KEYPRESS_ENABLED__

#include "epoc-key-codes.hpp"

#include "common/assertions.h"
#include "application_config.h"
#include "er_errors.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "sa_sensor_list_log_db.h"
#include "common/utilities.h"

CSensor_keypress* CSensor_keypress::NewL(LogDb* aLogDb)
{
  CSensor_keypress* self = new (ELeave) CSensor_keypress(aLogDb);
  CleanupStack::PushL(self);
  self->ConstructL();
  CleanupStack::Pop(self);
  return self;
};

CSensor_keypress::CSensor_keypress(LogDb* aLogDb) :
  CActiveRunG(CActive::EPriorityLow)
{
  iLogDb = aLogDb;
  CActiveScheduler::Add(this);
};

static const int allKeyCodes[] = SELECT_KEY_CODES;

void CSensor_keypress::ConstructL()
{
  // The size chosen so that we are not wasteful with space, but
  // neither should require too many reallocations.
  iKeysText = g_string_sized_new(20 + MAX_NUM_CAPTURED_KEYS * 2);
  User::LeaveIfNull(iKeysText);

  iSession = new (ELeave) RWsSession();
  
  User::LeaveIfError(iSession->Connect());
  iWinGroup = new (ELeave) RWindowGroup(*iSession);
  iWinGroup->Construct((TUint32)iWinGroup, EFalse);
  
  iWinGroup->SetOrdinalPosition(-1, ECoeWinPriorityNeverAtFront); 
  iWinGroup->EnableReceiptOfFocus(EFalse);
  
  iWinGroupName = CApaWindowGroupName::NewL(*iSession);
  iWinGroupName->SetHidden(ETrue); // Hide from tasklist.
  iWinGroupName->SetWindowGroupName(*iWinGroup);
};

void CSensor_keypress::RequestAllKeys()
{
  const int* code = &allKeyCodes[0];
  iNumCaptureHandles = 0;
  while (*code) {
    //logf("asking for key code %d", (*code));
    iCaptureHandles[iNumCaptureHandles] = SetKeyToBeCaptured(*code);
    code++;
    iNumCaptureHandles++;
  }
  //logt("done asking");
}

// This sensor is particularly delicate, since we are eating
// keypresses. Failing to forward them is a problem.
void CSensor_keypress::CancelAllKeys()
{
  while (iNumCaptureHandles > 0) {
    TInt32 handle = iCaptureHandles[iNumCaptureHandles];
    if (handle >= 0)
      RemoveKey(handle);
    iNumCaptureHandles--;
  }
}

// Negative return value indicates an error.
//
// e32keys.h defines some of the possible values in TKeyCode, but that's only the special ones, so potentially there are lots.
TInt32 CSensor_keypress::SetKeyToBeCaptured(TInt32 keyCode)
{
  TInt32 captureHandle;
  // Negative values indicate error, other values are capture handles,
  // of use when cancelling key capture for the same key.
  captureHandle = iWinGroup->CaptureKey(keyCode,0,0);
  return captureHandle;
};

void CSensor_keypress::RemoveKey(TInt32 captureHandle)
{
  iWinGroup->CancelCaptureKey(captureHandle);
};

gboolean CSensor_keypress::StartL(GError** error)
{
  if (!IsActive()) {
    RequestAllKeys();
    MakeRequest();
    log_db_log_status(iLogDb, NULL, "keypress sensor started");
  }
  return TRUE;
}

void CSensor_keypress::Stop()
{
  if (IsActive()) {
    Cancel();
    log_db_log_status(iLogDb, NULL, "keypress sensor stopped");
  }
  LogAndClear(NULL); // best effort, already being stopped
}

void CSensor_keypress::MakeRequest()
{
  iSession->EventReady(&iStatus);
  SetActive();
};

void CSensor_keypress::DoCancel()
{
  iSession->EventReadyCancel();
  CancelAllKeys();
};

CSensor_keypress::~CSensor_keypress()
{
  Cancel();
  LogAndClear(NULL);
  if(iWinGroup){
    iWinGroup->Close();
    delete iWinGroup;
  }
  delete iWinGroupName;
  if(iSession){
    iSession->Close();
    delete iSession;
  }
  g_string_free(iKeysText, TRUE);
};

gboolean CSensor_keypress::RunGL(GError** error)
{  
  assert_error_unset(error);

  TInt errCode = iStatus.Int();

  //logf("keypress event %d", errCode);

  if (errCode) {
    // This error really should not occur, but since it has, we will
    // simply stop this one scanner. For a retry, someone just call
    // StartL.
    log_db_log_status(iLogDb, NULL, "INACTIVATE: keypress: failure reading sensor: %s (%d)", plat_error_strerror(errCode), errCode);
    Stop();
  } else {
    TWsEvent event;
    iSession->GetEvent(event);
    
    //TInt32 lastCapturedKey = event.Key()->iCode;
    //logf("last captured key was %d", lastCapturedKey);

    time_t now = time(NULL); 
    if (now != -1) { // if no error getting time
      iCapturedKeys[iNumCapturedKeys] = now;
      iNumCapturedKeys++;
      if (iNumCapturedKeys == MAX_NUM_CAPTURED_KEYS) {
	if (!LogAndClear(error)) {
	  return FALSE;
	}
      }
    }

    // Forward keypress to original recipient.
    iSession->SendEventToWindowGroup(iSession->GetFocusWindowGroup(), event);

    MakeRequest();
  }

  return TRUE;
};

gboolean CSensor_keypress::LogAndClear(GError** error)
{
  if (iNumCapturedKeys > 0) {
    time_t base = iCapturedKeys[0];
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
    iNumCapturedKeys = 0;

    if (!log_db_log_keypress(iLogDb, iKeysText->str, error)) {
      assert_error_set(error);
      return FALSE;
    }
  }
  return TRUE;
}

const char* CSensor_keypress::Description()
{
  return "keypress";
}

#endif // __KEYPRESS_ENABLED__

