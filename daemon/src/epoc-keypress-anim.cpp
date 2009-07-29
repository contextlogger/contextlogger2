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
  Stop();
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
    logt("keypress sensor started");
  }
  return TRUE;
}

// Must be callable from the dtor, without ContructL necessarily
// having succeeded.
void CSensor_keypress::Stop()
{
  Cancel();
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
      logf("keypress: RProperty access error: %s (%d)", plat_error_strerror(errCode), errCode);
    } else {
      //logf("keypress: eventCount is %d", eventCount);
      { // record event time
	time_t now = time(NULL);
	if (now == -1) {
	  px_db_log_fatal_errno(iLogDb);
	  return;
	} else {
	  iCapturedKeys[iNumCapturedKeys] = now;
	  iNumCapturedKeys++;
	  if (iNumCapturedKeys == MAX_NUM_CAPTURED_KEYS) {
	    GError* localError = NULL;
	    if (!LogAndClear(&localError)) {
	      gx_db_log_clear_fatal_error(iLogDb, &localError);
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
      return FALSE;
    }
  }
  return TRUE;
}

#endif // __KEYPRESS_ENABLED__ && __HAVE_ANIM__
