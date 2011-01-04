#include "epoc-appfocus.hpp"

#if __APPFOCUS_ENABLED__

#include "application_config.h"
#include "er_errors.h"
#include "sa_sensor_list_log_db.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <apgwgnam.h> // library apgrfx.lib
#include <coedef.h> // for ECoeWinPriorityNeverAtFront

// A useful reference is http://developer.sonyericsson.com/message/99028.

// ----------------------------------------------------------------------------

NONSHARABLE_CLASS(CMyWindowGroup) :
  public CBase
{
 public:

  static CMyWindowGroup* NewL(RWsSession& aWsSession);

  virtual ~CMyWindowGroup();

  RWindowGroup& Ref() { return iWindowGroup; }

 private:

  CMyWindowGroup(RWsSession& aWsSession) : iWsSession(aWsSession), iWindowGroup(aWsSession) {}

  void ConstructL();

 private:

  RWsSession& iWsSession;

  DEF_SESSION(RWindowGroup, iWindowGroup);
};

CMyWindowGroup* CMyWindowGroup::NewL(RWsSession& aWsSession)
{
  CMyWindowGroup* obj = new (ELeave) CMyWindowGroup(aWsSession);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop();
  return obj;
}

void CMyWindowGroup::ConstructL()
{
  TUint32 clientHandle = reinterpret_cast<TUint32>(&iWindowGroup);
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iWindowGroup, iWindowGroup.Construct(clientHandle, EFalse));

  // Hide the icon for this window group. Not sure exactly what it
  // takes to do this, but all of this (in this order) at least
  // appears to work. People have been having trouble making this
  // happen, it seems. Probably they get this right with the PyS60
  // keycapture module, but have not checked.
  CApaWindowGroupName* wgName = CApaWindowGroupName::NewLC(iWsSession, clientHandle);
  wgName->SetHidden(ETrue);
  CleanupStack::PopAndDestroy(); // wgName

  iWindowGroup.EnableReceiptOfFocus(EFalse);
  iWindowGroup.SetOrdinalPosition(-1, ECoeWinPriorityNeverAtFront); // hide the window group
  iWindowGroup.DefaultOwningWindow();
}

CMyWindowGroup::~CMyWindowGroup()
{
  SESSION_CLOSE_IF_OPEN(iWindowGroup);
}

// ----------------------------------------------------------------------------

CSensor_appfocus* CSensor_appfocus::NewL(LogDb* aLogDb)
{
  CSensor_appfocus* obj = new (ELeave) CSensor_appfocus(aLogDb);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop();
  return obj;
}

CSensor_appfocus::CSensor_appfocus(LogDb* aLogDb) : 
  CActiveRunG(EPriorityStandard)
{
  iLogDb = aLogDb;
  CActiveScheduler::Add(this);
}

void CSensor_appfocus::ConstructL()
{
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iWsSession, iWsSession.Connect());

  // It seems the RWsSession passed to RWindowGroup must be open, so
  // we require some contortions here.
  iMyWindowGroup = CMyWindowGroup::NewL(iWsSession);
}

CSensor_appfocus::~CSensor_appfocus()
{
  Cancel(); // safe when AO inactive as DoCancel not called
  delete iMyWindowGroup;
  SESSION_CLOSE_IF_OPEN(iWsSession);
}

gboolean CSensor_appfocus::StartL(GError** error)
{
  if (!iFocusChangeEventsEnabled) {
    // I do not believe it's dangerous to leave this enabled, if say
    // there is a scanning error and we cannot continue processing
    // events. We do not invoke EventReady, and so the event delivery
    // should skip us. I hope.
    User::LeaveIfError(iMyWindowGroup->Ref().EnableFocusChangeEvents());
    iFocusChangeEventsEnabled = ETrue;
  }
  if (!IsActive()) {
    MakeRequest();
    //log_db_log_status(iLogDb, NULL, "appfocus sensor started");
  }
  return TRUE;
}

void CSensor_appfocus::Stop()
{
  if (IsActive()) {
    Cancel();
    //log_db_log_status(iLogDb, NULL, "appfocus sensor stopped");
  }

  if (iFocusChangeEventsEnabled) {
    // Okay to call even if not enabled.
    iMyWindowGroup->Ref().DisableFocusChangeEvents();
    iFocusChangeEventsEnabled = EFalse;
  }
}

void CSensor_appfocus::MakeRequest()
{
  // I guess we do actually require our very own window server session
  // here.
  iWsSession.EventReady(&iStatus);

  SetActive();
}

gboolean CSensor_appfocus::RunGL(GError** error)
{
  assert_error_unset(error);

  TInt errCode = iStatus.Int();

  //logg("appfocus event %d", errCode);

  //Leave(gx_error_no_memory); // test

  if (errCode) {
    // This error really should not occur, but since it has, we will
    // simply stop this one scanner. For a retry, someone just call
    // StartL.
    goto fail;
  } else {
    // Must do a GetEvent before doing another EventReady.
    TWsEvent event;
    iWsSession.GetEvent(event);

    TInt wgid = iWsSession.GetFocusWindowGroup();
    CApaWindowGroupName* gn = NULL;
    TRAP(errCode, gn = CApaWindowGroupName::NewL(iWsSession, wgid));
    if (errCode) {
      goto fail;
    }
    TUid uid = gn->AppUid(); // the uid of the focused app
    TInt32 appUid = uid.iUid;
    sqlite3_int64 appUid64 = (sqlite3_int64)((TUint32)appUid);
    TPtrC nameDes = gn->Caption();
    gchar appName[32+1]; // should be big enough for informative logging
    ConvToUtf8CString(appName, 32, nameDes);
    delete gn;
    
    guilogf("appfocus: '%s' 0x%08x", appName, appUid);

    if (!log_db_log_appfocus(iLogDb, appUid64, appName, error)) {
      return FALSE;
    }

    MakeRequest();
  }

  return TRUE;

 fail:
  if (!log_db_log_status(iLogDb, error, "INACTIVATE: appfocus: failure reading sensor: %s (%d)", plat_error_strerror(errCode), errCode)) {
    // Logging failing is quite severe. We shall report the error
    // upwards, where the framework will hopefully take corrective
    // action, and probably then call either our StartL or dtor,
    // depending on whether recovery was possible.
    return FALSE;
  }

  return TRUE;
}

const char* CSensor_appfocus::Description()
{
  return "appfocus";
}

void CSensor_appfocus::DoCancel()
{
  iWsSession.EventReadyCancel();
}

#endif // __APPFOCUS_ENABLED__

/**

epoc-appfocus.cpp

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
