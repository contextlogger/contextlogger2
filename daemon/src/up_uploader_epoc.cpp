#include "up_private.h"

#if __FEATURE_UPLOADER__

#include "up_poster_epoc.hpp"

#include "ac_app_context.h"
#include "cf_query.h"
#include "epoc-iap.h"
#include "er_errors.h"
#include "log-db.h"
#include "timer_generic_epoc.h"
#include "utils_cl2.h"

#include "moment_parser.h"

#include "common/assertions.h"
#include "common/epoc-time.h"
#include "common/error_list.h"
#include "common/logging-stack.h"
#include "common/logging-time.h"
#include "common/logging.h"
#include "common/platform_error.h"

#include <e32base.h>
#include <e32std.h>
#include <utf.h>

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

// --------------------------------------------------
// controller class
// --------------------------------------------------

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CUploader" ;; name
 "LogDb* aLogDb" ;; args
 "iLogDb(aLogDb)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CUploader  \
public: static CUploader* NewLC(LogDb* aLogDb); \
public: static CUploader* NewL(LogDb* aLogDb); \
private: CUploader(LogDb* aLogDb); \
private: void ConstructL();

#define CTOR_IMPL_CUploader  \
CUploader* CUploader::NewLC(LogDb* aLogDb) \
{ \
  CUploader* obj = new (ELeave) CUploader(aLogDb); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CUploader* CUploader::NewL(LogDb* aLogDb) \
{ \
  CUploader* obj = CUploader::NewLC(aLogDb); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CUploader::CUploader(LogDb* aLogDb) : iLogDb(aLogDb) \
{}
/***end***/

// Controls a timer AO (based on RTimer) and a "poster" AO (based on
// RHTTPSession). Our goal here is to have enough state that we can at
// any given time inspect that state and decide what to do next. There
// are essentially two strands of execution as well, one for the
// snapshot timing, and one for the upload control.
// 
// Initially, we start the snapshot timer if there is any future time.
// When the time is reached, we flag it as reached. Whenever a
// snapshot is actually taken, we clear the reached flag, and set a
// new timer as appropriate. The snapshot reached flag is also marked
// when explicitly requested via the API.
// 
// Initially, we also check if there are any old files. If so, we keep
// uploading them one by one until exhausted. We set the no more old
// files flag at that point. If an upload fails, we wait for a bit
// before a retry. When there are no more old files, we only do
// uploads after a snapshot has been taken. A snapshot is never taken
// while there are old files remaining.
NONSHARABLE_CLASS(CUploader) : 
  public CBase,
  public MTimerObserver,
  public MPosterObserver
{
  CTOR_DECL_CUploader;
  
 public:
  ~CUploader();

  void RefreshIap(TBool aNotInitial);
  void RefreshSnapshotTimeExpr(TBool aNotInitial);

  void RequestSnapshot();

 private: // MTimerObserver
  void HandleTimerEvent(CTimerAo* aTimerAo, TInt errCode);

 private: // MPosterObserver
  void PosterEvent(TInt anError);

 private: // methods
  void Inactivate();
  void StateChanged();
  void StateChangedL();
  void NextOldFileL();
  TInt CreatePosterAo();
  void DestroyPosterAo();
  TBool PosterAoIsActive() { return (iPosterAo && iPosterAo->IsActive()); }
  void HandleCommsError(TInt errCode);
  void PostNowL();
  void SetPostTimer();
  void SetSnapshotTimerL();
  void TakeSnapshotNowL();
  void FatalError(TInt anError);

 private: // property

  LogDb* iLogDb; // not owned

  TBool iNoConfig; // no upload URL
  TPtrC8 iUploadUrl; // data not owned
  TUint32 iIapId;

  //// posting state
  CPosterAo* iPosterAo;
  CTimerAo* iPostTimerAo;
  gchar* iFileToPost; // pathname of file to upload
  TBool iNoOldFiles; // getNextOldLogFile found nothing
  TInt iNumPostFailures; // affects retry timing

  //// snapshot taking state
  CTimerAo* iSnapshotTimerAo;
  TBool iSnapshotTimePassed;
  gchar* iSnapshotTimeExpr;
  time_t iSnapshotTimeCtx;
  TBool iNoNextSnapshotTime;

  //// blackboard
 public:
  void Set_uploads_allowed(TBool val);
 private:
  TBool i_uploads_allowed; // from blackboard
  bb_Closure iClosure;
  void BbRegisterL();
  void BbUnregister();

};

CTOR_IMPL_CUploader;

static void DataChanged(bb_Blackboard* bb, enum bb_DataType dt,
			gpointer data, int len, gpointer arg)
{
  (void)dt;
  (void)len;
  CUploader* self = (CUploader*)arg;
  bb_Board* bd = bb_Blackboard_board(bb);
  TBool val = bd->uploads_allowed;
  self->Set_uploads_allowed(val);
}

void CUploader::Set_uploads_allowed(TBool val)
{
  i_uploads_allowed = val;
  StateChanged();
}

void CUploader::BbRegisterL()
{
  // Initial value (internal).
  bb_Blackboard* bb = ac_global_Blackboard;
  bb_Board* bd = bb_Blackboard_board(bb);
  i_uploads_allowed = bd->uploads_allowed;

  // Closure init.
  iClosure.changed = DataChanged;
  iClosure.arg = this;

  // Registration proper.
  if (!bb_Blackboard_register(bb,
			      bb_dt_uploads_allowed,
			      iClosure, NULL))
    User::LeaveNoMemory();
}

void CUploader::BbUnregister()
{
  bb_Blackboard_unregister(ac_global_Blackboard, iClosure);
}

// The effect is not immediate. Will only take effect when the next
// poster is created.
// 
// We do logging here to make it possible to find out if the setting
// change succeeded.
void CUploader::RefreshIap(TBool aNotInitial)
{
  // TUint32 coercion hopefully okay.
  iIapId = (TUint32)get_config_iap_id();
  if (aNotInitial)
    log_db_log_status(iLogDb, NULL, "Uploader IAP changed to %d", iIapId);
}

void CUploader::RefreshSnapshotTimeExpr(TBool aNotInitial)
{
  GError* localError = NULL;
  gchar* newOne = NULL;
  if (!get_ConfigDb_str("uploader.time_expr", 
			&newOne, __UPLOAD_TIME_EXPR__, 
			&localError)) {
    if (aNotInitial)
      gx_dblog_error_free_check(iLogDb, localError, NULL);
    else
      gx_error_free(localError);
  } else {
    assert(newOne != NULL);
    g_free(iSnapshotTimeExpr);
    iSnapshotTimeExpr = newOne;
    logt("got time expression");
    logt(iSnapshotTimeExpr);
    if (aNotInitial) {
      log_db_log_status(iLogDb, NULL, "Upload time expression set to '%s'",
			newOne);

      // We may need to recompute the next snapshot time based on the
      // new expression. But if a previosly set time has already
      // passed, then that is not affected by the expression change.
      // What is past is past.
      {
	iSnapshotTimerAo->Cancel();
	iNoNextSnapshotTime = EFalse;
	StateChanged();
      }
    }
  }
}

void CUploader::ConstructL()
{
  const gchar* upload_url = ac_STATIC_GET(upload_url);
  if (!upload_url) {
    iNoConfig = ETrue;
    logt("uploads disabled: no upload URL");
  } else {
    iUploadUrl.Set((TUint8*)upload_url, strlen(upload_url)); 
    logf("upload URL: %s", upload_url);
  }

  RefreshIap(EFalse);
  logf("uploader using IAP %d", iIapId);

  // Ensure that uploads directory exists.
  GError* mdError = NULL;
  if (!mkdir_p(LOG_UPLOADS_DIR, &mdError)) {
    gx_txtlog_error_free(mdError);
    User::Leave(KErrGeneral);
  }

  //CreatePosterAo();

#if 0
  // Test code with a single part post. Just to see if can actually connect somewhere.
  if (iPosterAo) {
    _LIT8(KRequestBody, "Hello World!");
    iPosterAo->PostBufferL(iUploadUrl, KRequestBody);
  }
#endif
#if 0
  // Test code with a manually constructed multi part post.
  if (iPosterAo) {
    _LIT8(boundary, "-----AaB03xeql7dsxeql7ds");
    _LIT8(KRequestBody, "-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"metadata\"; filename=\"metadata.json\"\r\nContent-Type: application/json; charset=UTF-8\r\n\r\n{\"log filename\": \"test_log.db\", \"time\": {\"timezone\": -7200, \"daylight\": true, \"altzone\": -10800, \"time\": 1247093454.8903401}}\r\n-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"logdata\"; filename=\"test_log.db\"\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\n\r\nHello World!\r\n-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"logdata_submit\"\r\n\r\nUpload\r\n-------AaB03xeql7dsxeql7ds--\r\n");
    iPosterAo->PostMultiPartBufferL(iUploadUrl, boundary, KRequestBody);
  }
#endif
#if 0
  // Test code with a file based post.
  if (iPosterAo) {
    _LIT(KFileName, "e:\\data\\atwink.png");
    iPosterAo->PostFileL(iUploadUrl, KFileName);
  }
#endif
  iPostTimerAo = CTimerAo::NewL(*this, CActive::EPriorityStandard);
  iSnapshotTimerAo = CTimerAo::NewL(*this, CActive::EPriorityStandard);
  RefreshSnapshotTimeExpr(EFalse);
  iSnapshotTimeCtx = time(NULL); //xxx needs to come from ConfigDb -- but actually ones the next time is computed by a Lua expression, that expression can contain any required fixpoint as a constant
  if (iSnapshotTimeCtx == -1) User::Leave(KErrGeneral);
  //logf("using snapshot time '%s'", iSnapshotTimeExpr);

  // Note that if this ConstructL() leaves, the dtor of this will
  // unregister us.
  BbRegisterL();

  StateChangedL();
}

CUploader::~CUploader()
{
  BbUnregister();
  delete iPostTimerAo;
  delete iSnapshotTimerAo;
  DestroyPosterAo();
  g_free(iFileToPost); // safe when NULL
  g_free(iSnapshotTimeExpr); // safe when NULL
}

void CUploader::Inactivate()
{
  if (iPosterAo) iPosterAo->Cancel();
  if (iSnapshotTimerAo) iSnapshotTimerAo->Cancel();
  if (iPostTimerAo) iPostTimerAo->Cancel();
}

void CUploader::FatalError(TInt errCode)
{
  Inactivate();
  ex_dblog_fatal_error(iLogDb, errCode);
}

void CUploader::StateChanged()
{
  TRAPD(errCode, StateChangedL());
  if (errCode)
    FatalError(errCode);
}

void CUploader::NextOldFileL()
{
  if (iNoOldFiles) return;

  g_free(iFileToPost); // safe when NULL
  iFileToPost = NULL;

  GError* error = NULL;
  if (getNextOldLogFile(&iFileToPost, &error)) {
    if (iFileToPost) {
      return;
    } else {
      iNoOldFiles = ETrue;
      logt("no more old files to upload");
    }
  } else {
    // A leave here is considered fatal regardless of the error.
    gx_dblog_error_free(iLogDb, error);
    User::Leave(KErrGeneral);
  }
}

// External API.
void CUploader::RequestSnapshot()
{
  iSnapshotTimePassed = ETrue;
  iSnapshotTimerAo->Cancel();
  StateChanged();
}

// Computes next snapshot time (if any), and sets a timer for it as
// appropriate.
void CUploader::SetSnapshotTimerL()
{
  assert(!iSnapshotTimePassed);
  iSnapshotTimerAo->Cancel();
  if (iNoNextSnapshotTime) return; // flag to avoid needless computation
  time_t now = time(NULL);
  if (now == -1)
    User::Leave(KErrGeneral);
  time_t ctx = iSnapshotTimeCtx;
  time_t snaptime;
  GError* parseError = NULL;
  if (!parse_moment(iSnapshotTimeExpr, ctx, now, &snaptime, &parseError)) {
    gx_txtlog_error_free(parseError);
    iNoNextSnapshotTime = ETrue;
    return;
  }
  if (!snaptime) {
    logt("no snapshot time upcoming");
    iNoNextSnapshotTime = ETrue;
    return;
  }
  logt("next snapshot time computed");
  log_time(snaptime);
  // Note that RTimer At and AtUTC often return with a KErrAbort in
  // current phones. Still, here we definitely want to be using
  // absolute times. http://www.newlc.com/en/topic-5076
  // http://wiki.forum.nokia.com/index.php/TSS000261_-_Timer_issues_and_tips
  TTime epocTime;
#if 1
  UnixTimeToUtcEpocTime(epocTime, snaptime);
  iSnapshotTimerAo->AtUTC(epocTime);
#else
  UnixTimeToLocalEpocTime(epocTime, snaptime);
  iSnapshotTimerAo->At(epocTime);
#endif
}

void CUploader::SetPostTimer()
{
  assert(iNumPostFailures > 0);

  iPostTimerAo->Cancel();

  // Roughly num_failures * 5 mins.     xxx perhaps this should be computed by a Lua function coming from ConfigDb
  int secs = 5 * 60 * iNumPostFailures + (rand() % 60);
  TTimeIntervalMicroSeconds32 interval = SecsToUsecs(secs);
  logf("retrying upload in %d secs / %d usecs", secs, interval.Int());

  // Note that these timers should not complete with KErrAbort, since
  // a wait for an interval should not be affected by a system time
  // change.
  iPostTimerAo->After(interval);
}

// Make sure this method does not leave.
void CUploader::HandleTimerEvent(CTimerAo* aTimerAo, TInt errCode)
{
  logf("timer event (%d)", errCode);

  if (errCode == KErrAbort) {
    // System time changed. We should recompute the snapshot time, as
    // this may not be just a time zone change, but also the UTC time
    // may have been changed. Normally though the changes are trivial,
    // and frequent changes are often caused by network-based time
    // updates.
    logt("system time changed");
    assert(aTimerAo == iSnapshotTimerAo); // should be only for At timers
    iNoNextSnapshotTime = EFalse; // not sure of this anymore
    StateChanged(); // go recompute the time and set the timer again
    return;
  }

  if (errCode == KErrUnderflow) {
    // This is quite possible. We might compute a time that is a
    // fraction of a second later, and by the time we got to asking
    // for a timer event the time might have just passed. This is
    // okay.
    assert(aTimerAo == iSnapshotTimerAo); // our After intervals should not be negative
    logt("expiration time in the past");
    errCode = KErrNone;
  }

  if (errCode) {
    FatalError(errCode);
  } else if (aTimerAo == iPostTimerAo) {
    logt("was posting timer");
    StateChanged();
  } else if (aTimerAo == iSnapshotTimerAo) {
    logt("was snapshot timer");
    iSnapshotTimePassed = ETrue;
    StateChanged();
  } else {
    assert(0);
  }
}

void CUploader::PosterEvent(TInt anError)
{
  logf("poster reports %d", anError);

  switch (anError)
    {
    case POSTER_SUCCESS:
      {
	GError* localError = NULL;
	if (!rm_file(iFileToPost, &localError)) {
	  gx_txtlog_error_free(localError);
	  FatalError(KErrGeneral);
	} else {
	  logf("posted log file '%s'", iFileToPost);
	  iNumPostFailures = 0;
	  iFileToPost = NULL;

	  {
	    time_t t = time(NULL);
	    if (t == -1) {
	      px_dblog_fatal_errno(iLogDb);
	      return;
	    }
	    ac_global_Registry->last_upload_time = t;
	  }

	  StateChanged();
	}
        break;
      }
    case POSTER_TRANSIENT_FAILURE:
      {
	// Retry later.
	iNumPostFailures++;
	SetPostTimer();
        break;
      }
    case POSTER_PERMANENT_FAILURE:
      {
        // We must be doing something wrong. Better stop altogether,
        // barring external intervention.
	logt("inactivating uploader due to a permanent posting failure");
	Inactivate();
        break;
      }
    default: // Symbian error
      {
        assert(anError < 0);
	HandleCommsError(anError);
        break;
      }
    }
}

// Called to handle poster creation and poster request errors.
void CUploader::HandleCommsError(TInt anError)
{
  // Some errors are more severe than others.
  switch (anError)
    {   
      // Some errors warrant the recreation of iPosterAo (if we even
      // have one).
    case KErrCouldNotConnect:
    case KErrDisconnected:
    case KErrCommsLineFail:
    case KErrCommsFrame:
    case KErrCommsOverrun:
    case KErrCommsParity:
    case KErrInUse: // file in use maybe
    case KErrNotReady: // -18 ("A device required by an I/O operation is not ready to start operations.") We have actually gotten this error. Let us have it here to see if it is something transient.
    case KErrServerBusy: // local daemon, such as socket or file server
    case -8268: // not documented, but getting this in flight mode
    case -30180: // not documented, getting this when WLAN specified in IAP is not within range
    case -5120: // no response from DNS server
      {
	DestroyPosterAo();
      } // fall through...

      // For many errors, a retry later is a suitable action.
    case KErrTimedOut:
      {
	// Retry later.
	iNumPostFailures++;
	SetPostTimer();
	break;
      }

      // Some errors are considered fatal.
    case KErrNoMemory: // bad, resume this program later
    case KErrPathNotFound: // our state may be messed up
      {
	FatalError(anError);
	break;
      }

      // Some errors are considered permanently fatal. In such cases
      // the situation likely will not improve by restarting the
      // process.
    case KErrNotSupported: // perhaps uploader cannot function on this device
    case KErrNotFound: // some required resource missing perhaps
    default:
      {
	// We do not yet have logic for handling this kind of
	// error, so inactivate. Future versions may implement
	// this better.
	logf("inactivating uploader due to Symbian error %d", anError);
	Inactivate();
	break;
      }
    } // end switch
}

// Better by careful not to run out of stack calling this recursively.
// Can always use an "ImmediateAo" if necessary to avoid such a risk.
void CUploader::StateChangedL()
{
  if (iNoConfig)
    return;

  if (!iSnapshotTimePassed &&
      !iNoNextSnapshotTime &&
      !iSnapshotTimerAo->IsActive()) {
    SetSnapshotTimerL();
  }

 again:
  if (iFileToPost) {
    if (PosterAoIsActive() || 
	// 'iPostTimerAo' is a post retry timer.
	iPostTimerAo->IsActive())
      return;
    if (i_uploads_allowed) {
      PostNowL(); // not called elsewhere
    } else {
      DestroyPosterAo(); // make sure no connection remains
    }
  } else if (!iNoOldFiles) {
    NextOldFileL();
    goto again;
  } else if (iSnapshotTimePassed) {
    TakeSnapshotNowL(); // not called elsewhere
  } else {
    // Have nothing to post for now. This will see to it that we close
    // any connection that we do not require. With some operators,
    // with no fixed data, merely keeping an RConnection open incurs
    // an hourly charge (minimum charge per hour), and there is a
    // battery hit as well, and possible interference with phone calls
    // and such.
    DestroyPosterAo();
  }
}

TInt CUploader::CreatePosterAo()
{
  assert(!iPosterAo);

  TRAPD(errCode, iPosterAo = CPosterAo::NewL(*this, iIapId));
  if (errCode) {
    logf("poster creation failed with %d", errCode);
  }

  return errCode;
}

void CUploader::DestroyPosterAo()
{
  if (iPosterAo) {
    delete iPosterAo;
    iPosterAo = NULL;
    logt("poster destroyed");
  }
}

void CUploader::PostNowL()
{
  // For error handling testing.
#if 0
  logt("horrible test error");
  User::Leave(KErrTotalLossOfPrecision);
#endif

  logt("trying to post file now");

  if (!iPosterAo) {
    TInt errCode = CreatePosterAo();
    if (errCode) {
      HandleCommsError(errCode);
      return;
    }
  }

  TPtrC8 fileName((TUint8*)iFileToPost);
  TFileName fileNameDes;
  // Our names should all be ASCII, so this may be overkill.
  User::LeaveIfError(CnvUtfConverter::ConvertToUnicodeFromUtf8(fileNameDes, fileName));
  logf("asking poster to post '%s'", iFileToPost);
  iPosterAo->PostFileL(iUploadUrl, fileNameDes);
}

void CUploader::TakeSnapshotNowL()
{
  logt("taking snapshot now");

  // LOG_UPLOADS_DIR and LOGDB_DIR must be on the same device to allow
  // for renaming rather than copying.
  char* pathname = tempnam(LOG_UPLOADS_DIR, "log_"); // caller must free 'pathname'
  if (!pathname) {
    logf("failure in tempnam: %s (%d)", strerror(errno), errno);
    User::Leave(KErrGeneral);
  }
  
  gboolean wasRenamed = FALSE;
  GError* snapError = NULL;
  if (!log_db_take_snapshot(iLogDb, pathname, &wasRenamed, &snapError)) {
    logf("failure taking snapshot to file '%s'", pathname);
    free(pathname);
    logf("snapshot file was%s created", wasRenamed ? "" : " not");
    gx_txtlog_error_free(snapError);
    User::Leave(KErrGeneral);
  }

  iFileToPost = pathname;
  iSnapshotTimePassed = EFalse;
  StateChangedL();
}

// --------------------------------------------------
// global system state
// --------------------------------------------------

EXTERN_C gboolean up_global_init(GError** error)
{
  // Nothing to do.
  return TRUE;
}

EXTERN_C void up_global_cleanup()
{
  // Nothing to do.
}

// --------------------------------------------------
// API
// --------------------------------------------------

EXTERN_C gboolean up_Uploader_upload_now(up_Uploader* object, GError** error)
{
  ((CUploader*)object)->RequestSnapshot();
  return TRUE;
}

EXTERN_C gboolean up_Uploader_reconfigure(up_Uploader* object,
					  const gchar* key,
					  const gchar* value, 
					  GError** error)
{
  if (strcmp(key, "iap") == 0) {
    ((CUploader*)object)->RefreshIap(ETrue);
  } else if (strcmp(key, "uploader.time_expr") == 0) {
    ((CUploader*)object)->RefreshSnapshotTimeExpr(ETrue);
  }
  return TRUE;
}

EXTERN_C up_Uploader* up_Uploader_new(LogDb* logDb, GError** error)
{
  CUploader* object = NULL;
  TRAPD(errCode, object = CUploader::NewL(logDb));
  if (errCode) {
    if (error)
      *error = gx_error_new(domain_symbian, errCode, "Uploader init failure: %s (%d)", plat_error_strerror(errCode), errCode);
    return NULL;
  }
  return (up_Uploader*)object;
}

EXTERN_C void up_Uploader_destroy(up_Uploader* object)
{
  delete ((CUploader*)object);
}

#endif // __FEATURE_UPLOADER__

/**

up_uploader_epoc.cpp

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
