#include "up_private.h"

#if __FEATURE_UPLOADER__

#include "up_poster_epoc.hpp"

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

  ///xxx reconfigure api

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
  void PostNowL();
  void SetPostTimer();
  void SetSnapshotTimerL();
  void TakeSnapshotNowL();
  void FatalError(TInt anError);

 private: // property

  LogDb* iLogDb; // not owned

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
};

CTOR_IMPL_CUploader;

_LIT8(KPostUri, __UPLOAD_URL__); // xxx to come from ConfigDb

void CUploader::ConstructL()
{
  //logt("doing uploader init");

  // Ensure that uploads directory exists.
  GError* mdError = NULL;
  if (!mkdir_p(LOG_UPLOADS_DIR, &mdError)) {
    gx_error_log_free(mdError);
    User::Leave(KErrGeneral);
  }

  iPosterAo = CPosterAo::NewL(*this, __IAP_ID__); // xxx IAP ID to come from ConfigDb
  //logt("uploader poster init done");
#if 0
  // Test code with a single part post. Just to see if can actually connect somewhere.
  _LIT8(KRequestBody, "Hello World!");
  iPosterAo->PostBufferL(KPostUri, KRequestBody);
#endif
#if 0
  // Test code with a manually constructed multi part post.
  _LIT8(boundary, "-----AaB03xeql7dsxeql7ds");
  _LIT8(KRequestBody, "-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"metadata\"; filename=\"metadata.json\"\r\nContent-Type: application/json; charset=UTF-8\r\n\r\n{\"log filename\": \"test_log.db\", \"time\": {\"timezone\": -7200, \"daylight\": true, \"altzone\": -10800, \"time\": 1247093454.8903401}}\r\n-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"logdata\"; filename=\"test_log.db\"\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\n\r\nHello World!\r\n-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"logdata_submit\"\r\n\r\nUpload\r\n-------AaB03xeql7dsxeql7ds--\r\n");
  iPosterAo->PostMultiPartBufferL(KPostUri, boundary, KRequestBody);
#endif
#if 0
  // Test code with a file based post.
  _LIT(KFileName, "e:\\data\\atwink.png");
  iPosterAo->PostFileL(KPostUri, KFileName);
#endif
  iPostTimerAo = CTimerAo::NewL(*this, CActive::EPriorityStandard);
  iSnapshotTimerAo = CTimerAo::NewL(*this, CActive::EPriorityStandard);
  iSnapshotTimeExpr = strdup(__UPLOAD_TIME_EXPR__); //xxx needs to come from ConfigDb
  if (!iSnapshotTimeExpr) User::Leave(KErrNoMemory);
  //logt("uploader timer inits done");
  iSnapshotTimeCtx = time(NULL); //xxx needs to come from ConfigDb
  if (iSnapshotTimeCtx == -1) User::Leave(KErrGeneral);
  StateChangedL();
}

CUploader::~CUploader()
{
  delete iPostTimerAo;
  delete iSnapshotTimerAo;
  delete iPosterAo;
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
  ex_db_log_fatal_error(iLogDb, errCode);
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

  if (getNextOldLogFile(&iFileToPost, NULL)) {
    if (iFileToPost) {
      return;
    } else {
      iNoOldFiles = ETrue;
      logt("no more old files to upload");
    }
  } else {
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
    gx_error_log_free(parseError);
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
	  gx_error_log_free(localError);
	  FatalError(KErrGeneral);
	} else {
	  logf("posted log file '%s'", iFileToPost);
	  iNumPostFailures = 0;
	  iFileToPost = NULL;
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

	// Some errors are more severe than others.
	switch (anError)
	  {
	  case KErrCouldNotConnect:
	  case KErrDisconnected:
	  case KErrCommsLineFail:
	  case KErrCommsFrame:
	  case KErrCommsOverrun:
	  case KErrCommsParity:
	  case KErrTimedOut:
	  case KErrServerBusy: // local daemon, such as socket or file server
	  case KErrInUse: // file in use maybe
	  case KErrNotReady: // -18 ("A device required by an I/O operation is not ready to start operations.") We have actually gotten this error. Let us have it here to see if it is something transient.
	    {
	      // Retry later.
	      iNumPostFailures++;
	      SetPostTimer();
	      break;
	    }
	  case KErrNoMemory: // bad, resume this program later
	  case KErrPathNotFound: // our state may be messed up
	    {
	      FatalError(anError);
	      break;
	    }
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
	  }
        break;
      }
    }
}

// Better by careful not to run out of stack calling this recursively.
// Can always use an "ImmediateAo" if necessary to avoid such a risk.
void CUploader::StateChangedL()
{
  if (!iSnapshotTimePassed &&
      !iNoNextSnapshotTime &&
      !iSnapshotTimerAo->IsActive()) {
    SetSnapshotTimerL();
  }

  if (iFileToPost) {
    if (iPosterAo->IsActive() ||
	iPostTimerAo->IsActive())
      return;
    PostNowL();
  } else if (!iNoOldFiles) {
    NextOldFileL();
    StateChangedL();
  } else if (iSnapshotTimePassed) {
    TakeSnapshotNowL();
  }
}

void CUploader::PostNowL()
{
  logt("posting file now");

  // For error handling testing.
#if 0
  logt("horrible test error");
  User::Leave(KErrTotalLossOfPrecision);
#endif

  TPtrC8 fileName((TUint8*)iFileToPost);
  TFileName fileNameDes;
  // Our names should all be ASCII, so this may be overkill.
  User::LeaveIfError(CnvUtfConverter::ConvertToUnicodeFromUtf8(fileNameDes, fileName));
  iPosterAo->PostFileL(KPostUri, fileNameDes);
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
    gx_error_log_free(snapError);
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
					  const char* key,
					  const void* value, 
					  GError** error)
{
  return TRUE; //xxx we shall convert the values here and invoke appropriate uploader methods
}

EXTERN_C up_Uploader* up_Uploader_new(LogDb* logDb, GError** error)
{
  CUploader* object = NULL;
  TRAPD(errCode, object = CUploader::NewL(logDb));
  if (errCode) {
    if (error)
      *error = g_error_new(domain_symbian, errCode, "Uploader init failure: %s (%d)", plat_error_strerror(errCode), errCode);
    return NULL;
  }
  return (up_Uploader*)object;
}

EXTERN_C void up_Uploader_destroy(up_Uploader* object)
{
  delete ((CUploader*)object);
}

#endif // __FEATURE_UPLOADER__
