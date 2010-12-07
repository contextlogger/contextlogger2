#include "up_uploader_qt_private.hpp"

#include "cf_query.h"
#include "er_errors.h"
#include "ld_log_db.h"
#include "utils_cl2.h"

#include "moment_parser.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging-stack.h"
#include "common/logging-time.h"
#include "common/logging.h"
#include "common/platform_error.h"

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#if defined(__SYMBIAN32__)
#include "epoc-iap.h"
#include "common/epoc-time.h"
#endif /* __SYMBIAN32__ */

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
    logg("upload URL: %s", upload_url);
  }

  RefreshIap(EFalse);
  logg("uploader using IAP %d", iIapId);

  // Ensure that uploads directory exists.
  GError* mdError = NULL;
  if (!mkdir_p(LOG_UPLOADS_DIR, &mdError)) {
    gx_txtlog_error_free(mdError);
    User::Leave(KErrGeneral);
  }

  iPostTimerAo = CTimerAo::NewL(*this, CActive::EPriorityStandard);
  iSnapshotTimerAo = CTimerAo::NewL(*this, CActive::EPriorityStandard);
  RefreshSnapshotTimeExpr(EFalse);
  iSnapshotTimeCtx = time(NULL); //xxx needs to come from ConfigDb -- but actually ones the next time is computed by a Lua expression, that expression can contain any required fixpoint as a constant
  if (iSnapshotTimeCtx == -1) User::Leave(KErrGeneral);
  //logg("using snapshot time '%s'", iSnapshotTimeExpr);

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
      dblogt("no more old files to upload");
    }
  } else {
    er_log_gerror(er_FATAL|er_FREE, error, "getting next log file");
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
    dblogt("no snapshot time upcoming");
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
  dblogg("retrying upload in %d secs / %d usecs", secs, interval.Int());

  // Note that these timers should not complete with KErrAbort, since
  // a wait for an interval should not be affected by a system time
  // change.
  iPostTimerAo->After(interval);
}

// Make sure this method does not leave.
void CUploader::HandleTimerEvent(CTimerAo* aTimerAo, TInt errCode)
{
  logg("timer event (%d)", errCode);

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
  dblogg("poster reports %d", anError);

  switch (anError)
    {
    case POSTER_SUCCESS:
      {
	GError* localError = NULL;
	//assert(iFileToPost);
	//logg("removing file '%s'", iFileToPost);
	if (!rm_file(iFileToPost, &localError)) {
	  //logt("failure removing file");
	  gx_txtlog_error_free(localError);
	  FatalError(KErrGeneral);
	} else {
	  log_db_log_status(iLogDb, NULL, "posted log file '%s'", iFileToPost);
	  iNumPostFailures = 0;
	  iFileToPost = NULL;

	  {
	    time_t t = time(NULL);
	    if (t == -1) {
	      er_log_errno(er_FATAL, "time()");
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
	er_log_none(0, "inactivating uploader due to a permanent posting failure");
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
	dblogg("inactivating uploader due to Symbian error %d", anError);
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
    logg("poster creation failed with %d", errCode);
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
  dblogg("asking poster to post '%s'", iFileToPost);
  iPosterAo->PostFileL(iUploadUrl, fileNameDes);
}

void CUploader::TakeSnapshotNowL()
{
  dblogt("taking snapshot now");

  // LOG_UPLOADS_DIR and LOGDB_DIR must be on the same device to allow
  // for renaming rather than copying.
  char* pathname = tempnam(LOG_UPLOADS_DIR, "log_"); // caller must free 'pathname'
  if (!pathname) {
    er_log_errno(er_FATAL, "failure in tempnam");
  }
  
  gboolean wasRenamed = FALSE;
  GError* snapError = NULL;
  if (!log_db_take_snapshot(iLogDb, pathname, &wasRenamed, &snapError)) {
    logg("failure taking snapshot to file '%s'", pathname);
    free(pathname);
    logg("snapshot file was%s created", wasRenamed ? "" : " not");
    er_log_gerror(er_FATAL|er_FREE, snapError, "taking snapshot");
  }

  log_db_log_status(iLogDb, NULL, "snapshot taken as '%s'", pathname);

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

/**

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
