#include "up_uploader_qt_private.hpp"

#include "cf_query.h"
#include "er_errors.h"
#include "ld_log_db.h"
#include "up_private.h"
#include "utils_cl2.h"

#include "moment_parser.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/gx_exception.hpp"
#include "common/logging-stack.h"
#include "common/logging-time.h"
#include "common/logging.h"
#include "common/platform_error.h"

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#include <exception>

#include <QtDebug>

#if defined(__SYMBIAN32__)
#include "epoc-iap.h"
#include "common/epoc-time.h"
#endif /* __SYMBIAN32__ */

// --------------------------------------------------
// CUploader
// --------------------------------------------------

static time_t TimeNow()
{
  time_t now = time(NULL);
  if (now == -1)
    er_log_errno(er_FATAL, "time(NULL) failed");
  return now;
}

static void DataChanged(bb_Blackboard* bb, enum bb_DataType dt,
			gpointer data, int len, gpointer arg)
{
  (void)dt;
  (void)len;
  CUploader* self = (CUploader*)arg;
  bb_Board* bd = bb_Blackboard_board(bb);
  bool val = bd->uploads_allowed;
  self->Set_uploads_allowed(val);
}

void CUploader::Set_uploads_allowed(bool val)
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
    throw std::bad_alloc();
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
void CUploader::RefreshIap(bool aNotInitial)
{
#if defined(__SYMBIAN32__)
  // TUint32 coercion hopefully okay.
  iIapId = (TUint32)get_config_iap_id();
  if (aNotInitial)
    log_db_log_status(GetLogDb(), NULL, "Uploader IAP changed to %d", iIapId);
#endif /* __SYMBIAN32__ */
}

void CUploader::RefreshSnapshotTimeExpr(bool aNotInitial)
{
  GError* localError = NULL;
  gchar* newOne = NULL;
  if (!get_ConfigDb_str("uploader.time_expr", 
			&newOne, __UPLOAD_TIME_EXPR__, 
			&localError)) {
    if (aNotInitial)
      gx_dblog_error_free_check(GetLogDb(), localError, NULL);
    else
      gx_error_free(localError);
  } else {
    assert(newOne != NULL);
    g_free(iSnapshotTimeExpr);
    iSnapshotTimeExpr = newOne;
    logt("got time expression");
    logt(iSnapshotTimeExpr);
    if (aNotInitial) {
      log_db_log_status(GetLogDb(), NULL, "Upload time expression set to '%s'",
			newOne);

      // We may need to recompute the next snapshot time based on the
      // new expression. But if a previosly set time has already
      // passed, then that is not affected by the expression change.
      // What is past is past.
      {
	if (iSnapshotTimerAo.isActive())
	  iSnapshotTimerAo.stop();
	iNoNextSnapshotTime = false;
	StateChanged();
      }
    }
  }
}

static const char* KBoundary = "-----AaB03xeql7dsxeql7ds";

CUploader::CUploader(ac_AppContext* aAppContext) :
  iAppContext(aAppContext), iNoConfig(false),
  iNetworkReply(NULL), iNoOldFiles(false),
  iNumPostFailures(0),
  iFileToPost(NULL), 
  iPrologue(NULL), iEpilogue(NULL), 
  iPostData(NULL),
  iSnapshotTimePassed(false), iSnapshotTimeExpr(NULL),
  iNoNextSnapshotTime(false)
{
  // Ensure that uploads directory exists.
  GError* mdError = NULL;
  if (!mkdir_p(LOG_UPLOADS_DIR, &mdError)) {
    er_log_gerror(er_FATAL|er_FREE, mdError, 
		  "failure creating uploads directory");
  }

  // Note that we might be able to use
  // QNetworkRequest::setSslConfiguration to specify the SSL cert we
  // want to use, even one that is not installed. This would ease
  // deployment.

  const gchar* upload_url = ac_STATIC_GET(upload_url);
  if (!upload_url) {
    iNoConfig = true;
    logt("uploads disabled: no upload URL");
  } else {
    logg("upload URL: %s", upload_url);
    iNetworkRequest.setUrl(QUrl(upload_url));
    QByteArray ct("multipart/form-data, boundary=");
    ct.append(KBoundary);
    iNetworkRequest.setHeader(QNetworkRequest::ContentTypeHeader, ct);
    iNetworkRequest.setRawHeader("Connection", "close");
  }

  RefreshIap(false);
#if defined(__SYMBIAN32__)
  logg("uploader using IAP %d", iIapId);
#endif /* __SYMBIAN32__ */

  iPostTimerAo.setSingleShot(true);
  iSnapshotTimerAo.setSingleShot(true);
  connect(&iPostTimerAo, SIGNAL(timeout()), 
	  this, SLOT(handlePosterTimerEvent()));
  connect(&iSnapshotTimerAo, SIGNAL(timeout()), 
	  this, SLOT(handleSnapshotTimerEvent()));

  RefreshSnapshotTimeExpr(false);
  iSnapshotTimeCtx = TimeNow();
  //logg("using snapshot time '%s'", iSnapshotTimeExpr);

  // Note that if this ConstructL() leaves, the dtor of this will
  // unregister us.
  BbRegisterL();

  StateChangedL();
}

CUploader::~CUploader()
{
  BbUnregister();
  DestroyPosterAo();
  g_free(iSnapshotTimeExpr); // safe when NULL
}

void CUploader::Inactivate()
{
  DestroyPosterAo();
  if (iSnapshotTimerAo.isActive()) 
    iSnapshotTimerAo.stop();
  if (iPostTimerAo.isActive()) 
    iPostTimerAo.stop();
}

void CUploader::FatalError(const std::exception &ex)
{
  Inactivate();
  er_log_none(er_FATAL, "error in uploader: %s", ex.what());
}

#define CATCH_FATAL(_act)			\
  try {						\
    _act ;					\
  } catch (const std::exception &_ex) {		\
    FatalError(_ex);				\
  }

void CUploader::StateChanged()
{
  CATCH_FATAL(StateChangedL());
}

struct ScopedPointerGfree
{
  static inline void cleanup(gchar* pointer)
  {
    g_free(pointer);
  }
};

void CUploader::NextOldFileL()
{
  if (iNoOldFiles) return;

  DELETE_Z(iFileToPost);

  GError* error = NULL;
  gchar* pathname = NULL;
  if (getNextOldLogFile(&pathname, &error)) {
    if (pathname) {
      dblogg("found old log file '%s'", pathname);
      QScopedPointer<gchar, ScopedPointerGfree> pn(pathname);
      QString qs = QString::fromUtf8(pathname);
      //qDebug() << qs;
      iFileToPost = q_check_ptr(new QFile(qs));
      //qDebug() << (iFileToPost->fileName().toLocal8Bit());
      return;
    } else {
      iNoOldFiles = true;
      dblogt("no more old files to upload");
    }
  } else {
    er_log_gerror(er_FATAL|er_FREE, error, "getting next log file");
  }
}

// External API.
void CUploader::RequestSnapshot()
{
  iSnapshotTimePassed = true;
  if (iSnapshotTimerAo.isActive())
    iSnapshotTimerAo.stop();
  StateChanged();
}

static int SecsToMsecs(int secs)
{
  long long ms64 = (long long)(secs) * 1000LL;
  if (ms64 > 0x7fffffffLL) ms64 = 0x7fffffffLL;
  return (int)ms64;
}

// Computes next snapshot time (if any), and sets a timer for it as
// appropriate.
void CUploader::SetSnapshotTimerL()
{
  assert(!iSnapshotTimePassed);
  if (iSnapshotTimerAo.isActive())
    iSnapshotTimerAo.stop();
  if (iNoNextSnapshotTime) 
    return; // flag to avoid needless computation
  time_t now = TimeNow();
  time_t ctx = iSnapshotTimeCtx;
  time_t snaptime; // xxx should perhaps store this and use multiple interval timer requests if necessary to get this far
  GError* parseError = NULL;
  if (!parse_moment(iSnapshotTimeExpr, ctx, now, &snaptime, &parseError)) {
    gx_dblog_error_free(GetLogDb(), parseError);
    iNoNextSnapshotTime = true;
    return;
  }
  if (!snaptime) {
    dblogt("no snapshot time upcoming");
    iNoNextSnapshotTime = true;
    return;
  }
  logt("next snapshot time computed");
  log_time(snaptime);

  // We must specify time interval in milliseconds. For long time
  // intervals we might get an overflow. Another reason to use an
  // absolute timer where possible.
  int diffTime = snaptime - now; // both in UTC  //xxx negative?
  diffTime = SecsToMsecs(diffTime);
  if (diffTime < 5000) diffTime = 5000; // ensure some sanity
  logg("snapshot %d msecs from now", diffTime);
  iSnapshotTimerAo.start(diffTime);
}

void CUploader::SetPostTimer()
{
  assert(iNumPostFailures > 0);

  if (iPostTimerAo.isActive())
    iPostTimerAo.stop();

  // Roughly num_failures * 5 mins.
  int secs = 5 * 60 * iNumPostFailures + (rand() % 60);
  int interval = SecsToMsecs(secs);
  dblogg("retrying upload in %d secs / %d msecs", secs, interval);

  iPostTimerAo.start(interval);
}

// Make sure this method does not leave.
void CUploader::handlePosterTimerEvent()
{
  logt("posting timer event");
  StateChanged();
}

// Make sure this method does not leave.
void CUploader::handleSnapshotTimerEvent()
{
  logt("snapshot timer event");
  iSnapshotTimePassed = true;
  StateChanged();
}

bool CUploader::PosterAoIsActive()
{
  return (iNetworkReply != NULL) && iNetworkReply->isRunning();
}

void CUploader::postingFinished()
{
  QNetworkReply::NetworkError errCode = iNetworkReply->error();
  dblogg("poster finished with %d", errCode);

  iFileToPost->close();
  iNetworkReply->deleteLater();

  switch (errCode)
    {
      //// success
    case QNetworkReply::NoError:
      {
	GError* localError = NULL;
	QByteArray ba = (iFileToPost->fileName().toLocal8Bit());
	const char* pathname = ba.data();
	if (!rm_file(pathname, &localError)) {
	  FatalError(GException(localError));
	} else {
	  log_db_log_status(GetLogDb(), NULL, "posted log file '%s'", pathname);
	  iNumPostFailures = 0;
	  DELETE_Z(iFileToPost);

	  {
	    time_t t = TimeNow();
	    ac_global_Registry->last_upload_time = t;
	  }

	  StateChanged();
	}
        break;
      }
      //// aborted (do we ever get these signals)
    case QNetworkReply::OperationCanceledError:
      {
	logt("upload operation cancelled event");
	break;
      }
      //// transient failure
    case QNetworkReply::ConnectionRefusedError:
    case QNetworkReply::RemoteHostClosedError:
    case QNetworkReply::HostNotFoundError:
    case QNetworkReply::TimeoutError:
    case QNetworkReply::SslHandshakeFailedError:
    case QNetworkReply::TemporaryNetworkFailureError:
    case QNetworkReply::ProxyConnectionRefusedError:
    case QNetworkReply::ProxyConnectionClosedError:
    case QNetworkReply::ProxyNotFoundError:
    case QNetworkReply::ProxyTimeoutError:
    case QNetworkReply::ProxyAuthenticationRequiredError:
    case QNetworkReply::ContentAccessDenied:
    case QNetworkReply::ContentOperationNotPermittedError:
    case QNetworkReply::ContentNotFoundError:
    case QNetworkReply::ContentReSendError:
    case QNetworkReply::ProtocolUnknownError:
    case QNetworkReply::ProtocolInvalidOperationError:
    case QNetworkReply::UnknownNetworkError:
    case QNetworkReply::UnknownProxyError:
    case QNetworkReply::UnknownContentError:
    case QNetworkReply::ProtocolFailure:
      {
	// Retry later.
	logg("upload failure %d, retrying later", errCode);
	iNumPostFailures++;
	SetPostTimer();
        break;
      }
      //// permanent failure
    case QNetworkReply::AuthenticationRequiredError:
      {
        // We must be doing something wrong. Better stop altogether,
        // barring external intervention.
	er_log_none(0, "inactivating uploader due to a permanent posting failure (%d)", errCode);
	Inactivate();
        break;
      }
      //// unknown failure
    default:
      {
	logg("unknown upload failure %d, retrying later", errCode);
	iNumPostFailures++;
	SetPostTimer();
        break;
      }
    }
}

void CUploader::DestroyPosterAo()
{
  if (iNetworkReply) {
    iNetworkReply->abort();
    DELETE_Z(iNetworkReply);
  }
  DELETE_Z(iPostData);
  iPostElems.clear();
  if (iFileToPost) {
    iFileToPost->close();
  }
  DELETE_Z(iPrologue);
  DELETE_Z(iEpilogue);
}

void CUploader::CreatePosterAoL()
{
  assert(!iNetworkReply);
  assert(iFileToPost);

#if defined(__SYMBIAN32__)
  // Requires Qt 4.7.
  //QNetworkConfiguration cfg = iNetworkAccessManager.defaultConfiguration();
  QList<QNetworkConfiguration> cfgList = iNetworkAccessManager.allConfigurations();
  // We want to print out the list of configurations. Perhaps we can select one by platform specific ID, which we already have. xxx
  //iNetworkAccessManager.setConfiguration(cfg);
#endif /* __SYMBIAN32__ */

  QByteArray ba = (iFileToPost->fileName().toLocal8Bit());
  const char* pathname = ba.data();
  dblogg("asking poster to post '%s'", pathname);

  iPrologue = q_check_ptr(new QBuffer());
  iEpilogue = q_check_ptr(new QBuffer());

  const gchar* username = get_config_username();
  logg("uploader using username '%s'", username);

  static const char* KSep = "--";
  static const char* KCrLf = "\r\n";

  QByteArray& prologue = iPrologue->buffer();
  prologue.append(KSep);
  prologue.append(KBoundary);
  prologue.append(KCrLf);
  prologue.append("Content-Disposition: form-data; name=\"logdata\"; filename=\"");
  prologue.append(username);
  prologue.append(".db\"\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\n\r\n");

  QByteArray& epilogue = iEpilogue->buffer();
  epilogue.append(KCrLf);
  epilogue.append(KSep);
  epilogue.append(KBoundary);
  epilogue.append(KCrLf);
  epilogue.append("Content-Disposition: form-data; name=\"logdata_submit\"\r\n\r\nUpload\r\n");
  epilogue.append(KSep);
  epilogue.append(KBoundary);
  epilogue.append(KSep);
  epilogue.append(KCrLf);

  if (!iFileToPost->open(QIODevice::ReadOnly)) {
    int errCode = (iFileToPost->error());
    gx_throw(gx_error_new(domain_qt, errCode, 
			  "failed to open file '%s': QFile::FileError %d", 
			  pathname, errCode));
  }

  iPrologue->open(QIODevice::ReadOnly); //xxx check return bool
  iEpilogue->open(QIODevice::ReadOnly); //xxx check return bool

  iPostElems.append(iPrologue);
  iPostElems.append(iFileToPost);
  iPostElems.append(iEpilogue);

  iPostData = q_check_ptr(new QIODeviceSeq(iPostElems));

  // Note that the file object (or the file) may not be deleted until
  // we get the finished() signal for this reply.
  iNetworkReply = iNetworkAccessManager.post(iNetworkRequest, iPostData);

  /*
  connect(iNetworkReply, SIGNAL(error(QNetworkReply::NetworkError)),
	  this, SLOT(postingError(QNetworkReply::NetworkError)));
  */
  connect(iNetworkReply, SIGNAL(finished()),
	  this, SLOT(postingFinished()));
}

void CUploader::PostNowL()
{
  DestroyPosterAo();
  CreatePosterAoL();
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
  if (!log_db_take_snapshot(GetLogDb(), pathname, &wasRenamed, &snapError)) {
    logg("failure taking snapshot to file '%s'", pathname);
    free(pathname);
    logg("snapshot file was%s created", wasRenamed ? "" : " not");
    er_log_gerror(er_FATAL|er_FREE, snapError, "taking snapshot");
  }

  log_db_log_status(GetLogDb(), NULL, "snapshot taken as '%s'", pathname);

  assert(!iFileToPost);
  iFileToPost = q_check_ptr(new QFile(QString::fromUtf8(pathname)));
  iSnapshotTimePassed = false;
  StateChangedL();
}

// Better by careful not to run out of stack calling this recursively.
// Can always use an "ImmediateAo" if necessary to avoid such a risk.
void CUploader::StateChangedL()
{
  if (iNoConfig)
    return;

  if (!iSnapshotTimePassed &&
      !iNoNextSnapshotTime &&
      !iSnapshotTimerAo.isActive()) {
    SetSnapshotTimerL();
  }

 again:
  if (iFileToPost) {
    if (PosterAoIsActive() || 
	// 'iPostTimerAo' is a post retry timer.
	iPostTimerAo.isActive())
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
    ((CUploader*)object)->RefreshIap(true);
  } else if (strcmp(key, "uploader.time_expr") == 0) {
    ((CUploader*)object)->RefreshSnapshotTimeExpr(true);
  }
  return TRUE;
}

EXTERN_C up_Uploader* up_Uploader_new(ac_AppContext* aAppContext, GError** error)
{
  CUploader* object = NULL;
  try {
    object = q_check_ptr(new CUploader(aAppContext));
  } catch (const std::exception &ex) {
    if (error)
      *error = gx_error_new(domain_qt, -1, "Uploader init failure: %s", ex.what());
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
