/*
 !concept {:name => "Multipart HTTP posting (Qt)",
   :desc => "Multipart HTTP(S) POSTing using the QtNetwork API."}
*/

#ifndef __up_uploader_qt_private_hpp__
#define __up_uploader_qt_private_hpp__

#include "up_private.h"

#include "ac_app_context.h"
#include "iodeviceseq_qt.hpp"
#include "ut_abs_timer_qt.hpp"

#include <glib.h>

#include <QBuffer>
#include <QFile>
#include <QIODevice>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QSslConfiguration>
#include <QTimer>

class PostSession :
  public QObject
{
  Q_OBJECT

public:
  QNetworkReply* iNetworkReply;
  QFile* iFileToPost; // not owned
  QBuffer* iPrologue;
  QBuffer* iEpilogue;
  QList<QIODevice*> iPostElems;
  QIODeviceSeq* iPostData;

public:
  PostSession();
  ~PostSession();
};

class CUploader : 
  public QObject
{
  Q_OBJECT

 public:
  CUploader(ac_AppContext* aAppContext);
  ~CUploader();

  void RefreshIap(bool aNotInitial);
  void RefreshSnapshotTimeExpr(bool aNotInitial);

  void RequestSnapshot();

 private slots:
  void handlePosterTimerEvent();
  void handleSnapshotTimerEvent();

 private: // MPosterObserver
  void PosterEvent(int anError);

 private: // methods
  void Inactivate();
  void StateChanged();
  void StateChangedL();
  void NextOldFileL();
  void CreatePosterAoL();
  void DestroyPosterAo();
  bool PosterAoIsActive();
  void HandleCommsError(int errCode);
  void PostNowL();
  void SetPostTimer();
  void SetSnapshotTimerL();
  void TakeSnapshotNowL();
  void FatalError(const std::exception &ex);

 private: // property

  ac_AppContext* iAppContext; // not owned

  bool iNoConfig; // no upload URL
#if defined(__SYMBIAN32__)
  TUint32 iIapId;
#endif /* __SYMBIAN32__ */

  //// posting state
  QNetworkAccessManager iNetworkAccessManager;
  QSslConfiguration iSslConfiguration;
  QNetworkRequest iNetworkRequest;
  QTimer iPostTimerAo; // interval timer
  bool iNoOldFiles; // getNextOldLogFile found nothing
  int iNumPostFailures; // affects retry timing

  PostSession* iPostSession;
  QFile* iFileToPost; // pathname of file to upload

  //// snapshot taking state
  QAbsTimer iSnapshotTimerAo;
  bool iSnapshotTimePassed;
  gchar* iSnapshotTimeExpr;
  time_t iSnapshotTimeCtx;
  bool iNoNextSnapshotTime;

 private slots:
  //void 	postingError ( QNetworkReply::NetworkError code );
  void postingSslErrors(const QList<QSslError> & errors);
  void postingFinished();

 private:
  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }
  QUrl GetUrl() const { return iNetworkRequest.url(); }

  //// blackboard
 public:
  void Set_uploads_allowed(bool val);
 private:
  bool i_uploads_allowed; // from blackboard
  bb_Closure iClosure;
  void BbRegisterL();
  void BbUnregister();
};

#endif /* __up_uploader_qt_private_hpp__ */

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
