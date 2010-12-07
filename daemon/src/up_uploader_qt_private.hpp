#ifndef __up_uploader_qt_private_hpp__
#define __up_uploader_qt_private_hpp__

#include "up_private.h"

#include "ac_app_context.h"

class CUploader : 
  public QObject
{
  Q_OBJECT

 public:
  CUploader(ac_AppContext* aAppContext);
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
