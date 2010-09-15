#include "kr_plat_ao.h"

#include "ac_app_context.h"
#include "bb_blackboard.h"
#include "er_errors.h"
#include "kr_diskspace.h"
#include "sa_sensor_list_log_db.h"
#include "ut_diskspace_epoc.hpp"
#include "ut_telephony_epoc.h"
#include "ut_retry_epoc.hpp"
#include "utils_cl2.h"

// --------------------------------------------------
// disk space observing
// --------------------------------------------------

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CDiskObserver" ;; name
 "" ;; args
 "" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CDiskObserver  \
public: static CDiskObserver* NewLC(); \
public: static CDiskObserver* NewL(); \
private: CDiskObserver(); \
private: void ConstructL();

#define CTOR_IMPL_CDiskObserver  \
CDiskObserver* CDiskObserver::NewLC() \
{ \
  CDiskObserver* obj = new (ELeave) CDiskObserver(); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CDiskObserver* CDiskObserver::NewL() \
{ \
  CDiskObserver* obj = CDiskObserver::NewLC(); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CDiskObserver::CDiskObserver() \
{}
/***end***/
NONSHARABLE_CLASS(CDiskObserver) : public CBase, public MDiskSpace
{
  CTOR_DECL_CDiskObserver;

 public:
  ~CDiskObserver();

 private:
  virtual void DiskSpaceNotify(TInt aDrive, TInt errCode);

 private:
  DEF_SESSION(RFs, iFs);
  CDiskSpaceNotifier* iNotifier;
};

CTOR_IMPL_CDiskObserver;

void CDiskObserver::ConstructL()
{
  TChar driveLetter = DATABASE_DRIVE_LETTER;
  TInt driveNum = 0;
  User::LeaveIfError(RFs::CharToDrive(driveLetter, driveNum));

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iFs, iFs.Connect());
  int database_disk_threshold = ac_STATIC_GET(database_disk_threshold);
  iNotifier = CDiskSpaceNotifier::NewL(iFs, driveNum,
				       this, 
				       (TInt64)database_disk_threshold);
}

CDiskObserver::~CDiskObserver()
{
  delete iNotifier;
  SESSION_CLOSE_IF_OPEN(iFs);
}

void CDiskObserver::DiskSpaceNotify(TInt aDrive, TInt errCode)
{
  (void)aDrive;
  if (errCode) {
    // We are not really expecting any errors here, not any that are
    // in the API docs, anyway.
    logf("unexpected error in disk observer: %d", errCode);
  } else {
    // There are a number of causes why we might get an
    // RFs::NotifyDiskSpace() event, so we have to do some further
    // checking here.
    TRAPD(errCode, CheckLoggingMediumReadyL(iFs));
    if (errCode) {
      // One issue here is that we do not really know if it is safe to
      // try to log this error to the database. Possibly not, so the
      // debug log, if any, shall have to do.
      ex_txtlog_error(errCode);
      if (errCode == KErrDiskFull) {
	er_fatal_disk_low;
      } else {
	er_fatal_disk_not_ready;
      }
    }
  }
}

// --------------------------------------------------
// network registration status observing
// --------------------------------------------------

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CRegistrationObserver" ;; name
 "" ;; args
 "" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CRegistrationObserver  \
public: static CRegistrationObserver* NewLC(); \
public: static CRegistrationObserver* NewL(); \
private: CRegistrationObserver(); \
private: void ConstructL();

#define CTOR_IMPL_CRegistrationObserver  \
CRegistrationObserver* CRegistrationObserver::NewLC() \
{ \
  CRegistrationObserver* obj = new (ELeave) CRegistrationObserver(); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CRegistrationObserver* CRegistrationObserver::NewL() \
{ \
  CRegistrationObserver* obj = CRegistrationObserver::NewLC(); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CRegistrationObserver::CRegistrationObserver() \
{}
/***end***/
NONSHARABLE_CLASS(CRegistrationObserver) : 
  public CBase, 
  public MNetworkRegistrationRequestor,
  public MNetworkRegistrationObserver
{
  CTOR_DECL_CRegistrationObserver;

 public:
  ~CRegistrationObserver();

 private:
  virtual void HandleGotNetworkRegistration(TInt aError);
  virtual void HandleNetworkRegistrationChange(TInt aError);
  void HandleRegistration(TInt aError, CTelephony::TNetworkRegistrationV1 const & aData);

 private:
  CNetworkRegistrationGetter* iRegistrationInfoGetter;
  CNetworkRegistrationNotifier* iRegistrationInfoNotifier;
};

CTOR_IMPL_CRegistrationObserver;

void CRegistrationObserver::ConstructL()
{
  ac_AppContext* ac = ac_get_global_AppContext();

  iRegistrationInfoGetter = new (ELeave) CNetworkRegistrationGetter(ac_Telephony(ac), *this);
  iRegistrationInfoNotifier = new (ELeave) CNetworkRegistrationNotifier(ac_Telephony(ac), *this);

  iRegistrationInfoGetter->MakeRequest();
}

CRegistrationObserver::~CRegistrationObserver()
{
  delete iRegistrationInfoGetter;
  delete iRegistrationInfoNotifier;
}

void CRegistrationObserver::HandleGotNetworkRegistration(TInt aError)
{
  HandleRegistration(aError, iRegistrationInfoGetter->Data());
}

void CRegistrationObserver::HandleNetworkRegistrationChange(TInt aError)
{
  HandleRegistration(aError, iRegistrationInfoNotifier->Data());
}

void CRegistrationObserver::HandleRegistration(TInt aError, CTelephony::TNetworkRegistrationV1 const & aData)
{
  LogDb* logDb = ac_global_LogDb;
  if (aError) {
    if (logDb)
      ex_dblog_error_msg(logDb, "network registration status query failure", aError, NULL);
  } else {
    int status = aData.iRegStatus;
    logf("network registration status: %d", status);
    if (logDb) {
      log_db_log_registration(logDb, status, NULL);
    }
    iRegistrationInfoNotifier->MakeRequest();
  }
}

// --------------------------------------------------
// network information observing
// --------------------------------------------------

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CNetworkObserver" ;; name
 "" ;; args
 "" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CNetworkObserver  \
public: static CNetworkObserver* NewLC(); \
public: static CNetworkObserver* NewL(); \
private: CNetworkObserver(); \
private: void ConstructL();

#define CTOR_IMPL_CNetworkObserver  \
CNetworkObserver* CNetworkObserver::NewLC() \
{ \
  CNetworkObserver* obj = new (ELeave) CNetworkObserver(); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CNetworkObserver* CNetworkObserver::NewL() \
{ \
  CNetworkObserver* obj = CNetworkObserver::NewLC(); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CNetworkObserver::CNetworkObserver() \
{}
/***end***/
NONSHARABLE_CLASS(CNetworkObserver) : 
  public CBase, 
  public MNetworkInfoRequestor,
  public MNetworkInfoObserver,
  public MRetryAoObserver
{
  CTOR_DECL_CNetworkObserver;

 public:
  ~CNetworkObserver();

 private:
  virtual void RetryTimerExpired(CRetryAo* src, TInt errCode);
  virtual void HandleGotNetworkInfo(TInt aError);
  virtual void HandleNetworkInfoChange(TInt aError);
  void HandleData(TInt aError, CTelephony::TNetworkInfoV1 const & aData);

 private:
  CNetworkInfoGetter* iGetter;
  TBool iGetterDone;
  CNetworkInfoNotifier* iNotifier;
  CRetryAo* iRetryAo;
  CTelephony::TNetworkInfoV1 iOldData;
};

CTOR_IMPL_CNetworkObserver;

void CNetworkObserver::ConstructL()
{
  ac_AppContext* ac = ac_get_global_AppContext();

  iRetryAo = new (ELeave) CRetryAo(*this, 20, 60);

  iGetter = new (ELeave) CNetworkInfoGetter(ac_Telephony(ac), *this);
  iNotifier = new (ELeave) CNetworkInfoNotifier(ac_Telephony(ac), *this);

  iGetter->MakeRequest();
}

CNetworkObserver::~CNetworkObserver()
{
  delete iGetter;
  delete iNotifier;
  delete iRetryAo;
}

void CNetworkObserver::RetryTimerExpired(CRetryAo* src, TInt errCode)
{
  (void)src;
  if (errCode) {
    LogDb* logDb = ac_global_LogDb;
    ex_dblog_fatal_error_msg(logDb, "retry timer error", errCode);
  } else {
    if (iGetterDone)
      iNotifier->MakeRequest();
    else
      iGetter->MakeRequest();
  }
}

void CNetworkObserver::HandleGotNetworkInfo(TInt aError)
{
  HandleData(aError, iGetter->Data());
  if (!aError) 
    iGetterDone = ETrue;
}

void CNetworkObserver::HandleNetworkInfoChange(TInt aError)
{
  HandleData(aError, iNotifier->Data());
}

void CNetworkObserver::HandleData(TInt aError, 
				  CTelephony::TNetworkInfoV1 const & aData)
{
  if (aError) {
    LogDb* logDb = ac_global_LogDb;
    // xxx In flight mode, we are seeing KErrAccessDenied (-21) in here. See what the documentation says about flight mode for this API.
    ex_dblog_error_msg(logDb, "network info query failure", aError, NULL);
    if (!iRetryAo->Retry()) {
      er_log_none(er_FATAL, "network info queries failing");
    }
  } else {
    iRetryAo->ResetFailures();

    // Log interesting data, if it has changed.
    {
      if (aData.iLongName != iOldData.iLongName) {
	LogDb* logDb = ac_global_LogDb;
	HBufC8* text8 = ConvToUtf8ZL(aData.iLongName);
	CleanupStack::PushL(text8);
	//logf("operator name: '%s'", (char*)text8->Ptr());
	log_db_log_operator(logDb, (const char*)text8->Ptr(), NULL);
	kr_Controller_set_operator_name(ac_global_Controller, 
					(const char*)text8->Ptr());
	CleanupStack::PopAndDestroy(text8);
      }
    }

    if (iOldData.iCountryCode != aData.iCountryCode) {
      TLex lex(aData.iCountryCode);
      int mcc;
      TInt errCode = lex.Val(mcc);
      if (errCode) 
	// Unexpected MCC.
	mcc = -1;
      // Notify interested parties.
      kr_Controller_set_current_mcc(ac_global_Controller, mcc);
    }

    iOldData = aData;

    bb_Blackboard_notify(ac_global_Blackboard,
			 bb_dt_network_info,
			 (gpointer)&aData, 0);

    iNotifier->MakeRequest();
  }
}

// --------------------------------------------------
// network signal strength observing
// --------------------------------------------------

/* We require retries here in particular, as we have seen KErrOverflow frequently. In fact we have had enough many failures so as to run out of retries.

   xxx Should find out why this is, if there are conditions under which it is not okay to make this query. Cannot find any information about the cause, but something to account for is that "this functionality is not available when the phone is in flight mode".

   We try to account for the above by observing flightmode status, and refraining from making requests (or having outstanding requests) when flightmode is on. We shall see if this addresses the issue.
*/

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSignalObserver" ;; name
 "" ;; args
 "" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSignalObserver  \
public: static CSignalObserver* NewLC(); \
public: static CSignalObserver* NewL(); \
private: CSignalObserver(); \
private: void ConstructL();

#define CTOR_IMPL_CSignalObserver  \
CSignalObserver* CSignalObserver::NewLC() \
{ \
  CSignalObserver* obj = new (ELeave) CSignalObserver(); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSignalObserver* CSignalObserver::NewL() \
{ \
  CSignalObserver* obj = CSignalObserver::NewLC(); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSignalObserver::CSignalObserver() \
{}
/***end***/
NONSHARABLE_CLASS(CSignalObserver) : 
  public CBase, 
  public MSignalStrengthRequestor,
  public MSignalStrengthObserver,
  public MRetryAoObserver
{
  CTOR_DECL_CSignalObserver;

 public:
  ~CSignalObserver();

 private:
  virtual void RetryTimerExpired(CRetryAo* src, TInt errCode);
  virtual void HandleGotSignalStrength(TInt aError);
  virtual void HandleSignalStrengthChange(TInt aError);
  void HandleSignal(TInt aError, CTelephony::TSignalStrengthV1 const & aData);

 private:
  CRetryAo* iRetryAo;
  TBool iGetterDone;
  CSignalStrengthGetter* iGetter;
  CSignalStrengthNotifier* iNotifier;

 private:
  void MakeRequest();
  void Cancel();

  // Flight mode observation.
 private:
  bb_Closure iClosure;
  void BbRegisterL();
  void BbUnregister();
  TBool GetFlightMode();
 public:
  void HandleFlightModeChange();
};

CTOR_IMPL_CSignalObserver;

void CSignalObserver::MakeRequest()
{
  if (iGetterDone)
    iNotifier->MakeRequest();
  else
    iGetter->MakeRequest();
}

void CSignalObserver::Cancel()
{
  iRetryAo->Cancel();
  iNotifier->Cancel();
  iGetter->Cancel();

  iRetryAo->ResetFailures();

  // Once we begin observing again, we want a reading immediately,
  // rather than waiting for a change in readings.
  iGetterDone = EFalse;
}

static void SignalObserverFlightModeChanged
(bb_Blackboard* bb, enum bb_DataType dt,
 gpointer data, int len, gpointer arg)
{
  (void)dt;
  (void)len;
  CSignalObserver* self = (CSignalObserver*)arg;
  self->HandleFlightModeChange();
}

void CSignalObserver::HandleFlightModeChange()
{
  TBool fm = GetFlightMode();
  if (fm) {
    Cancel();

    // Notify with value +1 to indicate no signal.
    kr_Controller_set_signal_strength(ac_global_Controller, 1);
  } else {
    MakeRequest();
  }
}

TBool CSignalObserver::GetFlightMode()
{
  // Initial value (internal).
  bb_Blackboard* bb = ac_global_Blackboard;
  bb_Board* bd = bb_Blackboard_board(bb);
  return bd->flightmode;
}

void CSignalObserver::BbRegisterL()
{
  // Closure init.
  iClosure.changed = SignalObserverFlightModeChanged;
  iClosure.arg = this;

  // Registration proper.
  bb_Blackboard* bb = ac_global_Blackboard;
  if (!bb_Blackboard_register(bb, bb_dt_flightmode, iClosure, NULL))
    User::LeaveNoMemory();
}

void CSignalObserver::BbUnregister()
{
  bb_Blackboard_unregister(ac_global_Blackboard, iClosure);
}

void CSignalObserver::ConstructL()
{
  BbRegisterL();

  ac_AppContext* ac = ac_get_global_AppContext();

  iRetryAo = new (ELeave) CRetryAo(*this, 20, 60);

  iGetter = new (ELeave) CSignalStrengthGetter(ac_Telephony(ac), *this);
  iNotifier = new (ELeave) CSignalStrengthNotifier(ac_Telephony(ac), *this);

  TBool fm = GetFlightMode();
  if (!fm) {
    // We will not be getting any signal strength reading for as long
    // as flightmode is on. Makes sense.
    MakeRequest();
  }
}

CSignalObserver::~CSignalObserver()
{
  BbUnregister();
  delete iGetter;
  delete iNotifier;
  delete iRetryAo;
}

void CSignalObserver::RetryTimerExpired(CRetryAo* src, TInt errCode)
{
  (void)src;
  if (errCode) {
    LogDb* logDb = ac_global_LogDb;
    ex_dblog_fatal_error_msg(logDb, "retry timer error", errCode);
  } else {
    MakeRequest();
  }
}

void CSignalObserver::HandleGotSignalStrength(TInt aError)
{
  HandleSignal(aError, iGetter->Data());
  if (!aError) 
    iGetterDone = ETrue;
}

void CSignalObserver::HandleSignalStrengthChange(TInt aError)
{
  HandleSignal(aError, iNotifier->Data());
}

void CSignalObserver::HandleSignal(TInt aError, 
				   CTelephony::TSignalStrengthV1 const & aData)
{
  LogDb* logDb = ac_global_LogDb;
  if (aError) {
    ex_dblog_error_msg(logDb, "signal strength query failure", aError, NULL);
    if (!iRetryAo->Retry()) {
      er_log_none(er_FATAL, "signal strength queries failing");
    }
  } else {
    iRetryAo->ResetFailures();

    int dbm = -(aData.iSignalStrength);
    int bars = aData.iBar;
    logf("network signal strength: %d dBm (%d bars)", dbm, bars);
    log_db_log_signal(logDb, dbm, bars, NULL);
    iNotifier->MakeRequest();

    // Notify interested parties. Indiscriminately, receiver must
    // check for duplicates.
    kr_Controller_set_signal_strength(ac_global_Controller, dbm);
  }
}

// --------------------------------------------------
// auxiliary controller
// --------------------------------------------------

struct _kr_PlatAo {
  CDiskObserver* iDiskObserver;
  CRegistrationObserver* iRegistrationObserver;
  CNetworkObserver* iNetworkObserver;
  CSignalObserver* iSignalObserver;
};

extern "C" kr_PlatAo* kr_PlatAo_new(GError** error)
{
  kr_PlatAo* self = g_try_new0(kr_PlatAo, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  TRAPD(errCode, self->iDiskObserver = CDiskObserver::NewL());
  if (G_UNLIKELY(errCode)) {
    kr_PlatAo_destroy(self);
    if (error)
      *error = gx_error_new(domain_symbian, errCode, 
			    "disk observer creation failure: %s (%d)", 
			    plat_error_strerror(errCode), errCode);
    return NULL;
  }

  TRAP(errCode, self->iRegistrationObserver = CRegistrationObserver::NewL());
  if (G_UNLIKELY(errCode)) {
    kr_PlatAo_destroy(self);
    if (error)
      *error = gx_error_new(domain_symbian, errCode, 
			    "registration observer creation failure: %s (%d)", 
			    plat_error_strerror(errCode), errCode);
    return NULL;
  }

  TRAP(errCode, self->iNetworkObserver = CNetworkObserver::NewL());
  if (G_UNLIKELY(errCode)) {
    kr_PlatAo_destroy(self);
    if (error)
      *error = gx_error_new(domain_symbian, errCode, 
			    "network observer creation failure: %s (%d)", 
			    plat_error_strerror(errCode), errCode);
    return NULL;
  }

  TRAP(errCode, self->iSignalObserver = CSignalObserver::NewL());
  if (G_UNLIKELY(errCode)) {
    kr_PlatAo_destroy(self);
    if (error)
      *error = gx_error_new(domain_symbian, errCode, 
			    "signal strength observer creation failure: %s (%d)", 
			    plat_error_strerror(errCode), errCode);
    return NULL;
  }

  return self;
}

extern "C" void kr_PlatAo_destroy(kr_PlatAo* self)
{
  if (self) {
    delete self->iSignalObserver;
    delete self->iNetworkObserver;
    delete self->iRegistrationObserver;
    delete self->iDiskObserver;
    g_free(self);
  }
}

/**

kr_plat_ao_epoc.cpp

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
