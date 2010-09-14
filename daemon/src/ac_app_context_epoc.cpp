// Not a standalone file. It exists as a separate file only to avoid
// platform-specific clutter in the primary implementation file.

#include "sa_sensor_list_log_db.h"
#include "ut_telephony_epoc.h"
#include "utils_cl2.h" // DEF_SESSION

#include <f32file.h> // RFs

#include <etel3rdparty.h> // CTelephony

#if __NEED_CONTACT_DATABASE__
#include <cntdb.h> // CContactDatabase
#endif

// --------------------------------------------------
// battery status observing
// --------------------------------------------------

/*
The primary function of this AO is to exit the process if battery is running low. We do not want to be the ones to consume the last bit of battery.

As a secondary task, the battery level is also logged, if the required resources have been initialized.

It is worth noting that there also is the hwrmpowerstatesdkpskeys.h API, which it seems we might likewise use. Do not know if one is "better" than the other.
http://www.forum.nokia.com/document/Cpp_Developers_Library/GUID-759FBC7F-5384-4487-8457-A8D4B76F6AA6/html/hwrmpowerstatesdkpskeys_8h.html
*/

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CBatteryObserver" ;; name
 "CTelephony& tel, MBatteryInfoRequestor& obs" ;; args
 "iObserver(obs)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
 '(args-to-constructl)
)
 ***/
#define CTOR_DECL_CBatteryObserver  \
public: static CBatteryObserver* NewLC(CTelephony& tel, MBatteryInfoRequestor& obs); \
public: static CBatteryObserver* NewL(CTelephony& tel, MBatteryInfoRequestor& obs); \
private: CBatteryObserver(CTelephony& tel, MBatteryInfoRequestor& obs); \
private: void ConstructL(CTelephony& tel, MBatteryInfoRequestor& obs);

#define CTOR_IMPL_CBatteryObserver  \
CBatteryObserver* CBatteryObserver::NewLC(CTelephony& tel, MBatteryInfoRequestor& obs) \
{ \
  CBatteryObserver* obj = new (ELeave) CBatteryObserver(tel, obs); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(tel, obs); \
  return obj; \
} \
 \
CBatteryObserver* CBatteryObserver::NewL(CTelephony& tel, MBatteryInfoRequestor& obs) \
{ \
  CBatteryObserver* obj = CBatteryObserver::NewLC(tel, obs); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CBatteryObserver::CBatteryObserver(CTelephony& tel, MBatteryInfoRequestor& obs) : iObserver(obs) \
{}
/***end***/
NONSHARABLE_CLASS(CBatteryObserver) : 
  public CBase, 
  public MBatteryInfoRequestor,
  public MBatteryInfoObserver
{
  CTOR_DECL_CBatteryObserver;

 public:
  ~CBatteryObserver();

 private:
  virtual void HandleGotBatteryInfo(TInt aError);
  virtual void HandleBatteryInfoChange(TInt aError);
  void HandleBattery(TInt aError, CTelephony::TBatteryInfoV1 const & aData);

 private:
  CBatteryInfoGetter* iBatteryInfoGetter;
  CBatteryInfoNotifier* iBatteryInfoNotifier;

 private:
  MBatteryInfoRequestor& iObserver;
};

CTOR_IMPL_CBatteryObserver;

void CBatteryObserver::ConstructL(CTelephony& tel, MBatteryInfoRequestor& obs)
{
  iBatteryInfoGetter = new (ELeave) CBatteryInfoGetter(tel, *this);
  iBatteryInfoNotifier = new (ELeave) CBatteryInfoNotifier(tel, *this);

  iBatteryInfoGetter->MakeRequest();
}

CBatteryObserver::~CBatteryObserver()
{
  delete iBatteryInfoGetter;
  delete iBatteryInfoNotifier;
}

void CBatteryObserver::HandleGotBatteryInfo(TInt aError)
{
  HandleBattery(aError, iBatteryInfoGetter->Data());
  iObserver.HandleGotBatteryInfo(aError); // forward
}

void CBatteryObserver::HandleBatteryInfoChange(TInt aError)
{
  HandleBattery(aError, iBatteryInfoNotifier->Data());
}

void CBatteryObserver::HandleBattery(TInt aError, 
				     CTelephony::TBatteryInfoV1 const & aData)
{
  if (aError) {
    // This is unexpected, but if we cannot query it, then we shall
    // live without this feature for the rest of the runtime.
    er_log_symbian(0, aError, "battery info status query failure");
  } else {
    int status = aData.iStatus;
    int level = aData.iChargeLevel;
    logf("battery status: %d (%d%%)", status, level);

    LogDb* logDb = ac_global_LogDb;
    if (logDb) {
      log_db_log_battery(logDb, status, level, NULL);
    }

    if (level < 20) {
      er_log_none(0, "battery running low (at %d%%): exiting", level);
      if (logDb) {
	er_fatal_battery_low;
      } else {
        // This is to avoid repeated error dialogs when the logger
        // does not get as far as properly running due to low battery.
	er_fatal_quiet();
      }
    } else {
      iBatteryInfoNotifier->MakeRequest();
    }
  }
}

// --------------------------------------------------
// plat app context implementation
// --------------------------------------------------

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CAppContextImpl" ;; name
 "ac_AppContext* ac, MAppContextInitObserver& obs" ;; args
 "iCtx(ac), iObs(obs)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CAppContextImpl  \
public: static CAppContextImpl* NewLC(ac_AppContext* ac, MAppContextInitObserver& obs); \
public: static CAppContextImpl* NewL(ac_AppContext* ac, MAppContextInitObserver& obs); \
private: CAppContextImpl(ac_AppContext* ac, MAppContextInitObserver& obs); \
private: void ConstructL();

#define CTOR_IMPL_CAppContextImpl  \
CAppContextImpl* CAppContextImpl::NewLC(ac_AppContext* ac, MAppContextInitObserver& obs) \
{ \
  CAppContextImpl* obj = new (ELeave) CAppContextImpl(ac, obs); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CAppContextImpl* CAppContextImpl::NewL(ac_AppContext* ac, MAppContextInitObserver& obs) \
{ \
  CAppContextImpl* obj = CAppContextImpl::NewLC(ac, obs); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CAppContextImpl::CAppContextImpl(ac_AppContext* ac, MAppContextInitObserver& obs) : iCtx(ac), iObs(obs) \
{}
/***end***/

NONSHARABLE_CLASS(CAppContextImpl) : 
  public CBase,
  public MBatteryInfoRequestor
{
  CTOR_DECL_CAppContextImpl;

 public:
  ~CAppContextImpl();

 public:
  ac_AppContext* iCtx;

  // The observer is notified of the completion of any asynchronous
  // initialization.
  MAppContextInitObserver& iObs;

 public: // internally public
  DEF_SESSION(RFs, iFs);

  CTelephony* iTelephony;

#if __NEED_CONTACT_DATABASE__
  CContactDatabase* iContactDatabase;
#endif

 private:
  CBatteryObserver* iBatteryObserver;

 private: // MBatteryInfoRequestor
  virtual void HandleGotBatteryInfo(TInt aError);
};

CTOR_IMPL_CAppContextImpl;

void CAppContextImpl::HandleGotBatteryInfo(TInt aError)
{
  (void)aError;
  // We need no bookkeeping for as long as we are only waiting for
  // this one reading before we are ready.
  iObs.AppContextReady(0);
}

void CAppContextImpl::ConstructL()
{
  iTelephony = CTelephony::NewL();

  iBatteryObserver = CBatteryObserver::NewL(*iTelephony, *this);

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iFs, iFs.Connect());

#if __NEED_CONTACT_DATABASE__
  iContactDatabase = CContactDatabase::OpenL();
#endif
}

CAppContextImpl::~CAppContextImpl()
{
  delete iBatteryObserver;
#if __NEED_CONTACT_DATABASE__
  delete iContactDatabase;
#endif
  delete iTelephony;
  SESSION_CLOSE_IF_OPEN(iFs);
}

// --------------------------------------------------
// interface
// --------------------------------------------------

CAppContext* CAppContext::NewL(ac_AppContext* ac,
			       MAppContextInitObserver& obs)
{
  CAppContextImpl* impl = CAppContextImpl::NewL(ac, obs);
  CleanupStack::PushL(impl);
  CAppContext* obj = new (ELeave) CAppContext;
  obj->iImpl = impl;
  CleanupStack::Pop();
  return obj;
}

CAppContext::~CAppContext()
{
  delete iImpl;
}

/**

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
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
