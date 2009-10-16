#ifndef __epoc_callstatus_hpp__
#define __epoc_callstatus_hpp__

#include "application_config.h"

#if __CALLSTATUS_ENABLED__

#include "ac_app_context.h"
#include "ut_retry_epoc.hpp"
#include "ut_telephony_epoc.h"
#include "utils_cl2.h"

#include <e32base.h>
#include <etel3rdparty.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_callstatus" ;; name
 "ac_AppContext* aAppContext" ;; args
 "iAppContext(aAppContext)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_callstatus  \
public: static CSensor_callstatus* NewLC(ac_AppContext* aAppContext); \
public: static CSensor_callstatus* NewL(ac_AppContext* aAppContext); \
private: CSensor_callstatus(ac_AppContext* aAppContext); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_callstatus  \
CSensor_callstatus* CSensor_callstatus::NewLC(ac_AppContext* aAppContext) \
{ \
  CSensor_callstatus* obj = new (ELeave) CSensor_callstatus(aAppContext); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_callstatus* CSensor_callstatus::NewL(ac_AppContext* aAppContext) \
{ \
  CSensor_callstatus* obj = CSensor_callstatus::NewLC(aAppContext); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_callstatus::CSensor_callstatus(ac_AppContext* aAppContext) : iAppContext(aAppContext) \
{}
/***end***/

NONSHARABLE_CLASS(CSensor_callstatus) :
  public CBase,
  public MRetryAoObserver,
  public MFlightModeRequestor,
  public MFlightModeObserver,
  public MCallStatusObserver
{
  CTOR_DECL_CSensor_callstatus;

 public:
  virtual ~CSensor_callstatus();

  // Produces a leave or a GError if starting fails. No harm calling
  // this if already started.
  gboolean StartL(GError** error);

  // Stops observing for changes and logging them. No harm calling
  // this if already stopped.
  void Stop();

 private: // MRetryAoObserver

  virtual void RetryTimerExpired(CRetryAo* src, TInt errCode);

 private: // MFlightModeRequestor

  virtual void HandleGotFlightMode(TInt aError);

 private: // MFlightModeObserver

  virtual void HandleFlightModeChange(TInt aError);

 private: // MCallStatusObserver

  virtual void HandleCallStatusChange(TInt aError);

 private:

  void Cancel();

 private:

  ac_AppContext* iAppContext; // not owned

  CTelephony *iTelephony; // owned

  CRetryAo* iRetryAo; // owned

  CFlightModeGetter* iFlightModeGetter; // owned

  CFlightModeNotifier* iFlightModeNotifier; // owned

  CCallStatusNotifier* iCallStatusNotifier; // owned

  enum TState {
    EInactive = 0, // not started
    EQueryingFlightMode, // querying flight mode, after start only
    EQueryingCallStatus, // querying call status
    ERetryWaiting, // waiting to retry call status query
    EInFlightMode // in flight mode, so no point in querying or retrying
  };
  TState iState;

 public:

  TBool IsActive() const { return iState != EInactive; }

 private:

  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

};

#endif // __CALLSTATUS_ENABLED__

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __CALLSTATUS_ENABLED__
#define DECLARE_SENSOR_callstatus CSensor_callstatus* iSensor_callstatus
#define SENSOR_CALLSTATUS_START sa_typical_symbian_sensor_start(self->iSensor_callstatus, "failed to start callstatus scanning")
#define SENSOR_CALLSTATUS_STOP { self->iSensor_callstatus->Stop(); }
#define SENSOR_CALLSTATUS_IS_RUNNING (self->iSensor_callstatus->IsActive())
#define SENSOR_CALLSTATUS_DESTROY { delete self->iSensor_callstatus; self->iSensor_callstatus = NULL; }
#define SENSOR_CALLSTATUS_CREATE sa_typical_symbian_sensor_create(self->iSensor_callstatus = CSensor_callstatus::NewL(self->ac), "callstatus sensor initialization")
#define SENSOR_CALLSTATUS_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_callstatus
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __epoc_callstatus_hpp__ */
