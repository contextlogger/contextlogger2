#ifndef __epoc_indicator_hpp__
#define __epoc_indicator_hpp__

#include "application_config.h"

#if __INDICATOR_ENABLED__

#include "ac_app_context.h"
#include "utils_cl2.h"

#include <e32base.h>
#include <etel3rdparty.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_indicator" ;; name
 "ac_AppContext* aAppContext" ;; args
 "CActive(EPriorityStandard), iAppContext(aAppContext), iIndicatorDes(iIndicator)" ;; inits
 "CActiveScheduler::Add(this);" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_indicator  \
public: static CSensor_indicator* NewLC(ac_AppContext* aAppContext); \
public: static CSensor_indicator* NewL(ac_AppContext* aAppContext); \
private: CSensor_indicator(ac_AppContext* aAppContext); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_indicator  \
CSensor_indicator* CSensor_indicator::NewLC(ac_AppContext* aAppContext) \
{ \
  CSensor_indicator* obj = new (ELeave) CSensor_indicator(aAppContext); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_indicator* CSensor_indicator::NewL(ac_AppContext* aAppContext) \
{ \
  CSensor_indicator* obj = CSensor_indicator::NewLC(aAppContext); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_indicator::CSensor_indicator(ac_AppContext* aAppContext) : CActive(EPriorityStandard), iAppContext(aAppContext), iIndicatorDes(iIndicator) \
{CActiveScheduler::Add(this);}
/***end***/

NONSHARABLE_CLASS(CSensor_indicator) :
  public CActive
{
  CTOR_DECL_CSensor_indicator;

 public:
  virtual ~CSensor_indicator();

  // Produces a leave or a GError if starting fails. No harm calling
  // this if already started.
  gboolean StartL(GError** error);

  // Stops observing for changes and logging them. No harm calling
  // this if already stopped.
  void Stop();

 private: // CActive

  virtual void RunL();
  
  virtual void DoCancel();
  
 private:

  void MakeRequest();

  void SetTimer();
  
  void HandleTimer();
  
  void HandleRead();

 private:

  ac_AppContext* iAppContext; // not owned

  CTelephony::TIndicatorV1 iOldIndicator;

  CTelephony::TIndicatorV1 iIndicator;

  CTelephony::TIndicatorV1Pckg iIndicatorDes;

  CTelephony *iTelephony; // owned

  // Actually, we probably do not require retry support in this
  // sensor, but here it is anyway, for now.

  TInt iNumScanFailures;

  enum TState {
    EInactive = 0,
    EQuerying,
    ERetryWaiting
  };
  TState iState;

  DEF_SESSION(RTimer, iTimer);

};

#endif // __INDICATOR_ENABLED__

#endif /* __epoc_indicator_hpp__ */
