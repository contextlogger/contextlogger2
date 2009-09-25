#ifndef __epoc_cellid_hpp__
#define __epoc_cellid_hpp__

#include "application_config.h"

#if __CELLID_ENABLED__

#include "epoc-ao-gerror.hpp"
#include "log-db.h"
#include "utils_cl2.h"

#include <e32base.h>
#include <etel3rdparty.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_cellid" ;; name
 "LogDb* aLogDb" ;; args
 "CActiveRunG(EPriorityStandard), iLogDb(aLogDb), iDataDes(iData)" ;; inits
 "CActiveScheduler::Add(this);" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_cellid  \
public: static CSensor_cellid* NewLC(LogDb* aLogDb); \
public: static CSensor_cellid* NewL(LogDb* aLogDb); \
private: CSensor_cellid(LogDb* aLogDb); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_cellid  \
CSensor_cellid* CSensor_cellid::NewLC(LogDb* aLogDb) \
{ \
  CSensor_cellid* obj = new (ELeave) CSensor_cellid(aLogDb); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_cellid* CSensor_cellid::NewL(LogDb* aLogDb) \
{ \
  CSensor_cellid* obj = CSensor_cellid::NewLC(aLogDb); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_cellid::CSensor_cellid(LogDb* aLogDb) : CActiveRunG(EPriorityStandard), iLogDb(aLogDb), iDataDes(iData) \
{CActiveScheduler::Add(this);}
/***end***/

// If we should ever run into errors of a transient nature in
// accessing CTelephony, we might want to have an internal retry timer
// within this object. But I am not really expecting coming across
// such errors.
NONSHARABLE_CLASS(CSensor_cellid) :
  public CActiveRunG
{
  // We do not increment the refcount of the LogDb object, and the
  // framework is responsible for ensuring that the LogDb instance
  // stays alive for at least as long as this sensor object.
  CTOR_DECL_CSensor_cellid;

public:
  virtual ~CSensor_cellid();

  // Produces a leave or a GError if starting fails, in which case the
  // framework shall merely log this, and leave the particular sensor
  // unstarted. No harm calling this if already started.
  gboolean StartL(GError** error);

  // Stops observing for changes and logging them. No harm calling this if already stopped.
  void Stop();

private:

  void MakeRequest();

  void SetTimer();

  void HandleTimerL();

  gboolean HandleReadGL(GError** error);

  virtual gboolean RunGL(GError** error);
  
  virtual const char* Description();

  virtual void DoCancel();

private:

  LogDb* iLogDb; // not owned

  CTelephony::TNetworkInfoV1 iData;

  CTelephony::TNetworkInfoV1Pckg iDataDes;

  CTelephony *iTelephony; // owned

  TInt iNumScanFailures;

  CTelephony::TNetworkInfoV1 iOldData;

  enum TState {
    EInactive = 0,
    EQuerying,
    ERetryWaiting
  };
  TState iState;

  DEF_SESSION(RTimer, iTimer);

};

#endif // __CELLID_ENABLED__

#endif /* __epoc_cellid_hpp__ */
