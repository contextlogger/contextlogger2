#ifndef __epoc_flightmode_hpp__
#define __epoc_flightmode_hpp__

#include "application_config.h"

#if __FLIGHTMODE_ENABLED__

#include "epoc-ao-gerror.hpp"
#include "log-db.h"

#include <e32base.h>
#include <etel3rdparty.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_flightmode" ;; name
 "LogDb* aLogDb" ;; args
 "CActiveLogErrG(EPriorityStandard), iLogDb(aLogDb), iDataDes(iData)" ;; inits
 "CActiveScheduler::Add(this);" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_flightmode  \
public: static CSensor_flightmode* NewLC(LogDb* aLogDb); \
public: static CSensor_flightmode* NewL(LogDb* aLogDb); \
private: CSensor_flightmode(LogDb* aLogDb); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_flightmode  \
CSensor_flightmode* CSensor_flightmode::NewLC(LogDb* aLogDb) \
{ \
  CSensor_flightmode* obj = new (ELeave) CSensor_flightmode(aLogDb); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_flightmode* CSensor_flightmode::NewL(LogDb* aLogDb) \
{ \
  CSensor_flightmode* obj = CSensor_flightmode::NewLC(aLogDb); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_flightmode::CSensor_flightmode(LogDb* aLogDb) : CActiveLogErrG(EPriorityStandard), iLogDb(aLogDb), iDataDes(iData) \
{CActiveScheduler::Add(this);}
/***end***/

// If we should ever run into errors of a transient nature in
// accessing CTelephony, we might want to have an internal retry timer
// within this object. But I am not really expecting coming across
// such errors.
NONSHARABLE_CLASS(CSensor_flightmode) :
  public CActiveLogErrG
{
  // We do not increment the refcount of the LogDb object, and the
  // framework is responsible for ensuring that the LogDb instance
  // stays alive for at least as long as this sensor object.
  CTOR_DECL_CSensor_flightmode;

public:
  virtual ~CSensor_flightmode();

  // Produces a leave or a GError if starting fails, in which case the
  // framework shall merely log this, and leave the particular sensor
  // unstarted. No harm calling this if already started.
  gboolean StartL(GError** error);

  // Stops observing for changes and logging them. No harm calling
  // this if already stopped.
  void Stop();

private:

  void MakeRequest();

  virtual void RunL();
  
  virtual const char* Description();

  virtual void DoCancel();

private:

  CTelephony* iTelephony; // owned

  LogDb* iLogDb; // not owned

  CTelephony::TFlightModeV1 iData;

  CTelephony::TFlightModeV1Pckg iDataDes;

};

#endif // __FLIGHTMODE_ENABLED__

#endif /* __epoc_flightmode_hpp__ */
