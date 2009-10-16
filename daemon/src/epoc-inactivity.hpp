#ifndef __epoc_inactivity_hpp__
#define __epoc_inactivity_hpp__

#include "application_config.h"

#if __INACTIVITY_ENABLED__

#include "ac_app_context.h"
#include "utils_cl2.h"

#include <e32std.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_inactivity" ;; name
 "ac_AppContext* aC" ;; args
 "CActive(EPriorityHigh), iC(aC)" ;; inits
 "CActiveScheduler::Add(this);" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_inactivity  \
public: static CSensor_inactivity* NewLC(ac_AppContext* aC); \
public: static CSensor_inactivity* NewL(ac_AppContext* aC); \
private: CSensor_inactivity(ac_AppContext* aC); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_inactivity  \
CSensor_inactivity* CSensor_inactivity::NewLC(ac_AppContext* aC) \
{ \
  CSensor_inactivity* obj = new (ELeave) CSensor_inactivity(aC); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_inactivity* CSensor_inactivity::NewL(ac_AppContext* aC) \
{ \
  CSensor_inactivity* obj = CSensor_inactivity::NewLC(aC); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_inactivity::CSensor_inactivity(ac_AppContext* aC) : CActive(EPriorityHigh), iC(aC) \
{CActiveScheduler::Add(this);}
/***end***/
NONSHARABLE_CLASS(CSensor_inactivity) : public CActive
{
  CTOR_DECL_CSensor_inactivity;

 public:
  virtual ~CSensor_inactivity();

  gboolean StartL(GError** error);

  void Stop();

 private: // CActive
  virtual void RunL();

  virtual void DoCancel();

 private:
  void MakeRequest();
  void GetState();

 private:
  ac_AppContext* iC;
  DEF_SESSION(RTimer, iTimer);

  enum EState { EUnknown, EIdle, EActive };
  EState iCurrentState;
};

#endif // __INACTIVITY_ENABLED__

#endif /* __epoc_inactivity_hpp__ */
