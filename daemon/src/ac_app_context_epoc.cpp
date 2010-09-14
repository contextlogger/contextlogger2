// Not a standalone file. It exists as a separate file only to avoid
// platform-specific clutter in the primary implementation file.

#include "utils_cl2.h" // DEF_SESSION

#include <f32file.h> // RFs

#include <etel3rdparty.h> // CTelephony

#if __NEED_CONTACT_DATABASE__
#include <cntdb.h> // CContactDatabase
#endif

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

NONSHARABLE_CLASS(CAppContextImpl) : public CBase
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
};

CTOR_IMPL_CAppContextImpl;

void CAppContextImpl::ConstructL()
{
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iFs, iFs.Connect());

  iTelephony = CTelephony::NewL();

#if __NEED_CONTACT_DATABASE__
  iContactDatabase = CContactDatabase::OpenL();
#endif
}

CAppContextImpl::~CAppContextImpl()
{
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
