#ifndef __ac_app_context_epoc_hpp__
#define __ac_app_context_epoc_hpp__

// This is not a standalone header. It exists as a separate file only
// to avoid platform-specific clutter in the main header files.

#include "utils_cl2.h" // DEF_SESSION

#include <f32file.h> // RFs

#if __NEED_TELEPHONY__
#include <etel3rdparty.h> // CTelephony
#endif

#include <cntdb.h> // CContactDatabase

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CAppContext" ;; name
 "" ;; args
 "" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CAppContext  \
public: static CAppContext* NewLC(); \
public: static CAppContext* NewL(); \
private: CAppContext(); \
private: void ConstructL();

#define CTOR_IMPL_CAppContext  \
CAppContext* CAppContext::NewLC() \
{ \
  CAppContext* obj = new (ELeave) CAppContext(); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CAppContext* CAppContext::NewL() \
{ \
  CAppContext* obj = CAppContext::NewLC(); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CAppContext::CAppContext() \
{}
/***end***/

NONSHARABLE_CLASS(CAppContext) : public CBase
{
  CTOR_DECL_CAppContext;

 public:
  ~CAppContext();

 public: // internally public
  DEF_SESSION(RFs, iFs);

#if __NEED_TELEPHONY__
  CTelephony* iTelephony;
#endif

#if __NEED_CONTACT_DATABASE__
  CContactDatabase* iContactDatabase;
#endif
};

#endif /* __ac_app_context_epoc_hpp__ */

