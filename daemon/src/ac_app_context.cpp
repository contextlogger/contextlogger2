#include "ac_app_context_private.h"

#include "kr_controller_private.h"
#include "utils_cl2.h"

// --------------------------------------------------
// Symbian private implementation
// --------------------------------------------------

#ifdef __cplusplus
#if defined(__SYMBIAN32__)

#include <f32file.h> // RFs

#if __NEED_TELEPHONY__
#include <etel3rdparty.h> // CTelephony
#endif

#if __NEED_IMEI__
#include "ut_asynccallhandler_epoc.hpp"

void GetImeiCodeL(CTelephony& aTelephony, 
                  CTelephony::TPhoneIdV1& aPhoneId)
{
  TRequestStatus status;
  CTelephony::TPhoneIdV1Pckg phoneIdPckg(aPhoneId);

  // It seems that the event loop must get to run before CTelephony
  // can complete the request, and hence we cannot just invoke
  // aTelephony.GetPhoneId(status, phoneIdPckg). The call would block
  // forever. Hence we are using this nested loop handler thing.
  CAsyncCallHandler* ch = CAsyncCallHandler::NewL(aTelephony);
  ch->GetPhoneId(phoneIdPckg, status);
  User::WaitForRequest(status);
  delete ch;

  User::LeaveIfError(status.Int());
}
#endif

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

#if __NEED_IMEI__
  TBuf8<CTelephony::KPhoneSerialNumberSize + 1> iImeiBuf;
#endif
};

CTOR_IMPL_CAppContext;

void CAppContext::ConstructL()
{
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iFs, iFs.Connect());

#if __NEED_TELEPHONY__
  iTelephony = CTelephony::NewL();
#endif

#if __NEED_IMEI__
  CTelephony::TPhoneIdV1 phoneId;
  GetImeiCodeL(*iTelephony, phoneId);
  // Unlikely to contain exotic characters.
  iImeiBuf.Copy(phoneId.iSerialNumber);
  iImeiBuf.PtrZ();
  logf("IMEI code is '%s'", iImeiBuf.Ptr());
#endif
}

CAppContext::~CAppContext()
{
#if __NEED_TELEPHONY__
  delete iTelephony;
#endif
  SESSION_CLOSE_IF_OPEN(iFs);
}

#endif /* __SYMBIAN32__ */
#endif

// --------------------------------------------------
// common private implementation
// --------------------------------------------------

struct _ac_AppContext
{
  kr_Controller* kr; // not owned
  DEFINE_FOR_SYMBIAN_CXX(CAppContext* plat); // owned
};

EXTERN_C ac_AppContext* ac_AppContext_new(GError** error)
{
  ac_AppContext* self = g_try_new0(ac_AppContext, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

#if defined(__SYMBIAN32__)
  TRAPD(errCode, self->plat = CAppContext::NewL());
  if (G_UNLIKELY(errCode)) {
    ac_AppContext_destroy(self);
    if (error)
      *error = g_error_new(domain_symbian, errCode, 
                           "AppContext Symbian init failure: %s (%d)", 
                           plat_error_strerror(errCode), errCode);
    return NULL;
  }
#endif /* __SYMBIAN32__ */

  return self;
}

EXTERN_C void ac_AppContext_set_controller(ac_AppContext* self, 
					   kr_Controller* kr)
{
  self->kr = kr;
}

EXTERN_C void ac_AppContext_destroy(ac_AppContext* self)
{
  if (self) {
    WHEN_SYMBIAN(delete self->plat);
    g_free(self);
  }
}

// --------------------------------------------------
// global instance
// --------------------------------------------------

static ac_AppContext* iGlobal = NULL; // not owned

EXTERN_C void ac_set_global_AppContext(ac_AppContext* ac)
{
  iGlobal = ac;
}

// --------------------------------------------------
// common public API implementation
// --------------------------------------------------

EXTERN_C ac_AppContext* ac_get_global_AppContext()
{
  return iGlobal;
}

EXTERN_C LogDb* ac_LogDb(ac_AppContext* self)
{
  return self->kr->log;
}

EXTERN_C cf_RcFile* ac_RcFile(ac_AppContext* self)
{
  return self->kr->rcFile;
}

EXTERN_C ConfigDb* ac_ConfigDb(ac_AppContext* self)
{
  return self->kr->configDb;
}

// --------------------------------------------------
// Symbian public API implementation
// --------------------------------------------------

#if defined(__SYMBIAN32__)
#if defined(__cplusplus)

RFs& ac_Fs(ac_AppContext* self)
{
  return self->plat->iFs;
}

#if __NEED_TELEPHONY__
CTelephony& ac_Telephony(ac_AppContext* self)
{
  return *(self->plat->iTelephony);
}
#endif

#if __NEED_IMEI__
EXTERN_C const char* ac_Imei(ac_AppContext* self)
{
  return (const char*)(self->plat->iImeiBuf.Ptr());
}
#endif

#endif /* defined(__cplusplus) */
#endif /* __SYMBIAN32__ */

