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

#include <cntdb.h> // CContactDatabase

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

#if __NEED_CONTACT_DATABASE__
  CContactDatabase* iContactDatabase;
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

#if __NEED_CONTACT_DATABASE__
  iContactDatabase = CContactDatabase::OpenL();
#endif
}

CAppContext::~CAppContext()
{
#if __NEED_CONTACT_DATABASE__
  delete iContactDatabase;
#endif
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
  const char* logdb_dir; // not owned
  char* logdb_file; // owned
  char* log_uploads_dir; // owned
  ac_Registry registry; // initially zeroed
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
      *error = gx_error_new(domain_symbian, errCode, 
			    "AppContext Symbian init failure: %s (%d)", 
			    plat_error_strerror(errCode), errCode);
    return NULL;
  }
#endif /* __SYMBIAN32__ */

  return self;
}

// The result is "static", and not to be freed.
static const gchar* get_config_database_dir(ac_AppContext* self)
{
  const gchar* database_dir = cf_RcFile_get_database_dir(ac_RcFile(self));
  if (!database_dir) {
    database_dir = DATABASE_DIR_DEFAULT; // default value
  }
  return database_dir;
}

EXTERN_C void ac_AppContext_set_controller(ac_AppContext* self, 
					   kr_Controller* kr)
{
  self->kr = kr;
}

EXTERN_C gboolean ac_AppContext_configure(ac_AppContext* self, 
					  GError** error)
{
  const gchar* database_dir = get_config_database_dir(self);

  self->logdb_dir = database_dir;
  logf("log db stored in directory '%s'", self->logdb_dir);

  {
    TRAP_OOM(goto fail,
	     self->logdb_file = g_strdup_printf("%s%s%s",
						self->logdb_dir,
						DIR_SEP, 
						LOGDB_BASENAME)
	     );
  }

  {
    TRAP_OOM(goto fail,
	     self->log_uploads_dir = g_strdup_printf("%s%suploads",
						     database_dir,
						     DIR_SEP)
	     );
  }

  logf("uploads stored in directory '%s'", self->log_uploads_dir);
  return TRUE;

#if defined(__SYMBIAN32__)
 fail:
  if (error) *error = gx_error_no_memory;
  return FALSE;
#endif /* __SYMBIAN32__ */
}

EXTERN_C void ac_AppContext_destroy(ac_AppContext* self)
{
  if (self) {
    WHEN_SYMBIAN(delete self->plat);
    g_free(self->logdb_file);
    g_free(self->log_uploads_dir);
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
  if (!self || !(self->kr)) return NULL;
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

EXTERN_C ac_Registry* ac_get_Registry(ac_AppContext* self)
{
  return &self->registry;
}

EXTERN_C const char* ac_get_logdb_file(ac_AppContext* self)
{
  return self->logdb_file;
}

EXTERN_C const char* ac_get_logdb_dir(ac_AppContext* self)
{
  return self->logdb_dir;
}

EXTERN_C const char* ac_get_log_uploads_dir(ac_AppContext* self)
{
  return self->log_uploads_dir;
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

#if __NEED_CONTACT_DATABASE__
CContactDatabase& ac_ContactDatabase(ac_AppContext* self)
{
  return *(self->plat->iContactDatabase);
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

/**

ac_app_context.cpp

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
