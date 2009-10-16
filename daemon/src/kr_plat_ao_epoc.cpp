#include "kr_plat_ao.h"

#include "er_errors.h"
//#include "kr_controller_private.h"
#include "kr_diskspace.h"
#include "ut_diskspace_epoc.hpp"
#include "utils_cl2.h"

// --------------------------------------------------
// disk space observing
// --------------------------------------------------

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CDiskObserver" ;; name
 "" ;; args
 "" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CDiskObserver  \
public: static CDiskObserver* NewLC(); \
public: static CDiskObserver* NewL(); \
private: CDiskObserver(); \
private: void ConstructL();

#define CTOR_IMPL_CDiskObserver  \
CDiskObserver* CDiskObserver::NewLC() \
{ \
  CDiskObserver* obj = new (ELeave) CDiskObserver(); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CDiskObserver* CDiskObserver::NewL() \
{ \
  CDiskObserver* obj = CDiskObserver::NewLC(); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CDiskObserver::CDiskObserver() \
{}
/***end***/
NONSHARABLE_CLASS(CDiskObserver) : public CBase, public MDiskSpace
{
  CTOR_DECL_CDiskObserver;

 public:
  ~CDiskObserver();

 private:
  virtual void DiskSpaceNotify(TInt aDrive, TInt errCode);

 private:
  DEF_SESSION(RFs, iFs);
  CDiskSpaceNotifier* iNotifier;
};

CTOR_IMPL_CDiskObserver;

void CDiskObserver::ConstructL()
{
  TChar driveLetter = DATABASE_DRIVE_LETTER;
  TInt driveNum = 0;
  User::LeaveIfError(RFs::CharToDrive(driveLetter, driveNum));

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iFs, iFs.Connect());
  iNotifier = CDiskSpaceNotifier::NewL(iFs, driveNum,
				       this, 
				       (TInt64)DATABASE_VOLUME_THRESHOLD);
}

CDiskObserver::~CDiskObserver()
{
  delete iNotifier;
  SESSION_CLOSE_IF_OPEN(iFs);
}

void CDiskObserver::DiskSpaceNotify(TInt aDrive, TInt errCode)
{
  (void)aDrive;
  if (errCode) {
    // We are not really expecting any errors here, not any that are
    // in the API docs, anyway.
    logf("unexpected error in disk observer: %d", errCode);
  } else {
    // There are a number of causes why we might get an
    // RFs::NotifyDiskSpace() event, so we have to do some further
    // checking here.
    TRAPD(errCode, CheckLoggingMediumReadyL(iFs));
    if (errCode) {
      // One issue here is that we do not really know if it is safe to
      // try to log this error to the database. Possibly not, so the
      // debug log, if any, shall have to do.
      ex_log_fatal_error(errCode);
    }
  }
}

// --------------------------------------------------
// auxiliary controller
// --------------------------------------------------

struct _kr_PlatAo {
  CDiskObserver* iDiskObserver;
};

extern "C" kr_PlatAo* kr_PlatAo_new(GError** error)
{
  kr_PlatAo* self = g_try_new0(kr_PlatAo, 1);
  if (!self) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  TRAPD(errCode, self->iDiskObserver = CDiskObserver::NewL());
  if (errCode) {
    kr_PlatAo_destroy(self);
    if (error)
      *error = g_error_new(domain_symbian, errCode, 
			   "disk observer creation failure: %s (%d)", 
			   plat_error_strerror(errCode), errCode);
    return NULL;
  }

  return self;
}

extern "C" void kr_PlatAo_destroy(kr_PlatAo* self)
{
  if (self) {
    delete self->iDiskObserver;
    g_free(self);
  }
}
