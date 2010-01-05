#include "kr_plat_ao.h"

#include "ac_app_context.h"
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
  int log_disk_threshold = ac_STATIC_GET(log_disk_threshold);
  iNotifier = CDiskSpaceNotifier::NewL(iFs, driveNum,
				       this, 
				       (TInt64)log_disk_threshold);
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

/**

kr_plat_ao_epoc.cpp

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
