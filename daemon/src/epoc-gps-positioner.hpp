#ifndef __epoc_gps_positioner_hpp__
#define __epoc_gps_positioner_hpp__

#include "epoc-gps-observer.hpp"

#include "ac_app_context.h"
#include "epoc-ao-gerror.hpp"
#include "ld_log_db.h"
#include "utils_cl2.h"

#include <e32std.h>
#include <lbs.h> // link against lbs.lib
#include <lbssatellite.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)

(ctor-defines/spec
 "CPositioner_gps"
 "RPositionServer& aPositionServer, MObserver_gps& aObserver, TPositionModuleId aModuleId, TInt aUpdateIntervalSecs, TInt aUpdateTimeoutSecs"
 "CActiveRunG(EPriorityStandard), iPositionServer(aPositionServer), iObserver(aObserver), iModuleId(aModuleId), iUpdateIntervalSecs(aUpdateIntervalSecs), iUpdateTimeoutSecs(aUpdateTimeoutSecs)"
 "CActiveScheduler::Add(this);"
 #t)
 ***/
#define CTOR_DECL_CPositioner_gps  \
public: static CPositioner_gps* NewLC(RPositionServer& aPositionServer, MObserver_gps& aObserver, TPositionModuleId aModuleId, TInt aUpdateIntervalSecs, TInt aUpdateTimeoutSecs); \
public: static CPositioner_gps* NewL(RPositionServer& aPositionServer, MObserver_gps& aObserver, TPositionModuleId aModuleId, TInt aUpdateIntervalSecs, TInt aUpdateTimeoutSecs); \
private: CPositioner_gps(RPositionServer& aPositionServer, MObserver_gps& aObserver, TPositionModuleId aModuleId, TInt aUpdateIntervalSecs, TInt aUpdateTimeoutSecs); \
private: void ConstructL();

#define CTOR_IMPL_CPositioner_gps  \
CPositioner_gps* CPositioner_gps::NewLC(RPositionServer& aPositionServer, MObserver_gps& aObserver, TPositionModuleId aModuleId, TInt aUpdateIntervalSecs, TInt aUpdateTimeoutSecs) \
{ \
  CPositioner_gps* obj = new (ELeave) CPositioner_gps(aPositionServer, aObserver, aModuleId, aUpdateIntervalSecs, aUpdateTimeoutSecs); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CPositioner_gps* CPositioner_gps::NewL(RPositionServer& aPositionServer, MObserver_gps& aObserver, TPositionModuleId aModuleId, TInt aUpdateIntervalSecs, TInt aUpdateTimeoutSecs) \
{ \
  CPositioner_gps* obj = CPositioner_gps::NewLC(aPositionServer, aObserver, aModuleId, aUpdateIntervalSecs, aUpdateTimeoutSecs); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CPositioner_gps::CPositioner_gps(RPositionServer& aPositionServer, MObserver_gps& aObserver, TPositionModuleId aModuleId, TInt aUpdateIntervalSecs, TInt aUpdateTimeoutSecs) : CActiveRunG(EPriorityStandard), iPositionServer(aPositionServer), iObserver(aObserver), iModuleId(aModuleId), iUpdateIntervalSecs(aUpdateIntervalSecs), iUpdateTimeoutSecs(aUpdateTimeoutSecs) \
{CActiveScheduler::Add(this);}
/***end***/

// An active object for dealing with a single RPositioner.
NONSHARABLE_CLASS(CPositioner_gps) : 
  public CActiveRunG
{
 public:

  virtual ~CPositioner_gps();

  CTOR_DECL_CPositioner_gps;

 public:

  // Makes the next positioning request.
  void MakeRequest();

  TInt StatusCode() const { return iStatus.Int(); }

  const TPositionSatelliteInfo& PositionInfo() const { return iPositionInfo; }

  TPositionModuleId ModuleId() const { return iModuleId; }
  
 private: // CActiveRunG

  virtual gboolean RunGL(GError** error);
  
  virtual const char* Description();
  
  virtual void DoCancel();

 private:

  RPositionServer& iPositionServer;
  MObserver_gps& iObserver;
  TPositionModuleId iModuleId;

  DEF_SESSION(RPositioner, iPositioner);

  TPositionUpdateOptions iUpdateOptions;

  // Supertype of both TPositionCourseInfo and TPositionInfo.
  TPositionSatelliteInfo iPositionInfo;

  TInt iUpdateIntervalSecs;
  TInt iUpdateTimeoutSecs;
};

#endif /* __epoc_gps_positioner_hpp__ */

/**

Copyright 2009-2011 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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
