#ifndef __epoc_cellid_hpp__
#define __epoc_cellid_hpp__

#include "application_config.h"

#if __CELLID_ENABLED__

#include "ac_app_context.h"
#include "log-db.h"
#include "utils_cl2.h"

#include <e32base.h>
#include <etel3rdparty.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_cellid" ;; name
 "ac_AppContext* aAppContext" ;; args
 "iAppContext(aAppContext)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_cellid  \
public: static CSensor_cellid* NewLC(ac_AppContext* aAppContext); \
public: static CSensor_cellid* NewL(ac_AppContext* aAppContext); \
private: CSensor_cellid(ac_AppContext* aAppContext); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_cellid  \
CSensor_cellid* CSensor_cellid::NewLC(ac_AppContext* aAppContext) \
{ \
  CSensor_cellid* obj = new (ELeave) CSensor_cellid(aAppContext); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_cellid* CSensor_cellid::NewL(ac_AppContext* aAppContext) \
{ \
  CSensor_cellid* obj = CSensor_cellid::NewLC(aAppContext); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_cellid::CSensor_cellid(ac_AppContext* aAppContext) : iAppContext(aAppContext) \
{}
/***end***/

NONSHARABLE_CLASS(CSensor_cellid) :
  public CBase
{
  // We do not increment the refcount of the LogDb object, and the
  // framework is responsible for ensuring that the LogDb instance
  // stays alive for at least as long as this sensor object.
  CTOR_DECL_CSensor_cellid;

 public:
  virtual ~CSensor_cellid();

  void PostNewData(const CTelephony::TNetworkInfoV1& aData);

 private:
  // Leaves on OOM or logging error.
  void PostNewDataL(const CTelephony::TNetworkInfoV1& aData);

  void Unregister();

 private:
  ac_AppContext* iAppContext; // not owned

  bb_Closure iClosure;

  CTelephony::TNetworkInfoV1 iOldData;

 private:
  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

  bb_Blackboard* GetBlackboard() const { return ac_get_Blackboard(iAppContext); }
};

#endif // __CELLID_ENABLED__

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __CELLID_ENABLED__
#define DECLARE_SENSOR_cellid CSensor_cellid* iSensor_cellid
#define SENSOR_CELLID_DESTROY DELETE_Z(self->iSensor_cellid)
#define SENSOR_CELLID_CREATE sa_typical_symbian_sensor_create(self->iSensor_cellid = CSensor_cellid::NewL(self->ac), "cellid sensor initialization")
#define SENSOR_CELLID_START SENSOR_CELLID_CREATE
#define SENSOR_CELLID_STOP SENSOR_CELLID_DESTROY
#define SENSOR_CELLID_IS_RUNNING (self->iSensor_cellid != NULL)
#define SENSOR_CELLID_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_cellid
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __epoc_cellid_hpp__ */

/**

epoc-cellid.hpp

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
