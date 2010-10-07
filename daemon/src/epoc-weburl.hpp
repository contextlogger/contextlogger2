#ifndef __epoc_weburl_hpp__
#define __epoc_weburl_hpp__

#include "application_config.h"

#if __WEBURL_ENABLED__

#include "ac_app_context.h"
#include "ld_log_db.h"
#include "utils_cl2.h"

#include <badesca.h>

#if __HAVE_AHLECLIENT_LIB__
// Note that CAHLE relies on a DLL that is not available on 5th
// edition devices. Hence dynamic linking fails, and the logger will
// not even start.
#include <ahleclientobserver.h> 
#include <ahle.h>
typedef CAHLE AhleClientType;
typedef MAHLEClientObserver AhleObserverType;
#elif __HAVE_AHLE2CLIENT_LIB__
#include <ahleobserver.h>
class MAHLEGenericAPI;
typedef MAHLEGenericAPI AhleClientType;
typedef MAHLEObserver AhleObserverType;
#endif

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_weburl" ;; name
 "ac_AppContext* aAppContext" ;; args
 "iAppContext(aAppContext)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_weburl  \
public: static CSensor_weburl* NewLC(ac_AppContext* aAppContext); \
public: static CSensor_weburl* NewL(ac_AppContext* aAppContext); \
private: CSensor_weburl(ac_AppContext* aAppContext); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_weburl  \
CSensor_weburl* CSensor_weburl::NewLC(ac_AppContext* aAppContext) \
{ \
  CSensor_weburl* obj = new (ELeave) CSensor_weburl(aAppContext); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_weburl* CSensor_weburl::NewL(ac_AppContext* aAppContext) \
{ \
  CSensor_weburl* obj = CSensor_weburl::NewLC(aAppContext); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_weburl::CSensor_weburl(ac_AppContext* aAppContext) : iAppContext(aAppContext) \
{}
/***end***/

NONSHARABLE_CLASS(CSensor_weburl) :
  public CBase,
  public AhleObserverType
{
  CTOR_DECL_CSensor_weburl;

 public:
  virtual ~CSensor_weburl();

 private:
  ac_AppContext* iAppContext; // not owned

  AhleClientType* iAhle; // owned

  CDesCArray* iOldUrlArray; // owned

 private: // AhleObserverType
  virtual void AdaptiveListChanged(TInt aError);

 private:
  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

  void LogDataL();
};

#endif // __WEBURL_ENABLED__

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __WEBURL_ENABLED__
#define DECLARE_SENSOR_weburl CSensor_weburl* iSensor_weburl
#define SENSOR_WEBURL_DESTROY DELETE_Z(self->iSensor_weburl)
#define SENSOR_WEBURL_CREATE sa_typical_symbian_sensor_create(self->iSensor_weburl = CSensor_weburl::NewL(self->ac), "weburl sensor initialization")
#define SENSOR_WEBURL_START SENSOR_WEBURL_CREATE
#define SENSOR_WEBURL_STOP SENSOR_WEBURL_DESTROY
#define SENSOR_WEBURL_IS_RUNNING (self->iSensor_weburl != NULL)
#define SENSOR_WEBURL_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_weburl
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __epoc_weburl_hpp__ */

/**

epoc-weburl.hpp

Copyright 2009-2010 Helsinki Institute for Information Technology
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
