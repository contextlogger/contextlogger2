#ifndef __epoc_httpurl_hpp__
#define __epoc_httpurl_hpp__

#include "application_config.h"

#if __HTTPURL_ENABLED__

#include "ac_app_context.h"
#include "ld_log_db.h"
#include "utils_cl2.h"

#include "common/epoc-session.hpp"

#include <badesca.h>
#include <e32property.h>

NONSHARABLE_CLASS(CSensor_httpurl) :
  public CActive
{
 public: 
  static CSensor_httpurl* NewLC(ac_AppContext* aAppContext);
  static CSensor_httpurl* NewL(ac_AppContext* aAppContext);
  virtual ~CSensor_httpurl();

 private:
  CSensor_httpurl(ac_AppContext* aAppContext);
  void ConstructL();

 private:
  ac_AppContext* iAppContext; // not owned
  DEF_SESSION(RProperty, iProperty);

 private:
  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

  void MakeRequest();

 private: // CActive
  virtual void DoCancel();
  virtual TInt RunError(TInt errCode);
  virtual void RunL();
};

#endif // __HTTPURL_ENABLED__

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __HTTPURL_ENABLED__
#define DECLARE_SENSOR_httpurl CSensor_httpurl* iSensor_httpurl
#define SENSOR_HTTPURL_DESTROY DELETE_Z(self->iSensor_httpurl)
#define SENSOR_HTTPURL_CREATE 
#define SENSOR_HTTPURL_START sa_typical_symbian_sensor_create(self->iSensor_httpurl = CSensor_httpurl::NewL(self->ac), "httpurl sensor initialization")
#define SENSOR_HTTPURL_STOP SENSOR_HTTPURL_DESTROY
#define SENSOR_HTTPURL_IS_RUNNING (self->iSensor_httpurl != NULL)
#define SENSOR_HTTPURL_RECONFIGURE(key, value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_httpurl
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __epoc_httpurl_hpp__ */

/**

epoc-httpurl.hpp

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
