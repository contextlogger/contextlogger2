#ifndef __epoc_smsevent_hpp__
#define __epoc_smsevent_hpp__

#include "application_config.h"

#if __SMSEVENT_ENABLED__

#include "ac_app_context.h"
#include "ut_sms_epoc.hpp"
#include "utils_cl2.h"

#include <e32base.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_smsevent" ;; name
 "ac_AppContext* aAppContext" ;; args
 "iAppContext(aAppContext)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_smsevent  \
public: static CSensor_smsevent* NewLC(ac_AppContext* aAppContext); \
public: static CSensor_smsevent* NewL(ac_AppContext* aAppContext); \
private: CSensor_smsevent(ac_AppContext* aAppContext); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_smsevent  \
CSensor_smsevent* CSensor_smsevent::NewLC(ac_AppContext* aAppContext) \
{ \
  CSensor_smsevent* obj = new (ELeave) CSensor_smsevent(aAppContext); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_smsevent* CSensor_smsevent::NewL(ac_AppContext* aAppContext) \
{ \
  CSensor_smsevent* obj = CSensor_smsevent::NewLC(aAppContext); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_smsevent::CSensor_smsevent(ac_AppContext* aAppContext) : iAppContext(aAppContext) \
{}
/***end***/

NONSHARABLE_CLASS(CSensor_smsevent) :
  public CBase,
  public i_handle_received_sms
{
  CTOR_DECL_CSensor_smsevent;

 public:
  virtual ~CSensor_smsevent();

  // Produces a leave or a GError if starting fails. No harm calling
  // this if already started.
  gboolean StartL(GError** error);

  // Stops observing for changes and logging them. No harm calling
  // this if already stopped.
  void Stop();

  TBool IsActive() const { return (iSmsEventNotifier != NULL); }

 private: // i_handle_received_sms
  virtual void handle_reception(const TMsvId& entry_id, const TMsvId& folder_id, 
				const TDesC& sender, CRichText& body); 
  virtual void handle_sending(const TMsvId& entry_id, 
			      const TDesC& sender, CRichText& body);
  virtual void handle_error(TInt aError);
  virtual void handle_close();
  
 private:

  void ActivateL();

  void Disactivate();

  void LogEvent(const char* evType, const TDesC& aTelNo, CRichText& aBody);

 private:

  ac_AppContext* iAppContext; // not owned

  CSmsEventNotifier* iSmsEventNotifier;

 private:

  LogDb* GetLogDb() const { return ac_LogDb(iAppContext); }

};

#endif // __SMSEVENT_ENABLED__

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __SMSEVENT_ENABLED__
#define DECLARE_SENSOR_smsevent CSensor_smsevent* iSensor_smsevent
#define SENSOR_SMSEVENT_START sa_typical_symbian_sensor_start(self->iSensor_smsevent, "failed to start smsevent scanning")
#define SENSOR_SMSEVENT_STOP { self->iSensor_smsevent->Stop(); }
#define SENSOR_SMSEVENT_IS_RUNNING (self->iSensor_smsevent->IsActive())
#define SENSOR_SMSEVENT_DESTROY { delete self->iSensor_smsevent; self->iSensor_smsevent = NULL; }
#define SENSOR_SMSEVENT_CREATE sa_typical_symbian_sensor_create(self->iSensor_smsevent = CSensor_smsevent::NewL(self->ac), "smsevent sensor initialization")
#define SENSOR_SMSEVENT_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys
#else
#define DECLARE_SENSOR_smsevent
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __epoc_smsevent_hpp__ */

/**

epoc-smsevent.hpp

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
