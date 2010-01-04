#ifndef __epoc_cellid_hpp__
#define __epoc_cellid_hpp__

#include "application_config.h"

#if __CELLID_ENABLED__

#include "epoc-ao-gerror.hpp"
#include "log-db.h"
#include "utils_cl2.h"

#include <e32base.h>
#include <etel3rdparty.h>

#include <glib.h>

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CSensor_cellid" ;; name
 "LogDb* aLogDb" ;; args
 "CActiveRunG(EPriorityStandard), iLogDb(aLogDb), iDataDes(iData)" ;; inits
 "CActiveScheduler::Add(this);" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CSensor_cellid  \
public: static CSensor_cellid* NewLC(LogDb* aLogDb); \
public: static CSensor_cellid* NewL(LogDb* aLogDb); \
private: CSensor_cellid(LogDb* aLogDb); \
private: void ConstructL();

#define CTOR_IMPL_CSensor_cellid  \
CSensor_cellid* CSensor_cellid::NewLC(LogDb* aLogDb) \
{ \
  CSensor_cellid* obj = new (ELeave) CSensor_cellid(aLogDb); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSensor_cellid* CSensor_cellid::NewL(LogDb* aLogDb) \
{ \
  CSensor_cellid* obj = CSensor_cellid::NewLC(aLogDb); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSensor_cellid::CSensor_cellid(LogDb* aLogDb) : CActiveRunG(EPriorityStandard), iLogDb(aLogDb), iDataDes(iData) \
{CActiveScheduler::Add(this);}
/***end***/

NONSHARABLE_CLASS(CSensor_cellid) :
  public CActiveRunG
{
  // We do not increment the refcount of the LogDb object, and the
  // framework is responsible for ensuring that the LogDb instance
  // stays alive for at least as long as this sensor object.
  CTOR_DECL_CSensor_cellid;

public:
  virtual ~CSensor_cellid();

  // Produces a leave or a GError if starting fails, in which case the
  // framework shall merely log this, and leave the particular sensor
  // unstarted. No harm calling this if already started.
  gboolean StartL(GError** error);

  // Stops observing for changes and logging them. No harm calling this if already stopped.
  void Stop();

private:

  void MakeRequest();

  void SetTimer();

  void HandleTimerL();

  gboolean HandleReadGL(GError** error);

  virtual gboolean RunGL(GError** error);
  
  virtual const char* Description();

  virtual void DoCancel();

private:

  LogDb* iLogDb; // not owned

  CTelephony::TNetworkInfoV1 iData;

  CTelephony::TNetworkInfoV1Pckg iDataDes;

  CTelephony *iTelephony; // owned

  TInt iNumScanFailures;

  CTelephony::TNetworkInfoV1 iOldData;

  enum TState {
    EInactive = 0,
    EQuerying,
    ERetryWaiting
  };
  TState iState;

  DEF_SESSION(RTimer, iTimer);

};

#endif // __CELLID_ENABLED__

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
