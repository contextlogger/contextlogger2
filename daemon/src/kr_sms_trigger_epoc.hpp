#ifndef __kr_sms_trigger_epoc_hpp__
#define __kr_sms_trigger_epoc_hpp__

#include "ac_app_context.h"
#include "utils_cl2.h"

#include <e32base.h>
#include <es_sock.h>
#include <gsmubuf.h>
#include <gsmumsg.h>
#include <smsuaddr.h>

NONSHARABLE_CLASS(MSmsTrigger)
{
 public:
  virtual void SmsTriggerReceivedL(TInt aSecs) = 0;
  virtual void SmsTriggerErrorL(TInt errCode) = 0;
  virtual void SmsTriggerLeave(TInt errCode) = 0;
};

/***koog 
(require racket/class)
(require codegen/symbian-ctor)

(ctor-defines (new 
  (class ctor%
    (super-new)
    (define/override (class-name) "CSmsTrigger")
    (define/override (args/string) "ac_AppContext* aAppContext, MSmsTrigger& aObserver")
    (define/override (init-expr) "CActive(EPriorityStandard), iAppContext(aAppContext), iObserver(aObserver), iFs(ac_Fs(aAppContext))")
    (define/override (ctor-code) "CActiveScheduler::Add(this);")
   )))
 ***/
#define CTOR_DECL_CSmsTrigger  \
public: static CSmsTrigger* NewLC(ac_AppContext* aAppContext, MSmsTrigger& aObserver); \
public: static CSmsTrigger* NewL(ac_AppContext* aAppContext, MSmsTrigger& aObserver); \
private: CSmsTrigger(ac_AppContext* aAppContext, MSmsTrigger& aObserver); \
private: void ConstructL();

#define CTOR_IMPL_CSmsTrigger  \
CSmsTrigger* CSmsTrigger::NewLC(ac_AppContext* aAppContext, MSmsTrigger& aObserver) \
{ \
  CSmsTrigger* obj = new (ELeave) CSmsTrigger(aAppContext, aObserver); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CSmsTrigger* CSmsTrigger::NewL(ac_AppContext* aAppContext, MSmsTrigger& aObserver) \
{ \
  CSmsTrigger* obj = CSmsTrigger::NewLC(aAppContext, aObserver); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CSmsTrigger::CSmsTrigger(ac_AppContext* aAppContext, MSmsTrigger& aObserver) : CActive(EPriorityStandard), iAppContext(aAppContext), iObserver(aObserver), iFs(ac_Fs(aAppContext)) \
{CActiveScheduler::Add(this);}
/***end***/

NONSHARABLE_CLASS(CSmsTrigger) : 
  public CActive
{
  CTOR_DECL_CSmsTrigger;

 public:
  ~CSmsTrigger();

 private:
  ac_AppContext* iAppContext;
  MSmsTrigger& iObserver;
  RFs& iFs;
  DEF_SESSION(RSocketServ, iSocketServ);
  DEF_SESSION(RSocket, iSocket);
  TSmsAddr iSmsAddr;
  TPckgBuf<TUint> iPckgBuf;
  TBool iRead; // Ioctl read request (otherwise succeeded request)

 private: // CActive
  virtual void RunL();
  virtual void DoCancel();
  virtual TInt RunError(TInt errCode);

 private:
  void MakeRequest();
};

#endif /* __kr_sms_trigger_epoc_hpp__ */

/**

Copyright 2011 Helsinki Institute for Information Technology (HIIT)
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
