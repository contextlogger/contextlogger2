/*
 !concept {:name => "Multipart HTTP posting",
   :desc => "Multipart HTTP(S) POSTing using the Symbian HTTP client API."}
*/

#ifndef __up_poster_epoc_hpp__
#define __up_poster_epoc_hpp__

#include "application_config.h"

#if __FEATURE_UPLOADER__

#include "utils_cl2.h"

#include "common/epoc_rs_buf8.hpp"

#include <e32base.h>
#include <es_sock.h>
#include <http.h>
#include <s32file.h>

class MDataSupplier :
  public MHTTPDataSupplier
{
public:
  virtual void SetHttpTransaction(RHTTPTransaction* aTransaction) = 0;
};

NONSHARABLE_CLASS(RBufferDataSupplier) :
  public MDataSupplier
{
 public:
  void Set(HBufC8* aData) { Close(); iData = aData; }
  void Close() { delete iData; iData = NULL; }
 public: // MHTTPDataSupplier
  TBool GetNextDataPart(TPtrC8& aDataPart);
  void ReleaseData();
  TInt Reset();
  TInt OverallDataSize();
 public: // MDataSupplier
  void SetHttpTransaction(RHTTPTransaction* aTransaction) {}
 private: // property
  HBufC8* iData; // owned
};

#define KMultiPartBoundaryMaxLen 48

NONSHARABLE_CLASS(CFileDataSupplier) :
  public CBase, public MDataSupplier
{
 public:
  //CFileDataSupplier();
  virtual ~CFileDataSupplier();
  void OpenL(const TDesC& aFileName);
  void Close();
  const TDesC8& Boundary() const; // at most KMultiPartBoundaryMaxLen bytes
 public: // MHTTPDataSupplier
  TBool GetNextDataPart(TPtrC8& aDataPart); // May leave!
  void ReleaseData(); // May leave!
  TInt Reset();
  TInt OverallDataSize();
 public: // MDataSupplier
  void SetHttpTransaction(RHTTPTransaction* aTransaction) { iTransaction = aTransaction; }
 private: // property
  TFileName iFileName;
  DEF_SESSION(RFs, iFs);
  DEF_SESSION(RFile, iFile);
  RRsBuf8 iPrelude;
  RRsBuf8 iEpilogue;
  TBuf8<512> iBuffer;
  TInt iDataLen;
  TInt iPhase; // 0 = start, 1 = in file content, 2 = file content done (and suffix left)
  RHTTPTransaction* iTransaction;
};

// For the client to receive upload completion notifications.
class MPosterObserver
{
public:
  // The error code is either a Symbian error code (if negative), or
  // one of those given below.
  virtual void PosterEvent(TInt anError) = 0;
};

#define POSTER_SUCCESS 1
#define POSTER_TRANSIENT_FAILURE 2
#define POSTER_PERMANENT_FAILURE 3

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CPosterAo" ;; name
 "MPosterObserver& anObserver, TUint32 aIapId" ;; args
 "iObserver(anObserver), iIapId(aIapId)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CPosterAo  \
public: static CPosterAo* NewLC(MPosterObserver& anObserver, TUint32 aIapId); \
public: static CPosterAo* NewL(MPosterObserver& anObserver, TUint32 aIapId); \
private: CPosterAo(MPosterObserver& anObserver, TUint32 aIapId); \
private: void ConstructL();

#define CTOR_IMPL_CPosterAo  \
CPosterAo* CPosterAo::NewLC(MPosterObserver& anObserver, TUint32 aIapId) \
{ \
  CPosterAo* obj = new (ELeave) CPosterAo(anObserver, aIapId); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CPosterAo* CPosterAo::NewL(MPosterObserver& anObserver, TUint32 aIapId) \
{ \
  CPosterAo* obj = CPosterAo::NewLC(anObserver, aIapId); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CPosterAo::CPosterAo(MPosterObserver& anObserver, TUint32 aIapId) : iObserver(anObserver), iIapId(aIapId) \
{}
/***end***/

NONSHARABLE_CLASS(CPosterAo) : 
  public CBase,
  public MHTTPTransactionCallback
{
  CTOR_DECL_CPosterAo;

 public:
  ~CPosterAo();
  
  void Cancel();
  TBool IsActive() { return iRunning; }

  // Asynchronous method.
  void PostFileL(const TDesC8& aUri,
		 const TDesC& aFileName);

  // Asynchronous method.
  void PostBufferL(const TDesC8& aUri,
		   const TDesC8& aBody);
  
  // Asynchronous method.
  void PostMultiPartBufferL(const TDesC8& aUri,
			    const TDesC8& aBody,
			    const TDesC8& aBoundary);

 private: // MHTTPTransactionCallback
  void MHFRunL(RHTTPTransaction aTransaction, const THTTPEvent& aEvent);
  TInt MHFRunError(TInt aError,
		   RHTTPTransaction aTransaction,
		   const THTTPEvent& aEvent);
  
 private: // internal methods
  void SetHeaderL(RHTTPHeaders aHeaders,
		  TInt aHdrField,
		  const TDesC8& aHdrValue);

  void SetSinglePart() { iIsMultiPart = EFalse; }
  void SetMultiPart(const TDesC8& aBoundary) { iIsMultiPart = ETrue; iBoundary = aBoundary; }
  
  // Asynchronous method.
  void PostGenericL(const TDesC8& aUri,
		    MDataSupplier& aDataSupplier);

 private: // property
  MPosterObserver& iObserver;
  TUint32 iIapId;

  DEF_SESSION(RSocketServ, iSocketServ);
  DEF_SESSION(RConnection, iConnection);
  DEF_SESSION(RHTTPSession, iHttpSession);
  DEF_SESSION(RHTTPTransaction, iHttpTransaction); // closed with iHttpSession also
  TBool iRunning;
  TInt iHttpStatus;

  RBufferDataSupplier iBufferDataSupplier;
  CFileDataSupplier* iFileDataSupplier;

  TBool iIsMultiPart;
  TBuf8<KMultiPartBoundaryMaxLen> iBoundary;
};

#endif // __FEATURE_UPLOADER__

#endif /* __up_poster_epoc_hpp__ */

/**

up_poster_epoc.hpp

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
