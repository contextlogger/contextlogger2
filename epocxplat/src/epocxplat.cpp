#include "epocxplat.hpp"

// --------------------------------------------------
// platform info
// --------------------------------------------------

#include "sconfig.hrh"

#include "common/platform_config.h"

#if defined(__HAS_AHLECLIENT__)
#define __HAVE_AHLECLIENT_LIB__ 1
#else
#define __HAVE_AHLECLIENT_LIB__ 0
#endif

#if defined(__HAS_AHLE2CLIENT__)
#define __HAVE_AHLE2CLIENT_LIB__ 1
#else
#define __HAVE_AHLE2CLIENT_LIB__ 0
#endif

// --------------------------------------------------
// runtime feature query
// --------------------------------------------------

#define FEATURE_AhleBrowser __HAVE_AHLECLIENT_LIB__

EXPORT_C TBool epocxplat::HasFeature(TFeature aFeature)
{
  switch (aFeature)
    {
    case EFeatureAhleBrowser:
      {
	return FEATURE_AhleBrowser;
      }
    default:
      {
	return EFalse;
      }
    }
}

// --------------------------------------------------
// feature implementations
// --------------------------------------------------

using namespace epocxplat;

// --------------------------------------------------
// EFeatureAhleBrowser
// --------------------------------------------------
  
/*
http://wiki.forum.nokia.com/index.php/Adaptive_History_List_API
http://wiki.forum.nokia.com/index.php/Adaptive_History_List_API_for_5th_Edition
*/

#if FEATURE_AhleBrowser

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
#else
#error feature AhleBrowser enabled without API
#endif

#if __HAVE_AHLE2CLIENT_LIB__
#include <ahlegenericapi.h>
#endif

/***koog 
(require codegen/symbian-cxx)
(ctor-defines/spec
 "CMyAhleNotifier" ;; name
 "AhleBrowser::MObserver& aObserver" ;; args
 "iObserver(aObserver)" ;; inits
 "" ;; ctor
 #t ;; ConstructL
)
 ***/
#define CTOR_DECL_CMyAhleNotifier  \
public: static CMyAhleNotifier* NewLC(AhleBrowser::MObserver& aObserver); \
public: static CMyAhleNotifier* NewL(AhleBrowser::MObserver& aObserver); \
private: CMyAhleNotifier(AhleBrowser::MObserver& aObserver); \
private: void ConstructL();

#define CTOR_IMPL_CMyAhleNotifier  \
CMyAhleNotifier* CMyAhleNotifier::NewLC(AhleBrowser::MObserver& aObserver) \
{ \
  CMyAhleNotifier* obj = new (ELeave) CMyAhleNotifier(aObserver); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CMyAhleNotifier* CMyAhleNotifier::NewL(AhleBrowser::MObserver& aObserver) \
{ \
  CMyAhleNotifier* obj = CMyAhleNotifier::NewLC(aObserver); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CMyAhleNotifier::CMyAhleNotifier(AhleBrowser::MObserver& aObserver) : iObserver(aObserver) \
{}
/***end***/

NONSHARABLE_CLASS(CMyAhleNotifier) :
  public CBase,
  public epocxplat::AhleBrowser::MNotifier,
  public AhleObserverType
{
  CTOR_DECL_CMyAhleNotifier;

 public:
  virtual ~CMyAhleNotifier();

 private:
  epocxplat::AhleBrowser::MObserver& iObserver;

  AhleClientType* iAhle; // owned

  CDesCArray* iOldUrlArray; // owned

 private: // AhleObserverType
  virtual void AdaptiveListChanged(TInt aError);

 private:
  void HandleDataL();
};

CTOR_IMPL_CMyAhleNotifier;

void CMyAhleNotifier::ConstructL()
{
#if __HAVE_AHLECLIENT_LIB__
  iAhle = CAHLE::NewL();
#else
  // Can we find a list of these from somewhere? Not in API docs,
  // anyway. It would actually seem that the built-in apps use a
  // separate API (we would want EAHLEBrowser type client session via
  // that API). But we do not have the header in the SDK, which is
  // inconvenient. And who knows which LIB file would be required.
  _LIT(databaseName, "Browser");
  iAhle = NewAHLEClientL(databaseName);
  //logh();
#endif
  iAhle->SetObserverL(this);
}

CMyAhleNotifier::~CMyAhleNotifier()
{
  delete iAhle;
  delete iOldUrlArray;
}

void CMyAhleNotifier::AdaptiveListChanged(TInt errCode)
{
  //logf("AdaptiveListChanged(%d)", errCode);

  if (errCode) {
    iObserver.AhleBrowserError(errCode);
  } else {
    TRAP(errCode, HandleDataL());
    if (errCode) {
      iObserver.AhleBrowserError(errCode);
    }
  }
}

#define MAX_URLS 100

void CMyAhleNotifier::HandleDataL()
{
  CDesCArray* urlArray = new (ELeave) CDesCArrayFlat(MAX_URLS);
  CleanupStack::PushL(urlArray);

  CDesCArray* nameArray = new (ELeave) CDesCArrayFlat(MAX_URLS);
  CleanupStack::PushL(nameArray);

  // This just appears to give us the first items in the adaptive
  // list. There does not seem to be a way to request the latest
  // addition to the list. We may have to keep any previous copy of
  // urlArray, and log the new ones only. Difficult to analyze, but
  // better than nothing, I guess.
  //logh();
  iAhle->AdaptiveListL(*urlArray, *nameArray, MAX_URLS, KNullDesC
#if __HAVE_AHLECLIENT_LIB__
		       , 
		       //EAHLEAdaptiveSiteList // (short url, short url) 
		       EAHLEAdaptiveSiteDetails // (caption, long url)
		       //EAHLEAdaptiveAutoComplete // (caption, long url)
#endif
		       );
  //logh();

  TInt numItems = urlArray->Count();
  //logf("read total of %d adaptive history items", numItems);

  for (TInt i = 0; i < numItems; i++) {
    TPtrC url16(urlArray->MdcaPoint(i));
    TInt dummyPos;
    // We will not log the initial full set, which probably means that
    // we miss the first URL browsed. But then again, we do anyway
    // miss URLs that are revisited while still in history, and that
    // is a bigger problem.
    if ((iOldUrlArray) &&
	// This returns zero if a match is found.
	// iOldUrlArray must be sorted for this to work.
	iOldUrlArray->FindIsq(url16, dummyPos, ECmpNormal)) {

      iObserver.AhleBrowserData(nameArray->MdcaPoint(i), url16);
    }
  }

  CleanupStack::PopAndDestroy(nameArray);
  CleanupStack::Pop(urlArray);

  urlArray->Sort(ECmpNormal);
  delete iOldUrlArray;
  iOldUrlArray = urlArray;
}

#endif // FEATURE_AhleBrowser

EXPORT_C AhleBrowser::MNotifier* AhleBrowser::NewNotifierL(AhleBrowser::MObserver& aObserver)
{
#if FEATURE_AhleBrowser
  return ::CMyAhleNotifier::NewL(aObserver);
#else
  return NULL;
#endif
}

/**

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
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
