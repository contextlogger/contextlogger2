// This code based on an example retrieved from http://wiki.forum.nokia.com/index.php/Writing_an_HTTP_filter_plugin

#include <e32base.h>
#include <ecom/ecom.h>
#include <ecom/implementationproxy.h>
#include <http/cecomfilter.h>
#include <http/mhttpfilter.h>
#include <http/rhttptransaction.h>

#include "common/logging.h"

_LIT8(KCl2WebFilterName, "cl2webfilter");

class CCl2WebFilter : 
  public CEComFilter, 
  public MHTTPFilter
{
public:
  static CEComFilter* CreateFilterL(TAny* aHttpSession);
	
  // virtuals from MHTTPFilter
  void MHFUnload(RHTTPSession aSession, THTTPFilterHandle aHandle);
  void MHFLoad(RHTTPSession aSession, THTTPFilterHandle aHandle);
  void MHFRunL(RHTTPTransaction aTransaction, const THTTPEvent& aEvent);
  void MHFSessionRunL(const THTTPSessionEvent& aEvent);
  TInt MHFRunError(TInt aError, RHTTPTransaction aTransaction, const THTTPEvent& aEvent);
  TInt MHFSessionRunError(TInt aError, const THTTPSessionEvent& aEvent);
	
  ~CCl2WebFilter();
private:
  void ConstructL(RHTTPSession& aHttpSession);
	
private:
  RStringF iFilterName;
};
	
CEComFilter* CCl2WebFilter::CreateFilterL(TAny* aHttpSession)
{
  // The CEcomFilter class passes us a pointer to the RHTTPSession so we can install ourselves
  RHTTPSession* session = reinterpret_cast<RHTTPSession*>(aHttpSession);
  CCl2WebFilter* self = new (ELeave) CCl2WebFilter;
  CleanupStack::PushL(self);
  self->ConstructL(*session);
  CleanupStack::Pop(self);
  return self;
}
	
CCl2WebFilter::~CCl2WebFilter()
{
  iFilterName.Close();
}
	
void CCl2WebFilter::ConstructL(RHTTPSession& aSession)
{
  // install this filter in to the current session
  iFilterName = aSession.StringPool().OpenFStringL(KCl2WebFilterName);
  aSession.FilterCollection().AddFilterL(*this, THTTPEvent::EAnyTransactionEvent,
					 RStringF(), KAnyStatusCode, EClientFilters, iFilterName);
}

void CCl2WebFilter::MHFUnload(RHTTPSession aSession, THTTPFilterHandle aHandle)
{
  // called when our filter is unloaded
  logt("Demo Filter: HTTP filter unloaded.");
  // self loading filters manage their own life cycle...
  delete this;
}

void CCl2WebFilter::MHFLoad(RHTTPSession aSession, THTTPFilterHandle aHandle)
{
  // called when our filter is loaded.
  logt("Demo Filter: HTTP filter loaded.");
}

void CCl2WebFilter::MHFRunL(RHTTPTransaction aTransaction, const THTTPEvent& aEvent)
{
  // called when a RHTTPTransaction event happens.
  if (aEvent == THTTPEvent::ESubmit)
    {
      const TDesC8& uri = aTransaction.Request().URI().UriDes();
      logg("Demo Filter: New transaction submitted to '%S'", &uri);
    }
}

void CCl2WebFilter::MHFSessionRunL(const THTTPSessionEvent& aEvent)
{
  // called when an RHTTPSession even happens.
}

TInt CCl2WebFilter::MHFRunError(TInt aError, RHTTPTransaction aTransaction, const THTTPEvent& aEvent)
{
  // called when MHFRunL leaves, as with active objects
  return KErrNone;
}

TInt CCl2WebFilter::MHFSessionRunError(TInt aError, const THTTPSessionEvent& aEvent)
{
  // called when MHFSessionRunL leaves.
  return KErrNone;
}
	
// The standard ECOM initialisation stuff
	
const TImplementationProxy KImplementationTable[] = 
  {
    IMPLEMENTATION_PROXY_ENTRY(0xe846000f, CCl2WebFilter::CreateFilterL)
  };

EXPORT_C const TImplementationProxy* ImplementationGroupProxy(TInt& aTableCount)
{
  aTableCount = sizeof(KImplementationTable) / sizeof(TImplementationProxy);
  return KImplementationTable;
}
