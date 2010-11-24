#include "cl2appapp.h"

#include "application_config.h"
#include "cl2appdocument.h"

#include "common/logging.h"

#if __S60_VERNUM__ >= 30
#include <eikstart.h>
#endif

TUid CCl2appApp::AppDllUid() const
{
  return KUidcl2app;
}

CApaDocument* CCl2appApp::CreateDocumentL()
{
  return CCl2appDocument::NewL(*this);
}

EXPORT_C CApaApplication* NewApplication()
{
  return new CCl2appApp;
}

#if __S60_VERNUM__ >= 30
GLDEF_C TInt E32Main()
{
  TInt exitCode = EikStart::RunApplication(NewApplication);
  logg("exiting app with code %d", exitCode);
  return exitCode;
}
# else
GLDEF_C TInt E32Dll( TDllReason )
{
  return KErrNone;
}
#endif
