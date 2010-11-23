#include "cl2appapp.h"

#include "cl2appdocument.h"

#include "common/logging.h"

#ifdef __SERIES60_3X__
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

#ifdef __SERIES60_3X__
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
