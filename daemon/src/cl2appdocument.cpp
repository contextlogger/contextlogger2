#include "cl2appdocument.h"
#include "cl2appappui.h"

/***koog 
(require codegen/replace) 
(replacement "CCl2appDocument")
***//***end***/

/***koog (search-replace) ***/
CCl2appDocument::CCl2appDocument(CEikApplication& aApp) : 
  CAknDocument(aApp)
{
}

CCl2appDocument::~CCl2appDocument()
{
}

void CCl2appDocument::ConstructL()
{
}

CCl2appDocument* CCl2appDocument::NewL(CEikApplication& aApp)
{
  CCl2appDocument* self = new (ELeave) CCl2appDocument(aApp);
  CleanupStack::PushL(self);
  self->ConstructL();
  CleanupStack::Pop();
  return self;
}

CEikAppUi* CCl2appDocument::CreateAppUiL()
{
  return new (ELeave) CCl2appAppUi;
}
/***end***/
