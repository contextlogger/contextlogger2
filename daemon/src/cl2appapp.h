// -*- c++ -*-

#ifndef CL2APPAPP_H
#define CL2APPAPP_H

#include <aknapp.h>
#include "application_config.h"

const TUid KUidcl2app = { __UID__ };

class CCl2appApp : public CAknApplication
{
private:
  CApaDocument* CreateDocumentL();
  TUid AppDllUid() const;
};

#endif // CL2APPAPP_H
