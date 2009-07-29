// -*- c++ -*-

#ifndef CL2APPDOCUMENT_H
#define CL2APPDOCUMENT_H

#include <akndoc.h>

class CEikAppUi;

class CCl2appDocument : public CAknDocument
{
public:
  static CCl2appDocument* NewL(CEikApplication& aApp);
  virtual ~CCl2appDocument();

private:
  CCl2appDocument(CEikApplication& aApp);
  void ConstructL();
  CEikAppUi* CreateAppUiL();
};

#endif // CL2APPDOCUMENT_H
