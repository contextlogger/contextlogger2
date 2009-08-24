// -*- c++ -*-

#ifndef CL2APPAPPUI_H
#define CL2APPAPPUI_H

#include "kr_controller.h"

#include <aknappui.h>
#include <coeccntx.h>
#include <e32std.h>
#include <eikapp.h>
#include <eikdoc.h>

#include <glib.h>

class CCl2appContainer;

class CCl2appAppUi : public CAknAppUi
{
public:
  void ConstructL();
  ~CCl2appAppUi();

  void DestroyLogger();

  // Just making this public rather than protected.
  void Exit() { CAknAppUi::Exit(); }

private:
  void Start();
  void Stop();

private: // MEikMenuObserver
  void DynInitMenuPaneL(TInt aResourceId, CEikMenuPane* aMenuPane);

private: // CEikAppUi
  void HandleCommandL(TInt aCommand);
  virtual TKeyResponse HandleKeyEventL(const TKeyEvent& aKeyEvent, 
				       TEventCode aType);

private:
  CCl2appContainer* iAppContainer;
  kr_Controller* iClient;
};

#endif // CL2APPAPPUI_H
