// -*- c++ -*-

#ifndef CL2APPAPPUI_H
#define CL2APPAPPUI_H

#include <aknappui.h>
#include <coeccntx.h>
#include <e32std.h>
#include <eikapp.h>
#include <eikdoc.h>

#include <glib.h>
#include "client-cl2.h"

class CCl2appContainer;

class CCl2appAppUi : public CAknAppUi
{
public:
  void ConstructL();
  ~CCl2appAppUi();

  void KillLogger();

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
  //GError* iError;
  //CActiveScheduler* iDefaultAs;
  ClientCl2* iClient;
};

#endif // CL2APPAPPUI_H
