// -*- c++ -*-

#ifndef CL2APPAPPUI_H
#define CL2APPAPPUI_H

#include "ac_app_context_private.h"
#include "kr_controller.h"

#include <aknappui.h>
#include <coeccntx.h>
#include <e32std.h>
#include <eikapp.h>
#include <eikdoc.h>

#include <glib.h>

class CCl2appContainer;

class CCl2appAppUi : 
  public CAknAppUi,
  public MAppContextInitObserver
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

private: // MAppContextInitObserver
  void AppContextReady(TInt aError);

private:
  CCl2appContainer* iAppContainer;
  kr_Controller* iClient;
};

#endif // CL2APPAPPUI_H

/**

cl2appappui.h

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
