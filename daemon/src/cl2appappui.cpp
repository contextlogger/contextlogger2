#include <aknnotedialog.h>
#include <aknnotewrappers.h>
#include <avkon.hrh>
#include "cl2app.hrh"
#include <cl2app.rsg>
#include "cl2appappui.h"
#include "cl2appcontainer.h"

#include "client-run.h"
#include "utils_cl2.h"

#include "common/assertions.h"
#include "er_errors.h"
#include "common/logging.h"

#define KEnableSkinFlag 0x1000
#define KLayoutAwareFlag 0x08

// Immediate process exit.
extern "C" void ExitApplication()
{
  logt("ExitApplication");
  // This should make sure that the process gets killed, assuming it
  // is the main process that calls this.
  User::Exit(KErrGeneral);
}

extern "C" void ShutdownApplication()
{
  logt("ShutdownApplication");
#if 0
  // Yes, a leave from an Exit() actually is possible, and we
  // typically get KErrNoSuitable core. This makes little sense unless
  // one knows that the app UI exit is actually implemented with a
  // leave as a non-local return. More info here:
  // http://developer.symbian.com/forum/thread.jspa?messageID=76366&#76366
  TRAPD(errCode,
	((CCl2appAppUi*)(CEikonEnv::Static()->EikAppUi()))->Exit());
  logf("ShutdownApplication (%d)", errCode);
#else
  // Exiting an application from a RunL is tricky, but luckily in S60
  // there is a facility that takes care of this difficulty. We can
  // just invoke RunAppShutter.
  ((CAknAppUi*)(CEikonEnv::Static()->EikAppUi()))->RunAppShutter();
#endif
}

extern "C" void KillLogger()
{
  logt("KillLogger");
  ((CCl2appAppUi*)(CEikonEnv::Static()->EikAppUi()))->DestroyLogger();
}

void CCl2appAppUi::DestroyLogger()
{
  if (iClient) {
    kr_Controller_destroy(iClient);
    iClient = NULL;
    logt("logger killed");
  }
}

// This sort of thing is often required...
static void DisplayText(TDesC const &aText)
{
  CAknInformationNote *informationNote;
  (informationNote = new (ELeave) CAknInformationNote());
  informationNote->ExecuteLD(aText);
}

void CCl2appAppUi::ConstructL()
{
  // This renaming code would not appear to be effective, according to
  // Y-Tasks. As a workaround, the CL2 watchdog supports both this
  // name and the default name. Possibly the name can be set for
  // programs that are not applications (in the application framework
  // sense).
  COMPONENT_NAME_LIT(KProcessName); //_LIT(KProcessName, "cl2app");
  // Return value undocumented, assuming error code.
  User::LeaveIfError(RProcess().RenameMe(KProcessName));

#ifdef __SERIES60_3X__
  BaseConstructL(EAknEnableSkin);
#else
  BaseConstructL(KEnableSkinFlag | KLayoutAwareFlag);
#endif

  iAppContainer = new (ELeave) CCl2appContainer;
  iAppContainer->SetMopParent( this );
  iAppContainer->ConstructL( ClientRect() );
  AddToStackL( iAppContainer );

#if 0
  CCoeEnv* coeEnv = CCoeEnv::Static();
  assert(coeEnv && "CCoeEnv not set yet");
#endif

  // initializes logging, too
  if (cl2GlobalInit()) {
    _LIT(KInitErrMsg, "Global init failed");
    DisplayText(KInitErrMsg);
    return;
  }
  logt("global init complete");

  //cl_lua_eval_string("do x = \"hello world\"; return x end");

  {
    GError* localError = NULL;
    iClient = kr_Controller_new(&localError);
    logf("client init %s", iClient ? "ok" : "failed");
    if (!iClient) {
      gx_txtlog_error_free(localError);
      User::Leave(KErrGeneral);
    }
  }

  logf("scheduler running %d", (CEikonEnv::Static()->IsSchedulerRunning()) ? 1 : 0);

  Start();
}

CCl2appAppUi::~CCl2appAppUi()
{
  logt("clean application exit in progress");

  if (iAppContainer)
    {
      RemoveFromStack(iAppContainer);
      delete iAppContainer;
    }

  DestroyLogger();

  cl2GlobalCleanup();
}

void CCl2appAppUi::DynInitMenuPaneL(TInt /*aResourceId*/, 
					 CEikMenuPane* /*aMenuPane*/)
{
}

TKeyResponse CCl2appAppUi::HandleKeyEventL
(const TKeyEvent& /*aKeyEvent*/, TEventCode /*aType*/)
{
  return EKeyWasNotConsumed;
}

void CCl2appAppUi::Start()
{
  if (!iClient) return;

  GError* localError = NULL;
  if (!kr_Controller_start(iClient, &localError)) {
    gx_txtlog_error_free(localError);
    _LIT(msg, "failed to start");
    DisplayText(msg);
  } else {
    /*
    _LIT(msg, "started");
    DisplayText(msg);
    */
  }
}

void CCl2appAppUi::Stop()
{
  if (!iClient) return;
  kr_Controller_stop(iClient);
  /*
    _LIT(msg, "stopped");
    DisplayText(msg);
  */
}

void CCl2appAppUi::HandleCommandL(TInt aCommand)
{
  switch (aCommand)
    {
    case EAknSoftkeyBack:
    case EAknSoftkeyExit:
    case EEikCmdExit:
      {
	Exit();
	break;
      }
    case ECl2appCmdOneMenuCommand:
      {
	/*
	TBuf<50> buf;
	_LIT(fmt, "value is %d");
	buf.Format(fmt, 555);
	DisplayText(buf);
	*/
	//Start();
	break;
      }
    case ECl2appCmdAnotherMenuCommand:
      {
	//Stop();
	break;
      }
    default:
      break;
    }
}

/**

cl2appappui.cpp

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
