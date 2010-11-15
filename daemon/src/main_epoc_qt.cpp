// Defines a main function for running CL2 standalone, without being
// attached to any application.

#include "application_config.h"
#include "ac_app_context_private.h"
#include "kr_controller.h"
#include "client-run.h"
#include "epoc-ao-gerror.hpp"
#include "er_errors.h"

#include "common/utilities.h"

#include <e32std.h>
#include <exception>
#include <e32base.h>

#include <estlib.h> // __crt0

#include <qglobal.h> // QT_TRYCATCH_LEAVING
#include <QtCore/QCoreApplication>
#include <QTimer>

#include <stdlib.h> // abort

// Immediate process exit.
extern "C" void ExitApplication()
{
  logt("ExitApplication");
  User::Exit(KErrGeneral);
}

// Orderly shutdown. This is only done via a Lua binding, and there it
// is useful to be able to send a response before exiting, hence a
// proper shutdown.
extern "C" void ShutdownApplication()
{
  logt("ShutdownApplication");
  QCoreApplication::quit();
}

NONSHARABLE_CLASS(CMainObj) : 
  public CBase,
  public MAppContextInitObserver
{
 public:
  static CMainObj* NewL();
  ~CMainObj();
  TInt ExecuteL();
 private:
  void ConstructL();
 private: // MAppContextInitObserver
  void AppContextReady(TInt aError);
 private:
  kr_Controller* client;
};

CMainObj* CMainObj::NewL()
{
  CMainObj* obj = new (ELeave) CMainObj;
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop(obj);
  return obj;
}

CMainObj::~CMainObj()
{
  kr_Controller_destroy(client);
}

void CMainObj::ConstructL()
{
}

// xxx Do we need to catch C++ exceptions or anything here where we have Qt code?
TInt CMainObj::ExecuteL()
{
  /*
  int argc = 1;
  char* argv[] = {__APP_BASENAME__ ".exe"};
  // This apparently installs an active scheduler as well, and expects
  // there not to be an active scheduler yet, otherwise we get
  // E32USER-CBase 43.
  QApplication app(argc, argv);
  // Invokes AppContextReady upon completion.
  ac_AppContext_PlatInitAsyncL(ac_get_global_AppContext(), *this);
  return app.exec();
  */
  return 0;
}

void CMainObj::AppContextReady(TInt aError)
{
  logt("app context ready");

  if (aError) {
    er_log_symbian(er_FATAL, aError, "error in app ctx async init");
    return; // not reached
  }

  GError* localError = NULL;

  client = kr_Controller_new(&localError);
  if (!client) {
    er_log_gerror(er_FATAL|er_FREE, localError, "error in client creation");
    return; // not reached
  }
    
  if (!kr_Controller_start(client, &localError)) {
    kr_Controller_destroy(client);
    er_log_gerror(er_FATAL|er_FREE, localError, "error starting client");
    return; // not reached
  }
}

#if 0
static TInt MainLoopL()
{
  // Handles async initialization tasks. If and when those complete,
  // the logger proper is started.
  CMainObj* mainObj = CMainObj::NewL();
  CleanupStack::PushL(mainObj);

  // Will not return unless/until explicitly stopped by
  // ShutdownApplication.
  TInt errCode = mainObj->ExecuteL();

  CleanupStack::PopAndDestroy(1); // mainObj

  return errCode;
}
#endif

static TInt QtMainL()
{
  int argc = 0;
  char **argv = 0;
  char **envp = 0;
  __crt0(argc, argv, envp);

  logg("argc is %d", argc);
  for (int i=0; i<argc; i++)
    logg("arg %d is '%s'", i, argv[i]);

  QCoreApplication app(argc, argv);

  TInt errCode = 0;

#if 1
  logt("waiting");
  QTimer::singleShot(10000, &app, SLOT(quit()));
  errCode = app.exec();
  logt("done waiting");
#endif

  return errCode;
}

static TInt SubMain()
{
  TInt errCode = cl2GlobalInit();
  if (errCode) {
    logt("error in global init");
    return errCode;
  }

#define checkErrCode(_msg) { if (errCode) { logt(_msg); goto fail; } }
  TRAP(errCode,
       {
	 try {                                               
	   errCode = QtMainL();
	   checkErrCode("Qt error return");
	 } catch (const std::exception &ex) {
	   logg("Qt error exception: %s", ex.what());
	   errCode = qt_symbian_exception2Error(ex);
	   goto fail;
	 }   
       });
  checkErrCode("Qt error leave");

  fail:
  cl2GlobalCleanup();
  return errCode;
}

GLDEF_C TInt E32Main()
{
  TInt errCode = 0;
  __UHEAP_MARK;
  // It seems that QApplication tries to install a scheduler at some point. And possibly might want a specific subclass of it. Hence we do not install one here.
  //WITH_CLEANUP_STACK(WITH_ACTIVE_SCHEDULER(errCode = SubMain()));
  WITH_CLEANUP_STACK(errCode = SubMain());
  logg("exit code %d", errCode);
  __UHEAP_MARKEND;
  return errCode;
}

/**

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
