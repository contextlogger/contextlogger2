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
 private:
  void ConstructL();
 private: // MAppContextInitObserver
  void AppContextReady(TInt aError);
 private:
  kr_Controller* controller;
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
  if (controller) {
    logt("destroying controller");
    kr_Controller_destroy(controller);
    logt("controller destroyed");
  }
}

void CMainObj::ConstructL()
{
  // Invokes AppContextReady upon completion.
  ac_AppContext_PlatInitAsyncL(ac_get_global_AppContext(), *this);
}

void CMainObj::AppContextReady(TInt aError)
{
  logt("app context ready");

  if (aError) {
    er_log_symbian(er_FATAL, aError, "error in app ctx async init");
    return; // not reached
  }

  GError* localError = NULL;

  controller = kr_Controller_new(&localError);
  if (!controller) {
    er_log_gerror(er_FATAL|er_FREE, localError, "error in controller creation");
    return; // not reached
  }
    
  if (!kr_Controller_start(controller, &localError)) {
    kr_Controller_destroy(controller);
    er_log_gerror(er_FATAL|er_FREE, localError, "error starting controller");
    return; // not reached
  }
}

// May leave, but not throw an exception.
static TInt MainLoopL()
{
  // Handles async initialization tasks. If and when those complete,
  // the logger proper is started.
  CMainObj* mainObj = CMainObj::NewL();
  CleanupStack::PushL(mainObj);

  // Will not return unless/until explicitly stopped by
  // ShutdownApplication.
  assert(qApp);
  TInt errCode = 0;
  QT_TRYCATCH_LEAVING(qApp->exec());
  logg("qApp->exec() returned with %d", errCode);

  CleanupStack::PopAndDestroy(1); // mainObj

  return errCode;
}

// May throw an exception, but not leave.
static TInt QtMainE()
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

#if 0
  logt("waiting");
  QTimer::singleShot(10000, &app, SLOT(quit()));
  errCode = app.exec();
  logt("done waiting");
#else
  QT_TRAP_THROWING(errCode = MainLoopL());
#endif

  return errCode;
}

// No exceptions or leaves from here.
static TInt SubMain()
{
  TInt errCode = cl2GlobalInit();
  if (errCode) {
    logt("error in global init");
    return errCode;
  }

  try {                                               
    errCode = QtMainE();
    if (errCode) logt("Qt error return");
  } catch (const std::exception &ex) {
    logg("Qt error exception: %s", ex.what());
    errCode = qt_symbian_exception2Error(ex);
  }

  logt("doing global cleanup");
  cl2GlobalCleanup();
  logt("global cleanup done");
  return errCode;
}

GLDEF_C TInt E32Main()
{
  TInt errCode = 0;
  __UHEAP_MARK;
  // It seems that QApplication tries to install a scheduler at some point. And possibly might want a specific subclass of it. Hence we do not install one here.
  //WITH_CLEANUP_STACK(WITH_ACTIVE_SCHEDULER(errCode = SubMain()));
  WITH_CLEANUP_STACK(errCode = SubMain());
  __UHEAP_MARKEND;
  logg("exit code %d", errCode);
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
