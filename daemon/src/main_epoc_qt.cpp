// Defines a main function for running CL2 standalone, without being
// attached to any application.
// 
// It is difficult to get init and cleanup right in such a way that
// QtCoreApplication is happy, and some of the code here closely
// mirrors code in Qt's s60main.

#include <exception> // for uncaught exception (keep this first)

#include "application_config.h"
#include "ac_app_context_private.h"
#include "kr_controller.h"
#include "client-run.h"
#include "epoc-ao-gerror.hpp"
#include "er_errors.h"

#include "common/utilities.h"

#include <e32std.h>
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
    er_log_gerror(er_FATAL|er_FREE, localError, "error starting controller");
    return; // not reached
  }
}

// May throw exceptions, but not leave.
static TInt QtMainE(int argc, char *argv[], char *envp[])
{
  // This seems a fairly heavy-duty class. It may be unsafe to try to
  // do anything really after it gets destroyed. Hence we complete
  // cleanup for CL2 engine before letting it fall out of scope.
  QCoreApplication app(argc, argv);
  
  TInt errCode = 0;
  CMainObj* mainObj = NULL;

  // Creates application context.
  errCode = cl2GlobalInit();
  if (errCode) {
    logt("error in global init");
    goto gifail;
  }

  // Handles async initialization tasks. If and when those complete,
  // the controller is created and set up with things to do.
  TRAP(errCode, mainObj = CMainObj::NewL());
  if (errCode) {
    logt("error creating main object");
    goto mofail;
  }

  // This invokation actually runs the controller in an event loop.
  try {
    errCode = qApp->exec();
    logg("qApp->exec() returned with %d", errCode);
  } catch (const std::exception &ex) {
    logg("Qt error exception: %s", ex.what());
    errCode = qt_symbian_exception2Error(ex);
  }

  // Deletes controller.
  delete mainObj;

 mofail:
  // Deletes application context.
  cl2GlobalCleanup();

 gifail:
  logg("exit code %d", errCode);
  return errCode;
}

static TInt QtMainWrapper()
{
  // These must persist for as long as QCoreApplication does.
  int argc = 0;
  char **argv = 0;
  char **envp = 0;
  __crt0(argc, argv, envp);
  logg("argc is %d", argc);
  for (int i=0; i<argc; i++)
    logg("arg %d is '%s'", i, argv[i]);
  TRAPD(errCode, QT_TRYCATCH_LEAVING(errCode = QtMainE(argc, argv, envp);));
  delete[] argv;
  delete[] envp;
  return errCode;
}

GLDEF_C TInt E32Main()
{
  CTrapCleanup *cleanupStack = q_check_ptr(CTrapCleanup::New());
  TRAPD(errCode, errCode = QtMainWrapper());
  delete cleanupStack;
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
