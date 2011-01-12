#include "window_demo_qt.hpp"

#include "application_config.h"
#include "client-run.h"
#include "er_errors.h"
#include "guilog.h"
#include "kr_controller.h"

#include <QApplication>
#include <QDebug>
#include <QObject>

#include <QtGlobal>

#include <stdlib.h> // abort

// --------------------------------------------------
// exit functions
// --------------------------------------------------

// Immediate process exit.
extern "C" void ExitApplication()
{
  logt("ExitApplication");
  abort();
}

// Orderly shutdown. This is only done via a Lua binding, and there it
// is useful to be able to send a response before exiting, hence a
// proper shutdown.
extern "C" void ShutdownApplication()
{
  logt("ShutdownApplication");
  QApplication::quit();
}

// --------------------------------------------------
// main
// --------------------------------------------------

int main(int argc, char *argv[])
{
  QApplication app(argc, argv);

  MainWindow w;

  int errCode = cl2GlobalInit();
  if (errCode) {
    logt("global init failed");
    return 1;
  }

  logg("compiled against Qt %s", QT_VERSION_STR);
  logg("running with Qt %s", qVersion());
  
  GError* localError = NULL;
  kr_Controller* controller = kr_Controller_new(&localError);
  if (!controller) {
    logt("controller init failed");
    gx_txtlog_error_free(localError);
    return 2;
  }

  if (!kr_Controller_start(controller, &localError)) {
    logt("controller start failed");
    gx_txtlog_error_free(localError);
    return 3;
  }

  guilog(__APP_NAME__);
  guilogf("variant %s", __VARIANT_NAME__);
  guilogf("version %s", __VERSION_STRING__);
#ifdef __CERT_NAME__
  guilogf("capas %s", __CERT_NAME__);
#endif
  guilogf("compiled against Qt %s", QT_VERSION_STR);
  guilogf("running with Qt %s", qVersion());
  guilog("Welcome.");
  w.show();

  // This invokation actually runs the controller in an event loop.
  try {
    errCode = qApp->exec();
    logg("qApp->exec() returned with %d", errCode);
  } catch (const std::exception &ex) {
    logg("Qt error exception: %s", ex.what());
    return 4;
  }

  kr_Controller_destroy(controller);

  // Deletes application context.
  cl2GlobalCleanup();

  return 0;
}

/**

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
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
