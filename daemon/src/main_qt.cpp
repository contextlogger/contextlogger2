#include "client-run.h"
#include "er_errors.h"
#include "kr_controller.h"

#include <QtCore/QCoreApplication>

int main(int argc, char *argv[])
{
  QCoreApplication app(argc, argv);

  int errCode = cl2GlobalInit();
  if (errCode) {
    logt("global init failed");
    return 1;
  }
  
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

  return app.exec();
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
