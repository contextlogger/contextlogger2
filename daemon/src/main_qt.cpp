#include "client-run.h"
#include "er_errors.h"
#include "kr_controller.h"

#include <QApplication>

int main(int argc, char *argv[])
{
  QApplication app(argc, argv);

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
