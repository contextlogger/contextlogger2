#include "epoc-ao-gerror.hpp"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/platform_error.h"
#include "er_errors.h"
#include "common/logging.h"

#if __IS_APPLICATION__
#include <aknappui.h>
#include <stdlib.h> // exit(3)
#endif

// -----------------------------------------------------------------------------
// active object...

TInt CActiveLogErr::RunError(TInt errCode)
{
  logf("FATAL: error in the RunL of %s: %s (%d)", Description(), plat_error_strerror(errCode), errCode);
  ex_show_error(errCode);
  EXIT_APPLICATION;
  return 0;
}

// -----------------------------------------------------------------------------
// active object...

CActiveLogErrG::~CActiveLogErrG()
{
  //logt("~CActiveLogErrG() enter");
  //logf("iError is %d", (int)iError);
  gx_error_free(iError);
  //logt("~CActiveLogErrG() exit");
}

TInt CActiveLogErrG::RunError(TInt aError)
{
  if (aError != KGError) {
    return CActiveLogErr::RunError(aError);
  } else {
    logf("FATAL: error in the RunL of %s", Description());
    gx_error_log_clear(&iError);
    assert(iError == NULL);
    ex_show_default_error();
    EXIT_APPLICATION;
    return 0;
  }
}

// This is to be called within a 'RunL' method.
// Takes ownership of 'aError'.
void CActiveLogErrG::Leave(GError* aError)
{
  gx_error_free(iError);
  iError = aError;
  User::Leave(KGError);
}

// -----------------------------------------------------------------------------
// active object...

void CActiveRunG::RunL()
{
  // Whether or not any previously set error has been processed, we
  // may need to set a new error value in it now. The active scheduler
  // has had a chance to run and process any previous error, so it is
  // quite okay to do this.
  GError** error = &iError;
  g_clear_error(error);
  assert(iError == NULL);

  if (!RunGL(error))
    User::Leave(KGError);
}
