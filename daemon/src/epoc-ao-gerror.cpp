#include "epoc-ao-gerror.hpp"

#include "ac_app_context.h"
#include "er_errors.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/platform_error.h"
#include "common/logging.h"

#if __IS_APPLICATION__
#include <aknappui.h>
#include <stdlib.h> // exit(3)
#endif

// -----------------------------------------------------------------------------
// active object...

TInt CActiveLogErr::RunError(TInt errCode)
{
  log_db_log_status(ac_global_LogDb, NULL, "FATAL: error in the RunL of %s", Description());
  ex_dblog_fatal_error(ac_global_LogDb, errCode);
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
  if (aError != KGError) { // Symbian error code
    return CActiveLogErr::RunError(aError);
  } else { // GError object
    log_db_log_status(ac_global_LogDb, NULL, "FATAL: error in the RunL of %s", Description());
    gx_dblog_fatal_error_clear(ac_global_LogDb, &iError);
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

/**

epoc-ao-gerror.cpp

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
