#ifndef __er_errors_h__
#define __er_errors_h__

#include "log-db-logging.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"

#include <glib.h>

#include <errno.h>

// xxx We probably should also have macros for creating GError objects here, should also offer some protection for our switching to a custom function for GError creation.

#ifdef __cplusplus
extern "C" {
#endif

  void er_global_init();

  void er_global_cleanup();

  // --------------------------------------------------
  // general error reporting
  // --------------------------------------------------

  void er_fatal_error();

  // --------------------------------------------------
  // GLib extras
  // --------------------------------------------------

  // "error" must be non-NULL.
  gchar* gx_error_to_string(GError* error);

  // Frees the error.
  void gx_error_free(GError* error);

  // Does not free the error.
  void gx_error_log(GError* error);

  void gx_error_log_free(GError* error);

  void gx_error_log_clear(GError** error);

  // Frees any "errorToLog" even if fails.
  gboolean gx_db_log_free_error(LogDb* logDb, GError* errorToLog, GError** error);

  // Clears any "errorToLog" even if fails.
  gboolean gx_db_log_clear_error(LogDb* logDb, GError** errorToLog, GError** error);

  // Best effort. Invokes EXIT_APPLICATION as the last thing.
  void gx_db_log_free_fatal_error(LogDb* logDb, GError* errorToLog);

  // Best effort. Invokes EXIT_APPLICATION as the last thing.
  void gx_db_log_clear_fatal_error(LogDb* logDb, GError** errorToLog);

  void gx_propagate_error(GError** dest, GError* src);

  // Invokes EXIT_APPLICATION as the last thing.
  void gx_log_free_fatal_error(GError* errorToLog);

  // --------------------------------------------------
  // POSIX extras
  // --------------------------------------------------

  void px_db_log_fatal_error(LogDb* logDb, int errCode);

  void px_db_log_fatal_errno(LogDb* logDb);

  void px_log_fatal_error();

  // --------------------------------------------------
  // Symbian extras
  // --------------------------------------------------

#if defined(__SYMBIAN32__)
  void ex_log_error(int errCode);

  gboolean ex_db_log_error(LogDb* logDb, int errCode, GError** error);

  void ex_log_fatal_error(int errCode);

  void ex_db_log_fatal_error(LogDb* logDb, int errCode);

  // Displays the specified Symbian error code in a briefly displayed
  // global dialog.
  void ex_show_error(int errCode);

  void ex_show_default_error();
#endif /* __SYMBIAN32__ */

#ifdef __cplusplus
} /* extern "C" */
#endif

#define gx_error_no_memory NULL

#define gx_error_is(_errorptr, d, c) \
  ((_errorptr) && ((_errorptr)->domain == (d)) && ((_errorptr)->code == (c)))

#define new_not_found_error g_error_new(domain_cl2app, code_not_found, "not found");

#define is_not_found_error(_errorptr) gx_error_is(_errorptr, domain_cl2app, code_not_found)

#endif /* __er_errors_h__ */

/**

er_errors.h

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
