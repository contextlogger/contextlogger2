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

  // Clears any "errorToLog" even if fails.
  gboolean gx_db_log_clear_error(LogDb* logDb, GError** errorToLog, GError** error);

  // Best effort. Invokes EXIT_APPLICATION as the last thing.
  void gx_db_log_clear_fatal_error(LogDb* logDb, GError** errorToLog);

  // --------------------------------------------------
  // POSIX extras
  // --------------------------------------------------

  void px_db_log_fatal_error(LogDb* logDb, int errCode);

  void px_db_log_fatal_errno(LogDb* logDb);

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

#endif /* __er_errors_h__ */
