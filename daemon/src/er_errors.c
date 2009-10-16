#include "er_errors.h"

#include "application_config.h"

#include "common/utilities.h"

// Better just abort() on error here, system may not be ready for much else.
void er_global_init()
{
}

// May be invoked more than once.
void er_global_cleanup()
{
}

void er_fatal_error()
{
  logt("fatal error");
  WHEN_SYMBIAN(ex_show_default_error());
  EXIT_APPLICATION;
}

// The docs of g_error_free do not say if the error may be NULL. Well
// with this function it may.
void gx_error_free(GError* error)
{
  if (error) g_error_free(error);
}

void gx_propagate_error(GError** dest, GError* src)
{
  if (dest) {
    assert(!*dest);
    *dest = src;
  } else {
    gx_error_free(src);
  }
}

// Caller must free the returned buffer.
// Note that you may not pass "error" as NULL.
gchar* gx_error_to_string(GError* error)
{
  GString* gs = g_string_sized_new(128);
  g_string_printf(gs, "%s (%s: %d)",
		  error->message, 
		  g_quark_to_string(error->domain), 
		  error->code);
  gchar* ret = gs->str;
  g_string_free(gs, FALSE);
  return ret;
}

void gx_error_log(GError* error)
{
  if (error) {
    gchar* s = gx_error_to_string(error);
    logt(s);
    g_free(s);
  } else {
    logt("out of memory error");
  }
}

void gx_error_log_free(GError* error)
{
  gx_error_log(error);
  if (error) g_error_free(error);
}

void gx_error_log_clear(GError** error)
{
  if (error) {
    gx_error_log_free(*error);
    *error = NULL;
  }
}

// Takes ownership of "errorToLog" even if fails.
gboolean gx_db_log_free_error(LogDb* logDb, GError* errorToLog, GError** error)
{
  gboolean success = log_db_log_exception(logDb, errorToLog, error);
  gx_error_free(errorToLog);
  return success;
}

// Takes ownership of "errorToLog" even if fails.
gboolean gx_db_log_clear_error(LogDb* logDb, GError** errorToLog, GError** error)
{
  gboolean success = log_db_log_exception(logDb, *errorToLog, error);
  g_clear_error(errorToLog);
  return success;
}

// Best effort. Invokes EXIT_APPLICATION as the last thing.
void gx_db_log_free_fatal_error(LogDb* logDb, GError* errorToLog)
{
  gx_db_log_free_error(logDb, errorToLog, NULL);
  WHEN_SYMBIAN(ex_show_default_error());
  EXIT_APPLICATION;
}

// Best effort. Invokes EXIT_APPLICATION as the last thing.
void gx_db_log_clear_fatal_error(LogDb* logDb, GError** errorToLog)
{
  gx_db_log_clear_error(logDb, errorToLog, NULL);
  WHEN_SYMBIAN(ex_show_default_error());
  EXIT_APPLICATION;
}

// Invokes EXIT_APPLICATION as the last thing.
void gx_log_free_fatal_error(GError* errorToLog)
{
  gx_error_log_free(errorToLog);
  WHEN_SYMBIAN(ex_show_default_error());
  EXIT_APPLICATION;
}

void px_db_log_fatal_error(LogDb* logDb, int errCode)
{
  log_db_log_status(logDb, NULL, "FATAL: POSIX error: %s (%d)", strerror(errCode), errCode);
  WHEN_SYMBIAN(ex_show_default_error());
  EXIT_APPLICATION;
}

void px_db_log_fatal_errno(LogDb* logDb)
{
  px_db_log_fatal_error(logDb, errno);
}

#if defined(__SYMBIAN32__)

void ex_log_error(int errCode)
{
  logf("ERROR: Symbian error: %s (%d)", plat_error_strerror(errCode), errCode);
}

gboolean ex_db_log_error(LogDb* logDb, int errCode, GError** error)
{
  return log_db_log_status(logDb, error, "ERROR: Symbian error: %s (%d)", plat_error_strerror(errCode), errCode);
}

void ex_log_fatal_error(int errCode)
{
  logf("FATAL: Symbian error: %s (%d)", plat_error_strerror(errCode), errCode);
  ex_show_error(errCode);
  EXIT_APPLICATION;
}

void ex_db_log_fatal_error(LogDb* logDb, int errCode)
{
  log_db_log_status(logDb, NULL, "FATAL: Symbian error: %s (%d)", plat_error_strerror(errCode), errCode);
  ex_show_error(errCode);
  EXIT_APPLICATION;
}

#endif /* __SYMBIAN32__ */
