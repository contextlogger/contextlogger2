#include "er_errors.h"

#include "ac_app_context.h"
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

void er_fatal()
{
  WHEN_SYMBIAN(ex_show_default_error());
  EXIT_APPLICATION;
}

void er_txtlog_fatal()
{
  logt("fatal error");
  er_fatal();
}

// The docs of g_error_free do not say if the error may be NULL. Well
// with this function it may.
void gx_error_free(GError* error)
{
  if (error) g_error_free(error);
}

// The "src" error may be NULL.
void gx_propagate_error(GError** dest, GError* src)
{
  if (dest) {
    assert((!*dest) && "dest error already set");
    *dest = src;
  } else {
    gx_error_free(src);
  }
}

gchar* gx_error_to_string(GError* error)
{
  GString* gs = NULL;
  TRAP_OOM_FAIL(gs = g_string_sized_new(128);
		g_string_printf(gs, "%s (%s: %d)",
				error->message, 
				g_quark_to_string(error->domain), 
				error->code));
  gchar* ret = gs->str;
  g_string_free(gs, FALSE);
  return ret;
#if HAVE_TRAP_OOM
 fail:
  if (gs) g_string_free(gs, TRUE);
  return NULL;
#endif
}

void gx_txtlog_error(GError* error)
{
  if (error) {
    gchar* s = gx_error_to_string(error);
    if (G_LIKELY(s)) {
      logt(s);
      g_free(s);
    } else {
      logt("out of memory error");
    }
  } else { // error == gx_error_no_memory
    logt("out of memory error");
  }
}

void gx_txtlog_error_free(GError* error)
{
  gx_txtlog_error(error);
  if (error) g_error_free(error);
}

void gx_txtlog_error_clear(GError** error)
{
  if (error) {
    gx_txtlog_error_free(*error);
    *error = NULL;
  }
}

gboolean gx_dblog_error_check(LogDb* logDb, GError* errorToLog, GError** error)
{
  gchar* s = NULL;
  gboolean free_s = FALSE;

  if (errorToLog) {
    s = gx_error_to_string(errorToLog);
    if (G_LIKELY(s)) {
      free_s = TRUE;
    }
  }
  if (!s)
    s = "out of memory error";

  gboolean r = log_db_log_status(logDb, error, s);
  if (free_s) g_free(s);
  return r;
}

// Takes ownership of "errorToLog" even if fails.
gboolean gx_dblog_error_free_check(LogDb* logDb, GError* errorToLog, GError** error)
{
  gboolean success = gx_dblog_error_check(logDb, errorToLog, error);
  gx_error_free(errorToLog);
  return success;
}

// Takes ownership of "errorToLog" even if fails.
gboolean gx_dblog_error_clear_check(LogDb* logDb, GError** errorToLog, GError** error)
{
  if (!errorToLog)
    return TRUE; // nothing to log
  gboolean success = gx_dblog_error_free_check(logDb, *errorToLog, error);
  *errorToLog = NULL;
  return success;
}

// Best effort. Invokes EXIT_APPLICATION as the last thing.
void gx_dblog_fatal_error_free(LogDb* logDb, GError* errorToLog)
{
  gx_dblog_error_free(logDb, errorToLog);
  er_fatal();
}

// Best effort. Invokes EXIT_APPLICATION as the last thing.
void gx_dblog_fatal_error_clear(LogDb* logDb, GError** errorToLog)
{
  gx_dblog_error_clear(logDb, errorToLog);
  er_fatal();
}

// Invokes EXIT_APPLICATION as the last thing.
void gx_txtlog_fatal_error_free(GError* errorToLog)
{
  gx_txtlog_error_free(errorToLog);
  er_fatal();
}

void gx_txtlog_fatal_error_clear(GError** errorToLog)
{
  gx_txtlog_error_clear(errorToLog);
  er_fatal();
}

void px_dblog_fatal_error(LogDb* logDb, int errCode)
{
  log_db_log_status(logDb, NULL, "FATAL: POSIX error: %s (%d)", strerror(errCode), errCode);
  er_fatal();
}

void px_dblog_fatal_errno(LogDb* logDb)
{
  px_dblog_fatal_error(logDb, errno);
}

void px_txtlog_fatal_error(int errCode)
{
  logf("FATAL: POSIX error: %s (%d)", strerror(errCode), errCode);
  er_fatal();
}

void px_txtlog_fatal_errno()
{
  px_txtlog_fatal_error(errno);
}

#if defined(__SYMBIAN32__)

void ex_fatal_error(int errCode)
{
  ex_show_error(errCode);
  EXIT_APPLICATION;
}

void ex_txtlog_error(int errCode)
{
  logf("ERROR: Symbian error: %s (%d)", plat_error_strerror(errCode), errCode);
}

gboolean ex_dblog_error(LogDb* logDb, int errCode, GError** error)
{
  return log_db_log_status(logDb, error, "ERROR: Symbian error: %s (%d)", plat_error_strerror(errCode), errCode);
}

void ex_txtlog_fatal_error(int errCode)
{
  logf("FATAL: Symbian error: %s (%d)", plat_error_strerror(errCode), errCode);
  ex_fatal_error(errCode);
}

void ex_dblog_fatal_error(LogDb* logDb, int errCode)
{
  log_db_log_status(logDb, NULL, "FATAL: Symbian error: %s (%d)", plat_error_strerror(errCode), errCode);
  ex_fatal_error(errCode);
}

#endif /* __SYMBIAN32__ */

void er_log_fatal_str(const char* text)
{
  LogDb* logDb = ac_global_LogDb;
  if (!(logDb && log_db_log_status(logDb, NULL, text)))
    logt(text);
  er_fatal();
}

/**

er_errors.c

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
