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
  GString* gs;
  TRAP_OOM_NULL(gs = g_string_sized_new(128));
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

void px_log_fatal_error()
{
  logf("FATAL: POSIX error: %s (%d)", strerror(errno), errno);
  WHEN_SYMBIAN(ex_show_default_error());
  EXIT_APPLICATION;
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
