#include "er_errors.h"

#include "ac_app_context.h"
#include "application_config.h"

#include "common/utilities.h"

#include <glib/gprintf.h>

void er_fatal_quiet()
{
  EXIT_APPLICATION;
}

void er_fatal()
{
  er_fatal_general;
}

void er_fatal_msg(const char* msg)
{
  er_show_error_msg(msg);
  EXIT_APPLICATION;
}

void er_show_error_msg(const char* msg)
{
  WHEN_SYMBIAN(ex_show_error_msg(msg));
  UNLESS_SYMBIAN(logt(msg));
}

/*
 !concept {:name => "Flexible error reporting"}
*/

static
void er_log_base(int opt, void* errObj, 
		 const char* func, const char* file, int line, 
		 const char* user_msg)
{
  char* err_msg = NULL; // just "error" if errObj not given
  gboolean is_dynamic_err_msg = FALSE;
  char* log_msg = NULL;
  gboolean is_dynamic_log_msg = FALSE;

  SET_TRAP_OOM(goto nomemory);
  {
    {
      if (opt & er_NONE) {
	// Nothing to format.
      } else if (opt & er_POSIX) {
	int errCode = *(int*)errObj;
	err_msg = g_strdup_printf("POSIX error: %s (%d)", 
				  strerror(errCode), errCode);
	is_dynamic_err_msg = TRUE;
      } else if (opt & er_SYMBIAN) {
#if defined(__SYMBIAN32__)
	TInt errCode = *(TInt*)errObj;
	err_msg = g_strdup_printf("Symbian error: %s (%d)", 
				  plat_error_strerror(errCode), errCode);
	is_dynamic_err_msg = TRUE;
#else
	assert(0 && "Symbian error in non-Symbian code");
#endif /* __SYMBIAN32__ */
      } else if (opt & er_GERROR) {
	if (G_LIKELY(errObj)) {
	  GError* error = (GError*)errObj;
	  err_msg = g_strdup_printf("GError: %s (%s: %d)",
				    error->message, 
				    g_quark_to_string(error->domain), 
				    error->code);
	  is_dynamic_err_msg = TRUE;
	} else {
	  err_msg = "out of memory error";
	}
      } else {
	assert(0 && "unsupported error type");
      }
    }

    {
      const char* heading = ((opt & (er_FATAL|er_OOM)) ? "FATAL" : "ERROR");
      const char* inspect = (err_msg ? err_msg : "<no value>");
      const char* msg = (user_msg ? user_msg : "<no message>");
      log_msg = g_strdup_printf("%s: %s: %s [func %s, file %s, line %d]",
				heading, msg, inspect, func, file, line);
      is_dynamic_log_msg = TRUE;
    }
  }
  UNSET_TRAP_OOM();

#if HAVE_TRAP_OOM
 ready:
#endif
  {
    LogDb* logDb = ((opt & er_NODB) ? NULL : ac_global_LogDb);
    if (!logDb) {
      logt(log_msg);
    } else {
      if (!log_db_log_status(logDb, NULL, log_msg)) {
	logt("logging failure in er_log_base");
	opt |= er_FATAL;
      }
    }
    
    if (is_dynamic_err_msg)
      g_free(err_msg);
    if (is_dynamic_log_msg)
      g_free(log_msg);
    
    if (opt & er_FREE)
      if (opt & er_GERROR)
	gx_error_free((GError*)errObj);

    if ((opt & er_QUIET) && (opt & (er_FATAL|er_OOM))) {
      er_fatal_quiet();
    } else if (opt & er_OOM) {
      er_fatal_oom;
    }  else if (opt & er_FATAL) {
      er_fatal_general;
    }
  }
  return;

#if HAVE_TRAP_OOM
 nomemory:
  {
    log_msg = "FATAL: out of memory in er_log_base";
    opt |= er_OOM;
    goto ready;
  }
#endif
}

#define _er_log_impl(_errObj)						\
{									\
  char* user_msg = NULL;						\
  if (user_fmt) {							\
    SET_TRAP_OOM_FAIL();						\
    va_list argp;							\
    va_start(argp, user_fmt);						\
    g_vasprintf(&user_msg, user_fmt, argp);				\
    va_end(argp);							\
    UNSET_TRAP_OOM();							\
  }									\
  er_log_base(opt, _errObj, func, file, line, user_msg);		\
  g_free(user_msg);							\
  return;								\
  WHEN_TRAP_OOM(fail:							\
		g_free(user_msg);					\
		er_log_base(er_NONE | er_OOM | opt, NULL,		\
			    er_POSITION,				\
			    "out of memory in _er_log_impl"));		\
}

void _er_log_any(int opt, void* errObj, 
		 const char* func, const char* file, int line, 
		 const char* user_fmt, ...)
{
  _er_log_impl(errObj);
}

void _er_log_int(int opt, int errObj, 
		 const char* func, const char* file, int line, 
		 const char* user_fmt, ...)
{
  _er_log_impl(&errObj);
}

void _er_log_gerror(int opt, GError* errObj, 
		    const char* func, const char* file, int line, 
		    const char* user_fmt, ...)
{
  _er_log_impl(errObj);
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

gboolean ex_dblog_error_msg(LogDb* logDb, const char* msg, int errCode, GError** error)
{
  return log_db_log_status(logDb, error, "ERROR: %s: %s (%d)", msg, plat_error_strerror(errCode), errCode);
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

void ex_dblog_fatal_error_msg(LogDb* logDb, const char* msg, int errCode)
{
  log_db_log_status(logDb, NULL, "FATAL: %s: %s (%d)", msg, plat_error_strerror(errCode), errCode);
  ex_fatal_error(errCode);
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
