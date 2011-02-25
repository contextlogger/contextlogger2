#include "ld_private.h"

#include "ac_app_context.h"
#include "application_config.h"
#include "er_errors.h"
#include "ld_create.h"
#include "ut_compress.h"

#include "common/logging.h"
#include "common/platform_config.h"
#include "common/threading.h"

#include <glib/gprintf.h>

#include <errno.h>
#include <stdarg.h> // va_list
#include <stdlib.h>
#include <string.h> /* memset() */
#include <time.h>

static void 
log_db_close_session (LogDb * self)
{
  if (self->db) {
    destroy_sql_statements(self);

    // Note that prepared statements and BLOB handles must be
    // freed separately.
    int errCode = sqlite3_close(self->db);
#if __DO_LOGGING__
    if (errCode) {
      // A close failure is probably a programming error, so we
      // shall log it.
      logg("sqlite3_close failure %d", errCode);
    }
#endif
    self->db = NULL;
  }
}

static gboolean 
log_db_open_session (LogDb * self, GError ** error)
{
  // This still allocates a handle, except for those cases in which
  // the memory for the handle cannot be allocated. We can hence get
  // an error message if "db" is non-NULL.
  int errCode = sqlite3_open(LOGDB_FILE, &self->db);
  if (errCode) {
    if (error)
      *error = gx_error_new(domain_cl2app, code_database_open, 
			    "error opening database '%s': %s (%d)", 
			    LOGDB_FILE, 
			    sqlite_get_error_string(self->db), errCode);
    if (self->db) {
      log_db_close_session(self);
    }
    return FALSE;
  }

  if (!prepare_sql_statements(self, error)) {
    log_db_close_session(self);
    return FALSE;
  }
  
  return TRUE;
}

// text:: A zero-terminated string, in UTF-8. Ownership is not
//        taken, and the string need not persist after the call.
static gboolean log_text_to_db(LogDb* self, 
			       const char* text,
			       sqlite3_stmt* stmt,
			       const char* errorFmt,
			       GError** error)
{
  assert_error_unset(error);

  logt(text);

  // strftime('%s', 'now') evaluates to Unix time in SQL so we could
  // possibly evaluate that, but this might only happen during
  // preparation, which is not okay in the general case.
  time_t now = time(NULL);
  if (now == -1) {
    er_log_errno(er_FATAL, "time()");
  }
  if (sqlite3_bind_int(stmt, 1, now)) {
    goto fail;
  }

  // If we use SQLITE_TRANSIENT instead of SQLITE_STATIC, the text has
  // to persist "during all subsequent calls to sqlite3_step() on the
  // statement handle".
  if (sqlite3_bind_text(stmt, 2, text, strlen(text), SQLITE_STATIC)) {
    goto fail;
  }

#define set_sqlite_error {						\
    if (error)								\
      *error = gx_error_new(domain_cl2app, code_database_command, errorFmt, \
			    sqlite3_errmsg(self->db), sqlite3_errcode(self->db)); \
  }

  if (sqlite3_step(stmt) != SQLITE_DONE) {
    set_sqlite_error; // capture original error
    sqlite3_reset(stmt); // necessary due to SQLITE_STATIC
    return FALSE;
  }

  if (sqlite3_reset(stmt)) {
    goto fail;
  }

  return TRUE;

 fail:
  set_sqlite_error;
  return FALSE;
}

LogDb * 
LogDb_new (GError ** error)
{
  assert_error_unset(error);

  if (G_UNLIKELY(!ensure_log_db_created(error)))
    return NULL;

  LogDb* self = g_try_new0(LogDb, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }
  
  if (G_UNLIKELY(!log_db_open_session(self, error))) {
    LogDb_destroy(self);
    return NULL;
  }
  
  return self;
}

void 
LogDb_destroy (LogDb * self)
{
  if (self) {
    log_db_close_session(self);
    g_free(self);
  }
}

gboolean 
log_db_log_status_direct (LogDb * self, GError ** error, const char * text)
{
  return log_text_to_db(self, text, self->stmts.statusStmt,
			"failed to log status: %s (%d)", error);
}

gboolean 
log_db_log_status (LogDb * self, GError ** error, const char * fmt, ...)
{
  {
    gchar* buf = NULL;

    SET_TRAP_OOM(goto nomemory);
    {
      va_list argp;
      va_start(argp, fmt);
      g_vasprintf(&buf, fmt, argp);
      va_end(argp); // any cleanup
    }
    UNSET_TRAP_OOM();

    gboolean res = log_db_log_status_direct(self, error, buf);
    g_free(buf);
    return res;

#if HAVE_TRAP_OOM
  nomemory:
    g_free(buf);
    if (error) *error = gx_error_no_memory;
    return FALSE;
#endif
  }
}

gboolean 
log_db_take_snapshot (LogDb * self, gchar * pathname, 
		      gboolean * renamed, GError ** error)
{
  *renamed = FALSE;

  log_db_close_session(self);

  if (rename(LOGDB_FILE, pathname)) {
    if (error)
      *error = gx_error_new(domain_posix, errno, 
			    "failed to rename '%s' as '%s': %s (%d)", 
			    LOGDB_FILE, pathname, strerror(errno), errno);
    return FALSE;
  }
  *renamed = TRUE;

  if (g_file_test(LOGDB_FILE, G_FILE_TEST_EXISTS)) {
    logg("Oops, file '%s' still exists!", LOGDB_FILE);
    er_fatal_general;
  }

#if __FEATURE_COMPRESS_LOGS__
  if (ac_STATIC_GET(compress_logs)) {
    logt("compressing logfile");
    if (!compress_file(pathname, error)) {
      return FALSE;
    }
  }
#endif

  if (!create_log_db(error)) {
    return FALSE;
  }

  if (!log_db_open_session(self, error)) {
    return FALSE;
  }

  return TRUE;
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
