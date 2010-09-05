#ifndef __er_errors_h__
#define __er_errors_h__

#include "log-db-logging.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/gxerror.h"
#include "common/gxlowmem.h"
#include "common/logging.h"
#include "common/platform_error.h"

#include <glib.h>

#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

  void er_global_init();

  void er_global_cleanup();

  // --------------------------------------------------
  // generic error reporting
  // --------------------------------------------------

  void er_fatal();

  // Error types. (Mutually exclusive.)
#define er_NONE    (1<<0)
#define er_POSIX   (1<<1)
#define er_SYMBIAN (1<<2)
#define er_GERROR  (1<<3)

  // Modifiers.
#define er_FATAL   (1<<8)
#define er_FREE    (1<<9)

  // Do not use directly.
  void _er_log_any(int opt, void* errObj, 
		   const char* func, const char* file, int line, 
		   const char* user_fmt, ...);
  void _er_log_int(int opt, int errObj, 
		   const char* func, const char* file, int line, 
		   const char* user_fmt, ...);
  void _er_log_gerror(int opt, GError* errObj, 
		      const char* func, const char* file, int line, 
		      const char* user_fmt, ...);

#define er_POSITION __func__, __FILE__, __LINE__

#define er_log_int_(opt, err, fmt...) \
  _er_log_int(opt, err, __func__, __FILE__, __LINE__, fmt)

  // Type unsafe version.
#define er_log_any_(opt, err, fmt...) \
  _er_log_any(opt, err, __func__, __FILE__, __LINE__, fmt)

  // Type safe versions.
#define er_log_none(opt, fmt...) \
  er_log_any_((opt) | er_NONE, NULL, fmt) 
#define er_log_posix(opt, val, fmt...) \
  er_log_int_((opt) | er_POSIX, val, fmt)
#define er_log_errno(opt, fmt...) \
  er_log_int_((opt) | er_POSIX, errno, fmt)
#define er_log_symbian(opt, val, fmt...) \
  er_log_int_((opt) | er_SYMBIAN, val, fmt)
#define er_log_gerror(opt, val, fmt...) \
  _er_log_gerror((opt) | er_GERROR, val, __func__, __FILE__, __LINE__, fmt)

#define er_log_nomem \
  er_log_none(er_FATAL, "out of memory error")

#define er_log_nomem_on_false(x) \
  if (!(x)) { er_log_nomem; }

  // --------------------------------------------------
  // GLib extras
  // --------------------------------------------------

  // "error" must be non-NULL.
  // Caller must free the returned buffer.
  // Returns NULL on ENOMEM.
  gchar* gx_error_to_string(GError* error);

  // Frees the error.
  void gx_error_free(GError* error);

  // Does not free the error.
  void gx_txtlog_error(GError* error);

  void gx_txtlog_error_free(GError* error);

  void gx_txtlog_error_clear(GError** error);

  gboolean gx_dblog_error_check(LogDb* logDb, GError* errorToLog, GError** error);

  // Frees any "errorToLog" even if fails.
  gboolean gx_dblog_error_free_check(LogDb* logDb, GError* errorToLog, GError** error);

  // Clears any "errorToLog" even if fails.
  gboolean gx_dblog_error_clear_check(LogDb* logDb, GError** errorToLog, GError** error);

#define gx_dblog_error(_db, _err) gx_dblog_error_check(_db, _err, NULL)
#define gx_dblog_error_free(_db, _err) gx_dblog_error_free_check(_db, _err, NULL)
#define gx_dblog_error_clear(_db, _err) gx_dblog_error_clear_check(_db, _err, NULL)

  // Best effort. Invokes EXIT_APPLICATION as the last thing.
  void gx_dblog_fatal_error_free(LogDb* logDb, GError* errorToLog);

  // Best effort. Invokes EXIT_APPLICATION as the last thing.
  void gx_dblog_fatal_error_clear(LogDb* logDb, GError** errorToLog);

  void gx_propagate_error(GError** dest, GError* src);

  // Invokes EXIT_APPLICATION as the last thing.
  void gx_txtlog_fatal_error_free(GError* errorToLog);

  // Invokes EXIT_APPLICATION as the last thing.
  void gx_txtlog_fatal_error_clear(GError** errorToLog);

  // --------------------------------------------------
  // POSIX extras
  // --------------------------------------------------

  void px_dblog_fatal_error(LogDb* logDb, int errCode);

  void px_dblog_fatal_errno(LogDb* logDb);

  void px_txtlog_fatal_error(int errCode);

  void px_txtlog_fatal_errno();

  // --------------------------------------------------
  // Symbian extras
  // --------------------------------------------------

#if defined(__SYMBIAN32__)
  void ex_fatal_error(int errCode);

  void ex_txtlog_error(int errCode);

  gboolean ex_dblog_error(LogDb* logDb, int errCode, GError** error);

  gboolean ex_dblog_error_msg(LogDb* logDb, const char* msg, int errCode, GError** error);

  void ex_txtlog_fatal_error(int errCode);

  void ex_dblog_fatal_error(LogDb* logDb, int errCode);

  void ex_dblog_fatal_error_msg(LogDb* logDb, const char* msg, int errCode);

  // Displays the specified Symbian error code in a briefly displayed
  // global dialog.
  void ex_show_error(int errCode);

  void ex_show_nomem_error();

  // Like ex_show_error(KErrGeneral).
  void ex_show_default_error();
#endif /* __SYMBIAN32__ */

#ifdef __cplusplus
} /* extern "C" */
#endif

#define new_not_found_error \
  gx_error_new(domain_cl2app, code_not_found, "not found")

#define is_not_found_error(_errorptr) \
  gx_error_is(_errorptr, domain_cl2app, code_not_found)

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
