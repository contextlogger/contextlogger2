#include "utils_cl2.h"

#include "er_errors.h"

#include <glib/gprintf.h>
#include <glib/gstdio.h>

#include <ctype.h>
#include <errno.h>
#include <string.h>

// See <asm-generic/errno-base.h> for some related "errno" values.

gboolean mkdir_p(const gchar* pathname, GError** error)
{
  assert_error_unset(error);

  if (strlen(pathname) == 0)
    return TRUE;

  int hadErr;
  // This function may throw OOM.
  TRAP_OOM_FAIL(hadErr = g_mkdir_with_parents(pathname, 0777));
  if (G_UNLIKELY(hadErr)) {
    if (error)
      *error = gx_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), "error creating directory '%s': %s (%d)", pathname, strerror(errno), errno);
    return FALSE;
  }
  return TRUE;

#if defined(__SYMBIAN32__)
 fail:
  if (error) *error = gx_error_no_memory;
  return FALSE;
#endif /* __SYMBIAN32__ */
}

gboolean rm_file(const gchar* pathname, GError** error)
{
  assert_error_unset(error);
  // g_unlink is OOM safe on Symbian
  if (g_unlink(pathname)) {
    if (error)
      *error = gx_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), "error deleting file '%s': %s (%d)", pathname, strerror(errno), errno);
    return FALSE;
  }
  return TRUE;
}

#if !GLIB_CHECK_VERSION(2,14,6)
// On Symbian, reports ENOMEM by throwing a GLib OOM error.
void g_string_vprintf(GString *string,
		      const gchar *format,
		      va_list args)
{
  gchar* buf;
  g_vasprintf(&buf, format, args);
  g_string_assign(string, buf);
  g_free(buf);
}
#endif

gboolean is_ascii_ident(const gchar* s)
{
  if (!*s) return FALSE;
  if (!isalpha(*s) && (*s != '_')) return FALSE;
  s++;
  while (*s) {
    if (!isalnum(*s) && (*s != '_')) return FALSE;
    s++;
  }
  return TRUE;
}

/**

utils_cl2.c

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
