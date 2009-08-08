#include "utils_cl2.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/platform_error.h"

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
  if (g_mkdir_with_parents(pathname, 0777)) {
    if (error)
      *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), "error creating directory '%s': %s (%d)", pathname, strerror(errno), errno);
    return FALSE;
  }
  return TRUE;
}

gboolean rm_file(const gchar* pathname, GError** error)
{
  assert_error_unset(error);
  if (g_unlink(pathname)) {
    if (error)
      *error = g_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), "error deleting file '%s': %s (%d)", pathname, strerror(errno), errno);
    return FALSE;
  }
  return TRUE;
}

#if !GLIB_CHECK_VERSION(2,14,6)
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
  if (!isalpha(*s)) return FALSE;
  s++;
  while (*s) {
    if (!isalnum(*s)) return FALSE;
    s++;
  }
  return TRUE;
}
