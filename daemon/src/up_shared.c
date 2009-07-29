#include "up_private.h"

#if __FEATURE_UPLOADER__

#include "common/assertions.h"

// --------------------------------------------------
// errors
// --------------------------------------------------

GQuark up_quark()
{
  return g_quark_from_static_string("Uploader");
}

// --------------------------------------------------
// directory scanning
// --------------------------------------------------

// It is possible that the Uploader takes a snapshot but then fails to
// upload it before it is killed. Hence there might be snapshots
// waiting for a later upload. We scan the uploads directory to find
// such files, and upload and delete one at a time as our first
// priority until there are none left. The up_Uploader_upload_now
// function will cause the next snapshot time to be set to current
// time, but that is it, the snapshot will not actually be taken
// before any old files have been handled.

// Sets the full pathname to *pathname.
// This sets *pathname to NULL if there are no more files.
// The caller must free any set pathname.
gboolean getNextOldLogFile(gchar** pathname,
			   GError** error)
{
  assert(pathname && !*pathname);

  GDir* dir = g_dir_open(LOG_UPLOADS_DIR, 0, error);
  if (!dir)
    return FALSE;
  const gchar* filename = g_dir_read_name(dir);
  if (filename) {
    *pathname = g_strdup_printf("%s" DIR_SEP "%s", LOG_UPLOADS_DIR, filename);
  }
  g_dir_close(dir);

  return TRUE;
}

#endif // __FEATURE_UPLOADER__
