#include "up_private.h"

#include "ac_app_context.h"

#include "common/assertions.h"

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

  GDir* dir = NULL;

  TRAP_OOM_FAIL({
      dir = g_dir_open(LOG_UPLOADS_DIR, 0, error);
      if (!dir)
	return FALSE;
      const gchar* filename = g_dir_read_name(dir);
      if (filename) {
	*pathname = g_strdup_printf("%s" DIR_SEP "%s", LOG_UPLOADS_DIR, filename);
      }
    });

  g_dir_close(dir);
  return TRUE;

#if HAVE_TRAP_OOM
 fail:
  if (dir) g_dir_close(dir);
  if (error) *error = gx_error_no_memory;
  return FALSE;
#endif
}

/**

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
