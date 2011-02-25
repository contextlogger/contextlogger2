/*
 !concept {:name => "Gzipping files with Crypto++",
   :desc => "Straightforward gzip file compression with Crypto++."}
*/

/*
  To decompress, use
    gunzip compressed-file

  See these documents file format details.
    http://en.wikipedia.org/wiki/Gzip#File_format
    http://tools.ietf.org/html/rfc1952
 */

#include "ut_compress.h"

#include "ac_app_context.h"
#include "application_config.h"
#include "er_errors.h"

#include "cryptopp561/files.h"
#include "cryptopp561/gzip.h"

#include <glib/gstdio.h>

#include <stdio.h>
#include <string.h>
#include <assert.h>

// Throws a CryptoPP::Exception on error.
static void gzip_file(const char *in, const char *out)
{
  CryptoPP::FileSource(in, true, 
		       new CryptoPP::Gzip(new CryptoPP::FileSink(out)));
}

static gboolean compress_ft(const char* fs, const char* ft, GError** error)
{
  try {
    gzip_file(fs, ft);
  } catch (const CryptoPP::Exception& ex) {
    if (error)
      *error = gx_error_new(domain_cryptopp, 0, "gzip compression of '%s' failed: %s", fs, ex.what());
    return FALSE;
  }
  return TRUE;
}

extern "C" gboolean compress_file(const char* fn, GError** error)
{
  char* ft = tempnam(LOG_UPLOADS_DIR, "zlog_"); // caller must free result
  if (!ft) {
    if (error)
      *error = gx_error_new(domain_posix, errno,
			    "tempnam failed: %s (%d)",
			    strerror(errno), errno);
    return FALSE;
  }

  if (!compress_ft(fn, ft, error)) {
    g_free(ft);
    return FALSE;
  }

  if (g_unlink(fn)) {
    if (error)
      *error = gx_error_new(G_FILE_ERROR, g_file_error_from_errno(errno), 
			    "error deleting file '%s': %s (%d)", fn, 
			    strerror(errno), errno);
    g_free(ft);
    return FALSE;
  }

  if (rename(ft, fn)) {
    if (error)
      *error = gx_error_new(domain_posix, errno, 
                            "failed to rename '%s' as '%s': %s (%d)", 
                            ft, fn, strerror(errno), errno);
    g_free(ft);
    return FALSE;
  }

  g_free(ft);
  
  return TRUE;
}

/**

Copyright 2010-2011 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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
