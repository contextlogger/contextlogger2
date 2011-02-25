/*
 !concept {:name => "Simple file compression",
   :desc => "Straightforward file compression based on (raw) zlib."}
*/

/*
  To decompress, use
    ./zpipe -d < compressed-file > plain-file

  To allow gunzip to be used, we might want to use its file format.
    http://en.wikipedia.org/wiki/Gzip#File_format
    http://tools.ietf.org/html/rfc1952
 */

#include "ut_compress.h"

#include "ac_app_context.h"
#include "application_config.h"
#include "er_errors.h"

#include <zlib.h>

#include <stdio.h>
#include <string.h>
#include <assert.h>

#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

// inBuf and outBuf must be at least this large.
#define CHUNK (8 * 1024)

/* Compress from file source to file dest until EOF on source.
   def() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_STREAM_ERROR if an invalid compression
   level is supplied, Z_VERSION_ERROR if the version of zlib.h and the
   version of the library linked do not match, or Z_ERRNO if there is
   an error reading or writing the files. */
static int def(FILE *source, FILE *dest, 
	       int level,
	       unsigned char* inBuf,
	       unsigned char* outBuf)
{
    int ret, flush;
    unsigned have;
    z_stream strm;

    /* allocate deflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    ret = deflateInit(&strm, level);
    if (ret != Z_OK)
        return ret;

    /* compress until end of file */
    do {
        strm.avail_in = fread(inBuf, 1, CHUNK, source);
        if (ferror(source)) {
            (void)deflateEnd(&strm);
            return Z_ERRNO;
        }
        flush = feof(source) ? Z_FINISH : Z_NO_FLUSH;
        strm.next_in = inBuf;

        /* run deflate() on input until output buffer not full, finish
           compression if all of source has been read in */
        do {
            strm.avail_out = CHUNK;
            strm.next_out = outBuf;
            ret = deflate(&strm, flush);    /* no bad return value */
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            have = CHUNK - strm.avail_out;
            if (fwrite(outBuf, 1, have, dest) != have || ferror(dest)) {
                (void)deflateEnd(&strm);
                return Z_ERRNO;
            }
        } while (strm.avail_out == 0);
        assert(strm.avail_in == 0);     /* all input will be used */

        /* done when last data in file processed */
    } while (flush != Z_FINISH);
    assert(ret == Z_STREAM_END);        /* stream will be complete */

    /* clean up and return */
    (void)deflateEnd(&strm);
    return Z_OK;
}

/* report a zlib or i/o error */
static GError* zerr(int ret)
{
    switch (ret) {
    case Z_ERRNO:
      return gx_error_new(domain_posix, errno, 
			  "zlib: %s (%d)", strerror(errno), errno);
    case Z_STREAM_ERROR:
      return gx_error_new(domain_zlib, ret, 
			  "zlib: invalid compression level");
    case Z_DATA_ERROR:
      return gx_error_new(domain_zlib, ret, 
			  "zlib: invalid or incomplete deflate data");
    case Z_MEM_ERROR:
      return gx_error_new(domain_zlib, ret, "zlib: out of memory");
    case Z_VERSION_ERROR:
      return gx_error_new(domain_zlib, ret, "zlib version mismatch");
    default:
      return gx_error_new(domain_zlib, ret, "unexpected zlib error %d", ret);
    }
}

static gboolean compress_ft(const char* fs, const char* ft, GError** error)
{
  unsigned char* inBuf = NULL;
  unsigned char* outBuf = NULL;
  FILE* inFd = NULL;
  FILE* outFd = NULL;
  gboolean ret = FALSE;

  inBuf = g_try_malloc(CHUNK);
  if (!inBuf) {
    if (error) *error = gx_error_no_memory;
    goto done;
  }
  outBuf = g_try_malloc(CHUNK);
  if (!outBuf) {
    if (error) *error = gx_error_no_memory;
    goto done;
  }
  inFd = fopen(fs, "r");
  if (!inFd) {
    if (error)
      *error = gx_error_new(domain_posix, errno, 
			    "could not open file '%s' for reading: %s (%d)", 
			    fs, strerror(errno), errno);
    goto done;
  }
  outFd = fopen(ft, "w+");
  if (!outFd) {
    if (error)
      *error = gx_error_new(domain_posix, errno, 
			    "could not open file '%s' for writing: %s (%d)", 
			    ft, strerror(errno), errno);
    goto done;
  }

  int zret = def(inFd, outFd, Z_DEFAULT_COMPRESSION, inBuf, outBuf);
  if (zret != Z_OK) {
    if (error) *error = zerr(zret);
    goto done;
  }

  ret = TRUE;

 done:
  g_free(inBuf);
  g_free(outBuf);
  fclose(inFd);
  fclose(outFd);
  return ret;
}

gboolean compress_file(const char* fn, GError** error)
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

/* Some of the code is from zpipe.c.

   zpipe.c: example of proper use of zlib's inflate() and deflate()
   Not copyrighted -- provided to the public domain
   Version 1.4  11 December 2005  Mark Adler 
   retrieved from http://www.zlib.net/zpipe.c
*/

/* Version history:
   1.0  30 Oct 2004  First version
   1.1   8 Nov 2004  Add void casting for unused return values
                     Use switch statement for inflate() return values
   1.2   9 Nov 2004  Add assertions to document zlib guarantees
   1.3   6 Apr 2005  Remove incorrect assertion in inf()
   1.4  11 Dec 2005  Add hack to avoid MSDOS end-of-line conversions
                     Avoid some compiler warnings for input and output buffers
 */
