#ifndef __cf_rcfile_h__
#define __cf_rcfile_h__

#include "application_config.h"

#if __FEATURE_RCFILE__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _cf_RcFile cf_RcFile;

  /** Reads in the configuration from a static configuration file.
      A parse error or parameter value validity
      checking failure will cause an error return.
      Unrecognized settings shall be ignored.
      No configuration file shall cause the default static
      configuration information to be returned.

      As this object only contains fixed data (after initialization)
      accessing it is by nature thread safe.
  */
  cf_RcFile* cf_RcFile_new(GError** error);
  
  /** Frees any resources allocated for caching the static
      configuration information. */
  void cf_RcFile_destroy(cf_RcFile* self);

  int cf_RcFile_vm_id(cf_RcFile* self);

  // Note that you must strdup any strings that must be longer lived.
  int cf_RcFile_get_int_or(cf_RcFile* self, const char* name, int dval);
  const char* cf_RcFile_get_str_or(cf_RcFile* self, const char* name, const char* dval);
  const char* cf_RcFile_get_str_maybe(cf_RcFile* self, const char* name);

  // The configuration parameter value getters must be consistently
  // named, as we have macros assuming so.
#define cf_RcFile_get_username(_self) cf_RcFile_get_str_or(_self, "username", __USERNAME__)
#define cf_RcFile_get_upload_url(_self) cf_RcFile_get_str_maybe(_self, "upload_url")
#define cf_RcFile_get_remokon_host(_self) cf_RcFile_get_str_maybe(_self, "remokon_host")
#define cf_RcFile_get_remokon_port(_self) cf_RcFile_get_int_or(_self, "remokon_port", 5222)
#define cf_RcFile_get_remokon_password(_self) cf_RcFile_get_str_maybe(_self, "remokon_password")
#define cf_RcFile_get_jid(_self) cf_RcFile_get_str_maybe(_self, "jid")
#define cf_RcFile_get_iap(_self) cf_RcFile_get_int_or(_self, "iap", -1)
#define cf_RcFile_get_database_dir(_self) cf_RcFile_get_str_or(_self, "database_dir", DATABASE_DIR_DEFAULT)
#define cf_RcFile_get_database_disk_threshold(_self) cf_RcFile_get_int_or(_self, "database_disk_threshold", DATABASE_DISK_THRESHOLD_DEFAULT)

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __FEATURE_RCFILE__ */

#endif /* __cf_rcfile_h__ */

/**

cf_rcfile.h

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
