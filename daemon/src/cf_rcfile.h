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

  // The configuration parameter value getters must be consistently
  // named, as we have macros assuming so. And they are, as we are
  // generating them with consistent naming.

#include "cf_rcfile_list.h"

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
