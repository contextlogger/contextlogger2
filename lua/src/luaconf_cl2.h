#ifndef __luaconf_cl2_h__
#define __luaconf_cl2_h__

/* Any application-specific header file #includes must be contained here. */

#ifdef __IS_STATIC_LIB__

  /* When building a static library, the build must be good for just
     about all configurations. We can only really rely on the platform
     and the NDEBUG flag. So do not even load "application_config.h". */
#define __application_config_h__
  
  /* Do any required configuration manually. Just enough for the header
     files. The application itself must actually export the symbols that
     require an implementation. */
#if defined(NDEBUG)
#define __DO_LOGGING__ 1
#else
#define __DO_LOGGING__ 0
#endif

  /* This header may be loaded, however. */
#include "common/platform_config.h"

#else

  /* This is fine if built from source into an application. */
#include "application_config.h"

#endif

  //#include <assert.h>
#include "common/assertions.h"

#include "common/logging_c.h"

#endif /* __luaconf_cl2_h__ */

/**

Copyright 2011 Helsinki Institute for Information Technology (HIIT)
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
