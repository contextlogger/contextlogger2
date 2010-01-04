#ifndef __kr_plat_ao_h__
#define __kr_plat_ao_h__

// Adjust this depending on whether you actually have an
// implementation for your platform.
#if defined(__SYMBIAN32__)
#define HAVE_PLAT_AO 1
#else
#define HAVE_PLAT_AO 0
#endif /* __SYMBIAN32__ */

#if HAVE_PLAT_AO

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  // Defines a platform-specific AO that runs alongside the primary
  // controller. This is just so that we need not come up with a
  // separate wrapper API for every little platform-specific thing
  // that might have to be going on within the controller.

typedef struct _kr_PlatAo kr_PlatAo;

kr_PlatAo* kr_PlatAo_new(GError** error);

void kr_PlatAo_destroy(kr_PlatAo* self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif // HAVE_PLAT_AO

#endif /* __kr_plat_ao_h__ */

/**

kr_plat_ao.h

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
