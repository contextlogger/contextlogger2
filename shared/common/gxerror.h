#ifndef __gxerror_h__
#define __gxerror_h__

// Defines gx_error_new (an allocation failure safe version of
// g_error_new), as well as g_error_vnew (a va_list version of
// g_error_new, which we require but is not available in GLib).

#include <glib/gerror.h>

#if defined(__SYMBIAN32__)
#include <glowmem.h>
#endif /* __SYMBIAN32__ */

#ifdef __cplusplus
extern "C" {
#endif

  // Returns NULL in there is no memory for allocating a new error structure.
  //
  // (Note: G_GNUC_PRINTF (3, 4) states that the format index is 3, and the argument start index is 4. See glib/gmacros.h for details.)
GError* gx_error_new(GQuark         domain,
		     gint           code,
		     const gchar   *format,
		     ...) G_GNUC_PRINTF (3, 4);

GError* g_error_vnew(GQuark         domain,
		     gint           code,
		     const gchar   *format,
		     va_list        args);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __gxerror_h__ */

/**

gxerror.h

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
