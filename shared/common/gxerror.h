#ifndef __gxerror_h__
#define __gxerror_h__

#include <glib/gerror.h>

#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

  // An allocation failure safe version of g_error_new.
  // 
  // Returns NULL in there is no memory for allocating a new error
  // structure.
  // 
  // (Note: G_GNUC_PRINTF (3, 4) states that the format index is 3,
  // and the argument start index is 4. See glib/gmacros.h for
  // details.)
  GError* gx_error_new(GQuark         domain,
		       gint           code,
		       const gchar   *format,
		       ...) G_GNUC_PRINTF (3, 4);

  // An allocation failure safe version of g_error_new_literal.
  GError* gx_error_new_literal (GQuark         domain,
				gint           code,
				const gchar   *message);

  // A va_list version of gx_error_new. Note that GLib internally
  // defines g_error_new_valist, but does not export it.
  GError* gx_error_new_valist(GQuark         domain,
			      gint           code,
			      const gchar   *format,
			      va_list        args);

  // We use the GError API in a non-standard way in that we always accept a NULL ``GError`` pointer and interpret it as an out-of-memory error. This is to avoid trying to allocate a ``GError`` when there is no memory.
#define gx_error_no_memory NULL

#define gx_error_is(_errorptr, d, c) \
  ((_errorptr) && ((_errorptr)->domain == (d)) && ((_errorptr)->code == (c)))

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
