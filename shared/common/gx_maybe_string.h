#ifndef __gx_maybe_string_h__
#define __gx_maybe_string_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct {
    gboolean just;
    GString* value;
  } GMaybeString;

  void GMaybeString_init(GMaybeString* m);

  const gchar* GMaybeString_get(GMaybeString* m);

  gboolean GMaybeString_assign(GMaybeString* m, const gchar* rval, GError** error);

  gboolean GMaybeString_is_nothing(GMaybeString* m);

  gboolean GMaybeString_is(GMaybeString* m, const gchar* other);

  void GMaybeString_free(GMaybeString* m);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __gx_maybe_string_h__ */

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
