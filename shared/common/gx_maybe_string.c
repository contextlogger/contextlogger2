#include "gx_maybe_string.h"

#include "common/gxerror.h"
#include "common/gxlowmem.h"

#include <string.h> // strcmp

void GMaybeString_init(GMaybeString* m)
{
  m->just = FALSE;
  m->value = NULL;
}

const gchar* GMaybeString_get(GMaybeString* m)
{
  if (!m->just)
    return NULL;
  return m->value->str;
}

gboolean GMaybeString_is_nothing(GMaybeString* m)
{
  return GMaybeString_get(m) == NULL;
}

gboolean GMaybeString_is(GMaybeString* m, const gchar* other)
{
  const gchar* mine = GMaybeString_get(m);
  if (mine && other) {
    return strcmp(mine, other) == 0;
  } else if (!mine && !other) {
    return TRUE;
  } else {
    return FALSE;
  }
}

gboolean GMaybeString_assign(GMaybeString* m, const gchar* rval,
			     GError** error)
{
  SET_TRAP_OOM(goto fail);
  if (m->value) {
    m->value = g_string_assign(m->value, rval);
  } else {
    m->value = g_string_new(rval);
  }
  m->just = TRUE;
  UNSET_TRAP_OOM();
  return TRUE;

#if HAVE_TRAP_OOM
 fail:
  if (error) *error = gx_error_no_memory;
  return FALSE;
#endif
}

void GMaybeString_free(GMaybeString* m)
{
  if (m->value) {
    g_string_free(m->value, TRUE);
    m->value = NULL;
  }
  m->just = FALSE;
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
