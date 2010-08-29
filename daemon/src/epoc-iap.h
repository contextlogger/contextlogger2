#ifndef __epoc_iap_h__
#define __epoc_iap_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  // iapName must be given in UTF-8.
  gboolean epoc_iap_by_name(const gchar* iapName, 
			    guint32* iapId, 
			    gboolean* found,
			    GError** error); 

  gboolean epoc_iap_is_modem(guint32 iapId,
			     gboolean* found,
			     gboolean* yes,
			     GError** error); 

  void epoc_log_bearer_types();

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __epoc_iap_h__ */

/**

epoc-iap.h

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
