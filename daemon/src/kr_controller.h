#ifndef __kr_controller_h__
#define __kr_controller_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _kr_Controller kr_Controller;
  
  kr_Controller* kr_Controller_new(GError** error);
  
  void kr_Controller_destroy(kr_Controller* self);
  
  gboolean kr_Controller_start(kr_Controller* self,
			       GError** error);
  
  void kr_Controller_stop(kr_Controller* self);
  
  gboolean kr_Controller_run(kr_Controller* self,
			     GError** error);

  gboolean kr_Controller_reconfigure(kr_Controller* self,
				     const gchar* name,
				     const gchar* value,
				     GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __kr_controller_h__ */

/**

kr_controller.h

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
