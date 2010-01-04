#ifndef __sa_array_h__
#define __sa_array_h__

#include "ac_app_context.h"

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _sa_Array sa_Array;

  /** Instantiates a sensor array consisting of all supported sensors.
      Does not yet start the sensors. */
  sa_Array* 	sa_Array_new	(ac_AppContext* ac,
				 GError** error);
  
  /** Starts all supported sensors. */
  void sa_Array_start(sa_Array* self);
  
  /** Stops all supported sensors. */
  void sa_Array_stop(sa_Array* self);
  
  /** Destroys a sensor array. Naturally all the sensors in it are stopped. */
  void 	sa_Array_destroy	(sa_Array* self);
  
  /** Returns FALSE for unknown names. */
  gboolean sa_sensor_is_supported(const gchar* name);

  gboolean sa_Array_sensor_is_running(sa_Array* self, const gchar* name);

  void sa_Array_sensor_stop(sa_Array* self, const gchar* name);

  gboolean sa_Array_sensor_start(sa_Array* self, const gchar* name, GError** error);

  gboolean sa_Array_reconfigure(sa_Array* self, const gchar* key, const gchar* value, GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __sa_array_h__ */

/**

sa_array.h

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
