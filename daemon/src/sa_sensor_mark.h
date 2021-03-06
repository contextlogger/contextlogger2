#ifndef __sa_sensor_mark_h__
#define __sa_sensor_mark_h__

#include "application_config.h"

#if __MARK_ENABLED__

#include "ld_log_db.h"

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _sa_Sensor_mark sa_Sensor_mark;

  sa_Sensor_mark* sa_Sensor_mark_new(LogDb* log, GError** error);

  void sa_Sensor_mark_destroy(sa_Sensor_mark* self);

  gboolean sa_Sensor_mark_start(sa_Sensor_mark* self, GError** error);

  void sa_Sensor_mark_stop(sa_Sensor_mark* self);

  gboolean sa_Sensor_mark_is_active(sa_Sensor_mark* self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __MARK_ENABLED__ */

#endif /* __sa_sensor_mark_h__ */

/**

sa_sensor_mark.h

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
