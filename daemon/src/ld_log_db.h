#ifndef __ld_log_db_h__
#define __ld_log_db_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _LogDb LogDb;

  LogDb* LogDb_new(GError** error);
  
  void LogDb_destroy(LogDb* self);

  gboolean 	log_db_log_status_direct(LogDb * self,
					 GError ** error,
					 const char * text);

  gboolean 	log_db_log_status	(LogDb * self,
					 GError ** error,
					 const char * fmt,
					 ...);

  gboolean 	log_db_take_snapshot	(LogDb * self,
					 gchar * pathname,
					 gboolean * renamed,
					 GError ** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __ld_log_db_h__ */

/**

ld_log_db.h

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
