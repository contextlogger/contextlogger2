// Here we have some hand-written (as opposed to generated) sensor
// logging routines.

#ifndef __log_db_logging_h__
#define __log_db_logging_h__

#include "log-db.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__SYMBIAN32__)

  typedef struct {
    char* address; // ASCII, with colons
    char* name; // UTF-8
  } btprox_item;

  // Does not take ownership of "items", and "items" need not exist
  // after the call has completed.
  gboolean log_db_log_btprox(LogDb* self, 
			     GPtrArray* items /* array_of(btprox_item*) */,
			     GError** error);

#endif // defined(__SYMBIAN32__)

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __log_db_logging_h__ */

/**

log-db-logging.h

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
