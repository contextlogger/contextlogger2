// Do not include the SQLite header directly. Always do it via this
// file to ensure that you get the correct header.

#ifndef __sqlite_cl2_h__
#define __sqlite_cl2_h__

#include "application_config.h"

#if __USE_SQLITE3H__
#  include "sqlite3.h"
#  ifndef __SQLITE3H__
#    error include paths are wrong
#  endif
#elif __HAVE_SQLITE3__
#  include <sqlite3.h>
#else
#  error configuration without sqlite
#endif

#define sqlite_get_error_string(db) \
  ((db) ? sqlite3_errmsg(db) : "<no message>")

// We are not really planning on the fly schema changes, but still
// prefer to use sqlite3_prepare_v2 when available, just in case.
#if SQLITE_VERSION_NUMBER < 3006005
#define sqlite_prepare sqlite3_prepare
#else
#define sqlite_prepare sqlite3_prepare_v2
#endif

#define sqlite3_bind_text_or_null(db, ix, t, n, f) \
  ((t) ? sqlite3_bind_text(db, ix, t, n, f) : sqlite3_bind_null(db, ix))

#define sqlite3_bind_int_neqz(db, ix, val) \
  (((val) != 0) ? sqlite3_bind_int(db, ix, val) : sqlite3_bind_null(db, ix))

#define sqlite3_bind_int_ltez(db, ix, val) \
  (((val) <= 0) ? sqlite3_bind_int(db, ix, val) : sqlite3_bind_null(db, ix))

#endif /* __sqlite_cl2_h__ */

/**

sqlite_cl2.h

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
