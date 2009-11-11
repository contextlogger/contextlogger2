// Do not include the SQLite header directly. Always do it via this
// file to ensure that you get the correct header.

#ifndef __sqlite_cl2_h__
#define __sqlite_cl2_h__

#include "application_config.h"

#ifdef __USE_SQLITE3H__
#include "sqlite3.h"
#ifndef __SQLITE3H__
#error include paths are wrong
#endif
#else
#include <sqlite3.h>
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
  (t ? sqlite3_bind_text(db, ix, t, n, f) : sqlite3_bind_null(db, ix))

#endif /* __sqlite_cl2_h__ */
