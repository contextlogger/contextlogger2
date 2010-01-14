#include "log-db-create.h"

#include "ac_app_context.h"
#include "application_config.h"
#include "db_creation.h"
#include "sa_sensor_list_log_db.h"
#include "sqlite_cl2.h"
#include "utils_cl2.h" // for mkdir_p

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"

#include <glib/gstdio.h>

gboolean create_log_db(GError** error)
{
  const char const* sql = get_create_tables_sql();
  return create_database(LOGDB_DIR,
			 LOGDB_FILE,
			 sql,
			 error);
}

gboolean ensure_log_db_created(GError** error)
{
  assert_error_unset(error);

#if ALWAYS_RECREATE_DB
  // This may be useful during testing.
  g_unlink(LOGDB_FILE);
#endif
  
  if (!g_file_test(LOGDB_FILE, G_FILE_TEST_EXISTS)) {
    return create_log_db(error);
  }

  return TRUE;
}

/**

log-db-create.c

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
