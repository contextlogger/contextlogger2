#include "log-db-create.h"

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
