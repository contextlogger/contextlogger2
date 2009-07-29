#include "log-db-create.h"
#include "application_config.h"
#include "sqlite_cl2.h"
#include "common/logging.h"
#include "common/assertions.h"
#include "common/error_list.h"
#include "utils_cl2.h" // for mkdir_p
#include "sa_sensor_list_log_db.h"
#include <glib/gstdio.h>

gboolean create_log_db(GError** error)
{
  sqlite3* db = NULL;

  if (!mkdir_p(LOGDB_DIR, error)) {
    return FALSE;
  }
  logt("log db directory existence ensured");
  
  // This still allocates a handle, except for those cases in which
  // the memory for the handle cannot be allocated. We can hence get
  // an error message if "db" is non-NULL.
  int errCode = sqlite3_open(LOGDB_FILE, &db);
  if (errCode) {
    if (error)
      *error = g_error_new(domain_cl2app, code_database_open, "error opening database '%s': %s (%d)", LOGDB_FILE, sqlite_get_error_string(db), errCode);
    goto fail;
  }
  logf("database opened %d", errCode);

  const char const* sql = get_create_tables_sql();
  logt(sql);
  errCode = sqlite3_exec(db, sql, NULL, NULL, NULL);
  logf("errCode is %d", errCode);
  if (errCode) {
    if (error)
      *error = g_error_new(domain_cl2app, code_database_command, "error creating tables for database '%s': %s (%d)", LOGDB_FILE, sqlite_get_error_string(db), errCode);
    goto fail;
  }
  logt("database created");

  // We are not doing anything fancy here and would assume closing to
  // succeed. Or if it does not, it is likely not our fault, and not
  // much we can do about it.
  errCode = sqlite3_close(db);
  if (errCode) {
    if (error)
      *error = g_error_new(domain_cl2app, code_database_close, "error closing database '%s': (%d)", LOGDB_FILE, errCode);
    goto fail;
  }
  logt("database session closed");

  return TRUE;

 fail:
  // If the database creation should fail in any way, we shall delete
  // the potentially incompletely created file, so that at next launch
  // there will be a new attempt to create it, possibly under more
  // favorable conditions.
  rm_file(LOGDB_FILE, NULL);
  return FALSE;
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
