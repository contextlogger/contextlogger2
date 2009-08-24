#include "db_creation.h"

#include "application_config.h"
#include "sa_sensor_list_log_db.h"
#include "sqlite_cl2.h"
#include "utils_cl2.h" // for mkdir_p

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"

#include <glib/gstdio.h>

gboolean create_database(const char* db_dir,
			 const char* db_file,
			 const char* sql,
			 GError** error)
{
  sqlite3* db = NULL;

  if (!mkdir_p(db_dir, error)) {
    return FALSE;
  }
  //logt("log db directory existence ensured");
  
  // This still allocates a handle, except for those cases in which
  // the memory for the handle cannot be allocated. We can hence get
  // an error message if "db" is non-NULL.
  int errCode = sqlite3_open(db_file, &db);
  if (errCode) {
    if (error)
      *error = g_error_new(domain_cl2app, code_database_open, "error opening database '%s': %s (%d)", db_file, sqlite_get_error_string(db), errCode);
    goto fail;
  }
  //logf("database opened %d", errCode);

  logt(sql);
  errCode = sqlite3_exec(db, sql, NULL, NULL, NULL);
  //logf("errCode is %d", errCode);
  if (errCode) {
    if (error)
      *error = g_error_new(domain_cl2app, code_database_command, "error creating tables for database '%s': %s (%d)", db_file, sqlite_get_error_string(db), errCode);
    goto fail;
  }
  logt("database created");

  // We are not doing anything fancy here and would assume closing to
  // succeed. Or if it does not, it is likely not our fault, and not
  // much we can do about it.
  errCode = sqlite3_close(db);
  if (errCode) {
    if (error)
      *error = g_error_new(domain_cl2app, code_database_close, "error closing database '%s': (%d)", db_file, errCode);
    goto fail;
  }
  //logt("database session closed");

  return TRUE;

 fail:
  // If the database creation should fail in any way, we shall delete
  // the potentially incompletely created file, so that at next launch
  // there will be a new attempt to create it, possibly under more
  // favorable conditions.
  rm_file(db_file, NULL);
  return FALSE;
}

gboolean ensure_database_created(const char* db_dir,
				 const char* db_file,
				 const char* sql,
				 GError** error)
{
  assert_error_unset(error);

  if (!g_file_test(db_file, G_FILE_TEST_EXISTS)) {
    return create_database(db_dir, db_file, sql, error);
  }

  return TRUE;
}
