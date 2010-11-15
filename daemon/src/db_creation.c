#include "db_creation.h"

#include "application_config.h"
#include "er_errors.h"
#include "sa_sensor_list_log_db.h"
#include "sqlite_cl2.h"
#include "utils_cl2.h" // for mkdir_p

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
      *error = gx_error_new(domain_cl2app, code_database_open, "error opening database '%s': %s (%d)", db_file, sqlite_get_error_string(db), errCode);
    goto fail;
  }
  //logg("database opened %d", errCode);

  logt(sql);
  errCode = sqlite3_exec(db, sql, NULL, NULL, NULL);
  //logg("errCode is %d", errCode);
  if (errCode) {
    if (error)
      *error = gx_error_new(domain_cl2app, code_database_command, "error creating tables for database '%s': %s (%d)", db_file, sqlite_get_error_string(db), errCode);
    goto fail;
  }
  logt("database created");

  // We are not doing anything fancy here and would assume closing to
  // succeed. Or if it does not, it is likely not our fault, and not
  // much we can do about it.
  errCode = sqlite3_close(db);
  if (errCode) {
    if (error)
      *error = gx_error_new(domain_cl2app, code_database_close, "error closing database '%s': (%d)", db_file, errCode);
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

/**

db_creation.c

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
