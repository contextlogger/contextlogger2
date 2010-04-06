#include "config_db_private.h"

#include "application_config.h"
#include "db_creation.h"
#include "kr_controller_private.h"
#include "lua_cl2.h"
#include "sqlite_cl2.h"

#include "common/threading_mutex.h"

#include <string.h>

/** Config database file.
 */
#define CONFIGDB_BASENAME "config.db"
#define CONFIGDB_DIR CONFIG_DIR
#define CONFIGDB_FILE (CONFIGDB_DIR DIR_SEP CONFIGDB_BASENAME)

static const char create_tables_sql[] =
  "create table configuration (name TEXT UNIQUE, value TEXT);";

static gboolean create_config_db(GError** error)
{
  return create_database(CONFIGDB_DIR,
			 CONFIGDB_FILE,
			 create_tables_sql,
			 error);
}

static gboolean ensure_config_db_created(GError** error)
{
  assert_error_unset(error);

  if (!g_file_test(CONFIGDB_FILE, G_FILE_TEST_EXISTS)) {
    logf("file '%s' does not exist", CONFIGDB_FILE);
    return create_config_db(error);
  }

  return TRUE;
}

struct _ConfigDb
{
  sqlite3* db;

  sqlite3_stmt* getStmt;
  sqlite3_stmt* setStmt;

#if THREAD_SAFETY
  pthread_mutex_t mutex;
#endif
};

static gboolean prepare_sql_statements(ConfigDb *self, GError **error)
{
  if (sqlite_prepare(self->db, "select * from configuration where name = ? limit 1;", -1, &(self->getStmt), 0)) { goto fail; }
  // The OR REPLACE clause here states how to do conflict resolution
  // if the uniqueness constraints (in our case for "name") would be
  // violated.
  if (sqlite_prepare(self->db, "insert or replace into configuration (name, value) values (?, ?);", -1, &(self->setStmt), 0)) { goto fail; }
  return TRUE;

 fail:
  if (error) 
    *error = g_error_new(domain_cl2app, code_database_state_init, "error preparing statements for database '%s': %s (%d)", CONFIGDB_FILE, sqlite3_errmsg(self->db), sqlite3_errcode(self->db));
  return FALSE;
}

#define FINALIZE_SQL_STATEMENT(lvalue) \
  if (lvalue) { sqlite3_finalize(lvalue); lvalue = NULL; }

static void destroy_sql_statements(ConfigDb *self)
{
  FINALIZE_SQL_STATEMENT(self->getStmt);
  FINALIZE_SQL_STATEMENT(self->setStmt);
}

static void close_config_db_session(ConfigDb* self)
{
  if (self->db) {
    destroy_sql_statements(self);

    // Note that prepared statements and BLOB handles must be
    // freed separately.
    int errCode = sqlite3_close(self->db);
#if __DO_LOGGING__
    if (errCode) {
      // A close failure is probably a programming error, so we
      // shall log it.
      logf("sqlite3_close failure %d", errCode);
    }
#endif
    self->db = NULL;
  }
}

static gboolean open_config_db_session(ConfigDb* self, GError** error)
{
  // This still allocates a handle, except for those cases in which
  // the memory for the handle cannot be allocated. We can hence get
  // an error message if "db" is non-NULL.
  int errCode = sqlite3_open(CONFIGDB_FILE, &self->db);
  if (errCode) {
    if (error)
      *error = g_error_new(domain_cl2app, code_database_open, "error opening database '%s': %s (%d)", CONFIGDB_FILE, sqlite_get_error_string(self->db), errCode);
    close_config_db_session(self);
    return FALSE;
  }

  if (!prepare_sql_statements(self, error)) {
    close_config_db_session(self);
    return FALSE;
  }

  return TRUE;
}

ConfigDb* ConfigDb_new(GError** error)
{
  if (!ensure_config_db_created(error)) {
    // If getting this error, do make sure that you have any database
    // binary right, statically linked or otherwise.
    return NULL;
  }

  ConfigDb* self = g_try_new0(ConfigDb, 1);
  if (!self) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  mutex_init(&self->mutex);

  if (!open_config_db_session(self, error)) {
    // If getting this error, do make sure that you have any database
    // binary right, statically linked or otherwise.
    ConfigDb_destroy(self);
    return NULL;
  }

  return self;
}
  
void ConfigDb_destroy(ConfigDb* self)
{
  if (self) {
    close_config_db_session(self);
    mutex_destroy(&self->mutex);
    g_free(self);
  }
}

#define do_nothing ((void)0)

#define handle_sql_error						\
  {									\
    if (error)								\
      *error = g_error_new(domain_cl2app, code_database_command,	\
			   "ConfigDb database access error: %s (%d)",	\
			   sqlite3_errmsg(self->db),			\
			   sqlite3_errcode(self->db));			\
    goto done;								\
  }

static gchar* db_get(ConfigDb* self, 
		     const gchar* name,
		     GError** error)
{
  gchar* value = NULL;

  {
    if (sqlite3_bind_text(self->getStmt, 1, name, strlen(name), 
			  SQLITE_STATIC)) {
      handle_sql_error;
    }

    // Upon success we should get either SQLITE_ROW and SQLITE_DONE.
    // Anything else we consider an error, since there should be no
    // locking issues or anything. Any of a number of error codes
    // could be returned, since we are (possibly) using the "v2"
    // interface. SQLITE_ROW should mean that we got our (one) row
    // of data, and an (immediate) SQLITE_DONE should mean that
    // there is no data that matched the request.
    int res = sqlite3_step(self->getStmt);
    switch (res)
      {
      case SQLITE_DONE:
	{
	  if (error) *error = new_not_found_error;
	  sqlite3_reset(self->getStmt);
	  goto done;
	}
      case SQLITE_ROW:
	{
	  // "value" will be NULL if there is an OOM error.
	  value = (gchar *)sqlite3_column_text(self->getStmt, 1);
	  if (value) { 
	    value = strdup(value);
	  }
	  if (!value) {
	    if (error) *error = gx_error_no_memory;
	    sqlite3_reset(self->getStmt);
	    goto done;
	  }
	  break;
	}
      default: // some error
	{
	  sqlite3_reset(self->getStmt);
	  handle_sql_error;
	}
      }
      
    // Note that this does not clear bindings, but we should not
    // need to, unless we must later use unbound parameters (which
    // are interpreted as NULL); see sqlite3_clear_bindings().
    if (sqlite3_reset(self->getStmt)) {
      handle_sql_error;
    }

  done:
    do_nothing;
  }

  return value;
}

gchar* ConfigDb_get_generic(ConfigDb* self, 
			    const gchar* name,
			    GError** error)
{
  gchar* value = NULL;
  mutex_synchronized(&self->mutex, value = db_get(self, name, error));
  return value;
}

static gboolean db_set(ConfigDb* self, 
		       const gchar* name, 
		       const gchar* value, 
		       GError** error)
{
  assert(value);

  if (!validate_lua_syntax(value, error)) {
    return FALSE;
  }

  GError* getError = NULL;
  gchar* oldValue = db_get(self, name, &getError);
  if (!oldValue) {
    if (is_not_found_error(getError)) {
      g_error_free(getError);
    } else {
      if (error) *error = getError;
      else g_error_free(getError);
      return FALSE;
    }
  }
      
  if (oldValue) {
    if (strcmp(oldValue, value) == 0) {
      g_free(oldValue);
      return TRUE;
    }
    g_free(oldValue);
  }
    
  // Update database.
  {
    if (sqlite3_bind_text(self->setStmt, 1, name, strlen(name), 
			  SQLITE_STATIC)) {
      handle_sql_error;
    }

    if (sqlite3_bind_text(self->setStmt, 2, value, strlen(value), 
			  SQLITE_STATIC)) {
      sqlite3_reset(self->setStmt);
      handle_sql_error;
    }
    
    int res = sqlite3_step(self->setStmt);
    if (res != SQLITE_DONE) {
      sqlite3_reset(self->setStmt);
      handle_sql_error;
    }

    if (sqlite3_reset(self->setStmt)) {
      handle_sql_error;
    }

  done:
    do_nothing;
  }
    
  return TRUE;
}

gboolean ConfigDb_set_generic(ConfigDb* self, 
			      const gchar* name, 
			      const gchar* value, 
			      GError** error)
{
  gboolean success = FALSE;
  mutex_synchronized(&self->mutex, success = db_set(self, name, value, error));

  if (success) {
    // A question here is; what should we do if the notification should
    // fail. Should we perhaps roll back the ConfigDb change? We are not
    // doing that, and we consider it a separate issue whether the
    // concerned components are actually able to honor the new
    // configuration or not.
    kr_Controller* kr = getGlobalClient();
    if (!kr_Controller_reconfigure(kr, name, value, error))
      return FALSE;
  }

  return success;
}

/**

config_db.c

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
