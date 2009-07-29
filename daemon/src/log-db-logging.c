#include "log-db-logging.h"
#include "log-db-private.h"

#include "application_config.h"
#include "log-db-create.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_config.h"
#include "common/threading.h"

#include <glib/gprintf.h>

#include <time.h>
#include <stdarg.h> // va_list
#include <errno.h>

#if defined(__SYMBIAN32__)

#if __BTPROX_ENABLED__

#define SET_BTPROX_SQL_ERROR { rval = FALSE; if (error) *error = g_error_new(domain_cl2app, code_database_command, "failed to log btprox event: %s (%d)", sqlite3_errmsg(self->db), sqlite3_errcode(self->db)); }

gboolean log_db_log_btprox(LogDb* self, 
			   GPtrArray* items,
			   GError** error)
{
  //logt("log_db_log_btprox");

  time_t now = time(NULL);
  if (now == -1) {
    if (error)
      *error = g_error_new(domain_cl2app, code_time_query, "failed to get current time: %s (%d)", strerror(errno), errno);
    return FALSE;
  }

  mutex_lock(&self->mutex);

  gboolean rval = TRUE;
  gboolean inTran = FALSE;

  // Remember that after an sqlite3_step returns SQLITE_DONE, a
  // prepared statement may not be used again before resetting it.
  assert(self->_priv->stmts.transactionBeginStmt);
  if (sqlite3_step(self->_priv->stmts.transactionBeginStmt) != SQLITE_DONE)
    goto sql_fail;
  inTran = TRUE;
  if (sqlite3_reset(self->_priv->stmts.transactionBeginStmt))
    goto sql_fail;
  //logt("transaction began");

  assert(self->_priv->stmts.btproxScanStmt);
  if (sqlite3_bind_int(self->_priv->stmts.btproxScanStmt, 1, now))
    goto sql_fail;
  if (sqlite3_step(self->_priv->stmts.btproxScanStmt) != SQLITE_DONE)
    goto sql_fail;
  if (sqlite3_reset(self->_priv->stmts.btproxScanStmt))
    goto sql_fail;
  logt("btprox scan entry insert done");

  assert(self->db);
  sqlite3_int64 scanId = sqlite3_last_insert_rowid(self->db);
  assert((scanId != 0) && "just did an insert so there should be a row ID");
  //logt("got row id");

  // It is quite possible for us to have found 0 items. These will not
  // be visible if one takes a product over the scan and item tables.
  assert(items);
  //logst;
  int count;
  //logf("numItems = %d", items->len);
  for (count = 0; count < items->len; count++) {
    btprox_item* item = (btprox_item*)g_ptr_array_index(items, count);
    //logf("count = %d", count);
    assert(item);
    //logf("addr '%s'", item->address);
    assert(item->address);
    assert(item->name);
    //logf("name '%s'", item->name);
    //logf("bt device '%s' '%s'", item->address, item->name);

    if (sqlite3_bind_int64(self->_priv->stmts.btproxItemStmt, 1, scanId))
      goto sql_fail;
    if (sqlite3_bind_text(self->_priv->stmts.btproxItemStmt, 2, item->address, strlen(item->address), SQLITE_TRANSIENT))
      goto sql_fail;
    if (sqlite3_bind_text(self->_priv->stmts.btproxItemStmt, 3, item->name, strlen(item->name), SQLITE_TRANSIENT))
      goto sql_fail;
    if (sqlite3_step(self->_priv->stmts.btproxItemStmt) != SQLITE_DONE)
      goto sql_fail;
    if (sqlite3_reset(self->_priv->stmts.btproxItemStmt))
      goto sql_fail;
  }

  goto done;

 sql_fail:
  SET_BTPROX_SQL_ERROR;
    
 done:
  if (inTran) {
    if (rval) {
      // Commit.
      if (sqlite3_step(self->_priv->stmts.transactionCommitStmt) == SQLITE_DONE) {
	if (sqlite3_reset(self->_priv->stmts.transactionCommitStmt))
	  SET_BTPROX_SQL_ERROR;
      } else {
	SET_BTPROX_SQL_ERROR;
      }
    } else {
      // Rollback. Best effort.
      if (sqlite3_step(self->_priv->stmts.transactionRollbackStmt) == SQLITE_DONE)
	// Best effort.
	sqlite3_reset(self->_priv->stmts.transactionRollbackStmt);
    }
  }
  mutex_unlock(&self->mutex);
  return rval;
}

#endif // __BTPROX_ENABLED__

#endif // defined(__SYMBIAN32__)
