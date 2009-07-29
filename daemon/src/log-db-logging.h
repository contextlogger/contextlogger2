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
