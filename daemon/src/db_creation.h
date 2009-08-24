#ifndef __db_creation_h__
#define __db_creation_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  gboolean create_database(const char* dir,
			   const char* file,
			   const char* sql,
			   GError** error);

  gboolean ensure_database_created(const char* dir,
				   const char* file,
				   const char* sql,
				   GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __db_creation_h__ */
