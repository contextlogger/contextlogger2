#ifndef __log_db_create_h__
#define __log_db_create_h__

#include <glib.h>

gboolean create_log_db(GError** error);

gboolean ensure_log_db_created(GError** error);

#endif /* __log_db_create_h__ */
