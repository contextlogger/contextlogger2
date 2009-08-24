#ifndef __log_db_create_h__
#define __log_db_create_h__

#include <glib.h>

G_BEGIN_DECLS

gboolean create_log_db(GError** error);

gboolean ensure_log_db_created(GError** error);

G_END_DECLS

#endif /* __log_db_create_h__ */
