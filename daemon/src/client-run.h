#ifndef __client_run_h__
#define __client_run_h__

#include <glib.h>
#include "application_config.h"

G_BEGIN_DECLS

#if __IS_DAEMON__
gboolean cl2RunOnce(GError** error);

int cl2RunOnceGetExitCode();
#endif

void cl2GlobalInit();

void cl2GlobalCleanup();

G_END_DECLS

#endif /* __client_run_h__ */
