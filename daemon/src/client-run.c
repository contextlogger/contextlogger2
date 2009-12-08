#include "client-run.h"

#include "kr_controller.h"
#include "er_errors.h"
#include "up_uploader.h"
#include "utils_cl2.h"

#include "common/assertions.h"
#include "common/logging-stack.h"

#include <stdio.h>
#include <stdlib.h>

#ifndef __EPOC32__
#include <signal.h>
#endif

#if __IS_DAEMON__

// Note that these "Run" methods are not required in a typical Symbian
// application, since Symbian applications have a built in event loop,
// and a blocking call to "run" the client to the finish in the main
// thread makes little sense. More likely you will run it on the
// background in the main thread.

gboolean cl2RunOnce(GError** error)
{
  assert_error_unset(error);

  gboolean res = TRUE;

  kr_Controller* client = kr_Controller_new(error);
  if (!client) {
    logt("error in client creation");
    return FALSE;
  }

  if (!kr_Controller_start(client, error)) {
    logt("error starting client");
    res = FALSE;
    goto cleanup;
  }

  if (!kr_Controller_run(client, error)) {
    logt("error running client");
    res = FALSE;
  }
  logt("event loop exited");

  kr_Controller_stop(client);
  logt("client stopped");
    
cleanup:
  kr_Controller_destroy(client);
  logt("client destroyed");
  return res;
}

// public interface
int cl2RunOnceGetExitCode()
{
  GError* localError = NULL;
  GError** error = &localError;
  int exitCode = 0;

  if (!cl2RunOnce(error)) {
    gx_error_log_clear(error);
    exitCode = 1;
  }

  logt("exiting");
  return exitCode;
}

#endif // __IS_DAEMON__

// public interface
void cl2GlobalInit()
{
  log_clear(PRIMARY_LOG_FILENAME);
  log_text(PRIMARY_LOG_FILENAME, "initializing");
  logf("app '%s' v%s variant '%s'", 
       __APP_NAME__, __VERSION_STRING__, __VARIANT_NAME__);
#if defined(__ABLD_VARIANT__)
  logf("Symbian ABLD build variant is '%s'", __ABLD_VARIANT__);
#endif
#if defined(__GNUC__)
  logf("compiled with %s version %d.%d.%d (%06d)",
       __COMPILER_NAME__,
       __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__,
       __GCC_VERSION__);
#endif
  logf("built on %s at %s", __DATE__, __TIME__);
  log_ctx(PRIMARY_LOG_FILENAME, "context test");
#if __DO_LOGGING__
  gchar* eData = g_strescape("hello", NULL);
  logf("'hello' is '%s'", eData);
  g_free(eData);
#endif
  logst;

#ifndef __EPOC32__
  // Do not want any console popping up if STDIOSERVER is installed.
  printf("console test\n");
#endif

#if __DO_LOGGING__
  double td = 6.38000011;
  logf("printf %%f %f", td);
  logf("printf %%g %g", td);
  logf("printf %%.6f %.6f", td);
  char tdb[50];
  snprintf(tdb, 50, "snprintf %%f %f", td); logt(tdb);
  snprintf(tdb, 50, "snprintf %%g %g", td); logt(tdb);
  snprintf(tdb, 50, "snprintf %%.6f %.6f", td); logt(tdb);
  g_snprintf_fix(tdb, 50, "g_snprintf_fix %%f %f", td); logt(tdb);
  g_snprintf_fix(tdb, 50, "g_snprintf_fix %%g %g", td); logt(tdb);
  g_snprintf_fix(tdb, 50, "g_snprintf_fix %%.6f %.6f", td); logt(tdb);
#endif

#ifndef __EPOC32__
  signal(SIGPIPE, SIG_IGN); // return EPIPE instead
#endif

  srand(time(NULL));

  // Required when using the GObject object system.
  g_type_init(); // xxx check whether always succeeds, even on symbian

  er_global_init();

#if __FEATURE_UPLOADER__
  if (!up_global_init(NULL)) {
    logf("Uploader global init failure");
    abort();
  }
#endif
}

void cl2GlobalCleanup()
{
  logt("doing global cleanup");

#if __FEATURE_UPLOADER__
  up_global_cleanup();
#endif

  er_global_cleanup();

  logt("global cleanup complete");
}
