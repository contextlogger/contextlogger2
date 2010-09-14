#include "client-run.h"

#include "ac_app_context_private.h"
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

#if !defined(__SYMBIAN32__)

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
    gx_txtlog_error_clear(error);
    exitCode = 1;
  }

  logt("exiting");
  return exitCode;
}

#endif // run functions

#if defined(__SYMBIAN32__)
#include <pipsversion.h>
#endif /* __SYMBIAN32__ */

#define TRAP_OOM_ENOMEM(_x) TRAP_OOM_VALUE(ENOMEM, _x)

// public interface
// 
// We shall return a POSIX error code if there is an error, or -1 if
// there is no appropriate standard value. See
// <asm-generic/errno-base.h>.
int cl2GlobalInit()
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
#if defined(__SYMBIAN32__)
  logf("compiled against PIPS version %03u", PIPS_VERSION);
#endif /* __SYMBIAN32__ */
  logf("compiled against GLib %u.%u.%u", 
       GLIB_MAJOR_VERSION, GLIB_MINOR_VERSION, 
       GLIB_MICRO_VERSION);
#if !defined(__SYMBIAN32__)
  // These are not available on Symbian, as variables cannot be exported.
  logf("running with GLib %u.%u.%u", 
       glib_major_version, glib_minor_version, 
       glib_micro_version);
#endif /* __SYMBIAN32__ */
  logf("built on %s at %s", __DATE__, __TIME__);

  log_ctx(PRIMARY_LOG_FILENAME, "context test");
#if __DO_LOGGING__
  TRAP_OOM_ENOMEM({
      gchar* eData = g_strescape("hello", NULL);
      logf("'hello' is '%s'", eData);
      g_free(eData);
    });
#endif
  logst;

  // This ensures that there will be no errors allocating our standard
  // error domains at runtime.
  TRAP_OOM_ENOMEM(preallocate_all_quarks);

#ifndef __EPOC32__
  // Do not want any console popping up if STDIOSERVER is installed.
  printf("console test\n");
#endif

#if __DO_LOGGING__ && 0
  {
    SET_TRAP_OOM_VALUE(ENOMEM);
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
    UNSET_TRAP_OOM();
  }
#endif

#if 0 // OOM testing
  {
    SET_TRAP_OOM_VALUE(ENOMEM);
    logt("invoking g_strnfill");
    gchar* s = g_strnfill(100000000 /*gsize length*/,
			  'a' /*gchar fill_char*/);
    logf("s is %d", (int)s);
    s[5] = '\0';
    logf("s contains '%s'", s);
    g_free(s);
    UNSET_TRAP_OOM();
  }
#endif

#ifndef __EPOC32__
  signal(SIGPIPE, SIG_IGN); // return EPIPE instead
#endif

  srand(time(NULL));

  // Required when using the GObject object system.
  TRAP_OOM_ENOMEM(g_type_init());

  GError* error = NULL;
#if __FEATURE_UPLOADER__
  if (!up_global_init(&error)) {
    logf("uploader global init failure");
    gx_txtlog_error_clear(&error);
    return -1;
  }
#endif

  ac_AppContext* ac = ac_AppContext_new(&error);
  if (G_UNLIKELY(!ac)) {
    logt("failure creating app context");
    gx_txtlog_error_clear(&error);
    return -1;
  }
  ac_set_global_AppContext(ac);
  logt("app ctx sync init complete");

  return 0;
}

// It must be okay to invoke this even if cl2GlobalInit() failed.
void cl2GlobalCleanup()
{
  logt("doing global cleanup");

#if __FEATURE_UPLOADER__
  up_global_cleanup();
#endif

  ac_AppContext* ac = ac_get_global_AppContext();
  if (ac) {
    ac_set_global_AppContext(NULL);
    ac_AppContext_destroy(ac);
  }

  logt("global cleanup complete");
}

/**

client-run.c

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
