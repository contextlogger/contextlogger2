// Defines a main function for running CL2 standalone, without being
// attached to any application.

#include "application_config.h"
#include "client-cl2.h"
#include "client-run.h"
#include "epoc-ao-gerror.hpp"
#include "er_errors.h"

#include "common/utilities.h"

#include <e32base.h> // CTrapCleanup

#include <stdlib.h> // abort

static CActiveSchedulerWait* globalLoop = NULL;

extern "C" void ExitApplication()
{
  logt("ExitApplication");
  if (globalLoop) {
    globalLoop->AsyncStop();
  } else {
    logt("error, no scheduler");
    abort();
  }
}

#define DELETE_GLOBAL_LOOP { delete globalLoop; globalLoop = NULL; }

static TInt MainLoop()
{
  GError* localError = NULL;
  GError** error = &localError;

  globalLoop = new CActiveSchedulerWait;
  if (!globalLoop) {
    return KErrNoMemory;
  }

  ClientCl2* client = client_cl2_new(error);
  if (!client) {
    logt("error in client creation");
    gx_error_log_clear(error);
    DELETE_GLOBAL_LOOP;
    return KGError;
  }
    
  if (!client_cl2_start(client, error)) {
    logt("error starting client");
    gx_error_log_clear(error);
    g_object_unref(client);
    DELETE_GLOBAL_LOOP;
    return KGError;
  }

  // Will not return until explicitly stopped by ExitApplication.
  globalLoop->Start();

  // Note that calling client_cl2_stop is unnecessary since
  // destruction is quite sufficient for stopping.

  g_object_unref(client);
  DELETE_GLOBAL_LOOP;

  return 0;
}

static TInt SubMain()
{
  TInt errCode = 0;
  cl2GlobalInit();
  TRAPD(leaveCode, errCode = MainLoop());
  if (leaveCode) {
    assert(0 && "leave in C code");
  }
  cl2GlobalCleanup();
  return errCode;
}

// On Symbian at least we might want to name this process so that it
// is easier to identify and kill. But actually we should have a
// sensible name to begin with, since we should get something like
// ekern.exe[100041af]0001, where we have the executable name, UID,
// and instance number.
// 
// Apparently also some Open C functions require that they are running
// under a TRAP. Naturally then a cleanup stack is surely also
// required. At present we are defining an E32Main rather than a main,
// so we shall do things explicitly.
// 
// In the Symbian case we must create an active scheduler within which
// to run; our event delivery mechanism requires this, and most
// certainly a lot of our system access code requires it. Note that to
// start the event loop, we must call CActiveScheduler::Start().
GLDEF_C TInt E32Main()
{
  TInt errCode = 0;
  __UHEAP_MARK;
  WITH_CLEANUP_STACK(WITH_ACTIVE_SCHEDULER(errCode = SubMain()));
  __UHEAP_MARKEND;
  return errCode;
}
