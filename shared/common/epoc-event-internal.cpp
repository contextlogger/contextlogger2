#include "common/epoc-event-internal.h"
#include <e32std.h>
#include "application_config.h"
#include "common/assertions.h"
#include "common/logging.h"

// -----------------------------------------------------

#if EVENT_CALLBACK_WITH_GERROR
#include "epoc-ao-gerror.hpp" // CL2 only
#define ACTIVE_SCHEDULER_TYPE CActiveSchedulerG
#else
#define ACTIVE_SCHEDULER_TYPE CActiveScheduler
#endif

EXTERN_C int AoScheduler_install(MAYBE_ERROR_SOLE_PARAM)
{
#if __IS_DAEMON__
  CActiveScheduler* as = new ACTIVE_SCHEDULER_TYPE(MAYBE_ERROR_SOLE_ARG);
  if (!as)
    return KErrNoMemory;
  CActiveScheduler::Install(as);
#else
  assert(0 && "AoScheduler_install not supported for apps");
#endif
  return 0;
}

EXTERN_C void AoScheduler_uninstall() 
{
  CActiveScheduler* as = CActiveScheduler::Current();
  assert(as && "no active scheduler to uninstall");
  CActiveScheduler::Install(NULL); // uninstall (xxx does this delete the old one?)
  delete as;
}

// Replacing retains any registered active objects, but moves the
// registration to the new one.
EXTERN_C int AoScheduler_replace(MAYBE_ERROR_SOLE_PARAM)
{
#if __IS_DAEMON__
  CActiveScheduler* as = new ACTIVE_SCHEDULER_TYPE(MAYBE_ERROR_SOLE_ARG);
  if (!as)
    return KErrNoMemory;
  CActiveScheduler* old = CActiveScheduler::Replace(as);
  delete old;
#else
  assert(0 && "AoScheduler_replace not supported for apps");
#endif
  return 0;
}

// -----------------------------------------------------

// Returns NULL if cannot allocate.
EXTERN_C AoLoop* AoLoop_new()
{
  return (AoLoop*)(new CActiveSchedulerWait());
}

EXTERN_C void AoLoop_delete(AoLoop* asw) 
{
  delete ((CActiveSchedulerWait*)asw);
}

EXTERN_C void AoLoop_start(AoLoop* asw) 
{
  ((CActiveSchedulerWait*)asw)->Start();
}

EXTERN_C void AoLoop_stop(AoLoop* asw) 
{
  ((CActiveSchedulerWait*)asw)->AsyncStop();
}

