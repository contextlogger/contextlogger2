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

/**

epoc-event-internal.cpp

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
