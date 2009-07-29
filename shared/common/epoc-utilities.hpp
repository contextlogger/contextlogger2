#ifndef __epoc_utilities_hpp__
#define __epoc_utilities_hpp__

#include <e32base.h> // CTrapCleanup, CActiveScheduler

// Successful CTrapCleanup::New(); leads to installation as the
// current cleanup stack. The next cleanup stack on the stack (if any)
// becomes the current one as we delete the current one with
// ~CTrapCleanup().
// 
// Note that we are assuming KErrNoMemory error upon failure, as no
// other information is available.
#define WITH_CLEANUP_STACK(_task) \
  { \
  CTrapCleanup* cleanup = CTrapCleanup::New(); \
  if (!cleanup) return KErrNoMemory; \
  { _task ; }					  \
  delete cleanup; \
  }

// Note that we are assuming KErrNoMemory error upon failure, as no
// other information is available.
#define WITH_ACTIVE_SCHEDULER(_task) \
  { \
  CActiveScheduler* scheduler = new CActiveScheduler; \
  if (!scheduler) return KErrNoMemory; \
  CActiveScheduler::Install(scheduler); \
  { _task ; }				\
  delete scheduler; \
  }

#define WITH_CLEANUP_STACK_ERR(_errCode,_task)	\
  { \
  CTrapCleanup* _cleanup = CTrapCleanup::New(); \
  if (_cleanup) { { _task ; } delete _cleanup; } \
  else { _errCode = KErrNoMemory; } \
  }

#define WITH_ACTIVE_SCHEDULER_ERR(_errCode,_task)	\
  { \
  CActiveScheduler* _scheduler = new CActiveScheduler; \
  if (_scheduler) {				       \
    CActiveScheduler::Install(_scheduler);	       \
    { _task ; }					       \
    delete _scheduler;				       \
  } else {					       \
    _errCode = KErrNoMemory;			       \
  }						       \
  }

// This comes from Forum Nokia, and indeed should be quite safe when
// "op" is a free function of some kind that takes a single void*
// argument.
#define CleanupOpPushL(op,ptr) \
  CleanupStack::PushL(TCleanupItem(reinterpret_cast<TCleanupOperation>(op), ptr))

#endif /* __epoc_utilities_hpp__ */
