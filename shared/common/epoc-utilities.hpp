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

/**

epoc-utilities.hpp

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
