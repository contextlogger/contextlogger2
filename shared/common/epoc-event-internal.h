// What we have here is C wrappers for Symbian APIs related to active
// objects and event handling and such things.

#ifndef __epoc_event_internal_h__
#define __epoc_event_internal_h__

#ifdef __EPOC32__

#include "common/evq_event.h" // AoLoop
#include "common/utilities.h"

// This installs an active scheduler for the calling thread. The
// active scheduler may either be the default one, or a customized
// one, depending on the compile-time settings for the application.
EXTERN_C int AoScheduler_install(MAYBE_ERROR_SOLE_PARAM);

EXTERN_C void AoScheduler_uninstall();

// This replaces the current active scheduler with a custom one, if
// the application calls for the use of a custom active scheduler.
// Otherwise it does nothing.
// 
// Note that the standard, default active scheduler does a
// E32USER-CBase panic if there is an unhandled leave in a RunL.
EXTERN_C int AoScheduler_replace(MAYBE_ERROR_SOLE_PARAM);

EXTERN_C AoLoop* AoLoop_new();
EXTERN_C void AoLoop_delete(AoLoop*);
EXTERN_C void AoLoop_start(AoLoop*);
EXTERN_C void AoLoop_stop(AoLoop*);

#endif // __EPOC32__

#endif /* __epoc_event_internal_h__ */
