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

/**

epoc-event-internal.h

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
