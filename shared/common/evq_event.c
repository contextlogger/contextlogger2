#include "common/evq_event.h"

#include "common/assertions.h"

#include <stdlib.h>

// --------------------------------------------------------------------------------
// for POSIX...

#ifndef __EPOC32__

void event_init(EventQueue* queue)
{
  queue_init(queue);
}

#if EVENT_CALLBACK_WITH_GERROR
gboolean event_loop(EventQueue* queue, GError** error)
{
  Event* event;
  for (;;) {
    event = (Event*)queue_get(queue);
    if (!event)
      break;
    if (!((event->callback)(event, error))) {
      assert_error_set(error);
      return FALSE;
    }
  }
  return TRUE;
}
#else
void event_loop(EventQueue* queue)
{
  Event* event;
  for (;;) {
    event = (Event*)queue_get(queue);
    if (!event)
      break;
    (event->callback)(event);
  }
}
#endif

void event_put(EventQueue* queue, Event* event)
{
  queue_put((Queue*)queue, (QueueItem*)event);
}

void event_remove(EventQueue* queue, Event* event)
{
  queue_remove((Queue*)queue, (QueueItem*)event);
}

void event_loop_stop(EventQueue* queue)
{
  queue_stop(queue);
}

void event_close(EventQueue* queue)
{
  queue_close(queue);
}

#endif

// --------------------------------------------------------------------------------
// for Symbian...

#ifdef __EPOC32__

#include "common/epoc-event-internal.h"

void event_init(EventQueue* dummy)
{
}

// Processes events until requested otherwise. On POSIX, this runs the
// event queue. On Symbian, this creates a nested scheduling loop in
// the current active scheduler, using a CActiveSchedulerWait to
// control the loop; this call essentially invokes
// CActiveSchedulerWait::Start.
MAYBE_ERROR_RTYPE event_loop(EventQueue* queue MAYBE_ERROR_PARAM)
{
  assert(queue && "event queue may not be NULL when using nested loops");

#if EVENT_CALLBACK_WITH_GERROR
  assert_error_unset(error);

  // Use a scheduler that records errors to our error argument as appropriate.
  if (error)
    event_scheduler_replace(error);
#endif

  if (!queue->aoLoop) {
    AoLoop* loop = AoLoop_new();
    if (!loop)
      abort();
    queue->aoLoop = loop;
  }

  AoLoop_start(queue->aoLoop);

#if EVENT_CALLBACK_WITH_GERROR
  // Note that here we exceptionally ignore errors if "error" was
  // passed as NULL. That is, we never return FALSE in such a case.
  return !(error && *error);
#endif
}

// Stops the event loop. On POSIX, this simply stops running the event
// queue. On Symbian, this essentially invokes
// CActiveSchedulerWait::AsyncStop.
void event_loop_stop(EventQueue* queue)
{
  assert(queue && "event queue may not be NULL when using nested loops");
  assert(queue->aoLoop && "not looping");
  AoLoop_stop(queue->aoLoop);
}

// Frees up any resources associated with an event queue.
void event_close(EventQueue* queue)
{
  if (queue && queue->aoLoop) {
    AoLoop_delete(queue->aoLoop);
    queue->aoLoop = NULL;
  }
}

#endif

// --------------------------------------------------------------------------------
// common...

// On Symbian, installs a (new) active scheduler for the calling
// thread. This is essentially a call to CActiveScheduler::Install.
void event_scheduler_install(MAYBE_ERROR_SOLE_PARAM)
{
#ifdef __EPOC32__
  if (AoScheduler_install(MAYBE_ERROR_SOLE_ARG))
    abort(); // out of memory
#endif
}

// On Symbian, uninstalls and deletes (the current) active scheduler
// of the calling thread.
void event_scheduler_uninstall()
{
#ifdef __EPOC32__
  AoScheduler_uninstall();
#endif
}

void event_scheduler_replace(MAYBE_ERROR_SOLE_PARAM)
{
#ifdef __EPOC32__
  if (AoScheduler_replace(MAYBE_ERROR_SOLE_ARG))
    abort(); // out of memory
#endif
}

/**

evq_event.c

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
