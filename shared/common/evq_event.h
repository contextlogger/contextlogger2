// Sometimes one requires a way to perform callbacks in the main
// thread from a worker thread, and this component implements a
// mechanism for that. The approach is to store each callback in a
// queue processed by the main thread. The task of this generic
// callback is to call the actual, event-type specific callback.
// 
// This interface provides the functionality required to get such
// callbacks happening in different scenarios. Note that a typical
// Symbian application already comes with a thread that has the
// necessary mechanisms in place, and hence this interface is
// typically not required in such applications.
// 
// We have two variants of this interface, one intended for situations
// where there are no errors that should cause the event loop to exit.
// There is not much difference between the two, and hence we use the
// same component and header file for both; use the
// EVENT_CALLBACK_WITH_GERROR compile-time definition to indicate
// which it is that you want. The EVENT_CALLBACK_WITH_GERROR==1
// version provides a superset of the functionality, essentially, but
// slightly less efficiently.
// 
// One issue here naturally is the difference in the callback APIs,
// which basically require adjustments in every event source. Again,
// the EVENT_CALLBACK_WITH_GERROR #define can be checked in all event
// sources that must support both variants.

#ifndef __EVQ_EVENT_H__
#define __EVQ_EVENT_H__

#include "application_config.h"

#if EVENT_CALLBACK_WITH_GERROR
#include <glib.h>
#endif

#if EVENT_CALLBACK_WITH_GERROR
#define MAYBE_ERROR_RTYPE gboolean
#define MAYBE_ERROR_PARAM , GError** error
#define MAYBE_ERROR_SOLE_PARAM GError** error
#define MAYBE_ERROR_SOLE_ARG error
#define MAYBE_ERROR_INVOKE(func,args...) { if (!(func(args,error))) return FALSE; }
#define MAYBE_ERROR_RETURN return TRUE;
#else
#define MAYBE_ERROR_RTYPE void
#define MAYBE_ERROR_PARAM
#define MAYBE_ERROR_SOLE_PARAM
#define MAYBE_ERROR_SOLE_ARG
#define MAYBE_ERROR_INVOKE(func,args...) (func(args));
#define MAYBE_ERROR_RETURN return;
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __EPOC32__
typedef void AoLoop;

typedef struct {
  AoLoop* aoLoop; // CActiveSchedulerWait* or NULL
} EventQueue;
#else
#include "common/evq_queue.h"

typedef struct _Event {
  QueueItem item;
  MAYBE_ERROR_RTYPE (*callback)(struct _Event* event MAYBE_ERROR_PARAM);
  // Any event specific data goes here.
} Event;

typedef Queue EventQueue;
#endif

// On POSIX, initializes an event queue.
// 
// On Symbian, installs a (new) active scheduler for the calling
// thread. This is essentially a call to CActiveScheduler::Install.
void event_init(EventQueue* queue);

// Processes events until requested otherwise, or until a callback
// produces an error.
// 
// On POSIX, this runs the event queue.
// 
// On Symbian, this creates a nested scheduling loop in the current
// active scheduler, using a CActiveSchedulerWait to control the loop;
// this call essentially invokes CActiveSchedulerWait::Start.
// 
// Returns if the loop is exited due to a call to "event_loop_stop".
//
// The EVENT_CALLBACK_WITH_GERROR variant will also exit due to an an error produced by a
// callback, in which case that error is returned.
MAYBE_ERROR_RTYPE event_loop(EventQueue* queue MAYBE_ERROR_PARAM);

#ifndef __EPOC32__
// Adds an event into the event queue.
// 
// This function is not implemented for Symbian, where you must
// instead make requests to active objects to add events.
void event_put(EventQueue* queue, Event* event);

// Removes an event from the event queue (if it is there).
//
// Again, this function is not implemented for Symbian.
void event_remove(EventQueue* queue, Event* event);
#endif

// Stops the event loop.
// 
// On POSIX, this simply stops running the event queue.
// 
// On Symbian, this essentially invokes
// CActiveSchedulerWait::AsyncStop.
void event_loop_stop(EventQueue* queue);

// Frees up any resources associated with an event queue.
// 
// On Symbian, uninstalls and deletes (the current) active scheduler
// of the calling thread.
void event_close(EventQueue* queue);

// These can be used for active scheduler installation on Symbian if
// required. They presently have no effect on other platforms.
void event_scheduler_install(MAYBE_ERROR_SOLE_PARAM);
void event_scheduler_uninstall();
void event_scheduler_replace(MAYBE_ERROR_SOLE_PARAM);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __EVQ_EVENT_H__ */
