// This component produces timer events. On Symbian, it should be implementable based on RTimer. On a POSIX system, an implementation based on pthread_cond_timedwait should work just fine. In fact, the same implementation may just work with Open C on Symbian.

// Note that this API is not intended to be thread safe, even though some worker thread(s) may be used internally.

#ifndef __EVQ_TIMER_H__
#define __EVQ_TIMER_H__

#include "common/evq_event.h"

// 0 value indicates no error.
// Other error codes are POSIX style.
typedef MAYBE_ERROR_RTYPE (TimerCallback)(int errCode, void* user_data MAYBE_ERROR_PARAM);

// Only one timer request per Timer object may be outstanding at any one time.
typedef struct {
  Event event;
  EventQueue* queue;
  pthread_t worker;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int running; // worker thread is running
  int active; // timer is active
  struct timespec* time;
  TimerCallback* callback;
  void* user_data;
} Timer;

// Initializes the passed timer object.
// You must specify the event queue of the thread in which any callbacks are to be made.
// In the Symbian OS implementation the "queue" argument is pointless, and may be passed as NULL.
void timer_init(EventQueue* queue, Timer* timer);

// Makes a timer request. Expiration is at the specified absolute
// time.
// time:: Expiration time is not copied, and hence must persist until
//        completion.
void timer_at(Timer* timer, struct timespec* time, TimerCallback* callback, void* user_data);

// Cancels any outstanding timer request.
// No harm in calling this if there is no outstanding request.
void timer_cancel(Timer* timer);

// Frees the resources associated with the timer object.
void timer_close(Timer* timer);

#endif //  __EVQ_TIMER_H__
