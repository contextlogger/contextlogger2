#include "common/evq_timer.h"

#include "common/logging.h"
#include "common/assertions.h"

#include <assert.h>
#include <asm-generic/errno.h> // ETIMEDOUT
#include <stdio.h>

// We will have the event framework make this callback when it is time to process an event.
static MAYBE_ERROR_RTYPE event_cb(Event* event MAYBE_ERROR_PARAM)
{
  Timer* timer = (Timer*)event;
  MAYBE_ERROR_INVOKE((*timer->callback), 0, timer->user_data);
  MAYBE_ERROR_RETURN;
}

static void* worker_task(void* arg)
{
  int error;
  Timer* timer = (Timer*)arg;
  pthread_mutex_lock(&timer->mutex);
  while (timer->running) {
    if (timer->active) {
      // Wait for a signal with a timer.
      //logt("doing timed wait");
      // See asm-generic/errno.h and asm-generic/errno-base.h for the
      // relevant error codes.
      error = pthread_cond_timedwait(&timer->cond, &timer->mutex, timer->time);
      //logf("error is %d", error);
      if (timer->active) {
	if (error == ETIMEDOUT) {
	  timer->active = 0;
	  event_put(timer->queue, (Event*)timer);
	} else if (error) {
	  logf("pthread_cond_timedwait error %d", error);
	  assert(0 && "pthread_cond_timedwait invalid args?");
	}
      }
    } else {
      // Wait for a signal without a timer.
      pthread_cond_wait(&timer->cond, &timer->mutex); // no error code
    }
  }
  pthread_mutex_unlock(&timer->mutex);
  return ((void*)0);
}

void timer_init(EventQueue* queue, Timer* timer)
{
  timer->queue = queue;
  timer->active = 0;
  timer->running = 1;
  timer->event.callback = &event_cb;
  pthread_mutex_init(&timer->mutex, NULL);
  pthread_cond_init(&timer->cond, NULL);
  pthread_create(&timer->worker, NULL, &worker_task, timer);
}

void timer_at(Timer* timer, struct timespec* time, TimerCallback* callback, void* user_data)
{
  pthread_mutex_lock(&timer->mutex);
  assert(!timer->active && "timer already active");
  timer->time = time;
  timer->callback = callback;
  timer->user_data = user_data;
  timer->active = 1;
  pthread_cond_signal(&timer->cond);
  pthread_mutex_unlock(&timer->mutex);
}

void timer_cancel(Timer* timer)
{
  pthread_mutex_lock(&timer->mutex);
  if (timer->active) {
    timer->active = 0;
    pthread_cond_signal(&timer->cond);
  } else {
    event_remove(timer->queue, (Event*)timer);
  }
  pthread_mutex_unlock(&timer->mutex);
}

void timer_close(Timer* timer)
{
  timer_cancel(timer);

  pthread_mutex_lock(&timer->mutex);
  timer->running = 0;
  pthread_cond_signal(&timer->cond);
  pthread_mutex_unlock(&timer->mutex);

  void* exitValue;
  pthread_join(timer->worker, &exitValue);
  printf("timer worker exited with %d\n", (int)exitValue);

  // Destroying these is perfectly safe now that the worker thread has stopped running. Other than the owner thread no other thread should any longer be accessing this object, and even the owner should not after calling timer_close.
  pthread_cond_destroy(&timer->cond);
  pthread_mutex_destroy(&timer->mutex);

  // This ensures that once this call returns, the owning thread will not be processing any events relating to this timer.
  event_remove(timer->queue, (Event*)timer);
}

/**

evq_timer.c

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
