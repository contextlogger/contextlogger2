#include "common/evq_queue.h"

#include "common/threading.h"

#include <stdio.h>
#include <unistd.h>
#include <assert.h>

void queue_init(Queue* q)
{
  q->head = q->tail = NULL;
  mutex_init(&q->mutex);
  cond_init(&q->cond);
  q->running = 1;
}

void queue_put(Queue* q, QueueItem* item)
{
  mutex_lock(&q->mutex);

  if (q->running) {
    if (q->tail) {
      q->tail->next = item;
      q->tail = item;
    } else {
      q->head = q->tail = item;
    }
    item->next = NULL;
    cond_signal(&q->cond); // always succeeds
  }

  mutex_unlock(&q->mutex);
}

// Removes the first item of "q", which must exist.
static void unshift(Queue* q) {
  QueueItem* shifted;
  shifted = q->head;
  assert(shifted && "unshift on empty queue");
  if (shifted->next) {
    q->head = shifted->next;
    if (!q->head->next)
      q->tail = q->head;
  } else {
    q->head = q->tail = NULL;
  }
}

void queue_remove(Queue* q, QueueItem* item)
{
  mutex_lock(&q->mutex);

  if (q->running && q->head) {
    if (q->head == item) {
      unshift(q);
    } else {
      QueueItem* p = q->head;
      while (p->next) {
        if (p->next == item) {
          p->next = p->next->next;
          if (!p->next) q->tail = p;
          break;
        }
        p = p->next;
      }
    }
  }

  mutex_unlock(&q->mutex);
}

QueueItem* queue_get(Queue* q)
{
  QueueItem* item = NULL;

  mutex_lock(&q->mutex);

  while (q->running) {
    if (q->head) {
      item = q->head;
      unshift(q);
      break;
    } else {
      cond_wait(&q->cond, &q->mutex); // never returns an error
      // Note that due to the existence of queue_remove, it is possible that we get a signal without there actually being any items in the queue any longer.
    }
  }

  mutex_unlock(&q->mutex);

  return item;
}

// Set to stopped and notify any and all waiters that no further
// waiting on the condition is allowed any longer, and that there will
// be no more items in the queue.
void queue_stop(Queue* q)
{
  mutex_lock(&q->mutex);

  q->running = 0;
  cond_broadcast(&q->cond); // always succeeds

  mutex_unlock(&q->mutex);
}

void queue_close(Queue* q)
{
  // On Linux, there are no resources to free, but this might not be the case on other platforms. There is little to do if this fails since we do want "close" to always succeed. Note that there may not be threads waiting on the condition when we make this call.
  cond_destroy(&q->cond);

  // On Linux, there are no resources to free, but this might not be the case on other platforms. There is little to do if this fails since we do want "close" to always succeed. Note that the mutex may not be locked when we make this call.
  mutex_destroy(&q->mutex);
}

/**

evq_queue.c

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
