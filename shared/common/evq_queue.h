// This API is inspired by Python's Queue.

#ifndef __EVQ_QUEUE_H__
#define __EVQ_QUEUE_H__

#include <pthread.h>

typedef struct __QueueItem {
  struct __QueueItem* next;
} QueueItem;

typedef struct {
  QueueItem* head;
  QueueItem* tail;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int running;
} Queue;

void queue_init(Queue* q);

// Appends the passed item without blocking.
// The passed structure is modified.
// Ownership is not taken.
// Does nothing if the queue has been stopped.
void queue_put(Queue* q, QueueItem* item);

// Removes the specified item, if it is in the queue.
// Does nothing if the queue has been stopped.
void queue_remove(Queue* q, QueueItem* item);

// Blocks until an item becomes available.
// Returns NULL if the queue has been stopped.
QueueItem* queue_get(Queue* q);

// Releases all blocking getters.
// After the queue has been stopped, all puts will be ineffective,
// and all gets will return immediately with a NULL value.
void queue_stop(Queue* q);

// Frees internal resources, but the elements (QueueItem objects) are not freed.
// After calling this, do not attempt to use the queue any longer.
// You may not call this function more than once.
void queue_close(Queue* q);

#endif // __EVQ_QUEUE_H__

/**

evq_queue.h

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
