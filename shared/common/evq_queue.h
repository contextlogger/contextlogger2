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
