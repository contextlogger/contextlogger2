#ifndef __threading_cond_h__
#define __threading_cond_h__

#include <pthread.h>

#define cond_init(c) pthread_cond_init(c, NULL)
#define cond_signal(c) pthread_cond_signal(c)
#define cond_broadcast(c) pthread_cond_broadcast(c)
#define cond_wait(c,m) pthread_cond_wait(c, m)
#define cond_timedwait(c,m,t) pthread_cond_timedwait(c, m, t)

#ifdef NDEBUG

#define cond_destroy(c) pthread_cond_destroy(c)

#else

#include "common/assertions.h"

// pthread_cond_destroy should likewise never produce an error unless
// there is a coding error of some sort. The same is not true for
// pthread_cond_timedwait.
#define cond_destroy(c) assert(!pthread_cond_destroy(c))

#endif

#endif /* __threading_cond_h__ */
