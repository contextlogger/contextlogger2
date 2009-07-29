#ifndef __threading_h__
#define __threading_h__

#include <pthread.h>
#include "application_config.h"
#include "common/assertions.h"

#if NO_THREAD_SAFETY
#define mutex_init(m)
#define mutex_lock(m)
#define mutex_unlock(m)
#define mutex_destroy(m)
#endif

#if !NO_THREAD_SAFETY
#define mutex_init(m) pthread_mutex_init(m, NULL)
#endif

#define cond_init(c) pthread_cond_init(c, NULL)
#define cond_signal(c) pthread_cond_signal(c)
#define cond_broadcast(c) pthread_cond_broadcast(c)
#define cond_wait(c,m) pthread_cond_wait(c, m)
#define cond_timedwait(c,m,t) pthread_cond_timedwait(c, m, t)

#ifdef NDEBUG

#if !NO_THREAD_SAFETY
#define mutex_lock(m) pthread_mutex_lock(m)
#define mutex_unlock(m) pthread_mutex_unlock(m)
#define mutex_destroy(m) pthread_mutex_destroy(m)
#endif

#define cond_destroy(c) pthread_cond_destroy(c)

#else

// Note that if NDEBUG is defined at the time when <assert.h> is
// included, then "assert" generates no code. Otherwise it does, and
// we rely on that here.
// 
// Mutex use should never produce an error unless there is a coding
// error of some sort.
// 
// pthread_cond_destroy should likewise never produce an error unless
// there is a coding error of some sort. The same is not true for
// pthread_cond_timedwait.

#if !NO_THREAD_SAFETY
#define mutex_init(m) pthread_mutex_init(m, NULL)
#define mutex_lock(m) assert(!pthread_mutex_lock(m))
#define mutex_unlock(m) assert(!pthread_mutex_unlock(m))
#define mutex_destroy(m) assert(!pthread_mutex_destroy(m))
#endif

#define cond_destroy(c) assert(!pthread_cond_destroy(c))

#endif

#endif /* __threading_h__ */
