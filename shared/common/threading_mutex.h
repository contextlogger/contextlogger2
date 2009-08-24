#ifndef __threading_mutex_h__
#define __threading_mutex_h__

#include "application_config.h"

#if NO_THREAD_SAFETY

#define mutex_init(m)
#define mutex_lock(m)
#define mutex_unlock(m)
#define mutex_destroy(m)

#else // NO_THREAD_SAFETY

// Note that if NDEBUG is defined at the time when <assert.h> is
// included, then "assert" generates no code. Otherwise it does, and
// we rely on that here.
// 
// Mutex use should never produce an error unless there is a coding
// error of some sort.

#ifdef NDEBUG
#define mutex_assert_no_err(expr) expr
#else
#include "common/assertions.h"
#define mutex_assert_no_err(expr) assert((expr) == 0)
#endif //  NDEBUG

#include <pthread.h>

#define mutex_init(m) mutex_assert_no_err(pthread_mutex_init(m, NULL))
#define mutex_lock(m) mutex_assert_no_err(pthread_mutex_lock(m))
#define mutex_unlock(m) mutex_assert_no_err(pthread_mutex_unlock(m))
#define mutex_destroy(m) mutex_assert_no_err(pthread_mutex_destroy(m))

#endif // NO_THREAD_SAFETY

#define mutex_synchronized(m,stmt) {		\
    mutex_lock(m);				\
    stmt;					\
    mutex_unlock(m);				\
  }

#endif /* __threading_mutex_h__ */
