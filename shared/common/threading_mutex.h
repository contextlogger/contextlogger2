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

/**

threading_mutex.h

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
