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

/**

threading_cond.h

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
