// !concept {:name => "Portable assertions"}

#ifndef __ASSERTIONS_H__
#define __ASSERTIONS_H__

// Whether or not to compile in assertions is controlled by NDEBUG.
// The kind of assertion failure reporting that is done may be
// affected by the __DO_LOGGING__ flag.

#include "application_config.h"

#ifndef __EPOC32__
#include <assert.h>
#endif

#ifdef __EPOC32__

#if !defined(NDEBUG)

//#undef assert

#ifdef __cplusplus
extern "C" {
#endif

#if __DO_LOGGING__
// assertions are logged
void epoc_assert(const char *func, const char *file, int line, const char *s);
#define	assert(e) ((e) ? (void)0 : epoc_assert(__func__, __FILE__, __LINE__, #e))
#else
// assertions cause a panic
void epoc_assert_panic();
#define assert(e) ((e) ? (void)0 : epoc_assert_panic())
#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif // !defined(NDEBUG)

#endif // __EPOC32__

#define assert_error_unset(x) assert((!(x) || !*(x)) && "error already set upon entering function")

#define assert_error_set(x) assert((!(x) || *(x)) && "error not set despite error return of subroutine")

// Some compilers might whine about it being statically known that the GError** is non-NULL. This version deals directly with GError*, and naturally makes no such check.
#define assert_error_set_direct(x) assert((x) && "error not set despite error return of subroutine")

#define assert_error_unset_direct(x) assert(!(x) && "error not set despite error return of subroutine")

#endif // __ASSERTIONS_H__

/**

assertions.h

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
