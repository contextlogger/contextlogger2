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
