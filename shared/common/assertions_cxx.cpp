#include "common/assertions.h"
#include "common/logging.h"

#if defined(__SYMBIAN32__)

#include <e32std.h>

#if !defined(NDEBUG) && !__DO_LOGGING__

extern "C" void epoc_assert_panic()
{
  User::Invariant();
}

#endif

#if !defined(NDEBUG) && __DO_LOGGING__

extern "C" void epoc_assert(const char *func, const char *file, int line, const char *s)
{
  epoc_log_assert(func, file, line, s);
  //__assert(func, file, line, s); // does not return
  User::Invariant(); // this is more visible than the above, certainly if we have no console
}

#endif

#endif // __SYMBIAN32__
