// !concept {:name => "Portable assertions", :desc => "Assertions that are actually visible or logged on Symbian."}

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

/**

assertions_cxx.cpp

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
