#ifndef __guilog_h__
#define __guilog_h__

#include "application_config.h"

#if __FEATURE_GUILOG__

#ifdef __cplusplus
extern "C" {
#endif

  // Give any strings as UTF-8.
  void guilogf(const char* fmt, ...);

#ifdef __cplusplus
} /* extern "C" */
#endif

#if defined(__cplusplus)
void guilog(const char* s);
#endif

#if defined(__cplusplus) && defined(__SYMBIAN32__)

#include <e32cmn.h>

void guilog(const TDesC8& s);
void guilog(const TDesC16& s);

#endif /* __SYMBIAN32__ */

#if defined(__cplusplus) && defined(QT_CORE_LIB)

#include <QString>

void guilog(const QString& s);

#endif

#else // if __FEATURE_GUILOG__

#define guilogf(f...) ((void)0)
#define guilog(f) ((void)0)

#endif // if __FEATURE_GUILOG__

#endif /* __guilog_h__ */

/**

Copyright 2011 Helsinki Institute for Information Technology (HIIT)
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
