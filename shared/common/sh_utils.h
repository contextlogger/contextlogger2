#ifndef __sh_utils_h__
#define __sh_utils_h__

#include <string.h> // memset

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

#define memzero(x) memset(&(x), 0, sizeof(x))

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif

#ifdef __cplusplus
#define DEFINE_CLEANUP_CLASS(name,type,free) \
class name \
{ \
 public: \
 name(type aObj) : iObj(aObj) {} \
  ~##name() { free; }		 \
 private: \
  type iObj; \
}
#endif

#if defined(__SYMBIAN32__)
#define IF_SYMBIAN_EXPR(x,y) (x)
#define IF_SYMBIAN(x,y) { x ; }
#define WHEN_SYMBIAN(stmt) { stmt ; }
#define UNLESS_SYMBIAN(stmt)
#else
#define IF_SYMBIAN_EXPR(x,y) (y)
#define IF_SYMBIAN(x,y) { y ; }
#define WHEN_SYMBIAN(stmt)
#define UNLESS_SYMBIAN(stmt) { stmt ; }
#endif /* __SYMBIAN32__ */

#if defined(__SYMBIAN32__) && defined(__cplusplus)
#define DEFINE_FOR_SYMBIAN_CXX(def) def
#define WHEN_SYMBIAN_CXX(stmt) { stmt ; }
#define UNLESS_SYMBIAN_CXX(stmt)
#else
#define DEFINE_FOR_SYMBIAN_CXX(def)
#define WHEN_SYMBIAN_CXX(stmt)
#define UNLESS_SYMBIAN_CXX(stmt) { stmt ; }
#endif

#endif /* __sh_utils_h__ */

/**

sh_utils.h

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
