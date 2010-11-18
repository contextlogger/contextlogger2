#ifndef __platform_config_h__
#define __platform_config_h__

// --------------------------------------------------
// language
// --------------------------------------------------

#if defined(__cplusplus) && defined(__SYMBIAN32__)
#define __SYMBIAN_CXX__ 1
#else
#define __SYMBIAN_CXX__ 0
#endif

/* Throwing and non-throwing "new" for C++. 
 */
#if defined(__cplusplus)
#if defined(__SYMBIAN32__)

// On Symbian, it seems that the (std::nothrow) is not supported,
// although the <new> header is there. The semantics of Symbian's
// default plain "new" are presently the same as for "new
// (std::nothrow)", and hence this definition may suffice, with
// default settings in the MMP file.
#define new_nothrow new

// To implement this, we probably want to define a global, but
// overloaded "new" operator of our own, one that does throw
// std::badalloc. In standard C++ that is the default behavior.
#define new_dothrow xxx

#else
#include <new>
#define new_nothrow new (std::nothrow)
#define new_dothrow new
#endif
#endif

// --------------------------------------------------
// compiler
// --------------------------------------------------

#if defined(__GCCE__)
#define __COMPILER_NAME__ "GCCE"
#elif defined(__GNUC__)
#define __COMPILER_NAME__ "GCC"
#else
#define __COMPILER_NAME__ "non-GCC"
#endif

#if defined(__GNUC__)
#define __GCC_VERSION__ (__GNUC__ * 10000			\
			 + __GNUC_MINOR__ * 100			\
			 + __GNUC_PATCHLEVEL__)
#endif

// Be careful not to use an octal number here.
#if (defined(__GCCE__) && (__GCC_VERSION__ <= 30403))
#define EXCEPTION_CRASHES_VARARGS_BUG 1
#else
#define EXCEPTION_CRASHES_VARARGS_BUG 0
#endif

// --------------------------------------------------
// toolchain
// --------------------------------------------------

// Based on platform version we can compute a numerous
// platform-specific definitions based on the version numbers. We can
// also make use of the definitions in the __PRODUCT_INCLUDE__ should
// that be useful.
// 
// We can expect this component to be directly usable in a number of
// projects, as we are not including any application specifics in this
// header file. Keep those in "application_config.h".
// 
// When checking for boolean style conditionals, you should favor #if
// rather than #ifdef style checks to make it easier to notice if this
// file is not #included where it should.

#ifdef __SYMBIAN32__

// xxx check if __S60_VERNUM__ is defined, and if so, deduce all the
// information that sake gives us here rather than requiring
// sconfig.hrh, or at least the information that we require in CL2

#include "sconfig.hrh"

#endif // __SYMBIAN32__

// --------------------------------------------------
// platform
// --------------------------------------------------

#ifdef __SYMBIAN32__
#define DIR_SEP "\\"
#else
#define DIR_SEP "/"
#endif

// This header is apparently included automatically by the toolchain,
// provided that it holds that #if defined(__PRODUCT_INCLUDE__).
#if 0
#ifdef __SYMBIAN32__
#if defined(__SERIES60_30__)
#include <symbian_os_v9.1.hrh>
#elif defined(__SERIES60_31__)
#include <symbian_os_v9.2.hrh>
#elif defined(__SERIES60_32__)
#include <symbian_os_v9.3.hrh>
#elif defined(__SERIES60_50__)
#include <symbian_os.hrh>
#else
#error unknown Symbian platform version
#endif
#endif
#endif

// --------------------------------------------------
// libraries
// --------------------------------------------------

// It seems that the safer "n" variants of printf and g_printf are all
// broken for floats in Open C. May be fixed in some versions of Open
// C. Could compute this based on version number as well, if new of
// any working version. Implementing a similar replacement in Symbian
// C++ is definitely a possibility.
// 
// Possibly the bug does not apply to all locales.
//
// The functions that we have deemed broken include: vsprintf,
// vsnprintf.
#define PRINTF_DOUBLE_BUGGY defined(__SYMBIAN32__)

#endif /* __platform_config_h__ */

/**

platform_config.h

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
