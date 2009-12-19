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

// --------------------------------------------------
// toolchain
// --------------------------------------------------

// Based on __EKA_VERSION__ and __SYMBIAN_VERSION__ and
// __S60_VERSION__ we can compute a numerous platform-specific
// definitions based on the version numbers. We can expect this
// component to be directly usable in a number of projects, as we are
// not including any application specifics in this header file. Keep
// those in "application_config.h".
// 
// When checking for boolean style conditionals, you should favor #if
// rather than #ifdef style checks to make it easier to notice if this
// file is not #included where it should.
// 
// Note that Sake already defines some Symbian specifics for us, and
// we are not recomputing those here, although we could.

#ifdef __SYMBIAN32__

// We will eventually deprecate sconfig.hrh use altogether, but for
// now we assume its availability unless certain platform version
// identifiers have been #defined.
#if defined(__XXX_S60_VERSION__)
// xxx Define the sconfig.hrh stuff here.
#elif defined(__SYMBIANHAT_VERSION__)
// xxx Define the sconfig.hrh stuff here. based on Symbian^version
#else
#include "sconfig.hrh"
#endif

// xxx Is this set in sconfig.hrh?
#ifndef __SYMBIAN_VERSION__
#error incompatible build tool chain
#endif

#endif // __SYMBIAN32__

// --------------------------------------------------
// platform
// --------------------------------------------------

#ifdef __SYMBIAN32__
#define DIR_SEP "\\"
#else
#define DIR_SEP "/"
#endif

// xxx Is this already included automatically by the toolchain?
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

// Be careful not to use an octal number here.
#if (defined(__GCCE__) && (__GCC_VERSION__ <= 30403))
#define EXCEPTION_CRASHES_VARARGS_BUG 1
#else
#define EXCEPTION_CRASHES_VARARGS_BUG 0
#endif

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
