#ifndef __platform_config_h__
#define __platform_config_h__

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
#include "sconfig.hrh"

#ifndef __SYMBIAN_VERSION__
#error incompatible build tool chain
#endif
#endif // __SYMBIAN32__

#ifdef __SYMBIAN32__
#define DIR_SEP "\\"
#else
#define DIR_SEP "/"
#endif

// It seems that the safer "n" variants of printf and g_printf are all
// broken for floats in Open C. May be fixed in some versions of Open
// C. Could compute this based on version number as well, if new of
// any working version. Implementing a similar replacement in Symbian
// C++ is definitely a possibility.
// 
// The functions that we have deemed broken include: vsprintf,
// vsnprintf.
#define PRINTF_DOUBLE_BUGGY defined(__SYMBIAN32__)

#endif /* __platform_config_h__ */
