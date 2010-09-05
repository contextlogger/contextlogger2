// Logging utilities.
// 
// Note that the Symbian implementation does not depend on the
// standard C library. Rather basic Symbian libraries and RFileLogger
// are enough. This is to avoid an Open C dependency for such commonly
// required functionality.
// 
// The non-Symbian implementation is using standard C libraries only,
// for maximum portability.
// 
// Note that regardless of implementation, we require that logging be
// best effort, i.e. that failure to log in itself never disrupt the
// rest of the program. So, for instance, there must never be a
// non-local return from these functions, nor should anything invoke
// abort(), even if there is an out of memory error, or exhaustion of
// file handles, or whatever.
//
// !concept {:name => "Portable logging"}

#ifndef __LOGGING_H__
#define __LOGGING_H__

// Note that in this API, the "logfile" argument must always be given
// as an ASCII string.

#include "application_config.h"

// Whether or not to do logging is controlled by the __DO_LOGGING__
// flag alone. Logging is done differently on different platforms,
// however.
#if __DO_LOGGING__

#define WHEN_LOGGING(_act) { _act; }

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__EPOC32__)
    void log_clear(const char *logfile);
#else
#define log_clear(logfile) ((void)0)
#endif

    void log_text(const char *logfile, const char *s);
    void __log_ctx(const char *logfile, const char *func, const char *file, int line, const char *s);
    void log_fmt(const char* logfile, const char* fmt, ...);

#define log_ctx(logfile,s) __log_ctx(logfile, __func__, __FILE__, __LINE__, s)

#if defined(__EPOC32__)
    void epoc_log_assert(const char *func, const char *file, int line, const char *s);
#endif

#ifdef __cplusplus
}
#endif

#else // no logging

#define WHEN_LOGGING(_act)

#define log_clear(logfile) ((void)0)
#define log_text(logfile,s) ((void)0)
#define log_ctx(logfile,s) ((void)0)
#define log_fmt(logfile,f...) ((void)0)

#if defined(__EPOC32__)
#define epoc_log_assert(func,file,line,s) ((void)0)
#endif

#endif // no logging

#define logt(s) log_text(PRIMARY_LOG_FILENAME, s)
#define logc(s) log_ctx(PRIMARY_LOG_FILENAME, s)
#define logf(f...) log_fmt(PRIMARY_LOG_FILENAME, f)

#endif // __LOGGING_H__

/**

logging.h

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
