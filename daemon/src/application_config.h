#ifndef __application_config_h__
#define __application_config_h__

#include "common/platform_config.h"
#include "current_config.hrh"

// NDEBUG controls whether asserts are to be compiled in (NDEBUG is
// defined automatically for Symbian UDEB builds). Normally an assert
// results in something being printed to the console. Whether the
// assertion errors are also logged is controlled by __DO_LOGGING__.
#if !defined(__SYMBIAN32__) && !__FEATURE_DEBUGGING__
#define NDEBUG 1
#endif

#if __FEATURE_UPLOADER__ && !__UPLOAD_WITH_CURL__
#if !defined(__UPLOAD_TIME_EXPR__)
#error some default upload time expression should be defined
#endif
#if !defined(__USERNAME__)
#error some default username should be defined
#endif
#if !defined(__UPLOAD_URL__)
#error some default upload url should be defined
#endif
#if !defined(__IAP_ID__)
#error some default iap id should be defined
#endif
#endif /* __FEATURE_UPLOADER__ */

#if !defined(__SYMBIAN32__)
#define EVENT_CALLBACK_WITH_GERROR 1
#endif /* __SYMBIAN32__ */

#define NO_THREAD_SAFETY defined(__EPOC32__)
#define THREAD_SAFETY (!(NO_THREAD_SAFETY))

// Adapted from _LIT macro. For this one "s" must be a wide (i.e., L"") string.
#define _LIT_W(name,s) const static TLitC<sizeof(s)/2> name={sizeof(s)/2-1,s}

#define COMPONENT_NAME "cl2app"
#define COMPONENT_NAME_W L"cl2app"

#define COMPONENT_NAME_LIT8(ident) _LIT8(ident, COMPONENT_NAME)
#define COMPONENT_NAME_LIT(ident) _LIT_W(ident, COMPONENT_NAME_W)

#define PRIMARY_LOG_FILENAME "cl2app_log.txt"

// Invoked internally by logging.cpp.
#define DEFINE_LOG_FILE_DIR _LIT(KLogFileDir, "cl2");

// Invoked internally by logging.cpp.
#define DEFINE_ASSERT_LOG_FILENAME _LIT(KAssertLogFile, "cl2app_assert.txt");

// Invoked internally by panic.cpp.
#define DEFINE_PANIC_CATEGORY COMPONENT_NAME_LIT(KPanicCategory)

#ifdef __SYMBIAN32__
  // Note that anything under C: is automatically included in system
  // backups, so do not change this elsewhere without good reason. (We
  // might want to use RFs::GetSystemDrive() to get the drive letter
  // instead of assuming "C:".)
#define CONFIG_DIR "c:\\data\\cl2"
#define DATABASE_DIR "e:\\data\\cl2"
#define DATABASE_DRIVE_LETTER 'e'
#define LOG_DISK_THRESHOLD_DEFAULT 10e6
#else
#define CONFIG_DIR "."
#define DATABASE_DIR "."
#endif

/** Log database file.
 */
#define LOGDB_BASENAME "log.db"
#define LOGDB_DIR DATABASE_DIR
#define LOGDB_FILE (LOGDB_DIR DIR_SEP LOGDB_BASENAME)

/** These options should generally not be enabled,
    but may be useful for testing and debugging.
 */
//#define ALWAYS_RECREATE_DB !defined(NDEBUG)
#define ALWAYS_RECREATE_DB 0

#ifdef __cplusplus
extern "C" {
#endif

// If running a daemon, then most likely you wish to stop the entire
// process if there is a fatal error, and leave it for the watchdog to
// decide when to try to restart. If running within a host
// application, then you might just want to stop the logger component
// (or take some application-specific action), and leave the actual
// application running; in this case it is the responsibility of the
// application to attempt restarting as desired. In any case, you
// should define EXIT_APPLICATION to map to the desired behavior. Note that
// you should not assume that EXIT_APPLICATION will not return, rather
// consider it as making a request to get CL2 stopped soon.
// 
// Note that ExitApplication() is implemented differently on Symbian
// depending on whether __IS_APPLICATION__ is true.
#ifdef __SYMBIAN32__
  void ExitApplication();
#define EXIT_APPLICATION ExitApplication()
#else
#include <stdlib.h>
#define EXIT_APPLICATION abort()
#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __application_config_h__ */

/**

application_config.h

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
