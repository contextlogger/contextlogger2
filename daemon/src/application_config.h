#ifndef __application_config_h__
#define __application_config_h__

#include "common/platform_config.h"
#include "current_config.hrh"

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

/** Log database file.
 */
#define LOGDB_BASENAME "log.db"
#ifdef __SYMBIAN32__
#define LOGDB_DIR "e:\\data\\cl2"
#else
#define LOGDB_DIR "."
#endif
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
