#ifndef __application_config_h__
#define __application_config_h__

#include "common/platform_config.h"

#define PRIMARY_LOG_FILENAME "watchdog_log.txt"

// Invoked internally by logging.cpp.
#define DEFINE_LOG_FILE_DIR _LIT(KLogFileDir, "cl2");

// Invoked internally by logging.cpp.
#define DEFINE_ASSERT_LOG_FILENAME _LIT(KAssertLogFile, "watchdog_assert.txt");

// Invoked internally by panic.cpp.
#define DEFINE_PANIC_CATEGORY _LIT(KPanicCategory, "cl2watchdog")

#endif /* __application_config_h__ */
