#ifndef __application_config_h__
#define __application_config_h__

#include "common/platform_config.h"

// Invoked by logging.h.
#define PRIMARY_LOG_FILENAME "sqlite3h_log.txt"

// Invoked internally by logging.cpp.
#define DEFINE_LOG_FILE_DIR _LIT(KLogFileDir, "sqlite3h");

// Invoked internally by logging.cpp.
#define DEFINE_ASSERT_LOG_FILENAME _LIT(KAssertLogFile, "sqlite3h_assert.txt");

#endif /* __application_config_h__ */
