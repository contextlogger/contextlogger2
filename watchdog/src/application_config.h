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
