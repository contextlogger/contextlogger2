// !concept {:name => "Portable logging"}

#include "common/logging.h"

#if __DO_LOGGING__

#if defined(__EPOC32__)

#include <f32file.h> // RFs
#include <flogger.h> // flogger.lib required, Symbian 7.0-up

DEFINE_LOG_FILE_DIR; // KLogFileDir _LIT definition
DEFINE_ASSERT_LOG_FILENAME; // KAssertLogFile _LIT definition

extern "C"
void log_clear(const char *logfile)
{
  // We had some trouble with higher-level APIs on some Symbian
  // versions. This certainly should "truncate" the file on any
  // Symbian version.
  RFs fs;
  if (fs.Connect() == KErrNone) {
    TFileName wLogFile;
    _LIT(KLogPath, "c:\\logs\\");
    _LIT(KSlash, "\\");
    wLogFile.Copy(KLogPath);
    wLogFile.Append(KLogFileDir);
    wLogFile.Append(KSlash);
    TBuf<32> buf;
    buf.Copy(TPtrC8(reinterpret_cast<const TUint8*>(logfile)));
    wLogFile.Append(buf);
    TInt errCode = fs.Delete(wLogFile);
    fs.Close();

    if (errCode && (errCode != KErrNotFound)) {
      if (errCode == KErrInUse)
        // If you get this, make sure you do not log anything _before_
        // calling log_clear.
	log_fmt(logfile, "logfile truncation failed: in use (%d)", errCode);
      else
	log_fmt(logfile, "logfile truncation failed: %d", errCode);
    }
  }
}

extern "C"
void log_text(const char *logfile, const char *s)
{
  TFileName wLogFile;
  wLogFile.Copy(TPtrC8(reinterpret_cast<const TUint8*>(logfile)));
  RFileLogger::Write(KLogFileDir, wLogFile, EFileLoggingModeAppend, TPtrC8(reinterpret_cast<const TUint8*>(s)));
}

_LIT8(KCtxFmt, "func %s, file %s, line %d: %s");

extern "C"
void __log_ctx(const char *logfile, const char *func, const char *file, int line, const char *s)
{
  TFileName wLogFile;
  wLogFile.Copy(TPtrC8(reinterpret_cast<const TUint8*>(logfile)));
  RFileLogger::WriteFormat(KLogFileDir, wLogFile, EFileLoggingModeAppend, KCtxFmt, func, file, line, s);
}

extern "C"
void log_fmt(const char* logfile, const char* fmt, ...)
{
  VA_LIST argp;
  VA_START(argp, fmt);

  TFileName wLogFile;
  wLogFile.Copy(TPtrC8(reinterpret_cast<const TUint8*>(logfile)));

  RFileLogger::WriteFormat(KLogFileDir, wLogFile, EFileLoggingModeAppend, TPtrC8(reinterpret_cast<const TUint8*>(fmt)), argp);
}

#ifndef NDEBUG
extern "C"
void epoc_log_assert(const char *func, const char *file, int line, const char *s)
{
  RFileLogger::WriteFormat(KLogFileDir, KAssertLogFile, EFileLoggingModeOverwrite, KCtxFmt, func, file, line, s);
}
#endif

#else // not Symbian

#include <stdio.h>
#include <stdarg.h> // va_list

extern "C"
void log_text(const char *logfile, const char *s)
{
  printf("[%s] %s\n", logfile, s);
}

extern "C"
void __log_ctx(const char *logfile, const char *func, const char *file, int line, const char *s)
{
  printf("[%s] func %s, file %s, line %d: %s\n", logfile, func, file, line, s);
}

extern "C"
void log_fmt(const char* logfile, const char* fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);
  printf("[%s] ", logfile);
  vprintf(fmt, argp);
  printf("\n");
  va_end(argp); // any cleanup
}

#endif // not Symbian
#endif // no logging

/**

logging.cpp

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
