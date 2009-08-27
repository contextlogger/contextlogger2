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
#if 0
  TFileName wLogFile;
  wLogFile.Copy(TPtrC8(reinterpret_cast<const TUint8*>(logfile)));

#if 0
  RFileLogger::Write(KLogFileDir, wLogFile, EFileLoggingModeOverwrite, KNullDesC8);
#else
  // This code does truncate the log file on 3rd ed, but not on 3rd FP1.
  RFileLogger logger;
  TInt error = logger.Connect();
  if (!error) {
    // Neither CreateLog nor CloseLog can error return.
    logger.CreateLog(KLogFileDir, wLogFile, EFileLoggingModeOverwrite);
    logger.CloseLog();
    logger.Close();
  }
#endif
#else
  // This certainly should "truncate" the file on any Symbian version.
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
    fs.Delete(wLogFile); // ignoring any error
    fs.Close();
  }
#endif
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
