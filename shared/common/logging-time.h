#ifndef __logging_time_h__
#define __logging_time_h__

#include <time.h>
#include "common/logging.h"

#ifdef __cplusplus
extern "C" {
#endif

#if __DO_LOGGING__
  void log_time_f(const char *logfile, time_t t);
#else
#define log_time_f(f,t) ((void)0)
#endif

#define log_time(t) log_time_f(PRIMARY_LOG_FILENAME, t)

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __logging_time_h__ */
