#include "common/logging-time.h"

#if __DO_LOGGING__

void log_time_f(const char *logfile, time_t t)
{
  char buf[100];
  struct tm result_tm;
  if (!gmtime_r(&t, &result_tm)) {
    return; // error
  }
  size_t r = strftime(buf, sizeof(buf), "%F %T UTC", &result_tm);
  if (r == 0) return; // did not fit into buffer
  log_text(logfile, buf);
}

#endif // __DO_LOGGING__
