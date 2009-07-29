#include "time_utils.h"

int int_range(int value, int min, int max)
{
  return (((value) < (min)) ? (min) : (((value) > (max)) ? (max) : (value)));
}

// Returns NULL on error, otherwise "buf".
char* time_t_to_string(char* buf, int buflen, time_t t)
{
  struct tm result_tm;
  if (!gmtime_r(&t, &result_tm)) {
    return NULL; // error
  }
  if (strftime(buf, buflen, "%F %T", &result_tm) == 0) {
    // With this format string, 0 size output is an error.
    return NULL;
  }
  return buf;
}

int num_days_till_next_wday(int rel_wday, int want_wday)
{
  return ((want_wday > rel_wday) ? 
	  (want_wday - rel_wday) : 
	  (want_wday - rel_wday + 7));
}
