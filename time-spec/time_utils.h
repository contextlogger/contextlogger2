#ifndef __time_utils_h__
#define __time_utils_h__

#include <ctype.h>
#include <time.h>

#define seenYear seen.tm_year
#define seenMonth seen.tm_mon
#define seenDay seen.tm_mday
#define seenHour seen.tm_hour
#define seenMin seen.tm_min
#define seenSec seen.tm_sec

#define SET_TODAY_TO_TM(tm_name)		    \
  (tm_name).tm_year = now_tm.tm_year;		    \
  (tm_name).tm_mon = now_tm.tm_mon;		    \
  (tm_name).tm_mday = now_tm.tm_mday;

#define TODAY_SEEN_TIME_TO_TM(tm_name)		    \
  SET_TODAY_TO_TM(tm_name);			    \
  (tm_name).tm_hour = seenHour;			    \
  (tm_name).tm_min = seenMin;			    \
  (tm_name).tm_sec = seenSec;

#define DEF_NOW_DAY_WITH_SEEN_TIME(tm_name)	    \
  struct tm tm_name;				    \
  TODAY_SEEN_TIME_TO_TM(tm_name)

int int_range(int value, int minval, int maxval);

#define ATOI_RANGE(s,min,max) int_range(atoi(s),min,max)

#define SET_SEEN_DATE_PLUS_DAYS(rel, plus_days) \
  { \
  time_t plus_t = (rel) + (plus_days) * 24 * 60 * 60; \
  struct tm plus_tm; \
  gmtime_r(&plus_t, &plus_tm); \
  seenYear = plus_tm.tm_year; \
  seenMonth = plus_tm.tm_mon; \
  seenDay = plus_tm.tm_mday; \
  }

int num_days_till_next_wday(int rel_wday, int want_wday);

// Note that tm_mon range is 0 to 11.
// Note that tm_year is the number of years since 1900.
// "mktime" expects local time, but we have UTC in the struct.
// "timegm" is a UTC equivalent of "mktime", but is a GNU extension.
// SEEN_TIME_T evaluates to -1 in case of error.
#define SEEN_TIME_T timegm(&seen)

char* time_t_to_string(char* buf, int buflen, time_t t);

#define TIME_T_GET_STR(s, t) \
  char s[200]; \
  if (!time_t_to_string(s, 200, t)) return;

#endif /* __time_utils_h__ */
