#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "time_utils.h"
#include "moment_parser.h"

static GQuark this_quark()
{
  return g_quark_from_static_string("moment_parser");
}

#define this_domain (this_quark())

enum {
  code_parse_error = 1,
  code_incomplete_input
};

#define SET_SEEN_DATETIME_FROM_TIME_T(tt_expr) \
  {					       \
    time_t tt = (tt_expr);		       \
    gmtime_r(&tt, &seen);		       \
  }

%%{
  
  machine fsm;

  ## Common.

  include time_common "time_common.rl";

  action UpdateFuture {
    time_t seen_tt = SEEN_TIME_T;
    if (seen_tt >= now && (!future || (seen_tt < future)))
      future = seen_tt;
  }

  action EveryHourSeen {
    // Now "ctx" does not matter. Just compute next exact hour relative to "now".
    // Compute based on now_tm.tm_min and now_tm.tm_sec.
    time_t nowHourStart = now - now_tm.tm_sec - 60 * now_tm.tm_min;
    time_t nowHourMin = nowHourStart + seenMin * 60;
    if (now > nowHourMin)
      nowHourMin += (60 * 60);
    SET_SEEN_DATETIME_FROM_TIME_T(nowHourMin);
  }

  action EveryDaySeen {
    DEF_NOW_DAY_WITH_SEEN_TIME(wtm);
    time_t wtt = timegm(&wtm);
    if (wtt < now)
      wtt += 24 * 60 * 60;
    SET_SEEN_DATETIME_FROM_TIME_T(wtt);
  }

  action EveryDowSeen {
    // Want next seenWday relative to "now"; it may be the "now" day,
    // but only if the "now" time is no more than (seenHour, seenMin,
    // seenSec).
    DEF_NOW_DAY_WITH_SEEN_TIME(wtm);
    time_t wtt = timegm(&wtm);
    if (now_tm.tm_wday != seenWday || wtt < now) {
      int plusDays = num_days_till_next_wday(now_tm.tm_wday, seenWday);
      wtt += plusDays * 24 * 60 * 60;
    }
    SET_SEEN_DATETIME_FROM_TIME_T(wtt);
  }

  repeated_moment = 
    ((digit{1,2} >Mark %MinSeen space+ "minutes" space+ "past" space+)? 
     "every" space+ "hour") >ClearMinSec %EveryHourSeen |
    ("every" space+ "day" space+ "at" space+ just_time) %EveryDaySeen |
    ("every" space+ day_of_week space+ "at" space+ just_time) %EveryDowSeen ;

  plain_moment_elem = 
    (datetime | repeated_moment) %UpdateFuture ;

  ## Moments.

  moment_elem = 
    "(" space* plain_moment_elem space* ")" |
    plain_moment_elem ;

  moment_list = moment_elem ( space+ "and" space+ moment_elem)* ;

  ## Never.

  never = "never" ;

  ## Time expression.

  time_expr = space* (never | moment_list) space* 0;

main := time_expr;

}%%

%%write data;

// Resolves the next moment defined by expression "s" that is "now" or
// greater; relative expressions are interpreted relative to "ctx".
// s:: Must be a zero terminated string.
// result:: 0 time is used to indicate never. This makes sense since
//          it is the most likely moment to already be in the past.
gboolean parse_moment(const char* s, time_t ctx, time_t now,
		      time_t* result, GError** error)
{
  int cs = 0;
  const char* p = s;
  const char* pe = s + strlen(s) + 1;

  struct tm ctx_tm;
  gmtime_r(&ctx, &ctx_tm);

  struct tm now_tm;
  gmtime_r(&now, &now_tm);

  // The moment closest after "now" found so far, or 0.
  time_t future = 0;

  const char* mark;
  struct tm seen;
  int seenWday;

  %%write init;
  %%write exec;

  if (cs == fsm_error) {
    if (error)
      *error = g_error_new(this_domain, code_parse_error, "parse error in moment expression '%s'", s);
    return FALSE;
  } else if (cs == fsm_first_final) {
    *result = future;
    return TRUE;
  } else {
    if (error)
      *error = g_error_new(this_domain, code_incomplete_input, "incomplete moment expression '%s'", s);
    return FALSE;
  }
}
