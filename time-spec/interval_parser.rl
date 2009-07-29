#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include "time_utils.h"
#include "interval_parser.h"

static GQuark this_quark()
{
  return g_quark_from_static_string("interval_parser");
}

#define this_domain (this_quark())

enum {
  code_parse_error = 1,
  code_incomplete_input
};

typedef struct {
  time_t b;
  time_t e;
} time_range_t;

typedef struct {
  struct tm b;
  struct tm e;
} tm_range_t;

#define CLEAR_RANGE(range) {(range).b = 0; (range).e = 0;}

static int zero_or_max(int x, int y)
{
  if (!x || !y)
    return 0;
  return MAX(x, y);
}

static gboolean is_never(const time_range_t* r)
{
  return (r->b == 0 && r->e == 0);
}

static void time_range_copy(time_range_t* dest, const time_range_t* src)
{
  memcpy(dest, src, sizeof(time_range_t));
}

static gboolean time_range_overlap(const time_range_t* r1, const time_range_t* r2)
{
  if (!r1->e && !r2->e)
    // Necessarily eventually overlap.
    return TRUE;
  if (!r1->e) {
    return r1->b <= r2->e;
  }
  if (!r2->e) {
    return r2->b <= r1->e;
  }
  if (r1->e < r2->b)
    return FALSE;
  if (r2->e < r1->b)
    return FALSE;
  return TRUE;
}

static void time_range_union(time_range_t* dest, const time_range_t* src)
{
  dest->b = MIN(dest->b, src->b);
  dest->e = zero_or_max(dest->e, src->e);
}

#define CAND_RANGE_TO_TIME_T \
  cand.b = timegm(&cand_tm.b); \
  cand.e = timegm(&cand_tm.e);

#define SEEN_DATE_TO_TM(rec) \
  (rec).tm_year = seenYear; \
  (rec).tm_mon = seenMonth; \
  (rec).tm_mday = seenDay;

#define SEEN_TIME_TO_TM(rec) \
  (rec).tm_hour = seenHour; \
  (rec).tm_min = seenMin; \
  (rec).tm_sec = seenSec;

#define DUR_IN_SECS ((durHour * 60 + durMin) * 60)

%%{
  
  machine fsm;

  ## Common.

  include time_common "time_common.rl";

  ## Intervals.

  action ClearHour { seenHour = 0; }
  action ClearMin { seenMin = 0; }

  action StoreCandBegin { cand.b = SEEN_TIME_T; }
  action StoreCandEnd { cand.e = SEEN_TIME_T; }

  action StoreDur {
    durHour = seenHour;
    durMin = seenMin;
  }

  n_hours = digit{1,2} >Mark space+ "hour" "s"? %HourSeen ;
  n_minutes = digit{1,2} >Mark space+ "minute" "s"? %MinSeen ;
  n_hours_n_minutes = 
    ((n_hours space+ n_minutes) | 
     (n_hours %ClearMin) | 
     (n_minutes %ClearHour)) %StoreDur ;

  action EveryDaySeen {
    SET_TODAY_TO_TM(cand_tm.b); // time already set
    cand.b = timegm(&cand_tm.b);
    cand.e = cand.b + DUR_IN_SECS;
    if (cand.e < now) {
      cand.b += 24 * 60 * 60;
      cand.e += 24 * 60 * 60;
    }
  }

  action EveryDowSeen {
    SET_TODAY_TO_TM(cand_tm.b); // time already set
    cand.b = timegm(&cand_tm.b);
    cand.e = cand.b + DUR_IN_SECS;
    if (now_tm.tm_wday != seenWday || cand.e < now) {
      int plusDays = num_days_till_next_wday(now_tm.tm_wday, seenWday);
      cand.b += plusDays * 24 * 60 * 60;
      cand.e += plusDays * 24 * 60 * 60;
    }
  }

  action StoreCandBeginTime {
    SEEN_TIME_TO_TM(cand_tm.b);
  }

  repeated_interval = 
    ("every" space+ "day" space+ "from" space+ 
     just_time %StoreCandBeginTime space+
     "for" space+ n_hours_n_minutes) %EveryDaySeen |
    ("every" space+ day_of_week space+ "from" space+ 
     just_time %StoreCandBeginTime space+
     "for" space+ n_hours_n_minutes) %EveryDowSeen ;

  action CalcForCand {
    cand.b = SEEN_TIME_T;
    cand.e = cand.b + DUR_IN_SECS;
  }

  action StoreCandDate {
    SEEN_DATE_TO_TM(cand_tm.b);
    SEEN_DATE_TO_TM(cand_tm.e);
  }

  action StoreInDayRange {
    SEEN_TIME_TO_TM(cand_tm.e);
    CAND_RANGE_TO_TIME_T;
  }

  oneshot_interval = 
    datetime %StoreCandBegin space+ "-" space+ datetime %StoreCandEnd |
    just_date %StoreCandDate space+ just_time %StoreCandBeginTime space+ "-" space+ just_time %StoreInDayRange |
    "for" space+ n_hours_n_minutes space+ "after" space+ datetime %CalcForCand ;

  action UpdateFuture {
    if (cand.b && (cand.e > now || !cand.e)) {
      if (is_never(&future)) {
	time_range_copy(&future, &cand);
      } else if (time_range_overlap(&future, &cand)) {
	time_range_union(&future, &cand);
      } else if (cand.b < future.b) {
	time_range_copy(&future, &cand);
      }
    }
  }

  plain_interval_elem = 
    (oneshot_interval | repeated_interval) %UpdateFuture ;

  interval_elem = 
    "(" space* plain_interval_elem space* ")" |
    plain_interval_elem ;

  interval_list = interval_elem ( space+ "and" space+ interval_elem)* ;

  ## Never or always.

  action AlwaysSeen {
    future.b = now;
    future.e = 0;
  }

  always = "always" %AlwaysSeen ;

  never = "never" ;

  ## Time expression.

  time_expr = space* (never | always | interval_list) space* 0;

main := time_expr;

}%%

%%write data;

// Resolves the time interval defined by expression "s" that either
// encompasses "now", or is the first time interval after "now";
// relative expressions are interpreted relative to "ctx". If there
// are multiple overlapping or conjoined intervals, the union of those
// intervals is taken.
// s:: Must be a zero terminated string.
// beg, end:: (0,0) time is used to indicate never. (t,0) is used to
//            indicate always starting from time "t".
gboolean parse_interval(const char* s, time_t ctx, time_t now,
			time_t* beg, time_t* end, GError** error)
{
  int cs = 0;
  const char* p = s;
  const char* pe = s + strlen(s) + 1;

  struct tm ctx_tm;
  gmtime_r(&ctx, &ctx_tm);

  struct tm now_tm;
  gmtime_r(&now, &now_tm);

  // The interval closest after "now" found so far, or (0,0).
  time_range_t future;
  CLEAR_RANGE(future);

  // Candidate for becoming "future" or being joined with it.
  time_range_t cand;

  const char* mark;
  struct tm seen;
  int seenWday, durHour, durMin;
  tm_range_t cand_tm;

  %%write init;
  %%write exec;

  if (cs == fsm_error) {
    if (error)
      *error = g_error_new(this_domain, code_parse_error, "parse error in time interval expression '%s'", s);
    return FALSE;
  } else if (cs == fsm_first_final) {
    *beg = future.b;
    *end = future.e;
    return TRUE;
  } else {
    if (error)
      *error = g_error_new(this_domain, code_incomplete_input, "incomplete time interval expression '%s'", s);
    return FALSE;
  }
}
