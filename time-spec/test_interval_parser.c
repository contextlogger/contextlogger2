#include <stdio.h>
#include <string.h>
#include "time_utils.h"
#include "interval_parser.h"

static void test_parse_ctx(const char* s, time_t ctx)
{
  GError* error = NULL;

  TIME_T_GET_STR(ctx_s, ctx);

  time_t beg, end;
  if (parse_interval(s, ctx, time(NULL), &beg, &end, &error)) {
    if (!beg && !end) {
      printf("no next time interval in expression '%s' (relative to %s)\n", s, ctx_s);
    } else if (!end) {
      TIME_T_GET_STR(end_s, end);
      printf("next interval in expression '%s' (relative to %s) is forever from %s\n", s, ctx_s, end_s);
    } else {
      TIME_T_GET_STR(beg_s, beg);
      TIME_T_GET_STR(end_s, end);
      printf("next interval in expression '%s' (relative to %s) is %s - %s\n", s, ctx_s, beg_s, end_s);
    }
  } else {
    printf("error: %s (%s: %d)\n", 
	   error->message, 
	   g_quark_to_string(error->domain), 
	   error->code);
    g_error_free(error);
  }
}

static void test_parse(const char* s)
{
  test_parse_ctx(s, time(NULL));
}

// no proper error handling, assuming correct input
static time_t ts(const char* s)
{
  struct tm s_tm;
  if (!strptime(s, "%F %T", &s_tm))
    return 0;
  return timegm(&s_tm);
}

#define WEEK_EARLIER (time(NULL) - 7 * 24 * 60 * 60)

int main()
{
  test_parse("never");
  test_parse("always");
  test_parse("on 2008-11-30 at 13:13 - on 2008-11-30 at 13:16");
  test_parse("on 2008-12-30 at 13:13 - on 2008-12-30 at 13:16 and on 2008-11-30 at 13:13 - on 2008-11-30 at 13:16 and on 2008-12-1 at 13:13 - on 2008-12-1 at 13:16");
  test_parse("2008-11-30 13:13 - 2008-11-30 13:16 and 2008-11-30 13:11 - 2008-11-30 13:18");
  test_parse("2008-11-30 13:10 - 2008-11-30 13:16 and 2008-11-30 13:11 - 2008-11-30 13:18");
  test_parse("for 5 minutes after tomorrow at 10");
  test_parse("for 1 hour after tomorrow at 10");
  test_parse("for 2 hours 4 minutes after tomorrow at 10");
  test_parse_ctx("2008-11-30 13:13 - 13:16", WEEK_EARLIER);
  test_parse("every day from 10 for 5 hours");
  test_parse("every day from 4 for 3 hours");
  test_parse("every Friday from 10 for 5 hours");
  test_parse("every day from 14 for 1 hour");
  test_parse("every day from 13 for 1 hour");
  return 0;
}
