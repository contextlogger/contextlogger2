#define _XOPEN_SOURCE
#define _BSD_SOURCE
#include <time.h>

#include <stdio.h>
#include <string.h>
#include "moment_parser.h"
#include "time_utils.h"

static void test_parse_ctx(const char* s, time_t ctx)
{
  GError* error = NULL;

  TIME_T_GET_STR(ctx_s, ctx);

  time_t result;
  if (parse_moment(s, ctx, time(NULL), &result, &error)) {
    if (!result) {
      printf("no next moment in expression '%s' (relative to %s)\n", s, ctx_s);
    } else {
      struct tm result_tm;
      if (gmtime_r(&result, &result_tm)) {
	char result_buf[200];
	if (strftime(result_buf, 200, "%F %T", &result_tm) != 0) {
	  printf("next moment in expression '%s' (relative to %s) is %s\n", s, ctx_s, result_buf);
	}
      }
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

int main()
{
  test_parse("never");
  test_parse("today 23:59");
  test_parse("tomorrow 23:59");
  test_parse("on the day after tomorrow at 23:59");
  test_parse("next Saturday at 23:59");
  test_parse_ctx("next Saturday at 23:59", time(NULL) - 7 * 24 * 60 * 60);
  test_parse("(2009-Jan-30 13:13)");
  test_parse("(2009-January-30 13:13)");
  test_parse("2008-11-30 13 and 2008-11-30 12 and 2008-11-30 14 and 2008-01-01 11");
  test_parse("(2008-11-30 13:13) and on 2008-11-30 at 13:14");
  test_parse("on 2008-11-30 at 13:13");
  test_parse("at 13:13 on 2008-11-30");
  test_parse("at 0:3 on 2008-11-30 and every day at 15:15");
  test_parse("never at 13:12"); // wrong
  test_parse("every Wednesday at 10");
  test_parse("5 minutes past every hour and every Thursday at 3:30");
  test_parse("every hour");
  test_parse_ctx("every hour", ts("2008-08-30 23:59:59"));
  test_parse("every day at 14:17");
  test_parse("6 minutes past every hour");
  test_parse("0 minutes past every hour and 10 minutes past every hour and 20 minutes past every hour and 30 minutes past every hour and 40 minutes past every hour and 50 minutes past every hour");
  return 0;
}
