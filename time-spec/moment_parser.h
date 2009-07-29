#ifndef __moment_parser_h__
#define __moment_parser_h__

#include <glib.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

gboolean parse_moment(const char* s, time_t ctx, time_t now,
		      time_t* result, GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __moment_parser_h__ */
