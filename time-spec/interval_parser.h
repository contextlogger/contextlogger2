#ifndef __interval_parser_h__
#define __interval_parser_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

gboolean parse_interval(const char* s, time_t ctx, time_t now,
			time_t* beg, time_t* end, GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __interval_parser_h__ */
