#ifndef __kr_diskspace_h__
#define __kr_diskspace_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__EPOC32__)

  gboolean check_logging_medium_ready(GError** error);

#define LOGGING_MEDIUM_CHECK_SUPPORTED 1

#else /* not EPOC */

#define LOGGING_MEDIUM_CHECK_SUPPORTED 0

#endif /* defined(__EPOC32__) */

#ifdef __cplusplus
} /* extern "C" */
#endif

#ifdef __cplusplus
#if defined(__EPOC32__)
#include <e32std.h>
#include <f32file.h>
void CheckLoggingMediumReadyL(RFs& fs);
#endif
#endif

#endif /* __kr_diskspace_h__ */
