#ifndef __up_private_h__
#define __up_private_h__

#include "up_uploader.h"

#if __FEATURE_UPLOADER__

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __EPOC32__
#define LOG_UPLOADS_DIR "uploads"
#else
#define LOG_UPLOADS_DIR "e:\\data\\cl2\\uploads"
#endif

gboolean getNextOldLogFile(gchar** pathname,
			   GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif // __FEATURE_UPLOADER__

#endif /* __up_private_h__ */
