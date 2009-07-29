#ifndef __errors_symbian_h__
#define __errors_symbian_h__

#include "common/utilities.h"
#include <glib.h>

#ifdef __EPOC32__
EXTERN_C const char* symbian_error_strerror(int err);

#define plat_error_strerror(err) symbian_error_strerror(err)
#define NO_MEMORY_ERROR (-4) // KErrNoMem
#else
#define plat_error_strerror(err) strerror(err)
#define NO_MEMORY_ERROR ENOMEM
#endif

EXTERN_C gboolean code_means_no_error(int errCode, const char* desc, GError** error);

#endif /* __errors_symbian_h__ */
