#ifndef __application_config_h__
#define __application_config_h__

  /* Do any required configuration manually. Just enough for the header
     files. The application itself must actually export the symbols that
     require an implementation. */
#if defined(NDEBUG)
#define __DO_LOGGING__ 1
#else
#define __DO_LOGGING__ 0
#endif

  /* This header may be loaded, however. */
#include "common/platform_config.h"

#endif /* __application_config_h__ */

