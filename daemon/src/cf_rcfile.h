#ifndef __cf_rcfile_h__
#define __cf_rcfile_h__

#include "application_config.h"

#if __FEATURE_RCFILE__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _cf_RcFile cf_RcFile;

  /** Reads in the configuration from a static configuration file.
      A parse error or parameter value validity
      checking failure will cause an error return.
      Unrecognized settings shall be ignored.
      No configuration file shall cause the default static
      configuration information to be returned.

      As this object only contains fixed data (after initialization)
      accessing it is by nature thread safe.
  */
  cf_RcFile* cf_RcFile_new(GError** error);
  
  /** Frees any resources allocated for caching the static
      configuration information. */
  void cf_RcFile_destroy(cf_RcFile* self);

  // The configuration parameter value getters must be consistently
  // named, as we have macros assuming so. And they are, as we are
  // generating them with consistent naming.

#include "cf_rcfile_list.h"

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __FEATURE_RCFILE__ */

#endif /* __cf_rcfile_h__ */
