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
  */
  cf_RcFile* cf_RcFile_new(GError** error);
  
  /** Frees any resources allocated for caching the static
      configuration information. */
  void cf_RcFile_destroy(cf_RcFile* self);

  gchar* cf_RcFile_get_username(cf_RcFile* self);

  gchar* cf_RcFile_get_upload_url(cf_RcFile* self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __FEATURE_RCFILE__ */

#endif /* __cf_rcfile_h__ */
