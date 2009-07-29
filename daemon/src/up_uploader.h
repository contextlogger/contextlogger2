#ifndef __up_uploader_h__
#define __up_uploader_h__

#include "application_config.h"

#if __FEATURE_UPLOADER__

#include <glib.h>
#include "log-db.h"

#ifdef __cplusplus
extern "C" {
#endif

  GQuark up_quark();

#define up_DOMAIN (up_quark())

#define up_ERR_GENERAL 1

  // Does global initialization.
  //
  // Additionally, you will probably want to invoke signal(SIGPIPE,
  // SIG_IGN) as part of the global initialization.
  gboolean up_global_init(GError** error);

  // Does global cleanup.
  //
  void up_global_cleanup();

#if __UPLOAD_WITH_CURL__
  typedef struct _up_Uploader up_Uploader; 
#else
  typedef void* up_Uploader; 
#endif

  up_Uploader* up_Uploader_new(LogDb* logDb, GError** error);

  void up_Uploader_destroy(up_Uploader* object);

  // Overrides any timeout to cause a new LogDb snapshot to be taken
  // immediately.
  gboolean up_Uploader_upload_now(up_Uploader* object,
				  GError** error);

  // Modifies the up_Uploader runtime configuration. Any upload that is
  // in progress is not interrupted.
  gboolean up_Uploader_reconfigure(up_Uploader* object,
				   const char* key,
				   const void* value,
				   GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#else

typedef int up_Uploader; // dummy

#endif // __FEATURE_UPLOADER__

#endif /* __up_uploader_h__ */
