#ifndef __epoc_iap_h__
#define __epoc_iap_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  // iapName must be given in UTF-8.
  gboolean epoc_iap_by_name(const gchar* iapName, 
			    guint32* iapId, 
			    gboolean* found,
			    GError** error); 

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __epoc_iap_h__ */
