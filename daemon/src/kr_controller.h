#ifndef __kr_controller_h__
#define __kr_controller_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _kr_Controller kr_Controller;
  
  kr_Controller* kr_Controller_new(GError** error);
  
  void kr_Controller_destroy(kr_Controller* self);
  
  gboolean kr_Controller_start(kr_Controller* self,
			       GError** error);
  
  void kr_Controller_stop(kr_Controller* self);
  
  gboolean kr_Controller_run(kr_Controller* self,
			     GError** error);

  gboolean kr_Controller_reconfigure(kr_Controller* self,
				     const gchar* name,
				     const gchar* value,
				     GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __kr_controller_h__ */
