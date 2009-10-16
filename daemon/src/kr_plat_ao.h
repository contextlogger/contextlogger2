#ifndef __kr_plat_ao_h__
#define __kr_plat_ao_h__

// Adjust this depending on whether you actually have an
// implementation for your platform.
#if defined(__SYMBIAN32__)
#define HAVE_PLAT_AO 1
#else
#define HAVE_PLAT_AO 0
#endif /* __SYMBIAN32__ */

#if HAVE_PLAT_AO

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  // Defines a platform-specific AO that runs alongside the primary
  // controller. This is just so that we need not come up with a
  // separate wrapper API for every little platform-specific thing
  // that might have to be going on within the controller.

typedef struct _kr_PlatAo kr_PlatAo;

kr_PlatAo* kr_PlatAo_new(GError** error);

void kr_PlatAo_destroy(kr_PlatAo* self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif // HAVE_PLAT_AO

#endif /* __kr_plat_ao_h__ */
