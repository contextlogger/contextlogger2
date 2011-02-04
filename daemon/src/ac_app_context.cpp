#include "ac_app_context_private.h"

#include "kr_controller_private.h"
#include "utils_cl2.h"

#include <stdlib.h> // free

// --------------------------------------------------
// Symbian private implementation
// --------------------------------------------------

// We include here so that we can directly refer to anything and
// everything in here.
#ifdef __cplusplus
#if defined(__SYMBIAN32__)
#include "ac_app_context_epoc.cpp"
#endif /* __SYMBIAN32__ */
#endif

// --------------------------------------------------
// common private implementation
// --------------------------------------------------

struct _ac_AppContext
{
  kr_Controller* kr; // not owned
  DEFINE_FOR_SYMBIAN_CXX(CAppContext* plat); // owned
  const char* logdb_dir; // not owned
  char* logdb_file; // owned
  char* log_uploads_dir; // owned
  ac_Registry registry; // initially zeroed
  bb_Blackboard* blackboard; // owned
};

EXTERN_C ac_AppContext* ac_AppContext_new(GError** error)
{
  ac_AppContext* self = g_try_new0(ac_AppContext, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  self->blackboard = bb_Blackboard_new(error);
  if (G_UNLIKELY(!self->blackboard)) {
    ac_AppContext_destroy(self);
    return NULL;
  }

  return self;
}

// The result is "static", and not to be freed.
static const gchar* get_config_database_dir(ac_AppContext* self)
{
  const gchar* database_dir = cf_RcFile_get_database_dir(ac_RcFile(self));
  return database_dir;
}

EXTERN_C void ac_AppContext_set_controller(ac_AppContext* self, 
					   kr_Controller* kr)
{
  self->kr = kr;
}

//// Clear previous cached configuration.
static void free_config_cache(ac_AppContext* self)
{
  self->logdb_dir = NULL;
  FREE_Z(self->logdb_file, free);
  FREE_Z(self->log_uploads_dir, free);
}

EXTERN_C gboolean ac_AppContext_configure(ac_AppContext* self, 
					  GError** error)
{
  free_config_cache(self);

  const gchar* database_dir = get_config_database_dir(self);

  self->logdb_dir = database_dir;
  logg("log db stored in directory '%s'", self->logdb_dir);

  {
    TRAP_OOM(goto fail,
	     self->logdb_file = g_strdup_printf("%s%s%s",
						self->logdb_dir,
						DIR_SEP, 
						LOGDB_BASENAME)
	     );
  }

  {
    TRAP_OOM(goto fail,
	     self->log_uploads_dir = g_strdup_printf("%s%suploads",
						     database_dir,
						     DIR_SEP)
	     );
  }

  logg("uploads stored in directory '%s'", self->log_uploads_dir);
  return TRUE;

#if HAVE_TRAP_OOM
 fail:
  if (error) *error = gx_error_no_memory;
  return FALSE;
#else
  (void)error;
#endif
}

EXTERN_C void ac_AppContext_destroy(ac_AppContext* self)
{
  if (self) {
    WHEN_SYMBIAN(delete self->plat);
    free_config_cache(self);
    bb_Blackboard_destroy(self->blackboard);
    g_free(self);
  }
}

// --------------------------------------------------
// global instance
// --------------------------------------------------

static ac_AppContext* iGlobal = NULL; // not owned

EXTERN_C void ac_set_global_AppContext(ac_AppContext* ac)
{
  iGlobal = ac;
}

// --------------------------------------------------
// common public API implementation
// --------------------------------------------------

EXTERN_C ac_AppContext* ac_get_global_AppContext()
{
  return iGlobal;
}

EXTERN_C kr_Controller* ac_Controller(ac_AppContext* self)
{
  if (!self) return NULL;
  return self->kr;
}

EXTERN_C LogDb* ac_LogDb(ac_AppContext* self)
{
  if (!self || !(self->kr)) return NULL;
  return self->kr->log;
}

EXTERN_C cf_RcFile* ac_RcFile(ac_AppContext* self)
{
  if (!self || !(self->kr)) return NULL;
  return self->kr->rcFile;
}

EXTERN_C ConfigDb* ac_ConfigDb(ac_AppContext* self)
{
  if (!self || !(self->kr)) return NULL;
  return self->kr->configDb;
}

EXTERN_C ac_Registry* ac_get_Registry(ac_AppContext* self)
{
  return &self->registry;
}

EXTERN_C bb_Blackboard* ac_get_Blackboard(ac_AppContext* self)
{
  if (!self) return NULL;
  return self->blackboard;
}

EXTERN_C up_Uploader* ac_get_Uploader(ac_AppContext* self)
{
#if __FEATURE_UPLOADER__
  if (!self || !(self->kr)) return NULL;
  return self->kr->uploader;
#else
  return NULL;
#endif
}

EXTERN_C rk_Remokon* ac_get_Remokon(ac_AppContext* self)
{
#if __FEATURE_REMOKON__
  if (!self || !(self->kr)) return NULL;
  return self->kr->remokon;
#else
  return NULL;
#endif
}

EXTERN_C const char* ac_get_logdb_file(ac_AppContext* self)
{
  return self->logdb_file;
}

EXTERN_C const char* ac_get_logdb_dir(ac_AppContext* self)
{
  return self->logdb_dir;
}

EXTERN_C const char* ac_get_log_uploads_dir(ac_AppContext* self)
{
  return self->log_uploads_dir;
}

// --------------------------------------------------
// Symbian public API implementation
// --------------------------------------------------

#if defined(__SYMBIAN32__)
#if defined(__cplusplus)

void ac_AppContext_PlatInitAsyncL(ac_AppContext* self,
				  MAppContextInitObserver& obs)
{
  self->plat = CAppContext::NewL(self, obs);
}

CAppContext& ac_AppContext_plat(ac_AppContext* self)
{
  assert(self && self->plat);
  return *(self->plat);
}

RFs& ac_Fs(ac_AppContext* self)
{
  assert(self && self->plat);
  return self->plat->iImpl->iFs;
}

CTelephony& ac_Telephony(ac_AppContext* self)
{
  assert(self && self->plat);
  return *(self->plat->iImpl->iTelephony);
}

#if __NEED_CONTACT_DATABASE__
CContactDatabase& ac_ContactDatabase(ac_AppContext* self)
{
  assert(self && self->plat);
  return *(self->plat->iImpl->iContactDatabase);
}
#endif

const TPlatformVersion& ac_get_PlatformVersion(ac_AppContext* self)
{
  assert(self && self->plat);
  return self->plat->iImpl->iPlatformVersion;
}

#endif /* defined(__cplusplus) */
#endif /* __SYMBIAN32__ */

/**

ac_app_context.cpp

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

 **/
