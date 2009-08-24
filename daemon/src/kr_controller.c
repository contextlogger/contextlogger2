#include "kr_controller_private.h"

#include "application_config.h"
#include "utils_cl2.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_config.h"
#include "common/utilities.h"

// --------------------------------------------------
// global instance
// --------------------------------------------------

static kr_Controller* globalClient = NULL;

kr_Controller* getGlobalClient() {
  return globalClient;
}

// --------------------------------------------------
// internal
// --------------------------------------------------

static gboolean start_uploader(kr_Controller* self, GError** error)
{
#if __FEATURE_UPLOADER__
  assert(!self->uploader);
  self->uploader = up_Uploader_new(self->log, error);
  if (!self->uploader) {
    assert_error_set(error);
    return FALSE;
  }
#endif
  return TRUE;
}

static void stop_uploader(kr_Controller* self)
{
#if __FEATURE_UPLOADER__
  up_Uploader_destroy(self->uploader); // safe if NULL
  self->uploader = NULL;
  logt("uploader destroyed");
#endif
}

// --------------------------------------------------
// exported interface
// --------------------------------------------------

kr_Controller* kr_Controller_new(GError** error)
{
  assert_error_unset(error);

  kr_Controller* self = g_try_new0(kr_Controller, 1);
  if (!self) {
    if (error) *error = NULL;
    return NULL;
  }
  globalClient = self;

  UNLESS_SYMBIAN(event_init(&(self->eventQueue)));

  self->rcFile = cf_RcFile_new(error);
  if (!(self->rcFile)) {
    kr_Controller_destroy(self);
    return NULL;
  }

  self->configDb = ConfigDb_new(error);
  if (!(self->configDb)) {
    kr_Controller_destroy(self);
    return NULL;
  }
  
  LogDb* log = log_db_new(error);
  if (!log) {
    kr_Controller_destroy(self);
    return NULL;
  }
  self->log = log;
  
  if (!start_uploader(self, error)) {
    kr_Controller_destroy(self);
    return NULL;
  }
  
  sa_Array* scanner = sa_Array_new(&(self->eventQueue), log, error);
  if (!scanner) {
    kr_Controller_destroy(self);
    return NULL;
  }
  self->scanner = scanner;
  
  LocalServer* localServer = LocalServer_new(error);
  if (!localServer) {
    kr_Controller_destroy(self);
    return NULL;
  }
  self->localServer = localServer;
  
  return self;
}

void kr_Controller_destroy(kr_Controller* self)
{
  if (self) {
    // This stops all event delivery, so the cleanup of the other
    // subcomponents must be synchronous, not depending on event
    // delivery.
    UNLESS_SYMBIAN(event_close(&(self->eventQueue)));

    LocalServer_destroy(self->localServer); // safe if NULL
    self->localServer = NULL;
    logt("local server destroyed");

    stop_uploader(self);

    // We sometimes get USER 42 here. Say the cellid sensor is
    // enough to make this happen, but uploader may also be
    // required.
    logt("invoking sa_Array_destroy");
    sa_Array_destroy(self->scanner);
    self->scanner = NULL;
    logt("scanner array destroyed");
    
    XDECREF(self->log);
    logt("LogDb session destroyed");

    ConfigDb_destroy(self->configDb);
    self->configDb = NULL;

    cf_RcFile_destroy(self->rcFile);
    self->rcFile = NULL;
    
    g_free(self);
    logt("logger controller destroyed");
    
    globalClient = NULL;
  }
} 

// Starts the client "active object". This starts the activities
// (such as sensor scanning) of this component, but this does not
// start event handling.
gboolean kr_Controller_start(kr_Controller* self, GError** error)
{
  sa_Array_start(self->scanner);
  if (!LocalServer_start(self->localServer, error))
    return FALSE;
  return TRUE;
}

// Stops the client "active object". Stops all the activities of
// this component, but does not affect event handling, which is
// handled separately.
void kr_Controller_stop(kr_Controller* self)
{
  LocalServer_stop(self->localServer);
  sa_Array_stop(self->scanner);
}

// A convenience method for running the event loop until an
// interrupt event is delivered.
gboolean kr_Controller_run(kr_Controller* self, GError** error)
{
#if defined(__SYMBIAN32__)
  { assert(0); return TRUE; }
#else
  return event_loop(&(self->eventQueue), error);
#endif /* __SYMBIAN32__ */
}

#define NAME_STARTS_WITH(lit) \
  (pfx = lit, strncmp(pfx, name, strlen(pfx)) == 0)

#define NAME_EQUALS(lit) \
  (strcmp(lit, name) == 0)

gboolean kr_Controller_reconfigure(kr_Controller* self,
				   const gchar* name,
				   const gchar* value,
				   GError** error)
{
  const char* pfx;
  if (NAME_STARTS_WITH("sensor.") ||
      NAME_STARTS_WITH("array.")) {
    sa_Array* obj = self->scanner;
    if (!sa_Array_reconfigure(obj, name, value, error))
      return FALSE;
  } 
#if __FEATURE_UPLOADER__
  else if (NAME_STARTS_WITH("uploader.") ||
	   NAME_EQUALS("iap")) {
    up_Uploader* obj = self->uploader;
    if (!up_Uploader_reconfigure(obj, name, value, error))
      return FALSE;
  }
#endif
  return TRUE;
}
