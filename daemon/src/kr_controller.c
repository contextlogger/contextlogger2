#include "kr_controller_private.h"

#include "cf_query.h"
#include "epoc-iap.h"
#include "kr_diskspace.h"
#include "utils_cl2.h"

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_config.h"
#include "common/utilities.h"

#if __WITH_LIBEV__
#include <ev.h>
#endif

// --------------------------------------------------
// internal
// --------------------------------------------------

#if __FEATURE_UPLOADER__
static gboolean start_uploader(kr_Controller* self, GError** error)
{
  assert(!self->uploader);
  self->uploader = up_Uploader_new(self->appContext, error);
  if (!self->uploader) {
    return FALSE;
  }
  return TRUE;
}
#endif

#if __FEATURE_UPLOADER__
static void stop_uploader(kr_Controller* self)
{
  up_Uploader_destroy(self->uploader); // safe if NULL
  self->uploader = NULL;
  //logt("uploader destroyed");
}
#endif

// --------------------------------------------------
// uploads allowed flag
// --------------------------------------------------

static gboolean current_iap_is_cellular()
{
#if defined(__SYMBIAN32__) && __CAN_GET_NETWORK_INFO__
  int value = get_config_iap_id();
  if (value < 0) {
    // No IAP configured. User can manually choose IAP or not.
    return FALSE;
  }
  guint32 iapId = (guint32)value;
  gboolean found = FALSE;
  gboolean yes = FALSE;
  GError* error = NULL;
  LogDb* logDb = ac_global_LogDb;
  if (!epoc_iap_is_modem(iapId, &found, &yes, &error)) {
    log_db_log_status(logDb, NULL, "could not look up bearer for IAP ID %u", iapId);
    gx_dblog_error_free(logDb, error);
    return TRUE; // play it safe
  }
  if (!found) {
    log_db_log_status(logDb, NULL, "WARNING: no IAP ID %u", iapId);
    return TRUE; // play it safe
  }
  logg("iap %u is modem bearer: %d", iapId, yes);
  return yes;
#else
  return FALSE;
#endif /* __SYMBIAN32__ */
}

static void log_uploads_allowed(kr_Controller* self)
{
  //logg("uploads allowed: %d", self->are_uploads_allowed);
  LogDb* logDb = ac_global_LogDb;
  // http://en.wikipedia.org/wiki/Signal_strength says
  // "decibels above a reference level of one milliwatt (dBm)"
  log_db_log_status(logDb, NULL, "CHANGE: uploads allowed = %s (modem IAP %s, MCC %d at %d dBm)", boolstr_yes(self->are_uploads_allowed), boolstr_yes(self->is_cellular_ap), self->current_mcc, self->current_signal_strength);
}

static void uploads_allowed_update_board(gboolean value)
{
  bb_Blackboard* bb = ac_global_Blackboard;
  assert(bb);
  bb_Board* bd = bb_Blackboard_board(bb);
  bd->uploads_allowed = value;
  bb_Blackboard_notify(bb, bb_dt_uploads_allowed,
		       (gpointer)&(bd->uploads_allowed), 0);
}

static void uploads_allowed_changed(kr_Controller* self)
{
  log_uploads_allowed(self);
  uploads_allowed_update_board(self->are_uploads_allowed);
}

static void netpos_allowed_update_board(gboolean value)
{
  log_db_log_status(ac_global_LogDb, NULL, 
		    "CHANGE: network positioning allowed = %s", 
		    boolstr_yes(value));
  bb_Blackboard* bb = ac_global_Blackboard;
  bb_Board* bd = bb_Blackboard_board(bb);
  bd->netpos_allowed = value; // mirrored here
  bb_Blackboard_notify(ac_global_Blackboard, bb_dt_netpos_allowed,
		       GINT_TO_POINTER(value), 0);
}

static void recompute_netpos_allowed(kr_Controller* self);

static void init_uploads_allowed_state(kr_Controller* self)
{
  //WHEN_SYMBIAN(epoc_log_bearer_types());

  self->is_cellular_ap = current_iap_is_cellular();
  self->non_roaming_mcc = cf_RcFile_get_mcc(self->rcFile);
  self->non_roaming_operator_name = cf_RcFile_get_operator_name(self->rcFile);
  self->current_signal_strength = 1; // no reading yet
  self->current_mcc = -1; // no reading yet
  GMaybeString_init(&self->current_operator_name);

  // Initial value, until more information about any mobile network
  // becomes available.
  self->are_uploads_allowed = !self->is_cellular_ap;

  logg("non-roaming MCC: %d", self->non_roaming_mcc);
  WHEN_LOGGING({if (self->non_roaming_operator_name) logg("non-roaming operator: '%s'", self->non_roaming_operator_name);});
  uploads_allowed_changed(self);

  self->is_netpos_allowed = FALSE;
  recompute_netpos_allowed(self);
}

static void free_uploads_allowed_state(kr_Controller* self)
{
  GMaybeString_free(&self->current_operator_name);
}

static void recompute_netpos_allowed(kr_Controller* self)
{
  gboolean oldval = self->is_netpos_allowed;
  gboolean newval = TRUE;

  if (self->current_mcc == -1) {
      // no network
      newval = FALSE;
  }
  else
    if (self->non_roaming_mcc != -1) {
      // have a roaming restriction
      if (self->current_mcc != self->non_roaming_mcc)
	// is roaming
	newval = FALSE;
    }
    else
      if (self->non_roaming_operator_name) {
	// have a roaming restriction
	if (GMaybeString_is_nothing(&self->current_operator_name) ||
	    !GMaybeString_is(&self->current_operator_name,
			     self->non_roaming_operator_name))
	  // is roaming
	  newval = FALSE;
      }

  if (oldval != newval) {
    self->is_netpos_allowed = newval;
    netpos_allowed_update_board(newval);
  }
}

static void recompute_uploads_allowed(kr_Controller* self)
{
  gboolean old_flag = self->are_uploads_allowed;
  //logg("recomputing uploads allowed (now %d)", old_flag);
  self->are_uploads_allowed = TRUE;
  if (self->is_cellular_ap) {
    if ((self->current_signal_strength == 1) ||
	(self->current_mcc == -1)) {
      // no network
      self->are_uploads_allowed = FALSE;
    } else {
      if (self->non_roaming_mcc != -1) {
	// have a roaming restriction
	if (self->current_mcc != self->non_roaming_mcc)
	  // is roaming
	  self->are_uploads_allowed = FALSE;
      }

      if (self->non_roaming_operator_name) {
	// have a roaming restriction
	if (GMaybeString_is_nothing(&self->current_operator_name) ||
	    !GMaybeString_is(&self->current_operator_name,
			     self->non_roaming_operator_name))
	  // is roaming
	  self->are_uploads_allowed = FALSE;
      }

      // strength is from -123 dBm to -1 dBm (inclusive),
      // but apparently can also be 0 in flight mode
      if ((self->current_signal_strength < -110) ||
	  (self->current_signal_strength == 0))
	// poor signal
	self->are_uploads_allowed = FALSE;
    }
  }
  if (old_flag != self->are_uploads_allowed) {
    uploads_allowed_changed(self);
  }
}

// Called after "iap" configuration changes.
static void iap_config_changed(kr_Controller* self)
{
  gboolean nval = current_iap_is_cellular();
  if (nval != self->is_cellular_ap) {
    self->is_cellular_ap = nval;
    recompute_uploads_allowed(self);
  }
}

// pass +1 for no network
void kr_Controller_set_signal_strength(kr_Controller* self, int strength)
{
  //logg("setting strength to %d", strength);
  if (strength != self->current_signal_strength) {
    self->current_signal_strength = strength;
    recompute_uploads_allowed(self);
  }
}

// pass -1 for no network
void kr_Controller_set_current_mcc(kr_Controller* self, int mcc)
{
  logg("setting mcc to %d", mcc);
  if (mcc != self->current_mcc) {
    self->current_mcc = mcc;
    recompute_uploads_allowed(self);
    recompute_netpos_allowed(self);
  }
}

// pass NULL for no network or no name
void kr_Controller_set_operator_name(kr_Controller* self, const char* name)
{
  // As an optimization, we do not care unless the operator name
  // affects our roaming analysis.
  if (self->non_roaming_operator_name) {
    if (!GMaybeString_is(&self->current_operator_name, name)) {
      logg("setting operator to %s", name ? name : "<none>");
      er_log_oom_on_false(GMaybeString_assign(&self->current_operator_name, name, NULL));
      recompute_uploads_allowed(self);
      recompute_netpos_allowed(self);
    }
  }
}

// --------------------------------------------------
// exported interface
// --------------------------------------------------

kr_Controller* kr_Controller_new(GError** error)
{
  assert_error_unset(error);

  //er_log_none(er_FATAL, "terrible error (%d)", 555);

  kr_Controller* self = g_try_new0(kr_Controller, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  self->appContext = ac_get_global_AppContext();
  // A controller reference kept here whenever it exists. Querying
  // AppContext at any time will tell you which objects exist.
  ac_AppContext_set_controller(self->appContext, self);

#if __WITH_LIBEV__
  // Creates the default event loop, unless already created.
  struct ev_loop* loop = ev_default_loop(0);
  if (!loop) {
    if (error) *error = gx_error_no_memory;
    kr_Controller_destroy(self);
    return NULL;
  }
#endif

  self->rcFile = cf_RcFile_new(error);
  if (!(self->rcFile)) {
    kr_Controller_destroy(self);
    return NULL;
  }

  if (!ac_AppContext_configure(self->appContext, error)) {
    kr_Controller_destroy(self);
    return NULL;
  }

#if LOGGING_MEDIUM_CHECK_SUPPORTED
  // Must be done after config file reading, as the threshold is
  // configurable. We do want to do this as early as possible, though,
  // to avoid doing a lot of work when the watchdog repeatedly tries
  // to launch us.
  if (!check_logging_medium_ready(error)) {
    kr_Controller_destroy(self);
    return NULL;
  }
  logt("logging medium ready");
#endif

  self->configDb = ConfigDb_new(error);
  if (!(self->configDb)) {
    kr_Controller_destroy(self);
    return NULL;
  }
  
  LogDb* log = LogDb_new(error);
  if (G_UNLIKELY(!log)) {
    kr_Controller_destroy(self);
    return NULL;
  }
  self->log = log;

  init_uploads_allowed_state(self);

#if __FEATURE_UPLOADER__
  // This is a bit different in that Uploader is not affected by
  // controller start/stop. Even though Uploader typically does not
  // consume much resources (i.e., connections are only made when
  // there is something to upload), this is still somewhat
  // questionable. Consider uploader/start stop in the start stop
  // methods, but remember that then self->uploader will be NULL at
  // times.
  if (!start_uploader(self, error)) {
    kr_Controller_destroy(self);
    return NULL;
  }
#endif
  
  sa_Array* scanner = sa_Array_new(self->appContext, error);
  if (!scanner) {
    kr_Controller_destroy(self);
    return NULL;
  }
  self->scanner = scanner;
  
#if __FEATURE_LOCALSERVER__
  LocalServer* localServer = LocalServer_new(error);
  if (!localServer) {
    kr_Controller_destroy(self);
    return NULL;
  }
  self->localServer = localServer;
#endif

#if __FEATURE_REMOKON__
  self->remokon = rk_Remokon_new(error);
  if (!self->remokon) {
    kr_Controller_destroy(self);
    return NULL;
  }
#endif
  
#if HAVE_PLAT_AO
  self->platAo = kr_PlatAo_new(error);
  if (!self->platAo) {
    kr_Controller_destroy(self);
    return NULL;
  }
#endif

  return self;

#if 0
 fail:
  kr_Controller_destroy(self);
  if (error) *error = gx_error_no_memory;
  return NULL;
#endif
}

void kr_Controller_destroy(kr_Controller* self)
{
  if (self) {
    // Note that we are not destroying any event loop here, with
    // ev_default_destroy(), so any components signed up with it are
    // responsible for deregistering.

#if HAVE_PLAT_AO
    kr_PlatAo_destroy(self->platAo);
#endif

#if __FEATURE_REMOKON__
    FREE_Z(self->remokon, rk_Remokon_destroy);
#endif

#if __FEATURE_LOCALSERVER__
    LocalServer_destroy(self->localServer); // safe if NULL
    self->localServer = NULL;
#endif

#if __FEATURE_UPLOADER__
    stop_uploader(self);
#endif

    // We sometimes get USER 42 here. Say the cellid sensor is
    // enough to make this happen, but uploader may also be
    // required.
    sa_Array_destroy(self->scanner);
    self->scanner = NULL;
    //logt("scanner array destroyed");
    
    LogDb_destroy(self->log);
    self->log = NULL;
    //logt("LogDb session destroyed");

    ConfigDb_destroy(self->configDb);
    self->configDb = NULL;

    cf_RcFile_destroy(self->rcFile);
    self->rcFile = NULL;

    free_uploads_allowed_state(self);

    ac_AppContext_set_controller(self->appContext, NULL);
    
    g_free(self);
    //logt("logger controller destroyed");
  }
} 

// Starts the client "active object". This starts the activities
// (such as sensor scanning) of this component, but this does not
// start event handling.
gboolean kr_Controller_start(kr_Controller* self, GError** error)
{
  (void)error;
  sa_Array_start(self->scanner);
#if __FEATURE_LOCALSERVER__
  if (!LocalServer_start(self->localServer, error)) {
    return FALSE;
  }
#endif
#if __FEATURE_REMOKON__
  if (rk_Remokon_is_autostart_enabled(self->remokon)) {
    logt("controller: auto-starting remokon");
    if (!rk_Remokon_start(self->remokon, error)) {
      return FALSE;
    }
  }
#endif
  return TRUE;
}

// Stops the client "active object". Stops the activities of this
// component (apart from Uploader and any PlatAo), but does not affect
// event handling, which is handled separately.
void kr_Controller_stop(kr_Controller* self)
{
#if __FEATURE_REMOKON__
  rk_Remokon_stop(self->remokon);
#endif
#if __FEATURE_LOCALSERVER__
  LocalServer_stop(self->localServer);
#endif
  sa_Array_stop(self->scanner);
}

// A convenience method for running the event loop until an
// interrupt event is delivered.
gboolean kr_Controller_run(kr_Controller* self, GError** error)
{
  (void)self;
  (void)error;
#if __WITH_LIBEV__
  ev_loop(EV_DEFAULT, 0);
#else
  assert(0 && "explicit running of event loop unsupported");
#endif
  return TRUE;
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

  else if (NAME_EQUALS("iap")) {
#if __FEATURE_UPLOADER__
    if (!up_Uploader_reconfigure(self->uploader, name, value, error))
      return FALSE;
#endif
#if __FEATURE_REMOKON__
    if (!rk_Remokon_reconfigure(self->remokon, name, value, error))
      return FALSE;
#endif
    iap_config_changed(self);
  }

#if __FEATURE_UPLOADER__
  else if (NAME_STARTS_WITH("uploader.")) {
    if (!up_Uploader_reconfigure(self->uploader, name, value, error))
      return FALSE;
  }
#endif

#if __FEATURE_REMOKON__
  else if (NAME_STARTS_WITH("remokon.")) {
    if (!rk_Remokon_reconfigure(self->remokon, name, value, error))
      return FALSE;
  }
#endif

  return TRUE;
}

/**

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
