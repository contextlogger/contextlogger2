#include "rk_remokon_private.h"

#if __FEATURE_REMOKON__

#include "rk_jabber_session.h"

#include "cf_query.h" // get_ConfigDb_int
#include "er_errors.h"
#include "kr_controller_private.h" // cf_STATIC_GET
#include "lua_cl2.h"
#include "ut_timer.h"

#include <stdlib.h>

struct _rk_Remokon {
  // These come mostly from the configuration file. There will be no
  // default values, and hence Remokon will be automatically disabled
  // when no configuration setting is available. We require that
  // "server", "password", "username", and "jid" are all non-NULL. The
  // dynamically settable "iap_id" will be given some unlikely to work
  // default value.
  rk_JabberParams params;

  // This indicates whether we have at least some kind of a value for
  // all the required configuration parameters. Leaving any of these
  // out is a simple way to disable the remote control facility.
  gboolean have_config;

  // This is just a hint for the controller as to whether to start
  // Remokon automatically at startup.
  gboolean autostart_enabled;

  // We should always have a Jabber session object after having been
  // initialized. The object creation itself requires nothing but
  // memory. Getting the connection is harder.
  rk_JabberSession* session;

  // We use this flag to keep track of whether "session" has a
  // connection. It does not itself keep track of that, as it is too
  // low-level for such things. It is us who must decide whether to
  // request a disconnect on a write error, say.
  gboolean is_connected;

  // Number of consecutive failed connection attempts.
  int num_failures;

  // A retry timer.
  ut_Timer* timer;

  // Here we have only one Lua instance per session, making for a
  // proper REPL. We might consider providing a way to reset the VM.
  lua_State* L;
};

static void handleTimerError(rk_Remokon* self, GError* timerError)
{
  logt("timer error in Remokon");
  gx_db_log_free_fatal_error(getGlobalClient()->log, timerError);
}

static void setRetryTimer(rk_Remokon* self)
{
  (self->num_failures)++;

  int secs = 2 * 60 * self->num_failures + (rand() % 60);
  logf("retrying Jabber connection in %d secs", secs);

  GError* localError = NULL;
  if (!ut_Timer_set_after(self->timer, secs, &localError)) {
    handleTimerError(self, localError);
  }
}

static void stopSession(rk_Remokon* self)
{
  if (self->session) rk_JabberSession_stop(self->session);
  self->is_connected = FALSE;
}

static void startSessionOrRetry(rk_Remokon* self)
{
  GError* localError = NULL;
  if (!rk_JabberSession_start(self->session, &localError)) {
    gx_error_log_free(localError);
    setRetryTimer(self);
  }
}

// ut_Timer callback
static void cb_timerExpired(void* userdata, GError* timerError)
{
  rk_Remokon* self = (rk_Remokon*)userdata;
  if (timerError) {
    handleTimerError(self, timerError);
  } else {
    logt("retrying Jabber connection establishment");
    startSessionOrRetry(self);
  }
}

// rk_JabberObserver
static int cb_sessionEstablished(void* userdata)
{
  rk_Remokon* self = (rk_Remokon*)userdata;
  self->num_failures = 0;
  self->is_connected = TRUE;
  logt("Jabber connection established");
  return rk_PROCEED;
}

// rk_JabberObserver
static int cb_gotEof(void* userdata)
{
  rk_Remokon* self = (rk_Remokon*)userdata;
  logt("Jabber server closed connection");
  stopSession(self);
  setRetryTimer(self);
  return rk_HALT;
}

// rk_JabberObserver
static int cb_severeError(void* userdata, const char* msg)
{
  rk_Remokon* self = (rk_Remokon*)userdata;
  logf("Jabber error: %s", msg);
  stopSession(self);
  setRetryTimer(self);
  return rk_HALT;
}

// rk_JabberObserver
static int cb_fatalError(void* userdata, const char* msg)
{
  logf("fatal Jabber error: %s", msg);
  er_fatal_error();
  return rk_HALT;
}

// rk_JabberObserver
static int cb_gotMsg(void* userdata, const char* fromJid, const char* luaStr)
{
  rk_Remokon* self = (rk_Remokon*)userdata;
  lua_State* L = self->L;

  logf("remote message from %s: %s", fromJid, luaStr);

  const gchar* replyText = NULL;
  int level = lua_gettop(L);
  int pop = 0;

  int res = (luaL_loadstring(L, luaStr) || lua_pcall(L, 0, LUA_MULTRET, 0));
  if (res != 0) {
    pop = 1; // expecting error message only
    assert(pop == (lua_gettop(L) - level));
    assert(lua_isstring(L, -1));
    replyText = lua_tostring(L, -1);
    if (!replyText) replyText = "Error: out of memory";
    goto reply;
  }

  // Now we may have any number of values, and not all of them
  // necessarily strings.
  // 
  // Might be nice to get any values printed to string output and
  // concatenated, upto certain max length. Lua does have a "print"
  // function, but seems to use printf directly, so probably would
  // require a bit of work to have it use some safe sprintf instead.
  // Some macro magic in print.c could work.
  pop = lua_gettop(L) - level;
  logf("nresults is %d", pop);
  if (pop > 0) {
    if (pop > 1) {
      replyText = "<multiple results>";
    } else {
      // Stored within Lua state at least until the corresponding value is popped.
      replyText = lua_tostring(L, -1);
      if (!replyText) replyText = "<unconvertible expression>";
    }
    goto reply;
  }

  // Evaluated to nothing.
  replyText = "OK";

 reply:
  {
    assert(replyText); 
    GError* localError = NULL;
    if (!rk_JabberSession_send(self->session,
			       fromJid,
			       replyText,
			       &localError)) {
      gx_error_log_free(localError);
      if (pop) lua_pop(L, pop);
      cb_severeError(self, "failed to send Jabber reply");
      return rk_HALT;
    }
  }
  if (pop) lua_pop(L, pop);
  return rk_PROCEED;
}

rk_Remokon* rk_Remokon_new(GError** error)
{
  rk_Remokon* self = g_try_new0(rk_Remokon, 1);
  if (!self) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  self->params.userdata = self;

  self->params.observer.sessionEstablished = cb_sessionEstablished;
  self->params.observer.gotEof = cb_gotEof;
  self->params.observer.severeError = cb_severeError;
  self->params.observer.fatalError = cb_fatalError;
  self->params.observer.gotMsg = cb_gotMsg;
  // not defining self->params.observer.messageSent as have no flow control

  self->params.server = cf_STATIC_GET(remokon_host);
  self->params.port = cf_STATIC_GET(remokon_port);
  if (!self->params.port) self->params.port = 5222; // default Jabber port
  self->params.username = cf_STATIC_GET(username);
  self->params.password = cf_STATIC_GET(remokon_password);
  self->params.jid = cf_STATIC_GET(jid);
#if defined(__SYMBIAN32__)
  self->params.iap_id = get_ConfigDb_iap_id();
#endif /* __SYMBIAN32__ */

  self->have_config = ((self->params.server != NULL) &&
		       (self->params.username != NULL) &&
		       (self->params.password != NULL) &&
		       (self->params.jid != NULL));

  self->autostart_enabled = (force_get_ConfigDb_bool("remokon.autostart", TRUE));

  if (self->have_config) {
    logf("Jabber config: server %s:%d, username '%s', jid '%s', auto %d",
	 self->params.server, self->params.port,
	 self->params.username, self->params.jid,
	 self->autostart_enabled);
  }

  self->L = cl_lua_new_libs();
  if (!self->L) {
    rk_Remokon_destroy(self);
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  self->session = rk_JabberSession_new(&self->params, error);
  if (!self->session) {
    rk_Remokon_destroy(self);
    return NULL;
  }

  self->timer = ut_Timer_new(self, cb_timerExpired, error);
  if (!self->timer) {
    rk_Remokon_destroy(self);
    return NULL;
  }

  return self;
}

// Supports only partially initialized objects.
void rk_Remokon_destroy(rk_Remokon* self)
{
  if (self) {
    rk_Remokon_stop(self);
    ut_Timer_destroy(self->timer);
    rk_JabberSession_destroy(self->session);
    if (self->L) lua_close(self->L);
    // No params to free, all owned by config object.
    g_free(self);
  }
}

gboolean rk_Remokon_is_autostart_enabled(rk_Remokon* self)
{
  // Former value is constant, the latter may vary.
  return (self->have_config && self->autostart_enabled);
}

// Does nothing if already started.
gboolean rk_Remokon_start(rk_Remokon* self, GError** error)
{
  if (!rk_Remokon_is_started(self)) {
    if (!self->have_config) {
      // No point in retrying.
      if (error)
	*error = g_error_new(domain_cl2app, code_no_configuration,
			     "some Jabber config missing");
      return FALSE;
    }

    self->num_failures = 0;

    startSessionOrRetry(self);
  }
  return TRUE;
}

// Supports only partially initialized objects.
// Does nothing if already stopped.
void rk_Remokon_stop(rk_Remokon* self)
{
  if (self->timer) ut_Timer_cancel(self->timer);
  stopSession(self);
}

gboolean rk_Remokon_reconfigure(rk_Remokon* self,
				const gchar* key,
				const gchar* value,
				GError** error)
{
  if (strcmp(key, "remokon.autostart")) {
    self->autostart_enabled = force_lua_eval_bool(value, TRUE);
  }

#if defined(__SYMBIAN32__)
  else if (strcmp(key, "iap") == 0) {
    // It should be safe to just set this value. A change won't
    // take effect before a reconnect, but this is okay, a Remokon
    // stop followed by start will allow for that via the API.
    self->params.iap_id = force_lua_eval_int(value, __IAP_ID__);
  }
#endif /* __SYMBIAN32__ */

  return TRUE;
}

gboolean rk_Remokon_is_started(rk_Remokon* self)
{
  return (rk_JabberSession_is_started(self->session) ||
	  ut_Timer_is_active(self->timer));
}
  
gboolean rk_Remokon_is_connected(rk_Remokon* self)
{
  return self->is_connected;
}

gboolean rk_Remokon_send(rk_Remokon* self,
			 const char* toJid,
			 const char* msgText,
			 GError** error)
{
  if (!rk_Remokon_is_connected(self)) {
    if (error)
      *error = g_error_new(domain_cl2app, code_not_connected, "no Jabber connection");
    return FALSE;
  }

  return rk_JabberSession_send(self->session, toJid, msgText, error);
}

#endif /* __FEATURE_REMOKON__ */
