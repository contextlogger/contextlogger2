#include "rk_remokon.h"
#include "rk_remokon_qt.hpp"

#include "ac_app_context.h"
#include "cf_query.h" // get_ConfigDb_int
#include "er_errors.h"

#include <stdlib.h>

#if 0
static void handleTimerError(rk_Remokon* self, GError* timerError)
{
  logt("timer error in Remokon");
  gx_dblog_fatal_error_free(ac_global_LogDb, timerError);
}

static void setRetryTimer(rk_Remokon* self)
{
  (self->num_failures)++;

  int secs = 2 * 60 * self->num_failures + (rand() % 60);
  logg("retrying Jabber connection in %d secs", secs);

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
    gx_txtlog_error_free(localError);
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
  logg("Jabber error: %s", msg);
  stopSession(self);
  setRetryTimer(self);
  return rk_HALT;
}

// rk_JabberObserver
static int cb_fatalError(void* userdata, const char* msg)
{
  er_log_none(er_FATAL, "Jabber error: %s", msg);
  return rk_HALT;
}

// rk_JabberObserver
static int cb_gotMsg(void* userdata, const char* fromJid, const char* luaStr)
{
  rk_Remokon* self = (rk_Remokon*)userdata;
  lua_State* L = self->L;

  logg("remote message from %s: %s", fromJid, luaStr);

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
  logg("nresults is %d", pop);
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
      gx_txtlog_error_free(localError);
      if (pop) lua_pop(L, pop);
      cb_severeError(self, "failed to send Jabber reply");
      return rk_HALT;
    }
  }
  if (pop) lua_pop(L, pop);
  return rk_PROCEED;
}
#endif

// --------------------------------------------------
// _rk_Remokon
// --------------------------------------------------

_rk_Remokon::_rk_Remokon() :
  L(NULL)
{
#if 0
  this->params.userdata = this;

  this->params.observer.sessionEstablished = cb_sessionEstablished;
  this->params.observer.gotEof = cb_gotEof;
  this->params.observer.severeError = cb_severeError;
  this->params.observer.fatalError = cb_fatalError;
  this->params.observer.gotMsg = cb_gotMsg;
  // not defining this->params.observer.messageSent as have no flow control

  this->params.server = ac_STATIC_GET(remokon_host);
  this->params.port = ac_STATIC_GET(remokon_port);
  this->params.username = ac_STATIC_GET(username);
  this->params.password = ac_STATIC_GET(remokon_password);
  this->params.jid = ac_STATIC_GET(jid);
#if defined(__SYMBIAN32__)
  this->params.iap_id = get_config_iap_id();
#endif /* __SYMBIAN32__ */

  this->iHaveConfig = ((this->params.server != NULL) &&
		       (this->params.username != NULL) &&
		       (this->params.password != NULL) &&
		       (this->params.jid != NULL));

  this->autostart_enabled = (force_get_ConfigDb_bool("remokon.autostart", TRUE));

  if (this->iHaveConfig) {
    logg("Jabber config: server %s:%d, username '%s', jid '%s', auto %d",
	 this->params.server, this->params.port,
	 this->params.username, this->params.jid,
	 this->autostart_enabled);
  }
#endif

  this->L = cl_lua_new_libs();
  if (!this->L) {
    throw std::bad_alloc();
  }

  /*
  this->session = rk_JabberSession_new(&this->params, error);
  if (!this->session) {
    rk_Remokon_destroy(this);
    return NULL;
  }

  this->timer = ut_Timer_new(this, cb_timerExpired, error);
  if (!this->timer) {
    rk_Remokon_destroy(this);
    return NULL;
  }
  */ //xxx
}

_rk_Remokon::~_rk_Remokon()
{
  /*
    ut_Timer_destroy(self->timer);
    rk_JabberSession_destroy(self->session);
  */ //xxx
  if (L) 
    lua_close(L);
}

// --------------------------------------------------
// public API
// --------------------------------------------------

extern "C"
rk_Remokon* rk_Remokon_new(GError** error)
{
  rk_Remokon* self = NULL;
  try {
    self = q_check_ptr(new rk_Remokon);
  } catch (const std::exception &ex) {
    if (error)
      *error = gx_error_new(domain_qt, -1, "Remokon init failure: %s", ex.what());
    return NULL;
  }

  return self;
}

// Supports only partially initialized objects.
extern "C"
void rk_Remokon_destroy(rk_Remokon* self)
{
  delete self;
}

extern "C"
gboolean rk_Remokon_is_autostart_enabled(rk_Remokon* self)
{
  // Former value is constant, the latter may vary.
  return (self->iHaveConfig && self->autostart_enabled);
}

// Does nothing if already started.
extern "C"
gboolean rk_Remokon_start(rk_Remokon* self, GError** error)
{
  /*
  if (!rk_Remokon_is_started(self)) {
    if (!self->iHaveConfig) {
      // No point in retrying.
      if (error)
	*error = gx_error_new(domain_cl2app, code_no_configuration,
			     "some Jabber config missing");
      return FALSE;
    }

    self->num_failures = 0;

    startSessionOrRetry(self);
  }
  */ //xxx
  return TRUE;
}

// Supports only partially initialized objects.
// Does nothing if already stopped.
extern "C"
void rk_Remokon_stop(rk_Remokon* self)
{
  /*
  if (self->timer) ut_Timer_cancel(self->timer);
  stopSession(self);
  */
  //xxx
}

extern "C"
gboolean rk_Remokon_reconfigure(rk_Remokon* self,
				const gchar* key,
				const gchar* value,
				GError** error)
{
  if (strcmp(key, "remokon.autostart")) {
    self->autostart_enabled = force_lua_eval_bool(value, TRUE);
  }

#if 0 && defined(__SYMBIAN32__) ///xxx
  else if (strcmp(key, "iap") == 0) {
    // It should be safe to just set this value. A change won't
    // take effect before a reconnect, but this is okay, a Remokon
    // stop followed by start will allow for that via the API.
    self->params.iap_id = force_lua_eval_int(value, -1);
  }
#endif /* __SYMBIAN32__ */

  return TRUE;
}

extern "C"
gboolean rk_Remokon_is_started(rk_Remokon* self)
{
  /*
  return (rk_JabberSession_is_started(self->session) ||
	  ut_Timer_is_active(self->timer));
  */
  return FALSE; //xxx
}
  
extern "C"
gboolean rk_Remokon_is_connected(rk_Remokon* self)
{
  return self->is_connected;
}

extern "C"
gboolean rk_Remokon_send(rk_Remokon* self,
			 const char* toJid,
			 const char* msgText,
			 GError** error)
{
  if (!rk_Remokon_is_connected(self)) {
    if (error)
      *error = gx_error_new(domain_cl2app, code_not_connected, 
			    "no Jabber connection");
    return FALSE;
  }

  //return rk_JabberSession_send(self->session, toJid, msgText, error);
  return FALSE; //xxx
}

/**

Copyright 2009-2011 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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
