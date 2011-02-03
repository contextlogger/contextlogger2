#include "rk_remokon.h"
#include "rk_remokon_qt.hpp"

#include "ac_app_context.h"
#include "cf_query.h" // get_ConfigDb_int
#include "er_errors.h"

#include "QXmppLogger.h"

#include <stdlib.h>

// --------------------------------------------------
// _rk_Remokon
// --------------------------------------------------

static lua_State* new_my_lua()
{
  lua_State* L = cl_lua_new_libs();
  if (!L) {
    throw std::bad_alloc();
  }
  return L;
}

_rk_Remokon::_rk_Remokon() :
  iIsActive(false), L(new_my_lua())
{
  this->params.server = ac_STATIC_GET(remokon_host);
  this->params.port = ac_STATIC_GET(remokon_port);
  this->params.password = ac_STATIC_GET(remokon_password);
  this->params.jid = ac_STATIC_GET(jid);
#if defined(__SYMBIAN32__)
  this->params.iap_id = get_config_iap_id();
#endif /* __SYMBIAN32__ */

  this->iHaveConfig = ((this->params.server != NULL) &&
		       (this->params.password != NULL) &&
		       (this->params.jid != NULL));

  this->iAutostartEnabled = (force_get_ConfigDb_bool("remokon.autostart", TRUE));

  if (this->iHaveConfig) {
    logg("Jabber config: server %s:%d, jid '%s', auto %d",
	 this->params.server, this->params.port,
	 this->params.jid,
	 this->iAutostartEnabled);
  }

  iXmppPresence.setType(QXmppPresence::Available);
  QXmppPresence::Status& stat = iXmppPresence.status();
  stat.setType(QXmppPresence::Status::Chat);
  stat.setStatusText(QString("Logging away."));

  iXmppConfiguration.setHost(params.server);
  iXmppConfiguration.setPort(params.port);
  iXmppConfiguration.setJid(params.jid);
  iXmppConfiguration.setPassword(params.password);

  //xxx specify desired security policy

  // Note that "connect" produces a boolean return value if you want
  // to check.
  connect(&iSession, SIGNAL(error(QXmppClient::Error)),
	  this, SLOT(gotJabberError(QXmppClient::Error)));
  connect(&iSession, SIGNAL(messageReceived(const QXmppMessage&)),
	  this, SLOT(gotJabberMessage(const QXmppMessage&)));
}

_rk_Remokon::~_rk_Remokon()
{
  if (L) 
    lua_close(L);
}

void _rk_Remokon::start()
{
  iSession.connectToServer(iXmppConfiguration, iXmppPresence);
  iIsActive = true;
}

void _rk_Remokon::stop()
{
  iSession.disconnectFromServer();
  iIsActive = false;
}

void _rk_Remokon::send(const QString& toJid, const QString& msgText)
{
  iSession.sendMessage(toJid, msgText);
}

// Not sure yet if we need to handle any of these errors. The client
// object is supposed to itself do some retrying. For now we just log.
void _rk_Remokon::gotJabberError(QXmppClient::Error anError)
{
  switch (anError)
    {
    case QXmppClient::SocketError:
      {
	er_log_none(0, "remokon: %d (%s)", anError, "QXmppClient::SocketError");
        break;
      }
    case QXmppClient::KeepAliveError:
      {
	er_log_none(0, "remokon: %d (%s)", anError, "QXmppClient::KeepAliveError");
        break;
      }
    case QXmppClient::XmppStreamError:
      {
	er_log_none(0, "remokon: %d (%s)", anError, "QXmppClient::XmppStreamError");
        break;
      }
    default:
      {
	er_log_none(0, "remokon: %d (unknown QXmpp error)", anError);
        break;
      }
    }
}

class LuaStack
{
private:
  lua_State* L;
public:
  int pop;
public:
  LuaStack(lua_State* aL) : L(aL), pop(0) {}
  ~LuaStack() { 
    if (pop) {
      lua_pop(L, pop);
      //logg("remokon Lua stack: popped %d", pop);
    }
  }
};

#define TOCSTR(exp) ((exp).toUtf8().data())

void _rk_Remokon::gotJabberMessage(const QXmppMessage& msg)
{
  if (msg.body().isEmpty())
    return;

  const char* fromJid = TOCSTR(msg.from());
  const char* luaStr = TOCSTR(msg.body());
  logg("remote message from %s: %s", fromJid, luaStr);

  // If set, points to a literal or something within Lua state.
  const gchar* replyText = NULL;

  LuaStack stack(L);
  int level = lua_gettop(L);
  int& pop = stack.pop;

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
  assert(replyText); 
  send(msg.from(), QString(replyText));
}

// --------------------------------------------------
// public API
// --------------------------------------------------

#define TRAPFATAL(_stm) {					\
    try {							\
      _stm ;							\
    } catch (const std::exception &ex) {			\
      er_log_none(er_FATAL, "remokon: %s", ex.what());		\
    }								\
  }

#define GTRAP(_ret, _stm) {					\
    try {							\
      _stm ;							\
    } catch (const std::exception &ex) {			\
      if (error)						\
	*error = gx_error_new(domain_qt, -1,			\
			      "remokon: %s", ex.what());	\
      return _ret;						\
    }								\
  }

extern "C"
rk_Remokon* rk_Remokon_new(GError** error)
{
#if !defined(__SYMBIAN32__) && !defined(NDEBUG)
  QXmppLogger::getLogger()->setLoggingType(QXmppLogger::StdoutLogging);
#endif

  rk_Remokon* self = NULL;
  GTRAP(NULL, self = q_check_ptr(new rk_Remokon));
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
  return (self->iHaveConfig && self->iAutostartEnabled);
}

// Does nothing if already started.
extern "C"
gboolean rk_Remokon_start(rk_Remokon* self, GError** error)
{
  if (!rk_Remokon_is_started(self)) {
    if (!self->iHaveConfig) {
      // No point in trying to start without proper configuration.
      if (error)
	*error = gx_error_new(domain_cl2app, code_no_configuration,
			     "some Jabber config missing");
      return FALSE;
    }

    GTRAP(FALSE, self->start());
  }
  return TRUE;
}

// Supports only partially initialized objects.
// Does nothing if already stopped.
extern "C"
void rk_Remokon_stop(rk_Remokon* self)
{
  TRAPFATAL(self->stop());
}

extern "C"
gboolean rk_Remokon_reconfigure(rk_Remokon* self,
				const gchar* key,
				const gchar* value,
				GError** error)
{
  if (strcmp(key, "remokon.autostart")) {
    self->iAutostartEnabled = force_lua_eval_bool(value, TRUE);
  }

#if defined(__SYMBIAN32__)
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
  return self && self->iIsActive;
}
  
extern "C"
gboolean rk_Remokon_is_connected(rk_Remokon* self)
{
  return self && self->iSession.isConnected();
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

  GTRAP(FALSE, self->send(QString(toJid), QString(msgText)));

  return TRUE;
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
