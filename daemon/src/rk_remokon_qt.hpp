/*
 !concept {:name => "QXmpp and Lua based remote control mechanism",
   :desc => "Accepting remote commands via XMPP and executing them using a Lua runtime."}
*/

#ifndef __rk_remokon_qt_hpp__
#define __rk_remokon_qt_hpp__

#include "lua_cl2.h"

// http://code.google.com/p/qxmpp/
#include "QXmppClient.h"
#include "QXmppMessage.h"

#include <QObject>
#include <QTimer>

// We must use a QObject derived remote control state object so as to
// allow us to implement slots for observing Qt signals.
class _rk_Remokon :
  public QObject 
{
  Q_OBJECT

  private:

  // Not sure if these must persist during session, but just in case.
  QXmppPresence iXmppPresence;
  QXmppConfiguration iXmppConfiguration;
  
  int iRunForSecs;
  QTimer iRunTimer;

  public:

  // These come mostly from the configuration file. There will be no
  // default values, and hence Remokon will be automatically disabled
  // when no configuration setting is available. We require that
  // "server", "password", and "jid" are all non-NULL. The dynamically
  // settable "iap_id" will be given some unlikely to work default
  // value.
  struct {
    const char* server;
    int port;
    const char* password;
    const char* jid;
    int iap_id;
  } params;

  // We should always have a Jabber session object after having been
  // initialized. The object creation itself requires nothing but
  // memory. Getting the connection is harder.
  QXmppClient iSession;

  // Indicates whether iSession has been asked to connect, but not
  // disconnect. If so, it should be connected or actively trying to
  // connect.
  bool iIsActive;

  // This indicates whether we have at least some kind of a value for
  // all the required configuration parameters. Leaving any of these
  // out is a simple way to disable the remote control facility.
  bool iHaveConfig;

  // This is just a hint for the controller as to whether to start
  // Remokon automatically at startup.
  bool iAutostartEnabled;

  // Here we have only one Lua instance per session, making for a
  // proper REPL. We might consider providing a way to reset the VM.
  lua_State* L;

  public:

  _rk_Remokon();

  ~_rk_Remokon();

  void start();

  void stop();

  void startTimed(int secs);

  void send(const QString& toJid, const QString& msgText);

  private:

  void resetRunTimer();

  private slots:

  void gotJabberError(QXmppClient::Error anError);

  void gotJabberMessage(const QXmppMessage& aMessage);

  void runTimeout();

};

#endif /* __rk_remokon_qt_hpp__ */

/**

Copyright 2011 Helsinki Institute for Information Technology (HIIT)
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
