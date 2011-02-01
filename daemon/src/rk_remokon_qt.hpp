#ifndef __rk_remokon_qt_hpp__
#define __rk_remokon_qt_hpp__

#include "lua_cl2.h"

#include "QXmppClient.h"

#include <QObject>
#include <QTimer>

// We must use a QObject derived remote control state object so as to
// allow us to implement slots for observing Qt signals.
class _rk_Remokon :
  public QObject 
{
  Q_OBJECT

  private:

  // We should always have a Jabber session object after having been
  // initialized. The object creation itself requires nothing but
  // memory. Getting the connection is harder.
  QXmppClient* iSession; // xxx

  // These come mostly from the configuration file. There will be no
  // default values, and hence Remokon will be automatically disabled
  // when no configuration setting is available. We require that
  // "server", "password", "username", and "jid" are all non-NULL. The
  // dynamically settable "iap_id" will be given some unlikely to work
  // default value.
  
  // xxx

  public:

  // This indicates whether we have at least some kind of a value for
  // all the required configuration parameters. Leaving any of these
  // out is a simple way to disable the remote control facility.
  bool iHaveConfig;

  // This is just a hint for the controller as to whether to start
  // Remokon automatically at startup.
  bool iAutostartEnabled;

  // We use this flag to keep track of whether "iSession" has a
  // connection. It does not itself keep track of that, as it is too
  // low-level for such things. It is us who must decide whether to
  // request a disconnect on a write error, say.
  bool iIsConnected;

  // Number of consecutive failed connection attempts.
  int iNumFailures;

  // A retry timer.
  QTimer iTimer;

  // Here we have only one Lua instance per session, making for a
  // proper REPL. We might consider providing a way to reset the VM.
  lua_State* L;

  public:

  _rk_Remokon();

  ~_rk_Remokon();

  void send(const QString& toJid, const QString& msgText);

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
