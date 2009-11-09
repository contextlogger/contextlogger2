#ifndef __rk_jabber_session_h__
#define __rk_jabber_session_h__

// This is an internal Remoken component that encapsulates all of the
// functionality related to Jabber sessions. In particular, the
// Iksemel library is kept internal to this component.
// 
// You need to provide this component with all of the Jabber details,
// and it will then attempt to establish a session. The primary
// purpose is to pass, in callbacks, the incoming (textual) messages
// from "buddies". Naturally there will also be callbacks regarding
// session breakage and errors, and a facility for sending message(s)
// to a specific Jabber user.
// 
// There is no functionality for timeouts or retries of configuration
// changes, you must handle these at a higher level by reestablishing
// the Jabber session as required.

#include "application_config.h"

#if __FEATURE_REMOKON__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

#define rk_PROCEED 0 // IKS_OK
#define rk_HALT 3 // IKS_HOOK

  // Any returned messages as allocated internally by rk_JabberSession
  // as required. Should return rk_PROCEED to indicate that
  // rk_JabberSession may take further action, or rk_HALT to indicate
  // that it should not.
  typedef struct {
    int (*sessionEstablished)(void* userdata);
    int (*gotEof)(void* userdata);
    int (*severeError)(void* userdata, const char* msg);
    int (*fatalError)(void* userdata, const char* msg);
    int (*messageSent)(void* userdata);
    int (*gotMsg)(void* userdata, const char* fromJid, const char* msgText);
  } rk_JabberObserver;

  // Jabber parameters for the current (or next) Jabber session. These
  // are not owned by the session, but they may not be modified when
  // rk_JabberSession_is_started returns true. If you need to change
  // the params, first stop any existing session, then change them,
  // and then create a new session.
  typedef struct {
    void* userdata;
    rk_JabberObserver observer;
    const char* server; // hostname, with full domain as required for DNS
    int port; // server port to connect to (typically 5222)
    const char* username; // just username, no domain or anything
    const char* password;
    const char* jid; // Jabber ID, includes @domain and any /Home or whatever
#if defined(__SYMBIAN32__)
    int iap_id; // access point to use for connecting
#endif /* __SYMBIAN32__ */
  } rk_JabberParams;

  typedef struct _rk_JabberSession rk_JabberSession;

  rk_JabberSession* rk_JabberSession_new(const rk_JabberParams* params,
					 GError** error);

  void rk_JabberSession_destroy(rk_JabberSession* self);

  // Attempts to establish a session with the Jabber server.
  // Asynchronous.
  gboolean rk_JabberSession_start(rk_JabberSession* self,
				  GError** error);
  
  // Breaks any existing connection. Does nothing if already stopped.
  // Synchronous.
  void rk_JabberSession_stop(rk_JabberSession* self);

  // Whether is started, but not necessarily connected.
  gboolean rk_JabberSession_is_started(rk_JabberSession* self);
  
  // Sends a message containing the given UTF-8 text.
  // The message is sent to the specified JID.
  // You may not use this when there is no established session.
  gboolean rk_JabberSession_send(rk_JabberSession* self,
				 const char* toJid,
				 const char* msg,
				 GError** error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif // __FEATURE_REMOKON__

#endif /* __rk_jabber_session_h__ */
