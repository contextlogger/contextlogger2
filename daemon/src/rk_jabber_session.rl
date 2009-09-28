// -*- c -*-

// xxx What makes it hard to get memory management right with Iksemel
// is that some functions allocate memory from an internal data
// structure, and some return a fresh heap cell. Hard to know what to
// free and what not to free.
// 
// xxx Also, is that ikstack memory allocated ad infinitum, and never
// freed long as the stream exists? We may want to create our own
// ikstack object here, and clear its contents at suitable times, when
// we know no one is using the allocated values any longer.

#include "rk_jabber_session.h"

#if __FEATURE_REMOKON__

#include "er_errors.h"
#include "utils_cl2.h"

#include "iksemel.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
  !concept {:name => "Keeping track of AO state with Ragel.",
            :desc => "Using a Ragel-generated state machine to track active object (AO) state."}
*/

typedef char letter; // alphabet type, big enough for us

typedef struct {
  int cs; // current state
} RagelState;

struct _rk_JabberSession {
  const rk_JabberParams* params; // not owned, client-provided
  ikss_Stream* stream;
  int features;
  RagelState r;
  char* fromJid;
  char* inMsgText;
};

static void clearInState(rk_JabberSession* self)
{
  FREE_Z(self->fromJid, free);
  FREE_Z(self->inMsgText, free);
}

// The callbacks may be NULL, and none of them return anything, and
// hence this definition is suitable.
#define call_obs(_msg,_arg...) { \
  if (self->params->observer._msg)					\
    ((*(self->params->observer._msg))(self->params->userdata, _arg));	\
  }

// Yes, this is necessary, call_obs will not match.
#define call_obs_noargs(_msg) { \
  if (self->params->observer._msg)					\
    ((*(self->params->observer._msg))(self->params->userdata));		\
  }

// We never reach a final state, but we might get an unexpected state
// transition, i.e. a "parse error".
#define at_parse_error (self->r.cs == fsm_error)

#define is_session_established (self->r.cs == fsm_first_final)

static gboolean doNext(rk_JabberSession* self, letter evCode);

// Yes, you must define the label "fail" in all the necessary
// contexts. Not to worry, the compiler will tell you so.
#define handle_fatal_error(_str) \
  { call_obs(fatalError, _str); goto fail; }

#define handle_severe_error(_str) \
  { call_obs(severeError, _str); goto fail; }

#define handle_oom_error handle_fatal_error("out of memory")

#define handle_parse_error(_evcode) \
  { handle_severe_error("unexpected event " #_evcode); }

#define check_send_rval(_rval) \
  { \
    if (_rval) {		    \
      logf("iks return %d", _rval);		\
      handle_severe_error("messaging failure"); \
    } \
  }

#define got_event(_evcode) \
  logf("got event " #_evcode); \
  if (!doNext(self, _evcode)) { \
    handle_parse_error(_evcode); \
  }

// We want these defined both for C and Ragel, and putting them in a
// separate file appears to be the only way to make that happen.
#include "rk_transport_event_list.h"

// What we do here is we drive the protocol with Ragel. With know what
// sort of a reply to expect based on the state we are in, and we know
// what action to take to transition out of a state.
// 
// Our solution is somewhat different than the one shown in
// http://www.zedshaw.com/essays/ragel_state_charts.html, but I
// suppose the effect is the same.
%%{

  machine fsm;

  access self->r.;

  import "rk_transport_event_list.h";

  action SendHeader {
    int ret = ikss_Stream_send_header(self->stream);
    check_send_rval(ret);
  }

  action Authenticate {
    if ((self->features & (IKS_STREAM_SASL_MD5 | 
			   IKS_STREAM_SASL_PLAIN)) == 0) {
      handle_severe_error("authentication required but unsupported by server");
    } else {
      int ret = 0;
      if (self->features & IKS_STREAM_SASL_MD5) {
	ret = ikss_Stream_start_sasl(self->stream, IKS_SASL_DIGEST_MD5,
				     self->params->username, 
				     self->params->password);
      } else if (self->features & IKS_STREAM_SASL_PLAIN) { 
	ret = ikss_Stream_start_sasl(self->stream, IKS_SASL_PLAIN,
				     self->params->username, 
				     self->params->password);
      }
      check_send_rval(ret);
    }
  }

# xxx our state machine assumes that we do bind
  action Bind {
    if (self->features & IKS_STREAM_BIND) {
      iksid* id = iks_id_new(ikss_Stream_parser_stack(self->stream), 
			     self->params->jid);
      if (!id) handle_oom_error;
      iks *x = iks_make_resource_bind (id);
      if (!x) handle_oom_error;
      int ret = ikss_Stream_send_node(self->stream, x);
      iks_delete(x);
      check_send_rval(ret);
    }
  }

# xxx our state machine assumes that we request a session
  action ReqSession {
    // It seems this session bit cannot be sent before we
    // receive the bind reply.
    if (self->features & IKS_STREAM_SESSION) {
      iks* x = iks_make_session();
      if (!x) handle_oom_error;
      iks_insert_attrib (x, "to", self->params->server);
      iks_insert_attrib(x, "id", "sess_1");
      int ret = ikss_Stream_send_node(self->stream, x);
      iks_delete(x);
      check_send_rval(ret);
    }
  }

  action ShowPresence {
    iks *x = iks_make_pres (IKS_SHOW_CHAT, NULL);
    if (!x) handle_oom_error;
    int ret = ikss_Stream_send_node(self->stream, x);
    iks_delete(x);
    check_send_rval(ret);
  }

  action SessionReady {
    // Not sure if we want to be sending explicit announcements, as
    // presence info is anyway being sent. Probably not, but this may
    // be useful during testing. One issue certainly is that we do
    // not know who to send the message to, unless we query the server
    // for our buddy list or something.
    /*
    iks *x = iks_make_msg (IKS_TYPE_CHAT, peer_jid, "Hi! I have returned.");
    if (!x) handle_oom_error;
    int ret = ikss_Stream_send_node(self->stream, x);
    iks_delete(x);
    check_send_rval(ret);
    */

    // Let the client know that the Jabber channel is ready for messaging.
    call_obs_noargs(sessionEstablished);
  }

  protocol = connected >SendHeader 
    features_tag >Authenticate 
    success_tag >SendHeader 
    features_tag >Bind 
    iq_tag >ReqSession 
    iq_tag >ShowPresence 
    presence_tag >SessionReady 
    ;

# This is essential, or we get a segmentation fault.
 main := protocol;

}%%

%%write data;

// Our machine never gets to an EOF state. If we get a disconnect or
// something, we simply stop invoking the state machine. The machine
// must be reset to its original state when the user attempts a
// reconnect; this requires little else apart from setting self->r.cs
// to 0.
// 
// Note that you cannot really go invoking this function from within
// an action.
static gboolean doNext(rk_JabberSession* self, letter evCode)
{
  //logf("doNext %d", evCode);
  const letter* p = &evCode;
  const letter* pe = p + 1;
  //const int* eof = NULL;
  %%write exec;
  return !at_parse_error;

  // This is for "non-local" escaping from machine actions.
 fail:
  return FALSE;
}

// See http://xmpp.org/rfcs/rfc3921.html for info on the messaging protocol.
static int tagHook(void *user_data, int type, iks *node)
{
  rk_JabberSession* self = (rk_JabberSession*)user_data;

#if __DO_LOGGING__
  logf("tag event %d", type);
  if (node)
    logf("tag name '%s'", iks_name(node));
#endif

  switch (type)
    {
    case IKS_NODE_START:
      {
        // Server should reply to our initial <stream> send, and we
        // should get this event. Likely we get both the <stream> open
        // and then a <stream:features> element. We do not support TLS
        // even if the server does. But if the features list SASL, we
        // should do the SASL authentication. We will need to get our
        // session established in a reasonable amount of time, as not
        // all servers are going to wait forever, but once a session
        // has been set up, then hopefully it will stay up for as long
        // as we like.

	// Nothing to do here.
        break;
      }
    case IKS_NODE_NORMAL:
      {
	if (!is_session_established) {
	  // So we receive <stream:features> twice during a successful
	  // session establishment. First with the authentication
	  // features, and then with the binding and session features.
	  if (strcmp("stream:features", iks_name(node)) == 0) {
	    self->features = iks_stream_features(node);
	    got_event(features_tag);
	  }

	  else if (strcmp("failure", iks_name(node)) == 0) {
	    handle_severe_error("sasl authentication failed");
	  }

	  else if (strcmp("success", iks_name(node)) == 0) {
	    logf("jabber session ready");
	    got_event(success_tag);
	  }

	  else if (strcmp("iq", iks_name(node)) == 0) {
	    got_event(iq_tag);
	  }

	  else if (strcmp("iq", iks_name(node)) == 0) {
	    got_event(iq_tag);
	  }

	  else if (strcmp("presence", iks_name(node)) == 0) {
	    got_event(presence_tag);
	  }
	}

	else if (strcmp("message", iks_name(node)) == 0) {
	  char* jid = iks_find_attrib (node, "from"); // xxx free this?

	  // Do we need the message "id" attribute for anything, I wonder?
	  // char* id = iks_find_attrib (x, "id");

	  char* text = iks_find_cdata (node, "body"); // xxx free this?

	  if (jid && text) {
	    /*
	      clearInState(self);
	      self->inJid = jid;
	      self->inMsgText = text;
	    */

	    // void (*gotMsg)(void* userdata, const char* fromJid, const char* inMsgText);
	    call_obs(gotMsg, jid, text);
	  } 
	  /*
	  else {
            // This is nothing unusual, some clients do produce such
            // messages to send chat state information and such.

	    //logt("incoming 'message' without expected attrs and elems");
	  }
	  */
	}

        break;
      }
    case IKS_NODE_STOP:
      {
	handle_severe_error("server disconnected");
        break;
      }
    case IKS_NODE_ERROR:
      {
	handle_severe_error("error tag received");
        break;
      }
    }

 fail:
  return 0;
}

static int ctrlHook(void *user_data, int type, int data)
{
  rk_JabberSession* self = (rk_JabberSession*)user_data;

  logf("status event %d", type);

  switch (type)
    {
    case ikss_CONNECTED:
      {
	got_event(connected);
        break;
      }
    case ikss_EOF:
      {
	call_obs_noargs(gotEof);
	break;
      }
    case ikss_IKS_ERROR:
      {
	logf("transport error: iksemel %d", data);
	handle_severe_error("transport error");
	break;
      }
    case ikss_PLAT_ERROR:
      {
	logf("transport error: %s (%d)", plat_error_strerror(data), data);
	handle_severe_error("transport error");
	break;
      }
    case ikss_SENT:
      {
	call_obs_noargs(messageSent);
        break;
      }
    }

 fail:
  return 0;
}

static void resetMachine(rk_JabberSession* self)
{
  %%write init;
}

rk_JabberSession* rk_JabberSession_new(const rk_JabberParams* params,
				       GError** error)
{
  rk_JabberSession* self = g_try_new0(rk_JabberSession, 1);
  if (!self) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  self->params = params;

  return self;
}

void rk_JabberSession_destroy(rk_JabberSession* self)
{
  if (self) {
    rk_JabberSession_stop(self);
    clearInState(self);
    g_free(self);
  }
}

#define set_gerror_from_iks_error(_ret) { \
    if (error) \
      *error = g_error_new(domain_iksemel, _ret, "Iksemel error: %d", _ret); \
  }

#if defined(__SYMBIAN32__)
#define IAP_ID (self->params->iap_id)
#else
#define IAP_ID 0
#endif /* __SYMBIAN32__ */

gboolean rk_JabberSession_start(rk_JabberSession* self,
				GError** error)
{
  if (!self->stream) {
    ikss_Stream* stream = ikss_Stream_new(IKS_NS_CLIENT, self, tagHook, ctrlHook);
    if (!stream) {
      if (error) *error = gx_error_no_memory;
      return FALSE;
    }
      
    resetMachine(self);

    int ret = ikss_Stream_connect(stream, self->params->server, 
				  self->params->port, IAP_ID);
    if (ret) {
      set_gerror_from_iks_error(ret);
      ikss_Stream_destroy(stream);
      return FALSE;
    }

    self->stream = stream;
  }
  return TRUE;
}
  
void rk_JabberSession_stop(rk_JabberSession* self)
{
  if (self->stream) {
    ikss_Stream_destroy(self->stream);
    self->stream = NULL;
  }
}

// Note that this does not mean that a session is established (yet or
// any longer), but the client gets events about connection
// establishment and breakage, so if needed, the client can keep track
// of that.
gboolean rk_JabberSession_is_started(rk_JabberSession* self)
{
  return self->stream != NULL;
}

// Sends a message containing the given UTF-8 text.
// The message is sent to the specified JID.
gboolean rk_JabberSession_send(rk_JabberSession* self,
			       const char* toJid,
			       const char* msgText,
			       GError** error)
{
  iks *x = iks_make_msg(IKS_TYPE_CHAT, toJid, msgText);
  if (!x) {
    if (error) *error = gx_error_no_memory;
    return FALSE;
  }  
  int ret = ikss_Stream_send_node(self->stream, x);
  iks_delete(x);
  if (ret) {
    set_gerror_from_iks_error(ret);
    return FALSE;
  }
  return TRUE;
}

#endif // __FEATURE_REMOKON__
