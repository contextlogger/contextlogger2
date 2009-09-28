/* iksemel (XML parser for Jabber)
** Copyright (C) 2000-2007 Gurer Ozen
** This code is free software; you can redistribute it and/or
** modify it under the terms of GNU Lesser General Public License.
*/

// I guess the idea with the original was that a connect might take a
// long time, hence making it sensible to do an async connect, but
// that a read or write to an established connection would not take
// that long, and there was also a timeout for reads. We want to do
// everything in async manner, however.

// In the original, not all allocations were checked, and such
// robustness issues. Here we try to do checking, but this does not
// help much unless the rest of the library is improved as well.
// Allocation failures are reported via the callback function.

// We have stripped out all of the TLS related bits from here, to make
// to code shorter and easier to grasp. We may have to bring back some
// of that code if we find a way to implement TLS for Symbian.

/*
  !concept {:name => "Asynchronous Iksemel use.",
            :desc => "Hacking Iksemel to do asynchronous streaming."}
*/

#include "common.h"
#include "iksemel.h"

#include "ikst_transport.h"

#include <assert.h>

struct _ikss_Stream {
  iksparser *prs;
  ikstack *s;
  char *name_space;
  void *user_data; // passed to streamHook callbacks
  const char *server; // server domain name
  iksStreamHook *streamHook; // used to notify client of tags streamed in
  ikss_NotifyFunc* notifyFunc; // used to notify client about other events

  iks *current; // currently held XML node

  char *recv_buf;

  char *send_buf;
  int send_len; // total num bytes to send
  int send_count; // sent so far
  char *send_more_buf;

  void *sock; // have a connection when non-NULL
  const char *auth_username; // SASL
  const char *auth_pass; // SASL
};

static int send_more(ikss_Stream *self);

static void
insert_attribs (iks *x, char **atts)
{
  if (atts) {
    int i = 0;
    while (atts[i]) {
      iks_insert_attrib (x, atts[i], atts[i+1]);
      i += 2;
    }
  }
}

#define CNONCE_LEN 4

static void
parse_digest (char *message, const char *key, char **value_ptr, char **value_end_ptr)
{
  char *t;

  *value_ptr = NULL;
  *value_end_ptr = NULL;

  t = strstr(message, key);
  if (t) {
    t += strlen(key);
    *value_ptr = t;
    while (t[0] != '\0') {
      if (t[0] != '\\' && t[1] == '"') {
	++t;
	*value_end_ptr = t;
	return;
      }
      ++t;
    }
  }
}

static iks *
make_sasl_response (ikss_Stream *self, char *message)
{
  iks *x = NULL;
  char *realm, *realm_end;
  char *nonce, *nonce_end;
  char cnonce[CNONCE_LEN*8 + 1];
  iksmd5 *md5;
  unsigned char a1_h[16], a1[33], a2[33], response_value[33];
  char *response, *response_coded;
  int i;

  parse_digest(message, "realm=\"", &realm, &realm_end);
  parse_digest(message, "nonce=\"", &nonce, &nonce_end);

  /* nonce is necessary for auth */
  if (!nonce || !nonce_end) return NULL;
  *nonce_end = '\0';

  /* if no realm is given use the server hostname */
  if (realm) {
    if (!realm_end) return NULL;
    *realm_end = '\0';
  } else {
    realm = (char *) self->server;
  }

  /* generate random client challenge */
  for (i = 0; i < CNONCE_LEN; ++i)
    sprintf (cnonce + i*8, "%08x", rand());

  md5 = iks_md5_new();
  if (!md5) return NULL;

  iks_md5_hash (md5, (const unsigned char*)self->auth_username, iks_strlen (self->auth_username), 0);
  iks_md5_hash (md5, (const unsigned char*)":", 1, 0);
  iks_md5_hash (md5, (const unsigned char*)realm, iks_strlen (realm), 0);
  iks_md5_hash (md5, (const unsigned char*)":", 1, 0);
  iks_md5_hash (md5, (const unsigned char*)self->auth_pass, iks_strlen (self->auth_pass), 1);
  iks_md5_digest (md5, a1_h);

  iks_md5_reset (md5);
  iks_md5_hash (md5, (const unsigned char*)a1_h, 16, 0);
  iks_md5_hash (md5, (const unsigned char*)":", 1, 0);
  iks_md5_hash (md5, (const unsigned char*)nonce, iks_strlen (nonce), 0);
  iks_md5_hash (md5, (const unsigned char*)":", 1, 0);
  iks_md5_hash (md5, (const unsigned char*)cnonce, iks_strlen (cnonce), 1);
  iks_md5_print (md5, (char*)a1);

  iks_md5_reset (md5);
  iks_md5_hash (md5, (const unsigned char*)"AUTHENTICATE:xmpp/", 18, 0);
  iks_md5_hash (md5, (const unsigned char*)self->server, iks_strlen (self->server), 1);
  iks_md5_print (md5, (char*)a2);

  iks_md5_reset (md5);
  iks_md5_hash (md5, (const unsigned char*)a1, 32, 0);
  iks_md5_hash (md5, (const unsigned char*)":", 1, 0);
  iks_md5_hash (md5, (const unsigned char*)nonce, iks_strlen (nonce), 0);
  iks_md5_hash (md5, (const unsigned char*)":00000001:", 10, 0);
  iks_md5_hash (md5, (const unsigned char*)cnonce, iks_strlen (cnonce), 0);
  iks_md5_hash (md5, (const unsigned char*)":auth:", 6, 0);
  iks_md5_hash (md5, (const unsigned char*)a2, 32, 1);
  iks_md5_print (md5, (char*)response_value);

  iks_md5_delete (md5);

  i = iks_strlen (self->auth_username) + iks_strlen (realm) +
    iks_strlen (nonce) + iks_strlen (self->server) +
    CNONCE_LEN*8 + 136;
  response = iks_malloc (i);
  if (!response) return NULL;

  sprintf (response, "username=\"%s\",realm=\"%s\",nonce=\"%s\""
	   ",cnonce=\"%s\",nc=00000001,qop=auth,digest-uri=\""
	   "xmpp/%s\",response=%s,charset=utf-8",
	   self->auth_username, realm, nonce, cnonce,
	   self->server, response_value);

  response_coded = iks_base64_encode (response, 0);
  if (response_coded) {
    x = iks_new ("response");
    iks_insert_cdata (x, response_coded, 0);
    iks_free (response_coded);
  }
  iks_free (response);

  return x;
}

// xxx needs error return value
static void
send_sasl_challenge (ikss_Stream *self, iks *challenge)
{
  char *message;
  iks *x;
  char *tmp;

  tmp = iks_cdata (iks_child (challenge));
  if (!tmp) return;

  /* decode received blob */
  message = iks_base64_decode (tmp);
  if (!message) return;

  /* reply the challenge */
  if (strstr (message, "rspauth")) {
    x = iks_new ("response");
  } else {
    x = make_sasl_response (self, message);
  }
  if (x) {
    iks_insert_attrib (x, "xmlns", IKS_NS_XMPP_SASL);
    ikss_Stream_send_node (self, x); // xxx check return value
    iks_delete (x);
  }
  iks_free (message);
}

// iksparser calls this when it has a full tag
static int
tagHook (ikss_Stream *self, char *name, char **atts, int type)
{
  iks *x;
  int err;

  switch (type) {
  case IKS_OPEN:
  case IKS_SINGLE:
    if (self->current) {
      x = iks_insert (self->current, name);
      insert_attribs (x, atts);
    } else {
      x = iks_new (name);
      insert_attribs (x, atts);
      if (iks_strcmp (name, "stream:stream") == 0) {
	err = self->streamHook (self->user_data, IKS_NODE_START, x);
	if (err != IKS_OK) return err;
	break;
      }
    }
    self->current = x;
    if (IKS_OPEN == type) break;
  case IKS_CLOSE:
    x = self->current;
    if (NULL == x) {
      err = self->streamHook (self->user_data, IKS_NODE_STOP, NULL);
      if (err != IKS_OK) return err;
      break;
    }
    if (NULL == iks_parent (x)) {
      self->current = NULL;
      if (iks_strcmp (name, "challenge") == 0) {
	send_sasl_challenge(self, x);
	iks_delete (x);
      } else if (iks_strcmp (name, "stream:error") == 0) {
	err = self->streamHook (self->user_data, IKS_NODE_ERROR, x);
	if (err != IKS_OK) return err;
      } else {
	err = self->streamHook (self->user_data, IKS_NODE_NORMAL, x);
	if (err != IKS_OK) return err;
      }
      break;
    }
    self->current = iks_parent (x);
  }
  return IKS_OK;
}

// invoked by parser for character data
static int
cdataHook (ikss_Stream *self, char *cdata, size_t len)
{
  if (self->current) iks_insert_cdata (self->current, cdata, len);
  return IKS_OK;
}

// invoked only upon parser resetting or deletion (iks_parser_reset, iks_parser_delete)
static void
deleteHook (ikss_Stream *self)
{
  if (self->current) {
    iks_delete (self->current);
    self->current = NULL;
  }
}

// public API, inits stream instance; a particular namespace is to be
// used with Jabber, see the constant defined in the public header
// 
// note that interestingly, the mainline version allocates both an
// iksparser and stream_data, but returns the parser rather than
// stream_data -- perhaps the idea is to provide the parsing interface
// regardless of what the data source is, just creating a different
// kind of parser
ikss_Stream *
ikss_Stream_new (char *name_space, void *user_data, iksStreamHook *streamHook, ikss_NotifyFunc* notifyFunc)
{
  ikss_Stream *self;
  ikstack *s;

  s = iks_stack_new (DEFAULT_STREAM_CHUNK_SIZE, 0);
  if (NULL == s) return NULL;

  self = iks_stack_alloc (s, sizeof (ikss_Stream));
  if (!self) {
    iks_stack_delete(s);
    return NULL;
  }
  memset (self, 0, sizeof (ikss_Stream));

  self->s = s;

  // This creates the parser, placing the object into "s" memory. Note
  // that self->prs now takes ownership of "s", and iks_parser_delete
  // will delete it.
  self->prs = iks_sax_extend (s, self, (iksTagHook *)tagHook, (iksCDataHook *)cdataHook, (iksDeleteHook *)deleteHook);
  if (!self->prs) {
    iks_stack_delete(s);
    return NULL;
  }

  self->name_space = name_space;
  self->user_data = user_data;
  self->streamHook = streamHook;
  self->notifyFunc = notifyFunc;

  return self;
}

void ikss_Stream_destroy(ikss_Stream *self)
{
  if (!self) return;

  ikss_Stream_disconnect (self);

  // This is enough if all our memory is managed by the stack. We must
  // have the stack, as otherwise would not have "self". And if we
  // have "self" we also have "prs", which owns "self->s".
  iks_parser_delete(self->prs);
}

// Returns a handle to the user data passed to the ctor, if the client
// should forget it.
void *
ikss_Stream_user_data (ikss_Stream *self)
{
  return self->user_data;
}

iksparser* ikss_Stream_parser (ikss_Stream *self)
{
  return self->prs;
}

ikstack* ikss_Stream_parser_stack (ikss_Stream *self)
{
  return self->s;
}

static int request_recv (ikss_Stream *self)
{
  return ikst_Recv (self->sock, self->recv_buf, NET_IO_BUF_SIZE - 1);
}

static int handle_recv (ikss_Stream *self, int len)
{
  if (len < 0) return IKS_NET_RWERR;
  if (len == 0) return IKS_OK; // a non-receive like this is not strictly illegal
  self->recv_buf[len] = '\0';
#if !defined(__SYMBIAN32__)
  printf("RECV '%s'\n", self->recv_buf); // useful for debugging
#endif /* __SYMBIAN32__ */
  int ret = iks_parse (self->prs, self->recv_buf, len, 0);
  if (ret != IKS_OK) return ret;
  if (!self->sock) {
    /* stream hook called iks_disconnect */
    return IKS_NET_NOCONN;
  }

  return IKS_OK;
}

// ikst_NotifyFunc
static void transportCallback(void *user_data, ikst_event *event)
{
  ikss_Stream *self = (ikss_Stream*)user_data;

  switch (event->event)
    {
      // We will not automatically send any headers or anything, but
      // we do provide methods for doing that. The stream should be
      // ready for messaging right away.
    case ikst_CONNECTED:
      {
	int ret = request_recv(self);
	if (ret) {
	  (self->notifyFunc)(self->user_data, ikss_IKS_ERROR, event->data0);
	} else {
	  (self->notifyFunc)(self->user_data, ikss_CONNECTED, 0);
	}
	break;
      }

      // Really, if the server is behaving nicely, it should send
      // </stream> before EOF, and that should trigger us closing the
      // connection before we get the EOF. But this matters little,
      // getting an EOF error here should lead to the same end result.
    case ikst_EOF:
      // It is not safe for us to delete ourselves or anything, but
      // the client may want to do that right away upon receiving this
      // event.
    case ikst_IKS_ERROR:
    case ikst_PLAT_ERROR:
      {
#if !defined(__SYMBIAN32__)
	if (ikst_PLAT_ERROR == event->event) {
	  printf("transport error: %s (%d)\n", strerror(event->data0),
		 event->data0);
	}
#endif /* __SYMBIAN32__ */
	(self->notifyFunc)(self->user_data, event->event, event->data0);
        break;
      }

    case ikst_WRITE_OK:
      {
        // If there is anything left to send, resume sending,
        // otherwise propagate the event to the client so that it
        // knows it can send more messages.

	self->send_count += event->data0;

	if ((self->send_count < self->send_len) || self->send_more_buf) {
	  int ret = send_more(self);
	  if (ret) {
	    (self->notifyFunc)(self->user_data, ikss_IKS_ERROR, ret);
	  }
	} else {
	  (self->notifyFunc)(self->user_data, ikss_SENT, 0);
	}

        break;
      }

    case ikst_READ_OK:
      {
        // Pass the read data to the parser, and if it was
        // syntactically fine, then put in another recv request.
	int ret = (handle_recv (self, event->data0) ||
		   request_recv (self));
	if (ret) {
	  (self->notifyFunc)(self->user_data, ikss_IKS_ERROR, ret);
	}
        break;
      }

    default:
      {
	assert(0);
        break;
      }
    }
}

int
ikss_Stream_connect (ikss_Stream *self, const char *server, int port, int iapId)
{
  int ret;

  if (!self->recv_buf) {
    self->recv_buf = iks_stack_alloc (self->s, NET_IO_BUF_SIZE);
    if (NULL == self->recv_buf) return IKS_NOMEM;
  }

  // asynchronous, so does not complete immediately; self->sock
  // however gets set here or not, right away, so that we will
  // know if something is outstanding, and have a way of
  // cancelling that something
  ret = ikst_Connect (&self->sock, 
		      server, port, iapId,
		      self, transportCallback);
  if (ret) return ret;

  self->server = server;

  return IKS_OK;
}

// In the mainline version this function took "to" as an argument, but
// are we not always talking to self->server?
int
ikss_Stream_send_header (ikss_Stream *self)
{
  char *msg;
  int len, err;
  const char *to = self->server;

  len = 91 + strlen (self->name_space) + 6 + strlen (to) + 16 + 1;
  msg = iks_malloc (len);
  if (!msg) return IKS_NOMEM;
  // We are XMPP compliant, and hence sending a version number, too.
  // Note that "jabberd" is not compliant in this sense.
  sprintf (msg, "<?xml version='1.0'?>"
	   "<stream:stream xmlns:stream='http://etherx.jabber.org/streams' xmlns='"
	   "%s' to='%s' version='1.0'>", self->name_space, to);
  err = ikss_Stream_send_raw (self, msg);
  if (err) return err;
  return IKS_OK;
}

int
ikss_Stream_send_node (ikss_Stream *self, iks *x)
{
  // Note that iks_string caller is responsible for freeing the
  // returned string.
  char* xmlstr = iks_string (NULL, x);
  if (!xmlstr) return IKS_NOMEM;
  return ikss_Stream_send_raw (self, xmlstr);
}

#define reset_string(x) { if (x) { free(x); (x) = NULL; } }

static void resetSendBuf(ikss_Stream *self)
{
  reset_string(self->send_buf);
  reset_string(self->send_more_buf);
  self->send_len = self->send_count = 0;
}

static int send_more(ikss_Stream *self)
{
  int len = 0;
  if (self->send_buf) {
    len = self->send_len - self->send_count;
  }
  if (len == 0) {
    assert(self->send_more_buf);
    reset_string(self->send_buf);
    self->send_buf = self->send_more_buf;
    self->send_more_buf = NULL;
    len = self->send_count = self->send_len = strlen(self->send_buf);
  }
  assert(len > 0);
  return ikst_Send (self->sock, self->send_buf + self->send_count, len);
}

// Takes ownership of "xmlstr", unconditionally. Do NOT pass in any
// string allocated from an iks_stack, as mallocing is assumed.
int
ikss_Stream_send_raw (ikss_Stream *self, char *xmlstr)
{
#if !defined(__SYMBIAN32__)
  printf("SEND '%s'\n", xmlstr);
#endif /* __SYMBIAN32__ */

  if (ikst_IsSendActive(self->sock)) {
    // Not allowed to do another send, just buffer the data.
    if (self->send_more_buf) {
      char* newbuf = iks_malloc(strlen(self->send_more_buf) + strlen(xmlstr) + 1);
      if (!newbuf) return IKS_NOMEM;
      strcpy(newbuf, self->send_more_buf);
      strcat(newbuf, xmlstr);
      free(xmlstr);
      free(self->send_more_buf);
      self->send_more_buf = newbuf;
    } else {
      self->send_more_buf = xmlstr;
    }
    return 0;
  }

  resetSendBuf(self);

  int len = strlen(xmlstr);
  
  int ret = ikst_Send (self->sock, xmlstr, len);
  if (ret) {
    free(xmlstr);
    return ret;
  } else {
    self->send_buf = xmlstr;
    self->send_len = len;
    self->send_count = 0;
    return IKS_OK;
  }
}

// synchronous
// 
// Resets all session state to default values. Anything related to
// parsing and sending must hence be reset here.
void
ikss_Stream_disconnect (ikss_Stream *self)
{
  ikst_Close(self->sock);
  self->sock = NULL;

  iks_parser_reset (self->prs);

  resetSendBuf(self);
}

/*****  sasl  *****/

// if returns IKS_OK, do not send more before ikss_SENT callback
int
ikss_Stream_start_sasl (ikss_Stream *self, enum ikssasltype type, 
			const char *username, const char *pass)
{
  iks *x;

  x = iks_new ("auth"); // xxx check return?
  iks_insert_attrib (x, "xmlns", IKS_NS_XMPP_SASL); // xxx check return?
  switch (type) {
  case IKS_SASL_PLAIN: {
    int len = iks_strlen (username) + iks_strlen (pass) + 2;
    char *s = iks_malloc (80+len); // xxx check for oom, on error free x and return error code
    char *base64;

    iks_insert_attrib (x, "mechanism", "PLAIN"); // xxx check return?
    sprintf (s, "%c%s%c%s", 0, username, 0, pass);
    base64 = iks_base64_encode (s, len); // xxx check return?
    iks_insert_cdata (x, base64, 0);
    iks_free (base64);
    iks_free (s);
    break;
  }
  case IKS_SASL_DIGEST_MD5: {
    iks_insert_attrib (x, "mechanism", "DIGEST-MD5"); // xxx check return?
    self->auth_username = username;
    self->auth_pass = pass;
    break;
  }
  default:
    iks_delete (x);
    return IKS_NET_NOTSUPP;
  }
  int ret = ikss_Stream_send_node (self, x);
  iks_delete (x); // delete ok since above makes a copy
  if (ret) return ret;
  return IKS_OK;
}
