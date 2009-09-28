/* iksemel (XML parser for Jabber)
** Copyright (C) 2000-2007 Gurer Ozen
** This code is free software; you can redistribute it and/or
** modify it under the terms of GNU Lesser General Public License.
*/

#ifndef IKSEMEL_H
#define IKSEMEL_H 1

#include <stdlib.h> // size_t

#ifdef __cplusplus
extern "C" {
#endif

/*****  object stack  *****/

struct ikstack_struct;
typedef struct ikstack_struct ikstack;

ikstack *iks_stack_new (size_t meta_chunk, size_t data_chunk);
void *iks_stack_alloc (ikstack *s, size_t size);
char *iks_stack_strdup (ikstack *s, const char *src, size_t len);
char *iks_stack_strcat (ikstack *s, char *old, size_t old_len, const char *src, size_t src_len);
void iks_stack_stat (ikstack *s, size_t *allocated, size_t *used);
void iks_stack_delete (ikstack *s);

/*****  utilities  *****/

void *iks_malloc (size_t size);
void iks_free (void *ptr);
void iks_set_mem_funcs (void *(*malloc_func)(size_t size), void (*free_func)(void *ptr));

char *iks_strdup (const char *src);
char *iks_strcat (char *dest, const char *src);
int iks_strcmp (const char *a, const char *b);
int iks_strcasecmp (const char *a, const char *b);
int iks_strncmp (const char *a, const char *b, size_t n);
int iks_strncasecmp (const char *a, const char *b, size_t n);
size_t iks_strlen (const char *src);
char *iks_escape (ikstack *s, char *src, size_t len);
char *iks_unescape (ikstack *s, char *src, size_t len);

/*****  dom tree  *****/

enum ikstype {
	IKS_NONE = 0,
	IKS_TAG,
	IKS_ATTRIBUTE,
	IKS_CDATA
};

struct iks_struct;
typedef struct iks_struct iks;

iks *iks_new (const char *name);
iks *iks_new_within (const char *name, ikstack *s);
iks *iks_insert (iks *x, const char *name);
iks *iks_insert_cdata (iks *x, const char *data, size_t len);
iks *iks_insert_attrib (iks *x, const char *name, const char *value);
iks *iks_insert_node (iks *x, iks *y);
iks *iks_append (iks *x, const char *name);
iks *iks_prepend (iks *x, const char *name);
iks *iks_append_cdata (iks *x, const char *data, size_t len);
iks *iks_prepend_cdata (iks *x, const char *data, size_t len);
void iks_hide (iks *x);
void iks_delete (iks *x);
iks *iks_next (iks *x);
iks *iks_next_tag (iks *x);
iks *iks_prev (iks *x);
iks *iks_prev_tag (iks *x);
iks *iks_parent (iks *x);
iks *iks_root (iks *x);
iks *iks_child (iks *x);
iks *iks_first_tag (iks *x);
iks *iks_attrib (iks *x);
iks *iks_find (iks *x, const char *name);
char *iks_find_cdata (iks *x, const char *name);
char *iks_find_attrib (iks *x, const char *name);
iks *iks_find_with_attrib (iks *x, const char *tagname, const char *attrname, const char *value);
ikstack *iks_stack (iks *x);
enum ikstype iks_type (iks *x);
char *iks_name (iks *x);
char *iks_cdata (iks *x);
size_t iks_cdata_size (iks *x);
int iks_has_children (iks *x);
int iks_has_attribs (iks *x);
char *iks_string (ikstack *s, iks *x);
iks *iks_copy (iks *x);
iks *iks_copy_within (iks *x, ikstack *s);

/*****  sax parser  *****/

enum ikserror {
	IKS_OK = 0,
	IKS_NOMEM,
	IKS_BADXML,
	IKS_HOOK
};

enum ikstagtype {
	IKS_OPEN,
	IKS_CLOSE,
	IKS_SINGLE
};

typedef int (iksTagHook)(void *user_data, char *name, char **atts, int type);
typedef int (iksCDataHook)(void *user_data, char *data, size_t len);
typedef void (iksDeleteHook)(void *user_data);

struct iksparser_struct;
typedef struct iksparser_struct  iksparser;

iksparser *iks_sax_new (void *user_data, iksTagHook *tagHook, iksCDataHook *cdataHook);
iksparser *iks_sax_extend (ikstack *s, void *user_data, iksTagHook *tagHook, iksCDataHook *cdataHook, iksDeleteHook *deleteHook);
ikstack *iks_parser_stack (iksparser *prs);
void *iks_user_data (iksparser *prs);
unsigned long iks_nr_bytes (iksparser *prs);
unsigned long iks_nr_lines (iksparser *prs);
int iks_parse (iksparser *prs, const char *data, size_t len, int finish);
void iks_parser_reset (iksparser *prs);
void iks_parser_delete (iksparser *prs);

/*****  dom parser  *****/

enum iksfileerror {
	IKS_FILE_NOFILE = 4,
	IKS_FILE_NOACCESS,
	IKS_FILE_RWERR
};

iksparser *iks_dom_new (iks **iksptr);
void iks_set_size_hint (iksparser *prs, size_t approx_size);
iks *iks_tree (const char *xml_str, size_t len, int *err);
int iks_load (const char *fname, iks **xptr);
int iks_save (const char *fname, iks *x);

/*****  stream parser  *****/

// these may be used to interpret transport related error codes
enum iksneterror {
	IKS_NET_NODNS = -400,
	IKS_NET_NOSOCK,
	IKS_NET_NOCONN,
	IKS_NET_RWERR,
	IKS_NET_NOTSUPP,
	IKS_NET_TLSFAIL,
	IKS_NET_DROPPED,
	IKS_NET_UNKNOWN
};

#define IKS_JABBER_PORT 5222

enum iksnodetype {
	IKS_NODE_START,
	IKS_NODE_NORMAL,
	IKS_NODE_ERROR,
	IKS_NODE_STOP
};

enum ikssasltype {
	IKS_SASL_PLAIN,
	IKS_SASL_DIGEST_MD5
};

  // This is used to return incoming tags only.
typedef int (iksStreamHook)(void *user_data, int type, iks *node);

  // The ikss_SENT event is meant for flow control, so that one can
  // avoid filling the memory with buffered messages.
  enum ikss_NotifyType {
    ikss_CONNECTED, // 'data' is 0
    ikss_EOF, // 'data' is 0
    ikss_IKS_ERROR, // 'data' is Iksemel specific error code
    ikss_PLAT_ERROR, // 'data' is a platform specific error code
    ikss_SENT // tag sent, ready for more, 'data' is 0
  };

  // This is used to notify the user of major stream events, such as
  // connection establishment and breakage. The "type" value is one of
  // ikss_NotifyType. The "data" value is event type specific.
  typedef int (ikss_NotifyFunc)(void *user_data, int type, int data);

  typedef struct _ikss_Stream ikss_Stream;

  ikss_Stream *ikss_Stream_new (char *name_space, void *user_data, iksStreamHook *streamHook, ikss_NotifyFunc* notifyFunc);
  void ikss_Stream_destroy(ikss_Stream *stream);

  void *ikss_Stream_user_data (ikss_Stream *stream);

  iksparser* ikss_Stream_parser (ikss_Stream *stream);
  ikstack* ikss_Stream_parser_stack (ikss_Stream *stream);

  int ikss_Stream_connect (ikss_Stream *stream, const char *server, int port, int iapId);

  // Breaks connection and resets parser state.
  void ikss_Stream_disconnect (ikss_Stream *stream);

  int ikss_Stream_send_header (ikss_Stream *stream);

  int ikss_Stream_start_sasl (ikss_Stream *stream, enum ikssasltype type, const char *username, const char *pass);

  // Sends XML node. Caller must free "x".
  int ikss_Stream_send_node (ikss_Stream *stream, iks *x);

  // Sends XML string. Takes ownership of "xmlstr", no matter what.
  int ikss_Stream_send_raw (ikss_Stream *stream, char *xmlstr);

/*****  jabber  *****/

#define IKS_NS_CLIENT     "jabber:client"
#define IKS_NS_SERVER     "jabber:server"
#define IKS_NS_AUTH       "jabber:iq:auth"
#define IKS_NS_AUTH_0K    "jabber:iq:auth:0k"
#define IKS_NS_REGISTER   "jabber:iq:register"
#define IKS_NS_ROSTER     "jabber:iq:roster"
#define IKS_NS_XROSTER	"jabber:x:roster"
#define IKS_NS_OFFLINE    "jabber:x:offline"
#define IKS_NS_AGENT      "jabber:iq:agent"
#define IKS_NS_AGENTS     "jabber:iq:agents"
#define IKS_NS_BROWSE     "jabber:iq:browse"
#define IKS_NS_CONFERENCE "jabber:iq:conference"
#define IKS_NS_DELAY      "jabber:x:delay"
#define IKS_NS_VERSION    "jabber:iq:version"
#define IKS_NS_TIME       "jabber:iq:time"
#define IKS_NS_VCARD      "vcard-temp"
#define IKS_NS_PRIVATE    "jabber:iq:private"
#define IKS_NS_SEARCH     "jabber:iq:search"
#define IKS_NS_OOB        "jabber:iq:oob"
#define IKS_NS_XOOB       "jabber:x:oob"
#define IKS_NS_ADMIN      "jabber:iq:admin"
#define IKS_NS_FILTER     "jabber:iq:filter"
#define IKS_NS_GATEWAY    "jabber:iq:gateway"
#define IKS_NS_LAST       "jabber:iq:last"
#define IKS_NS_SIGNED     "jabber:x:signed"
#define IKS_NS_ENCRYPTED  "jabber:x:encrypted"
#define IKS_NS_ENVELOPE   "jabber:x:envelope"
#define IKS_NS_EVENT      "jabber:x:event"
#define IKS_NS_EXPIRE     "jabber:x:expire"
#define IKS_NS_XHTML      "http://www.w3.org/1999/xhtml"
#define IKS_NS_XMPP_SASL  "urn:ietf:params:xml:ns:xmpp-sasl"
#define IKS_NS_XMPP_BIND  "urn:ietf:params:xml:ns:xmpp-bind"
#define IKS_NS_XMPP_SESSION  "urn:ietf:params:xml:ns:xmpp-session"

#define IKS_ID_USER 1
#define IKS_ID_SERVER 2
#define IKS_ID_RESOURCE 4
#define IKS_ID_PARTIAL IKS_ID_USER | IKS_ID_SERVER
#define IKS_ID_FULL IKS_ID_USER | IKS_ID_SERVER | IKS_ID_RESOURCE

#define IKS_STREAM_STARTTLS                   1
#define IKS_STREAM_SESSION                    2
#define IKS_STREAM_BIND                       4
#define IKS_STREAM_SASL_PLAIN                 8
#define IKS_STREAM_SASL_MD5                  16

typedef struct iksid_struct {
	char *user;
	char *server;
	char *resource;
	char *partial;
	char *full;
} iksid;

iksid *iks_id_new (ikstack *s, const char *jid);
int iks_id_cmp (iksid *a, iksid *b, int parts);

enum ikspaktype {
	IKS_PAK_NONE = 0,
	IKS_PAK_MESSAGE,
	IKS_PAK_PRESENCE,
	IKS_PAK_IQ,
	IKS_PAK_S10N
};

enum iksubtype {
	IKS_TYPE_NONE = 0,
	IKS_TYPE_ERROR,

	IKS_TYPE_CHAT,
	IKS_TYPE_GROUPCHAT,
	IKS_TYPE_HEADLINE,

	IKS_TYPE_GET,
	IKS_TYPE_SET,
	IKS_TYPE_RESULT,

	IKS_TYPE_SUBSCRIBE,
	IKS_TYPE_SUBSCRIBED,
	IKS_TYPE_UNSUBSCRIBE,
	IKS_TYPE_UNSUBSCRIBED,
	IKS_TYPE_PROBE,
	IKS_TYPE_AVAILABLE,
	IKS_TYPE_UNAVAILABLE
};

enum ikshowtype {
	IKS_SHOW_UNAVAILABLE = 0,
	IKS_SHOW_AVAILABLE,
	IKS_SHOW_CHAT,
	IKS_SHOW_AWAY,
	IKS_SHOW_XA,
	IKS_SHOW_DND
};

typedef struct ikspak_struct {
	iks *x;
	iksid *from;
	iks *query;
	char *ns;
	char *id;
	enum ikspaktype type;
	enum iksubtype subtype;
	enum ikshowtype show;
} ikspak;

ikspak *iks_packet (iks *x);

iks *iks_make_auth (iksid *id, const char *pass, const char *sid);
iks *iks_make_msg (enum iksubtype type, const char *to, const char *body);
iks *iks_make_s10n (enum iksubtype type, const char *to, const char *msg);
iks *iks_make_pres (enum ikshowtype show, const char *status);
iks *iks_make_iq (enum iksubtype type, const char *xmlns);
iks *iks_make_resource_bind(iksid *id);
iks *iks_make_session(void);
int iks_stream_features(iks *x);

/*****  jabber packet filter  *****/

#define IKS_RULE_DONE 0
#define IKS_RULE_ID 1
#define IKS_RULE_TYPE 2
#define IKS_RULE_SUBTYPE 4
#define IKS_RULE_FROM 8
#define IKS_RULE_FROM_PARTIAL 16
#define IKS_RULE_NS 32

enum iksfilterret {
	IKS_FILTER_PASS,
	IKS_FILTER_EAT
};

typedef int (iksFilterHook)(void *user_data, ikspak *pak);

struct iksfilter_struct;
typedef struct iksfilter_struct iksfilter;
struct iksrule_struct;
typedef struct iksrule_struct iksrule;

iksfilter *iks_filter_new (void);
iksrule *iks_filter_add_rule (iksfilter *f, iksFilterHook *filterHook, void *user_data, ...);
void iks_filter_remove_rule (iksfilter *f, iksrule *rule);
void iks_filter_remove_hook (iksfilter *f, iksFilterHook *filterHook);
void iks_filter_packet (iksfilter *f, ikspak *pak);
void iks_filter_delete (iksfilter *f);

/*****  sha1  *****/

struct iksha_struct;
typedef struct iksha_struct iksha;

iksha *iks_sha_new (void);
void iks_sha_reset (iksha *sha);
void iks_sha_hash (iksha *sha, const unsigned char *data, size_t len, int finish);
void iks_sha_print (iksha *sha, char *hash);
void iks_sha_delete (iksha *sha);
void iks_sha (const char *data, char *hash);

/*****  md5  *****/

struct ikmd5_struct;
typedef struct iksmd5_struct iksmd5;

iksmd5 *iks_md5_new(void);
void iks_md5_reset(iksmd5 *md5);
void iks_md5_hash(iksmd5 *md5, const unsigned char *data, size_t slen, int finish);
void iks_md5_delete(iksmd5 *md5);
void iks_md5_print(iksmd5 *md5, char *buf);
void iks_md5_digest(iksmd5 *md5, unsigned char *digest);
void iks_md5(const char *data, char *buf);

/*****  base64  *****/

char *iks_base64_decode(const char *buf);
char *iks_base64_encode(const char *buf, int len);

#ifdef __cplusplus
}
#endif

#endif  /* IKSEMEL_H */
