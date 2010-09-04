#ifndef __bb_blackboard_h__
#define __bb_blackboard_h__

// This component is really just a glorified callback mechanism, a way
// for internal components to invoke callbacks without the target
// components actually having to expose an API for that purpose. Many
// frameworks (such as Qt), provide such a mechanism. An added benefit
// of sending callbacks via this component is that the communicating
// parties do not need to know who they're talking to, leading to
// decoupling.

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _bb_Blackboard bb_Blackboard;

  bb_Blackboard* bb_Blackboard_new(GError** error);

  void bb_Blackboard_destroy(bb_Blackboard* self);

  enum bb_DataType {
    bb_dt_uploads_allowed, /* gboolean */
    bb_dt_network_info     /* CTelephony::TNetworkInfoV1 */
  };

  typedef void (*bb_Func)(bb_Blackboard* self, enum bb_DataType dt,
			  gpointer data, int len, gpointer arg);

  typedef struct {
    bb_Func changed;
    gpointer arg;
  } bb_Closure;

  // The value of the "changed" function is used as a unique ID for
  // the registrant.
  gboolean bb_Blackboard_register(bb_Blackboard* self,
				  enum bb_DataType dt,
				  bb_Closure cb,
				  GError** error);

  // Deregisters all entries for the specified closure.
  // Does nothing if not registered.
  void bb_Blackboard_deregister(bb_Blackboard* self,
				bb_Closure cb);

  // Note that the delivery is synchronous, and usual caveats apply.
  // However, the good thing is that the data you pass (probably) will
  // not have time to change by the time the event has been received
  // by each observer. We suggest you always pass a pointer to the
  // data as "data"; the receiver can make a copy as required. If you
  // want to pass a value directly, at least consider using
  // GINT_TO_POINTER and GPOINTER_TO_INT to make this safe. The "len"
  // argument is not required for data with fixed, known size.
  void bb_Blackboard_notify(bb_Blackboard* self,
			    enum bb_DataType dt,
			    gpointer data, int len);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __bb_blackboard_h__ */
