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

  // This structure may be used to store data that the notifications
  // point to, for any datatype as desired. Note that no automatic
  // allocation or freeing is performed; this memory is simply
  // zero-initialized.
  typedef struct {
    gboolean flightmode;
    gboolean uploads_allowed;
    gboolean netpos_allowed;
  } bb_Board;

  bb_Board* bb_Blackboard_board(bb_Blackboard* self);

  enum bb_DataType {
    bb_dt_flightmode,      /* gboolean */
    bb_dt_network_info,    /* CTelephony::TNetworkInfoV1 */
    bb_dt_cell_id,         /* CTelephony::TNetworkInfoV1 */
    bb_dt_uploads_allowed, /* gboolean */
    bb_dt_netpos_allowed   /* gboolean, by value */
  };

  typedef void (*bb_Func)(bb_Blackboard* self, enum bb_DataType dt,
			  gpointer data, int len, gpointer arg);

  typedef struct {
    bb_Func changed;
    gpointer arg;
  } bb_Closure;

  // The value of the closure is used as a unique ID for the
  // registrant. This function does not check for duplicates.
  gboolean bb_Blackboard_register(bb_Blackboard* self,
				  enum bb_DataType dt,
				  bb_Closure cb,
				  GError** error);

  // Unregisters all entries for the specified closure.
  // Does nothing if not registered.
  void bb_Blackboard_unregister(bb_Blackboard* self,
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

// Symbian C++ convenience API for observing.
#if defined(__cplusplus) && defined(__SYMBIAN32__)
namespace bb {
  class RHandle;

  class MObserver
  {
  public:
    virtual void BbChangedL(bb::RHandle* self, enum bb_DataType dt,
			    gpointer data, int len) = 0;

    // Override if you do not consider leave in BbChangedL fatal.
    virtual void BbLeave(TInt errCode);
  };

  // Only one property may be observed per handle.
  class RHandle
  {
  private:
    bb_Blackboard* iBoard;
    bb_Closure iClosure;
    MObserver* iObserver;

  public:
    RHandle();

    ~RHandle();

    void Register(bb_Blackboard* aBoard,
		  enum bb_DataType dt, 
		  MObserver* observer);
    
    void Unregister();

    MObserver* Observer() const { return iObserver; }
  };
};
#endif // end Symbian C++

#endif /* __bb_blackboard_h__ */

/**

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
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
