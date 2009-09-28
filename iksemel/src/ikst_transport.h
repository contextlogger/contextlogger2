//
// Copyright 2009 Helsinki Institute for Information Technology (HIIT)
// and the authors.	 All rights reserved.
//
// Authors: Tero Hasu <tero.hasu@hut.fi>
//

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation files
// (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Alternatively, licensed under LGPL.

#ifndef __ikst_transport_h__
#define __ikst_transport_h__

// We are going to heavy-handedly customize the "ikstransport" API, so
// it will no longer be the standard version. Hence we are hiding it
// in a non-public header. It is only used internally anyway. All the
// names have been changed as well.
// 
// As with the original "ikstransport", we implement a message pipe
// type API.
// 
// (1) A select based implementation of reads/writes would probably
// first include the socket in the next select to see if
// readable/writeable, and then do the actual read/write in the select
// handler, just before the user callback. (2) An implementation based
// on proper async sockets can do the read/write request, and then do
// the user callback in the system callback. (3) With a socket
// monitoring system one would probably keep track of whether the
// socket is readable and writable, as one receives notifications, and
// then do reads/writes in the request functions when
// readable/writable, or in the notification handlers otherwise;
// callbacks should be done via the event loop in the former case
// also.

#include "iksemel.h"

#ifdef __cplusplus
extern "C" {
#endif

  // "ikst" == iks transport

  enum ikst_events {

    // A connection was established.
    // "data0" is 0.
    ikst_CONNECTED = ikss_CONNECTED,

    // Read or write or connect error. "data0" is the error code. Note
    // the distinction between platform-specific codes and Iksemel
    // specific codes in callbacks. All the functions return
    // Iksemel-specific codes, however.
    ikst_IKS_ERROR = ikss_IKS_ERROR,
    ikst_PLAT_ERROR = ikss_PLAT_ERROR,

    // EOF error.
    // "data0" is 0.
    ikst_EOF = ikss_EOF,

    // Wrote something, ready for writing more.
    // Number of bytes written in "data0".
    ikst_WRITE_OK = 100,

    // Read something, ready for reading more.
    // Number of bytes read in "data0".
    ikst_READ_OK

  };

  typedef struct ikst_event_struct {
    int event; // event type (see ikst_events)
    int data0; // type-specific value
    int data1; // unused
  } ikst_event;

  typedef void (ikst_NotifyFunc)(void *user_data, ikst_event *event);

  // We are not going to be implementing more than one of these per
  // target, so we do not want this ikstransport struct indirection.

  // This also frees "socket", so do not call this more than once.
  // Does nothing if "socket" is NULL.
  // 
  // You _should_ call this method after receiving an ikst_ERROR, as
  // there is nothing else then that you can do.
  void ikst_Close(void *socket);

  // You may not have more than one outstanding write, wait for
  // ikst_WRITE_OK before calling this again.
  // 
  // The data must persist until the request completes.
  // 
  // This function sends at most "len" bytes of "data". The number of
  // bytes sent is returned as callback "data0" parameter, or
  // otherwise an ikst_ERROR event is produced with an error code in
  // "data0". If this function fails immediately, it can also directly
  // return an error value, otherwise it returns 0. See enum
  // iksneterror for some of the possible error return values.
  int ikst_Send(void *socket, const char *data, size_t len);

  int ikst_IsSendActive(void *socket);

  // You may not have more than one outstanding read, wait for
  // ikst_READ_OK before calling this again.
  // 
  // The buffer must exist until the request completes.
  // 
  // buf_len is the maximum number of bytes to read.
  // 
  // This function reads at most "buf_len" bytes of data into
  // "buffer". The number of bytes read is returned as callback
  // "data0" parameter, or otherwise an ikst_ERROR event is produced
  // with an error code in "data0". If this function fails
  // immediately, it can also directly return an error value,
  // otherwise it returns 0. See enum iksneterror for some of the
  // possible error return values.
  // 
  // In the event of there being nothing but EOF left to read, the
  // reported error is number 0 (in data0 field). Zero is returned if
  // there is nothing to read but the socket is still readable,
  // provided that the underlying socket is non-blocking.
  int ikst_Recv(void *socket, char *buffer, size_t buf_len);

  // See enum iksneterror for some of the possible return values. An
  // immediate error is reported by returning an error code from this
  // function; otherwise this function returns 0. Normally this
  // function results in an ikst_CONNECTED callback, indicating that
  // the socket is ready for reading and writing.
  // 
  // Caller takes ownership of *socketptr if this function returns 0
  // (IKS_OK).
  // 
  // The mainline Iksemel API has a "server_name" parameter, and we do
  // not know what it is, but it seems it is always set to the same
  // value as "server" in the iksemel code. We just leave it out.
  // 
  // The mainline Iksemel API also passes iksparser, but cannot see
  // why.
  // 
  // The mainline Iksemel API does not take "iapId"; it is meant for
  // specifying the access point to use, and may not apply to all
  // platforms.
  // 
  // The use of void* socket is for flexibility, and one can for
  // instance cast to a C++ object, even, since we are not "C" naming
  // the structure type.
  int ikst_Connect(void **socketptr, 
		   const char *server, int port, int iapId,
		   void *notify_data, ikst_NotifyFunc *notify_func);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __ikst_transport_h__ */
