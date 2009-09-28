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

#include "ikst_transport.h"

#include <ev.h>

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h> // gethostbyname
#include <netinet/in.h>
#include <stddef.h> // offsetof macro
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

typedef int boolean;
#define true 1
#define false 0

// This object must contain all the state that we require, as the
// iksemel API does not allow for storing anything in the transport,
// say, or rather the transport handle is not available to its
// functions.
typedef struct {
  ev_io write_watcher;
  ev_io read_watcher;
  int fd;
  struct sockaddr_in servAddr;
  ikst_NotifyFunc *notify_func;
  void *notify_data; // not owned
  ikst_event write_event;
  ikst_event read_event;
  boolean is_connect_pending;
  boolean is_read_pending;
  boolean is_write_pending;
  const char* write_data; // not owned
  int write_len;
  char* read_buf; // not owned
  int read_max;
} Transport;

#define Transport_cast(p,w) ((Transport*)(((char*)p) - offsetof(Transport,w)))

static void handle_writable_event(Transport* self)
{
  if (self->is_connect_pending) {
    self->is_connect_pending = false;
    self->write_event.event = ikst_CONNECTED;
    (*(self->notify_func))(self->notify_data, &self->write_event);
  } else if (self->is_write_pending) {
    ssize_t retSize;
    for (;;) {
    retry:
      retSize = write(self->fd, self->write_data, self->write_len);
      if (retSize != -1) break;
      switch (errno)
        {
        case EINTR:
          {
            goto retry;
          }
        case EAGAIN:
          {
            // Not writable yet, after all, so wait for next writable
            // event.
	    return;
          }
        default: // Any other error is non-recoverable.
          {
	    self->is_write_pending = false;
	    self->write_event.event = ikst_PLAT_ERROR;
	    self->write_event.data0 = errno;
	    (*(self->notify_func))(self->notify_data, &self->write_event);
	    return;
          }
        }
    }
    self->is_write_pending = false;
    self->write_event.event = ikst_WRITE_OK;
    //printf("wrote %d bytes\n", retSize);
    self->write_event.data0 = retSize; // should be non-zero
    (*(self->notify_func))(self->notify_data, &self->write_event);
  } else {
    ev_io_stop(EV_DEFAULT, &self->write_watcher);
  }
}

static void write_watcher_cb(EV_P_ ev_io* w, int revents)
{
  //printf("write_watcher_cb\n");
  Transport* self = Transport_cast(w, write_watcher);
  handle_writable_event(self);
}

static void handle_readable_event(Transport* self)
{
  if (self->is_read_pending) {
    ssize_t retSize;
    for (;;) {
    retry:
      retSize = recv(self->fd, self->read_buf, self->read_max, 0);

      if (retSize == 0) { // orderly shutdown, i.e. EOF
	self->is_read_pending = false;
	self->read_event.event = ikst_EOF;
	self->read_event.data0 = 0;
	(*(self->notify_func))(self->notify_data, &self->read_event);
	return;
      }

      if (retSize != -1) break; // no error

      switch (errno)
        {
        case EINTR:
          {
            goto retry;
          }
        case EAGAIN:
          {
            // Not writable yet, after all, so wait for next writable
            // event.
	    return;
          }
        default: // Any other error is non-recoverable.
          {
	    self->is_read_pending = false;
	    self->read_event.event = ikst_PLAT_ERROR;
	    self->read_event.data0 = errno;
	    (*(self->notify_func))(self->notify_data, &self->read_event);
	    return;
          }
        }
    }
    self->is_read_pending = false;
    self->read_event.event = ikst_READ_OK;
    self->read_event.data0 = retSize; // is non-zero
    (*(self->notify_func))(self->notify_data, &self->read_event);
  } else {
    ev_io_stop(EV_DEFAULT, &self->read_watcher);
  }
}

static void read_watcher_cb(EV_P_ ev_io* w, int revents)
{
  //printf("read_watcher_cb\n");
  Transport* self = Transport_cast(w, read_watcher);
  handle_readable_event(self);
}

void ikst_Close(void *socket)
{
  if (socket) {
    Transport* self = (Transport*)socket;
    ev_io_stop(EV_DEFAULT, &self->write_watcher);
    ev_io_stop(EV_DEFAULT, &self->read_watcher);
    close(self->fd);
    free(self);
  }
}

int ikst_Send(void *socket, const char *data, size_t len)
{
  Transport* self = (Transport*)socket;

  assert(!self->is_write_pending);
  self->is_write_pending = true;

  self->write_data = data;
  self->write_len = len;

  ev_io_start(EV_DEFAULT, &self->write_watcher);

  return IKS_OK;
}

int ikst_IsSendActive(void *socket)
{
  Transport* self = (Transport*)socket;
  return self->is_write_pending;
}

int ikst_Recv(void *socket, char *buffer, size_t buf_len)
{
  Transport* self = (Transport*)socket;

  assert(!self->is_read_pending);
  self->is_read_pending = true;

  self->read_buf = buffer;
  self->read_max = buf_len;

  ev_io_start(EV_DEFAULT, &self->read_watcher);

  return IKS_OK;
}

// On error returns -1 and sets errno. Otherwise returns 0.
static int set_nonblocking(int fd)
{
  int mode = fcntl(fd, F_GETFL, 0);
  if (mode == -1) return -1;
  int ret = fcntl(fd, F_SETFL, mode|O_NONBLOCK);
  if (ret == -1) return -1;
  return 0;
}

int ikst_Connect(void **socketptr, 
		 const char *server, int port, int iapId,
		 void *notify_data, ikst_NotifyFunc *notify_func)
{
  (void)iapId; // not used

  Transport* self = (Transport*)calloc(1, sizeof(Transport));
  if (!self) {
    return IKS_NOMEM;
  }

  // It seems there really is no POSIX or Linux API for doing
  // asynchronous DNS lookups. To do async queries we would have to
  // cook up something with gethostbyname_r and pthread, or find a
  // suitable third-party library (there seem to be no suitably
  // licensed ones in Debian either), but since this is not production
  // code we can just use a synchronous call. But in the Symbian
  // variant we must do the lookups asynchronously.
  struct hostent* he = gethostbyname(server); // 'he' not owned, not freed by us
  if (!he) return IKS_NET_NODNS;

  // Do remember to ignore SIGPIPE unless you want your program
  // aborted when a read fails due to a closed connection.
  self->fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (self->fd == -1) {
    free(self);
    return IKS_NET_NOSOCK;
  }

  // This is recommended with libev IO watchers.
  if (set_nonblocking(self->fd) != 0) {
    close(self->fd);
    free(self);
    return IKS_NET_NOSOCK;
  }

  // Note that calloc already zeroed servAddr.
  self->servAddr.sin_family = AF_INET;
  memcpy(&self->servAddr.sin_addr, he->h_addr, he->h_length);
  self->servAddr.sin_port = htons(port);

  ev_init(&self->write_watcher, &write_watcher_cb);
  ev_init(&self->read_watcher, &read_watcher_cb);

  ev_io_set(&self->write_watcher, self->fd, EV_WRITE);
  ev_io_set(&self->read_watcher, self->fd, EV_READ);

  ev_io_start(EV_DEFAULT, &self->write_watcher);
  ev_io_start(EV_DEFAULT, &self->read_watcher);

  // In practice we should always be getting EINPROGRESS immediately,
  // unless things have somehow been set up wrong. The socket should
  // become writable once connected.
  if ((connect(self->fd, (struct sockaddr*)&self->servAddr, 
	       sizeof(self->servAddr)) == -1) &&
      (errno != EINPROGRESS)) {
    ikst_Close(self);
    return IKS_NET_NOCONN;
  }

  self->is_connect_pending = true;

  self->notify_func = notify_func;
  self->notify_data = notify_data;

  *socketptr = self;

  return IKS_OK;
}
