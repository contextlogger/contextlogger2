// The local server interface is only implemented for Symbian, using
// the Symbian client/server framework. On other platforms these
// functions do nothing.

#include "local_server.h"

#ifdef __EPOC32__
#include "epoc-cliapi-server.hpp"
#endif

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/platform_error.h"

// I have yet to see documentation on how to stop a server other than
// by destroying the object. So that is the hardcore solution that we
// use here to implement "start" and "stop" for this object.

struct _LocalServer {
#ifdef __EPOC32__
  CCliapiServer* iServer;
#endif
};

extern "C" LocalServer *LocalServer_new	(GError ** error)
{
#ifdef __EPOC32__
  LocalServer* self = (LocalServer*)g_malloc0(sizeof(LocalServer));
  return self;
#else
  return (LocalServer*)1; // dummy
#endif
}

extern "C" gboolean 	LocalServer_start	(LocalServer * self,
						 GError ** error)
{
#ifdef __EPOC32__
  if (!self->iServer) {
    CCliapiServer* server = NULL;
    TRAPD(errCode, server = CCliapiServer::NewL());
    if (errCode) {
      if (error)
	*error = g_error_new(domain_symbian, errCode, "Symbian client/server init failure: %s (%d)", plat_error_strerror(errCode), errCode);
      return NULL;
    }

    errCode = server->Start();
    if (errCode) {
      delete server;
      if (error)
	*error = g_error_new(domain_symbian, errCode, "Symbian client/server start failure: %s (%d)", plat_error_strerror(errCode), errCode);
      return NULL;
    }

    self->iServer = server;
  }
#endif
  return TRUE;
}

extern "C" void 	LocalServer_stop	(LocalServer * self)
{
#ifdef __EPOC32__
  delete self->iServer;
  self->iServer = NULL;
#endif
}

extern "C" void	LocalServer_destroy	(LocalServer * self)
{
#ifdef __EPOC32__
  if (self) {
    delete self->iServer;
    g_free(self);
  }
#endif
}

/**

local_server.cpp

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
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
