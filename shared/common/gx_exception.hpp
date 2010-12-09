#ifndef __gx_exception_hpp__
#define __gx_exception_hpp__

#include <exception>

#include <glib.h>

#include "common/gxerror.h"
#include "common/sh_utils.h"

NONSHARABLE_CLASS(GException) : 
  public std::exception
{
 public:
  // Argument may be NULL.
  GException(GError* aError) : iError(aError) {}

  virtual ~GException() throw () 
  {
    gx_error_free(iError);
  }

  virtual const char *what() const throw ()
  {
    if (!iError)
      return "out of memory error";
    return iError->message;
  }

 private:
  GError *iError; // owned
};

#define gx_throw(_error) throw GException(_error)

#define gx_throw_new(f...) gx_throw(gx_error_new(f))

#endif /* __gx_exception_hpp__ */

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
