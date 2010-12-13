#ifndef __ut_exceptions_hpp__
#define __ut_exceptions_hpp__

#include "common/gx_exception.hpp"
#include "common/sh_utils.h"

#include <stdlib.h>

#include <exception>

#include <glib.h>

#if defined(QT_CORE_LIB)
#include <QString>
#endif

//xxx do want functions that that strdup the text, and also allows for formatting and optionally position info

NONSHARABLE_CLASS(CstrLitException) :
  public std::exception
{
 private:
  const char* iText; // not owned
 public:
  CstrLitException(const char* aText) throw() : iText(aText) {}
  virtual const char* what() const throw() { return iText; }
};

NONSHARABLE_CLASS(CstrExceptionFree) :
  public std::exception
{
 private:
  char* iText; // owned
 public:
  CstrExceptionFree(char* aText) throw() : iText(aText) {}
  virtual ~CstrExceptionFree() throw() { if (iText) free(iText); }
  virtual const char* what() const throw() { return iText; }
};

// Note that free and g_free do not necessarily rely on the same
// memory allocation scheme. You may use g_mem_is_system_malloc() to
// find out whether this is the case.
NONSHARABLE_CLASS(CstrExceptionGfree) :
  public std::exception
{
 private:
  gchar* iText; // owned
 public:
  CstrExceptionGfree(gchar* aText) throw() : iText(aText) {}
  virtual ~CstrExceptionGfree() throw() { g_free(iText); }
  virtual const char* what() const throw() { return iText; }
};

#define throw_cstr_unless(_v, _msg) \
  if (!(_v)) { throw CstrLitException(_msg); }

#endif /* __ut_exceptions_hpp__ */

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
