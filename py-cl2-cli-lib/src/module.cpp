// 
// Copyright 2005-2009 Helsinki Institute for Information Technology
// (HIIT) and the authors. All rights reserved.
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

#include "py_session.hpp"
#include <symbian_python_ext_util.h>
#include "sconfig.hrh"
#include "local_epoc_py_utils.hpp"

// -------------------------------------------------------------------
// Python module...

static PyMethodDef const _ft__cl2_client[] = {
  {"Session", (PyCFunction)new_Session, METH_NOARGS, NULL},
  {NULL}};

DL_EXPORT(void) __INIT_FUNC_NAME__()
{
  PyObject *pyMod = Py_InitModule(__MODULE_NAME__, const_cast<PyMethodDef *>((&_ft__cl2_client[0])));
  if ((pyMod == NULL)) {
    return;
  }
  if (def_Session() < 0) return;
}

// -------------------------------------------------------------------
// DLL entry point...

#ifndef EKA2
GLDEF_C TInt E32Dll(TDllReason)
{
  return KErrNone;
}
#endif /*EKA2*/
