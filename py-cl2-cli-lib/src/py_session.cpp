//
// Copyright 2009 Helsinki Institute for Information Technology (HIIT)
// and the authors.  All rights reserved.
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
#include "local_epoc_py_utils.hpp"
#include "sconfig.hrh"

#include <cl2cli.hpp>

typedef struct
{
  PyObject_VAR_HEAD;
  RCl2Session iSession;
  TBool iSessionOpen;
} obj_Session;

static void destroyAnySession(obj_Session* self)
{
  if (self->iSessionOpen) {
    self->iSession.Close();
    self->iSessionOpen = EFalse;
  }
}

/***koog 
(require codegen/aux-include codegen/cc) 
(aux-file-include)
 ***/
#include "py_session.gen.cpp"
/***end***/

static PyObject* meth_close(obj_Session* self, PyObject* /*args*/)
{
  destroyAnySession(self);
  RETURN_NO_VALUE;
}

// test code
static PyObject* meth_tick_count(obj_Session* self, PyObject* /*args*/)
{
  TUint tickCount;
  TInt errCode = self->iSession.TickCount(tickCount);
  if (errCode) {
    return SPyErr_SetFromSymbianOSErr(errCode);
  }
  return Py_BuildValue("i", static_cast<TInt>(tickCount));
}

// test code
static PyObject* meth_des16_len(obj_Session* self, PyObject* args)
{
  char* b;
  int l;
  if (!PyArg_ParseTuple(args, "u#", &b, &l))
  {
    return NULL;
  }
  TPtrC text((TUint16*)b, l);

  TInt desLen = 0;
  TInt errCode = self->iSession.TryEval(text, desLen);
  if (errCode) {
    return SPyErr_SetFromSymbianOSErr(errCode);
  }
  return Py_BuildValue("i", static_cast<TInt>(desLen));
}

static PyObject* meth_eval_get_result(obj_Session* self, PyObject* args)
{
  char* b;
  int l;
  if (!PyArg_ParseTuple(args, "u#", &b, &l))
  {
    return NULL;
  }
  TPtrC text((TUint16*)b, l);

  TBuf<512> resultDes;
  TInt evalErrCode = 0;
  TInt errCode = self->iSession.EvalGetResult(text, evalErrCode, resultDes);
  if (errCode) {
    return SPyErr_SetFromSymbianOSErr(errCode);
  }
  return Py_BuildValue("(iu#)", evalErrCode, resultDes.Ptr(), resultDes.Length());
}

static const PyMethodDef Session_methods[] =
  {
    {"close", (PyCFunction)meth_close, METH_NOARGS, NULL},
    {"tick_count", (PyCFunction)meth_tick_count, METH_NOARGS, NULL}, //xxx
    {"des16_len", (PyCFunction)meth_des16_len, METH_VARARGS, NULL}, //xxx
    {"eval_get_result", (PyCFunction)meth_eval_get_result, METH_VARARGS, NULL},
    {NULL, NULL} /* sentinel */
  };

/*
static void del_Session(obj_Session *self)
{
  destroyAnySession(self);
  PyObject_Del(self);
}
*/

static PyObject *getattr_Session(obj_Session *self, char *name)
{
  return Py_FindMethod(METHOD_TABLE(Session), reinterpret_cast<PyObject*>(self), name);
}

const static PyTypeObject tmpl_Session =
  {
    PyObject_HEAD_INIT(NULL)
    0, /*ob_size*/
    __MODULE_NAME__ ".Session", /*tp_name*/
    sizeof(obj_Session), /*tp_basicsize*/
    0, /*tp_itemsize*/
    /* methods */
#if 0
    (destructor)del_Session, /*tp_dealloc*/
#else
    (destructor)/***koog (aux-def-func/ref "del_Session" "void" "(obj_Session *self) { destroyAnySession(self); PyObject_Del(self); }") ***/del_Session/***end***/,
#endif
    0, /*tp_print*/
    (getattrfunc)getattr_Session, /*tp_getattr*/
    0, /*tp_setattr*/
    0, /*tp_compare*/
    0, /*tp_repr*/
    0, /*tp_as_number*/
    0, /*tp_as_sequence*/
    0, /*tp_as_mapping*/
    0 /*tp_hash*/
  };

TInt def_Session()
{
  return ConstructType(&tmpl_Session, __MODULE_NAME__ ".Session");
}

PyObject* new_Session(PyObject* /*self*/, PyObject* /*args*/)
{
  PyTypeObject* typeObject = reinterpret_cast<PyTypeObject*>(SPyGetGlobalString(__MODULE_NAME__ ".Session"));
  obj_Session* self = PyObject_New(obj_Session, typeObject);
  if (self == NULL)
      return NULL;

  TInt errCode = self->iSession.Connect();
  self->iSessionOpen = (errCode == KErrNone);
  if (errCode) {
    PyObject_Del(self);
    return SPyErr_SetFromSymbianOSErr(errCode);
  }

  return reinterpret_cast<PyObject*>(self);
}

/***koog (aux-file-write) ***//***end***/
