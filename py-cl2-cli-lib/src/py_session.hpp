#ifndef __py_machine_hpp__
#define __py_machine_hpp__

//#include <math.h>
//#define HAVE_HUGE_VAL 1 // to avoid HUGE_VAL redefinition warning
#include <Python.h>
#include <e32std.h>

TInt def_Session();

PyObject* new_Session(PyObject* /*self*/, PyObject* /*args*/);

#endif /* __py_machine_hpp__ */
