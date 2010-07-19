#ifndef __gxlowmem_h__
#define __gxlowmem_h__

// What makes the use of these trap harnesses very delicate is that
// there can be no nesting. One thread, one trap set at a time, as
// there is only one jmp_buf (as filled in by setjmp()).
// 
// This mechanism may also not mesh well with C++ exceptions. Better
// not to have overlap between the extents of the two.
//
// For an "Introduction to Glib low memory handler", see http://library.forum.nokia.com/topic/S60_5th_Edition_Cpp_Developers_Library/GUID-FE27AB35-C6FD-4F11-802D-0D5FCFFC2976/html/mrt/s60_openc_using_glib8.html#topic7

#if defined(__SYMBIAN32__)

#include "common/glowmem_action.h"

#define TRAP_OOM(_err_act, _do_act) {		\
    SET_LOW_MEMORY_TRAP_ACTION(_err_act);	\
    { _do_act; }				\
    REMOVE_LOW_MEMORY_TRAP();			\
  }

#else

#define SET_LOW_MEMORY_TRAP_VOID()
#define SET_LOW_MEMORY_TRAP(failure_value)
#define SET_LOW_MEMORY_TRAP_ACTION(action)
#define REMOVE_LOW_MEMORY_TRAP()

#define TRAP_OOM(_err_act, _do_act) { _do_act; }

#endif /* not __SYMBIAN32__ */

#define TRAP_OOM_VALUE(_err_val, _do_act) \
  TRAP_OOM(return _err_val, _do_act)

#define TRAP_OOM_NULL(_do_act) \
  TRAP_OOM(return NULL, _do_act)

#define TRAP_OOM_VOID(_do_act) \
  TRAP_OOM(return, _do_act)

#define TRAP_OOM_FAIL(_do_act) \
  TRAP_OOM(goto fail, _do_act)

// These aliases are just to make it easy to grep for "TRAP_OOM".
#define SET_TRAP_OOM_VOID() SET_LOW_MEMORY_TRAP_VOID()
#define SET_TRAP_OOM_VALUE(_val) SET_LOW_MEMORY_TRAP(_val)
#define SET_TRAP_OOM(_act) SET_LOW_MEMORY_TRAP_ACTION(_act)
#define UNSET_TRAP_OOM() REMOVE_LOW_MEMORY_TRAP()

#endif /* __gxlowmem_h__ */

/**

gxlowmem.h

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
