#ifndef __lua_cl2_h__
#define __lua_cl2_h__

#ifdef __cplusplus
extern "C" {
#endif

#include "lua.h"
#include "lauxlib.h"

#include <glib.h>

  void *l_alloc (void *ud, void *ptr, size_t osize, size_t nsize); // as in lauxlib.c
  int atpanic_print(lua_State* L); // as in lauxlib.c

  lua_State* cl_lua_new();
  lua_State* cl_lua_new_libs();

  gboolean validate_lua_syntax(const gchar* value, GError** error);

#ifdef __EPOC32__
  // A Lua error string gives the description for errors like these.
#define KErrLuaErr (-10400)

  int atpanic_leave(lua_State* L);
#endif

  // Frees 'error' before a non-local return.
  // Returns whatever lua_error does.
  int lua_raise_gerror(lua_State* L, GError* error);

#ifdef __cplusplus
} /* extern "C" */
#endif

#ifdef __cplusplus

/***koog 
;; This does work on Symbian as well for long as __LEAVE_EQUALS_THROW__,
;; as then a leave causes throw-related stack unwinding and cleanup.

(require codegen/cxx)
(def-auto-ptr "lua_State"
  (lambda (x) (printf "lua_close(~a);" x)))
***/
class lua_State_auto_ptr
{
 public:
  lua_State_auto_ptr(lua_State* aPtr) : iPtr(aPtr) {}
  ~lua_State_auto_ptr() { lua_close(iPtr); }
 private:
  lua_State* iPtr;
};
/***end***/

#endif // __cplusplus

#endif /* __lua_cl2_h__ */

/**

lua_cl2.h

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
