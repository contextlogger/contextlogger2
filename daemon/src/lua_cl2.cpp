#include "lua_cl2.h"

#include "application_config.h"

#include "common/logging.h"

#include "lualib.h"

#ifdef __EPOC32__
#include <e32std.h>
#endif

#include <stdlib.h>

#ifdef __EPOC32__
extern "C" int atpanic_leave(lua_State* L)
{
  User::Leave(KErrLuaErr);
  // we did a leave so Lua will not call "exit"
  return 0; // number of Lua results (to keep compiler happy)
}
#endif

extern "C" lua_State *cl_lua_newstate(gboolean privileged, lua_CFunction panic_f)
{
  // Second arg is "ud" as passed to l_alloc.
  lua_State *L = lua_newstate(l_alloc, NULL);
  if (!L) return NULL;

  lua_atpanic(L, panic_f);

  luaL_openlibs(L); // make stdlibs and cl2 bindings available

  // xxx privileged flag

  return L;
}

extern "C" void cl_lua_close(lua_State *state)
{
  lua_close(state);
}

#if 0
// Evaluates a zero-terminated string of Lua code in a fresh VM
// instance, returning the resulting string.
extern "C" void cl_lua_eval_string(const char *str)
{
  lua_State* L = cl_lua_newstate(FALSE);
  if (!L) return;
  if (luaL_dostring(L, str)) {
    // error -- how to get info
    const char* msg = lua_tostring(L, -1);
    logf("Lua Error: %s", (msg ? msg : "<unknown error>"));
  } else {
    const char* msg = lua_tostring(L, -1);
    logf("Lua Result: %s", (msg ? msg : "<non-string result>"));
  }
  cl_lua_close(L);
}
#endif

// --------------------------------------------------
// code from Lua follows...
// --------------------------------------------------

/******************************************************************************
* Copyright (C) 1994-2008 Lua.org, PUC-Rio.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/

extern "C" void *l_alloc (void *ud, void *ptr, size_t osize, size_t nsize) {
  (void)ud;
  (void)osize;
  if (nsize == 0) {
    free(ptr);
    return NULL;
  }
  else
    return realloc(ptr, nsize);
}


extern "C" int atpanic_print (lua_State *L) {
  (void)L;  /* to avoid warnings */
  fprintf(stderr, "PANIC: unprotected error in call to Lua API (%s)\n",
                   lua_tostring(L, -1));
  return 0;
}

