#include "lua_cl2.h"

#include "application_config.h"
#include "er_errors.h"

#include "common/logging.h"

#include "lua.hpp"

#ifdef __EPOC32__
#include <e32std.h>
#endif

#include <stdlib.h>

// --------------------------------------------------
// app-specific libraries
// --------------------------------------------------

#include "lua_bindings.h" // CL2 bindings
#include "libluasqlite3.h"

static const luaL_Reg my_lualibs[] = {
  {LUA_CL2LIBNAME, luaopen_cl2},
  {"sqlite3", luaopen_sqlite3},
  {NULL, NULL}
};

static void my_openlibs (lua_State *L) {
  const luaL_Reg *lib = my_lualibs;
  for (; lib->func; lib++) {
    lua_pushcfunction(L, lib->func);
    // Library name for optional use by the luaopen_* functions.
    lua_pushstring(L, lib->name);
    lua_call(L, 1, 0);
  }
}

// --------------------------------------------------
// Lua state management
// --------------------------------------------------

// Does not provide access to _any_ libraries, either the standard
// ones or the application specific ones. Add them later as you like.
// See cl_lua_new_libs().
// 
// xxx We are not setting any privileged flag here either, as we do
// not have such a mechanism, at least not yet.
extern "C" lua_State* cl_lua_new()
{
  // Second arg is "ud" as passed to l_alloc.
  lua_State *L = lua_newstate(l_alloc, NULL);
  if (!L) {
    return NULL;
  }

  // On Symbian Lua shall recover from errors by doing a leave, and
  // lua_pcall should actually return in the case of an error. On
  // other platforms Lua will simply exit() as there is no panic
  // handler installed that would do a non-local return.
  WHEN_SYMBIAN(lua_atpanic(L, atpanic_leave));
  UNLESS_SYMBIAN(lua_atpanic(L, atpanic_print));

  return L;
}

extern "C" lua_State* cl_lua_new_libs()
{
  lua_State *L = cl_lua_new();
  if (L) {
    luaL_openlibs(L);
    my_openlibs(L);
  }
  return L;
}

extern "C" gboolean validate_lua_syntax(const gchar* value, GError** error)
{
  lua_State* L = cl_lua_new();
  if (!L) {
    if (error) 
      *error = gx_error_no_memory;
    return FALSE;
  }

  int res = luaL_loadstring(L, value);
  gboolean success;
  switch (res)
    {
    case 0:
      {
        success = TRUE;
        break;
      }
    case LUA_ERRMEM:
      {
	success = FALSE;
	if (error) 
	  *error = gx_error_no_memory;
        break;
      }
    default:
      {
	success = FALSE;
	if (error) {
	  const char* luaErr = lua_tostring(L, -1);
	  *error = g_error_new(domain_lua, res, "validation of Lua code failed: %s", luaErr);
	}
        break;
      }
    }

  lua_close(L);

  return success;
}

extern "C" int lua_raise_gerror(lua_State* L, GError* error)
{
  if (error) {
    gchar* s = gx_error_to_string(error);
    g_error_free(error);
    lua_pushstring(L, s); // Lua makes its own copy of "s"
    g_free(s);
  } else {
    lua_pushstring(L, "out of memory");
  }
  return lua_error(L);
}

#ifdef __EPOC32__
extern "C" int atpanic_leave(lua_State* L)
{
  User::Leave(KErrLuaErr);
  // we did a leave so Lua will not call "exit"
  return 0; // number of Lua results (to keep compiler happy)
}
#endif

/**

The code above this notice is covered by the following license:

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

