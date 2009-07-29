#include "lua_cl2.h"
#include "application_config.h"
#include "common/logging.h"

#ifdef __EPOC32__
#include <e32std.h>
#endif

#include "lualib.h"

// from lauxlib.c
extern "C" void *l_alloc (void *ud, void *ptr, size_t osize, size_t nsize);

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
