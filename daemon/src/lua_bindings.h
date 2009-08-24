#ifndef __lua_bindings_h__
#define __lua_bindings_h__

#ifdef __cplusplus
extern "C" {
#endif

#include "lua.h"

#define LUA_CL2LIBNAME "cl2"

int luaopen_cl2(lua_State *L);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __lua_bindings_h__ */
