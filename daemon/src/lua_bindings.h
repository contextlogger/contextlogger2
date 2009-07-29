#ifndef __lua_bindings_h__
#define __lua_bindings_h__

#include "lua.h"

#ifdef __cplusplus
extern "C" {
#endif

#define LUA_CL2LIBNAME "cl2"
LUALIB_API int (luaopen_cl2) (lua_State *L);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __lua_bindings_h__ */
