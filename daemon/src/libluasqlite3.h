#ifndef __libluasqlite3_h__
#define __libluasqlite3_h__

#ifdef __cplusplus
extern "C" {
#endif

#include "lua.h"

  int luaopen_sqlite3(lua_State* L);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __libluasqlite3_h__ */
