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
