#ifndef __lua_cl2_h__
#define __lua_cl2_h__

#ifdef __cplusplus
extern "C" {
#endif

#include <glib.h>
#include "lua.h"
#include "lauxlib.h"

  lua_State *cl_lua_newstate(gboolean privileged, lua_CFunction panic_f);

  void cl_lua_close(lua_State *state);

  //xxx signature will change
  void cl_lua_eval_string(const char *str);

#ifdef __EPOC32__
  // A Lua error string gives the description for errors like these.
#define KErrLuaErr (-10400)

  int atpanic_leave(lua_State* L);
#endif
  int atpanic_print(lua_State* L); // defined in lauxlib.c

#ifdef __cplusplus
} /* extern "C" */
#endif

#ifdef __cplusplus

/***koog 
;; This does work on Symbian as well for long as __LEAVE_EQUALS_THROW__,
;; as then a leave causes throw-related stack unwinding and cleanup.

(require codegen/cxx)
(def-auto-ptr "lua_State"
  (lambda (x) (printf "cl_lua_close(~a);" x)))
***/
class lua_State_auto_ptr
{
 public:
  lua_State_auto_ptr(lua_State* aPtr) : iPtr(aPtr) {}
  ~lua_State_auto_ptr() { cl_lua_close(iPtr); }
 private:
  lua_State* iPtr;
};
/***end***/

#endif // __cplusplus

#endif /* __lua_cl2_h__ */
