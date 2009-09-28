#include "lua_bindings.h"

#include "application_config.h"
#include "kr_controller_private.h"
#include "lua_cl2.h"
#include "sa_sensor_list_integration.h"

#if defined(__SYMBIAN32__)
#include "epoc-iap.h"
#endif /* __SYMBIAN32__ */

#include "lua.hpp"

// xxx A lot of Lua bindings do not appear to check if they are given
// the correct number of arguments, and we are not doing that either.
// We might want to start doing so at some point.
#define luai_check(L, cond) { if (!(cond)) {  } } // xxx how to report
#define luai_checknelems(L, n) luai_check(L, (n) <= (L->top - L->base))

/***koog (require codegen/lua-c) ***//***end***/

/***koog (lua-func shutdown) ***/
static int f_shutdown(lua_State* L)
/***end***/
{
  EXIT_APPLICATION;
  return 0;
}

static int lua_error_unsupported(lua_State* L)
{
  lua_pushstring(L, "unsupported");
  lua_error(L); // will not return
  return 0; // to avoid warnings
}

#define throw_error_unsupported { return lua_error_unsupported(L); }

/***koog (lua-func iap_id_by_name) ***/
static int f_iap_id_by_name(lua_State* L)
/***end***/
{
#if !defined(__SYMBIAN32__)
  throw_error_unsupported;
#else
  // When Lua invokes a C function a fresh new stack will be used;
  // hence our sole argument will be the only item on the stack;
  // hence here narg -1 and 1 both point to said argument.
  const char* iapName = luaL_checklstring(L, 1, NULL);
  guint32 iapId = 0;
  gboolean found = FALSE;
  GError* iapError = NULL;
  gboolean success = epoc_iap_by_name(iapName, &iapId, &found, &iapError);
  lua_pop(L, 1); // iapName (must pop arg before pushing results)
  if (success) {
    if (found) {
      lua_pushinteger(L, iapId);
    } else {
      lua_pushnil(L);
    }
  } else {
    lua_raise_gerror(L, iapError);
  }
  return 1;
#endif /* __SYMBIAN32__ */
}

/***koog (lua-func config_get) ***/
static int f_config_get(lua_State* L)
/***end***/
{
  // xxx should we check that the number of arguments is right
  const char* name = luaL_checklstring(L, 1, NULL);
  GError* getError = NULL;
  char* value = cf_DYNAMIC_GET_ERR(name, &getError);
  lua_pop(L, 1); // name
  if (value) {
    lua_pushstring(L, value);
    g_free(value);
  } else if (is_not_found_error(getError)) {
    g_error_free(getError);
    lua_pushnil(L);
  } else {
    lua_raise_gerror(L, getError);
  }
  return 1;
}

/***koog (lua-func config_set) ***/
static int f_config_set(lua_State* L)
/***end***/
{
  const char* name = luaL_checklstring(L, 1, NULL);
  const char* value = luaL_checklstring(L, 2, NULL);
  logf("config set '%s' -> '%s'", name, value);
  GError* setError = NULL;
  gboolean success = cf_DYNAMIC_SET_ERR(name, value, &setError);
  lua_pop(L, 2); // name, value (perhaps unnecessary)
  if (!success) {
    logt("config set failed");
    return lua_raise_gerror(L, setError);
  } else {
    logt("config set ok");
  }
  return 0;
}

/***koog (lua-func all_sensor_names) ***/
static int f_all_sensor_names(lua_State* L)
/***end***/
// Lists _all_ known sensors, whether supported by the build or not.
// Filter using is_sensor_supported as necessary.
{
  const char* names[] = ALL_SENSOR_NAMES_LITERAL_LIST; // do not duplicate this in many places
  lua_createtable(L, NUM_ALL_SENSORS, 0); // at Lua stack index 1
  for (int i = 0; i < NUM_ALL_SENSORS; i++)
  {
    lua_pushstring(L, names[i]);
    lua_rawseti(L, 1, i + 1); // Lua indexes start from 1
  }
  return 1; // return the table
}

/***koog (lua-func is_sensor_supported) ***/
static int f_is_sensor_supported(lua_State* L)
/***end***/
{
  const char* name = luaL_checklstring(L, 1, NULL);
  gboolean is_so = sa_sensor_is_supported(name);
  lua_pushboolean(L, is_so);
  return 1;
}

/***koog (lua-func is_sensor_running) ***/
static int f_is_sensor_running(lua_State* L)
/***end***/
{
  const char* name = luaL_checklstring(L, 1, NULL);
  kr_Controller* kr = getGlobalClient();
  gboolean is_so = sa_Array_sensor_is_running(kr->scanner, name);
  lua_pushboolean(L, is_so);
  return 1;
}

/***koog (lua-func sensor_stop) ***/
static int f_sensor_stop(lua_State* L)
/***end***/
{
  const char* name = luaL_checklstring(L, 1, NULL);
  kr_Controller* kr = getGlobalClient();
  sa_Array_sensor_stop(kr->scanner, name);
  return 0;
}

/***koog (lua-func sensor_start) ***/
static int f_sensor_start(lua_State* L)
/***end***/
{
  const char* name = luaL_checklstring(L, 1, NULL);
  kr_Controller* kr = getGlobalClient();
  GError* localError = NULL;
  if (!sa_Array_sensor_start(kr->scanner, name, &localError)) {
    return lua_raise_gerror(L, localError);
  }
  return 0;
}

/***koog (lua-func upload_now) ***/
static int f_upload_now(lua_State* L)
/***end***/
{
#if __FEATURE_UPLOADER__
  kr_Controller* kr = getGlobalClient();
  if (!kr->uploader) {
    lua_pushstring(L, "uploader not started");
    lua_error(L); // will not return
  }
  GError* localError = NULL;
  if (!up_Uploader_upload_now(kr->uploader, &localError)) {
    return lua_raise_gerror(L, localError);
  }
#else
  lua_error_unsupported(L);
#endif
  return 0;
}

/***koog (lua-func remokon_start) ***/
static int f_remokon_start(lua_State* L)
/***end***/
{
#if __FEATURE_REMOKON__
  kr_Controller* kr = getGlobalClient();
  GError* localError = NULL;
  if (!rk_Remokon_start(kr->remokon, &localError)) {
    return lua_raise_gerror(L, localError);
  }
#else
  lua_error_unsupported(L);
#endif
  return 0;
}

/***koog (lua-func remokon_stop) ***/
static int f_remokon_stop(lua_State* L)
/***end***/
{
#if __FEATURE_REMOKON__
  kr_Controller* kr = getGlobalClient();
  rk_Remokon_stop(kr->remokon);
#else
  lua_error_unsupported(L);
#endif
  return 0;
}

static const luaL_Reg function_table[] = {
/***koog (lua-entries) ***/
  {"remokon_stop", f_remokon_stop},
  {"remokon_start", f_remokon_start},
  {"upload_now", f_upload_now},
  {"sensor_start", f_sensor_start},
  {"sensor_stop", f_sensor_stop},
  {"is_sensor_running", f_is_sensor_running},
  {"is_sensor_supported", f_is_sensor_supported},
  {"all_sensor_names", f_all_sensor_names},
  {"config_set", f_config_set},
  {"config_get", f_config_get},
  {"iap_id_by_name", f_iap_id_by_name},
  {"shutdown", f_shutdown},
/***end***/
  {NULL, NULL}
};

extern "C" int luaopen_cl2(lua_State *L) 
{
  luaL_register(L, LUA_CL2LIBNAME, function_table);
  lua_pushnumber(L, __VERSION100__);
  lua_setfield(L, -2, "version");
  return 1;
}
