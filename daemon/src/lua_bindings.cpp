#include "lua_bindings.h"

#include "ac_app_context.h"
#include "application_config.h"
#include "kr_controller_private.h"
#include "lua_cl2.h"
#include "sa_sensor_list_integration.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#if defined(__SYMBIAN32__)
#include "epoc-iap.h"
#endif /* __SYMBIAN32__ */

#include "lua.hpp"

// xxx A lot of Lua bindings do not appear to check if they are given
// the correct number of arguments, and we are not doing that either.
// We might want to start doing so at some point.
#define luai_check(L, cond) { if (!(cond)) {  } } // xxx how to report
#define luai_checknelems(L, n) luai_check(L, (n) <= (L->top - L->base))

static int lua_error_unsupported(lua_State* L)
{
  lua_pushstring(L, "unsupported");
  lua_error(L); // will not return
  return 0; // to avoid warnings
}

#define throw_error_unsupported { return lua_error_unsupported(L); }

/***koog (require codegen/lua-c) ***//***end***/

/***koog (lua-func static_get_str) ***/
static int f_static_get_str(lua_State* L)
/***end***/
{
  const char* name = luaL_checklstring(L, 1, NULL);
  const char* value = cf_RcFile_get_str_maybe(ac_global_RcFile, name);
  if (value)
    lua_pushstring(L, value);
  else
    lua_pushnil(L);
  return 1;
}
/*
-- It seems okay to reenter the same Lua VM via a binding.
-- Try putting this in the config file, and then
-- remotely doing return cl2.static_get_str('foo')
foo = function () return cl2.static_get_str('bar') end
bar = function () return cl2.static_get_str('baz') end
baz = "bamf"
*/

/***koog (lua-func throw_unsupported) ***/
static int f_throw_unsupported(lua_State* L)
/***end***/
{
  throw_error_unsupported;
}

/***koog (lua-func die_now) ***/
static int f_die_now(lua_State* L)
/***end***/
{
  EXIT_APPLICATION;
  return 0;
}

/***koog (lua-func shutdown) ***/
static int f_shutdown(lua_State* L)
/***end***/
{
  SHUTDOWN_APPLICATION;
  return 0;
}

/***koog (lua-func log) ***/
static int f_log(lua_State* L)
/***end***/
{
  (void)L;
#if __DO_LOGGING__
  const char* text = luaL_checklstring(L, 1, NULL);
  logt(text);
#endif
  return 0;
}

/***koog (lua-func is_ascii_ident) ***/
static int f_is_ascii_ident(lua_State* L)
/***end***/
{
  const char* s = luaL_checklstring(L, 1, NULL);
  gboolean is_so = is_ascii_ident(s);
  lua_pushboolean(L, is_so);
  return 1;
}

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
  char* value = ac_DYNAMIC_GET_ERR(name, &getError);
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
  gboolean success = ac_DYNAMIC_SET_ERR(name, value, &setError);
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

/***koog (lua-func get_upload_time) ***/
static int f_get_upload_time(lua_State* L)
/***end***/
{
  time_t t = ac_global_Registry->last_upload_time;
  if (t == 0) {
    lua_pushnil(L);
  } else {
    lua_pushinteger(L, t);
  }
  return 1;
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

/***koog (lua-func log_message) ***/
static int f_log_message(lua_State* L)
/***end***/
{
#if __APPMESSAGE_ENABLED__
  const char* msgText = luaL_checklstring(L, 1, NULL);
  GError* localError = NULL;
  if (!log_db_log_appmessage(ac_global_LogDb, msgText, &localError)) {
    return lua_raise_gerror(L, localError);
  }
#else
  lua_error_unsupported(L);
#endif
  return 0;
}

static const luaL_Reg function_table[] = {
/***koog (lua-entries) ***/
  {"log_message", f_log_message},
  {"remokon_stop", f_remokon_stop},
  {"remokon_start", f_remokon_start},
  {"get_upload_time", f_get_upload_time},
  {"upload_now", f_upload_now},
  {"sensor_start", f_sensor_start},
  {"sensor_stop", f_sensor_stop},
  {"is_sensor_running", f_is_sensor_running},
  {"is_sensor_supported", f_is_sensor_supported},
  {"all_sensor_names", f_all_sensor_names},
  {"config_set", f_config_set},
  {"config_get", f_config_get},
  {"iap_id_by_name", f_iap_id_by_name},
  {"is_ascii_ident", f_is_ascii_ident},
  {"log", f_log},
  {"shutdown", f_shutdown},
  {"die_now", f_die_now},
  {"throw_unsupported", f_throw_unsupported},
  {"static_get_str", f_static_get_str},
/***end***/
  {NULL, NULL}
};

extern "C" int luaopen_cl2(lua_State *L) 
{
  luaL_register(L, LUA_CL2LIBNAME, function_table);

  /*
  lua_pushnumber(L, __VERSION100__);
  lua_setfield(L, -2, "version");
  */

  lua_pushstring(L, __APP_NAME__);
  lua_setfield(L, -2, "app_name");

  lua_pushstring(L, __VERSION_STRING__);
  lua_setfield(L, -2, "app_version");

  lua_pushstring(L, __VARIANT_NAME__);
  lua_setfield(L, -2, "app_variant");

  lua_pushstring(L, __COMPILER_NAME__);
  lua_setfield(L, -2, "compiler_name");

#if defined(__GNUC__)
  lua_pushfstring(L, "%d.%d.%d",
		  __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
#else
  lua_pushnil(L);
#endif
  lua_setfield(L, -2, "compiler_version");

#if defined(NDEBUG)
  lua_pushstring(L, "release");
#else
  lua_pushstring(L, "debug");
#endif
  lua_setfield(L, -2, "build_type");

  return 1;
}

/**

lua_bindings.cpp

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
