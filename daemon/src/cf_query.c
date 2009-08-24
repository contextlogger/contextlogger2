#include "cf_query.h"

#include "er_errors.h"
#include "lua_cl2.h"
#include "kr_controller_private.h"

#include <string.h>

#define SET_LUA_ERROR(_code) \
  if (error) { \
    const char* _luaErr = lua_tostring(L, -1); \
    *error = g_error_new(domain_lua, _code, _luaErr); \
  }

static gboolean try_get_lua(const gchar* name, lua_State** pL, GError** error)
{
  gchar* luaStr;

  {
    GError* localError = NULL;
    luaStr = cf_DYNAMIC_GET_ERR(name, &localError);
    if (!luaStr) {
      if (is_not_found_error(localError)) {
	g_error_free(localError);
	*pL = NULL;
	return TRUE;
      } else {
	gx_propagate_error(error, localError);
	return FALSE;
      }
    }
  }

  {
    lua_State* L = cl_lua_new_libs();
    if (!L) {
      if (error) *error = gx_error_no_memory;
      g_free(luaStr);
      return FALSE;
    }
    //logf("will load string '%s'", luaStr);

    int res = (luaL_loadstring(L, luaStr) || lua_pcall(L, 0, 1, 0));
    g_free(luaStr);
    if (res != 0) {
      SET_LUA_ERROR(res);
      lua_close(L);
      return FALSE;
    }
    //logt("lua string evaluated ok");

    if (lua_isnil(L, -1)) {
      lua_close(L);
      *pL = NULL;
      return TRUE;
    }

    *pL = L;
    return TRUE;
  }
}

// xxx code duplication, could implement based on try_get_lua
gboolean try_get_ConfigDb_int(const gchar* name, int* value, 
			      gboolean* found, GError** error)
{
  GError* localError = NULL;
  gchar* luaStr = cf_DYNAMIC_GET_ERR(name, &localError);
  if (!luaStr) {
    if (is_not_found_error(localError)) {
      g_error_free(localError);
      if (found) *found = FALSE;
      return TRUE;
    } else {
      gx_propagate_error(error, localError);
      return FALSE;
    }
  }

  // Evaluate Lua string.
  {
    lua_State* L = cl_lua_new_libs();
    if (!L) {
      if (error) *error = gx_error_no_memory;
      g_free(luaStr);
      return FALSE;
    }
    //logf("will load string '%s'", luaStr);

    int res = (luaL_loadstring(L, luaStr) || lua_pcall(L, 0, 1, 0));
    g_free(luaStr);
    if (res != 0) {
      SET_LUA_ERROR(res);
      lua_close(L);
      return FALSE;
    }
    //logt("lua string evaluated ok");

    if (lua_isnil(L, -1)) {
      lua_close(L);
      if (found) *found = FALSE;
      return TRUE;
    }
    //logt("lua value is not nil");

    if (!lua_isnumber(L, -1)) {
      if (error)
	*error = g_error_new(domain_cl2app, code_type_error, "code for '%s' did not yield a number", name);
      lua_close(L);
      return FALSE;
    }
    //logt("lua value is a number");
    
    lua_Number num = lua_tonumber(L, -1); // double by default
    *value = (int)num; // not checking for loss of precision
    lua_close(L);
  }

  if (found) *found = TRUE;
  return TRUE;
}

gboolean get_ConfigDb_int(const gchar* name, int* value, 
			  int default_value, GError** error)
{
  gboolean found = FALSE;
  if (!try_get_ConfigDb_int(name, value, &found, error))
    return FALSE;
  if (!found)
    *value = default_value;
  return TRUE;
}

gboolean try_get_ConfigDb_bool(const gchar* name, gboolean* value, 
			       gboolean* found, GError** error)
{
  lua_State* L = NULL;
  if (!try_get_lua(name, &L, error)) {
    return FALSE;
  }
  
  if (L) {
    if (!lua_isboolean(L, -1)) {
      if (error)
	*error = g_error_new(domain_cl2app, code_type_error, "code for '%s' did not yield a boolean", name);
      lua_close(L);
      return FALSE;
    }
    //logt("lua value is a boolean");
    
    int num = lua_toboolean(L, -1);
    *value = ((!num) ? FALSE : TRUE);
    lua_close(L);
  }

  if (found) *found = (L != NULL);
  return TRUE;
}

gboolean get_ConfigDb_bool(const gchar* name, gboolean* value, 
			   gboolean default_value, GError** error)
{
  gboolean found = FALSE;
  if (!try_get_ConfigDb_bool(name, value, &found, error))
    return FALSE;
  if (!found)
    *value = default_value;
  return TRUE;
}

#define SET_DEFAULT_STR { if (default_s) { gotStr = strdup(default_s); goto gotit; } else { *s = NULL; return TRUE; } }

gboolean get_ConfigDb_str(const gchar* name, gchar** s, 
			  const gchar* default_s, GError** error)
{
  gchar* gotStr = NULL;

  GError* localError = NULL;
  gchar* luaStr = cf_DYNAMIC_GET_ERR(name, &localError);
  if (!luaStr) {
    if (is_not_found_error(localError)) {
      g_error_free(localError);
      SET_DEFAULT_STR;
    } else {
      gx_propagate_error(error, localError);
      return FALSE;
    }
  }

  // Evaluate Lua string.
  {
    lua_State* L = cl_lua_new_libs();
    if (!L) {
      if (error) *error = gx_error_no_memory;
      g_free(luaStr);
      return FALSE;
    }
    //logf("will load string '%s'", luaStr);

    int res = (luaL_loadstring(L, luaStr) || lua_pcall(L, 0, 1, 0));
    g_free(luaStr);
    if (res != 0) {
      SET_LUA_ERROR(res);
      lua_close(L);
      return FALSE;
    }
    //logt("lua string evaluated ok");

    if (lua_isnil(L, -1)) {
      lua_close(L);
      SET_DEFAULT_STR;
    }
    //logt("lua value is not nil");

    if (!lua_isstring(L, -1)) {
      if (error)
	*error = g_error_new(domain_cl2app, code_type_error, "code for '%s' did not yield a string", name);
      lua_close(L);
      return FALSE;
    }
    //logt("lua value is a string");
    
    const char* luaOwned = lua_tostring(L, -1); // double by default
    gotStr = strdup(luaOwned);
    lua_close(L);
  }

 gotit:
  if (!gotStr) {
    if (error) *error = gx_error_no_memory;
    return FALSE;
  }
  *s = gotStr;
  return TRUE;
}
