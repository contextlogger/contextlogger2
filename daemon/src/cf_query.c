#include "cf_query.h"

#include "er_errors.h"
#include "lua_cl2.h"
#include "kr_controller_private.h"

#include <string.h>

// --------------------------------------------------
// general
// --------------------------------------------------

// Evaluates the Lua expression "luaStr". If it evaluates to a nil
// value, sets "L" to NULL. If there is an actual error, returns it.
// Otherwise leaves the result to the Lua state "L", and the caller
// takes ownership of "L".
static gboolean eval_lua_str(const gchar* luaStr, lua_State** pL, GError** error)
{
  {
    lua_State* L = cl_lua_new_libs();
    if (!L) {
      if (error) *error = gx_error_no_memory;
      return FALSE;
    }
    //logf("will load string '%s'", luaStr);

    int res = (luaL_loadstring(L, luaStr) || lua_pcall(L, 0, 1, 0));
    if (res != 0) {
      lua_set_gerror(L, error);
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

// Like get_lua_value, but uses a default Lua expression when not
// found from the database. "With default", that is. defStr may be
// NULL, and it will not be freed.
static gboolean get_lua_value_wd(const gchar* name,
				 const gchar* defStr,
				 lua_State** pL, GError** error)
{
  gchar* luaStr = NULL;

  {
    GError* localError = NULL;
    luaStr = cf_DYNAMIC_GET_ERR(name, &localError);
    if (!luaStr) {
      if (is_not_found_error(localError)) {
	g_error_free(localError);
	if (!defStr) {
	  *pL = NULL;
	  return TRUE;
	}
      } else {
	gx_propagate_error(error, localError);
	return FALSE;
      }
    }
  }

  gboolean ok = eval_lua_str(luaStr ? luaStr : defStr, pL, error);
  if (luaStr) g_free(luaStr);
  return ok;
}

// Gets a Lua expression from configuration by "name". If there is no
// value by "name", or if it evaluates to a nil value, sets "L" to
// NULL. If there is an actual error, returns it. Otherwise leaves the
// result to the Lua state "L", and the caller takes ownership of "L".
#define get_lua_value(name,L,err) get_lua_value_wd(name,NULL,L,err)

// --------------------------------------------------
// integer
// --------------------------------------------------

static gboolean get_int_from_lua(lua_State* L, int* value, GError** error)
{
  if (!lua_isnumber(L, -1)) {
    if (error)
      *error = gx_error_new(domain_cl2app, code_type_error, 
			    "integer type Lua value expected");
    return FALSE;
  }
  //logt("lua value is a number");
    
  lua_Number num = lua_tonumber(L, -1); // double by default
  *value = (int)num; // not checking for loss of precision
  return TRUE;
}

gboolean try_get_ConfigDb_int(const gchar* name, int* value, 
			      gboolean* found, GError** error)
{
  lua_State* L = NULL;
  if (!get_lua_value(name, &L, error)) {
    return FALSE;
  }
  
  if (L) {
    gboolean typeOk = get_int_from_lua(L, value, error);
    lua_close(L);
    if (!typeOk) return FALSE;
  }

  if (found) *found = (L != NULL);
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

int force_get_ConfigDb_int(const gchar* name, int default_value)
{
  int value = default_value;
  try_get_ConfigDb_int(name, &value, NULL, NULL);
  return value;
}

int force_lua_eval_int(const gchar* luaStr, int default_value)
{
  lua_State* L = NULL;
  if (eval_lua_str(luaStr, &L, NULL)) {
    int value;
    gboolean typeOk = get_int_from_lua(L, &value, NULL);
    lua_close(L);
    if (typeOk) return value;
  }
  return default_value;
}

// --------------------------------------------------
// boolean
// --------------------------------------------------

static gboolean get_bool_from_lua(lua_State* L, gboolean* value, GError** error)
{
  if (!lua_isboolean(L, -1)) {
    if (error)
      *error = gx_error_new(domain_cl2app, code_type_error, 
			    "boolean type Lua value expected");
    return FALSE;
  }
  //logt("lua value is a boolean");
    
  int num = lua_toboolean(L, -1);
  *value = (num ? TRUE : FALSE);
  return TRUE;
}

gboolean try_get_ConfigDb_bool(const gchar* name, gboolean* value, 
			       gboolean* found, GError** error)
{
  lua_State* L = NULL;
  if (!get_lua_value(name, &L, error)) {
    return FALSE;
  }
  
  if (L) {
    gboolean typeOk = get_bool_from_lua(L, value, error);
    lua_close(L);
    if (!typeOk) return FALSE;
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

gboolean force_get_ConfigDb_bool(const gchar* name, gboolean default_value)
{
  gboolean value = default_value;
  try_get_ConfigDb_bool(name, &value, NULL, NULL);
  return value;
}

gboolean force_lua_eval_bool(const gchar* luaStr, gboolean default_value)
{
  lua_State* L = NULL;
  if (eval_lua_str(luaStr, &L, NULL)) {
    gboolean value;
    gboolean typeOk = get_bool_from_lua(L, &value, NULL);
    lua_close(L);
    if (typeOk) return value;
  }
  return default_value;
}

// --------------------------------------------------
// string
// --------------------------------------------------

gboolean try_get_ConfigDb_str(const gchar* name, gchar** s, GError** error)
{
  lua_State* L = NULL;
  if (!get_lua_value(name, &L, error)) {
    return FALSE;
  }
  
  gchar* gotStr = NULL;

  if (L) {
    if (!lua_isstring(L, -1)) {
      if (error)
	*error = gx_error_new(domain_cl2app, code_type_error, "code for '%s' did not yield a string", name);
      lua_close(L);
      return FALSE;
    }
    //logt("lua value is a string");

    const char* luaOwned = lua_tostring(L, -1);
    gotStr = strdup(luaOwned);
    lua_close(L);

    if (!gotStr) {
      if (error) *error = gx_error_no_memory;
      return FALSE;
    }
  }

  *s = gotStr;
  return TRUE;
}

gboolean get_ConfigDb_str(const gchar* name, gchar** s, 
			  gchar* default_s, GError** error)
{
  if (!try_get_ConfigDb_str(name, s, error)) {
    return FALSE;
  }
  if (!*s && default_s) {
    *s = strdup(default_s);
    if (G_UNLIKELY(!*s)) {
      if (error) *error = gx_error_no_memory;
      return FALSE;
    }
  }
  return TRUE;
}

int get_config_iap_id()
{
  // Get from dynamic config.
  int value = -1;
  gboolean found = FALSE;
  try_get_ConfigDb_int("iap", &value, &found, NULL);
  if (found) return value;
  // Get from static config.
  return ac_STATIC_GET(iap);
}

const gchar* get_config_username()
{
  return ac_STATIC_GET(username);
}

/**

cf_query.c

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
