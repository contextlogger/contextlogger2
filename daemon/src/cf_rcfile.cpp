#include "cf_rcfile.h"

#if __FEATURE_RCFILE__

#include "er_errors.h"
#include "lua_cl2.h"
#include "utils_cl2.h"

struct _cf_RcFile {
  // Contains the named configuration values as global variables.
  lua_State *L;
};

#ifdef __EPOC32__
#define RCFILE_BASENAME "config.txt"
#else
#define RCFILE_BASENAME ".cl2rc"
#endif

#define RCFILE_DIR CONFIG_DIR
#define RCFILE_FILE (RCFILE_DIR DIR_SEP RCFILE_BASENAME)

#define return_with_error(s...) { if (error) *error = gx_error_new(domain_cl2app, code_unspecified_error, s); return FALSE; }
#define return_with_oom { if (error) *error = gx_error_no_memory; return FALSE; }

// See also boost/preprocessor/stringize.hpp for similar definitions.
#define STRINGIZE_DETAIL(x) #x
#define STRINGIZE(x) STRINGIZE_DETAIL(x)

#define EVALUATE(_s) { /*logt(_s);*/ if (luaL_dostring(L, _s)) throw LuaException(); }

/***koog 
    (require codegen/cpp-include) 
    (display-cpp-string-lit-decl/from-file 
    "CF_VALIDATE_IN_LUA"
    "cf_validate.lua")
***/
#define CF_VALIDATE_IN_LUA \
"function is_non_empty_string (s)\n" \
   "return (s ~= '')\n" \
"end\n" \
"function validate (n, rt, chk)\n" \
   "local v = _G[n]\n" \
   "if v then\n" \
      "local t = type(v)\n" \
      "if t ~= 'function' and t ~= rt then\n" \
	 "error(string.format('value %q not of required type %q', n, rt))\n" \
      "end\n" \
      "if chk and t ~= 'function' then\n" \
	 "if not chk(v) then\n" \
	    "error(string.format('value %q is not valid', n))\n" \
	 "end\n" \
      "end\n" \
   "end\n" \
"end\n" \
"validate('username', 'string', cl2.is_ascii_ident)\n" \
"validate('upload_url', 'string', is_non_empty_string)\n" \
"validate('remokon_host', 'string', is_non_empty_string)\n" \
"validate('remokon_port', 'number', nil)\n" \
"validate('remokon_password', 'string', is_non_empty_string)\n" \
"validate('jid', 'string', is_non_empty_string)\n" \
"validate('iap', 'number', nil)\n" \
"validate('database_dir_string', 'string', is_non_empty_string)\n" \
"validate('database_disk_threshold', 'number', nil)\n" \
"validate('mcc', 'number', nil)\n" \
"if iap == nil then\n" \
   "iap = IAP_DEFAULT\n" \
"end\n" \
"if upload_url == nil then\n" \
   "upload_url = UPLOAD_URL_DEFAULT\n" \
"end\n"
/***end***/

// This function validates the configuration, checking the types of
// the configured values and such. Any defaults can also be set here
// for values that are not present.
static void ValidateAdjustConfig(lua_State *L)
{
  // We first make any defaults available to Lua by defining them as
  // globals. (Defaults are typically given as Lua expressions to
  // begin with, but use STRINGIZE if you need to.) Then we invoke the
  // generated Lua code that does the validation etc.
  const char* s = 
    "IAP_DEFAULT = " __IAP_ID_EXPR__ ";\n"
    "UPLOAD_URL_DEFAULT = " __UPLOAD_URL__ ";\n"
    CF_VALIDATE_IN_LUA;
  EVALUATE(s);
}

static gboolean ReadRcFile(cf_RcFile* self, lua_State *L, GError** error)
{
  int errCode;

  if ((errCode = luaL_loadfile(L, RCFILE_FILE)) != 0) {
    if (errCode == LUA_ERRFILE) {
      // Could not open or read the file. This is okay since a
      // configuration file is not compulsory.
      logf("no (readable) configuration file '%s'", RCFILE_FILE);
      ValidateAdjustConfig(L);
      return TRUE;
    }

#if __DO_LOGGING__
    // An error occurred, and hence an error message should have been pushed by Lua.
    if (!lua_isnone(L, -1)) {
      const char* s = lua_tostring(L, -1);
      if (s) logt(s);
    }
#endif /* __DO_LOGGING__ */

    if (error) 
      *error = gx_error_new(domain_cl2app, code_unspecified_error, "error parsing configuration file '%s'", RCFILE_FILE);
    return FALSE;
  }
  logf("config file '%s' parsed OK", RCFILE_FILE);

  if (lua_pcall(L, 0, 1, 0)) {
    if (error)
      *error = gx_error_new(domain_cl2app, code_unspecified_error, "error evaluating configuration file '%s'", RCFILE_FILE);
    return FALSE;
  }
  logt("config file evaluated OK");

  ValidateAdjustConfig(L);

#if __DO_LOGGING__
  EVALUATE("do\n" 
	   "  local function f (n, fm) local v = _G[n]; if v ~= nil then if type(v) == 'function' then cl2.log(string.format('%s configured to <function>', n)) else cl2.log(string.format('%s configured to ' .. fm .. ' :: %s', n, v, type(v))); end; end; end;"
	   "  f('username', '%q');"
	   "  f('upload_url', '%q');"
	   "  f('remokon_host', '%q');"
	   "  f('iap', '%q');"
	   "  f('database_disk_threshold', '%q');"
	   "end");
#endif /* __DO_LOGGING__ */
  
  return TRUE;
}

extern "C" cf_RcFile* cf_RcFile_new(GError** error) 
{
  cf_RcFile* self = g_try_new0(cf_RcFile, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory;
    return NULL;
  }

  // Reentering the same VM instance to fetch some config info seems
  // to work. Not sure if it is quite safe, though, so you may want to
  // consider that when adding cl2 module using functions into your
  // config file.
  // 
  // Also note that *any* unprotected error occurring in this VM will
  // bring down the entire process. This is by design, as we consider
  // OOM errors and errors in config files very severe.
  self->L = cl_lua_new_libs();
  if (G_UNLIKELY(!self->L)) {
    if (error) *error = gx_error_no_memory;
    goto fail;
  }

  try {
    if (G_UNLIKELY(!ReadRcFile(self, self->L, error))) {
      goto fail;
    }
  } catch(const LuaException&) {
    lua_set_gerror(self->L, error);
    goto fail;
  }

  return self;

 fail:
  cf_RcFile_destroy(self);
  return NULL;
}
  
extern "C" void cf_RcFile_destroy(cf_RcFile* self)
{
  if (self) {
    if (self->L)
      lua_close(self->L);
    g_free(self);
  }
}

class Fail {};

static void get_value(lua_State *L, const char* name)
{
  // It is important to clear the stack occasionally. Note that we do
  // not pop any return value to prevent it from being collected
  // immediately.
  lua_settop(L, 0);

  // Not documented, but returns nil on non-existent key. Or produces
  // an error if there is no stack space, presumably.
  lua_getglobal(L, name);
  if (lua_isnil(L, -1)) {
    throw Fail();
  }
  if (lua_isfunction(L, -1)) {
    // Should always return the requested number of values, even if
    // must pad with nil values.
    lua_call(L, 0, 1); // void
    if (lua_isnil(L, -1)) {
      throw Fail();
    }
  }
}

extern "C" int cf_RcFile_get_int_or(cf_RcFile* self, const char* name, int dval)
{
  try {
    get_value(self->L, name);
  } catch(const Fail& e) {
    return dval;
  }
  if (!lua_isnumber(self->L, -1)) {
    return dval;
  }
  return lua_tointeger(self->L, -1);
}

// Note that unless you want to strdup the returned value, and if you want to keep it around for a while, then you better make sure (in your config file) that you are returning a pointer to a global Lua string.
// xxx We might want to internally query and strdup settings that cannot change during runtime.
extern "C" const char* cf_RcFile_get_str_maybe(cf_RcFile* self, const char* name)
{
  try {
    get_value(self->L, name);
  } catch(const Fail& e) {
    return NULL;
  }
  return lua_tostring(self->L, -1); // NULL if of wrong type
}

extern "C" const char* cf_RcFile_get_str_or(cf_RcFile* self, const char* name, const char* dval)
{
  const char* val = cf_RcFile_get_str_maybe(self, name);
  if (!val)
    return dval;
  return val;
}

extern "C" int cf_RcFile_vm_id(cf_RcFile* self)
{
  return (int)self->L;
}

#endif /* __FEATURE_RCFILE__ */

/**

cf_rcfile.cpp

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
