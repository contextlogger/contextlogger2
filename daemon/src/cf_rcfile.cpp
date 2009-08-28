#include "cf_rcfile.h"

#if __FEATURE_RCFILE__

#include "er_errors.h"
#include "lua_cl2.h"
#include "utils_cl2.h"

struct _cf_RcFile {
  gchar* username;
  gchar* upload_url;
};

#ifdef __EPOC32__
#define RCFILE_BASENAME "config.txt"
#else
#define RCFILE_BASENAME ".cl2rc"
#endif

#define RCFILE_DIR CONFIG_DIR
#define RCFILE_FILE (RCFILE_DIR DIR_SEP RCFILE_BASENAME)

#define return_with_error(s...) { if (error) *error = g_error_new(domain_cl2app, code_unspecified_error, s); return FALSE; }
#define return_with_oom { if (error) *error = NULL; return FALSE; }

static gboolean ReadRcFile(cf_RcFile* self, lua_State *L, GError** error)
{
  int errCode;

  if ((errCode = luaL_loadfile(L, RCFILE_FILE)) != 0) {
    if (errCode == LUA_ERRFILE) {
      // Could not open or read the file. This is okay since a
      // configuration file is not compulsory.
      logf("no (readable) configuration file '%s'", RCFILE_FILE);
      return TRUE;
    }

#if defined(__DO_LOGGING__)
    if (!lua_isnone(L, -1)) {
      const char* s = lua_tostring(L, -1);
      if (s) logt(s);
    }
#endif /* __DO_LOGGING__ */

    if (error) 
      *error = g_error_new(domain_cl2app, code_unspecified_error, "error parsing configuration file '%s'", RCFILE_FILE);
    return FALSE;
  }
  logt("config file parsed OK");

  if (lua_pcall(L, 0, 1, 0)) {
    if (error) 
      *error = g_error_new(domain_cl2app, code_unspecified_error, "error evaluating configuration file '%s'", RCFILE_FILE);
    return FALSE;
  }
  logt("config file evaluated OK");

  {
    lua_getfield(L, -1, "username");
    if (!lua_isnil(L, -1)) {
      const char* s = lua_tostring(L, -1);
      if (!s) return_with_error("value 'username' is not a string");
      if (!is_ascii_ident(s)) return_with_error("value 'username' is not a valid ident string");
      self->username = strdup(s);
      if (!self->username) return_with_oom;
    }
    lua_pop(L, 1);
  }
  
  {
    lua_getfield(L, -1, "upload_url");
    if (!lua_isnil(L, -1)) {
      const char* s = lua_tostring(L, -1);
      if (!s) return_with_error("value 'upload_url' is not a string");

      // We trust any uploader to do proper URL validation.
      if (!*s) return_with_error("value 'upload_url' may not be an empty string");

      self->upload_url = strdup(s);
      if (!self->upload_url) return_with_oom;
    }
    lua_pop(L, 1);
  }

#if defined(__DO_LOGGING__)
  if (self->username)
    logf("username configured to '%s'", self->username);
  if (self->upload_url)
    logf("upload_url configured to '%s'", self->upload_url);
#endif /* __DO_LOGGING__ */
  
  return TRUE;
}

extern "C" cf_RcFile* cf_RcFile_new(GError** error) 
{
  cf_RcFile* self = g_try_new0(cf_RcFile, 1);
  if (!self) {
    if (error) *error = NULL;
    return NULL;
  }

  // No, we are not providing access to _any_ libraries, either the
  // standard ones or the application specific ones.
  lua_State *L = lua_newstate(l_alloc, NULL);
  if (!L) {
    g_free(self);
    return NULL;
  }

  // On Symbian Lua shall recover from errors by doing a leave, and
  // lua_pcall should actually return in the case of an error. On
  // other platforms Lua will simply exit() as there is no panic
  // handler installed that would do a non-local return.
  WHEN_SYMBIAN(lua_atpanic(L, atpanic_leave));
  UNLESS_SYMBIAN(lua_atpanic(L, atpanic_print));

  if (!ReadRcFile(self, L, error)) {
    lua_close(L);
    cf_RcFile_destroy(self);
    return NULL;
  }

  lua_close(L);

  return self;
}
  
extern "C" void cf_RcFile_destroy(cf_RcFile* self)
{
  if (self) {
    g_free(self->username);
    g_free(self->upload_url);
    g_free(self);
  }
}

extern "C" gchar* cf_RcFile_get_username(cf_RcFile* self)
{
  return self->username;
}

#if __FEATURE_UPLOADER__
extern "C" gchar* cf_RcFile_get_upload_url(cf_RcFile* self)
{
  return self->upload_url;
}
#endif // __FEATURE_UPLOADER__

#endif /* __FEATURE_RCFILE__ */
