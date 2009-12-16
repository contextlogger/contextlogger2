#include "cf_rcfile.h"

#if __FEATURE_RCFILE__

#include "cf_rcfile_list_private.h"

#include "er_errors.h"
#include "lua_cl2.h"
#include "utils_cl2.h"

struct _cf_RcFile {
  DECLARE_STATE_ALL;
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
  logf("config file '%s' parsed OK", RCFILE_FILE);

  if (lua_pcall(L, 0, 1, 0)) {
    if (error) 
      *error = g_error_new(domain_cl2app, code_unspecified_error, "error evaluating configuration file '%s'", RCFILE_FILE);
    return FALSE;
  }
  logt("config file evaluated OK");

  STATE_INIT_ALL;

#if defined(__DO_LOGGING__)
  if (self->username) {
    logf("username configured to '%s'", self->username);
  }
  if (self->upload_url) {
    logf("upload_url configured to '%s'", self->upload_url);
  }
  if (self->remokon_host) {
    logf("remokon_host configured to '%s'", self->remokon_host);
  }
  if (self->iap) {
    logf("IAP expr configured to '%s'", self->iap);
  }
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

  lua_State *L = cl_lua_new_libs();
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
    CLEANUP_ALL;
    g_free(self);
  }
}

extern "C" {
#include "cf_rcfile_list.c"
}

#endif /* __FEATURE_RCFILE__ */
