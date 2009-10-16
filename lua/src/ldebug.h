/*
** $Id: ldebug.h,v 2.3.1.1 2007/12/27 13:02:25 roberto Exp $
** Auxiliary functions from Debug Interface module
** See Copyright Notice in lua.h
*/

#ifndef ldebug_h
#define ldebug_h

#include "common/platform_config.h"

#include "lstate.h"


#define pcRel(pc, p)	(cast(int, (pc) - (p)->code) - 1)

#define getline(f,pc)	(((f)->lineinfo) ? (f)->lineinfo[pc] : 0)

#define resethookcount(L)	(L->hookcount = L->basehookcount)


LUAI_FUNC void luaG_typeerror (lua_State *L, const TValue *o,
                                             const char *opname);
LUAI_FUNC void luaG_concaterror (lua_State *L, StkId p1, StkId p2);
LUAI_FUNC void luaG_aritherror (lua_State *L, const TValue *p1,
                                              const TValue *p2);
LUAI_FUNC int luaG_ordererror (lua_State *L, const TValue *p1,
                                             const TValue *p2);
LUAI_FUNC void luaG_errormsg (lua_State *L);
LUAI_FUNC int luaG_checkcode (const Proto *pt);
LUAI_FUNC int luaG_checkopenop (Instruction i);

#if EXCEPTION_CRASHES_VARARGS_BUG
// workaround
LUAI_FUNC void luaG_runerror_wa (lua_State *L, const char *s);
#define luaG_runerror_1(_L, _s) luaG_runerror_wa(_L, _s)
#define luaG_runerror_m(_L, _s, rest...) luaG_runerror_wa(_L, _s)
#else
LUAI_FUNC void luaG_runerror_ok (lua_State *L, const char *fmt, ...);
#define luaG_runerror_1(_L, _s) luaG_runerror_ok(_L, _s)
#define luaG_runerror_m(_L, rest...) luaG_runerror_ok(_L, rest)
#endif

#endif
