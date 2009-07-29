/*
 
Copyright (c) 2009 David Bolcsfoldi

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

*/

#include "active.h"
#include "activefunction.h"
#include "lauxlib.h"

using namespace ALua;

static TInt active_create(lua_State* L)
	{
	luaL_argcheck(L, lua_isfunction(L, 1) && !lua_iscfunction(L, 1), 1, "Lua function expected");

	CActiveFunction* func = NULL;
	TRAPD(err, func = CActiveFunction::NewL(L));

	if (err != KErrNone) 
		{
		return luaL_error(L, "Error creating active: %d", err);
		}

	lua_pushlightuserdata(L, func);
	return 1;
	}

static TInt active_resume(lua_State* L)
	{
	luaL_argcheck(L, lua_islightuserdata(L, 1), 1, "active expected");	
	CActiveFunction* func = static_cast<CActiveFunction*>(lua_touserdata(L, 1));
	luaL_argcheck(L, func, 1, "active expected");

	return func->Resume(L);
	}

static TInt active_yield(lua_State* L)
	{
	CActiveFunction* func = static_cast<CActiveFunction*>(active_ctx(L));
	luaL_argcheck(L, func, 1, "active expected");

	return func->Yield();
	}

static TInt active_wait(lua_State* L)
	{
	TInt millis = luaL_checkint(L, -1);
	CActiveFunction* func = static_cast<CActiveFunction*>(active_ctx(L));
	luaL_argcheck(L, func, 1, "active expected");

	return func->Wait(millis);
	}

static TInt active_start(lua_State* L)
	{
	CActiveScheduler::Start();
	return 0;
	}

static TInt active_stop(lua_State* L)
	{
	CActiveScheduler::Stop();
	return 0;
	}

static const struct luaL_Reg active[] = 
	{
	{"create", active_create},
	{"resume", active_resume},
	{"yield", active_yield},
	{"wait", active_wait},
	{"start", active_start},
	{"stop", active_stop},
	{NULL, NULL}
	};

EXPORT_C int active_open(lua_State* L)
	{
	CActiveScheduler* scheduler = new CActiveScheduler;

	if (!scheduler)
		{
		return 0;
		}

	CActiveScheduler::Install(scheduler);
	luaL_register(L, "active", active);
	return 1;
	}

