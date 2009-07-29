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

#include "activefunction.h"
#include "lauxlib.h"

#include <e32debug.h>

using namespace ALua;

static void stackDump (lua_State *L) {
      int i;
      int top = lua_gettop(L);
      
      for (i = 1; i <= top; i++) {  /* repeat for each level */
        int t = lua_type(L, i);
        switch (t) {
    
          case LUA_TSTRING:  /* strings */
            RDebug::Printf("`%s'", lua_tostring(L, i));
            break;
    
          case LUA_TBOOLEAN:  /* booleans */
        	RDebug::Printf(lua_toboolean(L, i) ? "true" : "false");
            break;
    
          case LUA_TNUMBER:  /* numbers */
        	RDebug::Printf("%g", lua_tonumber(L, i));
            break;
    
          default:  /* other values */
        	RDebug::Printf("%s", lua_typename(L, t));
            break;
    
        }
        RDebug::Printf("  ");  /* put a separator */
      }
      RDebug::Printf("\n");  /* end the listing */
    }

CActiveFunction* CActiveFunction::NewL(lua_State* aState)
	{
	CActiveFunction* self = new (ELeave) CActiveFunction();
	CleanupStack::PushL(self);
	self->ConstructL(aState);
	CleanupStack::Pop(self);
	return self;
	}

CActiveFunction::CActiveFunction() :
	CActive(CActive::EPriorityStandard)
	{
	CActiveScheduler::Add(this);
	}

void CActiveFunction::ConstructL(lua_State* aState)
	{
	User::LeaveIfError(iTimer.CreateLocal());

	iState = lua_newthread(aState);
	User::LeaveIfNull(iState);
	// Store active context
	active_register(iState, this);

	// Copy lua function
	lua_pushvalue(aState, 1);
	// and move it to the new stack
	lua_xmove(aState, iState, 1);

	lua_atpanic(iState, CActiveFunction::PanicHandler);
	}

CActiveFunction::~CActiveFunction()
	{
	Cancel();
	iTimer.Close();
	}

TInt CActiveFunction::PanicHandler(lua_State* aState)
	{
	return static_cast<CActiveFunction*>(active_context(aState))->AtPanic();
	}

TInt CActiveFunction::AtPanic()
	{
	Deque();
	// Log to logger as well
	
	size_t length = 0;
	TUint8* str = static_cast<TUint8*>(lua_tolstring(iState, -1, &length));
	TPtrC8 logStmt(str, length);
	iLogger.Log("active Lua panic: %S\nTerminating.", logStmt);
	return 0;
	}

// CActive	
void CActiveFunction::RunL()
	{
	if (iRequestor.IsSet())
		{
		switch(iRequestor(iStatus.Int()))
			{
		case EResume:
			//stackDump(iState);
			lua_resume(iState, lua_gettop(iState) - 1);
			break;

		case EYield:
			if (lua_status(iState) != LUA_YIELD)
				{
				lua_yield(iState, lua_gettop(iState));
				}
			SetActive();
			break;
			}
		}
	else
		{
		lua_resume(iState, lua_gettop(iState) - 1);
		}
	}

TInt CActiveFunction::RunError(TInt aError)
	{
	return aError;
	}

void CActiveFunction::DoCancel()
	{
	if (iRequestor.IsSet())
		{
		iRequestor.Cancel();	
		}	
	iTimer.Cancel();
	}

TInt CActiveFunction::Resume(lua_State* aState)
	{
	iRequestor = TRequestor();
	// Copy parameters
	lua_xmove(aState, iState, lua_gettop(aState) - 1);
	// and do *hack*
	lua_setlevel(aState, iState);

	// And schedule ourselves for the first time
	TRequestStatus* status = &iStatus;
	User::RequestComplete(status, KErrNone);
	SetActive();
	return 0;
	}

TInt CActiveFunction::Yield()
	{
	iRequestor = TRequestor();
	
	TRequestStatus* status = &iStatus;
	User::RequestComplete(status, KErrNone);
	SetActive();
	return lua_yield(iState, lua_gettop(iState));
	}

TInt CActiveFunction::Wait(TInt aMillis)
	{
	iRequestor = TRequestor();
	// In microseconds
	iTimer.After(iStatus, TTimeIntervalMicroSeconds32(aMillis * 1000));
	SetActive();
	return lua_yield(iState, lua_gettop(iState));
	}

