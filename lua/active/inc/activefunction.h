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

#ifndef ACTIVEFUNCTION_H
#define ACTIVEFUNCTION_H

#include <e32base.h>
#include "lua.h"
#include "logger.h"

namespace ALua
	{
	enum TRequestorStatus 
		{
		EResume,
		EYield
		};
	
	typedef TRequestorStatus (*TRequestorFunction)(TInt aStatus, TAny* aPtr);
	typedef void (*TRequestorCancel)(TAny* aPtr);

	class TRequestor
		{
	public:
		inline TRequestor() : iFunc(NULL), iCancel(NULL), iPtr(NULL) {}
		inline TRequestor(TRequestorFunction aFunc, TRequestorCancel aCancel, TAny* aPtr) : iFunc(aFunc), iCancel(aCancel), iPtr(aPtr) {}
		inline TRequestorStatus operator ()(TInt aStatus) { return iFunc(aStatus, iPtr); }
		inline TBool IsSet() const { return (iFunc != NULL); }
		inline void Cancel() { iCancel(iPtr); }

	private:
		TRequestorFunction iFunc;
		TRequestorCancel iCancel;
		TAny* iPtr;
		};

	NONSHARABLE_CLASS(CActiveFunction) : public CActive
		{
	public:
		static CActiveFunction* NewL(lua_State* aState, RLogger& aLogger);
		~CActiveFunction();

		// CActive	
		void RunL();
		TInt RunError(TInt aError);
		void DoCancel();

		TInt Resume(lua_State* aState);
		TInt Yield();
		TInt Wait(TInt aMillis);

		inline TRequestStatus& RequestStatus() 
			{
			return iStatus;
			}

		inline void SetRequestor(const TRequestor& aRequestor)
			{
			iRequestor = aRequestor;
			}

		inline TInt IssueRequest()
			{
			SetActive();
			return lua_yield(iState, lua_gettop(iState));
			}

		inline void RaiseError(const TDesC8& aMsg, TInt aError)
			{
			}
		inline lua_State* State() const
			{
			return iState;
			}
		
	private:
		CActiveFunction();
		void ConstructL(lua_State* aState);

	private:
		lua_State* iState;
		RTimer iTimer;
		TRequestor iRequestor;
		};
	}

#endif // ACTIVEFUNCTION_H

