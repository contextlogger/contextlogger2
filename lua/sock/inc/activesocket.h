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

#ifndef ACTIVESOCKET_H
#define ACTIVESOCKET_H

#include <e32base.h>
#include <es_sock.h>

#include "activefunction.h"

namespace ALua
	{
	NONSHARABLE_CLASS(CActiveSocket) : public CBase
		{
	public:
		static CActiveSocket* NewL(RSocketServ& aServer, TUint aAddrFamily, TUint aSockType, TUint aProtocol);
		~CActiveSocket();

		TInt Connect(CActiveFunction& aFunc, const TDesC8& aUri, TInt aPort);
		TInt Read(CActiveFunction& aFunc, TInt aSize);
		TInt Write(CActiveFunction& aFunc, const TDesC8& aData);
		TInt Close();
			
	private:
		CActiveSocket(RSocketServ& aServer, TUint aAddrFamily, TUint aProtocol);
		void ConstructL(TUint aSockType);

		static TRequestorStatus OnResolve(TInt aStatus, TAny* aPtr);
		static void CancelResolve(TAny* aPtr);

		static TRequestorStatus OnConnect(TInt aStatus, TAny* aPtr);
		static void CancelConnect(TAny* aPtr);

		static TRequestorStatus OnRead(TInt aStatus, TAny* aPtr);
		static void CancelRead(TAny* aPtr);

		static TRequestorStatus OnWrite(TInt aStatus, TAny* aPtr);
		static void CancelWrite(TAny* aPtr);

	private:
		RSocketServ& iServer;
		RHostResolver iHostResolver;
		TUint iAddrFamily;
		TUint iProtocol;
		RBuf iHostName;
		RBuf8 iReadBuffer;
		RBuf8 iWriteBuffer;
		RSocket iSocket;
		TNameEntry iNameEntry;
		CActiveFunction* iFunc;
		TInt iPort;
		};
	}

#endif // ACTIVESOCKET_H

