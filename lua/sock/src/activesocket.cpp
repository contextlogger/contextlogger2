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

#include "in_sock.h"
#include "activesocket.h"

using namespace ALua;

CActiveSocket* CActiveSocket::NewL(RSocketServ& aServer, TUint aAddrFamily, TUint aSockType, TUint aProtocol)
	{
	CActiveSocket* self = new (ELeave) CActiveSocket(aServer, aAddrFamily, aProtocol);
	CleanupStack::PushL(self);
	self->ConstructL(aSockType);
	CleanupStack::Pop(self);
	return self;
	}

CActiveSocket::CActiveSocket(RSocketServ& aServer, TUint aAddrFamily, TUint aProtocol) :
	iServer(aServer),
	iAddrFamily(aAddrFamily),
	iProtocol(aProtocol)
	{
	}

void CActiveSocket::ConstructL(TUint aSockType)
	{
	User::LeaveIfError(iSocket.Open(iServer, iAddrFamily, aSockType, iProtocol));
	User::LeaveIfError(iHostResolver.Open(iServer, iAddrFamily, iProtocol));
	}

CActiveSocket::~CActiveSocket()
	{
	iHostResolver.Close();
	iHostName.Close();
	iReadBuffer.Close();
	iWriteBuffer.Close();
	}

TInt CActiveSocket::Connect(CActiveFunction& aFunc, const TDesC8& aUri, TInt aPort)
	{
	iHostName.Close();
	TInt err = iHostName.Create(aUri.Length());

	if (err != KErrNone)
		{
		aFunc.RaiseError(_L8("Failed to allocate host name buffer"), err);
		return 0;
		}

	iHostName.Copy(aUri);
	iFunc = &aFunc;
	iPort = aPort;
	TRequestor req(CActiveSocket::OnResolve, CActiveSocket::CancelResolve, this);
	iFunc->SetRequestor(req);
	iHostResolver.GetByName(iHostName, iNameEntry, iFunc->RequestStatus());
	return iFunc->IssueRequest();
	}

TInt CActiveSocket::Read(CActiveFunction& aFunc, TInt aSize)
	{
	if (iReadBuffer.MaxSize() != aSize)
		{
		iReadBuffer.Close();
		TInt err = iReadBuffer.CreateMax(aSize);

		if (err != KErrNone)
			{
			aFunc.RaiseError(_L8("Failed to allocate read buffer"), err);
			return 0;
			}
		}
	else
		{
		iReadBuffer.Zero();
		}

	iFunc = &aFunc;
	TRequestor req(CActiveSocket::OnRead, CActiveSocket::CancelRead, this);
	iFunc->SetRequestor(req);
	iSocket.Read(iReadBuffer, iFunc->RequestStatus());
	// Push empty string onto stack, to be replaced with data received (if any)
	lua_pushstring(iFunc->State(), ""); 
	return iFunc->IssueRequest();
	}

TInt CActiveSocket::Write(CActiveFunction& aFunc, const TDesC8& aData)
	{
	iWriteBuffer.Close();
	TInt err = iWriteBuffer.Create(aData);

	if (err != KErrNone)
		{
		aFunc.RaiseError(_L8("Failed to allocate write buffer"), err);
		return 0;
		}

	iFunc = &aFunc;
	TRequestor req(CActiveSocket::OnWrite, CActiveSocket::CancelWrite, this);
	iFunc->SetRequestor(req);
	iSocket.Write(iWriteBuffer, iFunc->RequestStatus());
	return iFunc->IssueRequest();
	}

TInt CActiveSocket::Close()
	{
	if (iFunc)
		{
		iFunc->Cancel();
		}

	iSocket.Close();
	return 0;
	}
			
TRequestorStatus CActiveSocket::OnResolve(TInt aStatus, TAny* aPtr)
	{
	CActiveSocket* sock = static_cast<CActiveSocket*>(aPtr);
	sock->iHostName.Close();
	if (aStatus == KErrNone)
		{
		TRequestor req(CActiveSocket::OnConnect, CActiveSocket::CancelConnect, sock);
		sock->iFunc->SetRequestor(req);
		
		TInetAddr& inet = TInetAddr::Cast(sock->iNameEntry().iAddr);
		inet.SetPort(sock->iPort);
		
		sock->iSocket.Connect(inet, sock->iFunc->RequestStatus());
		return EYield;
		}
	else
		{
		// Raise error
		sock->iFunc->RaiseError(_L8("Resolving host failed"), aStatus);
		// Clear iFunc, we are done doing async invocations
		sock->iFunc = NULL;	
		return EResume;
		}
	}

void CActiveSocket::CancelResolve(TAny* aPtr)
	{
	CActiveSocket* sock = static_cast<CActiveSocket*>(aPtr);
	sock->iHostResolver.Cancel();
	}

TRequestorStatus CActiveSocket::OnConnect(TInt aStatus, TAny* aPtr)
	{
	CActiveSocket* sock = static_cast<CActiveSocket*>(aPtr);

	if (aStatus != KErrNone)
		{
		// Raise error	
		sock->iFunc->RaiseError(_L8("Connect failed"), aStatus);
		}

	// Clear iFunc, we are done doing async invocations
	sock->iFunc = NULL; 
	return EResume;
	}

void CActiveSocket::CancelConnect(TAny* aPtr)
	{
	CActiveSocket* sock = static_cast<CActiveSocket*>(aPtr);
	sock->iSocket.CancelConnect();
	}

TRequestorStatus CActiveSocket::OnRead(TInt aStatus, TAny* aPtr)
	{
	CActiveSocket* sock = static_cast<CActiveSocket*>(aPtr);
	
	if (aStatus != KErrEof &&
		aStatus != KErrDisconnected &&
		aStatus != KErrNone)
		{
		// Raise error	
		sock->iFunc->RaiseError(_L8("Read failed"), aStatus);
		}
	else
		{
		lua_pop(sock->iFunc->State(), 1);
		lua_pushlstring(sock->iFunc->State(), reinterpret_cast<const char*>(sock->iReadBuffer.Ptr()), sock->iReadBuffer.Size());
		}
	
	// Clear iFunc, we are done doing async invocations
	sock->iFunc = NULL; 
	return EResume;
	}

void CActiveSocket::CancelRead(TAny* aPtr)
	{
	CActiveSocket* sock = static_cast<CActiveSocket*>(aPtr);
	sock->iSocket.CancelRead();
	}

TRequestorStatus CActiveSocket::OnWrite(TInt aStatus, TAny* aPtr)
	{
	CActiveSocket* sock = static_cast<CActiveSocket*>(aPtr);

	if (aStatus != KErrNone)
		{
		// Raise error	
		sock->iFunc->RaiseError(_L8("Write failed"), aStatus);
		}

	// Clear iFunc, we are done doing async invocations
	sock->iFunc = NULL; 
	return EResume;
	}

void CActiveSocket::CancelWrite(TAny* aPtr)
	{
	CActiveSocket* sock = static_cast<CActiveSocket*>(aPtr);
	sock->iSocket.CancelWrite();
	}


