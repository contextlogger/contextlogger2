//
// socketaos.cpp
//
// Copyright 2004 Helsinki Institute for Information Technology (HIIT)
// and the authors.	 All rights reserved.
//
// Authors: Tero Hasu <tero.hasu@hut.fi>
//
// Some native active objects used for making and handling asynchronous
// socket requests. Symbian specific code and interface.
//

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation files
// (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Alternatively, licensed under LGPL.

#include "socketaos.hpp"

#include <in_sock.h>

// These definitions are different from our usual ones, and are not
// general purpose, but may be more efficient with the types that we
// use them with here.
#define IS_SESSION_OPEN(x) ((x).Handle() != 0)
#define IS_SUBSESSION_OPEN(x) ((x).SubSessionHandle() != 0)
#define SET_SESSION_OPEN(x)
#define SET_SESSION_CLOSED(x) Mem::FillZ(&x, sizeof(x))

// -----------------------------------------------------------
// CDnsResolver...

CDnsResolver::CDnsResolver(MGenericAoObserver& aObserver,
			   RSocketServ& aSocketServ,
			   RConnection* aConnection) :
  CActive(EPriorityStandard),
  iObserver(aObserver),
  iSocketServ(aSocketServ),
  iConnection(aConnection)
{
  CActiveScheduler::Add(this);
}

CDnsResolver::~CDnsResolver()
{
  // the CActive destructor will remove us from the active
  // scheduler list, right after this destructor has finished
  // executing
  Cancel();

  if (IS_SUBSESSION_OPEN(iHostResolver))
    {
      iHostResolver.Close();
      SET_SESSION_CLOSED(iHostResolver);
    }
}

void CDnsResolver::Resolve(const TDesC& aHostName)
{
  if (IsActive())
    {
      User::Invariant();
      return;
    }

  _LIT(KLocalHostName, "localhost");
  if (aHostName == KLocalHostName)
    {
      const TUint32 KLocalIpAddr = INET_ADDR(127,0,0,1);
      TInetAddr localIpAddr;
      localIpAddr.SetAddress(KLocalIpAddr);
      iNameEntry().iAddr = localIpAddr; // copy

      iStatus = KRequestPending;
      SetActive();
      TRequestStatus* status = &iStatus;
      User::RequestComplete(status, KErrNone);
      return;
    }

  // ensure that we have a resolver session we can use
  if (!IS_SUBSESSION_OPEN(iHostResolver))
    {
      TInt error;
      if (iConnection)
	{
	  error = iHostResolver.Open(iSocketServ, KAfInet, KProtocolInetUdp, 
				     *iConnection);
	}
      else
	{
	  error = iHostResolver.Open(iSocketServ, KAfInet, KProtocolInetUdp);
	}
      if (error != KErrNone && error != KErrAlreadyExists)
	{
	  iStatus = KRequestPending;
	  SetActive();
	  TRequestStatus* status = &iStatus;
	  User::RequestComplete(status, error);
	  return;
	}
    }
  SET_SESSION_OPEN(iHostResolver);

  iHostResolver.GetByName(aHostName, iNameEntry, iStatus);
  SetActive();
}

void CDnsResolver::DoCancel()
{
  if (IS_SUBSESSION_OPEN(iHostResolver))
    {
      iHostResolver.Cancel();
    }
}

void CDnsResolver::RunL()
{
  iObserver.AoEventOccurred(this, iStatus.Int());
  // note that the callback might do anything, such
  // as destroying this object, so it is imperative
  // that we do not do anything here -- certainly
  // not anything involving the property of this object
}

void CDnsResolver::GetResult(TSockAddr& aResult) const
{
  TNameRecord record = iNameEntry();
  TSockAddr& addr = record.iAddr;
  aResult = addr; // copy
}

// -----------------------------------------------------------
// CSocketConnecter...

CSocketConnecter::CSocketConnecter(MGenericAoObserver& aObserver,
				   RSocket& aSocket,
				   RSocketServ& aSocketServ) :
  CActive(EPriorityStandard),
  iObserver(aObserver),
  iSocket(aSocket),
  iSocketServ(aSocketServ)
{
  CActiveScheduler::Add(this);
}

CSocketConnecter::~CSocketConnecter()
{
  // the CActive destructor will remove us from the active
  // scheduler list, right after this destructor has finished
  // executing
  Cancel();
}

void CSocketConnecter::Connect(const TSockAddr& aServerAddress)
{
  if (IsActive())
    {
      User::Invariant();
      return;
    }

  iServerAddress = aServerAddress;
  iSocket.Connect(iServerAddress, iStatus);
  SetActive();
}

void CSocketConnecter::DoCancel()
{
  iSocket.CancelConnect();
}

void CSocketConnecter::RunL()
{
  TInt status = iStatus.Int();
  iObserver.AoEventOccurred(this, status);
  // note that the callback might do anything, such
  // as destroying this object, so it is imperative
  // that we do not do anything here -- certainly
  // not anything involving the property of this object
}

// -----------------------------------------------------------
// CResolvingConnecter...

CResolvingConnecter* CResolvingConnecter::NewL(MAoSockObserver& aObserver,
					       RSocket& aSocket,
					       RSocketServ& aSocketServ,
					       RConnection* aConnection)
{
  CResolvingConnecter* object = new (ELeave)
    CResolvingConnecter(aObserver, aSocket);
  CleanupStack::PushL(object);
  object->ConstructL(aSocketServ, aConnection);
  CleanupStack::Pop();
  return object;
}

CResolvingConnecter::CResolvingConnecter(MAoSockObserver& aObserver,
					 RSocket& aSocket) :
  CActive(EPriorityStandard),
  iObserver(aObserver),
  iSocket(aSocket)
{
  CActiveScheduler::Add(this);
}

void CResolvingConnecter::ConstructL(RSocketServ& aSocketServ,
				     RConnection* aConnection)
{
  iDnsResolver = new (ELeave) CDnsResolver(*this, aSocketServ, aConnection);
  iSocketConnecter = new (ELeave)
    CSocketConnecter(*this, iSocket, aSocketServ);
}

CResolvingConnecter::~CResolvingConnecter()
{
  // the CActive destructor will remove us from the active
  // scheduler list, right after this destructor has finished
  // executing
  Cancel();

  delete iDnsResolver;
  delete iSocketConnecter;
}

void CResolvingConnecter::Connect(const TDesC& aHostName, TInt aPort)
{
  if (IsActive())
    {
      User::Invariant();
      return;
    }

  iPort = aPort;

  iDnsResolver->Resolve(aHostName);
  iState = 1;

  iStatus = KRequestPending;
  SetActive();
}

void CResolvingConnecter::DoCancel()
{
  iDnsResolver->Cancel();
  iSocketConnecter->Cancel();

  // note that it is important that we do not signal
  // the same request twice
  if (iStatus == KRequestPending)
    {
      TRequestStatus* status = &iStatus;
      User::RequestComplete(status, KErrCancel);
    }
}

void CResolvingConnecter::RunL()
{
  iObserver.ClientConnected(iStatus.Int());
  // note that the callback might do anything, such
  // as destroying this object, so it is imperative
  // that we do not do anything here -- certainly
  // not anything involving the property of this object
}

void CResolvingConnecter::AoEventOccurred(CActive* aOrig, TInt aError)
{
  if (!IsActive())
    {
      User::Invariant();
      return;
    }
  if ((iState == 1) && (aOrig == iDnsResolver))
    {
      if (aError == KErrNone)
	{
	  TSockAddr addr;
	  iDnsResolver->GetResult(addr);
	  addr.SetPort(iPort);
	  iSocketConnecter->Connect(addr);
	  iState = 2;

	  return;
	}
    }
  else if ((iState == 2) && (aOrig == iSocketConnecter))
    {
      iState = 3;
    }
  else
    {
      User::Invariant();
    }
  TRequestStatus* status = &iStatus;
  User::RequestComplete(status, aError);
}

// -----------------------------------------------------------
// CSocketWriter...

CSocketWriter::CSocketWriter(MAoSockObserver& aObserver,
			     RSocket& aSocket) :
  CActive(EPriorityStandard),
  iObserver(aObserver),
  iSocket(aSocket)
{
  CActiveScheduler::Add(this);
}

CSocketWriter::~CSocketWriter()
{
  Cancel();
}

void CSocketWriter::DoCancel()
{
  iSocket.CancelWrite();
}

void CSocketWriter::RunL()
{
  iObserver.DataWritten(iStatus.Int());
  // note that the callback might do anything, such
  // as destroying this object, so it is imperative
  // that we do not do anything here
}

void CSocketWriter::WriteData(const TPtrC8& aData)
{
  if (IsActive())
    {
      User::Invariant();
      return;
    }
  iDataPtr.Set(aData);
  iSocket.Write(iDataPtr, iStatus);
  SetActive();
}

// -----------------------------------------------------------
// CSocketReader...

CSocketReader::CSocketReader(MAoSockObserver& aObserver,
			     RSocket& aSocket) :
  CActive(EPriorityStandard),
  iObserver(aObserver),
  iSocket(aSocket),
  iDataPtr(NULL, 0)
{
  CActiveScheduler::Add(this);
}

CSocketReader::~CSocketReader()
{
  Cancel();
}

void CSocketReader::DoCancel()
{
  iSocket.CancelRecv();
}

void CSocketReader::RunL()
{
  iObserver.DataRead(iStatus.Int());

  // note that the callback might do anything, such
  // as destroying this object, so it is imperative
  // that we do not do anything here
}

void CSocketReader::ReadSome(TPtr8 aDataPtr)
{
  if (IsActive())
    {
      User::Invariant();
      return;
    }

  iDataPtr.Set(aDataPtr);
  iDataPtr.Zero();
  // Reads at least one byte of data. iDataPtr->Length() will
  // give the number of bytes read after the operation
  // completes.
  iSocket.RecvOneOrMore(iDataPtr, 0, iStatus, iDummyLen);

  SetActive();
}
