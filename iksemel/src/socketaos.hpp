//
// socketaos.h
//
// Copyright 2004 Helsinki Institute for Information Technology (HIIT)
// and the authors.	 All rights reserved.
//
// Authors: Tero Hasu <tero.hasu@hut.fi>
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

#ifndef __SOCKETAOS_H__
#define __SOCKETAOS_H__

#include <e32std.h>
#include <es_sock.h>

// --------------------------------------------------------------------
// MAoSockObserver...

class MAoSockObserver
{
public:
  // Upon success, all of the passed data will have been written.
  virtual void DataWritten(TInt aError) = 0;

  // You can use Length() to get the amount of data read.
  virtual void DataRead(TInt aError) = 0;

  virtual void ClientConnected(TInt aError) = 0;
};

// --------------------------------------------------------------------
// for use internally only...

class MGenericAoObserver
{
public:
  virtual void AoEventOccurred(CActive* aOrig, TInt aError) = 0;
};

// --------------------------------------------------------------------
// CDnsResolver (active object)...

NONSHARABLE_CLASS(CDnsResolver) : public CActive
{
 public:
  /** it is okay for 'aConnection' to be NULL,
      in which case an implicit connection is created and used */
  CDnsResolver(MGenericAoObserver& aObserver,
	       RSocketServ& aSocketServ,
	       RConnection* aConnection);
  ~CDnsResolver();
  /** aHostName must persist long enough */
  void Resolve(const TDesC& aHostName);
  /** may call this after successful completion, before the next request */
  void GetResult(TSockAddr& aResult) const;
 protected:
  void DoCancel();
  void RunL();
 private:
  MGenericAoObserver& iObserver;
  RSocketServ& iSocketServ;
  RHostResolver iHostResolver;
  RConnection* iConnection; // not owned
  TNameEntry iNameEntry; // the result stored here
};

// --------------------------------------------------------------------
// CSocketConnecter (active object)...

NONSHARABLE_CLASS(CSocketConnecter) : public CActive
{
 public:
  /** takes an open socket as a parameter;
      this object attempts to connect it,
      but will not close or reopen the provided
      socket */
  CSocketConnecter(MGenericAoObserver& aObserver,
		   RSocket& aSocket,
		   RSocketServ& aSocketServ);
  ~CSocketConnecter();
  /** aServerAddress need not persist after call */
  void Connect(const TSockAddr& aServerAddress);
 protected:
  void DoCancel();
  void RunL();
 private:
  MGenericAoObserver& iObserver;
  RSocket& iSocket;
  RSocketServ& iSocketServ;
  TSockAddr iServerAddress;
};

// --------------------------------------------------------------------
// CResolvingConnecter (active object)...

NONSHARABLE_CLASS(CResolvingConnecter) : public CActive,
					 public MGenericAoObserver
{
 public:
  /** takes an open socket as a parameter;
      this object attempts to connect it,
      but will not close or reopen the provided
      socket;
      any RConnection is just passed to CDnsResolver */
  static CResolvingConnecter* NewL(MAoSockObserver& aObserver,
				   RSocket& aSocket,
				   RSocketServ& aSocketServ,
				   RConnection* aConnection);
  ~CResolvingConnecter();
  /** aServerAddress must persist long enough */
  void Connect(const TDesC& aHostName, TInt aPort);
 protected:
  void DoCancel();
  void RunL();
 private:
  CResolvingConnecter(MAoSockObserver& aObserver,
		      RSocket& aSocket);
  void ConstructL(RSocketServ& aSocketServ,
		  RConnection* aConnection);

  void AoEventOccurred(CActive* aOrig, TInt aError);
  MAoSockObserver& iObserver;
  RSocket& iSocket;
  CDnsResolver* iDnsResolver;
  CSocketConnecter* iSocketConnecter;
  TInt iPort;
  TInt iState; // 1 = resolving, 2 = connecting
};

// --------------------------------------------------------------------
// CSocketWriter (active object)...
//
// Note that the Series 60 SDK 'sockets' example provides
// a useful example for an active object like this.

NONSHARABLE_CLASS(CSocketWriter) : public CActive
{
 public:
  CSocketWriter(MAoSockObserver& aObserver, RSocket& aSocket);
  ~CSocketWriter();
  // the passed data must persist long enough
  void WriteData(const TPtrC8& aData);
  const TDesC8& Data() const { return iDataPtr; }
 protected:
  void DoCancel();
  void RunL();
 private:
  MAoSockObserver& iObserver;
  RSocket& iSocket;
  TPtrC8 iDataPtr;
};

// --------------------------------------------------------------------
// CSocketReader (active object)...
//
// Note that the Series 60 SDK 'sockets' example provides
// a useful example for an active object like this.

NONSHARABLE_CLASS(CSocketReader) : public CActive
{
 public:
  CSocketReader(MAoSockObserver& aObserver, RSocket& aSocket);
  ~CSocketReader();
  // Buffer pointed to must persist for long enough.
  void ReadSome(TPtr8 aDataPtr);
  TDesC8& Buffer() { return iDataPtr; }
 protected:
  void DoCancel();
  void RunL();
 private:
  MAoSockObserver& iObserver;
  RSocket& iSocket;
  TSockXfrLength iDummyLen;
  TPtr8 iDataPtr;
};

#endif // __SOCKETAOS_H__
