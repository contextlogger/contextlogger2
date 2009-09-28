//
// Copyright 2009 Helsinki Institute for Information Technology (HIIT)
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

#include "ikst_transport.h"

#define INTERNAL_LOGGING 0

#include "socketaos.hpp"

#include <commdb.h>
#include <cdbpreftable.h>
#include <commdbconnpref.h>
#include <es_sock.h>
#include <in_sock.h>
#include <utf.h>

#define DEF_SESSION_OPEN(x) TBool x##IsOpen
#define DEF_SESSION(t,x) t x; DEF_SESSION_OPEN(x)
#define IS_SESSION_OPEN(x) (x##IsOpen)
#define SET_SESSION_OPEN(x) x##IsOpen = ETrue
#define SET_SESSION_CLOSED(x) x##IsOpen = EFalse
#define SESSION_CLOSE_IF_OPEN(x) { if (IS_SESSION_OPEN(x)) { x.Close(); SET_SESSION_CLOSED(x); } }
#define LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(x,act) { User::LeaveIfError(act); x##IsOpen = ETrue; }

/*
  !concept {:name => "Simple logging.",
            :desc => "Quick and dirty configurable logging."}
*/
#if INTERNAL_LOGGING
// This really assumes Open C console to do something meaningful.
#include <stdio.h>
#define log(f...) printf(f...)
#else
#define log(f...) ((void)0)
#endif

/***koog 
(require codegen/symbian-cxx)

(ctor-defines/spec
 "CTransport"
 "const char* aServerName, int aPort, int iapId, void* notify_data, ikst_NotifyFunc* notify_func"
 "iServerName(aServerName), iPort(aPort), iIapId(iapId), iUserdata(notify_data), iCallback(notify_func)" ;; initializers
 "" ;; ctor code
 #t)
 ***/
#define CTOR_DECL_CTransport  \
public: static CTransport* NewLC(const char* aServerName, int aPort, int iapId, void* notify_data, ikst_NotifyFunc* notify_func); \
public: static CTransport* NewL(const char* aServerName, int aPort, int iapId, void* notify_data, ikst_NotifyFunc* notify_func); \
private: CTransport(const char* aServerName, int aPort, int iapId, void* notify_data, ikst_NotifyFunc* notify_func); \
private: void ConstructL();

#define CTOR_IMPL_CTransport  \
CTransport* CTransport::NewLC(const char* aServerName, int aPort, int iapId, void* notify_data, ikst_NotifyFunc* notify_func) \
{ \
  CTransport* obj = new (ELeave) CTransport(aServerName, aPort, iapId, notify_data, notify_func); \
  CleanupStack::PushL(obj); \
  obj->ConstructL(); \
  return obj; \
} \
 \
CTransport* CTransport::NewL(const char* aServerName, int aPort, int iapId, void* notify_data, ikst_NotifyFunc* notify_func) \
{ \
  CTransport* obj = CTransport::NewLC(aServerName, aPort, iapId, notify_data, notify_func); \
  CleanupStack::Pop(obj); \
  return obj; \
} \
 \
CTransport::CTransport(const char* aServerName, int aPort, int iapId, void* notify_data, ikst_NotifyFunc* notify_func) : iServerName(aServerName), iPort(aPort), iIapId(iapId), iUserdata(notify_data), iCallback(notify_func) \
{}
/***end***/

NONSHARABLE_CLASS(CTransport) : 
  public CBase,
  public MAoSockObserver
{
  CTOR_DECL_CTransport;

 public:
  ~CTransport();

  TBool IsWriteActive() const { return iSocketWriter->IsActive(); }

  void Read(TPtr8& aBuffer);
  void Write(const TDesC8& aData);

 private: // MAoSockObserver
  virtual void DataWritten(TInt aError);
  virtual void DataRead(TInt aError);
  virtual void ClientConnected(TInt aError);

 private:
  void Connect();

 private:
  const char *iServerName; // not owned
  HBufC* iServerDes; // owned
  TInt iPort;
  TUint32 iIapId;
  DEF_SESSION(RSocketServ, iSocketServ);
  DEF_SESSION(RConnection, iConnection);
  DEF_SESSION(RSocket, iSocket);
  CResolvingConnecter* iSocketConnecter;
  CSocketWriter* iSocketWriter;
  CSocketReader* iSocketReader;
  void *iUserdata; // not owned
  ikst_NotifyFunc *iCallback;
  ikst_event iWriteEvent;
  ikst_event iReadEvent;
};

CTOR_IMPL_CTransport;

CTransport::~CTransport()
{
  delete iSocketConnecter;
  delete iSocketWriter;
  delete iSocketReader;
  SESSION_CLOSE_IF_OPEN(iSocket);
  SESSION_CLOSE_IF_OPEN(iSocketServ);
  delete iServerDes;
}

void CTransport::ConstructL()
{
  TPtrC8 serverDes8((TUint8*)iServerName);
  iServerDes = CnvUtfConverter::ConvertToUnicodeFromUtf8L(serverDes8);

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iSocketServ, iSocketServ.Connect());
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iConnection, iConnection.Open(iSocketServ));

  TCommDbConnPref connPref;
  connPref.SetDialogPreference(ECommDbDialogPrefDoNotPrompt);
  connPref.SetIapId(iIapId);
  User::LeaveIfError(iConnection.Start(connPref));

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iSocket,
				     iSocket.Open(iSocketServ, KAfInet,
						  KSockStream, KProtocolInetTcp,
						  iConnection));

  iSocketConnecter = CResolvingConnecter::NewL(*this, iSocket,
					       iSocketServ,
					       &iConnection);

  // Okay, so we could optimize by creating these later, but this is
  // in some sense cleaner, since we can assume existence of these
  // objects after construction.
  iSocketWriter = new (ELeave) CSocketWriter(*this, iSocket);
  iSocketReader = new (ELeave) CSocketReader(*this, iSocket);

  // Yes, we attempt to connect right after construction.
  Connect();
}

void CTransport::DataWritten(TInt aError)
{
  if (aError) {
    iWriteEvent.event = ikst_PLAT_ERROR;
    iWriteEvent.data0 = aError;
  } else {
    iWriteEvent.event = ikst_WRITE_OK;
    iWriteEvent.data0 = iSocketWriter->Data().Length();
  }
  (*(iCallback))(iUserdata, &iWriteEvent);
}

void CTransport::DataRead(TInt aError)
{
  if (aError == KErrEof) {
    iReadEvent.event = ikst_EOF;
  } else if (aError) {
    iReadEvent.event = ikst_PLAT_ERROR;
    iReadEvent.data0 = aError;
  } else {
    iReadEvent.event = ikst_READ_OK;
    iReadEvent.data0 = iSocketReader->Buffer().Length();
  }
  (*(iCallback))(iUserdata, &iReadEvent);
}

void CTransport::ClientConnected(TInt aError)
{
  if (aError) {
    iWriteEvent.event = ikst_PLAT_ERROR;
    iWriteEvent.data0 = aError;
  } else {
    iWriteEvent.event = ikst_CONNECTED;
  }
  (*(iCallback))(iUserdata, &iWriteEvent);
}

void CTransport::Connect()
{
  iSocketConnecter->Connect(*iServerDes, iPort);
}

void CTransport::Read(TPtr8& aBuffer)
{
  iSocketReader->ReadSome(aBuffer);
}

void CTransport::Write(const TDesC8& aData)
{
  iSocketWriter->WriteData(aData);
}

extern "C" 
void ikst_Close(void *socket)
{
  delete (CTransport*)socket;
}

// "data" will persist until request completion.
extern "C" 
int ikst_Send(void *socket, const char *data, size_t len)
{
  CTransport* self = (CTransport*)socket;
  TPtrC8 dataDes((TUint8*)data, len);
  /*
  TRAPD(errCode, self->WriteL(dataDes));
  if (errCode)
    return IKS_NET_RWERR;
  */
  self->Write(dataDes);
  return IKS_OK;
}

extern "C" 
int ikst_IsSendActive(void *socket)
{
  CTransport* self = (CTransport*)socket;
  return self->IsWriteActive();
}

// "buffer" will exist until request completion.
extern "C" 
int ikst_Recv(void *socket, char *buffer, size_t buf_len)
{
  CTransport* self = (CTransport*)socket;
  // this sets length to zero
  TPtr8 bufferDes(reinterpret_cast<TUint8*>(buffer), buf_len);
  self->Read(bufferDes);
  return IKS_OK;
}

extern "C" 
int ikst_Connect(void **socketptr, 
		 const char *server, int port, int iapId,
		 void *notify_data, ikst_NotifyFunc *notify_func)
{
  CTransport* self = NULL;
  log("ikst_Connect with IAP %d\n", iapId);
  TRAPD(errCode, self = CTransport::NewL(server, port, iapId, 
					 notify_data, notify_func));
  if (errCode) {
    // This is not the only possible cause, but we cannot both
    // interpreting all the conceivable Symbian error codes.
    return IKS_NOMEM;
  }

  *socketptr = self;

  return IKS_OK;
}
