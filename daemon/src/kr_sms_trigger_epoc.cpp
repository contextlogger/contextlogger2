/*
  References:
  http://wiki.forum.nokia.com/index.php/SMS_Utilities_API
*/

#include "kr_sms_trigger_epoc.hpp"

#include <smsustrm.h> // read stream

CTOR_IMPL_CSmsTrigger;

void CSmsTrigger::ConstructL()
{
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iSocketServ, iSocketServ.Connect());
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iSocket, 
				     iSocket.Open(iSocketServ, 
						  KSMSAddrFamily, 
						  KSockDatagram, 
						  KSMSDatagramProtocol));
  _LIT8(KTag, "##cl");
  iSmsAddr.SetSmsAddrFamily(ESmsAddrMatchText); 
  iSmsAddr.SetTextMatch(KTag);
  User::LeaveIfError(iSocket.Bind(iSmsAddr));

  MakeRequest();
}

CSmsTrigger::~CSmsTrigger()
{
  Cancel();
  SESSION_CLOSE_IF_OPEN(iSocket);
  SESSION_CLOSE_IF_OPEN(iSocketServ);
}

void CSmsTrigger::RunL()
{
  TInt errCode = iStatus.Int();
  if (errCode) {
    RunError(errCode);
    return;
  }

  if (!iRead) {
    MakeRequest(); // next read
    return;
  }

  CSmsBuffer *smsBuffer = CSmsBuffer::NewL();
  CleanupStack::PushL(smsBuffer);
  // see enum TSmsPDUType
  CSmsMessage* message = CSmsMessage::NewL(iFs, CSmsPDU::ESmsDeliver, 
					   smsBuffer);
  CleanupStack::PushL(message);
  {
    RSmsSocketReadStream readStream(iSocket);
    CleanupClosePushL(readStream);
    message->InternalizeL(readStream);
    CleanupStack::PopAndDestroy(); // readStream
  }
  TInt bufLen = smsBuffer->Length(); // number of characters
  RBuf des;
  des.CreateL(bufLen);
  des.CleanupClosePushL();
  smsBuffer->Extract(des, 0, bufLen);

  logg("control message: '%S'", &des);

  CleanupStack::PopAndDestroy(3); // des, message, smsBuffer

  // Report success.
  {
    // Third arg ignored?
    iSocket.Ioctl(KIoctlReadMessageSucceeded, iStatus, &iPckgBuf, KSolSmsProv);
    iRead = EFalse;
    SetActive();
  }
}

void CSmsTrigger::DoCancel()
{
  iSocket.CancelIoctl();
}

TInt CSmsTrigger::RunError(TInt errCode)
{
  TRAPD(leaveCode, iObserver.SmsTriggerErrorL(errCode));
  if (leaveCode)
    iObserver.SmsTriggerLeave(leaveCode);
  return 0;
}

void CSmsTrigger::MakeRequest()
{
  if (IsActive())
    Cancel();
  iPckgBuf() = KSockSelectRead;
  iSocket.Ioctl(KIOctlSelect, iStatus, &iPckgBuf, KSOLSocket);
  iRead = ETrue;
  SetActive();
}

/**

Copyright 2011 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

 **/