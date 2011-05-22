// TODO:
// 
// * Consider using say the publish/subscribe mechanism for making
//   some information about application death and startup available to
//   the started application itself, for purposes of logging into the
//   CL2 client LogDb database.

#include "watchdog.h"

static TInt const KNormalWait = 30;

CWatchdog *CWatchdog::NewLC(CActiveSchedulerWait &aLoop)
{
  CWatchdog *object = new (ELeave) CWatchdog(aLoop);
  CleanupStack::PushL(object);
  object->ConstructL();
  return object;
}

CWatchdog *CWatchdog::NewL(CActiveSchedulerWait &aLoop)
{
  CWatchdog *object = NewLC(aLoop);
  CleanupStack::Pop();
  return object;
}

CWatchdog::CWatchdog(CActiveSchedulerWait &aLoop) : iLoop(aLoop)
{
  UpdateAppStartTime();
  iRandSeed = iAppStartTime.Int64();
}

void CWatchdog::ConstructL()
{
  User::LeaveIfError(iFs.Connect()); iFsOpen = ETrue;
  User::LeaveIfError(iWsSession.Connect()); iWsSessionOpen = ETrue;
  iTimerAo = CTimerAo::NewL(*this, CActive::EPriorityStandard);
  // It is okay to create this here, but no requests must be made before the "iProcess" handle has been opened.
  iLogonAo = CProcessHandleObserver::NewL(*this, CActive::EPriorityStandard, iProcess);
}

CWatchdog::~CWatchdog()
{
  (delete iLogonAo);
  (delete iTimerAo);
  CloseProcess();
  if (iWsSessionOpen) {
    iWsSession.Close();
  }
  if (iFsOpen) {
    iFs.Close();
  }
}

void CWatchdog::Start()
{
  WaitWhile(KNormalWait);
}

void CWatchdog::UpdateAppStartTime()
{
  iAppStartTime.UniversalTime();
}

void CWatchdog::WaitWhile(TInt aSecs)
{
  iTimerAo->After(SecsToUsecs(aSecs));
}

void CWatchdog::HandleMinorError()
{
  iNumMinorErrors++;
  TInt randFactor = (Math::Rand(iRandSeed) % 10);
  TInt waitTime = iNumMinorErrors * KNormalWait + randFactor;
  WaitWhile(waitTime);
}

void CWatchdog::ObserveProcess()
{
  assert((iProcessOpen) && "logic error");
  iLogonAo->MakeRequest();
}

void CWatchdog::StartApp()
{
  assert((!iProcessOpen) && "logic error");
  _LIT(literalDes1128, "\\sys\\bin\\cl2app.exe");
  TInt errCode = iProcess.Create(literalDes1128, KNullDesC);
  logg("RProcess.Create %d", errCode);
  switch (errCode) {
  case KErrNone:
    {
      iProcessOpen = ETrue;
      UpdateAppStartTime();
      ObserveProcess();
      iProcess.Resume();
      break;
    }
    // Expect e.g. KErrNotFound when the binary to start is not present.
  default:
    {
      HandleMinorError();
    }
  }
}

void CWatchdog::FindApp()
{
  assert((!iProcessOpen) && "logic error");

  // Better use TInt RProcess::RenameMe(const TDesC &aName) to
  // name the application process that we are looking for
  // here. Except it does not appear to work, not for
  // applications anyway, so we may have to search for the
  // process using a pattern.
  _LIT(KProcessName, "cl2app");
  TInt errCode = iProcess.Open(KProcessName, EOwnerThread);
  logg("RProcess.Open(KProcessName) %d", errCode);
  switch (errCode) {
  case KErrNone:
    {
      iProcessOpen = ETrue;
      ObserveProcess();
      break;
    }
  case KErrNotFound:
    {
      FindAppByPattern();
      break;
    }
  default:
    {
      HandleMinorError();
    }
  }
}

void CWatchdog::FindAppByPattern()
{
  _LIT(KProcessSpec, "*[e8460002]*");
  TFindProcess processFinder(KProcessSpec);
  TFullName processName;
  TInt errCode = processFinder.Next(processName);
  logg("TFindProcess.Next(KProcessSpec) %d", errCode);
  switch (errCode) {
  case KErrNone:
    {
      
#if __DO_LOGGING__
    TBuf8<KMaxFullName+1> buf8;
    buf8.Copy(processName); // convert to ASCII
    logg("found '%s'", (char*)buf8.PtrZ());
#endif

      errCode = iProcess.Open(processFinder, EOwnerThread);
      logg("RProcess.Open(TFindProcess) %d", errCode);
      switch (errCode) {
      case KErrNone:
        {
          iProcessOpen = ETrue;
          ObserveProcess();
          break;
        }
      case KErrNotFound:
        {
          StartApp();
          break;
        }
      default:
        {
          HandleMinorError();
        }
      }
      break;
    }
  case KErrNotFound:
    {
      StartApp();
      break;
    }
  default:
    {
      HandleMinorError();
    }
  }
}

void CWatchdog::CloseProcess()
{
  if (iProcessOpen) {
    iProcess.Close();
    (iProcessOpen = EFalse);
  }
}

void CWatchdog::HandleTimerEvent(TInt errCode)
{
  if (errCode) {
    ExitWatchdog(errCode, "timer error");
  } else {
    FindApp();
  }
}

void CWatchdog::HandleProcessHandleEvent(TInt errCode)
{
  // Probably we are also interested in the exit code of the
  // process. If we managed to even start observing the
  // process, that is. ExitReason, ExitCategory, and
  // ExitType may be of interest here. It would be nice to
  // make this information available to the logger process
  // itself.
  logg("process observation result %d", errCode);

  // This ensures that we are in a known state after this
  // call. Note that we must collect any exit reason
  // information about the process already before making
  // this call.
  CloseProcess();

  switch (errCode) {
  case KErrNoMemory:
    {
      HandleMinorError();
      break;
    }
  case KErrCancel:
    {
      // should not even get this error code here, though
      break;
    }
  default:
    // Apparently any other code comes from the
    // process itself. This API is a bit weird, why
    // not return KErrNone and we could then just
    // use ExitReason() to get the code.
    {
      if (DiedQuickly()) {
        HandleMinorError();
      } else {
        WaitWhile(KNormalWait);
      }
    }
  }
}

void CWatchdog::ExitWatchdog(TInt errCode, char const *errText)
{
  // Note that here we should ensure that we are not doing
  // anything that would keep this nesting level of the
  // event loop from stopping. A mere Cancel should
  // certainly make that happen in this case.
  Cancel();
  logg("exiting watchdog due to error: %s (%d)", errText, errCode);
  iLoop.AsyncStop();
}

void CWatchdog::Cancel()
{
  iTimerAo->Cancel();
  if (iLogonAo) {
    iLogonAo->Cancel();
  }
}

TBool CWatchdog::DiedQuickly()
{
  TTime now;
  now.UniversalTime();
  TTimeIntervalSeconds interval;
  TInt errCode;
  (errCode = now.SecondsFrom(iAppStartTime, interval));
  return (!errCode && (interval <= TTimeIntervalSeconds(10)));
}

/*
  ;; This function is used to check whether resources are low
  ;; prior to even attempting to start up the application.
  ;; This check failing is treated similarly to failure in
  ;; actually starting the application. And if we fail to do
  ;; the check, then presumably resources are running low, and
  ;; we can just return EFalse.
  ;; 
  ;; This simplistic AreResourcesLow check is no longer
  ;; appropriate now that the logging medium is configurable.
  ;; Also, this probably returns EFalse if there is no medium
  ;; in the E: drive.

  (func
  (name 'AreResourcesLow) private
  (returns (type 'TBool))
  (block
  ;; We might consider checking memory levels also, but with a logger running we are more likely to run out of disk space.
  (cxx-line "TBool ret = EFalse; TRAPD(errCode, ret = SysUtil::MMCSpaceBelowCriticalLevelL(&iFs, 1000000)); return ret;")
  ))
*/

/** 
    Copyright 2008 Helsinki Institute for Information Technology (HIIT)
    and Tero Hasu <tero.hasu@hut.fi>. All rights reserved.

    This license applies:

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Alternatively, this license applies:

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
 */
