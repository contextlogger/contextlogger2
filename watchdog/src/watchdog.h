// -*- c++ -*-

#ifndef __WATCHDOG_H__
#define __WATCHDOG_H__

#include <e32base.h>
#include <e32math.h>
#include <sysutil.h>
#include <w32std.h>
#include "epoc-time.h"
#include "timer_observer.h"
#include "process_handle_observer.h"
#include "common/assertions.h"
#include "common/logging.h"
#include "common/panic.h"

// The watchdog class. This contains the logic of the watchdog
// program.
class CWatchdog :
  public CBase,
  public MTimerObserver,
  public MProcessHandleObserver
{
public:

  static CWatchdog *NewLC(CActiveSchedulerWait &aLoop);

  static CWatchdog *NewL(CActiveSchedulerWait &aLoop);

private:

  CWatchdog(CActiveSchedulerWait &aLoop);

  void ConstructL();

  CActiveSchedulerWait &iLoop;

public:

  ~CWatchdog();

  void Start();

private:

  void UpdateAppStartTime();

  void WaitWhile(TInt aSecs);

  // In a severe error situation you would use ExitWatchdog.
  // In other situations you might call this function, which
  // will resume watchdog work after increasing retry time
  // intervals.
  void HandleMinorError();

  // This method may not be called with a closed "iProcess" handle.
  void ObserveProcess();

  // This method may not be called with an open "iProcess" handle.
  //
  // http://wiki.forum.nokia.com/index.php/How_to_start_and_stop_exe
  void StartApp();

  void FindApp();

  void FindAppByPattern();

  void CloseProcess();

  // This may get invoked as a response to a WaitWhile call.
  void HandleTimerEvent(TInt errCode);

  // We may get this invoked as a response to ObserveProcess call.
  void HandleProcessHandleEvent(TInt errCode);

  void ExitWatchdog(TInt errCode, char const *errText);

  void Cancel();

  TBool DiedQuickly();

  CTimerAo *iTimerAo;

  CProcessHandleObserver *iLogonAo;

  RWsSession iWsSession;

  TBool iWsSessionOpen;

  RFs iFs;

  TBool iFsOpen;

  // We get a handle either when we start the application, or
  // when we find it from the process list and then Open a
  // handle to the process.
  RProcess iProcess;

  TBool iProcessOpen;

  // The last time we started (or tried to start) the
  // application. If no app starting attempts have been made,
  // initialized to watchdog startup time. The purpose of this
  // is to find out if the started application dies (too)
  // quickly.
  TTime iAppStartTime;

  // Used to count consecutive failures. This affects the
  // amount of time waited before retrying.
  TInt iNumMinorErrors;

  TInt64 iRandSeed;
};

#endif // __WATCHDOG_H__

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
