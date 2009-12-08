/* 

Copyright (C) 2004  Mika Raento - Renaud Petit
email: mraento@cs.helsinki.fi - petit@cs.helsinki.fi 

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

Design:

We try to keep this module as small and free of dependencies as
possible. Hence no Open C or anything but Symbian native stuff here.
The whole thing is highly Symbian specific anyway.

We basically want to start a specific executable, and observe it. If
it dies, we attempt a restart after a little while; if it dies
quickly, increase the retry time interval; when system memory is very
low, do not even attempt a restart, but just reschedule.

We shall do some minimal logging or failures, but in release builds
the log must not grow indefinitely. Presently we have none in release
builds, and indefinitely growing (per runtime) in debug builds.
(Ideally we would check the file size to make sure that the log size
stays reasonable, and make it so that when the size limit is exceeded,
halve the size, keeping the latest entries only. But probably simpler
to just have the latest entry in the log, or, more generally, to
always clear the log after N entries.)

Details:

* Wait for a little while before doing anything, to hopefully ensure
that the system is somewhat up and running. Typically, after all, the
watchdog would be started at boot.

* Check if there is a magic file whose existence indicates there
should be no autostarting; if so, then die.

* Check if the application to launch is installed; if it is not,
then die, as there is little hope of launching something that does not
exist.

* Check if the application is already running by looking at the task
list. If it is, then get a handle to its main thread, and observe it
using Logon. If it is not, then start it.

* Wait for a little while, and see if the application is running. If
so, log that. If not, wait a bit longer, and keep checking. If still
not running log this fact, and set a fairly long retry period. If runs
but not for long, log that too, and again set a fairly long retry
period. We do not want to end up busy looping here.

To allow for possible client-server framework use, we shall be using
active objects to implement the watchdog logic. A timer and logon AO
should be just about enough here. We are presently not using the
client-server framework, but AOfication should also be useful for
testing, say when embedding the watchdog into a GUI app.

Nice to have would be (we presently do not): An in-memory log that
retains a number of recent entries, and such that CL2 can query for
those entries once running. Any entries served to a client should be
automatically deleted. Old entries not queried fast enough shall also
be deleted.

*/

#include "watchdog.h"

#include "common/epoc_app_uid_list.hrh"
#include "common/epoc-utilities.hpp"
#include "common/logging.h"
#include "common/panic.h"

#include <e32std.h>
#include <apgcli.h>
#include <bautils.h>

// We could consider having CActiveSchedulerWait owned by the watchdog
// object, as then the watchdog itself could implement a Loop method;
// no one else really uses CActiveSchedulerWait anyway.
void MainLoopL()
{
  CActiveSchedulerWait* loop = new (ELeave) CActiveSchedulerWait;
  CleanupStack::PushL(loop);
  // The watchdog will be running after its initialization. In error
  // situations it will invoke AsyncStop() on the passed loop.
  CWatchdog* watchdog = CWatchdog::NewLC(*loop);
  watchdog->Start();
  // AsyncStop will cause an exit from the loop, but we only want to
  // invoke AsyncStop in severe error situations. This means that if
  // Start() does return, then there must have been an error.
  loop->Start();
  CleanupStack::PopAndDestroy(watchdog);
  CleanupStack::PopAndDestroy(loop);
}

// This only works for applications, not your plain EXEs.
static TBool IsAppInstalledL()
{
#if 1
  return ETrue;
#else
  TBool result;
  RApaLsSession ls;
  User::LeaveIfError(ls.Connect());
  CleanupClosePushL(ls);
  TApaAppInfo appInfo;
  // Note that this returns false for applications which have not been
  // installed using a SIS file.
  TInt errCode = ls.GetAppInfo(appInfo, TUid::Uid(APP_UID_CL2_LOGGER_DAEMON));
  if (errCode == KErrNotFound)
    result = EFalse;
  else if (errCode)
    User::Leave(errCode);
  else
    result = ETrue;
  CleanupStack::PopAndDestroy(); // ls
  return result;
#endif
}

static TBool MagicFileExists(RFs& fs)
{
  _LIT(KFile, "c:\\data\\cl2\\disable_autostart.txt");
  TBool res = BaflUtils::FileExists(fs, KFile);
  return res;
}

static void WaitWhile()
{
  User::After(TTimeIntervalMicroSeconds32(7 * 1000000));
}

static void MainL()
{
  log_clear(PRIMARY_LOG_FILENAME);
  log_text(PRIMARY_LOG_FILENAME, "initializing");
  logf("value is %d", 555);
  log_ctx(PRIMARY_LOG_FILENAME, "context test");

  // Note that if our program does not run for at least five seconds
  // or so, the S60 auto launch mechanism will consider that an error,
  // and this may result in some error dialog popping up. We use
  // WaitWhile() to avoid that sort of thing where an intrusive error
  // report is not called for. Calling WaitWhile will hopefully also
  // ensure that the system is somewhat up and running even right after
  // boot, so that there are no problems with querying the installed
  // apps information or anything like that.
  WaitWhile();

  RFs fs;
  User::LeaveIfError(fs.Connect());
  CleanupClosePushL(fs);

  TBool magicFileExists = MagicFileExists(fs);
  if (!magicFileExists) {
    TBool isAppInstalled = IsAppInstalledL();
    if (isAppInstalled) {
      MainLoopL();
    } else {
      logt("program to launch not installed");
    }
  } else {
    logt("magic file exists");
  }

  CleanupStack::PopAndDestroy(); // fs
}

// Once this function gets invoked, the S60 autostart system should
// have seen to it that the system is already up and running. We may
// not want to hurry with launching anything else, however, as the
// system may still be somewhat busy right after (or in the later
// stages of) boot.
GLDEF_C TInt E32Main()
{
  int errCode = 0;
  __UHEAP_MARK;
  WITH_CLEANUP_STACK_ERR(errCode,
    WITH_ACTIVE_SCHEDULER_ERR(errCode,
      TRAP(errCode, MainL());
    );
  );
  __UHEAP_MARKEND;
  // xxx would like some way of logging error exit information in
  // non-debug builds also, here and elsewhere, but so that logs
  // cannot grow indefinitely; this means that we can should compile
  // in logging.cpp even when your usual logging is disabled; sounds
  // like we require another compilation option, and maybe some more
  // macros like log_always or somesuch
  return errCode;
}
