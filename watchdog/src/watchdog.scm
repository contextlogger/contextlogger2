#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (main)"
|#

#lang scheme

;;
;; Copyright 2008 Helsinki Institute for Information Technology (HIIT)
;; and Tero Hasu <tero.hasu@hut.fi>. All rights reserved.
;;
;; This license applies:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; Alternatively, this license applies:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

;; TODO:
;; 
;; * Consider using say the publish/subscribe mechanism for making
;;   some information about application death and startup available to
;;   the started application itself, for purposes of logging into the
;;   CL2 client LogDb database.
;; 
;; * And certainly do have the watchdog optionally log the same
;;   information into file(s), for manual checking.

(require (lib "usual.scm" "wg"))
(require (lib "ast-util.scm" "wg"))
(require (lib "file-util.scm" "wg"))
(require (lib "compact.scm" "wg"))
(require (lib "node-ctors-4.ss" "wg"))
(require (lib "mz.ss" "common"))
(require (lib "local-util.scm" "codegen"))
(require (lib "active-object.scm" "codegen"))

(define program-name (find-system-path 'run-file))
(define program-basename (path-drop-extension (path-basename program-name) ".scm"))

(define (logf fmt . args)
  (call 'logf (cons (cstr fmt) args)))

;; This defines the actual watchdog program.
(define program-1
  (unit
    (basename program-basename)

    (includes
     (system-include "e32base.h")
     (system-include "e32math.h")
     (system-include "sysutil.h")
     (system-include "w32std.h")
     (local-include "epoc-time.h")
     (local-include "timer_observer.h")
     (local-include "process_handle_observer.h")
     (local-include "common/assertions.h")
     ;;(local-include "common/epoc_app_uid_list.hrh")
     (local-include "common/logging.h")
     (local-include "common/panic.h")
     )

    (body

     (var static (name 'KNormalWait) (type (const 'TInt)) (init 30))

     ;; The watchdog class. This contains the logic of the watchdog
     ;; program.
     (let ((class-name 'CWatchdog))
       (class
         (name class-name) '(export #t)
         (bases 'CBase 'MTimerObserver 'MProcessHandleObserver)
         (body

          ;; You may see sensor_array.scm for examples of make-ctor.
          (let ((ctor-code
                 (sc
                  (call 'UpdateAppStartTime)
                  (cxx-line "iRandSeed = iAppStartTime.Int64();")
                  ))
                (construct-l-code
                 (sc
                  (cxx-line "User::LeaveIfError(iFs.Connect()); iFsOpen = ETrue;")
                  (cxx-line "User::LeaveIfError(iWsSession.Connect()); iWsSessionOpen = ETrue;")
                  (cxx-line "iTimerAo = CTimerAo::NewL(*this, CActive::EPriorityStandard);")
                  ;; It is okay to create this here, but no requests must be made before the "iProcess" handle has been opened.
                  (cxx-line "iLogonAo = CProcessHandleObserver::NewL(*this, CActive::EPriorityStandard, iProcess);")
                  )))
            (mz-call make-class-args
                     (list (list "Loop" (ref-to 'CActiveSchedulerWait)))
                     #:class-name class-name
                     #:newl? #t
                     #:newlc? #t
                     #:constructl? #t
                     #:ctor-code ctor-code
                     #:constructl-code construct-l-code
                     #:arg-list '()
                     #:init-list '()))

          (dtor
           public
           (block
            (cdelete 'iLogonAo)
            (cdelete 'iTimerAo)
            (call 'CloseProcess)
            (cif 'iWsSessionOpen (call-on 'iWsSession 'Close))
            (cif 'iFsOpen (call-on 'iFs 'Close))
            ))

          (func
           (name 'Start) public
           (block
            (call 'WaitWhile '(KNormalWait))
            ))

          (func
           (name 'UpdateAppStartTime) private
           (block
            (call-on 'iAppStartTime 'UniversalTime)
            ))

          (func
           (name 'WaitWhile) private
           (args (arg (type 'TInt) (name 'aSecs)))
           (block
            (cxx-line "iTimerAo->After(SecsToUsecs(aSecs));")
            ))

          ;; In a severe error situation you would use ExitWatchdog.
          ;; In other situations you might call this function, which
          ;; will resume watchdog work after increasing retry time
          ;; intervals.
          (func
           (name 'HandleMinorError) private
           (block
            (cxx-line "iNumMinorErrors++;")
            (cxx-line "TInt randFactor = (Math::Rand(iRandSeed) % 10);")
            (cxx-line "TInt waitTime = iNumMinorErrors * KNormalWait + randFactor;")
            (call 'WaitWhile '(waitTime))
            ))

          ;; This method may not be called with a closed "iProcess" handle.
          (func
           (name 'ObserveProcess) private
           (block
            (cxx-line "assert((iProcessOpen) && \"logic error\");")
            (call-via 'iLogonAo 'MakeRequest)
            ))

          ;; This method may not be called with an open "iProcess" handle.
          ;;
          ;; http://wiki.forum.nokia.com/index.php/How_to_start_and_stop_exe
          (func
           (name 'StartApp) private
           (block
            (cxx-line "assert((!iProcessOpen) && \"logic error\");")

            #;
            (cif (call 'AreResourcesLow)
                 (block
                  (call 'HandleMinorError)
                  (return)))
            
            (var (name 'errCode) (type 'TInt)
                 (init (call-on 'iProcess 'Create
                                (list (lit "\\sys\\bin\\cl2app.exe")
                                      'KNullDesC))))
            (logf "RProcess.Create %d" 'errCode)
            (sswitch 'errCode
                     (case 'KErrNone
                       (cxx-line "iProcessOpen = ETrue;")
                       (call 'UpdateAppStartTime)
                       (call 'ObserveProcess)
                       (call-on 'iProcess 'Resume))
                     (case 'KErrNotFound
                       ;; Do not know, there might be other causes for
                       ;; this error code.
                       (call 'ExitWatchdog
                             (list 'errCode
                                   (cstr "daemon binary not found"))))
                     (else (call 'HandleMinorError)))))

          ;; Better use TInt RProcess::RenameMe(const TDesC &aName) to
          ;; name the application process that we are looking for
          ;; here. Except it does not appear to work, not for
          ;; applications anyway, so we may have to search for the
          ;; process using a pattern.
          (func
           (name 'FindApp) private
           (block
            (cxx-line "assert((!iProcessOpen) && \"logic error\");")
            (cxx-line "_LIT(KProcessName, \"cl2app\");")
            (cxx-line "TInt errCode = iProcess.Open(KProcessName, EOwnerThread);")
            (cxx-line "logf(\"RProcess.Open(KProcessName) %d\", errCode);")
            (sswitch 'errCode
                     (case 'KErrNone
                       (cxx-line "iProcessOpen = ETrue;")
                       (call 'ObserveProcess))
                     (case 'KErrNotFound ;; the integer value is -1
                       (call 'FindAppByPattern))
                     (else (call 'HandleMinorError)))))

          (func
           (name 'FindAppByPattern) private
           (block
            (cxx-line "_LIT(KProcessSpec, \"*[e8460002]*\");")
            (cxx-line "TFindProcess processFinder(KProcessSpec);")
            (cxx-line "TFullName processName;")
            (cxx-line "TInt errCode = processFinder.Next(processName);")
            (cxx-line "logf(\"TFindProcess.Next(KProcessSpec) %d\", errCode);")
            (sswitch 'errCode
                     (case 'KErrNone
                       (cxx-line "
#if __DO_LOGGING__
    TBuf8<KMaxFullName+1> buf8;
    buf8.Copy(processName); // convert to ASCII
    logf(\"found '%s'\", (char*)buf8.PtrZ());
#endif
")
                       (cxx-line "errCode = iProcess.Open(processFinder, EOwnerThread);")
                       (cxx-line "logf(\"RProcess.Open(TFindProcess) %d\", errCode);")
                       (sswitch 'errCode
                                (case 'KErrNone
                                  (cxx-line "iProcessOpen = ETrue;")
                                  (call 'ObserveProcess))
                                (case 'KErrNotFound
                                  (call 'StartApp))
                                (else (call 'HandleMinorError))))
                     (case 'KErrNotFound
                       (call 'StartApp))
                     (else (call 'HandleMinorError)))))
          
          (func
           (name 'CloseProcess) private
           (block
            (cif 'iProcessOpen
                 (block
                  (call-on 'iProcess 'Close)
                  (assign 'iProcessOpen 'EFalse)))))

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

          #;
          (func
           (name 'AreResourcesLow) private
           (returns (type 'TBool))
           (block
            ;; We might consider checking memory levels also, but with a logger running we are more likely to run out of disk space.
            (cxx-line "TBool ret = EFalse; TRAPD(errCode, ret = SysUtil::MMCSpaceBelowCriticalLevelL(&iFs, 1000000)); return ret;")
            ))

          ;; This may get invoked as a response to a WaitWhile call.
          (func
           (name 'HandleTimerEvent) private
           (args (arg (type 'TInt) (name 'errCode)))
           (block
            (cif 'errCode
                 (call 'ExitWatchdog (list 'errCode (cstr "timer error")))
                 (call 'FindApp))))

          ;; We may get this invoked as a response to ObserveProcess call.
          (func
           (name 'HandleProcessHandleEvent) private
           (args (arg (type 'TInt) (name 'errCode)))
           (block
            ;; Probably we are also interested in the exit code of the
            ;; process. If we managed to even start observing the
            ;; process, that is. ExitReason, ExitCategory, and
            ;; ExitType may be of interest here. It would be nice to
            ;; make this information available to the logger process
            ;; itself.
            (cxx-line "logf(\"process observation result %d\", errCode);")
            
            ;; This ensures that we are in a known state after this
            ;; call. Note that we must collect any exit reason
            ;; information about the process already before making
            ;; this call.
            (call 'CloseProcess)
            
            (sswitch 'errCode
                     (case 'KErrNoMemory
                       (call 'HandleMinorError))
                     (case 'KErrCancel
                       ;; should not even get this error code here, though
                       )
                     (else
                      ;; Apparently any other code comes from the
                      ;; process itself. This API is a bit weird, why
                      ;; not return KErrNone and we could then just
                      ;; use ExitReason() to get the code.
                      (cif (call 'DiedQuickly)
                           (call 'HandleMinorError)
                           (call 'WaitWhile '(KNormalWait)))))
            ))

          (func
           (name 'ExitWatchdog) private
           (args (arg (type 'TInt) (name 'errCode))
                 (arg (type (ptr-to (const 'char))) (name 'errText)))
           (block
            ;; Note that here we should ensure that we are not doing
            ;; anything that would keep this nesting level of the
            ;; event loop from stopping. A mere Cancel should
            ;; certainly make that happen in this case.
            (call 'Cancel)
            (logf "exiting watchdog due to error: %s (%d)" 'errText 'errCode)
            (call-on 'iLoop 'AsyncStop)
            ))

          (func
           (name 'Cancel) private
           (block
            (call-via 'iTimerAo 'Cancel)
            (cif 'iLogonAo (call-via 'iLogonAo 'Cancel))))
          
          (func
           (name 'DiedQuickly) private
           (returns (type 'TBool))
           (block
            (var (name 'now) (type 'TTime))
            (call-on 'now 'UniversalTime)
            (var (name 'interval) (type 'TTimeIntervalSeconds))
            (var (name 'errCode) (type 'TInt))
            (assign 'errCode (call-on 'now 'SecondsFrom (list 'iAppStartTime 'interval)))
            (cxx-line "return (!errCode && (interval <= TTimeIntervalSeconds(10)));")))

          (var private (name 'iTimerAo) (type (ptr-to 'CTimerAo)))
          (var private (name 'iLogonAo) (type (ptr-to 'CProcessHandleObserver)))

          (var private (name 'iWsSession) (type 'RWsSession))
          (var private (name 'iWsSessionOpen) (type 'TBool))

          (var private (name 'iFs) (type 'RFs))
          (var private (name 'iFsOpen) (type 'TBool))
          
          ;; We get a handle either when we start the application, or
          ;; when we find it from the process list and then Open a
          ;; handle to the process.
          (var private (name 'iProcess) (type 'RProcess))
          (var private (name 'iProcessOpen) (type 'TBool))

          ;; The last time we started (or tried to start) the
          ;; application. If no app starting attempts have been made,
          ;; initialized to watchdog startup time. The purpose of this
          ;; is to find out if the started application dies (too)
          ;; quickly.
          (var private (name 'iAppStartTime) (type 'TTime))

          ;; Used to count consecutive failures. This affects the
          ;; amount of time waited before retrying.
          (var private (name 'iNumMinorErrors) (type 'TInt))

          (var private (name 'iRandSeed) (type 'TInt64))
          
          ))) ;; end watchdog AO
     
     ))) ; end program-1

(define* (main)
  ;;(pretty-nl program-1)
  (generate-h-and-cpp program-1)
  ;;(dump-h-and-cpp program-1)
  ;;(dump-analyzed-ast program-1)
  ;;(dump-compiled-ast program-1)
  (void))
