#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (main)"
|#

#lang scheme

(require (lib "usual-4.ss" "common"))
(require (lib "mz.ss" "common"))
(require (lib "file-util.scm" "wg"))
(require (lib "compact.scm" "wg")) ;; for "sc" and "sc-list"
(require (lib "cxx-syntax.ss" "wg"))
(require (only-in (lib "node-ctors.scm" "wg") only-unless))
(require (lib "local-util.scm" "codegen"))

(define program-name (find-system-path 'run-file))
(define program-basename (path-drop-extension (path-basename program-name) ".scm"))

(define-syntax let*-sc
  (syntax-rules ()
    ((_ defs cdecl ...)
     (let* defs (sc cdecl ...)))))

(define (compact-list . args)
  (compact args))

(define (make-telephony-ao #:data-name data-name
                           #:data-type data-type
                           #:req-name req-name
                           #:cancel-name cancel-name
                           #:getter? (getter? #f)
                           )
  (let*-sc
   ((pckg-type (format "~aPckg" data-type))
    (data-alias (format "TData_~a" data-name))
    (pckg-alias (format "TDataPckg_~a" data-name))
    (observer-name (format (if getter? "MGetterObs_~a" "MNotifyObs_~a") data-name))
    (notifier-name (format (if getter? "CGetterAo_~a" "CNotifyAo_~a") data-name))
    (handler-name (format (if getter? "GotData_~a" "ChangedData_~a") data-name)))

   (typedef cexport
    (name data-alias)
    (type data-type))
   
   (typedef cexport
    (name pckg-alias)
    (type pckg-type))
   
   (cclass
    (name observer-name)
    cexport
    (body
     (func
      (name handler-name)
      (args (arg (name 'aError) (type 'TInt)))
      cpublic pure virtual)))

   (cclass
    (name notifier-name)
    (bases 'CActive)
    cexport non-sharable
    (body
     (mz-call make-ctor
              #:ctor-code (call 'CActiveScheduler::Add '(this))
              #:arg-list (list
                          (arg (name 'aTelephony)
                               (type (ref-to 'CTelephony)))
                          (arg (name 'aInterface)
                               (type (ref-to observer-name))))
              #:init-list (list
                           (ctor-super 'CActive '(EPriorityStandard))
                           (ctor-var 'iDataDes '(iData))
                           (ctor-var 'iTelephony '(aTelephony))
                           (ctor-var 'iInterface '(aInterface)))
              #:class-name notifier-name)

     (dtor cpublic virtual
           (block
            (call 'Cancel)))

     (func
      cpublic
      (name 'MakeRequest)
      (block
       (only-unless
        (call 'IsActive)
        (if getter?
            (call-on 'iTelephony req-name
                     (list 'iStatus 'iDataDes))
            (call-on 'iTelephony 'NotifyChange
                     (list 'iStatus req-name 'iDataDes)))
        (call 'SetActive))))

     (func cprivate virtual leaving (name 'RunL)
           (block
            ;; This may presumably also yield KErrNotSupported if there
         ;; device has no flight mode.
         (call-on 'iInterface handler-name
                  (list (call-on 'iStatus 'Int)))))

       (func cprivate virtual (name 'DoCancel)
        (block
         (call-on 'iTelephony 'CancelAsync
                  (list cancel-name))))

       (func
        (name 'Data)
        (returns (type (const-ref-to data-type)))
        cpublic non-modifying inline
        (block
         (return 'iData)))
        
       (var
        (name 'iData)
        (type data-type)
        cprivate)

       (var
        (name 'iDataDes)
        (type pckg-type)
        cprivate)
       
       (var
        (name 'iTelephony)
        (type (ref-to 'CTelephony))
        cprivate)
       
       (var
        (name 'iInterface)
        (type (ref-to observer-name))
        cprivate)
       ))))

(define (make-observer-ao #:data-name data-name
                          #:retries? (retries? #f)
                          #:flightmode? (flightmode? #f)
                          )
  (let*-sc
   ((data-type (format "TData_~a" data-name))
    (observer-name (format "MObserverObs_~a" data-name))
    (notifier-name (format "CObserverAo_~a" data-name))
    )

   (cclass
    (name observer-name)
    cexport
    (body
     (func
      (name (format "ObservedData_~a" data-name))
      cpublic pure virtual)

     (if retries?
         (sc
          (func
           (name (format "ReportTransientError_~a" data-name))
           (args (arg (name 'aError) (type 'TInt)))
           cpublic pure virtual)
          (func
           (name (format "RetriesExhausted_~a" data-name))
           cpublic pure virtual)
          )
         (func
          (name (format "Failed_~a" data-name))
          (args (arg (name 'aError) (type 'TInt)))
          cpublic pure virtual))
     
     (ic flightmode?
         (func
          (name (format "InFlightMode_~a" data-name))
          cpublic pure virtual))
     ))

   (cclass cexport non-sharable
    (name notifier-name)
    (apply bases 
          (compact-list
           'CBase
           (format "MGetterObs_~a" data-name)
           (format "MNotifyObs_~a" data-name)
           (and retries? "MRetryAoObserver")))
    (body
     (mz-call make-ctor
              #:newl? #t
              #:newlc? #t
              #:call-constructl? #t
              #:arg-list (list
                          (arg (name 'aAppContext)
                               (type (ptr-to 'ac_AppContext)))
                          (arg (name 'aInterface)
                               (type (ref-to observer-name))))
              #:init-list (list
                           (ctor-var 'iAppContext '(aAppContext))
                           (ctor-var 'iInterface '(aInterface)))
              #:class-name notifier-name)

     (dtor cpublic virtual
           (block
            (ic flightmode? (call 'BbUnregister))
            (ic retries? (cdelete 'iRetryAo))
            (cdelete 'iNotifier)
            (cdelete 'iGetter)))

     (var
      (name 'iAppContext)
      (type (ptr-to 'ac_AppContext)))

     (var
      (name 'iInterface)
      (type (ref-to observer-name)))
     
     (var (name 'iGetterDone) (type 'TBool))
     
     (var (name 'iGetter)
          (type (ptr-to (format "CGetterAo_~a" data-name))))
     
     (var (name 'iNotifier)
          (type (ptr-to (format "CNotifyAo_~a" data-name))))
     
     (ic retries?
         (var (name 'iRetryAo)
              (type (ptr-to 'CRetryAo))))

     (ic flightmode?
         (var (name 'iClosure)
              (type 'bb_Closure)))
     
     (func leaving
           (name 'ConstructL)
           (block
            (ic flightmode?
                 (call 'BbRegisterL))
            (ic retries?
                (assign 'iRetryAo
                        (leaving-new "CRetryAo"
                                     (list self 20 60))))
            (assign 'iGetter
                    (leaving-new (format "CGetterAo_~a" data-name)
                                 (list (call 'ac_Telephony '(iAppContext))
                                       self)))
            (assign 'iNotifier
                    (leaving-new (format "CNotifyAo_~a" data-name)
                                 (list (call 'ac_Telephony '(iAppContext))
                                       self)))
            (if flightmode?
                (only-unless (call 'GetFlightMode)
                             (call-via 'iGetter 'MakeRequest))
                (call-via 'iGetter 'MakeRequest))
            ))

     (ic flightmode?
         (func
          (name 'BbRegisterL)
          (block
           (assign (field-on 'iClosure 'changed)
                   (format "ClosureFunc_~a" data-name))
           (assign (field-on 'iClosure 'arg) 'this)
           )))
     
     (ic flightmode?
          (func
           (name 'BbUnregister)
           (block
            (call 'bb_Blackboard_unregister
                  (list
                   (call 'ac_get_Blackboard '(iAppContext))
                   'iClosure)))))

     (ic (or retries? flightmode?)
         (func
          (name 'MakeRequest)
          (block
           (cif 'iGetterDone
                (call-via 'iNotifier 'MakeRequest)
                (call-via 'iGetter 'MakeRequest)))))

     (ic flightmode?
         (func
          (name 'Cancel)
          (block
           (call-via 'iNotifier 'Cancel)
           (call-via 'iGetter 'Cancel)
           (ic retries?
               (call-via 'iRetryAo 'Cancel))
           (ic retries?
               (call-via 'iRetryAo 'ResetFailures))
           (assign 'iGetterDone 'EFalse))))
     
     (ic flightmode?
         (func non-modifying
          (name 'GetFlightMode)
          (returns (type 'TBool))
          (block
           (return (field-via
                    (call 'bb_Blackboard_board
                          (list 'ac_global_Blackboard))
                    'flightmode)))))
           
     (ic flightmode?
         (func cpublic
          (name 'HandleFlightModeChange)
          (block
           (cif (call 'GetFlightMode)
                (block
                 (call 'Cancel)
                 (call-on 'iInterface
                          (format "InFlightMode_~a" data-name)))
                (call 'MakeRequest)))))
     
     (ic retries?
         (func virtual
           (name 'RetryTimerExpired)
           (args
            (arg (type (ptr-to 'CRetryAo)))
            (arg (type 'TInt) (name 'aError)))
           (block
            (cif 'aError
                 (call 'er_log_symbian
                       (list 'er_FATAL 'aError
                             (cstr (format "retry timer error in ~a"
                                           notifier-name))))
                 (call 'MakeRequest)))))
     
     (func virtual
      (name (format "GotData_~a" data-name))
      (args (arg (type 'TInt) (name 'aError)))
      (block
       (call 'HandleData (list 'aError (call-via 'iGetter 'Data)))
       (only-unless 'aError
                    (assign 'iGetterDone 'ETrue))))

     (func virtual
      (name (format "ChangedData_~a" data-name))
      (args (arg (type 'TInt) (name 'aError)))
      (block
       (call 'HandleData (list 'aError (call-via 'iNotifier 'Data)))))

     (func
      (name "HandleData")
      (args
       (arg (type 'TInt) (name 'aError))
       (arg (type (const-ref-to data-type)) (name 'aData)))
      (block
       (cif 'aError
            (if retries?
                (block
                 (call-on 'iInterface
                          (format "ReportTransientError_~a" data-name)
                          (list 'aError))
                 (only-unless
                  (call-via 'iRetryAo 'Retry)
                  (call-on 'iInterface
                           (format "RetriesExhausted_~a" data-name))))
                (call-on 'iInterface
                         (format "Failed_~a" data-name)
                         (list 'aError)))
            (block
             (ic retries?
                 (call-via 'iRetryAo 'ResetFailures))
             (call-via 'iNotifier 'MakeRequest)
             (call-on 'iInterface (format "ObservedData_~a" data-name))))))
     ))

   (func
    (name (format "ClosureFunc_~a" data-name))
    (args
     (arg (type (ptr-to 'bb_Blackboard)))
     (arg (type "enum bb_DataType"))
     (arg (type 'gpointer))
     (arg (type 'int))
     (arg (type 'gpointer) (name 'arg)))
    (block
     (call-via (reinterpret-cast (ptr-to notifier-name) 'arg)
               'HandleFlightModeChange)
     ))
   ))

(define program-1
  (cunit
    (basename (path->string program-basename))

    (includes
     (local-include "ac_app_context.h")
     (local-include "bb_blackboard.h")
     (local-include "er_errors.h")
     (local-include "ut_retry_epoc.hpp")
     (system-include "e32base.h")
     (system-include "etel3rdparty.h")
     (system-include "glib.h")
     )

    (body
     (let ((data-name "FlightMode")
           (data-type 'CTelephony::TFlightModeV1)
           (req-name 'GetFlightMode)
           (cancel-name 'CTelephony::EGetFlightModeCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name
                          #:getter? #t))

     (let ((data-name "FlightMode")
           (data-type 'CTelephony::TFlightModeV1)
           (req-name 'CTelephony::EFlightModeChange)
           (cancel-name 'CTelephony::EFlightModeChangeCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name))

     (let ((data-name "CallStatus")
           (data-type 'CTelephony::TCallStatusV1)
           (req-name 'CTelephony::EVoiceLineStatusChange)
           (cancel-name 'CTelephony::EVoiceLineStatusChangeCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name))

     ;; Have we got enough battery to be running.
     (let ((data-name "BatteryInfo")
           (data-type 'CTelephony::TBatteryInfoV1)
           (req-name 'GetBatteryInfo)
           (cancel-name 'CTelephony::EGetBatteryInfoCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name
                          #:getter? #t))

     ;; Have we got enough battery to be running.
     (let ((data-name "BatteryInfo")
           (data-type 'CTelephony::TBatteryInfoV1)
           (req-name 'CTelephony::EBatteryInfoChange)
           (cancel-name 'CTelephony::EBatteryInfoChangeCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name))

     ;; Have we got good enough signal to upload.
     (let ((data-name "SignalStrength")
           (data-type 'CTelephony::TSignalStrengthV1)
           (req-name 'GetSignalStrength)
           (cancel-name 'CTelephony::EGetSignalStrengthCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name
                          #:getter? #t))

     ;; Have we got good enough signal to upload.
     (let ((data-name "SignalStrength")
           (data-type 'CTelephony::TSignalStrengthV1)
           (req-name 'CTelephony::ESignalStrengthChange)
           (cancel-name 'CTelephony::ESignalStrengthChangeCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name))

     ;; Are we ERegisteredOnHomeNetwork.
     (let ((data-name "NetworkRegistration")
           (data-type 'CTelephony::TNetworkRegistrationV1)
           (req-name 'GetNetworkRegistrationStatus)
           (cancel-name 'CTelephony::EGetNetworkRegistrationStatusCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name
                          #:getter? #t))

     ;; Are we ERegisteredOnHomeNetwork.
     (let ((data-name "NetworkRegistration")
           (data-type 'CTelephony::TNetworkRegistrationV1)
           (req-name 'CTelephony::ENetworkRegistrationStatusChange)
           (cancel-name 'CTelephony::ENetworkRegistrationStatusChangeCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name))

     ;; CTelephony::TNetworkInfoV2 is only available in more recent
     ;; platform versions, not in S60 3.0, for instance.
     (let ((data-name "NetworkInfo")
           (data-type 'CTelephony::TNetworkInfoV1)
           (req-name 'GetCurrentNetworkInfo)
           (cancel-name 'CTelephony::EGetCurrentNetworkInfoCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name
                          #:getter? #t))

     ;; Note that we are to expect the occasional KErrOverflow.
     ;; http://developer.symbian.org/forum/showthread.php?t=7474
     (let ((data-name "NetworkInfo")
           (data-type 'CTelephony::TNetworkInfoV1)
           (req-name 'CTelephony::ECurrentNetworkInfoChange)
           (cancel-name 'CTelephony::ECurrentNetworkInfoChangeCancel))
       (make-telephony-ao #:data-name data-name
                          #:data-type data-type
                          #:req-name req-name
                          #:cancel-name cancel-name))

     (let ((data-name "SignalStrength"))
       (make-observer-ao #:data-name data-name
                         #:retries? #t
                         #:flightmode? #t))
     
     )))
     
(define* (main)
  ;;(pretty-nl program-1)
  ;;(dump-analyzed-ast program-1)
  ;;(dump-compiled-ast program-1)
  ;;(dump-h-and-cpp program-1)
  (generate-h-and-cpp program-1)
  (void))

#|

ut_telephony_epoc.scm

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
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

|#
