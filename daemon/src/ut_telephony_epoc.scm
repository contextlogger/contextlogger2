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
    (observer-name (format (if getter? "MGetterObs_~a" "M~aObserver") data-name))
    (notifier-name (format (if getter? "C~aGetter" "C~aNotifier") data-name))
    (handler-name (format (if getter? "GotData_~a" "ChangedData_~a") data-name)))

   (typedef
    (name data-alias)
    (type data-type))
   
   (typedef
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

(define program-1
  (cunit
    (basename (path->string program-basename))

    (includes
     (system-include "e32base.h")
     (system-include "etel3rdparty.h")
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
