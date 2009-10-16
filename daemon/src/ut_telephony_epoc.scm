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
    (observer-name (format (if getter? "M~aRequestor" "M~aObserver") data-name))
    (notifier-name (format (if getter? "C~aGetter" "C~aNotifier") data-name))
    (handler-name (format (if getter? "HandleGot~a" "Handle~aChange") data-name)))
   
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
     )))
     
(define* (main)
  ;;(pretty-nl program-1)
  (generate-h-and-cpp program-1)
  ;;(dump-h-and-cpp program-1)
  ;;(dump-analyzed-ast program-1)
  ;;(dump-compiled-ast program-1)
  (void))
