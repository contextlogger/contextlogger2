#lang scheme

#|

This file defines the API that all the variant specifications of this
project must implement.

|#

(require (lib "usual-4.ss" "common"))
(require konffaile/variant)

(define-variant* project-variant% variant%
  (super-new)

  (define/public (platform) 'symbian)

  (define/public (platform-str.attr) (symbol->string (platform)))
  
  (define/public (is-symbian.attr) (eq? (platform) 'symbian))
  
  (define/public (app-basename.attr) "cl2app")
  
  (define/public (app-name.attr) "CL2 App")

  (define/public (major-version.attr) 0)
  
  (define/public (minor-version.attr) 1)
  
  (define/public (version100.attr)
    (+ (* (major-version.attr) 100) (minor-version.attr)))
  
  (define/public (version-string.attr)
    (real->decimal-string
     (+ (major-version.attr) (/ (minor-version.attr) 100)) 2))

  ;; Only relevant for Symbian, of course.
  (define/public (uid-v9.attr)
    (make-hexnum #xe8460002))
  
  (define/public (binary-type)
    'daemon) ;; 'application or 'daemon or 'static-lib
  
  (define/public (is-application.attr)
    (eq? (binary-type) 'application))
  
  (define/public (is-daemon.attr)
    (eq? (binary-type) 'daemon))
  
  ;; --------------------------------------------------
  ;; features
  ;; --------------------------------------------------
  
  (define/public (feature-rcfile.attr)
    #t)

  (define/public (feature-uploader.attr)
    #t)

  (define/public (upload-with-curl.attr)
    (eq? (platform) 'linux))

  ;; --------------------------------------------------
  ;; sensors
  ;; --------------------------------------------------
  
  (define/public (transaction_enabled.attr)
    #t)
  
  (define/public (status_enabled.attr)
    #t)
  
  (define/public (timer_enabled.attr)
    (eq? (platform) 'linux))

  (define/public (flightmode_enabled.attr)
    #f)
  
  (define/public (profile_enabled.attr)
    #f)
  
  (define/public (cellid_enabled.attr)
    #f)
  
  (define/public (btprox_enabled.attr)
    #f)
  
  (define/public (gps_enabled.attr)
    #f)
  
  (define/public (appfocus_enabled.attr)
    #f)
  
  (define/public (keypress_enabled.attr)
    #f)

  ;; --------------------------------------------------
  ;; available libs
  ;; --------------------------------------------------
  
  (define/public (have-anim.attr)
    (eq? (platform) 'symbian))

  (define/public (have-euserhl.attr)
    (eq? (platform) 'symbian))
  
  )

(define-variant* linux-variant% project-variant%
    (super-new)
  
    (define/override (platform) 'linux)

    ;; No finished Linux implementation.
    (define/override (feature-uploader.attr) #f)

    )

(define-variant* symbian-variant% project-variant%
  (field (btype 'application) (slist '()))
  
  (super-new)

  (define/public (set-binary-type x) (set! btype x))
  
  (define/public (set-sensor-list x) (set! slist x))
  
  (define/override (platform) 'symbian)

  (define/override (binary-type) btype)
  
  (define/public (have-sensor? name)
    (true? (memq name slist)))
  
  (define/override (flightmode_enabled.attr)
    (have-sensor? 'flightmode))
  
  (define/override (profile_enabled.attr)
    (have-sensor? 'profile))
  
  (define/override (cellid_enabled.attr)
    (have-sensor? 'cellid))
  
  (define/override (btprox_enabled.attr)
    (have-sensor? 'btprox))
  
  (define/override (gps_enabled.attr)
    (have-sensor? 'gps))
  
  (define/override (appfocus_enabled.attr)
    (have-sensor? 'appfocus))
  
  (define/override (keypress_enabled.attr)
    (have-sensor? 'keypress))

  )

(define (all-symbian-sensors)
  '(flightmode profile cellid btprox gps appfocus keypress))

(define* (new-symbian-variant
          #:class (class symbian-variant%)
          #:btype (btype 'application)
          #:include (ilist #f)
          #:exclude (elist #f)
         )
  (let ((slist
         (cond
          (ilist ilist)
          (elist
           (filter
            (lambda (x) (not (memq x elist)))
            (all-symbian-sensors)))
          (else #f))))
    (alet obj (make-object class)
          (when btype
            (send obj set-binary-type btype))
          (when slist
            (send obj set-sensor-list slist))
          obj)))
