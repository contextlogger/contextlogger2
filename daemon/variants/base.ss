#lang scheme

#|

This file defines the API that all the variant specifications of this
project must implement.

|#

(require (lib "usual-4.ss" "common"))
(require konffaile/variant)
(require "../src/sa_sensor_list_spec.ss")
(require (lib "ast-util.scm" "wg"))

;; --------------------------------------------------
;; utilities
;; --------------------------------------------------

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (symbol-sort lst)
  (sort lst symbol<?))

(define (sublist? s lst)
  (true? (andmap (lambda (x) (memq x lst)) s)))

(define (include? e lst)
  (true? (memq e lst)))

;; --------------------------------------------------
;; sensors
;; --------------------------------------------------

(define FULL-SENSOR-LIST (cdr all-sensors))

(define (get-sensor-name sensor)
  (fget-reqd-nlist-elem-1 sensor 'name))

(define (sensor-essential? sensor)
  (true? (fget-opt-nlist-elem-1 sensor 'inactive)))

(define (enabled-symbol name)
  (string->symbol (format "~a-enabled" name)))

(define (symbol-sjoin lst)
  (string-join (map symbol->string lst) " "))

(define (make-sensor-attrs/sensor-pred p?)
  (for/hasheq
   ((sensor FULL-SENSOR-LIST))
   (alet name (get-sensor-name sensor)
         (values (enabled-symbol name)
                 (p? sensor)))))

(define essential-sensor-attrs
  (make-sensor-attrs/sensor-pred sensor-essential?))

;; Includes essential ones.
(define (make-sensor-attrs/name-pred p?)
  (for/hasheq
   ((sensor FULL-SENSOR-LIST))
   (alet name (get-sensor-name sensor)
         (values (enabled-symbol name)
                 (or (sensor-essential? sensor)
                     (p? name))))))

;; Includes essential ones.
(define (make-sensor-attrs/name-lst lst)
  (make-sensor-attrs/name-pred (lambda (name) (include? name lst))))

;; --------------------------------------------------
;; base variants
;; --------------------------------------------------

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

  (define/public (binary-type)
    'daemon) ;; 'application or 'daemon or 'static-lib
  
  (define/public (is-application.attr)
    (eq? (binary-type) 'application))
  
  (define/public (is-daemon.attr)
    (eq? (binary-type) 'daemon))

  (define/public (upload-time-expr.attr) "never")

  ;; This must be printable ASCII identifier, see "is_ascii_ident".
  ;; This really should be overridden in the config file.
  (define/public (username.attr) "john_doe")

  ;; The idea is that uploads to this URL will not work.
  ;; This really should be overridden in the config file.
  (define/public (upload-url.attr) "http://127.0.0.1:12345/dummy")

  ;; Likely such an ID will not exist. Again, the idea is to fail,
  ;; and keep failing until this is overridden via ConfigDb.
  (define/public (iap-id.attr) 99999)

  ;; --------------------------------------------------
  ;; features
  ;; --------------------------------------------------

  (define/public (feature-debugging.attr)
    #t)
  
  (define/public (do-logging.attr)
    (feature-debugging.attr))
  
  (define/public (feature-rcfile.attr)
    #t)

  (define/public (feature-uploader.attr)
    #t)

  (define/public (upload-with-curl.attr)
    (eq? (platform) 'linux))

  (define/public (feature-localserver.attr)
    (eq? (platform) 'symbian))

  (define/public (feature-remokon.attr)
    #t)

  (define/public (mark-enabled.attr)
    #t)

  (define/public (appmessage-enabled.attr)
    #t)

  ;; --------------------------------------------------
  ;; sensors
  ;; --------------------------------------------------

  (define/override (get-attrs) essential-sensor-attrs)
    
  )

;; --------------------------------------------------
;; Linux specific
;; --------------------------------------------------

(define-variant* linux-variant% project-variant%
  (super-new)
  
  (define/override (platform) 'linux)

  ;; No finished Linux implementation.
  (define/override (feature-uploader.attr) #f)

  (define/public (timer-enabled.attr) #t)

  )

;; --------------------------------------------------
;; Symbian specific
;; --------------------------------------------------

(define* SELF-CAPS-30 '(LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData))
(define* SELF-CAPS-32 (symbol-sort (cons 'Location SELF-CAPS-30)))
(define* DEV-CAPS (symbol-sort (append SELF-CAPS-32 '(PowerMgmt ProtServ ReadDeviceData SurroundingsDD SwEvent TrustedUI WriteDeviceData))))
(define* PUBLID-CAPS (symbol-sort (append DEV-CAPS '(CommDD DiskAdmin NetworkControl MultimediaDD))))

(define* ALL-SYMBIAN-SENSORS
  '(
    appfocus
    btprox
    callstatus
    cellid
    ;;flightmode
    gps
    inactivity
    indicator
    keypress
    profile
    smsevent
    ))

(define-variant* symbian-variant% project-variant%
  (super-new)
  
  (define/override (platform) 'symbian)
    
  (define/public (uid-v9.attr)
    (make-hexnum #xe8460002))
  
  (define/public (signed.attr)
    #t)
  
  (define/public (cert-name)
    'dev)
  
  (define/public (cert-name.attr)
    (symbol->string (cert-name)))
  
  (define/public (capabilities)
    DEV-CAPS)

  (define/public (capabilities.attr)
    (symbol-sjoin (capabilities)))

  (define/public (s60-vernum.attr) 30)

  (define/public (kit-name) 's60_30)
  
  (define/public (kit-name.attr)
    (symbol->string (kit-name)))

  ;; Abld build type. These days the options generally are "udeb",
  ;; "urel", and "all", but here only either udeb or urel is allowed.
  (define/public (abld-variant)
    (if (send this feature-debugging.attr) 'udeb 'urel))

  (define/public (abld-variant.attr)
    (symbol->string (abld-variant)))
  
  (define/public (have-devcert-caps.attr)
    (sublist? DEV-CAPS (capabilities)))

  (define/public (protected-uid-signable.attr)
    ;; Having a DevCert is not quite the same thing as having DevCert
    ;; caps, so override this as necessary.
    (have-devcert-caps.attr))
  
  ;; --------------------------------------------------
  ;; supportable components
  ;; --------------------------------------------------
  
  (define/public (watchdog-supported.attr)
    ;; Due to the requirement to use a protected development UID (or
    ;; the do proper Symbian signing), in practice we will only be
    ;; able to do this if we have a DevCert.
    (protected-uid-signable.attr))

  ;; --------------------------------------------------
  ;; available libs
  ;; --------------------------------------------------
  
  (define/public (have-profileengine-lib.attr)
    (>= (s60-vernum.attr) 31))
  
  (define/public (have-anim.attr) #t)

  (define/public (have-euserhl.attr) #t)

  ;; --------------------------------------------------
  ;; features
  ;; --------------------------------------------------
  
  ;; The "callstatus" sensor makes "flightmode" somewhat redundant. We
  ;; actually no longer even support "flightmode" as a standalone
  ;; sensor.
  (define/public (flightmode-enabled.attr)
    #f)
    
  ) ;; end symbian-variant%

(define-variant* devel-variant% symbian-variant%
  (init-field (binary-type/o 'application)
              (s60-vernum/o 30)
              (kit/o 's60_30)
              (sensor-list '())
              )
  
  (super-new)

  (define/override (binary-type) binary-type/o)
  
  (define/override (s60-vernum.attr) s60-vernum/o)
    
  (define/override (kit-name) kit/o)
    
  (define/override (get-attrs)
    (make-sensor-attrs/name-lst sensor-list))

  )

(define* (symbian-sensor-include ilist)
  ilist)

(define* (symbian-sensor-exclude elist)
  (filter
   (lambda (x) (not (memq x elist)))
   ALL-SYMBIAN-SENSORS))
  
(define-variant* release-variant% symbian-variant%
  (init-field caps/o
              cert/o
              (signed/o #t)
              (dist-variant-name #f)
              (s60-vernum/o 30)
              (binary-type/o 'daemon)
              (kit/o 's60_30))

  (super-new)

  (define/override (s60-vernum.attr) s60-vernum/o)
    
  (define/override (kit-name) kit/o)
    
  (define/override (binary-type) binary-type/o)

  (define/override (capabilities) caps/o)

  (define/override (signed.attr) signed/o)
  
  (define/override (cert-name) cert/o)

  (define/public (dist-variant-name.attr)
    (aif n dist-variant-name
         (symbol->string n)
         (send this variant-name.attr)))
         
  ;; --------------------------------------------------
  ;; sensors
  ;; --------------------------------------------------

  ;; In release builds we enable all sensors that we can, assuming
  ;; they make some sense. Redundant sensors and test sensors and such
  ;; we do not enable.
  
  (define/public (keypress-enabled.attr)
    (sublist? '(ReadDeviceData WriteDeviceData PowerMgmt ProtServ SwEvent)
              (capabilities)))
  
  (define/public (gps-enabled.attr)
    (sublist? '(Location)
              (capabilities)))
  
  (define/public (cellid-enabled.attr)
    (sublist? '(ReadDeviceData)
              (capabilities)))

  (define/public (profile-enabled.attr)
    (or (send this have-profileengine-lib.attr)
        (sublist? '(ReadDeviceData) (capabilities))))
  
  (define/override (get-attrs)
    (make-sensor-attrs/name-lst ALL-SYMBIAN-SENSORS))
    
  ) ;; end release-variant%

#|

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
