#lang scheme

#|

This file defines the API that all the variant specifications of this
project must implement.

|#

(require (lib "usual-4.ss" "common"))
(require konffaile/variant)
(require (lib "ast-util.scm" "wg"))

(require "../src/sa_sensor_list_spec.ss")
(require (for-syntax "../src/sa_sensor_list_spec.ss"))

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

(define (symbol-sjoin lst)
  (string-join (map symbol->string lst) " "))

;; --------------------------------------------------
;; sensors
;; --------------------------------------------------

(define-syntax (define-sensor-methods stx)
  (syntax-case stx ()
    ((_)
     #`(begin
         #,@(map
             (lambda (sensor)
               (let* ((sensor-name (get-sensor-name sensor))
                      (method-sym (sensor-enabled-method-name sensor-name)))
                 #`(define/public (#,(datum->syntax stx method-sym))
                     #,(datum->syntax stx (sensor-essential? sensor)))))
             FULL-SENSOR-LIST)))))

(define-syntax (override-sensor-methods stx)
  (syntax-case stx ()
    ((_ plat on/off)
     #`(begin
         ;; Debugging aid.
         ;;(define/public (#,(datum->syntax stx 'foo))
         ;;(list #,@(map (lambda (sym) #`'#,sym)
         ;;(active-sensor-names-for (syntax->datum (syntax plat))))))
         #,@(map
             (lambda (sensor)
               (let* ((sensor-name (get-sensor-name sensor))
                      (method-sym (sensor-enabled-method-name sensor-name)))
                 #`(define/override (#,(datum->syntax stx method-sym))
                     on/off)))
             (active-sensors-for (syntax->datum (syntax plat))))))))

#|
(define (make-sensor-attrs/sensor-pred p?)
  (for/hasheq
   ((sensor FULL-SENSOR-LIST))
   (alet name (get-sensor-name sensor)
         (values (sensor-enabled-symbol name)
                 (p? sensor)))))

(define essential-sensor-attrs
  (make-sensor-attrs/sensor-pred sensor-essential?))

;; Includes essential ones.
(define (make-sensor-attrs/name-pred p?)
  (for/hasheq
   ((sensor FULL-SENSOR-LIST))
   (alet name (get-sensor-name sensor)
         (values (sensor-enabled-symbol name)
                 (or (sensor-essential? sensor)
                     (p? name))))))

;; Includes essential ones.
(define (make-sensor-attrs/name-lst lst)
  (make-sensor-attrs/name-pred (lambda (name) (include? name lst))))
|#

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
  
  (define/public (minor-version.attr) 14)
  
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

  (define/public (with-qt.attr)
    #f)

  (define/public (with-qmake.attr)
    (with-qt.attr))

  (define/public (with-libev.attr)
    (not (or (is-symbian.attr) (with-qt.attr))))

  (define/public (upload-time-expr.attr) "never")

  ;; This must be printable ASCII identifier, see "is_ascii_ident".
  ;; This really should be overridden in the config file.
  (define/public (username.attr) "john_doe")

  ;; No uploads by default. (A Lua expression.)
  (define/public (upload-url.attr) "nil")

  ;; No sensible IAP by default. (A Lua expression.)
  (define/public (iap-id-expr.attr) "-1")
  
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
    (eq? (platform) 'symbian))

  (define/public (upload-with-curl.attr)
    #f)

  (define/public (feature-localserver.attr)
    (eq? (platform) 'symbian))

  (define/public (feature-remokon.attr)
    #t)

  ;; --------------------------------------------------
  ;; sensors
  ;; --------------------------------------------------

  (define-sensor-methods)
  )

;; --------------------------------------------------
;; Linux specific
;; --------------------------------------------------

(define-variant* linux-variant% project-variant%
  (super-new)
  
  (define/override (platform) 'linux)

  ;; No finished Linux implementation.
  (define/override (feature-uploader.attr) #f)

  (override-sensor-methods linux #t)
  )

;; --------------------------------------------------
;; Symbian specific
;; --------------------------------------------------

(define* SELF-CAPS-30 '(LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData))
(define* SELF-CAPS-32 (symbol-sort (cons 'Location SELF-CAPS-30)))
(define* DEV-CAPS (symbol-sort (append SELF-CAPS-32 '(PowerMgmt ProtServ ReadDeviceData SurroundingsDD SwEvent TrustedUI WriteDeviceData))))
(define* PUBLID-CAPS (symbol-sort (append DEV-CAPS '(CommDD DiskAdmin NetworkControl MultimediaDD))))

(define* ALL-SYMBIAN-SENSORS (active-sensor-names-for 'symbian))

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
    (capabilities))

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
  
  (define/public (have-ahleclient-lib.attr)
    (and (>= (s60-vernum.attr) 30)
         (< (s60-vernum.attr) 50)))
  
  (define/public (have-ahle2client-lib.attr)
    (= (s60-vernum.attr) 50))
  
  (define/public (have-ahle.attr)
    (or (have-ahleclient-lib.attr)
        (have-ahle2client-lib.attr)))
  
  (define/public (have-anim.attr) #t)

  (define/public (have-epocxplat.attr) #t)

  (define/public (have-euserhl.attr) #t)

  ;; --------------------------------------------------
  ;; features
  ;; --------------------------------------------------
  
  ;; If a Contacts DB session is needed in the application context.
  (define/public (need-contact-database.attr)
    #t) ;; xxx
  
  ) ;; end symbian-variant%

(define-variant* symbian/all-variant% symbian-variant%
  (super-new)
  (override-sensor-methods symbian #t)
  )

(define-variant* devel-variant% symbian-variant%
  (init-field (binary-type/o 'application)
              (s60-vernum/o 30)
              (kit/o 's60_30)
              )
  
  (super-new)

  (define/override (binary-type) binary-type/o)
  
  (define/override (s60-vernum.attr) s60-vernum/o)
    
  (define/override (kit-name) kit/o)
  )

(define-variant* devel/all-variant% devel-variant%
  (super-new)
  (override-sensor-methods symbian #t)
  )

(define* (symbian-sensor-include ilist)
  ilist)

(define* (symbian-sensor-exclude elist)
  (filter
   (lambda (x) (not (memq x elist)))
   ALL-SYMBIAN-SENSORS))
  
(define-variant* release-variant% symbian/all-variant%
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

  ;; This may affect some of the deployment options. Private trial
  ;; releases may be packaged and deployed differently than public
  ;; releases.
  (define/public (is-trial.attr) #f)
         
  ;; --------------------------------------------------
  ;; sensors
  ;; --------------------------------------------------

  ;; In release builds we enable all sensors that we can, assuming
  ;; they make some sense. Redundant sensors and test sensors and such
  ;; we do not enable.
  
  (define/override (keypress-enabled.attr)
    (sublist? '(ReadDeviceData WriteDeviceData PowerMgmt ProtServ SwEvent)
              (capabilities)))
  
  (define/override (gps-enabled.attr)
    (sublist? '(Location)
              (capabilities)))
  
  (define/override (cellid-enabled.attr)
    (sublist? '(ReadDeviceData)
              (capabilities)))

  (define/override (weburl-enabled.attr)
    (or
     (and (send this have-ahleclient-lib.attr)
          (sublist? '(ReadDeviceData WriteDeviceData) (capabilities)))
     (and (send this have-ahle2client-lib.attr)
          (sublist? '(ReadUserData WriteUserData) (capabilities))))
    (and (send this have-epocxplat.attr)
         (sublist? '(ReadDeviceData WriteDeviceData) (capabilities)))
    )
  
  (define/override (profile-enabled.attr)
    (or (send this have-profileengine-lib.attr)
        (sublist? '(ReadDeviceData) (capabilities))))
  
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
