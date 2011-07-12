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

;; --------------------------------------------------
;; base variants
;; --------------------------------------------------

(define-variant* project-variant% variant%
  (super-new)

  (define/public (platform) 'symbian)

  (define/public (platform-str.attr) (symbol->string (platform)))
  
  (define/public (is-symbian.attr) (eq? (platform) 'symbian))
  
  (define/public (app-basename.attr) "cl2app")
  
  (define/public (app-name.attr) "CL2 Logger")

  (define/public (major-version.attr) 0)
  
  (define/public (minor-version.attr) 22)
  
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

  (define/public (have-signal.attr)
    #t)
  
  (define/public (with-qt.attr)
    #f)

  (define/public (with-qmake.attr)
    (and (with-qt.attr) (not (is-symbian.attr))))

  (define/public (with-qt-gui.attr)
    (and (with-qt.attr) (is-application.attr)))

  (define/public (with-qt-xml.attr)
    (and (with-qt.attr) (send this feature-remokon.attr)))

  (define/public (with-qt-mobility.attr)
    (and (with-qt.attr) (is-symbian.attr)))

  (define/public (with-libev.attr)
    (not (or (is-symbian.attr) (with-qt.attr))))

  (define/public (have-sqlite3.attr)
    ;; The Symbian Qt SDK plugin also exposes the standard SQLite3
    ;; API, and Qt itself depends on the library. Symbian also used to
    ;; provide an SQLite3 (without Qt), but not sure what platform
    ;; versions it was compatible with.
    (or (not (is-symbian.attr)) (with-qt.attr)))

  (define/public (use-sqlite3h.attr)
    (not (have-sqlite3.attr)))
  
  (define/public (lua-from-source.attr)
    #f)
  
  (define/public (lua-as-static-lib.attr)
    (and (is-symbian.attr) (not (lua-from-source.attr))))
  
  (define/public (upload-time-expr.attr) "never")

  ;; This must be printable ASCII identifier, see "is_ascii_ident".
  ;; This really should be overridden in the config file.
  (define/public (username.attr) "john_doe")

  ;; No uploads by default. (A Lua expression.)
  (define/public (upload-url.attr) "nil")

  ;; No remote control by default. (A Lua expression.)
  (define/public (remokon-host.attr) "nil")

  ;; No remote control by default. (A Lua expression.)
  (define/public (remokon-jid.attr) "nil")

  ;; No sensible IAP by default. (A Lua expression.)
  (define/public (iap-id-expr.attr) "-1")
  
  ;; --------------------------------------------------
  ;; sensors
  ;; --------------------------------------------------

  (define-sensor-methods)

  ;; Whether any tap sensor is enabled.
  (define/public (with-tap-sensors.attr)
    (or (singletap-enabled.attr)
        (doubletap-enabled.attr)))
  
  (define/public (use-qt-mobility.attr)
    (or (light-enabled.attr)
        (proximity-enabled.attr)
        (with-tap-sensors.attr)))

  ;; Whether to link against QtSensors and to build in our QtSensors
  ;; utility code.
  (define/public (use-qt-sensors.attr)
    (use-qt-mobility.attr))
  
  ;; --------------------------------------------------
  ;; features
  ;; --------------------------------------------------

  (define/public (feature-debugging.attr)
    #t)

  (define/public (feature-guilog.attr)
    (with-qt-gui.attr))
  
  (define/public (do-logging.attr)
    (feature-debugging.attr))
  
  (define/public (feature-rcfile.attr)
    #t)

  (define/public (feature-uploader.attr)
    (or (eq? (platform) 'symbian)
        (and (eq? (platform) 'linux)
             (with-qt.attr))))

  (define/public (upload-with-qt.attr)
    (eq? (platform) 'linux))

  (define/public (feature-localserver.attr)
    (eq? (platform) 'symbian))

  (define/public (feature-remokon.attr)
    #f)

  (define/public (feature-compress-logs.attr)
    #t)

  (define/public (quit-on-low-battery.attr)
    #t)
  
  )

;; --------------------------------------------------
;; Linux specific
;; --------------------------------------------------

(define-variant* linux-variant% project-variant%
  (super-new)
  
  (define/override (platform) 'linux)

  (define/override (mark-enabled.attr) #t)
  (define/override (timer-enabled.attr) #t)
  (define/override (light-enabled.attr) (send this with-qt-mobility.attr))
  (define/override (singletap-enabled.attr) (send this with-qt-mobility.attr))
  (define/override (doubletap-enabled.attr) (send this with-qt-mobility.attr))
  (define/override (proximity-enabled.attr) (send this with-qt-mobility.attr))
  )

;; --------------------------------------------------
;; Symbian specific
;; --------------------------------------------------

(define* SELF-CAPS-30 '(LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData))
(define* SELF-CAPS-32 (symbol-sort (cons 'Location SELF-CAPS-30)))
(define* DEV-CAPS (symbol-sort (append SELF-CAPS-32 '(PowerMgmt ProtServ ReadDeviceData SurroundingsDD SwEvent TrustedUI WriteDeviceData))))
(define* PUBLID-CAPS (symbol-sort (append DEV-CAPS '(CommDD DiskAdmin NetworkControl MultimediaDD))))
(define* MANUFACTURER-CAPS (symbol-sort (append PUBLID-CAPS '(AllFiles DRM TCB))))

(define-variant* symbian-variant% project-variant%
  (super-new)
  
  (define/override (platform) 'symbian)
    
  (define/public (uid-v9.attr)
    (make-hexnum #xe8460002))

  (define/public (uid.attr)
    (uid-v9.attr))
  
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

  (define/public (kit-vernum.attr)
    (let ((s (kit-name.attr)))
      (aif m (regexp-match #px"_([0-9]{2})$" s)
           (string->number (second m))
           (error "cannot determine kit version" s))))

  ;; This is intended for megasis builds only, and can be used to
  ;; specify the way the SIS and PKG files are to be named.
  (define/public (dist-variant-name.attr)
    (send this variant-name.attr))

  ;; This may affect some of the deployment options. Private trial
  ;; releases may be packaged and deployed differently than public
  ;; releases.
  (define/public (is-trial.attr) #f)

  ;; This may affect configurations, as demos are somewhat different
  ;; in nature.
  (define/public (is-demo.attr) #f)
  
  (define/override (have-signal.attr)
    (> (kit-vernum.attr) 50))

  ;; TThreadStackInfo
  (define/public (has-thread-stack-info.attr)
    (>= (s60-vernum.attr) 30))

  (define/public (have-mplayerremotecontrol.attr)
    (and
     ;; Based on docs, it sounds like at least v3.1 and v3.2 are
     ;; supported, with exceptions. Namely that not all v3.1 devices
     ;; are actually supported, but that just results in no events
     ;; being received, and is hence not harmful. "S60 3rd Edition,
     ;; FP1 devices are shipped with a new Music Player, which is not
     ;; the same S60 Music Player that is delivered with the S60 3rd
     ;; Edition, FP1 SDK. This new Music Player does not support the
     ;; Music Player Remote Control API." But the SDK plug-in is only
     ;; available for v3.1 kits, do not know if the same plug-in can
     ;; be installed onto others.
     (and (>= (s60-vernum.attr) 31) (<= (s60-vernum.attr) 32))
     (= (kit-vernum.attr) 31)
     (sublist? '(ReadDeviceData ReadUserData WriteUserData WriteDeviceData)
               (capabilities))))

  ;; Capas for this API not documented.
  (define/public (have-mpxplaybackutility.attr)
    (and (>= (s60-vernum.attr) 50)
         (= (kit-vernum.attr) 50)))
  
  (define/public (sqlite3h-as.attr)
    "static") ;; "static", "dynamic", or "source"
  
  ;; Abld build type. These days the options generally are "udeb",
  ;; "urel", and "all", but here only either udeb or urel is allowed.
  (define/public (abld-variant)
    (if (send this feature-debugging.attr) 'udeb 'urel))

  (define/public (abld-variant.attr)
    (symbol->string (abld-variant)))
  
  (define/public (have-devcert-caps.attr)
    (sublist? DEV-CAPS (capabilities)))

  (define/public (can-get-cell-id)
    (sublist? '(ReadDeviceData) (capabilities)))
  
  (define/public (can-get-network-info.attr)
    (can-get-cell-id))

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

  ;; This library requires high capabilities, but not in this process,
  ;; and hence it makes no sense to have this depend on our capas.
  (define/public (have-cl2webfilter.attr) #t)

  ;; We have no dependency on the DLL anymore, perhaps on some of the
  ;; headers.
  (define/public (have-euserhl.attr) #f)

  ;; --------------------------------------------------
  ;; features
  ;; --------------------------------------------------
  
  ;; If a Contacts DB session is needed in the application context.
  (define/public (need-contact-database.attr)
    #t) ;; xxx
  
  (define/override (mark-enabled.attr) #t)

  (define/override (upload-with-qt.attr)
    (and (send this with-qt.attr)
         (>= (s60-vernum.attr) 52)))

  (define/override (position-enabled.attr)
    (or (send this gps-enabled.attr)
        (send this cellpos-enabled.attr)))
  
  ) ;; end symbian-variant%

;; Enables all passive sensors that can be enabled.
(define-variant* symbian/all-passive-variant% symbian-variant%
  (super-new)

  (define/override (battery-enabled.attr) #t)
  (define/override (flightmode-enabled.attr) #t)
  (define/override (operator-enabled.attr) #t)
  (define/override (registration-enabled.attr) #t)
  (define/override (signal-enabled.attr) #t)
  )

;; Enables all event-based active sensors that can be enabled.
(define-variant* symbian/all-event-variant% symbian/all-passive-variant%
  (super-new)

  (define/public (have-caps? lst)
    (sublist? lst (send this capabilities)))
  
  (define/override (appfocus-enabled.attr)
    #t)

  (define/override (appmessage-enabled.attr)
    #t)

  (define/override (callstatus-enabled.attr)
    #t)

  (define/override (cellid-enabled.attr)
    (send this can-get-cell-id))

  (define/override (inactivity-enabled.attr)
    #t)

  (define/override (indicator-enabled.attr)
    #t)

  (define/override (keypress-enabled.attr)
    ;;xxx have two implementations to consider
    (have-caps? '(ReadDeviceData WriteDeviceData PowerMgmt ProtServ SwEvent)))
  
  (define/override (light-enabled.attr)
    (and (send this with-qt-mobility.attr)
         (have-caps? '(ReadDeviceData))))

  (define/override (music-enabled.attr)
    (send this have-mpxplaybackutility.attr))
  
  (define/override (proximity-enabled.attr)
    (and (send this with-qt-mobility.attr)
         (have-caps? '(ReadDeviceData))))

  (define/override (profile-enabled.attr)
    (or (send this have-profileengine-lib.attr)
        (have-caps? '(ReadDeviceData))))

  (define/override (smsevent-enabled.attr)
    #t)

  ;; It seems that on Symbian we only get readings from the singletap
  ;; sensor when double taps occur. Not sure of the exact semantics
  ;; there, but this sensor is not very useful on Symbian then.
  (define/override (singletap-enabled.attr)
    #f)

  (define/override (doubletap-enabled.attr)
    (and (send this with-qt-mobility.attr)
         (have-caps? '(ReadDeviceData))))

  (define/override (httpurl-enabled.attr)
    (and (send this have-cl2webfilter.attr)
         ;; This capa required by RProperty::Define().
         (have-caps? '(WriteDeviceData))))
    
  (define/override (weburl-enabled.attr)
    (and (not (httpurl-enabled.attr))
         (send this have-epocxplat.attr)
         (or
          (send this have-ahleclient-lib.attr)
          (send this have-ahle2client-lib.attr))
         (have-caps? '(ReadDeviceData WriteDeviceData))))
  )

;; Enables all sensors that can be enabled.
(define-variant* symbian/all-variant% symbian/all-event-variant%
  (super-new)

  (define/override (cellpos-enabled.attr)
    (and (send this can-get-cell-id)
         (send this have-caps? '(Location))))

  (define/override (gps-enabled.attr)
    (not (cellpos-enabled.attr)))

  (define/override (btprox-enabled.attr)
    #t))

;; Demo app configurations.
(define-variant* symbian/demo-variant% symbian/all-variant%
  (super-new)
  
  (define/override (is-demo.attr) #t)
  
  (define/override (quit-on-low-battery.attr) #f)

  (define/override (binary-type) 'application)
  
  (define/override (have-anim.attr) #f)
  
  (define/override (have-epocxplat.attr) #f)

  (define/override (have-cl2webfilter.attr) #f)

  (define/override (feature-remokon.attr) #f)
  
  (define/override (feature-uploader.attr) #t)

  (define/override (upload-time-expr.attr) "never")
  )

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
