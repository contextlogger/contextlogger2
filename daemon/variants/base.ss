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
;; sensors
;; --------------------------------------------------

(define sensor-list (cdr all-sensors))

(define (get-sensor-name sensor)
  (fget-reqd-nlist-elem-1 sensor 'name))

(define (sensor-essential? sensor)
  (true? (fget-opt-nlist-elem-1 sensor 'inactive)))

(define (enabled-symbol name)
  (string->symbol (format "~a-enabled" name)))

(define (symbol-sjoin lst)
  (string-join (map symbol->string lst) " "))

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
  
  (define/public (feature-rcfile.attr)
    #t)

  (define/public (feature-uploader.attr)
    #t)

  (define/public (upload-with-curl.attr)
    (eq? (platform) 'linux))

  ;; --------------------------------------------------
  ;; sensors
  ;; --------------------------------------------------
  
  (define/override (get-attrs)
    (for/hasheq
     ((sensor sensor-list))
     (alet name (get-sensor-name sensor)
           (values (enabled-symbol name)
                   (sensor-essential? sensor)))))
    
  )

(define-variant* linux-variant% project-variant%
    (super-new)
  
    (define/override (platform) 'linux)

    ;; No finished Linux implementation.
    (define/override (feature-uploader.attr) #f)

    (define/public (timer-enabled.attr) #t)

    )

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (symbol-sort lst)
  (sort lst symbol<?))

(define* SELF-CAPS-30 '(LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData))
(define* SELF-CAPS-32 (symbol-sort (cons 'Location SELF-CAPS-30)))
(define* DEV-CAPS (symbol-sort (append SELF-CAPS-32 '(PowerMgmt ProtServ ReadDeviceData SurroundingsDD SwEvent TrustedUI WriteDeviceData))))
(define* PUBLID-CAPS (symbol-sort (append DEV-CAPS '(CommDD DiskAdmin NetworkControl MultimediaDD))))

(define-variant* symbian-variant% project-variant%
  (super-new)
  
  (define/override (platform) 'symbian)
    
  (define/public (uid-v9.attr)
    (make-hexnum #xe8460002))
  
  (define/public (capabilities)
    '())

  (define/public (capabilities.attr)
    (symbol-sjoin (capabilities)))
  
  ;; --------------------------------------------------
  ;; available libs
  ;; --------------------------------------------------
  
  (define/public (have-anim.attr)
    (eq? (platform) 'symbian))

  (define/public (have-euserhl.attr)
    (eq? (platform) 'symbian))
  
  )

(define-variant* slist-variant% symbian-variant%
  (field
   (btype 'application)
   (slist '())
   (caps DEV-CAPS))
  
  (super-new)

  (define/public (set-binary-type x) (set! btype x))
  
  (define/public (set-sensor-list x) (set! slist x))
  
  (define/override (binary-type) btype)
  
  (define/override (capabilities) caps)
  
  (define/public (have-sensor? name)
    (true? (memq name slist)))
  
  (define/override (get-attrs)
    (for/hasheq
     ((sensor sensor-list))
     (alet name (get-sensor-name sensor)
           (values (enabled-symbol name)
                   (or (sensor-essential? sensor)
                       (have-sensor? name))))))
    
  )

(define (all-symbian-sensors)
  '(flightmode profile cellid btprox gps appfocus keypress))

(define* (new-symbian-variant
          #:class (class slist-variant%)
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

(define (sublist? s lst)
  (true? (andmap (lambda (x) (memq x lst)) s)))

(define (include? e lst)
  (true? (memq e lst)))

(define-variant* release-variant% symbian-variant%
  (init-field caps cert-name
              (signed? #t)
              (dist-variant-name #f))

  (super-new)

  (define/public (dist-variant-name.attr)
    (aif n dist-variant-name
         (symbol->string n)
         (send this variant-name.attr)))
         
  (define/override (binary-type) 'daemon)

  (define/override (capabilities) caps)

  (define/public (keypress-enabled.attr)
    (sublist? '(ReadDeviceData WriteDeviceData PowerMgmt ProtServ SwEvent)
              (capabilities)))
  
  (define/public (gps-enabled.attr)
    (sublist? '(Location)
              (capabilities)))
  
  (define/public (cellid-enabled.attr)
    (sublist? '(Location)
              (capabilities)))

  (define/public (signed.attr)
    signed?)
  
  (define/public (devcert-caps.attr)
    (true? (memq cert-name '(dev))))
    
  (define/public (watchdog-supported.attr)
    ;; Due to the requirement to use a protected development UID (or
    ;; the do proper Symbian signing), in practice we will only be
    ;; able to do this if we have a DevCert.
    (devcert-caps.attr))

  (define/public (cert-name.attr)
    (symbol->string cert-name))
  
  (define/override (get-attrs)
    (for/hasheq
     ((sensor sensor-list))
     (alet name (get-sensor-name sensor)
           (values (enabled-symbol name)
                   (or (sensor-essential? sensor)
                       (include? name (all-symbian-sensors)))))))
    
  )

  