#lang scheme

(require (lib "usual-4.ss" "common"))
(require konffaile/class-attr)
(require konffaile/component)
(require konffaile/variant)

(define (string-split-space s)
  (regexp-split #rx"[ \t]+" s))

(define* (info)
  (define var (current-variant))

  (define (attr x) (object-get-attr var x))

  (define flatten-compact (compose compact flatten))

  (define platform (send var platform))
  
  (define on-symbian (eq? platform 'symbian))
  
  (define (symbian-list . x)
    (if on-symbian (flatten-compact x) '()))

  (define (compact-list . x) (compact x))

  (new abld-component%
       (deps (compact-list
              'common/logging
              (and on-symbian
                   (attr 'profile-enabled)
                   'epoc-profile)
              (and on-symbian
                   (attr 'smsevent-enabled)
                   'epoc-smsevent)
              ))
       (mmp-libs
        (symbian-list
         "apgrfx.lib"
         "apparc.lib"
         "avkon.lib"
         "bafl.lib"
         "bluetooth.lib"
         "btmanclient.lib"
         "charconv.lib"
         "commdb.lib"
         "commonui.lib" ;; CErrorUI
         "cone.lib"
         "efsrv.lib"
         "eikcoctl.lib"
         "eikcore.lib"
         "esock.lib"
         "etel3rdparty.lib" ;; CTelephony
         "http.lib"
         "inetprotutil.lib" ;; URI parsers
         "insock.lib" ;; Internet protocol support for esock
         "lbs.lib"
         "ws32.lib"
         ))
       ))
