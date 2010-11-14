#lang scheme

#|

Sensible defaults for Symbian/Qt builds.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* klass%
   (variant-class
    symbian-variant%
    (super-new)
    
    (define/override (binary-type) 'daemon)
    (define/override (s60-vernum.attr) 50)
    (define/override (kit-name) 's60_50)
    
    (define/override (with-qmake.attr) #t)
    (define/override (with-qt.attr) #t)

    (define/override (appfocus-enabled.attr) #t)
    (define/override (appmessage-enabled.attr) #t)
    (define/override (indicator-enabled.attr) #t)
    (define/override (btprox-enabled.attr) #f)
    (define/override (gps-enabled.attr) #f)
    (define/override (cellid-enabled.attr) #t)
    (define/override (profile-enabled.attr) #t)
    (define/override (keypress-enabled.attr) #t)
    (define/override (inactivity-enabled.attr) #t)
    (define/override (weburl-enabled.attr) #t)

    (define/override (feature-remokon.attr) #f)
    (define/override (feature-uploader.attr) #f)

    )) ;; end class

(define* (info)
  (new klass%)) 
