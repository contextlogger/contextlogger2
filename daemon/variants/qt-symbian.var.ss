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
    (define/override (s60-vernum.attr) 52)
    (define/override (kit-name) 's60_52)

    ;; qmake is not quite there yet for Symbian; we can refer to the generated .mmp to see what we need, and invoke moc ourselves as required
    (define/override (with-qmake.attr) #f)
    
    (define/override (with-qt.attr) #t)

    (define/override (appfocus-enabled.attr) #t)
    (define/override (appmessage-enabled.attr) #t)
    (define/override (indicator-enabled.attr) #t)
    (define/override (btprox-enabled.attr) #f)
    (define/override (gps-enabled.attr) #f)
    (define/override (cellid-enabled.attr) #t)
    (define/override (profile-enabled.attr) #t)
    (define/override (keypress-enabled.attr) #f)
    (define/override (inactivity-enabled.attr) #t)
    (define/override (weburl-enabled.attr) #t)

    (define/override (feature-remokon.attr) #f)
    (define/override (feature-uploader.attr) #f)

    )) ;; end class

(define* (info)
  (new klass%)) 
