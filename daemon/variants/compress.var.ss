#lang scheme

#|

This variant is for testing logfile compression.

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

    (define/override (with-qmake.attr) #f)
    (define/override (with-qt.attr) #t)

    (define/override (appfocus-enabled.attr) #t)
    (define/override (appmessage-enabled.attr) #f)
    (define/override (indicator-enabled.attr) #t)
    (define/override (btprox-enabled.attr) #f)
    (define/override (gps-enabled.attr) #f)
    (define/override (cellid-enabled.attr) #f)
    (define/override (profile-enabled.attr) #t)
    (define/override (keypress-enabled.attr) #t)
    (define/override (inactivity-enabled.attr) #t)
    (define/override (weburl-enabled.attr) #f)

    (define/override (feature-remokon.attr) #f)
    (define/override (feature-uploader.attr) #t)
    (define/override (feature-compress-logs.attr) #t)

    )) ;; end class

(define* (info)
  (new klass%)) 
