#lang scheme

#|

For development and testing.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new 
   (variant-class
    devel-variant%
    (super-new)
    
    ;;(define/override (binary-type) 'daemon)
  
    (define/override (appfocus-enabled.attr) #t)
    (define/override (appmessage-enabled.attr) #t)
    (define/override (indicator-enabled.attr) #t)
    (define/override (btprox-enabled.attr) #f)
    (define/override (gps-enabled.attr) #f)
    (define/override (cellid-enabled.attr) #f)
    (define/override (profile-enabled.attr) #t)
    (define/override (keypress-enabled.attr) #t)
    (define/override (inactivity-enabled.attr) #t)

    (define/override (feature-remokon.attr) #t)

    (define/override (upload-time-expr.attr) "never")

    (define/override (username.attr) "developer")

    ) ;; end class
   (s60-vernum/o 31)
   (kit/o 's60_31)))
