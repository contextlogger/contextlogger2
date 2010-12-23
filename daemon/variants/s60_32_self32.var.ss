#lang scheme

#|

S60 v3.2 self-signed daemon build, with all supported sensors.
Intended for mega-SIS inclusion. Depends on Qt. With Location
capability.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new 
   (variant-class
    symbian/all-variant%
    (super-new)
    
    (define/override (signed.attr) #t)
  
    (define/override (cert-name) 'self32)
  
    (define/override (capabilities) SELF-CAPS-32)

    (define/override (s60-vernum.attr) 32)
    
    (define/override (kit-name) 's60_32)
    
    (define/override (binary-type) 'daemon)
  
    (define/override (with-qt.attr) #t)

    (define/override (have-anim.attr) #t)

    (define/override (have-epocxplat.attr) #t)

    (define/override (feature-uploader.attr) #t)

    (define/override (upload-time-expr.attr) "never")

    )))
