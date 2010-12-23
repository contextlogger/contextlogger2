#lang scheme

#|

S60 v3.0 DevCert-signed daemon build, with all supported sensors.
Intended for mega-SIS inclusion, but not for releasing.

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
  
    (define/override (cert-name) 'dev)
  
    (define/override (capabilities) DEV-CAPS)

    (define/override (s60-vernum.attr) 30)
    
    (define/override (kit-name) 's60_30)
    
    (define/override (binary-type) 'daemon)
  
    (define/override (have-anim.attr) #t)

    (define/override (have-epocxplat.attr) #t)

    (define/override (feature-uploader.attr) #t)

    (define/override (upload-time-expr.attr) "never")

    )))
