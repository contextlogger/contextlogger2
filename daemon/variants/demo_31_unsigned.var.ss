#lang scheme

#|

S60 v3.1 unsigned demo build for DevCert signing. With a GUI, and Open
C/C++ and Qt as dependencies.

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

    (define/override (s60-vernum.attr) 31)
    
    (define/override (kit-name) 's60_31)
    
    (define/override (binary-type) 'application)
  
    (define/override (with-qt.attr) #t)

    (define/override (have-anim.attr) #f)

    (define/override (have-epocxplat.attr) #f)

    (define/override (feature-uploader.attr) #t)

    (define/override (upload-time-expr.attr) "never")

    )))
