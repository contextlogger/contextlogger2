#lang scheme

#|

S60 v3.0 unsigned demo build for DevCert signing. With a GUI, and Open
C/C++ as the only dependency.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new 
   (variant-class
    symbian/demo-variant%
    (super-new)
    
    (define/override (signed.attr) #f)
  
    (define/override (capabilities) DEV-CAPS)

    (define/override (s60-vernum.attr) 30)
    
    (define/override (kit-name) 's60_30)
    
    )))
