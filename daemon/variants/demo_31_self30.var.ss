#lang scheme

#|

S60 v3.1 self-signed demo build. With a GUI, and Open C/C++ and Qt as
dependencies.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new 
   (variant-class
    symbian/demo-variant%
    (super-new)
    
    (define/override (signed.attr) #t)
  
    (define/override (cert-name) 'self30)
  
    (define/override (capabilities) SELF-CAPS-30)

    (define/override (s60-vernum.attr) 31)
    
    (define/override (kit-name) 's60_31)
    
    (define/override (with-qt.attr) #t)

    )))
