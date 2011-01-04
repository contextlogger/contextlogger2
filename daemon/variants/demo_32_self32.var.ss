#lang scheme

#|

S60 v3.2 self-signed demo build, with Location capability. With a GUI,
and Open C/C++ and Qt as dependencies.

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
  
    (define/override (cert-name) 'self32)
  
    (define/override (capabilities) SELF-CAPS-32)

    (define/override (s60-vernum.attr) 32)
    
    (define/override (kit-name) 's60_32)
    
    (define/override (with-qt.attr) #t)

    )))
