#lang scheme

#|

Symbian^3 unsigned demo build for DevCert signing. With a
GUI, and Open C/C++ and Qt as dependencies. Uploads with Qt, as the
Symbian HTTP stack appears too broken on Symbian^3.

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

    (define/override (s60-vernum.attr) 52)
    
    ;; As there is still no 1.0 release of the Symbian^3 SDK.
    (define/override (kit-name) 's60_50)
    
    (define/override (with-qt.attr) #t)

    )))
