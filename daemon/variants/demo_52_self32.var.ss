#lang scheme

#|

Symbian^3 self-signed demo build, with Location capability. With a
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
    
    (define/override (signed.attr) #t)
  
    (define/override (cert-name) 'self32)
  
    (define/override (capabilities) SELF-CAPS-32)

    (define/override (s60-vernum.attr) 52)

    ;; As there is still no 1.0 release of the Symbian^3 SDK.
    (define/override (kit-name) 's60_50)
    
    (define/override (with-qt.attr) #t)

    )))
