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
    symbian/all-variant%
    (super-new)
    
    (define/override (signed.attr) #t)
  
    (define/override (cert-name) 'self32)
  
    (define/override (capabilities) SELF-CAPS-32)

    (define/override (s60-vernum.attr) 32)
    
    (define/override (kit-name) 's60_32)
    
    (define/override (binary-type) 'application)
  
    (define/override (with-qt.attr) #t)

    (define/override (have-anim.attr) #f)

    (define/override (have-epocxplat.attr) #f)

    (define/override (feature-uploader.attr) #t)

    (define/override (upload-time-expr.attr) "never")

    )))
