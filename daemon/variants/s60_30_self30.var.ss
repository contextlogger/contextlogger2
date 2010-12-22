#lang scheme

#|

S60 v3.0 self-signed demo build. With a GUI, and Open C/C++ as the
only dependency.

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
  
    (define/override (cert-name) 'self30)
  
    (define/override (capabilities) SELF-CAPS-30)

    (define/override (s60-vernum.attr) 30)
    
    (define/override (kit-name) 's60_30)
    
    (define/override (binary-type) 'application)
  
    (define/override (have-sqlite3.attr) #f)

    (define/override (have-anim.attr) #f)

    (define/override (have-epocxplat.attr) #f)

    (define/override (feature-uploader.attr) #t)

    (define/override (upload-time-expr.attr) "never")

    )))
