#lang scheme

#|

For testing Symbian Qt GUI builds.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new 
   (variant-class
    ;;symbian/all-event-variant%
    symbian-variant%
    (super-new)
    
    (define/override (signed.attr) #t)
  
    (define/override (cert-name) 'dev)
  
    (define/override (capabilities) DEV-CAPS)

    (define/override (s60-vernum.attr) 50)
    
    (define/override (kit-name) 's60_50)
    
    (define/override (binary-type) 'application)
  
    (define/override (with-qt.attr) #t)

    (define/override (feature-remokon.attr) #f)
    (define/override (feature-uploader.attr) #f)

    (define/override (light-enabled.attr) #t)
    (define/override (doubletap-enabled.attr) #t)
    (define/override (proximity-enabled.attr) #t)
    
    )))
