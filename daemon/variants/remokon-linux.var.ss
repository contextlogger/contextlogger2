#lang scheme

#|

For testing remote control on Linux.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* klass%
   (variant-class
    linux-variant%
    (super-new)
    
    (define/override (binary-type) 'application)

    (define/override (with-qmake.attr) #t)
    (define/override (with-qt.attr) #t)
    (define/override (have-sqlite3.attr) #t)

    (define/override (feature-remokon.attr) #t)
    
    (define/override (feature-uploader.attr) #f)

    )) ;; end class

(define* (info)
  (new klass%)) 
