#lang scheme

#|

For testing remote control on Symbian.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* klass%
   (variant-class
    symbian-variant%
    (super-new)
    
    (define/override (binary-type) 'application)
    (define/override (s60-vernum.attr) 50)
    (define/override (kit-name) 's60_50)

    (define/override (with-qmake.attr) #f)
    (define/override (with-qt.attr) #t)
    (define/override (have-sqlite3.attr) #t)

    (define/override (feature-remokon.attr) #t)
    (define/override (feature-uploader.attr) #f)

    )) ;; end class

(define* (info)
  (new klass%)) 
