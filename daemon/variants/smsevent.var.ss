#lang scheme

#|

For testing the "smsevent" sensor.

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

    (define/override (smsevent-enabled.attr) #t)

    (define/override (feature-remokon.attr) #f)
    (define/override (feature-uploader.attr) #f)
    (define/override (upload-with-qt.attr) #f)

    )) ;; end class

(define* (info)
  (new klass%)) 
