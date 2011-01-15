#lang scheme

#|

For testing the "music" sensor on Symbian.

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

    (define/override (have-epocxplat.attr) #f)
    (define/override (feature-remokon.attr) #f)
    (define/override (feature-uploader.attr) #f)
    
    (define/override (music-enabled.attr) #t) ;;xxx

    )) ;; end class

(define* (info)
  (new klass%)) 
