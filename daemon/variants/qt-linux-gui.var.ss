#lang scheme

#|

A Linux/Qt GUI build, with the demo GUI.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define-variant qt-variant% linux-variant%
  (super-new)
  (define/override (binary-type) 'application)
  (define/override (with-qmake.attr) #t)
  (define/override (with-qt.attr) #t)
  (define/override (feature-remokon.attr) #t)
  (define/override (feature-uploader.attr) #t)
  )

(define* (info)
  (make-object qt-variant%))

