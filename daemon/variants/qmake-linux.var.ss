#lang scheme

#|

Sensible defaults for Linux/libev builds with qmake.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define-variant qt-variant% linux-variant%
  (super-new)
  (define/override (with-qmake.attr) #t)
  (define/override (feature-remokon.attr) #f)
  (define/override (lua-from-source.attr) #t)
  )

(define* (info)
  (make-object qt-variant%))

