#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new-symbian-variant #:btype 'daemon
                       #:exclude '()))


