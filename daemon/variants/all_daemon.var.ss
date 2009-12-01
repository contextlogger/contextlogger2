#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new devel-variant%
       (binary-type/o 'daemon)
       (sensor-list ALL-SYMBIAN-SENSORS)))

