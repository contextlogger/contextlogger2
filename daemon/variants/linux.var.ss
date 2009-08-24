#lang scheme

#|

Sensible defaults for Linux builds.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (make-object linux-variant%))

