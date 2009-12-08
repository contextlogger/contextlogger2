#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new release-variant%
       (cert/o 'self30)
       (caps/o SELF-CAPS-30)))



