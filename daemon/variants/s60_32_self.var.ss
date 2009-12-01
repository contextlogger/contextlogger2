#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new release-variant%
       (kit/o 's60_32)
       (s60-vernum/o 32)
       (caps/o SELF-CAPS-32)
       (cert/o 'self32)))



