#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new release-variant%
       (kit/o 's60_31)
       (s60-vernum/o 31)
       (cert/o 'dev)
       (caps/o DEV-CAPS)))



