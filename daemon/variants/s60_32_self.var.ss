#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new release-variant%
       (kit 's60_32)
       (s60-vernum 32)
       (caps SELF-CAPS-32)
       (cert-name 'self32)))



