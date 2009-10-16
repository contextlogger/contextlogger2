#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new release-variant%
       (kit 's60_31)
       (s60-vernum 31)
       (cert-name 'dev)
       (caps DEV-CAPS)))



