#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new release-variant%
       (cert-name 'dev)
       (signed? #f)
       (dist-variant-name 's60_30_dev)
       (caps DEV-CAPS)))



