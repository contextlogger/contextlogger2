#lang scheme

#|



|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new release-variant%
       (cert/o 'dev)
       (signed/o #f)
       (dist-variant-name 's60_30_dev)
       (caps/o DEV-CAPS)))



