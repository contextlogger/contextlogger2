#lang scheme

#|

This one could be useful for photo tagging after a trip abroad.
It avoids the expense of uploads, but records location.
To help save battery, it has a GUI for easy ON/OFF.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new-symbian-variant #:class (variant-class
                                symbian-variant%
                                (super-new)
                                (define/override (feature-uploader.attr)
                                  #f))
                       #:btype 'application
                       #:include '(cellid gps)))
