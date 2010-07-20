#lang scheme

#|

A daemon for Symbian.

|#

(require "base.ss")
(require (prefix-in d. "devel.var.ss"))
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new (variant-class
        d.klass%
        (super-new)
        (define/override (binary-type) 'daemon)
        )))
