#lang scheme

#|

A daemon for Symbian.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new-symbian-variant #:btype 'daemon))

