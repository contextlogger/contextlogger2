#lang scheme

#|

This file defines the API that a variant specification must implement.

There can be variation in feature sets, target platforms, or
toolchains, for example.

The variant specifications should strive to be fairly high-level, so
that only the actual variable choices can be made there. A
specification should pretty much be just a set of key/value pairs,
which we call attributes.

To avoid duplication in specifications, inheritance is supported.
(For this we use the built-in PLT Scheme OO facility.) Each variant
spec should be an instance of a subclass of variant%. Macros are
provided for making it easier to define such subclasses.

|#

(require common/usual-4)
(require common/class)

(define* current-variant (make-parameter #f))

;; There is nothing here really, as a variant specification really is
;; project specific. However, a common baseclass for variants may turn
;; out to be of use.
(define* variant%
  (class object%
    (inspect #f) ;; transparent
    
    (field (name #f))
    
    (super-new)

    (define/public (variant-name.attr)
      (symbol->string name))

    ;; This can be useful for more reflective configuration setups.
    ;; Note, though, that these attributes can still be overridden
    ;; with .attr methods.
    (define/public (get-attrs)
      #hasheq())
    ))

(define-syntax* variant-class
  (syntax-rules ()
    ((_ super body ...)
     (class super
       (inspect #f)
       body ...))))

(define-syntax* define-variant
  (syntax-rules ()
    ((_ name super body ...)
     (define name
       (class super
         (inspect #f)
         body ...)))))

(define-syntax* define-variant*
  (syntax-rules ()
    ((_ name rest ...)
     (begin
       (define-variant name rest ...)
       (provide name)))))

(define-struct* hexnum (num))

#|

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
