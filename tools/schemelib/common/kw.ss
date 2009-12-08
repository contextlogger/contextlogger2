#lang scheme

(define-syntax lambda/pass-kw
  (syntax-rules ()
    ((_ arg-spec apply/kw body ...)
     (make-keyword-procedure
      (lambda (kw-names kw-vals . args)
        (let ((apply/kw
               (lambda (f . rest)
                 (keyword-apply f kw-names kw-vals rest))))
          (apply
           (lambda arg-spec body ...)
           args)))))))

(provide lambda/pass-kw)

#|

Might also want to look at
http://planet.plt-scheme.org/package-source/schematics/spgsql.plt/2/3/private/compat.ss
which redefines lambda/kw to be compatible with the "kw.ss" calling convention.

Example code:

(require "usual-4.ss")

(define (inner #:a (a 1) #:b (b 2))
  (list a b))

(write-nl (inner))
(write-nl (inner #:a 5))
(write-nl (inner #:b 5))
(write-nl (inner #:a 5 #:b 5))

(define outer
  (lambda/pass-kw (c d . rest) apply/kw
                  (list c d rest (apply/kw inner))))

(write-nl (outer 3 4))
(write-nl (outer 3 4 #:a 5))
(write-nl (outer 3 4 #:b 5))
(write-nl (outer 3 4 #:a 5 #:b 5))
(write-nl (outer 3 4 5 6 7 #:b 5))
|#

#|

Copyright 2008 the authors. All rights reserved.

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
