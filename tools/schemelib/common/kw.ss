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
