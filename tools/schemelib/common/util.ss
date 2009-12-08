#lang scheme

(require (except-in (lib "util.scm" "wg")
                    identity nor nand true? false?
                    any-pred every-pred
                    puts string-join
                    case-eq inspect flatten
                    write-nl display-nl))
(provide (all-from-out (lib "util.scm" "wg")))

(require (lib "module-util.scm" "wg"))
(require "module.ss")

(require* "pred.ss")

(define* puts
  (case-lambda
    ((val) (begin
             (display val)
             (newline)))
    ((val out) (begin
                 (display val out)
                 (newline out)))))

(define* write-nl
  (case-lambda
    ((datum) (begin (write datum) (newline)))
    ((datum out) (begin (write datum out) (newline out)))))

(define* display-nl
  (case-lambda
    ((datum) (begin (display datum) (newline)))
    ((datum out) (begin (display datum out) (newline out)))))

(define-syntax* thunk
  (syntax-rules ()
    ((_ body ...)
     (lambda () body ...))))

(define* (constant x)
  (thunk x))

;; Apparently this is defined in recent versions of PLT Scheme,
;; with exactly the same name as we came up with for it.
;;(define* (hash-has-key? hash key)
;;  (and (hash-ref hash key #f) #t))

;; For whatever reason this macro no longer works when defined in the
;; "mzscheme" language, so we export a copy that is defined in
;; "scheme" language.
(define-syntax* case-eq
  (syntax-rules (else)
    ((_ e (a b) ... (else c))
     (cond ((eq? e a) b) ... (else c)))
    ((_ e (a b) ...)
     (cond ((eq? e a) b) ...))))

;; "case" appears to be broken in some versions of PLT Scheme, so we
;; are defining this one to avoid problems. Surely "cond" at least
;; must work. Here we use equal? instead of eqv?.
(define-syntax* case-equal
  (syntax-rules (else)
    ((_ e (a b) ... (else c))
     (cond ((equal? e a) b) ... (else c)))
    ((_ e (a b) ...)
     (cond ((equal? e a) b) ...))))

;; Note that action and cleanup are expressions, not thunks. Note also
;; that as we are doing functional programming, it is important to
;; preserve the result of "action" in the event there is no exception.
(define-syntax* try-finally
  (syntax-rules ()
    ((_ action cleanup)
     (let ((res #f))
       (with-handlers
           ((void (lambda (exn) cleanup (raise exn))))
         (set! res action))
       cleanup
       res))))

(define* (path-dirname path)
  (let-values (((base name must-be-dir) (split-path path)))
    (unless (path? base)
      (error "cannot get directory component from pathname" path))
    base))

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
