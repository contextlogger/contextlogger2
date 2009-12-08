#lang scheme/base

;; 
;; Copyright 2009 Helsinki Institute for Information Technology (HIIT)
;; and the authors. All rights reserved.
;; 
;; Authors: Tero Hasu <tero.hasu@hut.fi>
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(require (lib "usual-4.ss" "common"))
(require* "node-ctors.scm")

(define-syntax sswitch/i
  (syntax-rules (case else)
    ((_)
     'nocode)
    ((_ (else z ...))
     (switch-default (block z ...)))
    ((_ (case x y ...) z ...)
     (switch-case x (block y ... (cbreak)) (sswitch/i z ...)))
    ))

;; An alternative SPECS switch implementation.
;; 
;; For some reason this does not work if defined in the mzscheme
;; language.
(define-syntax* sswitch
  (syntax-rules ()
    ((_ ex x ...)
     (switch-stmt ex (sswitch/i x ...)))))

