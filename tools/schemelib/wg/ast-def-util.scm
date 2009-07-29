;; 
;; ast-def-util.scm
;; 
;; Copyright 2007 Helsinki Institute for Information Technology (HIIT)
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

(module
 ast-def-util
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "type-util.scm" "wg"))

 (define-syntax** def-null-ctor
   (syntax-rules ()
     ((_ fn)
      (define fn
	(lambda ()
	  (list (quote fn)))))))

 (define-syntax** def-string-ctor
   (syntax-rules ()
     ((_ fn)
      (define fn
	(lambda (arg)
	  (list (quote fn) (to-string arg)))))))

 (define-syntax** def-symbol-ctor
   (syntax-rules ()
     ((_ fn)
      (define fn
	(lambda (arg)
	  (list (quote fn) (to-symbol arg)))))))

 (define-syntax** def-basic-ctor-1
   (syntax-rules ()
     ((_ fn)
      (define fn
	(lambda (arg)
	  (list (quote fn) arg))))))

 (define-syntax** def-list-ctor
   (syntax-rules ()
     ((_ fn)
      (define fn
	(lambda args
	  (cons (quote fn) args))))))

 (define-syntax** def-nlist-ctor
   (syntax-rules ()
     ((_ fn sym)
      (define fn
	(lambda args
	  (cons (quote sym) args))))))

 ;; Produces a definition that has the specified "bare" symbol
 ;; evaluate to a quoted version of itself.
 (define-syntax** def-symbol
   (syntax-rules ()
    ((_ sym)
     (define sym (quote sym)))))

 (define-syntax** def-true-symbol
   (syntax-rules ()
    ((_ sym)
     (define sym (list (quote sym) #t)))))

 (define-syntax** def-doc-ctor
   (syntax-rules ()
     ((_ fn)
      (define (fn . arg)
	(list (quote fn) (apply string-append (map to-s arg)))))))

)
