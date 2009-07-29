;; 
;; op.scm
;; 
;; Copyright 2006 Helsinki Institute for Information Technology (HIIT)
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
 op
 mzscheme

 (require (lib "usual.scm" "wg"))

 (define (matching-op? elem sym)
   (cond ((symbol? elem) (eq? elem sym))
	 ((list? elem) (eq? (car elem) sym))
	 ((pair? elem) (eq? (car elem) sym))
	 (else #f)))

 (define get-op-elem
   (let ((f 
	  (lambda (l sym def)
	    (find-first l (lambda (x) (matching-op? x sym))))))
     (case-lambda
      ((l sym) (f l sym #f))
      ((l sym def) (f l sym def)))))

 (define (elem-value elem)
   (cond ((symbol? elem) #t)
	 ((pair? elem) (cdr elem))
	 ((list? elem) (cdr elem))
	 (error "unsupported op type")))

 (define get-op-value
   (let ((f
	  (lambda (l sym def)
	    (let ((elem (get-op-elem l sym)))
	      (if elem
		  (elem-value elem)
		  def)))))
     (case-lambda
      ((l sym) (f l sym #f))
      ((l sym def) (f l sym def)))))

 (provide get-op-elem get-op-value))
