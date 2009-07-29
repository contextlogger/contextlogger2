;; 
;; documentation.scm
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
 documentation
 mzscheme

 (require "ast-util.scm")
 (require (lib "prop-util.scm" "wg"))
 (require "mop.scm")
 (require "usual.scm")

 (define* (doc-sym? sym)
   (or (eq? sym 'doc)
       (symbol-ends-with? sym "-doc")))

 (define* (can-be-doc-owner? ast)
   ;; xxx this should probably just be declaration? as defined in ast-spec
   (or (global-decl? ast)
       (list-named-one-of? ast '(cxx-unit cxx-namespace local-var))))

 (define* (mark-doc-forbidden ast)
   (push ast '(doc-forbidden #t)))

 (define* (doc-forbidden? ast)
   (has-true-bool-prop-memb? ast 'doc-forbidden))

 ;; Deletes all documentation attributes.
 (define* delete-doc-attr
   (lambda (ast)
     (filter
      (lambda (elem)
 	(not (and (named-list? elem)
 		  (doc-sym? (car elem)))))
      ast)))

;  ;; Deletes all documentation from an object.
;  (define* (delete-doc ast)
;    (set! ast (delete-doc-attr ast))
;    (when (function-decl? ast)
; 	 (let* ((rw-ret
; 		 (lambda (ast)
; 		   (set! ast (delete-doc-attr ast))
; 		   (set! ast (reject-nlist-elems ast 'error-codes))
; 		   ast)))
; 	   (set! ast (reject-nlist-elems-by-names ast '(panics leaves)))
; 	   (set! ast (func-modify-arg ast delete-doc-attr))
; 	   (set! ast (func-modify-returns ast rw-ret))))
;    ast)

 (define* (join-words . words)
   (string-join (compact (flatten words)) " "))

 (define* (join-lines lines)
   (string-join (compact lines) "\n"))

 (define* (add-period string)
   (and string
	(if (string-ends-with? string ".")
	    string
	    (string-append string "."))))

 (define-syntax* def-tag-0*
   (syntax-rules ()
     ((_ fn)
      (define* fn
	(lambda () (list (quote fn)))))))

 (define-syntax* def-tag-1*
   (syntax-rules ()
     ((_ fn)
      (define* fn
	(lambda (arg) (list (quote fn) (list 'description (to-string arg))))))))

 (define-syntax* def-tag-0-or-1*
   (syntax-rules ()
     ((_ fn)
      (define* fn
	(case-lambda
	 (() (list (quote fn)))
	 ((arg) (list (quote fn) (list 'description (to-string arg)))))))))

) ; end module
