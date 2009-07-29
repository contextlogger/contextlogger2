;; 
;; declarations.scm
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
 declarations
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (prefix h. (lib "hash.scm" "wg")))

 (define (symbol-append-num sym num)
   (string->symbol
    (string-append
     (symbol->string sym)
     (inspect num))))

 (define* (d-table-next-id d-table name)
   (letrec ((f 
	     (lambda (num) 
	       (let ((nid (symbol-append-num name num)))
		 (if (h.has-key? d-table nid)
		     (f (+ num 1))
		     nid)))))
     (f 1)))

 (define* (get-decl-by-id d-table id)
   (let ((res (h.fetch d-table id)))
     (if res 
	 res
	 (error "declaration table does not have ID" id))))

 ) ; end module
