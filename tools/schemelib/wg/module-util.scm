;; 
;; module-util.scm
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
 module-util
 mzscheme

 (require (lib "swindle-util.scm" "wg"))

 (define-syntax* require*
   (syntax-rules ()
     ((_ spec)
      (begin
	(require spec)
	(provide (all-from spec))))))

 ;; Binds a symbol in the top-level transformer environment.
 (define-for-syntax make-star-symbol
   (lambda (sym) (string->symbol (string-append (symbol->string sym) "*"))))

 ;; Usage is (_ name), where name must name a syntax form that defines
 ;; another symbol. Creates and provides a name* variant of the syntax
 ;; that, in addition to defining the symbol, also provides it. Be
 ;; careful, as this macro is not hygienic.
 (define-syntax* (define-star-variant* stx)
   (syntax-case stx ()
 		((_ name)
 		 (with-syntax ((starname (datum->syntax-object stx (make-star-symbol (syntax-object->datum (syntax name))))))
 			      (syntax (begin (make-provide-syntax name starname) (provide starname)))))))

 ;; May be used to define and provide symbol defining syntax and its *
 ;; variant, all in one go.
 (define-syntax* define-syntax**
   (syntax-rules ()
     ((_ name rules)
      (begin (define-syntax* name rules)
	     (define-star-variant* name)))))

 (define-syntax* define-syntaxes*
   (syntax-rules ()
     ((_ (name ...) rest ...)
      (begin
	(define-syntaxes (name ...) rest ...)
	(provide name ...)))))

) ; end module
