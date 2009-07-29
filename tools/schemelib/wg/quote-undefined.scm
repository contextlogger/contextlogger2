;; 
;; quote-undefined.scm
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
 quote-undefined
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))

 ;; We quote anything that does not appear to be defined globally as a
 ;; Scheme identifier in the default environment. As an exception, we
 ;; quote nothing prefixed with the dollar sign, to provide some way
 ;; to use _local_ Scheme identifiers. Given that there could be any
 ;; sort of macros used within an evaled expression, we cannot presume
 ;; to know what construct is defining a local variable and what is
 ;; not (not without somehow hooking into the Scheme evaluator), so
 ;; the user has to tell us. Should be tolerable to Perl fans, at
 ;; least.
 (define (do-quote? sym)
   (and (not (defined-i? sym))
	(not (symbol-starts-with? sym "$"))))

 (define* (quote-undef-symbols ast)
   (define memo '())

   ;; Returns true or false indicating whether a value should be
   ;; quoted or not.
   ;; 
   ;; We memoize under the assumption that exception handling is more
   ;; expensive. (defined-i? is presently implemented using an
   ;; exception.) Which may well be wrong. We could use a generic
   ;; memoization solution, which I guess would be easy to implement
   ;; in Scheme.
   (define (do-quote-m? sym)
     ;;(write-nl (list "do-quote?" sym))
     (aif entry (assq sym memo)
	  (cdr entry)
	  (let ((res (do-quote? sym)))
	    ;;(write-nl (list "res" sym res))
	    (push-set! memo (cons sym res))
	    res)))

   (define (do-quote ast)
     ;;(write-nl (list "tmp" ast))
     (cond
      ((list? ast)
       (cond
	((null? ast) ast)
	((eq? (car ast) 'quote) ast)
	(else (map do-quote ast))))
      ((symbol? ast)
       (let ((res (do-quote-m? ast)))
	 ;;(write-nl (list "tmp" ast res))
	 (if res `(quote ,ast) ast)))
      (else
       ast)))

   (do-quote ast))

 (define* (evalq ast)
   (eval (quote-undef-symbols ast)))

) ; end module

#;
(begin
  (require quote-undefined)
  (require (lib "util.scm" "wg"))

  (let ((test-list
	 (list
	  '(list a b)
	  '(list my-list '(my-sublist 444) 555 "string")
	  '(let (($x (list my-symbol))) $x)
	  )))
    (for-each
     (lambda (ast)
       (display "original: ")
       (write-nl ast)
       (display "quoted: ")
       (write-nl (quote-undef-symbols ast))
       (display "quoted evaled: ")
       (write-nl (evalq ast))
       )
     test-list)))
