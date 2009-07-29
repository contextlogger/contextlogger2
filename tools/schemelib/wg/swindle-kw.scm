;; This code is based on code in Swindle, and is covered by the LGPL
;; license. See http://barzilay.org/Swindle/ for more details.

;; Swindle extensions.
(module
 swindle-kw
 mzscheme

 (require (lib "kw.ss"))
 (require (lib "swindle-util.scm" "wg"))

 (define-syntax* (define/kw* stx)
   (syntax-case stx ()
		[(_ x . xs)
		 (memq (syntax-local-context) '(module module-begin))
		 (let ([name (let loop ([x #'x])
			       (syntax-case x () [(x . xs) (loop #'x)] [_ x]))])
		   (if name
		       #`(begin (provide #,name) (define/kw x . xs))
		       #`(define/kw x . xs)))]
		[(_ x . xs) #`(define/kw x . xs)]))

) ; end module
