;; 
;; match.scm
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
 match
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require* (lib "plt-match.ss"))

 ;; Here we define a pattern matcher with useful constructs for
 ;; matching against our AST. It behaves like "match-lambda", but
 ;; supports a couple of extra patterns.
 ;; 
 ;; The syntax is:
 ;; 
 ;;   expr :- (mast-lambda clause+)
 ;;   clause :- (pat expr)
 ;;   pat :- <normal pat>
 ;;     | (map map-name pat*)
 ;;     | (field field-name bound-ident)
 ;; 
 ;; where "map-name" may be "_" for any field to match, and ordering
 ;; of "pat" within "map" does not matter, and "field" binds an entire
 ;; field named "field-name" to "bound-ident". Field contents can be
 ;; matched with normal patterns, or with "map".
 ;; 
 ;; Our implementation simply recognizes "map" and "field" as special
 ;; syntax, and replaces them with other syntax.

 (require-for-syntax (lib "plt-match.ss"))
 (require-for-syntax (lib "usual.scm" "wg"))

 ;; This is useful for printing the expanded patterns, and those
 ;; expansions can then be tested separately.
 (define-for-syntax debug-stx
   (case-lambda
    ((val) (begin (write val) (newline) val))
    (vals (begin (write vals) (newline) vals))))

 ;; This is all fun and good, but we ought to see if there is an
 ;; easier way to implement this -- what is "define-match-expander",
 ;; for instance? If we used a plt-match facility for extending
 ;; pattern syntax (assuming there is something suitable), then we
 ;; could use the "match-lambda" etc. constructs, but at least by
 ;; using a separate macro we do not risk breaking all the standard
 ;; match macros.

 ;; Note that we want to do the rewriting recursively, but the problem
 ;; is that it is somewhat hard to know exactly what terms we can
 ;; safely rewrite in the pattern. It may be that we are rewriting
 ;; somewhat too eagerly.
 (define-for-syntax (rewrite-pattern pat)
   (define (map-rw ll)
     (map rewrite-pattern ll))
   (match pat
    ((list 'map name-pat pat ...)
     `(and (list-no-order ,@(map-rw pat) _ ...)
	   (list ,(rewrite-pattern name-pat) _ ...)))
    ((list 'field fname fident)
     `(and ,fident (list ,fname _ ...)))
    (pat (if (list? pat) (map-rw pat) pat))))

 (define-for-syntax (rewrite-clauses cl-stx)
   (let* ((cl-dat (syntax-object->datum cl-stx)))
     (datum->syntax-object 
      cl-stx
      (map ;; (compose debug-stx map) ;; xxx
       (match-lambda
	((list pat expr)
	 (list (rewrite-pattern pat) expr)))
       cl-dat))))

 (define-syntax* mast
   (lambda (stx)
     (syntax-case stx ()
		  ((_ pat . clauses)
		   (quasisyntax/loc stx (match pat #,@(rewrite-clauses (syntax/loc stx clauses))))))))

 (define-syntax* mast-lambda
   (lambda (stx)
     (syntax-case stx ()
		  ((_ . clauses)
		   (quasisyntax/loc stx (match-lambda #,@(rewrite-clauses (syntax/loc stx clauses))))))))

) ; end module



;; NOTE: There appears to be a bug in (at least some versions of) plt-match, as demonstrated by one of the cases below producing a "no" rather than a "yes". So we must make sure to order "and" conditions correctly when using nested "list-no-order" patterns.

#;
(begin
  (require (lib "plt-match.ss"))

  (display
   (match
    '(555)
    ((and (list 555)
	  (list-no-order 555))
     "yes") 
    (_ "no"))) ;; prints "yes"

  (display
   (match
    '((555))
    ((list-no-order (and (list 555)
			 (list-no-order 555)))
     "yes") 
    (_ "no"))) ;; prints "no"

  (display
   (match
    '((555))
    ((list-no-order (and (list-no-order 555)
			 (list 555)))
     "yes") 
    (_ "no"))) ;; prints "yes"

  (display
   (match
    '((555))
    ((list (and (list 555)
		(list-no-order 555)))
     "yes") 
    (_ "no")))) ;; prints "yes"
