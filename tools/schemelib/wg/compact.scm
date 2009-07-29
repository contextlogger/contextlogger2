;; 
;; compact.scm
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

;; Utilities for compacting and flattening lists. The terms "compact"
;; and "flatten" come from Ruby, but the semantics here differ somewhat.

(module 
 compact
 mzscheme

 (require (lib "usual.scm" "wg"))

 ;; Compacts the specified list, returning the result. Does not
 ;; compact recursively.
 (define (shallow-compact ll)
   (filter
    (lambda (ee) (not (eq? ee 'nocode)))
    ll))

 ;; Compacts the specified list, returning the result. Recursively
 ;; compacts any lists within the given list.
 (define (deep-compact ll)
   (let ((nl (shallow-compact ll)))
     (map (lambda (x) (if (list? x) (deep-compact x) x)) nl)))

 ;; Flattens the given list so that any "splice" lists within the list
 ;; get spliced into their parent lists. Works recursively.
 (define (splice-fold ll)
   (letrec ((f (lambda (l r)
		 (if (null? r) l
		     (let ((e (car r))
			   (nr (cdr r)))
		       (if (list? e)
			   (if (eq? (car e) 'splice)
			       (f (append l (cdr (splice-fold e))) nr)
			       (f (append l (list (splice-fold e))) nr))
			   (f (append l (list e)) nr)))))))
     (f '() ll)))

 ;; Does both deep compaction and flattening.
 ;; 
 ;; Example use:
 ;; 
 ;;  (define-syntax list-c
 ;;    (syntax-rules ()
 ;;      ((_ code ...)
 ;;       (compact-flatten (list code ...)))))
 ;;
 ;;  (list-c 1 'nocode (sc 2 3) 'nocode 4) ; #=> (1 2 3 4)
 (define (compact-flatten ll)
   (splice-fold (deep-compact ll)))

 (define* (maybe-code code)
   (or code 'nocode))
 
 ;; If code.
 (define-syntax ic
   (syntax-rules ()
     ((ic cond code ...)
      (if cond (sc code ...) 'nocode))))

 ;; Unless code.
 (define-syntax uc
   (syntax-rules ()
     ((uc cond code ...)
      (if cond 'nocode (sc code ...)))))

 ;; Splice this code. Think "," within a quasiquote.
 (define (sc . args) 
   (cons 'splice args))

 ;; Splice this list. Think ",@" within a quasiquote.
 (define (sc-list ll)
   (cons 'splice ll))

 (define (write-nl-c ll)
   (write-nl 
    (if (list? ll) (compact-flatten ll) ll)))

 (define* (pretty-nl-c ll)
   (pretty-nl 
    (if (list? ll) (compact-flatten ll) ll)))

 (provide ic sc sc-list uc 
	  shallow-compact deep-compact compact-flatten
	  write-nl-c))
