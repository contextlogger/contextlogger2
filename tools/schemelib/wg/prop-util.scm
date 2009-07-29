;; 
;; prop-util.scm
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
 prop-util
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))

 ;; -----------------
 ;; properties...

 (define is-prop?
   (lambda (elem key)
     (or (eq? elem key)
	 (and (list-named? elem key)))))

 (define (one-of is?)
   (lambda (elem keys)
     (list-memb-match?
      keys
      (lambda (key)
	(is? elem key)))))

 (define* is-prop-one-of?
   (one-of is-prop?))

 (define* (has-prop-memb? obj key)
   (list-memb-match? 
    obj
    (lambda (x) (is-prop? x key))))

 (define* (reject-prop-membs ll key)
   (filter
    (lambda (elem)
      (not (is-prop? elem key)))
    ll))

 (define* (reject-prop-membs-by-names ll keys)
   (filter
    (lambda (elem) 
      (not 
       (list-memb-match? 
	keys
	(lambda (key) (is-prop? elem key)))))
    ll))

 (define* (set-prop-memb ll elem)
   (let ((key (get-prop-name elem)))
     (push (reject-prop-membs ll key) elem)))

 (define* (set-prop-membs ll names)
   (for-each
    (lambda (name)
      (set! ll (set-prop-memb ll name)))
    names)
   ll)

 (define (get-prop-name elem)
   (cond ((symbol? elem) elem)
	 ((named-list? elem) (car elem))
	 (else (error "cannot determine key"))))

 ;; -----------------
 ;; boolean properties...

 (define is-true-bool-prop?
   (lambda (key elem)
     (or (eq? elem key)
	 (and (list-named? elem key)
	      (not (null? (cdr elem)))
	      (true? (cadr elem))))))

 (define (is-true-bool-prop-one-of? names elem)
   (list-memb-match?
    names
    (lambda (name) (is-true-bool-prop? name elem))))

 ;; Looks for a member with the specified key, such that the member
 ;; either is the key, or it is a list where the first member is the
 ;; key, and the second member is #t.
 (define* (has-true-bool-prop-memb? ll key)
   (list-memb-match?
    ll
    (fix is-true-bool-prop? key)))

 (define* (has-true-bool-prop-memb-one-of? ll keylist)
   (list-memb-match? 
    keylist
    (lambda (x) (has-true-bool-prop-memb? ll x))))

 ;; Note:: Returns the name, not the entire element.
 (define* (get-true-bool-prop-memb-one-of? ll names)
   (let/ec esc
	   (for-each
	    (lambda (elem)
	      (if (is-true-bool-prop-one-of? names elem)
		  (esc (get-prop-name elem))))
	    ll)
	   (esc #f)))

)
