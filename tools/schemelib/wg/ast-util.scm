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

;; Utilities for manipulating certain kinds of S-expression forms.
;; 
;; In our naming we use this terminology:
;; * memb (any element of a list)
;; * elem (any but the first element of a list)
;; * nlist (named list element)
;; * prop (named list or symbol element)

(module 
 ast-util
 mzscheme 

 (require (lib "usual.scm" "wg"))
 (require* (lib "type-util.scm" "wg"))

 ;; -----------------
 ;; list manipulation

 ;; Like find-first, but applies to elements only. Returns the first
 ;; element of list "l" that satisfies predicate "f", or #f if there
 ;; is no match. The first element is excluded from consideration.
 (define* (find-first-elem l f)
   (if (null? l)
       #f
       (find-first (cdr l) f)))
 
 (define* (has-any-memb-matching? c l)
   (if (null? l)
       #f
       (let* ((memb (car l))
	      (is-match (c memb)))
	 (if is-match #t
	     (has-any-memb-matching? c (cdr l))))))

 (define* (has-any-elem-matching? c l)
   (if (null? l)
       #f
       (has-any-memb-matching? c (cdr l))))

 (define* (all-elems-match? c l)
   (not (has-any-elem-matching? (negate c) l)))

 (define* (map-elems f ll)
   (cons (car ll) (map f (cdr ll))))

 (define* (for-each-elem f ll)
   (for-each f (cdr ll)))

 (define* (map-elems-matching f ll c?)
   (map-elems
    (lambda (elem)
      (if (c? elem)
	  (f elem)
	  elem))
    ll))

 (define* (replace-car ll name)
   (cons name (cdr ll)))

 (define* (modify-car ll f)
   (cons (f (car ll)) (cdr ll)))

 (define* (modify-cdr ll f)
   (cons (car ll) (f (cdr ll))))
 
 (define* (replace-cadr ll elem)
   (cons (car ll) (cons elem (cddr ll))))

 (define* (modify-cadr ll f)
   (replace-cadr ll (f (cadr ll))))

 (define (modify-at ll i f)
   (if (= i 0)
       (cons (f (car ll)) (cdr ll))
       (cons (car ll) (modify-at (cdr ll) (- i 1) f))))

 (define* (replace-third ll elem)
   (modify-at ll 2 (lambda (dummy) elem)))

 (define* (error-no-memb obj arg)
   (error-f "no required member matching ~s in ~s" arg obj))

 (define/kw* (delete-first-matching ll f? #:key fail-f)
   (define (g mm)
     (cond
      ((null? mm) (if fail-f (fail-f) mm))
      ((f? (car mm)) (cdr mm))
      (else (cons (car mm) (g (cdr mm))))))
   (g ll))

 ;; -----------------
 ;; named list transformations...

 (define* (prepend-elem ll elem)
   (cons (car ll) (cons elem (cdr ll))))

 (define (on-elems-trans f)
   (lambda (ll arg)
     (cons (car ll) (f (cdr ll) arg))))

 (define* (set-nlist-elem ast elem)
   (push (reject-nlist-elems ast (car elem)) elem))

 (define* (set-nlist-elem-1 ast name value)
   (push (reject-nlist-elems ast name) (list name value)))

 (define-syntax* set-nlist-elem!
   (syntax-rules ()
     ((_ ast elem)
      (set! ast (set-nlist-elem ast elem)))))

 (define-syntax* set-nlist-elem-1!
   (syntax-rules ()
     ((_ ast name value)
      (set! ast (set-nlist-elem-1 ast name value)))))

 ;; xxx non-standard naming
 (define* (map-elem-lists-named f ll name)
   (map-elems-matching 
    f ll 
    (lambda (elem) (list-named? elem name))))

 ;; xxx non-standard naming
 (define* (map-elem-lists-named-one-of f ll namelist)
   (map-elems-matching 
    f ll 
    (lambda (elem) 
      (list-named-one-of? elem namelist))))

 ;; xxx non-standard naming
 (define* (map-memb-lists-named f lll name)
   (map 
    (lambda (ll)
      (if (list-named? ll name)
	  (f ll)
	  ll))
    lll))

 ;; xxx non-standard naming
 ;;
 ;; Like map-memb-lists-named, but goes through all lists recursively.
 ;; So traverses a tree, essentially. Note that any lists within
 ;; matching lists will not be traversed automatically.
 (define* (map-memb-lists-named-r f list^ name)
   (map 
    (lambda (elem)
      (if (non-empty-list? elem)
	  (if (eq? (car elem) name)
	      (f elem)
	      (map-memb-lists-named-r f elem name))
	  elem))
    list^))

 (define* (map-matching-nlist-r c? f ast)
   (map 
    (lambda (elem)
      (if (non-empty-list? elem)
	  (if (c? elem)
	      (f elem)
	      (map-matching-nlist-r c? f elem))
	  elem))
    ast))

 ;; -----------------
 ;; named list getters...

 (define (nlg-elems f)
   (lambda (ll . args)
     (apply f (cons (cdr ll) args))))

 (define (nlg-res-modify f mf)
   (lambda (ll . args)
     (aif res (apply f (cons ll args))
	  (mf res)
	  #f)))

 (define (nlg-1 f)
   (nlg-res-modify f cadr))

 (define (nlg-1up f)
   (nlg-res-modify f cdr))

 (define (nlg-1up-e f)
   (lambda (ll . args)
     (aif res (apply f (cons ll args))
	  (cdr res)
	  '())))

 (define (nlg-def f)
   (lambda/kw (ll arg #:optional def)
     (aif res (apply f (cons ll (list arg)))
	  res
	  def)))

 (define (nlg-reqd f)
   (lambda (ll arg . kw)
     (aif res (apply f (cons ll (cons arg kw)))
	  res
	  (error-no-memb ll arg))))

 (define (nlg-assq-name ll arg)
   (assq arg ll))

 (define (nlg-find-name ll arg)
   (find-first 
    ll
    (lambda (elem)
      (list-named? elem arg))))

 (define (nlg-find-names ll arg)
   (find-first
    ll
    (lambda (elem)
      (list-named-one-of? elem arg))))

 (define (nlg-find-names-ordered ll names)
   (let ((elem #f))
     (find-first
      names
      (lambda (name)
	(aif res (nlg-find-name ll name)
	     (begin
	       (set! elem res)
	       #t) 
	     #f)))
     elem))

 (define (nlg-map-elems f)
   (lambda (ll . args)
     (cons (car ll) (apply f (cons (cdr ll) args)))))

 (define/kw (nlg-del-name ll arg #:key reqd)
   (delete-first-matching
    ll
    (lambda (elem) (list-named? elem arg))
    #:fail-f (if reqd
		 (lambda () (error-no-memb ll arg))
		 #f)))

 (define (nlg-select-name ll arg)
   (filter
    (lambda (elem) (list-named? elem arg))
    ll))

 (define (nlg-select-names ll arg)
   (filter
    (lambda (elem) (list-named-one-of? elem arg))
    ll))

 (define (nlg-reject-name ll arg)
   (filter
    (lambda (elem) (not (list-named? elem arg)))
    ll))

 (define (nlg-reject-names ll arg)
   (filter
    (lambda (elem) (not (list-named-one-of? elem arg)))
    ll))

 ;; Like get-*, but faster, and relies on all inspected members being
 ;; cons cells.
 (define* fget-opt-nlist-memb nlg-assq-name)
 (define* fget-opt-nlist-elem (nlg-elems nlg-assq-name))
 (define* fget-opt-nlist-memb-1 (nlg-1 nlg-assq-name))
 (define* fget-opt-nlist-elem-1 (nlg-1 fget-opt-nlist-elem))
 (define* fget-opt-nlist-elem-1up-e (nlg-1up-e fget-opt-nlist-elem))
 (define* fget-reqd-nlist-memb (nlg-reqd nlg-assq-name))
 (define* fget-reqd-nlist-elem (nlg-elems fget-reqd-nlist-memb))
 (define* fget-reqd-nlist-memb-1 (nlg-1 fget-reqd-nlist-memb))
 (define* fget-reqd-nlist-elem-1 (nlg-1 fget-reqd-nlist-elem))

 (define* get-opt-nlist-memb nlg-find-name)
 (define* get-opt-nlist-elem (nlg-elems nlg-find-name))
 (define* get-opt-nlist-elem-1up (nlg-1up get-opt-nlist-elem))
 (define* get-opt-nlist-elem-1up-e (nlg-1up-e get-opt-nlist-elem))
 (define* get-reqd-nlist-memb (nlg-reqd nlg-find-name))
 (define* get-reqd-nlist-elem (nlg-elems get-reqd-nlist-memb))
 (define* get-reqd-nlist-memb-1 (nlg-1 get-reqd-nlist-memb))
 (define* get-reqd-nlist-elem-1 (nlg-elems get-reqd-nlist-memb-1))
 (define* get-reqd-nlist-elem-1up (nlg-1up get-reqd-nlist-elem))

 (define* get-opt-nlist-elem-1
   (let ((getval
	  (lambda (ll key def)
	    (let ((elem 
		   (find-first-elem ll (lambda (x) (list-named? x key)))))
	      (if elem (second elem) def)))))
     (case-lambda
      ((ll key) (getval ll key #f))
      ((ll key def) (getval ll key def)))))

 (define* get-opt-nlist-memb-1
   (let ((getval
	  (lambda (ll key)
	    (let ((elem (get-opt-nlist-memb ll key)))
	      (if elem (cadr elem) #f)))))
   (case-lambda
    ((ll key) (getval ll key))
    ((ll key def) (let ((val (getval ll key))) (if val val def))))))

 (define* get-opt-nlist-memb-by-names nlg-find-names)
 (define* get-opt-nlist-elem-by-names (nlg-elems nlg-find-names))
 (define* get-opt-nlist-elem-by-names-1 (nlg-1 (nlg-elems nlg-find-names)))

 ;; Looks for the keys in "keylist" in order, returning the first
 ;; match, if any.
 (define* get-opt-nlist-memb-by-names-o nlg-find-names-ordered)
 (define* get-opt-nlist-elem-by-names-o (nlg-elems nlg-find-names-ordered))
 (define* get-opt-nlist-memb-by-names-o-1 (nlg-1 nlg-find-names-ordered))

 (define* delete-nlist-memb nlg-del-name)
 (define* delete-nlist-elem (nlg-map-elems nlg-del-name))

 (define* select-nlist-membs nlg-select-name)
 (define* select-nlist-elems (nlg-map-elems nlg-select-name))
 (define* select-nlist-membs-by-names nlg-select-names)
 (define* select-nlist-elems-by-names (nlg-map-elems nlg-select-names))

 (define* reject-nlist-membs nlg-reject-name)
 (define* reject-nlist-elems (nlg-map-elems nlg-reject-name))
 (define* reject-nlist-membs-by-names nlg-reject-names)
 (define* reject-nlist-elems-by-names (nlg-map-elems nlg-reject-names))

 ;; -----------------
 ;; complex operations...

 (define* (nlist-modify-by-path ast path f)
   (cond
    ((null? path)
     (error "path must have at least one component"))
    ((null? (cdr path))
     (map-elem-lists-named f ast (car path)))
    (else
     (map-elem-lists-named
      (lambda (elem)
	(nlist-modify-by-path elem (cdr path) f))
      ast
      (car path)))))

 ) ; end module
