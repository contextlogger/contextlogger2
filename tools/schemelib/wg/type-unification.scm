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

;; Provides functionality for comparing types directly, without any
;; prior simplification or modification. Ignores annotations, and
;; considers equality only in the sense appropriate for the C++
;; language.
;; 
;; Comparison of function types is also supported, so function
;; overloads can be resolved simply by constructing a type expression
;; for a called function based on type information derived from the
;; call context, and then by comparing the type expression against the
;; set of overloads of a function by that name. A variable argument
;; specification is supported on the left side of the comparison, so
;; when resolving overloads, pass the overload candidate as "t1"; any
;; zero or more arguments on the right will match "varargs".
;; 
;; Note that given how a single type could be referred to by different
;; names, we prefer to compare type IDs ("decl-id-ref" values) instead
;; of comparing names; perhaps C++ also has type identifiers, as
;; returned by typeid(x).
;; 
;; Optionally considers typedefs, if what is necessary for resolving
;; them is provided. Typedefs are not resolved unnecessarily, i.e.
;; when types can be deemed unequal even without resolving the
;; typedef.
;; 
;; Optionally considers implicit coercions. If say a function
;; parameter has type X, and the corresponding argument has type Y,
;; and Y implicitly coerces to X in C++, then we might want to
;; consider those types equal. To support coercions, a function that
;; determines the coercion relation between two types must be
;; provided; the relation can be either unidirectional (in either
;; direction) or bidirectional.
;; 
;; Optionally considers subtyping. If type Y inherits from type X,
;; then it is possible to consider Y equal to X. This issue is be
;; similar to that of implicit coercions, and the mechanism for
;; supporting subtyping is the same as for coercions. See above.
;; 
;; The "equiv.ss" library at Planet might be potentially useful in
;; implementing recursive comparisons such as these. SRFI 85 also
;; discussed Recursive Equivalence Predicates, but was withdrawn.
(module
 type-unification
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 
 (define* (texpr-eq? t1 t2 . kw)
   (true? (apply texpr-unify t1 t2 kw)))

 ;; If unification is successful, returns (list t1' t2'), where t1'
 ;; and t2' are the unified versions of t1 and t2. If unification is
 ;; not successful, returns #f.
 ;;
 ;; t1, t2:: May not be "type" elements; must be type expressions.
 ;; td-texpr:: A function that takes the ID of a declaration and
 ;;            returns its type, or #f if the declaration is not a
 ;;            typedef.
 ;; coerce?:: A function that takes the IDs of two declarations, and
 ;;           returns true iff the second type can be coerced to the
 ;;           first type. This can be used to account for both
 ;;           implicit coercions and subtype relationships.
 (define/kw* (texpr-unify t1 t2 #:key td-texpr coerce?)
   (define (unify-texpr t1 t2)
     (define n1 (car t1))
     (define n2 (car t2))
     (cond
      ((eq? n1 n2)
       (unify-same t1 t2))
      ((eq? n1 'tvar)
       (list t2 t2))
      ((eq? n2 'tvar)
       (list t1 t1))
      ((not td-texpr)
       #f)
      ((eq? n1 'tname-ref)
       (unify-td-left t1 t2))
      ((eq? n2 'tname-ref)
       (unify-td-right t1 t2))
      (else #f)))

   ;; May not be used if td-texpr is #f.
   ;; t1:: Of type tname-ref, possibly a typedef.
   ;; t2:: Not of type tname-ref, or not of the same name as t1.
   (define (unify-td-left t1 t2)
     (aand* id1 (fget-opt-nlist-elem-1 t1 'decl-id-ref)
	    new-t1 (td-texpr id1)
	    (unify-texpr new-t1 t2)))

   ;; May not be used if td-texpr is #f.
   ;; t1:: Not of type tname-ref, or not of the same name as t2.
   ;; t2:: Of type tname-ref, possibly a typedef.
   (define (unify-td-right t1 t2)
     (aand* id2 (fget-opt-nlist-elem-1 t2 'decl-id-ref)
	    new-t2 (td-texpr id2)
	    (unify-texpr t1 new-t2)))

   ;; The idea here is to be semantically the same as (or (unify-td-left
   ;; t1 t2) (unify-td-left t2 t1)), but more efficient. May not be used
   ;; if td-texpr is #f.
   ;; t1:: Of type tname-ref, possibly a typedef.
   ;; t2:: Of type tname-ref, but not of the same name as t1; possibly
   ;;      a typedef.
   (define (unify-either-td t1 t2)
     ;; Important: We are not to call "unify-texpr" more than once here.
     (aif new-t1 (aand* id1 (fget-opt-nlist-elem-1 t1 'decl-id-ref)
			(td-texpr id1))
	  (unify-texpr new-t1 t2)
	  (aand* id2 (fget-opt-nlist-elem-1 t2 'decl-id-ref)
		 new-t2 (td-texpr id2)
		 (unify-texpr t1 new-t2))))

   (define (unify-type-attrs ast1 ast2)
     (let* ((t1 (get-texpr ast1))
	    (t2 (get-texpr ast2))
	    (result (unify-texpr t1 t2)))
       (and result
	    (list (set-texpr ast1 (first result))
		  (set-texpr ast2 (second result))))))

   (define (get-texpr ast)
     (fget-reqd-nlist-elem-1 ast 'type))

   (define (set-texpr ast texpr)
     (set-nlist-elem-1 ast 'type texpr))

   ;; t1:: Some type expression.
   ;; t2:: Same kind of type expression.
   (define (unify-same t1 t2)
     (define n (car t1))
     (cond

      ((eq? n 'tvar)
       ;; Would need to ensure the type variables can be equal, but
       ;; for now, they are always "free" variables (since we have no
       ;; type IDs yet), and hence match anything.
       (list t1 t2))

      ((eq? n 'tname-ref)
       (let ((id1 (fget-opt-nlist-elem-1 t1 'decl-id-ref))
	     (id2 (fget-opt-nlist-elem-1 t2 'decl-id-ref)))
	 (cond
	  ((and id1 id2)
	   (cond
	    ((eq? id1 id2)
	     (list t1 t2))
	    ((and coerce? (coerce? id1 id2))
	     (list t1 t2))
	    (else
	     (and td-texpr (unify-either-td t1 t2)))))
	  (id1
	   (unify-td-left t1 t2))
	  (id2
	   (unify-td-right t1 t2))
	  (else
           ;; We only resort to comparing names if both sides have an
           ;; undefined name.
	   (let ((name1 (fget-reqd-nlist-elem-1 t1 'name))
		 (name2 (fget-reqd-nlist-elem-1 t2 'name)))
	     (and (eq? name1 name2) (list t1 t2)))))))

      ((memq n '(const volatile ptr-to ref-to))
       (unify-type-attrs t1 t2))

      ;; Only considered equal if the number of elements is equal (is
      ;; this true?), but unfortunately we would need to be able to
      ;; compare the constant expressions to determine equality, so
      ;; for now, element count is ignored. Indeed, we have no plans
      ;; to implement a C++ evaluator, so this restriction may be
      ;; permanent. XXX
      ((eq? n 'array-of)
       (unify-type-attrs t1 t2))

      ((eq? n 'func-t)
       (let ((returns1 (fget-opt-nlist-elem t1 'returns))
	     (returns2 (fget-opt-nlist-elem t2 'returns))
	     (args1 (fget-reqd-nlist-elem t1 'args))
	     (args2 (fget-reqd-nlist-elem t2 'args)))
	 (aand* new-r (unify-ret returns1 returns2)
		new-a (unify-args args1 args2)
		(list 
		 (set-func-attrs t1 (first new-r) (first new-a))
		 (set-func-attrs t2 (second new-r) (second new-a))))))

      (else #f)))

   (define (set-func-attrs f returns args)
     (when returns
	   (set-nlist-elem! f returns))
     (when args
	   (set-nlist-elem! f args))
     f)

   ;; returns1, returns2:: Either argument may be #f. (Some function
   ;;                      like object such as ctors and dtors return
   ;;                      nothing.)
   ;; Returns:: Returns two "returns" elements, or #f.
   (define (unify-ret returns1 returns2)
     (cond
      ((and returns1 returns2)
       (unify-type-attrs returns1 returns2))
      (returns1 #f)
      (returns2 #f)
      (else (list returns1 returns2))))

   ;; args1:: An "args" element. May have a variable-length argument
   ;;         list ("varargs") as its last "parameter".
   ;; args2:: An "args" element. May not have "varargs".
   ;; Returns:: Returns two "args" elements, or #f.
   (define (unify-args args1 args2)
     (let* ((arglist1 (cdr args1))
	    (arglist2 (cdr args2))
	    (varargs1? (and (pair? arglist1)
			    (list-named? (last arglist1) 'varargs))))
       (when varargs1?
	     (set! arglist1 (drop-right arglist1 1)))
       ;;(write-nl (list 'arglists varargs1? arglist1 arglist2))

       (let ((len1 (length arglist1))
	     (len2 (length arglist2)))
	 (and (or (= len1 len2)
		  (and varargs1? (<= len1 len2)))
              ;; Note that using (take arglist2 len1) is unnecessary
              ;; here, due to the SRFI-1 "map" semantics.
	      (aand* pairs (unify-pairwise 
			    unify-type-attrs 
			    (s:map list arglist1 arglist2))
		     (list (cons 'args (map first pairs))
			   (cons 'args (map second pairs))))))))

   (define (unify-pairwise unify pairs)
     (fold-right-while-true
      (lambda (this result)
	(aand* u-this (apply unify this)
	       (cons u-this result)))
      '()
      pairs))
   
   (define (fold-right-while-true kons knil clist)
     (fold-while-true kons knil (reverse clist)))

   ;; Like "fold", but takes no more than one list, and if "kons"
   ;; returns a false value, returns immediately with value #f,
   ;; without iterating over the rest of the list.
   (define (fold-while-true kons knil clist)
     (and knil
	  (if (null? clist)
	      knil
	      (fold-while-true kons (kons (car clist) knil) (cdr clist)))))

   ;;(write-nl (list 'unify t1 t2))
   (unify-texpr t1 t2))

) ; end module
