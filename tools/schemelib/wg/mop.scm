;; 
;; mop.scm
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

;; Defines a meta-object protocol (MOP) for querying the model of an
;; analyzed program. In our case, what we are querying is an AST and
;; some additional information. This structure makes pretty printing
;; easy, but is not necessarily optimal for queries like this. Still,
;; it's not that bad, and when these queries are sufficient, the user
;; does not really have to care about the queried structure.
(module
 mop
 mzscheme

 (require (lib "ast-spec.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "prop-util.scm" "wg"))
 (require (lib "tables.scm" "wg"))
 (require (lib "declarations.scm" "wg"))
 (require (lib "usual.scm" "wg"))
 (require (lib "graph.scm" "wg"))
 (require (prefix h. (lib "hash.scm" "wg")))
 (require (prefix e. (lib "environment.scm" "wg")))

 (define* (make-qname names)
   (symbol-join names "::"))

 ;; If the passed expression names a value, this function returns
 ;; the relevant name-ref (sub)expression; #f is returned otherwise.
 (define* (expr-get-name-ref ast)
   (cond
    ((name-ref? ast)
     ast)
    ((any-pred dot-expr? arrow-expr? ast)
     (expr-get-name-ref (get-expr ast 'expr2)))
    (else
     #f)))

 (define/kw* (get-expr ast #:optional (name 'expr))
   (fget-reqd-nlist-elem-1 ast name))

 (define* (get-texpr-or-tvar ast)
   (or (fget-opt-nlist-elem-1 ast 'type)
       '(tvar)))
 
 (define* (get-texpr ast)
   (or (fget-opt-nlist-elem-1 ast 'type)
       (error "expression has no type" ast)))

 (define* (strip-any-ref-to texpr)
   (if (ref-to? texpr)
       (get-texpr texpr)
       texpr))

 (define* (global-decl-and-cont? ast)
   (and (global-decl? ast)
	(global-decl-cont? ast)))

 (define* (function-decl? ast)
   (list-named-one-of? ast '(global-func inst-meth class-meth cxx-ctor cxx-dtor cxx-oper cxx-conv)))

 (define* (global-decl? ast)
   (or (function-decl? ast)
       (list-named-one-of? ast '(cxx-class cxx-struct cxx-union cxx-typedef global-var class-var inst-var))))

 (define* (global-decl-cont? ast)
   (list-named-one-of? ast '(cxx-unit cxx-namespace body cxx-class cxx-struct cxx-union)))

 (define* (texpr-check-func-t texpr)
   (unless (list-named? texpr 'func-t)
	   (error "not a function type" texpr))
   texpr)

 (define* (func-get-func-t ast)
   (texpr-check-func-t (fget-reqd-nlist-elem-1 ast 'type)))

 (define (nlist-get-reqd-type ast)
   (fget-reqd-nlist-elem ast 'type))

 (define* (func-modify-arg ast f)
   (nlist-modify-by-path ast '(type func-t args arg) f))
   
 (define* (func-modify-returns ast f)
   (nlist-modify-by-path ast '(type func-t returns) f))

 ;; Returns #f for an unresolved base type.
 (define (get-base-type-id ast)
   (unless (list-named? ast 'base)
	   (error "argument" ast))
   (let ((type (fget-reqd-nlist-elem-1 ast 'type)))
     (unless (list-named? type 'tname-ref)
	     (error (format "bases of type ~s unsupported" type)))
       (let ((id-ref (fget-opt-nlist-elem-1 type 'decl-id-ref)))
	 ;;(write-nl (list "ref" id-ref))
	 id-ref)))

 ;; Gets the decl IDs for all immediate bases of "ast". Unresolved
 ;; bases are excluded. The order is the same as in the declaration.
 ;; 
 ;; XXX We only support "public" inheritance at present, and treat all
 ;; inheritance as public inheritance. Anything else is quite rare
 ;; anyway.
 (define (get-base-refs d-table ast)
   (let* ((elem-list (get-opt-nlist-elem-1up-e ast 'bases))
	  (ref-list (map get-base-type-id elem-list)))
     ;;(write-nl (list "reflist" (compact ref-list)))
     (compact ref-list)))

 ;; Gets the decl IDs recursively for all bases of "ast". Implemented
 ;; so that inheritance loops will not cause non-termination. Left
 ;; bases are favored over right ones; the leftmost base is fully
 ;; recursed before any others.
 (define* (get-base-refs-r d-table ast)
   (define result '())
   (define (trav ast)
     (for-each
      (lambda (id-ref)
	(unless (include-eq? result id-ref)
		(push-set! result id-ref)
		(trav (get-decl-by-id d-table id-ref))))
      (get-base-refs d-table ast)))
   (trav ast)
   result)
       
 ;; Checks for cycles in "bases" of the given element. Inheritance
 ;; cycles are not allowed. "decl-id-ref" attributes should already be
 ;; present as appropriate. Unknown bases are ignored.
 ;; 
 ;; Note that in C++ it may be okay to inherit from the same base more
 ;; than once via other bases. We just want to make sure that no base
 ;; inherits from itself, as that is not legal in C++, and we might
 ;; want to let the user know about such problems.
 (define (bases-cyclic? ast tables)
   #f) ;; xxx may not use get-base-refs-r here

 ;; Determines whether all of the bases of the given record are known.
 ;; Not just the immediate children, but the whole ancestor hierarchy.
 ;; In practice, if bases-known? is not true for a record, then we may
 ;; not know all of the methods it has, and this may lead to incorrect
 ;; name resolution (in the C++ sense).
 (define* (bases-known? ast tables)
   (define d-table (get-d-table tables))

   ;; Used to keep track of bases already examined. We do not want to
   ;; examine any branch more than once.
   (define id-list '())

   (define (seen? id)
     (if (include-eq? id-list id)
	 #t
	 (begin
	   (push-set! id-list id)
	   #f)))

   (define (do-decl ast)
     ;;(write-nl (list "decl" ast))
     (let ((id (fget-reqd-nlist-elem-1 ast 'decl-id)))
       (if (seen? id)
	   #t
	   (aif bases (fget-opt-nlist-elem ast 'bases)
		(do-bases bases)
		#t))))
	   
   (define (do-bases ast)
     ;;(write-nl (list "bases" ast))
     (let ((bases (cdr ast)))
       (list-membs-all-match? bases do-base)))

   (define (do-base ast)
     ;;(write-nl (list "base" ast))
     (let* ((type (fget-reqd-nlist-elem-1 ast 'type)))
       (unless (list-named? type 'tname-ref)
	       (error (format "bases of type ~s unsupported" type)))
       (let* ((id-ref (fget-opt-nlist-elem-1 type 'decl-id-ref)))
	 ;;(write-nl (list "id" id-ref))
	 (if (not id-ref)
	     #f
	     (let* ((base-decl (get-decl-by-id d-table id-ref)))
	       (do-decl base-decl))))))

   (do-decl ast))

 (define* (get-decl-name ast)
   (fget-reqd-nlist-elem-1 ast 'name))

 (define (get-member-from-list-by-name mlist name)
   (find-first
    mlist
    (lambda (ast)
      (let ((mname (get-decl-name ast)))
	(eq? name mname)))))

 (define* (get-member-by-name ast name)
   (get-member-from-list-by-name
    (get-members-list ast)
    name))

 ;; Currently compares by name only. This is not good enough for
 ;; overloads. xxx
 (define (members-include? mlist melem)
   (true? (get-member-from-list-by-name 
	   mlist 
	   (get-decl-name melem))))

 ;; Gets all members of the specified declaration.
 (define* (get-members-list ast)
   (let* ((members (get-opt-nlist-elem-1up-e ast 'body)))
     members))

 ;; Excludes private members, ctors, and dtors.
 (define (get-inherited-members-list ast)
   (let* ((members (get-opt-nlist-elem-1up-e ast 'body)))
     (filter
      (lambda (ast)
	(and (not (list-named-one-of? ast '(cxx-ctor cxx-dtor)))
	     (let ((access (get-reqd-nlist-elem-1 ast 'access)))
	       (not (eq? access 'private)))))
      members)))

 ;; This function builds a full members list for the container
 ;; specified by "ast", excluding private ones in ancestors. Full in
 ;; terms of all known members. To determine whether the list is
 ;; really complete, call the bases-known? function also.
 (define* (build-full-members-list ast tables)
   ;; We are not quite sure about the order in which we should examine
   ;; the bases, but if we have two parents that both either directly
   ;; or indirectly provide a method, then we shall favor the one
   ;; listed first in the list of bases. Indirectly inheriting the
   ;; same member more than once via different parents does not really
   ;; cause a conflict here.

   (define result (get-members-list ast))
   (define d-table (get-d-table tables))
   (define base-refs (get-base-refs-r d-table ast))
   (write-nl (list "bases" base-refs))
   (for-each
    (lambda (ast)
      (define mlist (get-inherited-members-list ast))
      (for-each
       (lambda (melem)
	 (unless (members-include? result melem)
		 (push-set! result melem)))
       mlist))
    (map (fix get-decl-by-id d-table) base-refs))
   result)

 ;; Looks to see whether the passed members list contains any abstract
 ;; (pure virtual) members. This can be useful in determining which
 ;; classes can be instantiated, and which cannot.
 (define* (has-abstract-members? mlist)
   (find-first
    (lambda (mb)
      (has-true-bool-prop-memb? mb 'pure))
    mlist))

) ; end module
