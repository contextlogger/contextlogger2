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
;;

(module 
 pass-ctx-info
 mzscheme 

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-spec.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "prop-util.scm" "wg"))
 (require (lib "tables.scm" "wg"))
 (require (lib "mop.scm" "wg"))
 (require (lib "type-unification.scm" "wg"))

 ;; To allow for the use of assq with certain known node types in
 ;; subsequent passes, this pass converts all atomic (a single symbol)
 ;; attributes of those node types into nlist form.
 (define* (pass-prop-to-nlist ast tables)
   (letrec
       ((trans-node
	 (lambda (ast)
	   (cond
            ;; Here list the node types whose attributes are to be
            ;; nlist converted. Not any containing nodes, just those
            ;; ones.
	    ((list-named-one-of? ast '(cxx-unit cxx-namespace cxx-class cxx-struct cxx-union cxx-func cxx-ctor cxx-dtor cxx-conv cxx-oper cxx-typedef cxx-var call-expr dot-expr arrow-expr new-expr py-unit py-module py-class py-func py-ctor py-var))
	     (map-elems trans-attr ast))
	    ((named-list? ast)
	     (map-elems trans-node ast))
	    (else ast))))

	(trans-attr
	 (lambda (ast)
	   (cond 
	    ((symbol? ast)
	     (list ast #t))
	    (else (trans-node ast))))))

     (list (trans-node ast) tables)))

 (define* pass-stmt-seq
   (carry-tables
    (lambda (root-ast)
      (define (branch-trans ast)
	(if (stmt-seq? ast)
	    (replace-car ast 'block)
	    ast))

      (define (inline-trans ast)
	(define (f result ast)
	  (if (null? ast)
	      result
	      (let ((this (car ast))
		    (rest (cdr ast)))
		(cond
		 ((stmt-seq? this)
		  (alet stmts (fget-reqd-nlist-elem this 'stmts)
			(f (append result (inline-trans (cdr stmts))) rest)))
		 (else
		  (f (push result (trans this)) rest))))))

	(f '() ast))

      (define (trans ast)
	(cond
	 ((list-named-one-of? ast '(stmt then-stmt else-stmt))
	  (modify-cadr ast branch-trans))
	 ((list-named? ast 'stmts)
	  (modify-cdr ast inline-trans))
	 ((stmt-seq? ast)
	  (error "stmt-seq in illegal context" ast))
	 ((named-list? ast)
	  (map-elems trans ast))
	 (else ast)))
	  
      (trans root-ast))))

 (define* pass-lift-stmt
   (carry-tables
    (lambda (ast)
      (define (check-res res)
	(if (or (not (list? res))
		(!= (length res) 2)
		(not (list? (first res))))
	    (error "not a result" res)))

      (define (res-to-list res)
	(check-res res)
	(push (first res) (second res)))

      ;; Handles an expression. Returns a list of lifted statements
      ;; and the possibly modified expression.
      (define (do-expr ast)
	(define stmt-list '())

	(define (g res)
	  ;;(unless (null? (first res)) (write-nl (list "in-res" (first res))))
	  (check-res res)
          ;; Watch out scoping here; we must set the "stmt-list" so
          ;; that the changes are visible where we want them.
	  (set! stmt-list (append stmt-list (first res)))
	  ;;(write-nl (list 'accum stmt-list))
	  (second res))

	(define (f ast)
	  (if (list-named? ast 'lift-stmt-expr)
	      (let* ((expr (get-reqd-nlist-elem-1 ast 'expr))
		     (stmt (get-reqd-nlist-elem-1 ast 'stmt)))
                ;; We must look for expressions in the replacement
                ;; expression, as well as in the introduced statement.
		;;(write-nl (list "lift" stmt expr))
		(append-set! stmt-list (do-stmt stmt))
		(f expr))
	      (g (do-thing ast))))

	;;(write-nl (list "expr" ast))
	(set! ast (f ast))
	;;(write-nl (list "expr res" stmt-list ast))
	(list stmt-list ast))

      ;; Handles a single statement that should be retained as a
      ;; single statement. This is done by turning it into a block if
      ;; any statements are lifted, assuming it is not already a
      ;; block. The resulting statement is returned.
      (define (do-branch ast)
	(alet stmt-list (do-stmt ast)
	      (if (> (length stmt-list) 1)
		  `(block (stmts ,@stmt-list))
		  (car stmt-list))))

      ;; Handles a statement that may contain multiple expressions,
      ;; and possibly also other statements, not including any lifted
      ;; ones. Returns a list of statements, which includes any lifted
      ;; statements, as well as the possibly modified "ast".
      (define (do-thing ast)
	(define stmt-list '())

	(define (g res)
	  ;;(unless (null? (first res)) (write-nl (list "in-res" (first res))))
	  (check-res res)
	  (set! stmt-list (append stmt-list (first res)))
	  ;;(write-nl (list 'accum stmt-list))
	  (second res))

	;;(define g-expr (compose g do-expr))
	(define (g-expr ast)
	  (set! ast (g (do-expr ast)))
	  ;;(write-nl (list 'stmt-list stmt-list))
	  ast)

	(define (do-attr ast)
	  (cond
	   ((list-named-one-of? ast '(expr expr1 expr2 init cond-expr then-expr else-expr))
	    (modify-cadr ast g-expr))
	   ((list-named? ast 'exprs)
	    (map-elems g-expr ast))
	   ((list-named-one-of? ast '(stmt then-stmt else-stmt))
	    (modify-cadr ast do-branch))
	   ((list-named? ast 'stmts)
	    (do-stmts ast))
	   (else ast)))
	  
	;;(write-nl (list "thing" ast))
	(set! ast (map-elems do-attr ast))
	;;(write-nl (list "thing res" stmt-list ast))
	(list stmt-list ast))

      ;; Handles anything in a statement position. Returns a list of
      ;; statements, which includes any lifted statements, as well as
      ;; the possibly modified "ast".
      (define (do-stmt ast)
	;;(write-nl (list "stmt" ast))

	(cond
	 ((not (named-list? ast))
	  (error "illegal item in statement position" ast))
	 
	 ((list-named? ast 'block)
	  (list (do-block ast)))

	 (else
	  (res-to-list (do-thing ast)))))

      ;; Transforms "stmts" and basically any element just containing
      ;; a list of statements.
      (define (do-stmts ast)
	(cons (car ast)
	      (apply append (map do-stmt (cdr ast)))))

      ;; Transforms a "block", returning the transformed block.
      (define (do-block ast)
	(map-elem-lists-named do-stmts ast 'stmts))

      ;; We presently only support expressions ultimately enclosed
      ;; within a block; this covers all function bodies, but not
      ;; variable initializers and such things.
      (map-memb-lists-named-r do-block ast 'block))))

 ;; Replaces the special type "self-type" with a reference to the name
 ;; of the containing class.
 (define* (pass-self-type ast^ tables)
   (letrec
       ((trans-type
	 (lambda (class-name ast)
	   (map-memb-lists-named-r 
	    (lambda (stype)
	      (if class-name
		  `(tname-ref (name ,class-name))
		  (error "self-type not in named class context" stype)))
	    ast
	    'self-type)))

	(trans-type-attr
	 (lambda (class-name ast)
	   (map-elem-lists-named (fix trans-type class-name) ast 'type)))

	(trans-bases-attr 
	 (lambda (class-name ast)
	   (map-elem-lists-named
	    (lambda (bases)
	      (map-elem-lists-named
	       (fix trans-type-attr class-name)
	       bases
	       'base))
	    ast
	    'bases)))

	(trans
	 (lambda (class-name ast)
	   (cond

            ;; Affects current class name. Has no type, but its bases
            ;; may have.
	    ((list-named-one-of? ast '(cxx-class cxx-struct cxx-union))
	     (begin
	       (set! ast (trans-bases-attr class-name ast))
	       (let ((name (get-opt-nlist-elem-1 ast 'name)))
		 (map-elems (fix trans name) ast))))

	    ((list-named? ast 'bases)
	     ast)

	    ((list-named-one-of? ast '(type type-expr))
	     (trans-type class-name ast))

	    ((named-list? ast)
	     (map-elems (fix trans class-name) ast))

	    (else 
	     (begin
	       ;;(write-nl (list "self-type" ast))
	       ast))))))

     (list (trans #f ast^) tables)))

 ;; This pass ensures that each function has a valid type. We also
 ;; include annotations known to be associated with the type, but as
 ;; this can only be done to known types, we suggest explicitly giving
 ;; the function a "type" if it must have other type annotations.
 (define* (pass-func-type ast^ tables)
   (letrec
       ((trans
	 (lambda (ast)
	   (cond
	    ((list-named-one-of? ast '(cxx-unit cxx-namespace body cxx-class cxx-struct cxx-union))
	     (map-elems trans ast))
	    
	    ((list-named-one-of? ast '(cxx-ctor cxx-dtor cxx-func))
	     (let ((type (fget-opt-nlist-elem ast 'type))
		   (known-list '(returns args leaving)))

               ;; If has no "type" element, give it one, using known
               ;; type components from the declaration, if any.
	       (unless type
		       (alet func-t '(func-t)
			     (for-each
			      (lambda (name)
				(awhen elem (get-opt-nlist-elem ast name)
				       (push-set! func-t elem)))
			      known-list)
			     (set! type (list 'type func-t))))

               ;; Remove these. Just "type" is enough, as it contains
               ;; all of these components.
	       (set! ast (reject-nlist-elems-by-names ast known-list))

               ;; Ensure the "func-t" type has all the elements we are
               ;; expecting.
	       (let* ((func-t (fget-reqd-nlist-elem type 'func-t)))

		 ;; Every function requires a return type, which
		 ;; defaults to "void". Ctors and dtors are exceptions
		 ;; here.
		 (when (eq? (car ast) 'cxx-func)
		       (alet returns (get-opt-nlist-elem func-t 'returns)
			     (unless returns
				     (push-set! func-t '(returns (type (tname-ref (name void))))))))

		 ;; Every function should have an argument list, too,
		 ;; even if empty.
		 (alet args (get-opt-nlist-elem func-t 'args)
		       (unless args
			       (push-set! func-t '(args))))

		 (set-nlist-elem! type func-t)
		 (set-nlist-elem! ast type))

	       ast))
	    
	    (else ast)))))

     (list (trans ast^) tables)))

 ;; Renames certain elements so as to have the name reveal information
 ;; that this pass derives from the context in which the element
 ;; appears. For instance, a funtion declaration appearing within a
 ;; class body is actually a method declaration.
 (define* (pass-ctx-rename ast^ tables)
   (letrec
       ((trans
	 (lambda (ctx ast)
	   (cond
	    ;; Needs no renaming, but may contain something to rename.
	    ((list-named-one-of? ast '(cxx-unit cxx-namespace cxx-ctor cxx-dtor body))
	     (map-elems (fix trans ctx) ast))
	    
	    ;; Needs no renaming, but may contain something to rename.
	    ((list-named-one-of? ast '(cxx-class cxx-struct cxx-union))
	     (map-elems (fix trans 'in-class) ast))
	    
	    ;; Needs no renaming, but may contain something to rename.
	    ((list-named-one-of? ast '(block if-stmt while-stmt for-stmt do-while-stmt then-stmt else-stmt switch-stmt trap-block stmt stmts))
	     (map-elems (fix trans 'in-block) ast))
	    
	    ;; Needs renaming, and may contain something to rename.
	    ((list-named-one-of? ast '(cxx-func))
	     (begin
	       (map-elems 
		(fix trans ctx)
		(replace-car 
		 ast
		 (cond
		  ((eq? ctx 'global) 'global-func)
		  ((eq? ctx 'in-class)
		   (if (has-true-bool-prop-memb? ast 'static)
		       'class-meth 'inst-meth))
		  (else (error "unexpected context" ctx)))))))
	    
	    ;; Needs renaming, but contains nothing to rename.
	    ((list-named-one-of? ast '(cxx-var))
	     (replace-car
	      ast
	      (cond
	       ((eq? ctx 'global) 'global-var)
	       ((eq? ctx 'in-class)
		(if (has-true-bool-prop-memb? ast 'static)
		    'class-var 'inst-var))
	       ((eq? ctx 'in-block) 'local-var)
	       (else (error "unexpected context" ctx)))))
	    
	    ;; Needs no renaming, and contains nothing to rename.
	    (else ast)))))

     (list (trans 'global ast^) tables)))

 ;; This pass converts "public", "protected", and "private" flags into
 ;; a single "access" element. Note that (public #f) etc. elements do
 ;; not really make sense.
 (define* (pass-access ast^ tables)
   (letrec
       ((keys '(public protected private))

	(do-access
	 (lambda (ast)
	   (if (get-opt-nlist-elem ast 'access)
	       ast
	       (let ((access (get-true-bool-prop-memb-one-of? ast keys)))
		 (if access
		     (push (reject-prop-membs-by-names ast keys) `(access ,access))
		     ast)))))

	(trans
	 (lambda (ast)
	   (cond

	    ((list-named-one-of? ast '(cxx-unit cxx-namespace body))
	     (map-elems trans ast))
	    
	    ((list-named-one-of? ast '(cxx-class cxx-struct cxx-union))
	     (map-elems trans (do-access ast)))
	    
	    ((list-named-one-of? ast '(cxx-ctor cxx-dtor class-meth inst-meth cxx-enum inst-var class-var cxx-typedef))
	     (do-access ast))
	    
	    (else ast)))))

     (list (trans ast^) tables)))

 ;; This pass assigns "access" to any member that does not yet have an
 ;; access flag set. Struct and union members get "public" access by
 ;; default, and classes get "private" access by default.
 (define* pass-default-access
   (carry-tables
    (lambda (ast)
      (define (ensure-access access ast)
	;;(write-nl (list "ensure-access" access ast))
	(let ((have-it (get-opt-nlist-elem ast 'access)))
	  (if have-it
	      ast
	      (push ast `(access ,access)))))

      (define (trans-decl access ast)
	;;(write-nl (list "trans-decl" access (car ast)))
	(map-elem-lists-named (fix trans-body access) ast 'body))

      (define (trans-body access ast)
	;;(write-nl (list "trans-body" access (car ast)))
	(map-elems
	 (lambda (ast)
	   (cond 
	    ((list-named-one-of? ast '(cxx-class cxx-struct cxx-union))
	     (trans #f (ensure-access access ast)))
	    ((list-named-one-of? ast '(cxx-ctor cxx-dtor class-meth inst-meth cxx-enum inst-var class-var cxx-typedef))
	     (ensure-access access ast))
	    (else ast)))
	 ast))

      (define (trans access ast)
	(cond 
	 ((list-named? ast 'cxx-class)
	  (trans-decl 'private ast))
	 ((list-named-one-of? ast '(cxx-struct cxx-union))
	  (trans-decl 'public ast))
	 ((named-list? ast)
	  (map-elems (fix trans #f) ast))
	 (else ast)))
	      
      (trans #f ast))))
	    
 ;; This pass has the children of an "export" parent flagged as
 ;; "export". Similarly for "dll-export", except that "dll-export" is
 ;; only set for non-pure, non-inline functions.
 (define* (pass-flags ast^ tables)
   (letrec
       ((trans
	 (lambda (is-e is-c ast)

	   (let ((do-flag
		  (lambda (ast)

		    (if (and is-e
			     (not (has-prop-memb? ast 'export)))
			(push-set! ast '(export #t)))
		    
		    (if (and is-c
			     (not (has-prop-memb? ast 'dll-export))
			     (not (has-true-bool-prop-memb? ast 'pure))
			     (not (has-true-bool-prop-memb? ast 'inline)))
			(push-set! ast '(dll-export #t)))

                    ;; "pure" is always "virtual".
                    (when (and (has-true-bool-prop-memb? ast 'pure)
                               (not (has-true-bool-prop-memb? ast 'virtual)))
                      (push-set! ast '(virtual #t)))
                    
		    ast)))

	     (cond

	      ((list-named-one-of? ast '(cxx-unit body))
	       (map-elems (fix trans is-e is-c) ast))

	      ((list-named-one-of? ast '(cxx-namespace cxx-class cxx-struct cxx-union))
	       (set! ast (do-flag ast))
	       (map-elems 
		(fix trans
		     (has-true-bool-prop-memb? ast 'export)
		     (has-true-bool-prop-memb? ast 'dll-export))
		ast))

	      ((list-named-one-of? ast '(cxx-typedef cxx-enum global-var class-var inst-var global-func class-meth cxx-ctor cxx-dtor inst-meth))
	       (do-flag ast))

	      (else ast))))))

     (list (trans #f #f ast^) tables)))

 ;; Converts any expressions appearing in a statement position into an
 ;; expression statement.
 (define* (pass-expr-stmt ast^ tables)
   (letrec
       ((trans
	 (lambda (in-stmt-pos ast)
	   (cond
	    
	    ((list-named-one-of? ast '(cxx-unit cxx-namespace cxx-class cxx-struct cxx-union body global-func inst-meth class-meth cxx-ctor cxx-dtor if-stmt while-stmt for-stmt do-while-stmt switch-stmt trap-block block))
	     (map-elems (fix trans #f) ast))
	    
	    ((list-named-one-of? ast '(stmts stmt then-stmt else-stmt))
	     (map-elems (fix trans #t) ast))

	    ((and in-stmt-pos
		  (named-list? ast)
		  (symbol-ends-with? (car ast) "-expr"))
	     `(expr-stmt (expr ,ast)))

	    (else ast)))))

     (list (trans #f ast^) tables)))

 ;; Names elements that do not have a name.
 ;; 
 ;; We must provide some way of calling the ctor and dtor of a class,
 ;; since it seems that it is quite possible to call them in C++. A
 ;; straightforward way to enable this is by giving each ctor and dtor
 ;; a name, although we otherwise would not require one (until
 ;; printing).
 ;; 
 ;; Having a name table entry may also be useful for declaration
 ;; lookups. dtors never have arguments, and all classes have at least
 ;; an implicit one, so it is not really terribly useful to be able to
 ;; look up a dtor declaration. Ctors, on the other hand, can have
 ;; arguments, and we want to be able to look up the relevant ctor for
 ;; each "new" expression; this can be arranged simply by looking up
 ;; the definition of the class being instantiated, and then going
 ;; through all ctors in the class definition. But to make it easy to
 ;; look up ctors and dtor of the containing class from within the
 ;; context of a class, we can still give each ctor and dtor some
 ;; known name.
 ;; 
 ;; The superclass ctor tends to be "called" via the ctor init list
 ;; only, presumably. Ctors of other classes are probably only ever
 ;; called implicitly via "new". Dtors may sometimes be called
 ;; directly, usually followed by a placement new, as follows:
 ;; 
 ;;   T* t = new T;
 ;;   t->~T();
 ;;   new (t) T;
 ;;   delete t;
 ;; 
 ;;   T t;
 ;;   t.~T();
 ;;   new (&t) T;
 ;;
 (define* (pass-name-unnamed ast^ tables)
   (letrec
       ((trans
	 (lambda (ast)
	   (cond
	    ;; Needs a name.
	    ((list-named? ast 'cxx-ctor)
	     (push ast '(name ctor)))

	    ;; Needs a name.
	    ((list-named? ast 'cxx-dtor)
	     (push ast '(name dtor)))

	    ;; Does not require a name, but may contain something to name.
	    ((list-named-one-of? ast '(cxx-unit cxx-namespace cxx-class cxx-struct cxx-union body))
	     (map-elems trans ast))
	    
	    ;; Needs no naming, and contains nothing to name.
	    (else ast)))))

     (list (trans ast^) tables)))

 ;; This pass flags default constructors and copy constructors.
 (define* (pass-ctor-flag ast tables)
   (define (trans decl-id ast)
     (cond
      ((list-named-one-of? ast '(cxx-unit cxx-namespace body))
       (map-elems (fix trans decl-id) ast))

      ((record-type-decl? ast)
       (let ((decl-id (fget-reqd-nlist-elem-1 ast 'decl-id)))
         (map-elems (fix trans decl-id) ast)))

      ((cxx-ctor? ast)
       (begin
         (unless decl-id
           (error "ctor without containing declaration" ast))
         (let ((tp (func-get-func-t ast)))
           (cond
            ;; Default ctor.
            ((let ((arglist (fget-opt-nlist-elem-1up-e tp 'args)))
               (null? arglist))
             (push ast '(default-ctor #t)))

            ;; Copy ctor.
            ((let ((d-tp `(func-t (args
                                   (arg (type
                                         (ref-to (type (const (type
                                                               (tname-ref (decl-id-ref ,decl-id))))))))))))
               (if (texpr-eq? tp d-tp)
                   (push ast '(copy-ctor #t))
                   ast)))

            ;; Other kind of ctor.
            (else ast)))))

      (else ast)))
     
   (list (trans #f ast) tables))
 
 (define* (pass-check-form root-ast tables)
   (define (f ast)
     (cond
      ((func-like-decl? ast)
       (when (get-opt-nlist-elem ast 'body)
	     (error "function has a member list" ast)))
      ((any-pred cxx-unit? cxx-namespace? record-type-decl? ast)
       (if (get-opt-nlist-elem ast 'block)
	   (error "declaration has a code block" ast)
	   (for-each-elem f ast)))
      ((named-list? ast)
       (for-each-elem f ast))))
       
   (f root-ast)
   (list root-ast tables))

) ; end module
