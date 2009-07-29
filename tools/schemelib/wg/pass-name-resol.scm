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
 pass-name-resol
 mzscheme 

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "ast-spec.scm" "wg"))
 (require (lib "tables.scm" "wg"))
 (require (lib "declarations.scm" "wg"))
 (require (lib "mop.scm" "wg"))
 (require (lib "settings.scm" "wg"))
 (require (lib "type-unification.scm" "wg"))
 (require (prefix e. (lib "environment.scm" "wg")))
 (require (prefix h. (lib "hash.scm" "wg")))

 (define (with-id-and-name ast f)
   (let* ((name (get-reqd-nlist-elem-1 ast 'name))
	  (id (get-reqd-nlist-elem-1 ast 'decl-id)))
     (f id name)))

 (define (fully-qualified? sym)
   (symbol-starts-with? sym "::"))
 
 (define (strip-qualifier sym)
   (string->symbol
    (substring
     (symbol->string sym)
     2)))

 ;; This function creates an environment by collecting any definitions
 ;; from the body of the passed node.
 ;; 
 ;; Given that we do not require forward declarations, it is quite
 ;; essential that we have a function that collects all the names from
 ;; parent namespaces before attempting to resolve any names in that
 ;; or deeper levels of the AST.
 (define/kw (env-from-body ast #:key inherit?)
   (letrec ((memb-list (get-opt-nlist-elem-1up-e ast 'body))
	    (env (e.new))
	    (do-memb
	     (lambda (memb-decl)
	       ;;(write-nl (list "memb-decl" memb-decl))
	       (awhen id (get-opt-nlist-elem-1 memb-decl 'decl-id)
		      (let ((name (get-reqd-nlist-elem-1 memb-decl 'name))
			    (is-func (func-like-decl? memb-decl)))
			(unless (and inherit? 
				     (eq? (get-reqd-nlist-elem-1 memb-decl 'access) 'private))
				(set! env (e.add-binding env name id #:is-func is-func))))))))
     (for-each do-memb memb-list)
     ;;(write-nl (list "env finally" env))
     env))

 ;; Like "env-from-body", but actually creates a stack of environments
 ;; so that members in possible bases also get an environment frame.
 ;; 
 ;; You must pass "d-table", and ensure that it is current, with
 ;; declaration IDs for all the declarations (and their members)
 ;; present, as any declarations without an ID naturally cannot be
 ;; added to the environment.
 (define (env-from-body-r d-table ast)
   ;; Gets bases only, not "ast" declaration.
   (define id-list (get-base-refs-r d-table ast))
   ;;(write-nl (list 'ids id-list))
   (alet env (env-from-body ast)
	 (for-each
	  (lambda (id)
	    (define decl (get-decl-by-id d-table id))
	    (define frame (env-from-body decl #:inherit? #t))
	    ;;(write-nl (list "push" env frame))
	    (e.unshift-frame! env frame)
	    ;;(write-nl (list "after push" env))
	    )
	  id-list)
	 ;;(write-nl (list "env-r" env))
	 env))

 ;; In this pass we annotate each name reference with the ID of the
 ;; corresponding definition. We traverse the AST depth first, looking
 ;; for references, and keep all the enclosing environments in mind.
 ;; 
 ;; We tie environments to our traversal simply by pushing/popping an
 ;; env whenever we enter/leave a scope. And in a language like
 ;; Scheme, popping tends to be implicit.
 (define* (pass-resolve-names root-ast tables)
   (define g-table (get-g-table tables))
   (define d-table (get-d-table tables))

   (define td-texpr (make-td-texpr d-table))

   ;; We may need to update "d-table" within this pass to have the
   ;; most current information available at all times. Updates must be
   ;; via this routine, so that "td-texpr" is also updated as
   ;; required.
   (define (update-d-table! decl-ast)
     ;;(write-nl (list "update" decl-ast))
     (awhen decl-id (get-opt-nlist-elem-1 decl-ast 'decl-id)
	    (set! d-table (h.store d-table decl-id decl-ast))
	    (when (cxx-typedef? decl-ast)
		  (set! td-texpr (make-td-texpr d-table))))
     decl-ast)

   (define (get-decl-type decl-id)
     (if decl-id
	 (let* ((decl (get-decl-by-id d-table decl-id))
		(type (fget-reqd-nlist-elem decl 'type)))
	   type)
	 '(type (var))))

   (define (envlist-new)
     (e.new g-table))

   (define (envlist-new-frame envlist)
     (e.new envlist))

   (define envlist-add-frame e.push-frame)

   ;; Remember to specify "is-func" as required.
   (define envlist-add-name e.add-binding)
	
   (define/kw (lookup-name env name #:key want-type sig)
     (when (fully-qualified? name)
	   (set! env g-table)
	   (set! name (strip-qualifier name)))

     (let ((decl-id
	    (aand* result (e.lookup-name-r env name)
		   (if (symbol? result)

		       ;; Not a function name.
		       (cond
			(want-type
			 (ensure-type-decl-id result name))
			(sig
			 (and 
			  (unify sig 
				 (get-decl-type 
				  (ensure-value-decl-id result))) 
			  result))
			(else result))

		       ;; Is a function name.
		       (cond
			(want-type
			 (error-not-type-name name))
			(else
			 (lookup-one-overload env name #:sig sig)))))))
       (when (and (not decl-id) (gop-true? 'show-name-lookup-fail))
	     (write-nl (format "name lookup failure for ~s" name)))
       decl-id))

   (define/kw (lookup-one-overload env name #:key sig)
     (alet olist (e.lookup-overloads-r env name)
	   (when sig
		 (set! olist (filter (fix match-sig? sig) olist)))
	   (set! olist (signatures-uniq olist))
	   (and (= 1 (length olist))
		(first olist))))

   (define (match-sig? sig decl-id)
     (define decl (get-decl-by-id d-table decl-id))
     (define t2 (get-texpr decl))
     (true? (unify sig t2)))

   (define (ensure-value-decl-id decl-id name)
     (if (value-decl-id? decl-id)
	 decl-id
	 (error-not-value-name name)))

   (define (ensure-type-decl-id decl-id name)
     (if (type-decl-id? decl-id)
	 decl-id
	 (error-not-type-name name)))

   (define (value-decl-id? decl-id)
     (value-decl? (get-decl-by-id d-table decl-id)))

   (define (type-decl-id? decl-id)
     (type-decl? (get-decl-by-id d-table decl-id)))

   (define (error-not-value-name name)
     (error-f "name ~s refers to a non-value" name))

   (define (error-not-type-name name)
     (error-f "name ~s refers to a non-type" name))

   ;; Modifies the passed list so that it no longer contains any two
   ;; declaration IDs such that their corresponding declarations have
   ;; the same type.
   ;; id-list:: A list of declaration ID.
   (define (signatures-uniq id-list)
     (map first
	  (delete-duplicates
	   (map list
		id-list
		(map 
		 (compose get-texpr (fix get-decl-by-id d-table))
		 id-list))
	   (lambda (x y)
	     (true? (unify (second x) (second y)))))))

   (define (unify t1 t2)
     ;;(pretty-nl (list "unify" t1 t2))

     ;; xxx coercions and subtyping not yet supported
     (texpr-unify t1 t2 #:td-texpr td-texpr))

   (define trans-type
     (lambda (envlist ast)
       (cond
	;; (ptr-to (type TYPE-EXPR) ...)
	;; (ref-to (type TYPE-EXPR) ...)
	;; (const (type TYPE-EXPR) ...)
	;; (volatile (type TYPE-EXPR) ...)
	;; (arg (type TYPE-EXPR) (name SYMBOL) ...)
	;; (returns (type TYPE-EXPR) ...)
	;; (class (type TYPE-EXPR) ...) 
	;; (class-member (type TYPE-EXPR) ...)
	((list-named-one-of? ast '(ptr-to ref-to const volatile returns class class-member))
	 (trans-type-attr envlist ast))
	
	;; (tvar (type-id SYMBOL) ...)
	((list-named? ast 'tvar)
	 ast)

	;; (tname-ref (name SYMBOL) ...)
	((list-named? ast 'tname-ref)
	 (trans-tname-attr envlist ast))

	;; (array-of (type TYPE-EXPR) (num-elems CONST-EXPR) ...)
	((list-named? ast 'array-of)
	 (begin
	   (set! ast (trans-type-attr envlist ast))
	   (set! ast (trans-opt-expr-attr envlist ast 'num-elems))
	   ast))

	;; (func-t (args ...) (returns ...))
	((list-named? ast 'func-t)
	 (map-elem-lists-named-one-of (fix trans-type envlist) ast '(args returns)))

	;; (args (arg ...)*)
	((list-named? ast 'args)
	 (map-elems (fix trans-type envlist) ast))
	
	((list-named? ast 'arg)
	 (update-d-table! (trans-type-attr envlist ast)))
	
	((list-named? ast 'varargs)
	 ast)

	;; (ptr-to-member (class ...) (class-member ...) ...)
	((list-named? ast 'ptr-to-member)
	 (map-elem-lists-named-one-of (fix trans-type envlist) ast '(class class-member)))
	
	(else (error "unrecognized type subexpression" ast)))))

   ;; handles:
   ;; (ctor-super (type TYPE-EXPR) (exprs CTOR-ARG-EXPR ...) ...)
   ;; (ctor-var (name VAR-NAME) (exprs CTOR-ARG-EXPR ...) ...)
   (define (trans-ctor-init-list envlist ast)
     (map-elems
      (lambda (ast)
	(cond
	 ((list-named? ast 'ctor-super)
          ;; xxx is there actually the option of calling the ctor of
          ;; some other than the most immediate ancestor -- if not,
          ;; why the "type" attr here?
          ;; 
          ;; xxx env -- must consider which of the possibly multiple
          ;; overloaded constructors is being called here
	  (begin
	    (set! ast (trans-type-attr envlist ast))
	    (set! ast (trans-exprs-attr envlist ast))
	    ast))
	 ((list-named? ast 'ctor-var)
          ;; Here the variable is not declared, just assigned to, so
          ;; its type should be known elsewhere. Since it does not
          ;; appear in any interesting context here, we may not bother
          ;; resolving its type. But if we wanted to support inferring
          ;; the type of the variable based on its initializer, then
          ;; this would be the place to do that.
	  (begin
	    (set! ast (trans-name-attr envlist ast))
	    (set! ast (trans-exprs-attr envlist ast))
	    ast))
	 (else 
	  (error "unknown constructor initializer" ast))))
      ast))

   ;; (dot-expr (expr1 EXPR) (expr2 (name-ref (name SYMBOL) ...)) ...)
   ;; 
   ;; (arrow-expr (expr1 EXPR) (expr2 (name-ref (name SYMBOL) ...)) ...)
   (define (trans-field-ref-expr envlist ast indirect? sig)
     (define (type-of-expr ast attr-name)
       (get-texpr-or-tvar (get-expr ast attr-name)))

     ;; Takes a type expression, and if that expression names a record
     ;; type, then returns the declaration of that record type.
     ;; Otherwise returns #f.
     (define (get-record-decl texpr)
       (and (tname-ref? texpr)
	    (aand* decl-id (get-decl-id-ref texpr)
		   (alet decl (get-decl-by-id d-table decl-id)
			 (cond
			  ((record-type-decl? decl)
			   decl)
			  ((cxx-typedef? decl)
			   (get-record-decl
			    (fget-reqd-nlist-elem-1 decl 'type)))
			  (else #f))))))

     ;; Resolves expression "expr" in the context of an environment
     ;; consisting of the members of the record type (and its
     ;; ancestors) "rdecl". Returns the resolved expression.
     (define (resolve-in-context rdecl expr)
       (define renv (env-from-body-r d-table rdecl))
       (trans-expr renv expr #:sig sig))

     ;; We must first resolve the receiver type.
     (set! ast (trans-expr-attr envlist ast 'expr1))

     ;; The receiver type should be a pointer if the field
     ;; reference is indirect. Otherwise it should be a record
     ;; type. If it is a pointer reference or record reference,
     ;; that is okay.
     (let* ((type1 (type-of-expr ast 'expr1))
	    (rtype (strip-any-ref-to type1)))
       (when indirect?
	     (set! rtype (and (ptr-to? rtype) (get-texpr rtype))))
       (aif rdecl (and rtype (get-record-decl rtype))
	    (let* ((expr2 (get-expr ast 'expr2)))
	      (set! expr2 (resolve-in-context rdecl expr2))
	      (set-nlist-elem-1! ast 'expr2 expr2)
	      (alet type2 (get-texpr-or-tvar expr2)
		    (set-nlist-elem-1! ast 'type type2)))
	    (set-nlist-elem-1! ast 'type (tvar))))

     ast) ; end trans-field-ref-expr

   (define (get-func-t-return-type texpr)
     (aand* returns (fget-opt-nlist-elem texpr 'returns)
	    (get-texpr-or-tvar returns)))

   (define (trans-call-expr envlist ast)
     ;; Resolve arguments.
     (set! ast (trans-exprs-attr envlist ast))
     
     ;; Resolve call target.
     ;; 
     ;; Note that we cannot resolve function reference expressions to
     ;; any particular function declaration, since the function the
     ;; expression refers to (if any) is not known until runtime. We
     ;; can try to resolve any names appearing in the expression,
     ;; however, as well as the type of the function reference.
     ;; 
     ;; xxx we presently do not correctly handle situations where a
     ;; function expects a ref-to argument
     ;; 
     ;; xxx Could consider having additional context information
     ;; passed from enclosing context, namely the expected type of an
     ;; expression. We could then use any such expected context info
     ;; as the return value type here. A more sophisticated approach
     ;; (but a less efficient one) would be to have an "alt-t" type
     ;; for specifying multiple alternatives for the type of this
     ;; expression, and then consider those during unification in the
     ;; containing expression/statement/declaration. For now, we just
     ;; use "tvar" as the return value type.
     (let* ((arglist (fget-opt-nlist-elem-1up-e ast 'exprs))
	    (argtypes (map get-texpr-or-tvar arglist))
	    (aa (map (compose arg type) argtypes))
	    (sig `(func-t (args ,@aa) (returns (type (tvar))))))
       (set! ast (trans-expr-attr envlist ast 'target #:sig sig)))

     ;; Resolve the type of this expression.
     (let* ((target-expr (get-expr ast 'target))
	    (target-type (get-texpr-or-tvar target-expr))
	    (stripped (strip-any-ref-to target-type))
	    (this-type 
	     (or
	      (and (func-t? stripped)
		   (get-func-t-return-type stripped))
	      (tvar))))
       (set-nlist-elem-1! ast 'type this-type))

     ;; If the "target" expression names a function, and the name was
     ;; resolved, add the declaration ID to this expression.
     (aand* nref (expr-get-name-ref (get-expr ast 'target))
	    decl-id (get-decl-id-ref nref)
	    (set-nlist-elem-1! ast 'decl-id-ref decl-id))

     ast)

   (define (has-attr? attr-name ast)
     (true? (fget-opt-nlist-elem ast attr-name)))

   (define (has-type? ast)
     (has-attr? 'type ast))

   (define (set-texpr ast texpr)
     (set-nlist-elem-1 ast 'type texpr))

   (define (get-decl-id-ref ast)
     (fget-opt-nlist-elem-1 ast 'decl-id-ref))

   ;; Resolves a type name to its declaration. A type name cannot
   ;; refer to a function, since a function declaration does not
   ;; declare a type; what it declares is more like a value, since one
   ;; can at least takes its address, and refer to it. So overloading
   ;; is not to be considered here.
   (define (trans-tname-attr envlist ast)
     (let* ((name (get-reqd-nlist-elem-1 ast 'name))
	    (id (lookup-name envlist name #:want-type #t)))
       (if id
	   (push ast (list 'decl-id-ref id))
	   ast)))

   ;; Resolves a name to its declaration. The resolution is not done
   ;; if: the name does not appear in the environment "envlist" at
   ;; all; or, the name resolves to a function with overloads, in
   ;; which case the name cannot be resolved to a particular overload
   ;; without context information, which may optionally be passed.
   (define/kw (trans-name-attr envlist ast #:key sig)
     (let* ((name (get-reqd-nlist-elem-1 ast 'name))
	    (id (lookup-name envlist name #:sig sig)))
       (when id
	     (push-set! ast (list 'decl-id-ref id))))
     ast)

   ;; sig:: A type expression. May optionally be passed to specify a
   ;;       full or partial type signature that should be matched by
   ;;       the type of the declaration associated with any name that
   ;;       is to be resolved in the context for the surrounding
   ;;       expression. Expression types to whose resolution this
   ;;       might apply include at least "name-ref", "dot-expr", and
   ;;       "arrow-expr", as these cases may include a name associated
   ;;       with the immediately surrounding expression context.
   (define/kw (trans-expr envlist ast #:key sig)
     (cond
      ((name-ref? ast)
       (begin
	 (set! ast (trans-name-attr envlist ast #:sig sig))
	 (unless (has-type? ast)
		 (let* ((decl-id (fget-opt-nlist-elem-1 ast 'decl-id-ref))
			(type (get-decl-type decl-id)))
		   (set-nlist-elem! ast type)))
	 ast))

      ((literal? ast)
       ;; One of the few expressions that should have a type to begin
       ;; with. Resolve that type, if it is there. Some literals, such
       ;; as list/structure literals (if they can be called literals)
       ;; may not have a type to begin with. We could try to resolve
       ;; the type for those, based on the types of their elements.
       ;; xxx
       (if (has-type? ast)
	   (trans-type-attr envlist ast)
	   (set-nlist-elem-1 ast 'type (tvar))))

      ((sizeof-expr? ast)
       (begin
         ;; Must already have a type. I wonder if it would be better
         ;; to set the type here, rather than require the type to be
         ;; provided. This way is more flexible, but more work for the
         ;; frontend(s).
	 (set! ast (trans-type-attr envlist ast))
	 (set! ast (trans-type-attr envlist ast 'type-expr))
	 ast))

      ((texpr-expr? ast)
       (begin
	 (set! ast (trans-type-attr envlist ast 'type-expr))
         ;; An expression of this type cannot have any type at all,
         ;; since it is not even legal in C++; possibly we could have
         ;; macros declare arguments of this sort as "tvar".
	 (set-nlist-elem-1 ast 'type (tvar))
	 ast))

      ((any-pred dot-expr? arrow-expr? ast)
       (trans-field-ref-expr envlist ast (arrow-expr? ast) sig))
	     
      ((call-expr? ast)
       (trans-call-expr envlist ast))

      ((ctor-call-expr? ast)
       (begin
         ;; xxx Might want to add a decl-id-ref pointing to the called
         ;; constructor.
	 (set! ast (trans-exprs-attr envlist ast))
	 (set! ast (trans-type-attr envlist ast))
	 ast))

      ((new-expr? ast)
       (begin
	 (set! ast (trans-type-attr envlist ast 'type-expr))
	 (set! ast (trans-exprs-attr envlist ast))
	 (unless (has-type? ast)
		 (let* ((te (fget-reqd-nlist-elem-1 ast 'type-expr))
			(nt (ptr-to (type te))))
		   (set-nlist-elem-1! ast 'type nt)))
	 ast))

      ((new-array-expr? ast)
       ;; xxx is this even an expression in C++; does it return any
       ;; value?
       (error "unsupported" ast))

      ((any-pred delete-expr? delete-array-expr? ast)
       ;; The return values of these are uninteresting, especially
       ;; since I have no idea what they return. So we care not about
       ;; the type.
       (begin
	 (set! ast (trans-expr-attr envlist ast))
	 (set-nlist-elem-1 ast 'type (tvar))
	 ast))

      ((addr-of-expr? ast)
       (begin
	 (set! ast (trans-expr-attr envlist ast))
	 (let* ((expr (get-expr ast))
		(e-type (get-texpr-or-tvar expr)))
	   (set-nlist-elem-1 ast 'type (ptr-to (type e-type))))))

      ((deref-expr? ast)
       (begin
	 (set! ast (trans-expr-attr envlist ast))
	 (let* ((expr (get-expr ast))
		(e-type (get-texpr-or-tvar expr))
                ;; This might not be okay if our "tvar" appeared
                ;; elsewhere.
		(a-type 
		 (cond
		  ((tvar? e-type) (tvar))
		  ((ptr-to? e-type) (get-texpr e-type))
		  (else (error "cannot dereference non-pointer" ast)))))
	   (set-nlist-elem-1 ast 'type a-type))))

      ((index-expr? ast)
       ;; (index-expr (expr EXPR) (index INDEX-EXPR) ...)
       (begin
	 (set! ast (trans-expr-attr envlist ast))
	 (set! ast (trans-expr-attr envlist ast 'index))

         ;; If the type of "expr" has been resolved as an array type,
         ;; or a reference to one, we can resolve a type for this
         ;; expression.
	 (let* ((expr (get-expr ast))
		(a-type (strip-any-ref-to (get-texpr-or-tvar expr)))
		(this-type
		 (or
		  (and (array-of? a-type)
		       (get-texpr a-type))
		  (tvar))))
	   (set-nlist-elem-1! ast 'type this-type))

	 ast))

      ((unary-op-expr? ast)
       ;; Here we implement default resolution of sorts for unary
       ;; operations. This sort of it the behavior one would generally
       ;; expect, but perhaps operators could be redefined to behave
       ;; differently. xxx look into it once we support cxx-oper
       (begin
	 (set! ast (trans-expr-attr envlist ast))
	 (let* ((expr (get-expr ast))
		(e-type (get-texpr-or-tvar expr)))
	   (set-nlist-elem-1 ast 'type e-type))))

      ((binary-op-expr? ast)
       (begin
	 (set! ast (trans-expr-attr envlist ast 'expr1))
	 (set! ast (trans-expr-attr envlist ast 'expr2))
         ;; The type of this expression should be unifiable both with
         ;; then-expr and else-expr.
	 ;;xxx to implement
	 (set-nlist-elem-1 ast 'type (tvar))))

      ((multi-op-expr? ast)
       (begin
	 (set! ast (trans-exprs-attr envlist ast))
         ;; The type of this expression should be unifiable with the
         ;; types of all the operands. And actually this AST node
         ;; maybe should have already been converted to a tree of
         ;; binary expressions according to precedence rules; we do
         ;; not necessarily want to deal with such high-level stuff
         ;; here. But long as we just want the type, and not the
         ;; value, we may not want to concern ourselves with
         ;; precedence anywhere in this compiler.
         ;; 
         ;; xxx to implement
	 (set-nlist-elem-1 ast 'type (tvar))))

      ((if-expr? ast)
       (begin
	 (set! ast (trans-expr-attr envlist ast 'cond-expr))
	 (set! ast (trans-expr-attr envlist ast 'then-expr))
	 (set! ast (trans-expr-attr envlist ast 'else-expr))
         ;; The type of this expression should be unifiable both with
         ;; then-expr and else-expr.
	 ;;xxx to implement
	 (set-nlist-elem-1 ast 'type (tvar))))

      ((cast-expr? ast)
       ;; A cast must already have a "type", and that type could at
       ;; least in some cases be different from any type resolved for
       ;; the expression, so not much we can or should do here.
       (trans-expr-attr envlist ast))

      ((lift-stmt-expr? ast)
       (error "should have been lifted already" ast))

      ((expression? ast)
       (error "unknown expression" ast))

      (else
       (error "expected expression" ast))))

   (define/kw (trans-attr-value f attr-name ast #:key reqd)
     (aif attr-elem ((if reqd 
			 fget-reqd-nlist-elem
			 fget-opt-nlist-elem)
		     ast
		     attr-name)
	  (alet new-elem (modify-cadr attr-elem f)
		(set-nlist-elem ast new-elem))
	  ast))

   (define (trans-local-var envlist ast)
     (update-d-table! (trans-var envlist ast)))

   (define (trans-var envlist ast)
     ;;(pretty-nl ast)

     ;; First ensure has a type with resolved names.
     (if (has-type? ast)
	 (set! ast (trans-type-attr envlist ast))
	 (set-nlist-elem! ast '(type (tvar))))

     ;; If has an initializer expression, resolve it, and then try to
     ;; unify its type with that of the variable.
     (when (has-attr? 'init ast)
	   (set! ast (trans-attr-value
		      (fix trans-expr envlist)
		      'init ast #:reqd #t))
	   (let* ((i-ast (fget-reqd-nlist-elem-1 ast 'init))
		  (i-type (get-texpr-or-tvar i-ast))
		  (v-type (get-texpr ast))
		  (u-res (unify v-type i-type)))
	     (when u-res
		   (set! ast (set-texpr ast (first u-res)))
		   (set! i-ast (set-texpr i-ast (second u-res)))
		   (set-nlist-elem-1! ast 'init i-ast))))

     ast)

   ;; Items processed here may introduce changes to the
   ;; environment, but only to their own or their children's
   ;; environment, and not to their siblings' or parents'. Thus
   ;; this function returns just the possibly modified AST, but
   ;; not any environment information.
   (define (trans-stmt envlist ast)
     (cond
      ((block? ast)
       (trans-block envlist ast))

      ((expr-stmt? ast)
       (trans-expr-attr envlist ast))

      ((trap-block? ast)
       (begin
	 (set! ast (trans-expr-attr envlist ast))
	 (set! ast (trans-stmt-attr envlist ast))
	 ;;(write-nl ast)
	 ast))

      ((return-stmt? ast)
       ;; xxx We could try to unify the returned expression against
       ;; the function return value type. And match "void" to no
       ;; return value.
       (trans-opt-expr-attr envlist ast))

      ((if-stmt? ast)
       (begin
	 (set! ast (trans-expr-attr envlist ast 'cond-expr))
	 (set! ast (trans-stmt-attr envlist ast 'then-stmt))
	 (set! ast (trans-opt-stmt-attr envlist ast 'else-stmt))
	 ast))

      ((any-pred while-stmt? do-while-stmt? ast)
       (begin
	 (set! ast (trans-expr-attr envlist ast 'cond-expr))
	 (set! ast (trans-stmt-attr envlist ast))
	 ast))

      ((for-stmt? ast)
       (begin
	 (set! ast (trans-opt-expr-attr envlist ast 'init-expr))
	 (set! ast (trans-opt-expr-attr envlist ast 'cond-expr))
	 (set! ast (trans-opt-expr-attr envlist ast 'step-expr))
	 (set! ast (trans-stmt-attr envlist ast))
	 ast))

      ((switch-stmt? ast)
       (begin
	 ;;(pretty-nl ast)
	 (set! ast (trans-expr-attr envlist ast 'switch-expr))
	 (set! ast (trans-stmts-attr envlist ast))
	 ast))
      ((switch-case? ast)
       (trans-expr-attr envlist ast 'switch-expr))
      ((switch-default? ast)
       ast)

      ((any-pred goto-stmt? break-stmt? continue-stmt? ast)
       ast)

      ((cxx-chunk? ast)
       ast)

      ((statement? ast)
       (error "unknown statement" ast))

      (else
       (error "expected statement" ast))))

   (define/kw (trans-type-attr envlist ast #:optional (name 'type))
     (alet expr (fget-reqd-nlist-elem-1 ast name)
	   (set-nlist-elem-1 ast name (trans-type envlist expr))))

   (define/kw (trans-expr-attr envlist ast 
			       #:optional (name 'expr)
			       #:key sig)
     (alet expr (fget-reqd-nlist-elem-1 ast name)
	   (set-nlist-elem-1 ast name 
			     (trans-expr envlist expr #:sig sig))))

   (define/kw (trans-opt-expr-attr envlist ast #:optional (name 'expr))
     (aif expr (fget-opt-nlist-elem-1 ast name)
	   (set-nlist-elem-1 ast name (trans-expr envlist expr))
	   ast))

   (define/kw (trans-stmt-attr envlist ast #:optional (name 'stmt))
     (alet stmt (fget-reqd-nlist-elem-1 ast name)
	   (set-nlist-elem-1 ast name (trans-stmt envlist stmt))))

   (define/kw (trans-opt-stmt-attr envlist ast #:optional (name 'stmt))
     (aif stmt (fget-opt-nlist-elem-1 ast name)
	  (set-nlist-elem-1 ast name (trans-stmt envlist stmt))
	  ast))

   (define (trans-exprs-attr envlist ast)
     (alet exprs (fget-reqd-nlist-elem ast 'exprs)
	   (set! exprs (map-elems (fix trans-expr envlist) exprs))
	   (set-nlist-elem ast exprs)))

   ;; Transforms a statement sequence, as stored in the "stmts"
   ;; attribute of "ast". Certain statements may introduce new
   ;; bindings which are made visible to the statements that follow
   ;; them in the sequence.
   (define (trans-stmts-attr envlist ast)
     (let ((newenvlist (envlist-new-frame envlist)))
       (map-stmts-values
	(lambda (item)
	  (let ((r (trans-stmt-with-env newenvlist item)))
	    (set! newenvlist (first r))
	    (second r)))
	ast)))

   ;; Each block, whether a function or method body, or an
   ;; anonymous block, has its own environment, which will be
   ;; filled in sequentially, i.e. not all at once. Thus
   ;; names in blocks can only be referred to once they have
   ;; been declared. Any names introduced within a block are
   ;; not visible outside the block.
   (define trans-block trans-stmts-attr)

   ;; There are certain kind of "statements" that may affect the local
   ;; environment, in a manner that is visible to subsequently
   ;; processed siblings. Hence this function returns the possibly
   ;; affected environment, as well as the transformed node.
   (define (trans-stmt-with-env envlist ast)
     (set! 
      ast
      (cond

       ;; local-var
       ((list-named? ast 'local-var)
	(begin
	  (with-id-and-name
	   ast
	   (lambda (id name)
	     (set! envlist (envlist-add-name envlist name id))))
	  (trans-local-var envlist ast)))
       
       (else (trans-stmt envlist ast))))
       
     (list envlist ast))

   ;; Maps the individual elements in any "stmts" attribute of the
   ;; passed node.
   (define (map-stmts-values f ast)
     (map-elem-lists-named
      (fix map-elems f)
      ast 'stmts))

   (define (trans-body envlist ast)
     (map-elems (fix trans-decl envlist) ast))

   (define (trans-body-attr envlist ast)
     (map-elem-lists-named 
      (fix trans-body envlist)
      ast
      'body))

   ;; Resolves any type referred to in a "base" element. We are
   ;; looking at something like:
   ;;   (base (type (tname-ref (name ,name))) (base-access ,kind)))
   (define (trans-base envlist ast)
     (trans-type-attr envlist ast))
	   
   (define (trans-bases envlist ast)
     (map-elem-lists-named
      (fix trans-base envlist)
      ast
      'base))

   (define (trans-decl envlist ast)
     (update-d-table! (aux-trans-decl envlist ast)))

   (define (aux-trans-decl envlist ast)
     (cond
      ;; unit
      ;; 
      ;; Any names appearing at top level are already in the
      ;; global namespace, so it is not necessary to collect
      ;; them into a local environment. Hence we will not do
      ;; "env-from-body" here.
      ((cxx-unit? ast)
       (trans-body-attr envlist ast))

      ((cxx-namespace? ast)
       (let ((newenvlist 
	      (envlist-add-frame envlist (env-from-body ast))))
	 (trans-body-attr newenvlist ast)))

      ((cxx-typedef? ast)
       (trans-type-attr envlist ast))

      ((any-pred global-var? class-var? inst-var? ast)
       (trans-var envlist ast))

      ((record-type-decl? ast)
       (begin
	 ;; Resolves base class names. Note that we do not want
	 ;; to resolve these in the context of this class, as we
	 ;; do not want to end up inheriting from an inner class
	 ;; of this class, for example.
	 ;; 
	 ;; XXX If we wanted to account for subtyping during
	 ;; overload resolution or type unification, we would
	 ;; need to resolve bases earlier, and already have the
	 ;; class hierarchy information available at the start
	 ;; of this phase. This could be done in a separate,
	 ;; earlier phase, but we would want to refactor the
	 ;; environment management code so as to allow it to be
	 ;; reused in that phase.
	 (set! ast 
	       (map-elem-lists-named 
		(fix trans-bases envlist)
		ast
		'bases))

	 ;; Resolves names appearing within class body.
	 ;; 
	 ;; Accounts for inherited members. Note that bases must
	 ;; be resolved before this, lest there will be no known
	 ;; bases to inherit from.
	 (with-id-and-name
	  ast
	  (lambda (class-id class-name)
	    (let* ((new-env (env-from-body-r d-table ast))
		   (this-id (get-reqd-nlist-elem-1 ast 'this-id)))
	      (e.add-binding! new-env 'this this-id)
	      (e.push-stack! envlist new-env)
	      (trans-body-attr new-env ast))))))

      ((cxx-oper? ast)
       ast) ;; xxx to be supported

      ;; function, method
      ;; 
      ;; Functions and methods introduce a new environment
      ;; containing the names appearing in their argument list.
      ((func-like-decl? ast)
       (let* ((block (get-opt-nlist-elem ast 'block)))

	 ;; Resolve type.
	 (set! ast (trans-type-attr envlist ast))

	 ;; Resolve names in any ctor initializer.
	 (when (cxx-ctor? ast)
	       (set! ast
		     (map-elem-lists-named
		      (fix trans-ctor-init-list envlist)
		      ast
		      'ctor-init-list)))

	 ;; Resolve any function body in an environment that
	 ;; also includes any named arguments.
	 (when block
	       (let* ((type (get-reqd-nlist-elem ast 'type))
		      (func (get-reqd-nlist-elem type 'func-t))
		      (args (get-reqd-nlist-elem func 'args))
		      (newenvlist (envlist-new-frame envlist)))
		 (for-each
		  (lambda (argelem)
		    (when (and (list-named? argelem 'arg)
			       (get-opt-nlist-elem argelem 'name))
			  (with-id-and-name
			   argelem
			   (lambda (id name)
			     (set! newenvlist (envlist-add-name newenvlist name id))))))
		  (cdr args))
		 (set-nlist-elem! ast (trans-block newenvlist block))))

	 ast))

      ((cxx-chunk? ast)
       ;; Nothing we can resolve here.
       ast)

      ((cxx-enum? ast)
       ;; To be supported if there is anything to resolve in "enum"
       ;; definitions. Could the assigned values be names of constants
       ;; instead of integer literals?
       ;;
       ;; xxx enum values could be given the int type already when constructing AST nodes
       ;; xxx are we even collecting them to a name table at present?
       ast)

      ((declaration? ast)
       ;; xxx makes it hard to add declaration types
       (error "unknown declaration" ast))

      (else
       (error "expected declaration" ast))))

   (define (trans-any envlist ast)
     (cond
      ((type-expression? ast)
       (trans-type envlist ast))
      ((expression? ast)
       (trans-expr envlist ast))
      ((statement? ast)
       (trans-stmt envlist ast))
      ((declaration? ast)
       (trans-decl envlist ast))
      ((ast-node? ast)
       (error "unknown AST node" ast))
      (else
       (error "expected AST node" ast))))

   ;;(pretty-nl root-ast)
   (set! root-ast (trans-any (envlist-new) root-ast))
   (list root-ast tables))

) ;; end module
