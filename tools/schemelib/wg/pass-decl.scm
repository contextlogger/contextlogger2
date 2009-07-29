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

;; A pass during which a table of all the implicit definitions, and
;; the declarations in the unit is built.

(module 
 pass-decl
 mzscheme 

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-spec.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "prop-util.scm" "wg"))
 (require (lib "settings.scm" "wg"))
 (require (lib "tables.scm" "wg"))
 (require (lib "declarations.scm" "wg"))
 (require (lib "mop.scm" "wg"))
 (require (lib "phases.scm" "wg"))
 (require (prefix h. (lib "hash.scm" "wg")))
 (require (prefix e. (lib "environment.scm" "wg")))

 (define (trans-global-decl trans ast)
   ;;(write-nl ast)
   (cond
    ((global-decl-and-cont? ast)
     ;; The order here is questionable: Whether to transform children
     ;; before parent or the other way around.
     (map-elems (fix trans-global-decl trans) (trans ast)))
    ((global-decl-cont? ast)
     (map-elems (fix trans-global-decl trans) ast))
    ((global-decl? ast)
     (trans ast))
    (else ast)))

 ;; Fundamental types.
 ;; 
 ;; These are the set of fundamental types from which all other C++
 ;; data types can be composed. The members of this set are considered
 ;; semantically unequivalent with each other, and thus form the
 ;; smallest possible set. There may be other types that are
 ;; equivalent to these, but by definition, cannot be included in this
 ;; set.
 ;; 
 ;; For integers and doubles, we use canonical names similar to what
 ;; GCC uses.
 (define fundamental-list
   (list
    '(cxx-fund-type (name void))
    '(cxx-fund-type (name bool))
    '(cxx-fund-type (name wchar_t)) ;; wchar_t
    '(cxx-fund-type (name |signed char|))
    '(cxx-fund-type (name |unsigned char|))
    '(cxx-fund-type (name |short signed int|))
    '(cxx-fund-type (name |short unsigned int|))
    '(cxx-fund-type (name |signed int|))
    '(cxx-fund-type (name |unsigned int|))
    '(cxx-fund-type (name |long signed int|))
    '(cxx-fund-type (name |long unsigned int|))
    '(cxx-fund-type (name |long long signed int|))
    '(cxx-fund-type (name |long long unsigned int|))
    '(cxx-fund-type (name float))
    '(cxx-fund-type (name double))
    '(cxx-fund-type (name |long double|))
    ))

 ;; Non-fundamental basic data types.
 ;; 
 ;; We define these as typedefs to fundamental types. An alternative
 ;; would be to simple define these as syntax that actually produces a
 ;; fundamental type, but since readability of the output code is
 ;; important to us, we want to avoid losing semantics wherever
 ;; possible.
 (define implicit-typedef-list
   (let ((alias
	  (lambda (s t)
	    `(cxx-typedef (name ,s) 
			  (type (tname-ref (name ,t))))))
	 (alias-gop
	  (lambda (s t)
	    `(cxx-typedef (name ,s) 
			  (type (tname-ref (name ,(gop-value s t))))))))
     (list
      ;; Standard basic types.

      ;; * char is either signed-char or unsigned-char
      (alias-gop 'char '|signed char|)
      ;; * short is signed-short
      (alias '|short int| '|short signed int|)
      ;; * int is signed-int
      (alias 'int '|signed int|)
      ;; * long is signed-long
      (alias '|long int| '|long signed int|)
      ;; * long-long is signed-long-long
      (alias '|long long int| '|long long signed int|)
      ;; * size-t is unsigned-int
      (alias 'size_t '|unsigned int|)

      ;; Universal custom basic types.

      ;; * nothing is void
      (alias 'nothing 'void)
      ;; * int8 is signed-char
      (alias 'int8 '|signed char|)
      ;; * uint8 is unsigned-char
      (alias 'uint8 '|unsigned char|)
      ;; * byte is unsigned-char
      (alias 'byte '|unsigned char|)
      ;; * nchar is unsigned-char (narrow character, cf. wchar)
      (alias 'nchar '|unsigned char|)

      ;; Platform-specific custom basic types.

      ;; * int16 is e.g. signed-short
      (alias-gop 'int16 '|short signed int|)
      ;; * int32 is e.g. signed-int
      (alias-gop 'int32 '|signed int|)
      ;; * int64 is e.g. signed-long-long
      (alias-gop 'int64 '|long long signed int|)
      ;; * uint16 is e.g. unsigned-short
      (alias-gop 'uint16 '|short unsigned int|)
      ;; * uint32 is e.g. unsigned-int
      (alias-gop 'uint32 '|unsigned int|)
      ;; * uint64 is e.g. unsigned-long-long
      (alias-gop 'uint64 '|long long unsigned int|)
      ;; * float32 is e.g. float
      (alias-gop 'float32 'float)
      ;; * float64 is e.g. double
      (alias-gop 'float64 'double)
      ;; * float128 is e.g. long-double
      (alias-gop 'float128 '|long double|)
      )))
   
 ;; Built-in global variables and constants and functions.
 (define built-in-global-list
   (list
    `(cxx-var (name NULL) (type (const (type (tname-ref (name int))))))
    ))

 ;; This function adds all the implicitly defined fundamental types
 ;; and typedefs into the provided d-table (definition table) and
 ;; g-table (global name table) data structures.
 (define* (pass-add-implicits ast^ tables)
   (define d-table (get-d-table tables))
   (define g-table (get-g-table tables))

   (let* ((add-id
	   (lambda (value)
	     (let ((r (tables-add-id-and-name d-table g-table value)))
	       (set! d-table (first r))
	       (set! g-table (second r))
	       (third r))))

	  (do-type
	   (lambda (entry)
             ;; Resolves tname-refs in types. We know these must
             ;; resolve to fundamentals, so are making sure that they
             ;; do by adding an explicit reference now; we do not want
             ;; these resolving to locals, ever.
	     (map-memb-lists-named 
	      (lambda (type-elem)
		(map-memb-lists-named-r
		 (lambda (name-ref)
		   (let* ((name (get-reqd-nlist-memb-1 name-ref 'name))
			  (g-id (e.lookup g-table name #:reqd #t)))
		     (push name-ref
			   `(decl-id-ref ,g-id))))
		 type-elem
		 'tname-ref))
	      entry
	      'type)))
	  (do-entry
	   (lambda (entry) 
	     (add-id (do-type entry)))))
     (for-each add-id fundamental-list)
     (for-each do-entry implicit-typedef-list)
     (for-each do-entry built-in-global-list)
     (list ast^ (set-d-g-table tables d-table g-table))))

 ;; Returns a table of declarations, and a modified AST with
 ;; declaration IDs added.
 ;; 
 ;; A "decl-id" attribute is _not_ added to any declaration that
 ;; already has it, as this allows the programmer to refer to
 ;; declarations by ID in some situations. Any existing "decl-id" is
 ;; used as is.
 (define* (pass-add-decl-id root-ast tables)
   (define d-table (get-d-table tables))

   (letrec
       ((add-id
	 (lambda (value)
	   (let ((r (d-table-add-id d-table value)))
	     (set! d-table (first r))
	     (second r))))

        ;; Can be used to define the name "this" for a record type, to
        ;; allow for the type of a "this" reference to be resolved in
        ;; the context of that record.
	(add-this-decl
	 (lambda (ast)
	   (let* ((class-id (get-reqd-nlist-elem-1 ast 'decl-id))
		  (class-name (get-reqd-nlist-elem-1 ast 'name))
		  (this-id (d-table-next-id d-table 'this))
		  (this-def 
		   `(local-var 
		     (name this) 
		     (type (ptr-to (type (tname-ref (name ,class-name) 
						    (decl-id ,class-id)))))
		     (decl-id ,this-id))))
	     (set! d-table (h.store d-table this-id this-def))
	     (push ast `(this-id ,this-id)))))

	;; Transforms the content of a function with a body.
	;;
	;; The not so obvious case here is that of (<function> (type
	;; (func-t (args (arg (decl-id ID) ...) ...) ...)) ...).
	;;
	;; Note that an "arg" without a name requires no ID.
	(trans-func
	 (lambda (ast)
	   (cond

	    ((list-named-one-of? ast '(type func-t args block if-stmt while-stmt do-while-stmt for-stmt switch-stmt then-stmt else-stmt trap-block stmt stmts))
	     (map-elems trans-func ast))

	    ((list-named? ast 'local-var)
	     (add-id ast))

	    ((and (list-named? ast 'arg)
		  (get-opt-nlist-elem ast 'name)
		  (not (has-true-bool-prop-memb? ast 'unused)))
	     (add-id ast))

	    (else ast))))
	
	(trans
	 (lambda (ast)
	   (cond

            ;; Not declarations, but may contain some.
            ;; 
            ;; No, we do not consider a namespace to be a declaration
            ;; in itself, we just regard a namespace as something that
            ;; affects the qualified name of anything within it.
	    ((list-named-one-of? ast '(cxx-unit cxx-namespace body))
	     (map-elems trans ast))
	    
	    ;; Declarations by themselves, and may contain some more.
	    ((list-named-one-of? ast '(cxx-class cxx-struct cxx-union))
	     (begin 
	       (set! ast (add-id ast))
	       (set! ast (add-this-decl ast))
	       (map-elems trans ast)))

            ;; Functions. Declarations by themselves, and may contain
            ;; some more.
	    ((func-like-decl? ast)
	     (begin
	       (set! ast (add-id ast))
	       
	       (when (get-opt-nlist-elem ast 'block)
                     ;; It only makes sense to assign declaration IDs
                     ;; to arguments if the function has a body, and
                     ;; otherwise only the body may contain any
                     ;; declarations.
		     ;;(write-nl (list "name" (get-decl-name ast)))
		     (set! ast (map-elems trans-func ast)))
	       ast))

	    ;; Declarations, but do not contain other declarations.
	    ((list-named-one-of? ast '(cxx-typedef global-var class-var inst-var))
	     (add-id ast))

            ;; Anything else is neither a declaration, nor contains
            ;; any.
	    (else ast)))))

     (set! root-ast (trans root-ast))
     ;;(pretty-nl root-ast)
     (list root-ast (set-d-table tables d-table))))

 ;; This updates the declaration table based on the IDs appearing in
 ;; the AST. No old keys are removed from the table.
 ;; 
 ;; (Mainly the idea is to just update the values for existing keys,
 ;; to avoid having to do declaration lookups from the AST, which
 ;; would be slower. We generally cannot merely store references to
 ;; AST components as values, since we are not programming
 ;; imperatively here; we are not modifying AST nodes, we are
 ;; replacing them.)
 (define* (pass-update-decl-table ast^ tables)
   (define d-table (get-d-table tables))

   (letrec
       ((trav
	 (lambda (ast)
	   (if (named-list? ast)
	       (begin
		 ;(write-nl (list 'trav (car ast)))
		 (let ((id 
			(get-opt-nlist-elem-1 ast 'decl-id)))
		   (if id
		       (begin
			 ;(write-nl (list 'found id))
			 (set! d-table (h.store d-table id ast)))))
		 (for-each-elem
		  trav
		  ast))))))
     (trav ast^)
     (list ast^ (set-d-table tables d-table))))

 (phase-add 'd-table-update pass-update-decl-table)
 
 (define* (pass-artificial ast tables)
   (list
    (trans-global-decl
     (lambda (ast)
       (if (has-true-bool-prop-memb? ast 'artificial)
	   (push ast '(no-emit #t))
	   ast))
     ast)
    tables))

) ;; end module
