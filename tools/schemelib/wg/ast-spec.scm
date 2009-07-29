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

;; This module serves as a specification for our AST. It also provides
;; some very basic AST node constructors. The naming of the
;; constructors is one-to-one with the AST node naming.
;; 
;; Our rules for node naming:
;; * no name may clash with a built-in Scheme name
;; * a node name must uniquely identify node semantics; there may be
;;   no two nodes with the same name but different semantics
;; 
;; For instance, "name" in (cxx-class (name NAME)) and (name-ref (name
;; NAME)) does have the same semantics, and hence this is okay.

(module
 ast-spec
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "compact.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "adt.scm" "wg"))

 ;; -------------------------
 ;; common attributes...

 ;; (name SYMBOL)
 (def-symbol-ctor* name)

 ;; (type TYPE-EXPR)
 (def-list-ctor* type)

 ;; (expr EXPR)
 (def-list-ctor* expr)

 ;; (expr1 EXPR)
 (def-list-ctor* expr1)

 ;; (expr2 EXPR)
 (def-list-ctor* expr2)

 ;; (exprs EXPR ...)
 (def-list-ctor* exprs)

 ;; A variable-initializer expression.
 ;;
 ;; (init EXPR)
 ;; 
 ;; Note that for classes with a suitable single-argument constructor,
 ;; it is equivalent to say "X x = 3;" and "X x = X(3);" or whatever;
 ;; this does not work with multi-argument ctors, as initializer lists
 ;; cannot be used with types for which a ctor is defined, i.e. it is
 ;; not okay to say "X x = {1,2};" instead of "X x = X(1,2);".
 (def-list-ctor* init)

 ;; -------------------------
 ;; documentation...

 (define-data
   (new-adt* named docstring

	  ;; (doc STRING)
	  (new-cdt* (named doc def-doc-ctor))

	  ;; (cxx-doc STRING)
	  (new-cdt* (named cxx-doc def-doc-ctor))

	  ;; (cxx-cmt-doc STRING)
	  (new-cdt* (named cxx-cmt-doc def-doc-ctor))))

 ;; -------------------------
 ;; directives...

 ;; Includes. An attribute of cxx-unit.
 ;;
 ;; (includes (include ...) ...)
 ;; (include (incl-kind system) (file STRING) ...)
 ;; (include (incl-kind local) (file STRING) ...)

 (def-list-ctor* includes)
 (def-list-ctor* include)
 (def-string-ctor* file)
 (def-symbol-ctor* incl-kind)

 ;; -------------------------
 ;; type expressions...

 (define-data
   (new-adt* named ast-node))

 (define-data
   (existing-dt named ast-node
		(new-adt* named type-expression)))

 ;; (ptr-to (type TYPE-EXPR) ...)
 (extend-adt* type-expression ptr-to)

 ;; (ptr-to-member (of-class (type TYPE-EXPR) ...) (class-member (type TYPE-EXPR) ...) ...)
 (extend-adt* type-expression ptr-to-member)
 (def-list-ctor* of-class)
 (def-list-ctor* class-member)

 ;; (ref-to (type TYPE-EXPR) ...)
 (extend-adt* type-expression ref-to)

 ;; (array-of (type TYPE-EXPR) (num-elems CONST-EXPR) ...)
 (extend-adt* type-expression array-of)
 (def-list-ctor* num-elems)

 ;; (func-t (args (arg (type TYPE-EXPR) (name SYMBOL) ...) ...) (returns (type TYPE-EXPR) ...) ...)
 (extend-adt* type-expression func-t)
 (def-list-ctor* args)
 (def-list-ctor* arg)
 (def-list-ctor* varargs)
 (def-list-ctor* returns)
 
 ;; (const (type TYPE-EXPR) ...)
 (extend-adt* type-expression const)

 ;; (volatile (type TYPE-EXPR) ...)
 (extend-adt* type-expression volatile)

 ;; (tname-ref (name SYMBOL) ...)
 (extend-adt* type-expression tname-ref)

 ;; Type variable.
 ;; (tvar (type-id SYMBOL) ...)
 (extend-adt* type-expression tvar)
 (def-symbol-ctor* type-id)

 (extend-adt* type-expression self-type def-null-ctor)
 (define* (this-type) '(ptr-to (type (self-type))))

 ;; These are not type expressions, but they are related nonetheless.
 (define* this '(name-ref (name this)))
 (define* self `(deref-expr (expr ,this)))
     
 (define* (check-type-expression expr)
   (check-with type-expression? "~s is not a type expression" expr))

 ;; -------------------------
 ;; namespace declarations...

 (define-data
   (existing-dt named ast-node
		(new-adt* named declaration)))

 ;; Compilation unit definition. The unnamed top-level namespace.
 ;; 
 ;; (cxx-unit (body DEFINITION ...) ...)
 (extend-adt* declaration cxx-unit)

 ;; Full or partial namespace content definition.
 ;;
 ;; (cxx-namespace (name SYMBOL) (body DEFINITION ...) ...)
 (extend-adt* declaration cxx-namespace)

 (def-string-ctor* basename)

 ;; -------------------------
 ;; type declarations...

 (define-data
   (existing-dt named declaration
		(new-adt* named type-decl)))

 ;; Fundamental type. A type that cannot be expressed in terms of
 ;; other types.
 ;; 
 ;; (cxx-fund-type (name SYMBOL) ...)
 (extend-adt* type-decl cxx-fund-type)

 ;; Type alias (i.e. typedef-based type definition).
 ;;
 ;; (cxx-typedef (name SYMBOL) (type TYPE-EXPR) ...)
 (extend-adt* type-decl cxx-typedef)

 ;; Class definition/declaration.
 ;;
 ;; (cxx-class (name SYMBOL) (body DEFINITION ...) ...)
 (define-data
   (existing-dt named type-decl
		(new-adt* named record-type-decl
			  (new-cdt* named cxx-class)
			  (new-cdt* named cxx-struct)
			  (new-cdt* named cxx-union))))

 ;; Enumeration type definition/declaration.
 ;;
 ;; (cxx-enum (name SYMBOL) (enum-items (enum-item (name SYMBOL) (enum-value CONST-INT-EXPR) ...) ...) ...)
 (extend-adt* type-decl cxx-enum)
 (def-list-ctor* enum-items)
 (def-list-ctor* enum-item)
 (def-list-ctor* enum-value)

 (def-list-ctor* body)

 ;; (bases (base ...) (base ...) ...)
 (def-list-ctor* bases)

 ;; (base (base-access ACCESS) (type TYPE-EXPR) (virtual #t) ...)
 (extend-adt* ast-node base)
 (def-list-ctor* base-access)

 ;; xxx friend
 ;; xxx template
 ;; xxx using

 ;; -------------------------
 ;; variable declarations...

 ;; Variable definition/declaration. Is one of:
 ;; * local variable, in a function or method
 ;; * global variable, at top level or in a namespace
 ;; * class variable, i.e. static variable within a class
 ;; * instance variable, in a class, struct, or union
 ;; 
 ;; (cxx-var (name SYMBOL) (type TYPE-EXPR) (init EXPR) (exprs EXPR
 ;; ...) ...)
 ;; 
 ;; where "exprs" can be used to specify ctor arguments. "init" and
 ;; "exprs" are mutually exclusive.
 (define-data
   (existing-dt named declaration
		(new-adt* named value-decl
			  (new-adt* named var-decl
				    (new-cdt* named cxx-var)
				    (new-cdt* named global-var)
				    (new-cdt* named class-var)
				    (new-cdt* named inst-var)
				    (new-cdt* named local-var)))))

 ;; -------------------------
 ;; function and method declarations...

 ;; Function or method definition/declaration.
 ;;
 ;; (cxx-func (name SYMBOL) (args (arg (type TYPE-EXPR) (name SYMBOL)) ...) (returns (type TYPE-EXPR) ...) (block ...) ...)
 (define-data 
   (existing-dt named value-decl
		(new-adt* named func-like-decl
			  (new-adt* named func-decl
				    (new-cdt* named cxx-func)
				    (new-cdt* named global-func)
				    (new-adt* named method-decl
					      (new-cdt* named class-meth)
					      (new-cdt* named inst-meth)))
			  (new-cdt* named cxx-ctor)
			  (new-cdt* named cxx-dtor)
			  (new-cdt* named cxx-oper) ;; operator
			  (new-cdt* named cxx-conv) ;; defined-cast in SPECS
			  )))

 ;; Constructor initializer list.
 ;;
 ;; (ctor-init-list (ctor-super ...) (ctor-var ...) ...)
 (def-list-ctor* ctor-init-list)

 ;; (ctor-super (type TYPE-EXPR) (exprs CTOR-ARG-EXPR ...) ...)
 (extend-adt* ast-node ctor-super)

 ;; (ctor-var (name VAR-NAME) (exprs CTOR-ARG-EXPR ...) ...)
 (extend-adt* ast-node ctor-var)

 ;; -------------------------
 ;; public, protected, private

 ;; These are attributes associated with declarations.
 (def-symbol* public)
 (def-symbol* protected)
 (def-symbol* private)

 ;; (access public|protected|private)
 (def-list-ctor* access)

 ;; This represents an access label (public, protected, private);
 ;; there may be many of these within a class body.
 ;; 
 ;; (access-label SYMBOL)
 (def-list-ctor* access-label)

 ;; -------------------------
 ;; statements...

 (define-data 
   (existing-dt named ast-node
		(new-adt* named statement)))

 ;; Block of code.
 ;; 
 ;; (block (stmts BLOCK-ITEM ...) ...)
 ;; 
 ;; where BLOCK-ITEM is one of
 ;; * variable declaration
 ;; * block
 ;; * statement
 ;; 
 ;; This is a statement in the sense that it can invariably appear
 ;; where a statement is expected.
 (extend-adt* statement block)

 ;; (stmt-seq (stmts BLOCK-ITEM ...) ...)
 ;; 
 ;; Like a block, but does not introduce new scope. Not valid in C++.
 ;; Translated by turning into a block in a single-statement context,
 ;; and by splicing into a multi-statement context otherwise.
 (extend-adt* statement stmt-seq)

 ;; Expression statement.
 ;;
 ;; (expr-stmt (expr EXPR) ...)
 (extend-adt* statement expr-stmt)

 ;; Return statement. "expr" is optional.
 ;;
 ;; (return-stmt (expr EXPR) ...)
 (extend-adt* statement return-stmt)

 ;; (goto-stmt (addr-label SYMBOL) ...)
 (extend-adt* statement goto-stmt)
 (def-list-ctor* addr-label)

 (extend-adt* statement break-stmt)
 (extend-adt* statement continue-stmt)

 ;; (if-stmt (cond-expr EXPR) (then-stmt STATEMENT) (else-stmt STATEMENT) ...)
 (extend-adt* statement if-stmt)
 (def-list-ctor* then-stmt)
 (def-list-ctor* else-stmt)

 ;; (while-stmt (cond-expr EXPR) (stmt STATEMENT) ...)
 (extend-adt* statement while-stmt)

 ;; (do-while-stmt (cond-expr EXPR) (stmt STATEMENT) ...)
 (extend-adt* statement do-while-stmt)

 ;; "init" is an optional expression or simple declaration. "cond" is
 ;; an optional conditional expression. "step" is an optional
 ;; expression.
 ;;
 ;; (for-stmt (init-expr EXPR) (cond-expr EXPR) (step-expr STEP-EXPR) (stmt STATEMENT) ...)
 (extend-adt* statement for-stmt)
 (def-list-ctor* init-expr)
 (def-list-ctor* step-expr)
 (def-list-ctor* stmt)

 ;; This is the C++ style switch, not SPECS style. Any SPECS style one
 ;; will have to be translated.
 ;; 
 ;; We are using "switch-expr" rather than "expr" to indicate the
 ;; somewhat different semantics here. Not just any expression, but
 ;; one that is legal in C++. Note that while some C++ compilers allow
 ;; multiple values per "case", the C++ standard does not allow that.
 ;; We really do not need to support non-standard features, except
 ;; maybe temporarily, as we can compile such things to legal code.
 ;; 
 ;; (switch-stmt (switch-expr EXPR) (stmts STATEMENT ...))
 ;; 
 ;; Here a STATEMENT may also be either (switch-case (switch-expr
 ;; EXPRESSION)) or (switch-default), and hence the statement
 ;; classification for these constructs.
 (extend-adt* statement switch-stmt) ; "switch" statement
 (def-list-ctor* switch-expr)
 (def-list-ctor* stmts)
 (extend-adt* statement switch-case) ; "case", as in C++
 (extend-adt* statement switch-default) ; "default", as in C++

 ;; Symbian specific.
 ;;
 ;; (trap-block (expr LVALUE-EXPR) (stmt STATEMENT) ...)
 (extend-adt* statement trap-block)

 ;; -------------------------
 ;; expressions...

 (define-data 
   (existing-dt named ast-node
		(new-adt* named expression)))

 ;; (name-ref (name SYMBOL) ...)
 (extend-adt* expression name-ref)

 ;; Unusual in that holds a type expression.
 ;;
 ;; (sizeof-expr (type-expr TYPE-EXPR) ...)
 (extend-adt* expression sizeof-expr)

 ;; Types can not normally appear as an expression, but these rules
 ;; don't hold for when one is using a macro. This expression type
 ;; lets you put a type expression anywhere in a controlled way, to be
 ;; printed as is. With this one can let the compiler know that the
 ;; usual rules do not apply.
 ;;
 ;; (texpr-expr (type-expr TYPE-EXPR) ...)
 (extend-adt* expression texpr-expr)

 ;; Method/function call expression.
 ;; 
 ;; A "target" expression must always be given. A simple "name-ref" is
 ;; sufficient with functions, static methods, and when the receiver
 ;; is implicit, i.e. when the receiver is "*this". Otherwise you will
 ;; want to provide a function reference, a dereferenced function
 ;; pointer, or perhaps either a "dot-expr" or an "arrow-expr", for
 ;; method calls with an explicit receiver. Specifying a type (for
 ;; overload resolution) would usually be unnecessary, since "exprs"
 ;; ought to reveal enough, except maybe if there are overloads that
 ;; differ only in return value type (const and non-const variants are
 ;; common). We do not consider the option of passing a pointer to
 ;; member, since that is not presently supported.
 ;; 
 ;; (call-expr (target EXPR) (exprs EXPR ...) ...)
 ;; 
 ;; While any "decl-id-ref" indicating the possibly resolved target
 ;; would naturally go within "target", we are putting it directly
 ;; under "call-expr" at present.
 (extend-adt* expression call-expr)
 (def-list-ctor* target)

 ;; Can be used to create a value of a given type, assuming the value
 ;; has a constructor matching the arguments specified in "exprs".
 ;; 
 ;; (ctor-call-expr (type TYPE-EXPR) (exprs EXPR ...) ...)
 (extend-adt* expression ctor-call-expr)

 ;; Calls the specified method in a superclass. XXX Not presently
 ;; supported, and do not know if even C++ supports this.
 (extend-adt* expression super-call-expr)

 ;; (new-expr (type-expr TYPE-EXPR) (exprs EXPR ...) ...)
 ;; 
 ;; These should be annotated with a "decl-id-ref" indicating the
 ;; corresponding constructor definition.
 (extend-adt* expression new-expr)

 ;; "new[]" operator.
 ;;
 ;; does the num items arg here need to be constant? XXX
 (extend-adt* expression new-array-expr)

 ;; XXX new and new[] must support overloading also; I guess in
 ;; addition to having to specify type and count, we must also be able
 ;; to specify call arguments, of which there usually are none

 ;; "delete" operator. Yes, its application is an expression.
 ;; 
 ;; (delete-expr (expr EXPR) ...)
 (extend-adt* expression delete-expr)

 ;; "delete[]" operator. Yes, its application is an expression.
 ;;
 ;; (delete-array-expr (expr EXPR) ...)
 (extend-adt* expression delete-array-expr)

 ;; Index-based array value reference.
 ;;
 ;; (index-expr (expr EXPR) (index INDEX-EXPR) ...)
 (extend-adt* expression index-expr)
 (def-list-ctor* index)

 ;; Field reference.
 ;; 
 ;; We do not yet support ptr-to-member, if it is even appropriate for
 ;; fields.
 ;; 
 ;; (dot-expr (expr1 EXPR) (expr2 (name-ref (name SYMBOL) ...)) ...)
 ;; 
 ;; (arrow-expr (expr1 EXPR) (expr2 (name-ref (name SYMBOL) ...)) ...)
 ;; 
 ;; The first expression must resolve to a structure type (for
 ;; dot-expr), or to a pointer to a structure type (for arrow-expr).
 ;; The second expression is a name-ref, unless we come up with any
 ;; exceptions.
 (extend-adt* expression dot-expr)
 (extend-adt* expression arrow-expr)

 ;; A comma-separated list of expressions. There may be some
 ;; restrictions as to what types of expressions may appear where,
 ;; but we do not care.
 ;;
 ;; (expr-list-expr (exprs EXPR ...) ...)
 (extend-adt* expression expr-list-expr)

 ;; (lift-stmt-expr (stmt STATEMENT) (expr EXPRESSION))
 (extend-adt* expression lift-stmt-expr)

 ;; -------------------------
 ;; unary operator expressions...

 (define-data (existing-dt named expression
			   (new-adt* named unary-op-expr)
			   (new-adt* named binary-op-expr)
			   (new-adt* named multi-op-expr)
			   (new-adt* named trinary-op-expr)))

 ;; &expr
 (extend-adt* unary-op-expr addr-of-expr)

 ;; *expr
 (extend-adt* unary-op-expr deref-expr)

 ;; -expr
 (extend-adt* unary-op-expr uminus-expr)

 ;; +expr
 ;;
 ;; Yes, we do support uplus-expr in our AST, and C++ does also
 ;; support it; supporting it makes sense in the context of operator
 ;; overloading.
 (extend-adt* unary-op-expr uplus-expr)

 ;; !expr. Logical not.
 (extend-adt* unary-op-expr not-expr)

 ;; ~expr. Bitwise two's complement.
 (extend-adt* unary-op-expr neg-expr)

 ;; ++expr
 (extend-adt* unary-op-expr pre-inc-expr)

 ;; --expr
 (extend-adt* unary-op-expr pre-dec-expr)

 ;; expr++
 (extend-adt* unary-op-expr post-inc-expr)

 ;; expr--
 (extend-adt* unary-op-expr post-dec-expr)

 ;; -------------------------
 ;; binary operator expressions...

 ;; These are all binary operations.
 ;;
 (extend-adt* binary-op-expr eq-expr) ;; == (in C++), = (in SPECS)
 (extend-adt* binary-op-expr neq-expr) ;; !=
 (extend-adt* binary-op-expr lt-expr) ;; <
 (extend-adt* binary-op-expr gt-expr) ;; >
 (extend-adt* binary-op-expr lte-expr) ;; <=
 (extend-adt* binary-op-expr gte-expr) ;; >=

 ;; These are binary operations, but could all conceivably operate on
 ;; more than two arguments, assuming we know their associativity, and
 ;; make things happen. I do not know what C++ itself supports.
 (extend-adt* multi-op-expr add-expr) ; +
 (extend-adt* multi-op-expr sub-expr) ; -
 (extend-adt* multi-op-expr mul-expr) ; *
 (extend-adt* multi-op-expr div-expr) ; /
 (extend-adt* binary-op-expr mod-expr) ; %
 (extend-adt* multi-op-expr or-expr) ; ||
 (extend-adt* multi-op-expr and-expr) ; &&
 (extend-adt* binary-op-expr lshift-expr) ; <<
 (extend-adt* binary-op-expr rshift-expr) ; >>
 (extend-adt* multi-op-expr xor-expr) ; ^
 (extend-adt* multi-op-expr bit-or-expr) ; |
 (extend-adt* multi-op-expr bit-and-expr) ; &

 ;; These are all binary operations.
 ;;
 (extend-adt* binary-op-expr assign-expr) ; = (in C++), := (in SPECS)
 (extend-adt* binary-op-expr add-assign-expr) ; +=
 (extend-adt* binary-op-expr sub-assign-expr) ; -=
 (extend-adt* binary-op-expr mul-assign-expr) ; *=
 (extend-adt* binary-op-expr div-assign-expr) ; /=
 (extend-adt* binary-op-expr mod-assign-expr) ; %=
 (extend-adt* binary-op-expr lshift-assign-expr) ; <<=
 (extend-adt* binary-op-expr rshift-assign-expr) ; >>=
 (extend-adt* binary-op-expr xor-assign-expr) ; ^=
 (extend-adt* binary-op-expr bit-or-assign-expr) ; |=
 (extend-adt* binary-op-expr bit-and-assign-expr) ; &=

 ;; -------------------------
 ;; trinary operator expressions...

 ;; c-expr ? t-expr : e-expr
 ;; (if-expr (cond-expr EXPR) (then-expr EXPR) (else-expr EXPR) ...)
 (extend-adt* trinary-op-expr if-expr)
 (def-list-ctor* cond-expr)
 (def-list-ctor* then-expr)
 (def-list-ctor* else-expr)

 ;; -------------------------
 ;; cast expressions...

 (define-data 
   (existing-dt named expression
		(new-adt* named cast-expr)))

 ;; Cast expressions.
 ;;
 ;; (static-cast-expr (type TYPE-EXPR) (expr EXPR) ...)
 ;; (const-cast-expr (type TYPE-EXPR) (expr EXPR) ...)
 ;; (reinterpret-cast-expr (type TYPE-EXPR) (expr EXPR) ...)
 ;; (dynamic-cast-expr (type TYPE-EXPR) (expr EXPR) ...)
 ;; (old-style-cast-expr (type TYPE-EXPR) (expr EXPR) ...)

 (extend-adt* cast-expr static-cast-expr)
 (extend-adt* cast-expr const-cast-expr)
 (extend-adt* cast-expr reinterpret-cast-expr)
 (extend-adt* cast-expr dynamic-cast-expr)
 (extend-adt* cast-expr old-style-cast-expr)

 ;; -------------------------
 ;; literal expressions...

 ;; Literal expressions. It is up to the parser to determine the type
 ;; of a literal expression. Some implicit coercions are supported by
 ;; C++, so not all literal types need be expressible in the syntax;
 ;; presumably not all are expressible in C++ syntax, but there
 ;; probably is nothing stopping one from supporting all of them.
 ;; 
 ;; In particular, we _could_ support a typed NULL literal, unlike C++
 ;; itself.
 ;; 
 ;; VALUE is dependent on type. For literals of complex types such as
 ;; arrays and structures the VALUE may be an S-expression of some
 ;; sort, but in other cases a primitive Scheme value will probably
 ;; suffice.
 ;; 
 ;; "literal-format" is optional, but may be provided to guide pretty
 ;; printing. For instance, one could indicate whether to print an
 ;; integer in octal ("oct"), decimal ("dec"), or hexadecimal ("hex"),
 ;; or whether a char should be printed as a quoted character or a
 ;; quoted escaped number.
 ;; 
 ;; (literal (type TYPE-EXPR) (literal-value VALUE) (literal-format
 ;; SYMBOL) ...)

 (extend-adt* expression literal)
 (def-list-ctor* literal-value)
 (def-list-ctor* literal-format)

 ;; -------------------------
 ;; inline foreign language...

 ;; C++ code to be retained as is.
 ;; 
 ;; (cxx-chunk (pp-code AST) ...)
 ;; 
 ;; These nodes contain code-print.scm language rather than a text
 ;; string and directives. This is simpler and more flexible.
 (define-data
   (existing-dt named ast-node
		(new-cdt* named cxx-chunk)))

 ) ; end module

#;
(begin
  (require ast-spec)
  (require (lib "util.scm" "wg"))
  (require (lib "compact.scm" "wg"))
  (require (lib "ast-util.scm" "wg"))

  (write-nl (statement? (switch-stmt)))
  (write-nl (statement? (literal)))
  )
