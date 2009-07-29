;; 
;; node-ctors.scm
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

;; Provides less generic, but typically usable and more convenient
;; node constructors than those provided by "ast-spec.scm". This
;; module serves the same purpose as the classic, retired "cxx.scm"
;; module.
;; 
;; For related work, see C FFIs of Scheme implementations, for
;; instance. http://square.umin.ac.jp/hchang/ksm/ref/ksm_10.html is
;; one example, and indeed http://square.umin.ac.jp/~hchang/ksm/
;; KSM-Scheme attempts to easily integrate with the C language.
(module
 node-ctors
 mzscheme

 (require (lib "ast-util.scm" "wg"))
 (require (lib "ast-def-util.scm" "wg"))
 (require (lib "prop-util.scm" "wg"))
 (require (lib "compact.scm" "wg"))
 (require (lib "quote-undefined.scm" "wg"))
 (require (lib "settings.scm" "wg"))
 (require (lib "usual.scm" "wg"))
 (require (lib "mop.scm" "wg"))

 ;; Here in this file we will want to be making use of these functions
 ;; to help in protect against AST format changes; for one thing,
 ;; using the functions may help us remain compatible, and for another
 ;; thing, using the functions may get the compiler to complain if
 ;; things change.
 (require (prefix a. (lib "ast-spec.scm" "wg")))

 ;; -------------------------
 ;; utilities...

 (define-for-syntax make-a-symbol
   (lambda (sym) 
     (string->symbol (string-append "a." (symbol->string sym)))))

 (define-syntax (provide-a stx)
   (syntax-case 
    stx ()
    ((_ name)
     (with-syntax ((aname (datum->syntax-object stx (make-a-symbol (syntax-object->datum (syntax name))))))
		  (syntax (define* name aname))))))

 ;; -------------------------
 ;; common attributes...

 (provide-a name)

 (define-syntax* nameq
   (syntax-rules ()
     ((_ sym)
      (a.name (quote sym)))))

 (define* (qual-name . names)
   (make-qname (map to-symbol names)))
 
 (define* (init expr)
   (a.init (to-expr expr)))

 (define* (ctor-args . arglist)
   (call-args arglist))

 ;; -------------------------
 ;; documentation...

 (provide-a doc)
 (provide-a cxx-doc)
 (provide-a cxx-cmt-doc)

 (def-list-ctor* source)
 (def-list-ctor* file)
 (def-list-ctor* line)
 (def-list-ctor* lines)

 ;; -------------------------
 ;; annotation...

 (define* (annot-lambda name value)
   (lambda (f . args)
     (cond 
      ((procedure? f)
       (set-nlist-elem-1 (apply f args) name value))
      ((null? args)
       (set-nlist-elem-1 f name value))
      (else
       (sc-list 
	(map
	 (lambda (elem)
	   (set-nlist-elem-1 elem name value))
	 (cons f args)))))))

 (define* annot set-nlist-elem)

 ;; -------------------------
 ;; directives...

 (provide-a includes)

 (define* (system-include file)
   `(include (incl-kind system) (file ,(to-string file))))

 (define* (local-include file)
   `(include (incl-kind local) (file ,(to-string file))))

 (define* (import fpath)
   (unless (path? fpath)
	   (set! fpath (string->path fpath)))
   (set! fpath 
	 (if (absolute-path? fpath)
	     (expand-path fpath)
	     (path->complete-path
	      fpath (current-load-relative-directory))))

   (let ((res '()))
     (with-input-from-file 
	 fpath
       (lambda ()
	 (let loop ()
	   (let ((datum (read)))
	     (unless (eof-object? datum)
		     (push-set! res (eval datum))
		     (loop))))))
     (sc-list res)))

 ;; -------------------------
 ;; type expressions...

 (define* (to-texpr texpr)
   (cond
    ((list? texpr)
     texpr)
    ((symbol? texpr)
     (tname-ref texpr))
    ((string? texpr)
     (tname-ref texpr))
    (else
     (error "type expression" texpr))))

 (define* (type texpr)
   ;;(write-nl texpr)
   (a.type (to-texpr texpr)))

 (provide-a tvar)

 (define* (ptr-to texpr)
   `(ptr-to (type ,(to-texpr texpr))))
    
 (define* (ptr-to-member classname texpr)
   `(ptr-to-member (of-class (type (tname-ref (name ,(to-symbol classname))))) (class-member (type ,(to-texpr texpr)))))

 (define* (ref-to texpr)
   `(ref-to (type ,(to-texpr texpr))))
    
 (define* array-of
   (case-lambda
    ((texpr) `(array-of (type ,(to-texpr texpr))))
    ((texpr cexpr) `(array-of (type ,(to-texpr texpr)) (num-elems ,cexpr)))))
    
 (define* (const texpr)
   `(const (type ,(to-texpr texpr))))
    
 (define* (volatile texpr)
   `(volatile (type ,(to-texpr texpr))))
    
 (define* (tname-ref name)
   `(tname-ref (name ,(to-symbol name))))

 (define-syntax* tname-refq
   (syntax-rules ()
     ((_ sym)
      (tname-ref (quote sym)))))

 (define* (func-t argtypes rettype)
   (a.func-t 
    (apply a.args (map (compose a.arg type) argtypes))
    (a.returns (type rettype))))

 (define* ast-func-t a.func-t)

 (provide-a this-type)
 (provide-a self-type)

 (define* long-long '|long long int|)

 (define* (sizeof texpr)
   (a.sizeof-expr `(type-expr ,(to-texpr texpr)) (type 'int)))

 (define* (macro-type texpr)
   (a.texpr-expr `(type-expr ,(to-texpr texpr))))

 ;; -------------------------
 ;; namespace declarations...

 (define* unit a.cxx-unit)
 (define* namespace a.cxx-namespace)
 (provide-a basename)

 ;; -------------------------
 ;; type declarations...

 (define* fund-type a.cxx-fund-type)
 (define* typedef a.cxx-typedef)
 (define* class a.cxx-class)
 (define* struct a.cxx-struct)
 (define* union a.cxx-union)

 (define* enumeration a.cxx-enum)
 (provide-a enum-items)
 (provide-a enum-item)
 (provide-a enum-value)

 (provide-a body)

 (define* (bases . baselist)
   (cons 
    'bases
    (map
     (lambda (ast)
       (cond
	((list-named? ast 'base)
	 ast)
	((any-pred symbol? string? ast)
	 (base (base-access 'public) (type ast)))
	(else
	 (error "base" ast))))
     baselist)))

 (define* (base-access access)
   (a.base-access (to-symbol access)))

 (define* (base . attr)
   (apply a.base attr))

 ;; -------------------------
 ;; variable declarations...

 (define* var a.cxx-var)

 ;; For convenience. Documentation or other annotations not supported,
 ;; but may still suit local variables in most cases, and hence the
 ;; name.
 (define* lvar
   (case-lambda
    ((tp nm)
     (var (name nm) (type (to-texpr tp))))
    ((tp nm in)
     (push (lvar tp nm) (init (to-expr in))))))

 ;; -------------------------
 ;; function and method declarations...

 (define* func a.cxx-func)
 (define* ctor a.cxx-ctor) 
 (define* dtor a.cxx-dtor) 
 (define* oper a.cxx-oper)
 (define* conv a.cxx-conv)

 (provide-a returns)

 ;; Errors. The "error-code" elements are optional children of
 ;; "returns", and not of the function itself.
 ;; 
 ;; In case the error must be checked programmatically, the values
 ;; must all be given as valid expressions. An error code is not
 ;; always an integer; it could be NULL or !NULL, for instance, but
 ;; expressions allow all this, and should print sufficiently well,
 ;; too.
 ;; 
 ;; It would be nice to be able to pass a symbolic name of the error
 ;; code, and then, if the name maps to a constant literal, grab the
 ;; literal and include it in the documentation along with the
 ;; symbolic name.
 ;; 
 ;; (error-code (name KErrNone) (reason (cnot cnull)) (description "no
 ;; error"))
 ;; 
 ;; If only "name" is provided, an attempt will be made to resolve
 ;; "reason" based on the name, but this only succeeds if the name is
 ;; known, and resolves to a constant literal. If an entry has no
 ;; "reason", then one can always try just using the name in code, and
 ;; hope it will be known at compile time.
 (def-list-ctor* error-codes)
 (def-list-ctor* error-code)
 (define* (reason expr)
   `(reason ,(to-expr expr)))
 (define* (description string)
   `(description ,(to-string string)))

 (provide-a args)
 (provide-a arg)
 (provide-a varargs)

 (provide-a ctor-init-list)

 (define* (ctor-super texpr arglist)
   (a.ctor-super
    (type texpr)
    (call-args arglist)))

 (define* (ctor-var varname arglist)
   (a.ctor-var
    (name varname)
    (call-args arglist)))

;  ;; operator xxx
;  ;; defined-cast xxx

 (define* (block . items)
   (a.block (apply a.stmts items)))

 (define (stmt-seq . items)
   (a.stmt-seq (apply a.stmts items)))

 ;; -------------------------
 ;; public, protected, private

 (provide-a public)
 (provide-a protected)
 (provide-a private)

 (define* (group props . elems)
   (when (symbol? props)
	 (set! props (list props)))
   (sc-list
    (map
     (lambda (elem)
       (if (named-list? elem)
	   (set-prop-membs elem props)
	   elem))
     elems)))

 ;; -------------------------
 ;; modifiers...

 (def-symbol* virtual)
 (def-symbol* artificial)
 (def-symbol* auto)
 (def-symbol* dll-export)
 (def-symbol* explicit)
 (def-symbol* export)
 (def-symbol* extern)
 (def-symbol* extern-c)
 (def-symbol* external)
 (def-symbol* inline)
 (def-symbol* no-emit)
 (def-symbol* non-modifying)
 (def-symbol* pure)
 (def-symbol* register)
 (def-symbol* static)
 (def-symbol* unused)

 (define* (verbatim-modifier m)
   (list 'verbatim-modifier (to-string m)))
 
 ;; -------------------------
 ;; statements...

 (define* (expr-stmt expr)
   (a.expr-stmt (a.expr expr)))

 (define* return
   (case-lambda
    (() '(return-stmt))
    ((expr) `(return-stmt (expr ,(to-expr expr))))))

 (define* (goto label)
   `(goto (addr-label ,(to-symbol label))))

 (define* cbreak a.break-stmt)
 (define* continue a.continue-stmt)

 (define* cif
   (case-lambda
    ((c tb) (cif c tb none))
    ((c tb eb)
     (let ((res `(if-stmt (cond-expr ,(to-expr c)))))
       (when tb
	     (push-set! res `(then-stmt ,tb)))
       (when eb
	     (push-set! res `(else-stmt ,eb)))
       res))))

 (define* cunless
   (case-lambda
    ((c tb) (cunless c tb none))
    ((c tb eb) (cif (cnot c) tb eb))))

 (define* (only-if c . tb)
   (cif c (apply block tb)))

 (define* (only-unless c . tb)
   (cunless c (apply block tb)))

 (define-syntax* ccond
   (syntax-rules (default)
     ((_) none)
     ((_ expr1 stmt1 default stmt2)
      (cif expr1 stmt1 stmt2))
     ((_ expr stmt)
      (cif expr stmt))
     ((_ expr stmt rest ...)
      (cif expr stmt (ccond rest ...)))))
 
 ;; The action may be a block or a statement.
 (define* (cwhile cond action)
   (let ((res `(while-stmt (cond-expr ,(to-expr cond)))))
     (when action
	   (push-set! res `(stmt ,action)))
     res))

 (define* (cuntil cond action)
   (cwhile (cnot cond) action))

 (define* (do-while action cond)
   (define ret `(do-while-stmt (cond-expr ,(to-expr cond))))
   (if action (push-set! ret `(stmt ,action)))
   ret)

 ;; "init" is an optional expression or simple declaration. "cond" is
 ;; an optional conditional expression. "step" is an optional
 ;; expression. "action" is a required expression, but it may be an
 ;; empty (block).
 (define* (cfor init cond step action)
   (let ((ret '(for-stmt)))
     ;; xxx We presently do not support simple declarations. This
     ;; would actually not be that easy to do, since the whole
     ;; system does not know that concept.
     (if init (push-set! ret `(init-expr ,(to-expr init))))
     (if cond (push-set! ret `(cond-expr ,(to-expr cond))))
     (if step (push-set! ret `(step-expr ,(to-expr step))))
     (if action
         (push-set! ret `(stmt ,action))
         (error-f "no action for ~s" ret))
     ret))

 (define* (forever action)
   (cfor none none none action))

 ;; This is like the C++ "switch". The syntax is (cswitch expr (case
 ;; value action ...) (case value action ...) (else action ...)).
 (define-syntax* cswitch
   (syntax-rules (case else)
     ((_ ex (else z ...)) 
      (switch-stmt ex (switch-default z ...)))
     ((_ ex (case x ...) ... (else z ...))
      (switch-stmt ex (switch-case x ...) ... (switch-default z ...)))
     ((_ ex (case x ...) ...) 
      (switch-stmt ex (switch-case x ...) ...))))

 (define* (switch-stmt expr . cases)
   (a.switch-stmt (a.switch-expr (to-expr expr))
		  (apply a.stmts cases)))

 (define* (switch-case expr . stmtlist)
   (sc-list (cons (a.switch-case (a.switch-expr (to-expr expr)))
		  stmtlist)))
 
 (define* (switch-default . stmtlist)
   (sc-list (cons (a.switch-default) stmtlist)))

 ;; A switch with SPECS semantics. Using this you can avoid the common
 ;; error of forgetting a break to end a case. xxx
;  (define-syntax* sswitch
;    (syntax-rules ()
;      ((_ (else z)) 
;       (switch-stmt (switch-default z)))
;      ((_ (case x ... y) ... (else z))
;       (switch-stmt (switch-case (list x ...) y) ... (switch-default z)))
;      ((_ (case x ... y) ...) 
;       (switch-stmt (switch-case (list x ...) y) ...))))
;  (define (switch-case values action)
;    `(switch-case (values ,@values) (action ,action)))
;  (define (switch-default action)
;    `(switch-default (action ,action)))

 ;; We would like a switch that supports not only integers, but
 ;; anything that can be compared with the "==" operator. But to
 ;; implement, we would need to store the result of the expression to
 ;; compare against stored in a variable, and that variable needs to
 ;; be given a type. For purposes of usability, it would be nice to be
 ;; able to infer the type, at least in a typical case (it could still
 ;; optionally be provided). A name we can some up with automagically,
 ;; with "gensym". xxx

 ;; Some of our functions expect an argument that is actually
 ;; optional. In those cases one may use #f, or "none".
 (define* none #f)

 ;; -------------------------
 ;; expressions...

 (define* (to-expr expr)
   (cond
    ((list? expr)
     expr)
    ((symbol? expr)
     (name-ref expr))
    ((string? expr)
     (name-ref expr))
    ((integer? expr)
     (int-lit expr))
    (else
     (error "expression" expr))))

 (define* name-ref
   (case-lambda
    ((name)
     `(name-ref (name ,(to-symbol name))))
    ((name texpr)
     (push (name-ref name) (type texpr)))))

 (define-syntax* name-refq
   (syntax-rules ()
     ((_ sym)
      (name-ref (quote sym)))))

 (define (call-args arglist)
   `(exprs ,@(map to-expr arglist)))

 (define (add-call-args arglist elem)
   (push elem (call-args arglist)))

 ;; A general-purpose call function. The other variants may be more
 ;; convenient in some cases.
 (define* call
   (case-lambda
    ((fexpr) (call fexpr '()))
    ((fexpr arglist) 
     (add-call-args arglist (a.call-expr (a.target (to-expr fexpr)))))))

 ;; Use this form for calls via function references.
 (define* call-ref call)

 ;; Use this form for calls via function pointers.
 (define* call-ptr
   (case-lambda
    ((fexpr) (call-ptr fexpr '()))
    ((fexpr arglist) (call (deref fexpr) arglist))))

 ;; Use this form for method calls via an explicit receiver reference.
 (define* call-on
   (case-lambda
    ((rexpr fexpr) (call-on rexpr fexpr '()))
    ((rexpr fexpr arglist)
     (call (field-on rexpr fexpr) arglist))))
 
 ;; Use this form for method calls via an explicit receiver pointer.
 (define* call-via
   (case-lambda
    ((rexpr fexpr) (call-via rexpr fexpr '()))
    ((rexpr fexpr arglist)
     (call (field-via rexpr fexpr) arglist))))

 (define* call-ctor
   (case-lambda
    ((texpr) 
     (a.ctor-call-expr (type texpr)))
    ((texpr arglist) 
     (add-call-args arglist (call-ctor texpr)))))

 ;; xxx super-call-expr
 
 ;; "new" operator.
 (define* new
   (case-lambda
    ((texpr) `(new-expr (type-expr ,(to-texpr texpr))))
    ((texpr arglist) (add-call-args arglist (new texpr)))))

 ;; xxx "new[]" operator.
 ;; xxx "new" overload
 ;; xxx "new[]" overload

 ;; "delete" operator.
 (define* (cdelete expr)
   `(delete-expr (expr ,(to-expr expr))))

 ;; "delete[]" operator.
 (define* (cdelete-array expr)
   `(delete-array-expr (expr ,(to-expr expr))))

 ;; expr[iexpr]
 (define* (index-ref expr iexpr)
   (a.index-expr (a.expr (to-expr expr)) (a.index (to-expr iexpr))))

 ;; expr.field_name
 (define* (field-on expr name)
   (a.dot-expr (a.expr1 (to-expr expr))
	       (a.expr2 (name-ref name))))

 ;; expr->field_name
 (define* (field-via expr name)
   (a.arrow-expr (a.expr1 (to-expr expr))
		 (a.expr2 (name-ref name))))

 ;; A comma-separated list of expressions. There may be some
 ;; restrictions as to what types of expressions may appear where,
 ;; but we do not care.
 (define* (expr-list . expr)
   (a.expr-list-expr (apply a.exprs (map to-expr expr))))

 ;; -------------------------
 ;; nullary expressions...

 (provide-a this)
 (provide-a self)

 ;; -------------------------
 ;; unary operator expressions...

 (define-syntax def-unary*
   (syntax-rules ()
     ((_ fn sym)
      (define* (fn expr)
	(list (quote sym) `(expr ,(to-expr expr)))))))

 ;; &expr
 (def-unary* addr-of addr-of-expr)

 ;; *expr
 (def-unary* deref deref-expr)

 ;; -expr
 ;; expr - expr
 (define* c-
   (case-lambda
    (() (error "c- is not a nullary expression"))
    ((x) `(uminus-expr (expr ,(to-expr x))))
    (e `(sub-expr (exprs ,@(map to-expr e))))))

 ;; +expr
 ;; expr + expr
 ;;
 ;; Yes, we do support uplus-expr in our AST, and C++ does also
 ;; support it; supporting it makes sense in the context of operator
 ;; overloading.
 (define* c+
   (case-lambda
    (() (error "c+ is not a nullary expression"))
    ((x) `(uplus-expr (expr ,(to-expr x))))
    (e `(add-expr (exprs ,@(map to-expr e))))))

 ;; !expr. Logical not.
 (def-unary* cnot not-expr)

 ;; ~expr. Bitwise two's complement.
 (def-unary* neg neg-expr)

 ;; ++expr
 (def-unary* pre++ pre-inc-expr)

 ;; --expr
 (def-unary* pre-- pre-dec-expr)

 ;; expr++
 (def-unary* post++ post-inc-expr)

 ;; expr--
 (def-unary* post-- post-dec-expr)

 ;; -------------------------
 ;; binary operator expressions...

 (define-syntax def-binary*
   (syntax-rules ()
     ((_ fn sym)
      (define* (fn e1 e2)
	(list (quote sym) 
	      `(expr1 ,(to-expr e1)) 
	      `(expr2 ,(to-expr e2)))))))

 (define-syntax def-multi*
   (syntax-rules ()
     ((_ fn sym)
      (define* fn
	(case-lambda
	 (() (error "not a nullary expression" (quote fn)))
	 ((x) (error "not a unary expression" (quote fn)))
	 (e (list (quote sym) `(exprs ,@(map to-expr e)))))))))

 (def-binary* c== eq-expr) ;; == (in C++), = (in SPECS)
 (def-binary* c!= neq-expr) ;; !=
 (def-binary* c< lt-expr) ;; <
 (def-binary* c> gt-expr) ;; >
 (def-binary* c<= lte-expr) ;; <=
 (def-binary* c>= gte-expr) ;; >=

 (def-multi* c* mul-expr) ; *
 (def-multi* c/ div-expr) ; /
 (def-binary* c% mod-expr) ; %
 (def-multi* cor or-expr) ; ||
 (def-multi* cand and-expr) ; &&
 (def-binary* c<< lshift-expr) ; <<
 (def-binary* c>> rshift-expr) ; >>
 (def-multi* cxor xor-expr) ; ^
 (def-multi* bit-or bit-or-expr) ; |
 (def-multi* bit-and bit-and-expr) ; &

 (def-binary* assign assign-expr) ; = (in C++), := (in SPECS)

 (def-binary* c+= add-assign-expr) ; +=
 (def-binary* c-= sub-assign-expr) ; -=
 (def-binary* c*= mul-assign-expr) ; *=
 (def-binary* c/= div-assign-expr) ; /=
 (def-binary* c%= mod-assign-expr) ; %=
 (def-binary* c<<= lshift-assign-expr) ; <<=
 (def-binary* c>>= rshift-assign-expr) ; >>=
 (def-binary* xor= xor-assign-expr) ; ^=
 (def-binary* bit-or= bit-or-assign-expr) ; |=
 (def-binary* bit-and= bit-and-assign-expr) ; &=

 ;; -------------------------
 ;; trinary operator expressions...

 ;; c-expr ? t-expr : e-expr
 (define* (if-expr c te ee)
   (a.if-expr (a.cond-expr (to-expr c))
	      (a.then-expr (to-expr te))
	      (a.else-expr (to-expr ee))))

 ;; -------------------------
 ;; cast expressions...

 (define-syntax def-cast*
   (syntax-rules ()
     ((_ fn nn)
      (define* (fn tp expr)
	(list (quote nn)
	      (type tp) 
	      `(expr ,(to-expr expr)))))))

 (def-cast* static-cast static-cast-expr)
 (def-cast* const-cast const-cast-expr)
 (def-cast* reinterpret-cast reinterpret-cast-expr)
 (def-cast* dynamic-cast dynamic-cast-expr)
 (def-cast* old-style-cast old-style-cast-expr)

 ;; -------------------------
 ;; literal expressions...

 (provide-a literal)
 (provide-a literal-value)
 (provide-a literal-format)

 ;; xxx can use some more conveniences for defining common literal
 ;; types here

 (define* (tnull texpr)
   `(literal (type ,(to-texpr texpr))
	     (literal-value 0)))

 (define* (bool-lit value)
   (a.literal (type 'bool)
	      (a.literal-value (true? value))))

 (define* ctrue (bool-lit #t))
 (define* cfalse (bool-lit #f))

 ;; In C++, (void*)0 does not assign to another pointer type, but
 ;; plain 0 does assign to any pointer type. Hence we could simply map
 ;; "null" to the "int" literal 0. However, this would lose semantics,
 ;; and we probably want to instead map to "NULL" variable reference,
 ;; and make sure that our global name table contains the name, and a
 ;; built-in definition for it. Or, we could use an "int" literal, and
 ;; use an annotation to preserve semantics. Either alternative will
 ;; do for now.
 (define* cnull (name-refq NULL))

 (define (make-int-lit v t f)
   (a.literal (type t)
	      (a.literal-value (check-integer v))
	      (a.literal-format (to-symbol f))))

 (define* (int-lit value)
   (make-int-lit value 'int 'dec))

 (define* (hex-int-lit value)
   (make-int-lit value 'int 'hex))

 (define* (oct-int-lit value)
   (make-int-lit value 'int 'oct))

 (define* long-lit
   (case-lambda
    ((value) (make-int-lit value '|long int| 'dec))
    ((value fmt) (make-int-lit value '|long int| fmt))))

 (define* long-long-lit
   (case-lambda
    ((value) (make-int-lit value '|long long int| 'dec))
    ((value fmt) (make-int-lit value '|long long int| fmt))))

 (define* (cstr* value)
   `(literal ,(type (ptr-to 'char))
	     (literal-value ,value)
	     (literal-format cstr)))

 (define* (cstr value)
   (cstr* (to-string value)))

 ;; Values for some structural object lacking a constructor. The
 ;; values must presumably all be known at compile time. We may or may
 ;; not be able to determine the type of the literal, but this is not
 ;; attempted here, at parse time, but maybe later, by looking at
 ;; context. For instance, if the literal is assigned to a variable
 ;; that has a known type, then obviously the literal type must be the
 ;; same. But for now, we do not even know whether we have an array or
 ;; a structure.
 (define* (clist . value)
   (a.literal
    (a.literal-value (map to-expr value))
    (a.literal-format 'clist)))

 ;; -------------------------
 ;; special expressions...

 ;; The first argument is a statement/declaration that is to be lifted
 ;; out of the expression, and added as the preceding statement. The
 ;; second argument is the expression to replace this expression once
 ;; processed.
 (define* (lift-above stmt expr)
   (a.lift-stmt-expr (a.stmt stmt) (a.expr (to-expr expr))))

 ;; -------------------------
 ;; inline foreign language...

 (define* (context-spec? context)
   (or (not context)
       (and (list? context)
            (= (length context) 2)
            (memq (first context) '(iface impl _))
            (memq (second context) '(vint vext _)))))
 
 ;; The context specifier can be used to restrict emission to certain
 ;; contexts. Otherwise emission is otherwise unrestricted, but may be
 ;; affected by restrictions that apply to the emission of any
 ;; surrounding container(s). In particular, if the surrounding
 ;; containers are discarded in their entirety, then the same will
 ;; happen to any contained items.
 ;; 
 ;; For flexibility, the content of the chunk is specified not as a
 ;; string but as an AST of pretty printing instructions, as defined
 ;; in code-print.scm. In a simple case you may use `(text ,string).
 ;; 
 ;; Some "context" combinations cannot be expressed, despite the
 ;; possibility of using the "_" wildcard; in such cases one still has
 ;; the option of making more than one of the same chunk.
 (define/kw (make-chunk pp-ast #:key context)
   (define ast `(cxx-chunk (pp-code ,pp-ast)))
   (when context
     (if (context-spec? context)
         (push-set! ast `(context ,context))
         (error "invalid context specifier" context)))
   ;;(write-nl ast)
   ast)
 
 (define* (cxx-line string . op)
   (apply make-chunk `(text ,(to-string string)) op))

 (define* (cxx-iface-line string)
   (cxx-line string #:context '(iface _)))
 
 (define* (cxx-lines strings . op)
   (apply make-chunk
          `(sep nl ,@(map to-string strings))
          op))

  (define (do-cxx-lines ctx lines)
    ((if (list? lines) cxx-lines cxx-line) lines #:context ctx))

  (define* (cpp-if expr-string . op)
    (let ((string (format "#if ~a" (to-string expr-string))))
      (apply make-chunk `(indent0 (text ,string)) op)))
  
  (define* (cpp-else . op)
    (apply make-chunk `(indent0 (text "#else")) op))
  
  (define* (cpp-end . op)
    (apply make-chunk `(indent0 (text "#endif")) op))
  
  ;; Convenience utilities. Presently, there is lack of support for
  ;; things like #defines, forward declarations, and internal
  ;; #includes, and a possible workaround for this it to use these
  ;; utilities and textual C++ code.
  
  (define* (cxx-internal-declarations . lines-list)
    (sc-list
     (map
      (fix do-cxx-lines '(iface vint))
      lines-list)))

  (define* (cxx-exported-declarations . lines-list)
    (sc-list
     (map
      (fix do-cxx-lines '(iface vext))
      lines-list)))
 
 ;; -------------------------
 ;; C++ conveniences...

 (define* (return-if-null expr)
   (cif (c== expr cnull)
	(return)))

 (define* (return-null-if-null expr)
   (cif (c== expr cnull)
	(return cnull)))

 ;; -------------------------
 ;; Symbian specific...

 ;; We could consider defining Symbian-specific stuff in a separate
 ;; file. XXX

 (define* KErrNone-code
   (error-code (name "KErrNone") (reason 0)
	       (description "No error.")))

 (define* leaving '(leaving #t))
 (define* not-leaving '(leaving #f))

 (define* known-leaving (annot-lambda 'leaving #t))
 (define* known-not-leaving (annot-lambda 'leaving #f))
 (define* assume-leaving (annot-lambda 'assume-leaving #t))
 (define* assume-not-leaving (annot-lambda 'assume-leaving #f))

 (define* non-sharable '(non-sharable #t))
 
 ;; This is just a pretty-printing hint.
 (def-symbol* gldef-c)

 ;; A leaving "new" operator, i.e. "new (ELeave)", specifically for
 ;; Symbian OS.
 (define* leaving-new
   (case-lambda
    ((texpr) (leaving-new texpr '()))
    ((texpr arglist) (push (new texpr arglist) (list 'leaving #t)))))

 ;; for Symbian
 (define* (leave expr)
   (push (call 'User::Leave (list expr)) (list 'leaving #t)))
 
 ;; for Symbian
 (define* (leave-if-error expr)
   (push (call 'User::LeaveIfError (list expr)) (list 'leaving #t)))
 
 ;; for Symbian
 (define* (leave-if-null expr)
   (push (call 'User::LeaveIfNull (list expr)) (list 'leaving #t)))
 
 ;; for Symbian
 (define* (leave-no-memory)
   (push (call 'User::LeaveNoMemory) (list 'leaving #t)))
 
 ;; for Symbian
 (define* (panic cat reason)
   (call 'User::Panic (list cat reason)))

 ;; for Symbian
 (define* (invariant)
   (call 'User::Invariant))

 (define* (assert-invariant e)
   (call '__ASSERT_ALWAYS (list (to-expr e) (invariant))))
 
 (define* (trap lvalue stmt)
   ;; This is a special kind of an assignment expression (in a
   ;; semantical sense), but due to the way it is implemented, it
   ;; cannot appear in an expression position, but must appear where a
   ;; declaration can appear. And can contain any statement, including
   ;; a block (possibly containing declarations); in a typical case
   ;; would contain only an expression statement.
   (a.trap-block (a.expr (to-expr lvalue)) (a.stmt stmt)))

 (define* (trapd varname stmt)
   ;; We do it like this to avoid having a separate AST node for TRAPD
   ;; as well. The output may not be quite as compact, but it should
   ;; still be just as readable.
   (stmt-seq
    (var (name varname) (type 'TInt))
    (trap varname stmt)))

 ;; Uses existing "error" variable.
 (define* (trap-error stmt)
   (trap 'error stmt))

 ;; Defines "error" as the variable to use.
 (define* (trapd-error stmt)
   (trapd 'error stmt))

 (define* (trap* arg stmt)
   (trap arg (assume-leaving stmt)))
 (define* (trapd* arg stmt)
   (trapd arg (assume-leaving stmt)))
 (define* (trap-error* stmt)
   (trap-error (assume-leaving stmt)))
 (define* (trapd-error* stmt)
   (trapd-error (assume-leaving stmt)))

 (define* (ctor* . args)
   (annot (apply ctor args) not-leaving))
 (define* (dtor* . args)
   (annot (apply dtor args) not-leaving))

 (define* (return-error-if-error expr)
   (cif (c!= expr 'KErrNone) (return expr)))

 ;; Actually, we would probably want to use an array of characters in
 ;; the literal definitions rather than a C string; this way we would
 ;; save space, since there would be no trailing null to go into the
 ;; data section. This should be quite easy to do, but would have to
 ;; account for string/character escapes.

 (define (make-lit-decl type-s lit-type name string)
   (let* ((sstring (to-string string))
	  (tstring (format type-s sstring))
	  (tsym (string->symbol tstring)))
     `(cxx-var (name ,(to-symbol name))
	       ,(type (const tsym))
	       (string ,sstring)
	       (lit-type ,lit-type)
	       (static #t))))

 (define* (lit-decl name string)
   (make-lit-decl "TLitC<sizeof(L~s)/2>" '_LIT name string))

 (define* (lit8-decl name string)
   (make-lit-decl "TLitC<sizeof(~s)>" '_LIT8 name string))

 (define* (lit16-decl name string)
   (make-lit-decl "TLitC<sizeof(L~s)/2>" '_LIT16 name string))

 (define (make-lit f)
   (lambda (string)
     (let ((name (gensym "literalDes")))
       (lift-above (f name string) name))))
 
 (define* lit (make-lit lit-decl))
 (define* lit8 (make-lit lit8-decl))
 (define* lit16 (make-lit lit16-decl))

 (define* (basic-dll-entry-func)
   (ic (gop-false? 'eka2)
       (func
	;; GLDEF_C seems to be defined as nothing, so we are using it
	;; purely as a pretty printing hint.
	gldef-c
	(name 'E32Dll)
	(args 
	 (arg (type 'TDllReason)
	      (doc "Specifies the reason for entering the DLL.")))
	(returns (type 'TInt))
	(doc "Symbian DLL entry point.")
	(block (return 'KErrNone)))))

 ;; -------------------------
 ;; Python specific...

 ;; Any Python documentation.
 ;;
 ;; (py-doc STRING)
 (def-doc-ctor* py-doc)

 ;; Python docstring.
 ;;
 ;; (py-doc-doc STRING)
 (def-doc-ctor* py-doc-doc)

 (define* (py-inc-ref expr)
   (call 'Py_INCREF (list expr)))

 (define* (py-dec-ref expr)
   (call 'Py_DECREF (list expr)))

 (define* (py-xdec-ref expr)
   (call 'Py_XDECREF (list expr)))

 ;; This is PyS60 specific, I believe.
 (define* (py-restore-current-thread)
   (call 'PyEval_RestoreThread
	 (list (field-via 'PYTHON_TLS
			  'thread_state))))

 (define* (py-save-thread)
   (call 'PyEval_SaveThread))

 (define* (py-symbian-error expr)
   (call 'SPyErr_SetFromSymbianOSErr (list expr)))

 (define* (py-throw-symbian-error expr)
   (return (py-symbian-error expr)))

 (define* (py-throw-if-error expr)
   (cif (c!= expr 'KErrNone) (py-throw-symbian-error expr)))

 (define* (py-return-none)
   (known-not-leaving cxx-line "RETURN_NO_VALUE;"))

 (define* (py-return-none-or-error expr)
   (stmt-seq (py-throw-if-error expr)
	     (py-return-none)))

 ;; Converts a descriptor to unicode. The argument must specify a
 ;; descriptor reference.
 (define* (py-des-to-unicode dexpr)
   (call 'Py_BuildValue (list (cstr "u#")
			      (call-on dexpr 'Ptr)
			      (call-on dexpr 'Length))))

 (define* (py-make-int expr)
   (call 'Py_BuildValue (list (cstr "i") expr)))

 (define* (py-parse-args expr fmt . args)
   (cunless (call 'PyArg_ParseTuple
		  (append
		   (list expr (cstr fmt))
		   (map addr-of args)))
	    (return cnull)))

) ; end module

#;
(begin
  (require node-ctors)
  (require "util.scm")
  (write-nl (name 'Foo))
  (write-nl (nameq Bar))
  (write-nl (func-t (list (tname-refq Foo)) (tname-ref 'Bar)))
  (write-nl
   (unit
    (basename "miso")
    (body
     (class
      (nameq MyClass)
      (body
       (func 
	public (nameq MyMethod)
	(block
	 (return (cnot (name-refq MyVar)))))))))))

