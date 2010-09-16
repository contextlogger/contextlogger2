;; 
;; emission.scm
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
 emission
 mzscheme

 (require "ast-spec.scm")
 (require "ast-util.scm")
 (require (lib "prop-util.scm" "wg"))
 (require "compact.scm")
 (require "op.scm")
 (require "snippets.scm")
 (require "type-ast-simplify.scm")
 (require "type-dcl-form.scm")
 (require "usual.scm")

 (define baa-elems
   (lambda (ast)
     (map-elems backend-adapt-ast ast)))

 (define rename-baa-elems
   (lambda (ast name)
     (baa-elems (replace-car ast name))))

 (define* (expr-to-string expr)
   ;; Peculiar as this may seem, string conversion should succeed for
   ;; all expressions. Of course, we could restructure our code to
   ;; produce a string for all expressions, but it's actually
   ;; conceptually easier if everything converts to a snippet.
   (snippet->string (ast-to-snippet expr)))

 (define cexpr-conv expr-to-string)

 (define (type-to-cxx-string type)
   (let ((dform (type-to-dcl-form (simplify-texpr type))))
     (type-spec-to-cxx dform cexpr-conv)))

 ;; To be used with variables and typedefs.
 (define* var-type-string
   (lambda (ast)
     (let* ((type (fget-reqd-nlist-elem-1 ast 'type))
	    (name (fget-reqd-nlist-elem-1 ast 'name))
	    (dform (type-to-dcl-form (simplify-texpr type))))
       (set! dform (type-spec-set-name dform name))
       (type-spec-to-cxx dform cexpr-conv))))

 ;; Printing functions, we must preserve "name" and "unused" of args
 ;; that may appear within an immediate func-t -> we can handle this
 ;; as a special case, as we do generally want any func-t elements
 ;; appearing within the type stripped. This function does the
 ;; required fixing of the passed function type.
 (define (fix-arg-names dform arglist)
   ;;(write-nl arglist)
   (dcl-texpr-modify-inner-func 
    dform
    (lambda (dcl-func)
      (letrec ((do-zip
		(lambda (darg arg)
		  (define argtype (get-reqd-nlist-elem-1 arg 'type))
		  (awhen argname (get-opt-nlist-elem-1 arg 'name)
			 (if (has-true-bool-prop-memb? arg 'unused)
			     (set! argname (string-append "/*" (symbol->string argname) "*/")))
			 (set! darg (type-spec-set-name darg argname)))
		  darg))
	       (dargs (third dcl-func))
	       (nargs (map do-zip dargs arglist)))
	(replace-third dcl-func nargs)))))

 (define (func-type-string ast)
   (define func-name (fget-reqd-nlist-elem-1 ast 'name))
   (define func-t (fget-reqd-nlist-elem-1 ast 'type))
   (unless (list-named? func-t 'func-t)
	   (error "function not of function type" ast))

   (let ((dform (type-to-dcl-form (simplify-texpr func-t)))
	 (arglist (get-opt-nlist-elem-1up-e func-t 'args)))
     (unless (null? arglist)
	     (set! dform (fix-arg-names dform arglist)))
     (set! dform (type-spec-set-name dform func-name))
     (type-spec-to-cxx dform cexpr-conv)))

 (define (get-members ast)
   (fget-opt-nlist-elem-1up-e ast 'body))

 (define (any-docstring ast)
   (aand* doc (fget-opt-nlist-elem-1 ast 'doxy-doc)
          `(c-block-comment ,doc)))

 (define (add-any-docstring ast past)
   `(sep nl
     ,(any-docstring ast)
     ,past))

 (define (get-name ast)
   (fget-opt-nlist-elem-1 ast 'name))
 
 (define (baa-structure kind ast)
   (define bases (fget-opt-nlist-elem-1up-e ast 'bases))
   (define members (get-members ast))
   (define non-sharable? (fget-opt-nlist-elem-1 ast 'non-sharable))
   (define kind-and-name-string
     (alet name (get-name ast)
           (if name
               (alet name-string (to-string name)
                     (if non-sharable?
                         (format "NONSHARABLE_CLASS(~a)" name-string)
                         (format "~a ~a" kind name-string)))
               kind)))
   
   (add-any-docstring
    ast
    `(seq
      (sep space
       (text ,kind-and-name-string)
       ,(and (not (null? bases))
             `(seq (text-nl ":")
                   (indent
                    (sep comma-nl
                     ,@(map baa bases))))))
      (nl)
      (c-block skip ,@(map baa members))
      (text ";"))))

 (define (maybe-text text)
   (and text `(text ,(to-string text))))
 
 (define (baa-enumeration ast)
   (define (baa-item enum-item)
     (define name (or (get-name enum-item)
                      (error "no name" enum-item)))
     (define value (fget-opt-nlist-elem-1 enum-item 'value))
     (add-any-docstring
      enum-item
      `(sep equ
            (text ,(to-string name))
            ,(and value (ast-to-snippet value)))))
   
   (let* ((ename (get-name ast))
          (items (fget-opt-nlist-elem-1up-e ast 'enum-items)))
     (add-any-docstring
      ast
      `(seq
        (sep space (text "enum") ,(maybe-text ename)) (nl)
        (c-block comma-skip ,@(map baa-item items)) (text ";")))))
 
 (define (baa-func ast)
   (define type-string (func-type-string ast))

   (define (qual s key)
     (and (fget-opt-nlist-elem-1 ast key) `(text ,s)))

   (define ctor-init (fget-opt-nlist-elem ast 'ctor-init-list))

   (define body (fget-opt-nlist-elem ast 'block))
   
   (define ppi
     `(sep space
           ,(aand* s (fget-opt-nlist-elem-1 ast 'verbatim-modifier) `(text ,s))
           ,(qual "extern \"C\"" 'extern-c)
           ,(qual "GLDEF_C" 'gldef-c)
           ,(qual "IMPORT_C" 'import-c)
           ,(qual "EXPORT_C" 'export-c)
           ,(qual "inline" 'inline)
           ,(qual "static" 'static)
           ,(qual "virtual" 'virtual)
           (seq
            (text ,type-string)
            ,(qual " const" 'non-modifying)
            ,(and ctor-init
                  (not (null? (cdr ctor-init)))
                  `(seq
                    (text " : ")
                    ,(baa ctor-init)))
            ,(if body
                 `(seq (nl) ,(baa body))
                 `(seq ,(qual " = 0" 'pure) (text ";"))))))
   
   (add-any-docstring ast ppi))

 (define (baa-typedef ast)
   (define type-string (var-type-string ast))
   (define ppi `(seq (text "typedef ") (text ,type-string) (text ";")))
   (add-any-docstring ast ppi))
 
 (define (baa-var ast)
   (define lit-type (fget-opt-nlist-elem-1 ast 'lit-type))

   (define ppi
     (if lit-type

         ;; Symbian literal descriptor declaration.
         `(text-append
           ,(to-string lit-type)
           "("
           ,(to-string (fget-reqd-nlist-elem-1 ast 'name))
           ", "
           ,(inspect (fget-reqd-nlist-elem-1 ast 'string))
           ");")

         ;; Some other kind of a variable declaration.
         (let ((init (fget-opt-nlist-elem-1 ast 'init))
               (exprs (fget-opt-nlist-elem ast 'exprs))
               (type-string (var-type-string ast))
               (is-arg (eq? (car ast) 'arg))
               (qual 
                (lambda (s key)
                  (and (fget-opt-nlist-elem-1 ast key) `(text ,s)))))
           (when (and init exprs)
             (error "may not have both init and exprs"))
           `(seq
             ;; The storage-class specifiers include "auto", "extern",
             ;; "register", and "static" in C99, but whether C++ has exactly
             ;; the same set, do not know. "const" and "volatile", on the
             ;; other hand, are a part of the type, and are called type
             ;; qualifiers.
             (sep space
                  ,(qual "auto" 'auto)
                  ,(qual "extern" 'extern)
                  ,(qual "register" 'register)
                  ,(qual "static" 'static)
                  (text ,type-string))
             ,(and init
                   `(seq
                     (text " = ")
                     ,(ast-to-snippet init)))
             ,(and exprs
                   `(seq
                     (text "(")
                     ,(exprs-snippet (cdr exprs))
                     (text ")")))
             ,(and (not is-arg)
                   '(text ";"))))))

   (add-any-docstring ast ppi))
 
 (define* backend-adapt-ast
   (lambda (ast)
     ;;(write-nl ast)
     (cond

      ;; These are handled when transforming their parent, and
      ;; are not to be touched otherwise.
      ((list-named? ast 'type)
       ast)

      ((list-named? ast 'cxx-unit)
       ;; At the top level, comments do not naturally belong with
       ;; any unit member; rather, we are treating comments as
       ;; member-like objects.
       (let* ((doc (any-docstring ast))
              (members (map baa (get-members ast))))
         (when doc
           (set! members (cons doc members)))
         `(seq (sep skip ,@members))))

      ((list-named? ast 'access-label)
       `(exdent
         (seq (text ,(to-string (cadr ast)))
              (text ":"))))

      ((list-named? ast 'cxx-namespace)
       (baa-structure "namespace" ast))
      
      ((list-named? ast 'cxx-class)
       (baa-structure "class" ast))

      ((list-named? ast 'cxx-struct)
       (baa-structure "struct" ast))

      ((list-named? ast 'cxx-union)
       (baa-structure "union" ast))

      ((list-named-one-of? ast '(global-func cxx-ctor cxx-dtor
                                             inst-meth class-meth))
       (baa-func ast))

      ((list-named? ast 'ctor-init-list)
       (baa-ctor-init-list ast))

      ((list-named-one-of? ast '(global-var class-var
                                            inst-var local-var))
       (baa-var ast))

      ((list-named? ast 'cxx-typedef)
       (baa-typedef ast))

      ((cxx-enum? ast)
       (baa-enumeration ast))
      
      ((list-named? ast 'base)
       (let* ((type (get-reqd-nlist-memb-1 ast 'type))
	      (kind (get-reqd-nlist-memb-1 ast 'base-access)))
	 `(sep space
               (text ,(to-string kind))
               (text ,(type-to-cxx-string type)))))

      ((list-named? ast 'init)
       (list (car ast) (ast-to-snippet (cadr ast))))

      ((list-named-one-of? ast '(trap-block cxx-chunk))
       (ast-to-snippet ast))

      ((any-pred statement? expression? ast)
       (ast-to-snippet ast))

      ((list-named? ast 'together)
       `(sep nl ,@(map baa (cdr ast))))

      ((list-named? ast 'include)
       (let* ((itype (fget-reqd-nlist-elem-1 ast 'incl-kind))
              (ifile (fget-reqd-nlist-elem-1 ast 'file))
              (fmt
               (case-eq itype
                        ('system "#include <~a>")
                        ('local "#include \"~a\"")
                        (else (error "unknown include type" itype))))
              (itext (format fmt (to-string ifile))))
         `(indent0 (text ,itext))))

      ((named-list? ast)
       (baa-elems ast))

      (else ast))))

 (define baa backend-adapt-ast)
 
 ;; -----------------------------------------------------------------
 ;; Pretty-printing support

 ;; We actually do some of the hard work related to pretty printing
 ;; here already, by producing fairly low-level instructions for the
 ;; pretty printer.
   
 (define/kw (type-attr-as-string ast #:optional (attr-name 'type))
   (let* ((type (get-reqd-nlist-memb-1 ast attr-name)))
     (type-to-cxx-string type)))

 (define (baa-ctor-init-list ast)
   (define initlist (cdr ast))
   
   (define (do-elem initelem)
     (cond
	 
      ((list-named? initelem 'ctor-super)
       (let* ((type (get-reqd-nlist-elem-1 initelem 'type))
              (class-name (type-to-cxx-string type))
              (args-snippet (call-args-attr-snippet initelem)))
         (snippet-list class-name "(" args-snippet ")")))
      
      ((list-named? initelem 'ctor-var)
       (let* ((var-name (get-reqd-nlist-elem-1 initelem 'name))
              (args-snippet (call-args-attr-snippet initelem)))
         (snippet-list var-name "(" args-snippet ")")))
      
      (else
       (error "illegal ctor init element"))))
    
    (snippet-join
     (map do-elem initlist)
     ", "))

 (define (exprs-snippet expr-list)
   (snippet-join
      (map ast-to-snippet expr-list)
      ", "))

 (define (call-args-attr-snippet ast)
   (let* ((call-args (get-opt-nlist-elem-1up-e ast 'exprs)))
     (exprs-snippet call-args)))

 (define (cast-to-snippet ast cast-string)
   (let ((texpr (get-reqd-nlist-elem-1 ast 'type))
	 (expr (get-reqd-nlist-elem-1 ast 'expr)))
     (snippet-list
      "("
      cast-string
      "<"
      (type-to-cxx-string texpr)
      ">("
      (ast-to-snippet expr)
      "))")))

 (define (fmt-number value fmt)
   (cond
    ((eq? fmt 'dec) (number->string value))
    ((eq? fmt 'hex) (string-append "0x" (number->string value 16)))
    ((eq? fmt 'oct) (string-append "0" (number->string value 8)))
    (else
     (error "unsupported number format" fmt))))

 (define (get-type-tname-ref-name ast)
   (define (f type)
     (cond 
      ((list-named? type 'tname-ref)
       (get-reqd-nlist-elem-1 type 'name))
      ((const? type)
       (f (get-reqd-nlist-elem-1 type 'type)))
      (else
       #f)))

   (aand* type (get-opt-nlist-elem-1 ast 'type)
	  (f type)))

 (define (number-suffix lit)
   (let ((name (get-type-tname-ref-name lit)))
     ;; Number literals should use this known set of "canonical"
     ;; type names, lest we do not know how to print them.
     (cond
      ((eq? name 'int) "")
      ((eq? name '|long int|) "L")
      ((eq? name '|long long int|) "LL")
      (else #f))))

 (define (baa-sub-block-or-stmt ast)
   (if (list-named? ast 'block)
       (baa ast)
       `(seq (text "{") (nl)
             (indent
              ,(baa ast)
              (nl))
             (text "}"))))
 
 ;; This routine converts some AST nodes to snippets. We use the word
 ;; "snippet" to refer to a data structure that is compatible with the
 ;; pretty printer.
 ;; 
 ;; You may only pass known node types to this routine; anything that
 ;; may appear in an annotation position should not be passed.
 ;; 
 ;; xxx one thing to sort out is the printing of parenthesis; we will
 ;; probably need to be analyzing all appearing expressions as a
 ;; whole, and introduce a "parens-expr" where required and nowhere
 ;; else; some source-to-source transformation toolkits have a
 ;; specific tool for sorting out the parens, perhaps we should learn
 ;; from those.
 (define (ast-to-snippet ast)
   (cond

    ;; We presently do not support automated indentation of textual
    ;; C++ code here, and likely never will, since we are no big fans
    ;; of C++ parsers, heuristic or otherwise. We had a Ruby
    ;; implementation of a heuristic C++ indenter at one point, as I
    ;; recall, but do not want to bring it here.
    ((list-named? ast 'cxx-chunk)
     (fget-reqd-nlist-elem-1 ast 'pp-code))

    ((list-named? ast 'literal)
     (let ((tname (get-type-tname-ref-name ast))
	   (value (get-reqd-nlist-elem-1 ast 'literal-value))
	   (fmt (get-opt-nlist-elem-1 ast 'literal-format)))
       (cond
	((eq? tname 'bool)
	 `(text ,(if value "true" "false")))
	((eq? fmt 'cstr)
         `(text ,(inspect value)))
	((eq? fmt 'clist)
         `(seq (text "{")
               (sep comma ,@(map ast-to-snippet value))
               (text "}")))
	(else
         `(text
           ,(aif num-sfix (number-suffix ast)
                 (string-append (fmt-number value fmt) num-sfix)
                 (to-s value)))))))

    ((list-named? ast 'block)
     (alet stmts (fget-reqd-nlist-elem ast 'stmts)
           `(c-block nl ,@(map ast-to-snippet (cdr stmts)))))

    ((list-named? ast 'trap-block)
     (let ((expr (get-reqd-nlist-elem-1 ast 'expr))
	   (stmt (get-reqd-nlist-elem-1 ast 'stmt)))
       (snippet-list
	"TRAP(" (ast-to-snippet expr) ", "
	(ast-to-snippet stmt) ");")))

    ((list-named? ast 'local-var)
     (backend-adapt-ast ast))

    ((list-named? ast 'expr-stmt)
     (let ((expr (get-reqd-nlist-elem-1 ast 'expr)))
       (snippet-list (ast-to-snippet expr) ";")))

    ((list-named? ast 'return-stmt)
     (let ((expr (get-opt-nlist-elem-1 ast 'expr)))
       (snippet-list 
	"return"
	(if expr
	    (snippet-list " " (ast-to-snippet expr))
	    no-snippet)
	";")))

    ((list-named? ast 'break-stmt)
     '(text "break;"))

    ((list-named? ast 'continue-stmt)
     '(text "continue;"))

    ((list-named? ast 'if-stmt)
     (let* ((expr (get-reqd-nlist-elem-1 ast 'cond-expr))
	    (thens (get-reqd-nlist-elem-1 ast 'then-stmt))
	    (elses (get-opt-nlist-elem-1 ast 'else-stmt)))
       `(seq
         (text "if (")
         ,(ast-to-snippet expr)
         (text ") ")
         ;; Note that we print single statements "blocked" also.
         ,(baa-sub-block-or-stmt thens)
         ,(and elses
               `(seq (text " else ")
                     ,(baa-sub-block-or-stmt elses))))))

    ((while-stmt? ast)
     ;; (while-stmt (cond-expr EXPR) (stmt STATEMENT) ...)
     (let* ((cond-expr (get-reqd-nlist-elem-1 ast 'cond-expr))
	    (stmt (get-reqd-nlist-elem-1 ast 'stmt)))
       `(seq
         (text "while (")
         ,(ast-to-snippet cond-expr)
         (text ") ")
         ;; Note that we print single statements "blocked" also.
         ,(baa-sub-block-or-stmt stmt))))
    
    ((for-stmt? ast)
     ;; (for-stmt (init-expr EXPR) (cond-expr EXPR) (step-expr STEP-EXPR) (stmt STATEMENT) ...)
     (let* ((init-expr (get-opt-nlist-elem-1 ast 'init-expr))
	    (cond-expr (get-opt-nlist-elem-1 ast 'cond-expr))
	    (step-expr (get-opt-nlist-elem-1 ast 'step-expr))
	    (stmt (get-reqd-nlist-elem-1 ast 'stmt)))
       `(seq
         (text "for (")
         ,(and init-expr (ast-to-snippet init-expr))
         (text "; ")
         ,(and cond-expr (ast-to-snippet cond-expr))
         (text "; ")
         ,(and step-expr (ast-to-snippet step-expr))
         (text ") ")
         ;; Note that we print single statements "blocked" also.
         ,(baa-sub-block-or-stmt stmt))))
    
    ((list-named? ast 'switch-stmt)
     (let* ((expr (get-reqd-nlist-elem-1 ast 'switch-expr))
	    (stmtlist (get-opt-nlist-elem-1up-e ast 'stmts)))
       `(seq (text "switch (")
             ,(ast-to-snippet expr)
             (text ") ")
             (c-block nl
                      ,@(map ast-to-snippet stmtlist)))))

    ((list-named? ast 'switch-case)
     (let* ((expr (get-reqd-nlist-elem-1 ast 'switch-expr)))
       `(exdent
         (text "case ")
         ,(ast-to-snippet expr)
         (text ":"))))

    ((list-named? ast 'switch-default)
     `(exdent (text "default:")))

    ((list-named? ast 'name-ref)
     (let ((name (get-reqd-nlist-elem-1 ast 'name)))
       `(text ,(to-string name))))

    ((list-named? ast 'call-expr)
     (begin
       ;;(write-nl ast)
       (let* ((target-expr (fget-reqd-nlist-elem-1 ast 'target))
	      (target-snippet (ast-to-snippet target-expr))
	      (args-snippet (call-args-attr-snippet ast)))
	 (snippet-list target-snippet "(" args-snippet ")"))))

    ((list-named? ast 'ctor-call-expr)
     (snippet-list
      (type-attr-as-string ast)
      "("
      (call-args-attr-snippet ast)
      ")"))

    ((list-named? ast 'new-expr)
     (begin
       ;;(write-nl ast)
       (snippet-join
	(list "new"
	      (if (has-true-bool-prop-memb? ast 'leaving) "(ELeave)" no-snippet)
	      (snippet-list
	       (type-attr-as-string ast 'type-expr)
	       "("
	       (call-args-attr-snippet ast)
	       ")")
	      )
	" ")))

    ((list-named-one-of? ast '(dot-expr arrow-expr))
     (let* ((expr1 (fget-reqd-nlist-elem-1 ast 'expr1))
	    (expr2 (fget-reqd-nlist-elem-1 ast 'expr2))
	    (sep (if (eq? (car ast) 'dot-expr) "." "->")))
       (snippet-list
	(ast-to-snippet expr1)
	sep
	(ast-to-snippet expr2))))

    ((list-named? ast 'delete-expr)
     (pretty-pre-unary-expr ast "delete "))

    ((list-named? ast 'delete-array-expr)
     (pretty-pre-unary-expr ast "delete[] "))

    ((list-named? ast 'index-expr)
     (snippet-list
      (ast-to-snippet (get-reqd-nlist-elem-1 ast 'expr))
      "["
      (ast-to-snippet (get-reqd-nlist-elem-1 ast 'index))
      "]"))

    ((list-named? ast 'sizeof-expr)
     (snippet-list "sizeof(" (type-attr-as-string ast 'type-expr) ")"))

    ((list-named? ast 'texpr-expr)
     `(text ,(type-attr-as-string ast 'type-expr)))

    ;; casts...

    ((list-named? ast 'static-cast-expr)
     (cast-to-snippet ast "static_cast"))

    ((list-named? ast 'reinterpret-cast-expr)
     (cast-to-snippet ast "reinterpret_cast"))

    ((list-named? ast 'const-cast-expr)
     (cast-to-snippet ast "const_cast"))

    ((list-named? ast 'dynamic-cast-expr)
     (cast-to-snippet ast "dynamic_cast"))

    ;; unary operators...

    ((list-named? ast 'addr-of-expr)
     (pretty-pre-unary-expr ast "&"))
    
    ((list-named? ast 'deref-expr)
     (pretty-pre-unary-expr ast "*"))
    
    ((list-named? ast 'uminus-expr)
     (pretty-pre-unary-expr ast "-"))
    
    ((list-named? ast 'uplus-expr)
     (pretty-pre-unary-expr ast "+"))
    
    ((list-named? ast 'not-expr)
     (pretty-pre-unary-expr ast "!"))
    
    ((list-named? ast 'neg-expr)
     (pretty-pre-unary-expr ast "~"))
    
    ((list-named? ast 'pre-inc-expr)
     (pretty-pre-unary-expr ast "++"))
    
    ((list-named? ast 'pre-dec-expr)
     (pretty-pre-unary-expr ast "--"))
   
    ((list-named? ast 'post-inc-expr)
     (pretty-post-unary-expr ast "++"))
   
    ((list-named? ast 'post-dec-expr)
     (pretty-post-unary-expr ast "--"))

    ;; binary operators...

    ((list-named? ast 'eq-expr)
     (pretty-binary-expr ast "=="))

    ((list-named? ast 'neq-expr)
     (pretty-binary-expr ast "!="))

    ((list-named? ast 'lt-expr)
     (pretty-binary-expr ast "<"))

    ((list-named? ast 'gt-expr)
     (pretty-binary-expr ast ">"))

    ((list-named? ast 'lte-expr)
     (pretty-binary-expr ast "<="))

    ((list-named? ast 'gte-expr)
     (pretty-binary-expr ast ">="))

    ((list-named? ast 'add-expr)
     (pretty-binary-expr ast "+"))

    ((list-named? ast 'sub-expr)
     (pretty-binary-expr ast "-"))

    ((list-named? ast 'mul-expr)
     (pretty-multi-expr ast "*"))

    ((list-named? ast 'div-expr)
     (pretty-multi-expr ast "/"))

    ((list-named? ast 'mod-expr)
     (pretty-binary-expr ast "%"))

    ((list-named? ast 'or-expr)
     (pretty-multi-expr ast "||"))

    ((list-named? ast 'and-expr)
     (pretty-multi-expr ast "&&"))

    ((list-named? ast 'lshift-expr)
     (pretty-binary-expr ast "<<"))

    ((list-named? ast 'rshift-expr)
     (pretty-binary-expr ast ">>"))

    ((list-named? ast 'xor-expr)
     (pretty-multi-expr ast "^"))

    ((list-named? ast 'bit-or-expr)
     (pretty-multi-expr ast "|"))

    ((list-named? ast 'bit-and-expr)
     (pretty-multi-expr ast "&"))

    ;; assignments...

    ((list-named? ast 'assign-expr)
     (pretty-binary-expr ast "="))

    ((list-named? ast 'add-assign-expr)
     (pretty-binary-expr ast "+="))

    ((list-named? ast 'sub-assign-expr)
     (pretty-binary-expr ast "-="))

    ((list-named? ast 'mul-assign-expr)
     (pretty-binary-expr ast "*="))

    ((list-named? ast 'div-assign-expr)
     (pretty-binary-expr ast "/="))

    ((list-named? ast 'mod-assign-expr)
     (pretty-binary-expr ast "%="))

    ((list-named? ast 'lshift-assign-expr)
     (pretty-binary-expr ast "<<="))

    ((list-named? ast 'rshift-assign-expr)
     (pretty-binary-expr ast ">>="))

    ((list-named? ast 'xor-assign-expr)
     (pretty-binary-expr ast "^="))

    ((list-named? ast 'bit-or-assign-expr)
     (pretty-binary-expr ast "|="))

    ((list-named? ast 'bit-and-assign-expr)
     (pretty-binary-expr ast "&="))

    ;; trinary operators...

    ((if-expr? ast)
     (let ((cond-expr (fget-reqd-nlist-elem-1 ast 'cond-expr))
           (then-expr (fget-reqd-nlist-elem-1 ast 'then-expr))
           (else-expr (fget-reqd-nlist-elem-1 ast 'else-expr)))
       (snippet-list "("
                     (ast-to-snippet cond-expr) " ? "
                     (ast-to-snippet then-expr) " : "
                     (ast-to-snippet else-expr)
                     ")")))

    (else 
     (error "backend adapt: unsupported" ast))))

 (define (pretty-pre-unary-expr ast op-string)
   (let ((expr (get-reqd-nlist-elem-1 ast 'expr)))
     (snippet-list "(" op-string (ast-to-snippet expr) ")")))

 (define (pretty-post-unary-expr ast op-string)
   (let ((expr (get-reqd-nlist-elem-1 ast 'expr)))
     (snippet-list "(" (ast-to-snippet expr) op-string ")")))

 (define (pretty-binary-expr ast op-string)
   (let ((expr1 (get-reqd-nlist-elem-1 ast 'expr1))
	 (expr2 (get-reqd-nlist-elem-1 ast 'expr2)))
     (snippet-list "(" (ast-to-snippet expr1) " " op-string " "
		   (ast-to-snippet expr2) ")")))

 (define (pretty-multi-expr ast op-string)
   (let ((exprs (get-reqd-nlist-elem-1up ast 'exprs)))
     (snippet-list 
      "("
      (snippet-join (map ast-to-snippet exprs)
		    (string-append " " op-string " "))
      ")")))

) ; end module
