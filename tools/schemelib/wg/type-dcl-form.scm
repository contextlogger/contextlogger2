;; 
;; type-dcl-form.scm
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

;; DECLARATOR GRAMMAR
;;
;; An ADT for declarators (sketched in Haskell syntax) could be:
;; 
;;   data DclAny =
;;       DclConst DclAny |
;;       DclVolatile DclAny |
;;       DclPtrTo DclAny |
;;       DclRefTo DclAny |
;;       DclFunc DclAny DclArgs |
;;       DclArrayOf DclAny ArrayIndex |
;;       DclName Name |
;;       DclNil
;;   type DclArgs = [DclTypeSpec]
;;   type TypeSpec = (TypeName, DclAny)

;; ON VARIANT ORDERING
;;
;; Note that "char const*" is the same as "const char*", but "char*
;; const" has a different meaning; with "const" as a part of the
;; declarator (only), it's possible to declare "char const*" and
;; "char* const", but not "const char*", and this is good enough since
;; all the different meanings can be covered; naturally we can also
;; write "char const* const".
;; 
;; GCC-XML naturally supports variant orderings, but we do our
;; printing strictly in declarator-based order. Essentially, we get a
;; canonical form for free.

(module
 type-dcl-form mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "op.scm" "wg"))
 (require (lib "plt-match.ss"))

 ;; Transforms a type specifier into declarator form.
 ;;
 ;; Examples:
 ;;   (tname-ref int) -> (int nil) i.e. "int"
 ;;   (const (tname-ref int)) -> (int (const nil)) i.e. "int const"
 ;;   (ref-to (ptr-to (tname-ref int))) -> (int (ptr-to (ref-to nil))) i.e. "int *&"
 ;;   (array-of (ptr-to (tname-ref int))) -> (int (ptr-to (array-of nil))) i.e. "int *[]"
 ;;   (ptr-to (func ((tname-ref int)) (tname-ref void))) -> (void (func (ptr-to nil) ((int nil)))) i.e. "void (*)(int)"
 ;;   (ptr-to (func () (ref-to (tname-ref int)))) -> (int (ref-to (func (ptr-to nil) ()))) i.e. "int &(*)()"
 (define* (type-to-dcl-form ast)
   (letrec
       ((type-name #f)

	(ast-error
	 (lambda (ast)
	   (error "illegal type specifier" ast)))

	(to-dcl
	 (lambda (ast sub-dcl)
	   (match 
	    ast

	    ('none
	     sub-dcl)

	    ((list 'tname-ref name)
	     (begin
	       (set! type-name name)
	       sub-dcl))

	    ((list 'const texpr)
	     (to-dcl texpr `(dcl-const ,sub-dcl)))

	    ((list 'volatile texpr)
	     (to-dcl texpr `(dcl-volatile ,sub-dcl)))

	    ((list 'ptr-to texpr)
	     (to-dcl texpr `(dcl-ptr-to ,sub-dcl)))

	    ((list 'ref-to texpr)
	     (to-dcl texpr `(dcl-ref-to ,sub-dcl)))
	    
	    ((list 'array-of texpr)
	     (to-dcl texpr `(dcl-array-of ,sub-dcl)))
	    
	    ((list 'array-of texpr index)
	     (to-dcl texpr `(dcl-array-of ,sub-dcl ,index)))
	    
	    ((list 'func-t args returns)
	     (to-dcl 
	      returns 
	      `(dcl-func ,sub-dcl ,(map type-to-dcl-form args))))

	    (x (ast-error ast)))))

	(type-dcl (to-dcl ast 'dcl-nil)))

     ;; Dtor and ctor are function types without any return type, and
     ;; thus they do not get a type-name component.
     ;;
     ;;(unless type-name
     ;;(ast-error ast))

     (list type-name type-dcl)))

 ;; Replaces either a dcl-nil or a dcl-name in a declarator with the
 ;; specified declarator, which would typically be a name declarator,
 ;; but not necessarily.
 ;; type-dcl:: The declarator in which to do the replacement.
 ;; r-dcl:: The replacement declarator.
 (define (set-dcl-nil type-dcl r-dcl)
   (letrec
       ((ast-error
	 (lambda (ast)
	   (error "illegal declarator" ast)))

	(repl-sub-dcl 
	 (lambda (ast) (modify-cadr ast repl)))

	(repl
	 (match-lambda

	  ('dcl-nil 
	   r-dcl)

	  ((list 'dcl-name old-name)
	   r-dcl)

	  (ast
	   (cond

	    ((list-named-one-of? ast '(dcl-const dcl-volatile dcl-ptr-to dcl-ref-to dcl-array-of dcl-func))
	     (repl-sub-dcl ast))

	    (ast (ast-error ast)))))))

     (repl type-dcl)))

 ;; Sets a name to a type specifier.
 (define* (type-spec-set-name type-spec name)
   (list
    (first type-spec)
    (set-dcl-nil (second type-spec) `(dcl-name ,name))))

 (define (dcl-name-holder? ast)
   (or (eq? ast 'dcl-nil)
       (list-named? ast 'dcl-name)))

 ;; Applies function f to the first dcl-func encountered in ast such
 ;; that the dcl-func has a child of type dcl-nil or dcl-name. The
 ;; element is substituted with the result of the function
 ;; application.
 (define (dcl-modify-inner-func ast f)
   (letrec
       ((trans-sub-dcl 
	 (lambda (ast) (modify-cadr ast trans)))

	(trans
	 (lambda (ast)
	   (cond
	    
	    ((dcl-name-holder? ast)
	     (error "was excepting function" ast))
	    
	    ((list-named-one-of? ast '(dcl-const dcl-volatile dcl-ptr-to dcl-ref-to dcl-array-of))
	     (trans-sub-dcl ast))

	    ((list-named? ast 'dcl-func)
	     (let ((sub-dcl (cadr ast)))
	       (if (dcl-name-holder? sub-dcl)
		   (f ast)
		   (trans-sub-dcl ast))))
	    
	    (else
	     (error "illegal declarator" ast))))))

     (trans ast)))

 (define* (dcl-texpr-modify-inner-func ast f)
   (modify-cadr 
    ast
    (lambda (dcl)
      (dcl-modify-inner-func dcl f))))

 ;; "ctos" is a function for converting a constant expression to a string.
 (define* (type-spec-to-cxx ast^ ctos)
   (letrec
       ((do-dcl
	 (lambda (ast)
	   (match 
	    ast
	    ('dcl-nil "")
	    (`(dcl-name ,name) (do-sym name))
	    (`(dcl-const ,d) (string-append "const " (do-dcl d)))
	    (`(dcl-volatile ,d) (string-append "volatile " (do-dcl d)))
	    (`(dcl-ptr-to ,d) (string-append "*" (do-dcl d)))
	    (`(dcl-ref-to ,d) (string-append "&" (do-dcl d)))
	    (`(dcl-array-of ,d) (string-append (do-dcl d) "[]"))
	    (`(dcl-array-of ,d ,i) (string-append (do-dcl d) "[" (ctos i) "]"))
	    (`(dcl-func ,d ,args) 
	     (string-append 
              ;; Note the special case here; if a function
              ;; subdeclarator is a name declarator, it is okay to
              ;; leave out the parentheses, and indeed, that is
              ;; typically done. It is okay, though, to write "int
              ;; (f)(int a) { return a; }".
	      (if (list-named? d 'dcl-name)
		  (do-dcl d)
		  (string-append "(" (do-dcl d) ")"))
	      "(" (do-list args) ")"))
	    )))

	(do-sym
	 (lambda (sym)
	   (to-string sym))) ; xxx some fundamental names we must map

	(do-type
	 (match-lambda
	  ((list #f d)
	   (do-dcl d))
	  ((list n 'dcl-nil)
	   (do-sym n))
	  ((list n d)
	   (string-append (do-sym n) " " (do-dcl d)))
	  (ast (ast-error ast))))

	(ast-error
	 (lambda (ast)
	   (error "illegal declarator form" ast)))

	(do-list 
	 (lambda (types)
	   (string-join (map do-type types) ", "))))

     (do-type ast^)))

)

#;
(begin
  ;; Test code.
  (require (lib "util.scm" "wg"))
  (require type-dcl-form)

  (define ast-list
    '(
      (tname-ref int)
      (const (tname-ref int))
      (ref-to (ptr-to (tname-ref int)))
      (array-of (ptr-to (tname-ref int)))
      (ptr-to (func-t ((tname-ref int)) (tname-ref void)))
      (ptr-to (func-t () (ref-to (tname-ref int))))
      ))

  (for-each
   (lambda (ast)
     (let* ((dform (type-to-dcl-form ast))
	    (named (type-spec-set-name dform 'name))
	    (cxxs (type-spec-to-cxx named to-s)))
       (pretty-nl (list ast dform named cxxs))))
   ast-list))

; Examples:
;
; 	(rw-type
; 	 (lambda (ast)
; 	   (let ((dform (type-to-dcl-form (simplify-texpr ast))))
; 	     (type-spec-to-cxx dform to-s))))
;
; 	(rewrite-types
; 	 (lambda (ast)
;            ;; The attribute "type" always has the same semantics, so
;            ;; this is quite safe.
; 	   (map-memb-lists-named-r rw-type ast 'type)))
