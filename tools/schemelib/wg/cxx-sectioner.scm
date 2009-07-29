;; 
;; cxx-sectioner.scm
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
 cxx-sectioner
 mzscheme

 (require (lib "ast-util.scm" "wg"))
 (require (lib "ast-spec.scm" "wg"))
 (require (lib "prop-util.scm" "wg"))
 (require (lib "compact.scm" "wg"))
 (require (lib "documentation.scm" "wg"))
 (require (lib "op.scm" "wg"))
 (require (lib "type-constness.scm" "wg"))
 (require (lib "usual.scm" "wg"))

 ;; "iisect" is either :iface or :impl (interface or implementation).
 ;; "visib" is either :vext or :vint or :both (visible externally,
 ;; internally, or both).
 ;; 
 ;; :h == '(iface vext). :inl == '(impl vext). :cpph == '(iface vint).
 ;; :cppb == '(impl vint). If there is no separate .inl file, the .h
 ;; file contains '(iface vext) and '(impl vext). Or, if nothing is to
 ;; be exported, the declaration section of the .cpp file is '(iface
 ;; both), and the implementation section is '(impl both).
 ;; 
 ;; "no-external" indicates that we should pretend that nothing is
 ;; externally defined, meaning that we will be including a
 ;; declaration for it, meaning that no header file needs including
 ;; (or available). This might sometimes be useful.
 (define/kw* (ast-section ast^ iisect visib #:key no-external)
   (unless (and (memq iisect '(iface impl))
                (memq visib '(vext vint both)))
     (error "illegal arguments" (list iisect visib)))

   (letrec
       (
	(any-iface-section (eq? iisect 'iface))
	
	;(all-iface-section (and any-iface-section (eq? visib 'both)))

	(trans-if-nlist
	 (lambda (ast)
	   (if (not (named-list? ast))
	       ast
	       (trans-named-list ast))))

	(trans-elems
	 (lambda (ast)
	   (map-elems trans-if-nlist ast)))
	
	(trans-unit
	 (lambda (ast)
	   (map-elem-lists-named trans-elems ast 'body)))

	;; Sets "name" attr value with "qname" attr value.
	(give-qname
	 (lambda (ast)
	   (let ((qname (get-reqd-nlist-elem-1 ast 'qname)))
	     (set-prop-memb ast `(name ,qname)))))

	(get-access
	 (lambda (ast)
	   (get-opt-nlist-elem-1 ast 'access)))

	(add-access-labels-to-body
	 (lambda (ast)
	   (let ((body '(body))
		 (members (cdr ast))
		 (last-access #f))
	     (for-each
	      (lambda (member)
                (when (declaration? member)
                  (let ((access (get-access member)))
                    (when (not (eq? access last-access))
                      (push-set! body `(access-label ,access))
                      (set! last-access access))))
                (push-set! body member))
	      members)
	     body)))

	(add-access-labels-to-class
	 (lambda (ast)
	   (map-elem-lists-named add-access-labels-to-body ast 'body)))

	;; This is what we want for external functions at least; if
	;; they have no header, we want to IMPORT_C them. And there
	;; should be no problem with locally defined, non-exported ones
	;; either; we'll get both IMPORT_C and EXPORT_C in the same
	;; .cpp file, but the semantics is the same as importing the
	;; generated .h file that has the IMPORT_C, and that's done all
	;; the time.
	(fix-dll-export ; export-to-import
	 (lambda (ast new-sym)
	   (if (has-true-bool-prop-memb? ast 'dll-export)
	       (push (reject-prop-membs ast 'dll-export)
		     (list new-sym #t))
	       ast)))

        ;; The implementation of a method cannot have qualifiers such
        ;; as "static", and this function removes qualifiers as
        ;; appropriate.
	(remove-qual
	 (lambda (ast)
	   (reject-prop-membs-by-names ast '(static virtual))))

	(trans-named-list
	 (lambda (ast)
	   (let* 
	       (
                (is-decl (declaration? ast))
                
                ;; Whether externally defined, and any header file is
                ;; to be used. In this case neither an interface
                ;; declaration or an implementation needs emitting.
		(is-external (has-true-bool-prop-memb? ast 'external))

		;; Whether the definition is to show up in a public
		;; header file.
		(is-export (has-true-bool-prop-memb? ast 'export))

		;; Whether the definition is for an inlined function.
		(is-inline (has-true-bool-prop-memb? ast 'inline))
		)
             ;;(pretty-nl ast)
             
	     ;; We do not want documentation both in the interface and
	     ;; implementation section, as that quickly gets annoying
	     ;; with long doc strings. We choose to document the
	     ;; interface definition (only), as that should be the
	     ;; better choice for API doc generators.
	     (if (and (eq? iisect 'impl)
		      (can-be-doc-owner? ast))
                 ;; Just flagging documentation as forbidden should be
                 ;; faster than trying to delete all attributes that
                 ;; might cause documentation to be generated.
		 (set! ast (mark-doc-forbidden ast)))
	     
	     (cond
              ;; The export attribute determines which interface
              ;; section to use for a definition, the external or the
              ;; internal one, and the other interface section gets
              ;; nothing.
	      ((and is-decl
                    (eq? iisect 'iface) 
                    (or (and is-export (eq? visib 'vint))
                        (and (not is-export) (eq? visib 'vext))))
               (begin
                 ;;(write-nl (list 'discard-1 iisect visib ast))
                 'nocode))

              ;; The export attribute determines which implementation
              ;; section to use for a definition, and the other header
              ;; section gets nothing. Note that only inlines go into
              ;; an external implementation section, since normally
              ;; implementations are always internal. Note, however,
              ;; that the inlines could be within a class, and the
              ;; containing class may not be discarded here.
	      ((and is-decl
                    (eq? iisect 'impl)

                    (if (func-like-decl? ast)
                        ;; Function-line things are somewhat special,
                        ;; as their implementation goes into the
                        ;; implementation section, regardless of
                        ;; whether internal or external. The only
                        ;; exception is exported inlines.
                        (or (and (eq? visib 'vext)
                                 (not (and is-inline is-export)))
                            (and (eq? visib 'vint)
                                 is-inline
                                 is-export))

                        ;; Other declarations are only affected by
                        ;; "export", as they may not be marked
                        ;; "inline". Non-exported declarations never
                        ;; belong to the implementation section, but
                        ;; some members of exported declarations might
                        ;; have an internal implementation, and hence
                        ;; we cannot just ignore all exported decls
                        ;; here.
                        (or (and (eq? visib 'vext) (not is-export))
                            ;;(and (eq? visib 'vint) is-export)
                            )))
               (begin
                 ;;(write-nl (list 'discard-2 iisect visib ast))
                 'nocode))

	      ;; For any externally defined construct, we need nothing
	      ;; but an include, if an include is available.
	      ((and is-decl
                    is-external
		    (not no-external))
	       'nocode)

	      ((list-named? ast 'cxx-chunk)
	       (let ((this-ctx (fget-opt-nlist-elem-1 ast 'context)))
                 ;;(write-nl (list ast iisect visib this-ctx))
                 (if (not this-ctx)
                     ast
                     (let ((this-iisect (first this-ctx))
                           (this-visib (second this-ctx)))
                       (if (and (or (eq? this-iisect '_)
                                    (eq? this-iisect iisect))
                                (or (eq? visib 'both)
                                    (eq? this-visib '_)
                                    (eq? this-visib visib)))
                           ast
                           'nocode)))))

              ((list-named-one-of? ast '(cxx-typedef cxx-enum))
               (if any-iface-section
                   ast
                   'nocode))
              
	      ((list-named-one-of? ast '(cxx-namespace cxx-class cxx-struct cxx-union))
	       (if any-iface-section

		   ;; Interface section. The declaration shall remain,
		   ;; but contents may be modified. It may also be
		   ;; necessary to add access labels.
		   (begin
		     (set! ast (trans-elems ast))
		     (if (eq? (car ast) 'cxx-class)
			 (set! ast (add-access-labels-to-class ast)))
		     ;;(write-nl ast)
		     ast)

		   ;; Body section. The declaration will not be
		   ;; retained, but some of its contents might, with
		   ;; some modifications.
		   (let ((body (get-opt-nlist-memb ast 'body)))
		     (if body
			 (sc-list (map trans-if-nlist (cdr body)))
			 'nocode))))

	      ;; For purposes of documentation, we do not want any arguments
	      ;; marked "unused" in the interface declaration of a function.
	      ((list-named? ast 'arg)
	       (if any-iface-section
		   (reject-prop-membs ast 'unused)
		   ast))

	      ((list-named-one-of? ast '(global-func class-meth inst-meth cxx-ctor cxx-dtor))
	       (if any-iface-section

                   ;; Functions and similar should not have a body in
                   ;; the interface section. We also do not want any
                   ;; ctor initializer.
		   (begin 
		     (set! ast (reject-nlist-elems-by-names ast '(block ctor-init-list)))

		     (trans-elems (fix-dll-export ast 'import-c)))

                   ;; Should have a fully qualified name in the
                   ;; implementation section.
		   (cond
                    ;; Abstract functions have no implementation.
                    ;; Externally defined ones may have, but not one
                    ;; that is known to us.
		    ((has-true-bool-prop-memb-one-of? ast '(pure external))
		     'nocode)

		    ;; Non-abstract ones should have an implementation.
		    ((not (get-opt-nlist-elem ast 'block))
		     (error "non-abstract function has no body" ast))

		    (else
		     (begin
                       ;;(pretty-nl ast)
		       (unless (eq? (car ast) 'global-func)
                         ;; Method implementations may not have
                         ;; certain qualifiers, due to C++ syntax
                         ;; weirdness, so remove those.
                         (set! ast (remove-qual ast)))
		       (set! ast (fix-dll-export ast 'export-c))
		       (set! ast (give-qname ast))
		       (trans-elems ast))))))

	      ;; On Symbian, exported non-@const@ global or class
	      ;; variables make little sense, as including a header
	      ;; defining such a variable would introduce static
	      ;; writable data. But of course, we do not want to be
	      ;; Symbian-specific here, and probably in more recent
	      ;; Symbian releases (or older EXEs) static writable data
	      ;; is okay anyway.
	      ((list-named-one-of? ast '(global-var inst-var class-var))
	       (let* ((node-name (car ast))
		      (type (get-reqd-nlist-memb ast 'type))
		      (is-const (type-is-const? type))
		      (init (get-opt-nlist-memb ast 'init))
		      (exprs (get-opt-nlist-memb ast 'exprs))
		      (has-init (or init exprs)))

		 (if (and (eq? node-name 'inst-var) has-init)
		     (error "instance variable cannot have an initializer" ast))
		 
		 (if any-iface-section
		     ;; declaration
		     (if is-const
                         ;; Constants initialized as soon as declared,
                         ;; assuming they have an initializer. (It is
                         ;; possible to "extern" a const variable
                         ;; without an initializer, so if it appears
                         ;; forward declarations of "const" variables
                         ;; are ever necessary, we can try that.)
                         ;; Applies to class variables also.
			 ast
			 ;; No initializer for non-constant
			 ;; declarations in the interface section; the
			 ;; initializer shall go into the body section.
			 (if has-init
			     (reject-nlist-elems-by-names ast '(init exprs))
			     ast))
		     ;; implementation
		     (if (or 
			  ;; Constants fully declared already.
			  is-const
                          ;; Anything without an initializer has been
                          ;; sufficiently declared already.
			  (and (not has-init)))
			 'nocode
			 (begin
			   ;; Need a fully qualified name in the
			   ;; implementation section.
			   (give-qname ast))))))

	      ((list-named? ast 'type)
	       ast)

	      (else (trans-elems ast))))))
	)
     (trans-unit ast^)))

) ; end module

#;
(begin
  ;; Tests.
  (require cxx-sectioner)
  (require (lib "util.scm" "wg"))
  (require (lib "compact.scm" "wg"))

  (define ast-1
    '(cxx-unit
      (body
       (global-var (qname gVar) (name gVar) (type (tname-ref char)) (init 1) (doc "first variable"))
       (global-var (qname gVarNoInit) (name gVarNoInit) (type (tname-ref char)) (doc "second variable"))
       (global-var (qname gConstVar) (name gConstVar) (type (const (type (tname-ref int)))) (init 2) (cxx-doc "third variable"))
       (cxx-class 
	(qname Class) (name Class)
	(body
	 (class-var (qname Class::iClassVar) (name iClassVar) static (type (tname-ref int)))
	 ))
       (global-func
         (qname ExportedFunc) (name ExportedFunc)
         (export #t)
         (block))
       (global-func
         (qname ExportedInlineFunc) (name ExportedInlineFunc)
         (export #t) (inline #t)
         (block))
       )))

  (define sect-list
    ;'((iface both) (impl both))
    '((impl vext))
    ;'((iface vext))
    ;'((iface vint))
    ;'((iface both))
    ;'((impl vint))
    )

  (for-each
   (lambda (sect)
     ;; can use pretty-nl-c also, but this can be useful
     (pretty-nl (ast-section ast-1 (car sect) (cadr sect))))
   sect-list))
