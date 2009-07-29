;; 
;; pass-backend-adapt.scm
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

;; Contains passes to adapt the AST for consumption by the C++
;; backend.
(module
 pass-backend-adapt
 mzscheme

 (require "ast-util.scm")
 (require (lib "prop-util.scm" "wg"))
 (require "compact.scm")
 (require "documentation.scm")
 (require "doxygen-cxx.scm")
 (require "emission.scm")
 (require "op.scm")
 (require "mop.scm")
 (require "snippets.scm")
 (require "tables.scm")
 (require "type-ast-simplify.scm")
 (require "type-dcl-form.scm")
 (require "usual.scm")

 (define (trans-matching-nlist-elems
	  ast
	  parent-first?
	  match?
	  trans-func)
   (define (f ast)
     (if (named-list? ast)
	 (if (match? ast)
	     (if parent-first?
		 (map-elems f (trans-func ast))
		 (trans-func (map-elems f ast)))
	     (map-elems f ast))
	 ast))
   (f ast))

 ;; Removes all elements for which the "no-emit" Boolean attribute has
 ;; been set to true. This is used say for declarations that do appear
 ;; in the AST for name resolution purposes, but which are not
 ;; supposed to get emitted as the resulting program gets printed.
 (define* (pass-no-emit ast^ tables)
   (letrec
       ((matches?
	 (lambda (ast)
	   (has-true-bool-prop-memb? (cdr ast) 'no-emit)))
	
	(trans
	 (lambda (ast)
	   (if (named-list? ast)
	       (if (matches? ast)
		   'nocode
		   (map-elems trans ast))
	       ast))))

   (list (deep-compact (trans ast^)) tables)))

 ;; Names constructors and destructors (etc.) with proper C++ names.
 (define* (pass-cxx-names ast^ tables)
   (letrec
       ((trav
	 (lambda (names ast)
	   (cond

	    ;; Not a named declaration, but may contain some.
	    ;; Is nameless.
	    ((list-named-one-of? ast '(cxx-unit body))
	     (map-elems (fix trav names) ast))
	    
	    ;; Is a named declaration, and may contain others.
	    ((list-named-one-of? ast '(cxx-namespace cxx-class cxx-struct cxx-union))
	     (let ((name (fget-reqd-nlist-elem-1 ast 'name)))
	       (map-elems (fix trav (push names name)) ast)))

            ;; Is a named declaration requiring renaming.
	    ((list-named-one-of? ast '(cxx-ctor cxx-dtor))
             ;; We use a name derived from the name of the surrounding
             ;; named container.
	     (let* ((pname (car (last-pair names)))
		    (name (if (eq? (car ast) 'cxx-ctor)
			      pname
			      (string->symbol
			       (string-append
				"~"
				(symbol->string pname))))))
	       (set-prop-memb ast `(name ,name))))

            ;; Anything else is neither an interesting declaration,
            ;; nor contains any.
	    (else ast)))))
     
     (list (trav '() ast^) tables)))

 ;; Assigns qualified names to declarations. This naturally does not
 ;; apply to any local declarations, since they do not have qualified
 ;; names.
 (define* (pass-qname ast^ tables)
   (letrec
       ((give-qname
	 (lambda (names ast)
	   (let ((qname (make-qname names)))
             (set-nlist-elem-1! ast 'qname qname)
             (set-nlist-elem-1! ast 'qname-comps names)
             ast)))

	(trav
	 (lambda (names ast)
	   (cond

	    ;; Not a named declaration, but may contain some.
	    ;; Is nameless.
	    ((list-named-one-of? ast '(cxx-unit body))
	     (map-elems (fix trav names) ast))
	    
	    ;; Is a named declaration, and may contain others.
	    ((list-named-one-of? ast '(cxx-namespace cxx-class cxx-struct cxx-union))
	     (let* ((name (fget-reqd-nlist-elem-1 ast 'name))
		    (new-names (push names name)))
	       (give-qname 
		new-names
		(map-elems (fix trav new-names) ast))))

            ;; Is a named declaration, but does not contain others. 
	    ((list-named-one-of? ast '(cxx-typedef global-var inst-var class-var global-func class-meth inst-meth cxx-ctor cxx-dtor))
	     (let* ((name (fget-reqd-nlist-elem-1 ast 'name))
		    (new-names (push names name)))
	       (give-qname new-names ast)))

            ;; Anything else is neither an interesting declaration,
            ;; nor contains any.
	    (else ast)))))
     
     (list (trav '() ast^) tables)))

 (define* pass-doxygen
   (carry-tables
    (lambda (ast)
      (trans-matching-nlist-elems
       ast
       #t
       can-be-doc-owner?
       add-doxy-doc))))

 (define* (pass-adapt ast^ tables)
   (list (backend-adapt-ast ast^) tables))

 ;; The backend no longer supports non-compacted programs fully, so we
 ;; better do full compaction before passing over the program, which
 ;; is what this pass does.
 (define* (pass-compact ast^ tables)
   (list (compact-flatten ast^) tables))

 ) ; end module
