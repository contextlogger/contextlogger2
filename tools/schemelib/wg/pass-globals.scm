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
 pass-globals
 mzscheme 

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "tables.scm" "wg"))
 (require (lib "mop.scm" "wg"))
 (require (prefix e. (lib "environment.scm" "wg")))

 ;; This pass finds all the globals in an AST, and adds them into a
 ;; name table of global names.
 ;; 
 ;; The global namespace is not something consisting simply of
 ;; top-level items, no, because say static methods of classes (that
 ;; is, contained items) can also be referred to using fully qualified
 ;; names. This means that we require two separate name tables; one
 ;; containing qualified names, and another one containing local,
 ;; unqualified names. Global variables go to the table of qualified
 ;; names (in qualified form).
 ;; 
 ;; When it comes to lookups, any fully qualified name (any name
 ;; beginning with "::") should only be looked up from the global name
 ;; table (in qualified form), and everything else is first looked up
 ;; from the chain of local environments, and only then from the
 ;; top-level environment (i.e., the globals).
 ;; 
 ;; We build the global environment by doing a depth-first traversal,
 ;; which is simple to do, but we do have to keep track of the path
 ;; traversed to be able to form qualified names.
 ;; 
 ;; This function returns a possibly modified version of g-table, but
 ;; ast^ and d-table are returned as is.
 (define* (pass-table-globals ast^ tables)
   (define g-table (get-g-table tables))

   (letrec 
       ((add-to-table-and-do
	 (lambda (names ast action . kw)
	   (let* ((name (get-reqd-nlist-elem-1 ast 'name))
		  (id (get-reqd-nlist-elem-1 ast 'decl-id))
		  (newnames (push names name))
		  (qname (make-qname newnames)))
	     (set! g-table (apply e.add-binding g-table qname id kw))
	     (action newnames))))

	(trav
	 (lambda (names ast)
	   (cond

	    ;; Not itself a global declaration, but may contain some.
	    ;; Is nameless.
	    ((list-named-one-of? ast '(cxx-unit body))
	     (for-each-elem (fix trav names) ast))
	    
	    ;; Not itself a global declaration, but may contain some.
	    ;; Has a name.
	    ((list-named? ast 'cxx-namespace)
	     (let ((name (get-reqd-nlist-memb-1 ast 'name)))
	       (for-each-elem (fix trav (push names name)) ast)))

	    ;; Is a global declaration, and may contain others.
	    ((list-named-one-of? ast '(cxx-class cxx-struct cxx-union))
	     (add-to-table-and-do
	      names ast
	      (lambda (newnames)
		(for-each-elem (fix trav newnames) ast))))

            ;; Is a global declaration, but does not contain others.
            ;; 
            ;; "inst-meth" is an exception here; in some sense it is
            ;; not global, as instance methods cannot be called in
            ;; just any context, but subclasses that want to
            ;; explicitly call the superclass method of a particular
            ;; ancestor should be able to resolve these as if they
            ;; were globals.
	    ((list-named-one-of? ast '(cxx-typedef global-var class-var))
	     (add-to-table-and-do names ast do-nothing))
	    ((list-named-one-of? ast '(global-func class-meth inst-meth))
	     (add-to-table-and-do names ast do-nothing #:is-func #t))

            ;; Anything else is neither a global declaration, nor
            ;; contains any.
	    ))))

     (trav '() ast^)
     (list ast^ (set-g-table tables g-table))))

) ;; end module
