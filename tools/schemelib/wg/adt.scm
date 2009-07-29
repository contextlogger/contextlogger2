;; 
;; adt.scm
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

;; An algebraic datatype implementation of sorts, in that we may
;; define datatypes, each of which may have multiple constructors/tags
;; and associated "arguments". (We also support non-tagged values,
;; since we use predicates to check for datatype membership, but...)
;; 
;; A classic example:
;; 
;;   (define-data
;;     (new-adt named tree
;;       (new-cdt (named empty def-empty-ctor))
;;       (new-cdt (named leaf def-int-ctor))
;;       (new-cdt (named node def-node-ctor))))
;; 
;;   (tree? (empty)) => #t
;;   (tree? (leaf 1)) => #t
;;   (tree? (node (leaf 1) (empty)) => #t
;; 
;; This is a very simple implementation, only providing very basic
;; functionality. Could possibly be replaced by something more
;; advanced later on.
;; 
;; Note that while libraries such as (planet "datatype.ss" ("dherman"
;; "struct.plt" 2 3)) do implement algebraic datatypes, and are
;; readily available, we have specific requirements regarding the
;; representation of our datatypes.
(module
 adt
 mzscheme

 (require (lib "ast-util.scm" "wg"))
 (require (prefix h. (lib "hash.scm" "wg")))
 (require (lib "usual.scm" "wg"))
 (require* (lib "ast-def-util.scm" "wg"))

 ;; ----------------------------------------------------------
 ;; Datatype record-keeping...

 ;; As we frequently need to look up algebraic datatypes by name, we
 ;; must maintain a lookup table.
 (define adt-table (h.new))

 ;; An entry of "adt-table".
 ;; 
 ;; "choices" is a list of predicates for checking for datatype
 ;; alternatives.
 (define-struct e-adt (choices))

 (define adt-new
   (lambda (adt-name)
     (h.check-no-key adt-table adt-name)
     (let* ((adt-entry (make-e-adt '())))
       (h.store-set! adt-table adt-name adt-entry))))
 
 (define adt-extend
   (lambda (adt-name choice-pred)
     (define adt-entry (adt-lookup adt-name))
     (define choices (e-adt-choices adt-entry))
     ;;(write-nl (list "adt-extend" adt-name choice-pred))
     (set-e-adt-choices! adt-entry (push choices choice-pred))))
 
 (define adt-lookup
   (lambda (adt-name)
     (h.fetch-reqd adt-table adt-name)))

 ;; ----------------------------------------------------------
 ;; Predicate utils...
 
 ;; We want predicates for both abstract (non-instantiable) and
 ;; concrete datatypes. For any datatype defined using our macros we
 ;; automatically define a predicate, regardless whether the datatype
 ;; in question is abstract.

 (define-for-syntax (make-pred-symbol name)
   (string->symbol (string-append (symbol->string name) "?")))
 
 (define-for-syntax (datum-apply stx f . args)
   (datum->syntax-object stx (apply f (map syntax-object->datum args))))

 (define-for-syntax (make-pred-name name^)
   (datum-apply name^ make-pred-symbol name^))
 
 (define (make-adt-pred adt-name)
   (lambda (ast)
     (define adt-entry (adt-lookup adt-name))
     (define choices (e-adt-choices adt-entry))
     (list-memb-match? 
      choices
      (lambda (choice-pred)
	(choice-pred ast)))))
 
 (define (make-named-pred name)
   (lambda (ast)
     (list-named? ast name)))
 
 ;; ----------------------------------------------------------
 ;; Datatype definition...

 ;; This macro enables us to write things like:
 ;; 
 ;;   (define-data (new-adt named expr)
 ;;   (define-data (new-adt* named expr (existing-cdt pred string?)))
 ;;   (define-data (existing-dt named expr (whatever-cdt named my-expr is cool)))
 ;;   (define-data (existing-dt named expr (new-cdt* named foo-expr) (new-cdt named bar-expr)))
 ;; 
 ;; Naturally it is necessary to implement new-adt, new-cdt, etc., but
 ;; the beauty of this solution is that they can typically be
 ;; implemented in a straightforward way with just syntax-rules.
 (define-syntax* (define-data stx)
   (define (f fstx)
     ;; Note that it is essential that we use the "stx" object here
     ;; when creating syntax, rather than "fstx". This would seem to
     ;; have something to do with hygiene; perhaps when we have a
     ;; particular identifier still visible in "stx", and an
     ;; identifier by the same name is introduced, then there is no
     ;; renaming being done for that identifier, since the same
     ;; environment and same name must mean that it is the exact same
     ;; identifier.
     (syntax-case fstx (named pred)
		  ((mac/ owner/ named name/)
		   (with-syntax ((pred/ (make-pred-name (syntax name/))))
				(syntax/loc stx (mac/ owner/ pred/ name/))))
		  ((mac/ owner/ pred pred/)
		   (syntax/loc stx (mac/ owner/ pred/)))
		  ((mac/ owner/ (named name/ arg/ ...))
		   (with-syntax ((pred/ (make-pred-name (syntax name/))))
				(syntax/loc stx (mac/ owner/ pred/ name/ arg/ ...))))
		  ((mac/ owner/ named name/ . sublist/)
		   (with-syntax ((pred/ (make-pred-name (syntax name/)))
				 ((subcall/ ...) 
				  (map 
				   (lambda (item)
				     (define item^ (datum->syntax-object stx item))
				     (syntax-case item^ ()
						  ((submac/ more/ ...)
						   (f (syntax/loc item^ (submac/ name/ more/ ...))))))
				   (syntax-object->datum (syntax sublist/)))))
				(syntax/loc stx (begin
						  (mac/ owner/ pred/ name/)
						  subcall/ ...))))))
   (syntax-case stx (named)
		((_ (adt-mac/ named adt-name/ more/ ...))
		 (f (syntax/loc stx (adt-mac/ none named adt-name/ more/ ...))))))

 ;; For macro debugging. The enclosed code always gets printed, and
 ;; may optionally also be evaluated.
 (define-syntax* (wn stx)
   (syntax-case stx (disable enable)
     ((_ disable x)
      #`(write-nl (format "~s becomes ~s becomes ~s" '#,(syntax x) '#,(expand-once (syntax x)) '#,(expand (syntax x)))))
     ((_ enable x)
      (syntax/loc stx
		  (begin
		    (wn disable x)
		    x)))))

 (define-syntax* existing-dt
   (syntax-rules (none)
     ((_ none pred . ignored)
      (void))
     ((_ owner pred . ignored)
      (adt-extend (quote owner) pred))))

 (define-syntax* new-adt
   (syntax-rules ()
     ((_ owner pred name)
      (begin
	(adt-new (quote name))
	(define pred (make-adt-pred (quote name)))
	(existing-dt owner pred)))))

 (define-syntax* new-adt*
   (syntax-rules ()
     ((_ owner pred name)
      (begin
	(new-adt owner pred name)
	(provide pred)))))

 (define-syntax* new-cdt
   (syntax-rules ()
     ((_ owner pred name)
      (new-cdt owner pred name def-list-ctor))
     ((_ owner pred name def-ctor)
      (begin
	(def-ctor name)
	(define pred (make-named-pred (quote name)))
	(existing-dt owner pred)))))

 (define-syntax* new-cdt*
   (syntax-rules ()
     ((_ owner pred name more ...)
      (begin
	(new-cdt owner pred name more ...)
	(provide name pred)))))

 (define-syntax* (extend-adt* stx)
   (syntax-case stx ()
		((_ owner/ name/ more/ ...)
		 (with-syntax ((pred/ (make-pred-name (syntax name/))))
			      (syntax/loc stx (new-cdt* owner/ pred/ name/ more/ ...))))))

 ) ; end module

#;
(begin

  (require adt)
  (require (lib "usual.scm" "wg"))
  (require (lib "ast-util.scm" "wg"))

  (begin
    (define bbb 1)
    (begin
      (define ccc 2)))
  (write-nl (list bbb ccc))

  (wn enable (define-data (new-adt named expr)))
  (wn enable (define-data (existing-dt named expr
  				(existing-dt pred string?))))
  (write-nl (expr? "string"))
  (write-nl (expr? 555))

  ;#;
  (begin
    (wn enable (define-data (new-cdt named aaa)))
    (write-nl (aaa 1))
    (write-nl (aaa? (aaa))))

  ;#;
  (begin
    (wn enable (define-data (new-adt named yyy
				      (existing-dt pred number?)
				      (new-cdt named yyy-data)
				      )))

    ;; An extract of the code generated by the macro.
    #;
    (begin 
      (define-values (yyy-data) 
	(lambda args (#%app cons (quote yyy-data) args))) 
      (define-values (yyy-data?) 
	(#%app make-named-pred (quote yyy-data))) 
      (#%app adt-extend (quote yyy) (#%top . yyy-data?)))

    (write-nl (yyy? 555))
    (write-nl (yyy-data 1 2))
    (write-nl (yyy-data? (yyy-data 1 2)))
    (write-nl (yyy? (yyy-data 1 2)))
    )
  
  (define-data
    (new-adt named statement
	     (new-cdt named return-stmt)
	     (new-cdt (named string-stmt def-string-ctor))
	     (new-adt named loop-stmt
		      (new-cdt named for-stmt)
		      (new-cdt named while-stmt))))
  (write-nl (statement? 555))
  (write-nl (string-stmt 'foo-bar))
  (write-nl (statement? (return-stmt)))
  (write-nl (loop-stmt? (return-stmt)))
  (write-nl (for-stmt? (for-stmt)))
  (write-nl (for-stmt? (while-stmt)))
  (write-nl (statement? (for-stmt)))
  (write-nl (statement? (while-stmt)))

) ; end test
