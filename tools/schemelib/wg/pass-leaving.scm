;; 
;; pass-leaving.scm
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
 pass-leaving
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "ast-spec.scm" "wg"))
 (require (lib "tables.scm" "wg"))
 (require (lib "declarations.scm" "wg"))
 (require (lib "mop.scm" "wg"))
 (require (lib "match.scm" "wg"))
 (require (lib "settings.scm" "wg"))
 (require (prefix h. (lib "hash.scm" "wg")))

 ;; Returns a list of function declarations in the given AST.
 (define (build-func-list ast)
   (cond
    ((any-pred cxx-oper? cxx-conv? ast)
     '()) ;; xxx not yet supported
    ((func-like-decl? ast)
     (list ast))
    ((named-list? ast)
     (apply append (map build-func-list (cdr ast))))
    (else
     '())))

 ;; A record of information regarding the leave status of a function.
 ;; "s" is the leave status, which is one of: 'yes (is leaving), 'no
 ;; (not leaving), 'unknown (whether leaving), 'impossible (to
 ;; resolve). "d" is the declaration (its AST).
 (define-struct lstat (s d))

 (define (infer-stat lmap root-ast)
   (define (stat-sum xs)
     (cond
      ((memq 'yes xs)
       'yes)
      ((list-membs-all-match? xs (fix eq? 'no))
       'no)
      ((list-membs-all-match? xs (fix include-eq? '(no impossible)))
       'impossible)
      (else
       'unknown)))

   (define (infer-stmtlist xs)
     (stat-sum (map infer-stmt xs)))

   (define (infer-exprlist xs)
     (stat-sum (map infer-expr xs)))

   (define/kw (infer-expr-attr ast #:optional (name 'expr))
     (alet expr (fget-reqd-nlist-elem-1 ast name)
	   (infer-expr expr)))

   (define/kw (infer-opt-expr-attr ast #:optional (name 'expr))
     (aif expr (fget-opt-nlist-elem-1 ast name)
	  (infer-expr expr)
	  'no))

   (define (infer-exprs-attr ast)
     (alet exprs (fget-reqd-nlist-elem ast 'exprs)
	   (infer-exprlist (cdr exprs))))

   ;; Uses "lmap", but does not modify it. Returns the inferred leave
   ;; status; the possible values are the same as for "lstat-s".
   (define (infer-stmt ast)
     ;;(write-nl (list 'inferring ast))
     (or (any-leaving-annot ast)
	 (cond
	  ((block? ast)
	   (alet stmts (fget-reqd-nlist-elem ast 'stmts)
		 (infer-stmtlist (cdr stmts))))
	  ((local-var? ast)
	   (aif init (fget-opt-nlist-elem-1 ast 'init)
		(infer-expr init) 'no))
	  ((any-pred trap-block? expr-stmt? ast)
	   (alet expr (fget-reqd-nlist-elem-1 ast 'expr)
		 (infer-expr expr)))
	  ((return-stmt? ast)
	   (aif expr (fget-opt-nlist-elem-1 ast 'expr)
		(infer-expr expr) 'no))
	  ((if-stmt? ast)
	   (let ((expr (fget-reqd-nlist-elem-1 ast 'cond-expr))
		 (stmt1 (fget-reqd-nlist-elem-1 ast 'then-stmt))
		 (stmt2 (fget-opt-nlist-elem-1 ast 'else-stmt)))
	     (stat-sum (list (infer-expr expr)
			     (infer-stmt stmt1)
			     (if stmt2 (infer-stmt stmt2) 'no)))))
	  ((any-pred while-stmt? do-while-stmt? ast)
	   (let ((expr (fget-reqd-nlist-elem-1 ast 'cond-expr))
		 (stmt (fget-reqd-nlist-elem-1 ast 'stmt)))
	     (stat-sum (list (infer-expr expr)
			     (infer-stmt stmt)))))
	  ((for-stmt? ast)
	   (let ((expr1 (fget-opt-nlist-elem-1 ast 'init-expr))
		 (expr2 (fget-opt-nlist-elem-1 ast 'cond-expr))
		 (expr3 (fget-opt-nlist-elem-1 ast 'step-expr))
		 (stmt (fget-reqd-nlist-elem-1 ast 'stmt)))
	     (stat-sum (list (if expr1 (infer-expr expr1) 'no)
			     (if expr2 (infer-expr expr2) 'no)
			     (if expr3 (infer-expr expr3) 'no)
			     (infer-stmt stmt)))))
	  ((switch-stmt? ast)
	   (let ((expr (fget-reqd-nlist-elem-1 ast 'switch-expr))
		 (stmts (fget-reqd-nlist-elem ast 'stmts)))
	     (stat-sum (list (infer-expr expr)
			     (infer-stmtlist (cdr stmts))))))
	  ((list-named-one-of? ast '(switch-case switch-default))
	   ;; These are labels only, and can only contain constant integer
	   ;; values at most, nothing leaving.
	   'no)
	  ((any-pred goto-stmt? break-stmt? continue-stmt? ast)
	   'no)
	  ((list-named? ast 'cxx-chunk)
	   'impossible)
	  ((statement? ast)
	   ;; xxx this kind of strict checking causes extensibility
	   ;; problems; suppose someone extends the language with a new
	   ;; statement type, as we actually have already done...
	   (error "unsupported statement" ast))
	  (else
	   (error "expected statement" ast)))))

   ;; Checks if the passed AST node has a "leaving" annotation. If so,
   ;; returns either 'yes or 'no as the leaving status (as
   ;; appropriate). Otherwise returns #f.
   (define (any-leaving-annot ast)
     (aif leaving (fget-opt-nlist-elem ast 'leaving)
	  (alet stat (second leaving)
		(if stat 'yes 'no))
	  #f))
   
   ;; Most expressions as such do not leave, but many expressions may
   ;; contain subexpressions (such as those of type call-expr) that
   ;; may leave.
   ;; 
   ;; Constructors may not leave, according to Symbian conventions,
   ;; and we assume these conventions are followed, and hence that new
   ;; and new[] do not cause a leave due to the constructor. If the
   ;; "new" operator might leave, we assume it is of the ELeave
   ;; overload variant, and has the appropriate "leaving" annotation.
   ;; Overloaded new and new[] operators themselves might cause a
   ;; leave for cases other than new (ELeave) also; see the notes
   ;; regarding our operator support below.
   ;; 
   ;; If the Symbian conventions allowed for leaving destructors,
   ;; delete and delete[] operations might leave; we assume the
   ;; conventions are followed. Overloaded delete and delete[]
   ;; operators themselves might cause a leave; see the notes
   ;; regarding our operator support below.
   ;; 
   ;; If Symbian conventions allowed for leaving constructors, the
   ;; constructor invocations resulting from ctor-call-expr and
   ;; super-call-expr and ctor-super initializer expressions should
   ;; also be considered, but we assume that the conventions are
   ;; followed.
   ;; 
   ;; xxx currently not resolving leave status for operator
   ;; invocations -- just assuming the status as 'no (unless there is
   ;; an explicit annotation, as there should be with new (ELeave), at
   ;; least), which is dangerous; but certainly Symbian OS has APIs
   ;; such that say operator<< is leaving
   ;; 
   ;; xxx do not know whether cxx-conv special functions may leave
   ;; 
   ;; xxx invocations via member function pointers are not supported
   ;; at all
   (define (infer-expr ast)
     (define (g ast)
       (if ast (infer-expr ast) 'no))

     (or (any-leaving-annot ast)
	 (cond
	  ((call-expr? ast)
           ;; Resolving this is not too straightforward. Arguments are
           ;; trivial; if any may leave, then the overall call
           ;; expression may. If the target expression may leave, then
           ;; its leave status is 'yes. Otherwise, if the target
           ;; expression resolves to a function reference, then the
           ;; leave status is that indicated by func-t type; if it
           ;; indicates nothing, then the status is 'impossible.
           ;; Otherwise the target expression must resolve to a
           ;; function; if the function is known, the leave status is
           ;; that declared or inferred for that function (possibly
           ;; still 'unknown); if the function is unknown (undefined),
           ;; the leave status is 'impossible.
	   (let* ((arglist (fget-opt-nlist-elem-1up-e ast 'exprs))
		  (argstat (infer-exprlist arglist))
		  (fid (fget-opt-nlist-elem-1 ast 'decl-id-ref))
		  (target (fget-reqd-nlist-elem-1 ast 'target))
		  (tstat (infer-expr target))
		  (cstat 
		   (if (expr-get-name-ref target)
		       (if fid
			   (alet entry (h.fetch-reqd lmap fid)
				 (lstat-s entry))
			   'impossible)
		       (or
			(let* ((ttype (strip-any-ref-to (get-texpr-or-tvar target))))
			  (and (func-t? ttype)
			       (any-leaving-annot ttype)))
			'impossible))))
	     (stat-sum (list tstat argstat cstat))))
	  ((any-pred unary-op-expr? cast-expr? 
		     delete-expr? delete-array-expr? ast)
	   (infer-expr-attr ast))
	  ((any-pred dot-expr? arrow-expr? binary-op-expr? ast)
	   (stat-sum (list (infer-expr-attr ast 'expr1)
			   (infer-expr-attr ast 'expr2))))
	  ((any-pred new-expr? ctor-call-expr? multi-op-expr? ast)
	   (infer-exprs-attr ast))
	  ((new-array-expr? ast)
	   (error "unsupported" ast))
	  ((index-expr? ast)
	   (stat-sum (list (infer-expr-attr ast 'expr)
			   (infer-expr-attr ast 'index))))
	  ((if-expr? ast)
	   (stat-sum (list (infer-expr-attr ast 'cond-expr)
			   (infer-expr-attr ast 'then-expr)
			   (infer-expr-attr ast 'else-expr))))
	  ((lift-stmt-expr? ast)
	   (error "should have been lifted already" ast))
	  ((any-pred name-ref? literal? texpr-expr? sizeof-expr? ast)
	   'no)
	  ((expression? ast)
	   (error "unsupported expression" ast))
	  (else
	   (error "expected expression" ast)))))

   (define (infer-any ast)
     (cond
      ((expression? ast)
       (infer-expr ast))
      (else
       (infer-stmt ast))))

   (infer-any root-ast))

 (define* (pass-leave-infer ast tables)
   (define func-list (build-func-list ast))

   (define lmap (h.new))

   (define (to-stat leaving? known? inferable?)
     (if known?
	 (if leaving? 'yes 'no)
	 (if inferable? 'unknown 'impossible)))

   ;; Build an initial version of the map.
   (for-each
    (lambda (ast)
      (let* ((decl-id (fget-reqd-nlist-elem-1 ast 'decl-id))
	     (type (func-get-func-t ast))
	     (leaving (fget-opt-nlist-elem type 'leaving))
	     (known? (true? leaving))
	     (leaving? (and leaving (cadr leaving)))
	     (body (fget-opt-nlist-elem ast 'block))
	     (inferable? (true? body))
	     (status (to-stat leaving? known? inferable?))
	     (entry (make-lstat status ast)))
	(h.store-set! lmap decl-id entry)))
    func-list)

   ;; Infer until can infer no more. We know that a TRAP cannot leave,
   ;; and hence any calls within the body of a TRAP need not be
   ;; inspected. Any non-trapped call to a leaving function will
   ;; result in (leaving #t) status. If nothing is known to be
   ;; leaving, and only calls to unknown functions remain, then the
   ;; leave status is unresolvable. Otherwise it may become resolvable
   ;; if we can infer the leave status of the remaining calls. If
   ;; there is a "leaving" annotation within a call, then that
   ;; information will be taken for a fact.
   ;; 
   ;; xxx for functions that are virtual, we should probably adopt the
   ;; leave status of any overridden method whose leave status is
   ;; known; this is because when calling a virtual, we do not know
   ;; (based on the static receiver type) which exact method we are
   ;; calling; we should actually add a pass that adds annotations
   ;; indicating the parent method of an overriding virtual method, to
   ;; make the required information available
   (let loop ()
     (alet found #f
	   (for-each
	    (lambda (entry)
	      (when (eq? (lstat-s entry) 'unknown)
		    (let* ((decl (lstat-d entry))
			   (body (fget-reqd-nlist-elem decl 'block))
			   (new-stat (infer-stat lmap body)))
		      (when (not (eq? new-stat 'unknown))
			    (if (gop-true? 'show-leave-infer) (write-nl (list 'inferred 'leave 'status (to-string new-stat) 'for (get-decl-name decl))))
			    (set-lstat-s! entry new-stat)
			    (set! found #t)))))
	    (h.all-values lmap))
	   (when found (loop))))

   (list ast (tables-set tables 'l-table lmap)))

 ;; A pass for checking that if a function has been declared as
 ;; (leaving #f) and it has a body, then there is nothing that we know
 ;; might leave in the body of the function; essentially we just
 ;; re-resolve the leaving status of the function from its body, using
 ;; one pass of the routine we already have.
 (define* (pass-leave-check ast tables)
   (define lmap (tables-get-reqd tables 'l-table))
   (define func-list (build-func-list ast))

   (define (check-func ast)
     (let* ((decl-id (fget-reqd-nlist-elem-1 ast 'decl-id))
	    (body (fget-opt-nlist-elem ast 'block))
	    (entry (h.fetch-reqd lmap decl-id))
	    (claimed-stat (lstat-s entry)))
       (when (and body (eq? claimed-stat 'no))
	     (alet actual-stat (infer-stat lmap body)
		   (when (eq? actual-stat 'yes)
                         ;; If there had not been a declaration, then
                         ;; the status should agree with the inferred
                         ;; status.
			 (error "function declared as non-leaving, but its body has potentially leaving expressions" ast))))))

   (for-each check-func func-list)

   (list ast tables))

 ;; Inspects all elements in "ast". For any elements that have an
 ;; "assume-leaving" attribute, we check that our idea of the leave
 ;; status for that AST node is consistent with the assumption.
 (define* (pass-leave-assumption ast tables)
   (define lmap (tables-get-reqd tables 'l-table))

   (define (check ast)
     (when (named-list? ast)
	   (when (any-pred statement? expression? ast)
		 (awhen assume-elem (fget-opt-nlist-elem ast 'assume-leaving)
			(let* ((assume-value (second assume-elem))
			       (assume-stat (if assume-value 'yes 'no))
			       (actual-stat (infer-stat lmap ast)))
			  (when (memq actual-stat '(yes no))
				(alet actual-value (case-eq actual-stat
							    ('yes #t)
							    ('no #f)
							    (else (assert-fail)))
				      (unless (eq? assume-value actual-value)
					      (error-f "incorrect assumption in ~s: leaving status assumed as ~s, but is ~s" ast assume-value actual-value)))))))

	   (for-each check ast)))

   ;; Only checks blocks. We do not support anything else anyway, so
   ;; this will save us some time.
   (define (trav ast)
     (cond
      ((block? ast)
       (check ast))
      ((named-list? ast)
       (for-each trav ast))))

   (trav ast)

   (list ast tables)) ;; end assumption checking pass

) ; end module
