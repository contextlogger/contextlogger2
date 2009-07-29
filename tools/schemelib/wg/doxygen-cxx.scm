;; 
;; doxygen-cxx.scm
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

;; Derives Doxygen comments for C++ code based on annotations.
(module 
 doxygen-cxx
 mzscheme

 (require (lib "ast-util.scm" "wg"))
 (require (lib "prop-util.scm" "wg"))
 (require (lib "compact.scm" "wg"))
 (require (lib "documentation.scm" "wg"))
 (require (lib "emission.scm" "wg"))
 (require (lib "mop.scm" "wg"))
 (require (lib "usual.scm" "wg"))

 (define (get-most-specific-doc ast)
   (or (fget-opt-nlist-elem-1 ast 'cxx-cmt-doc)
       (fget-opt-nlist-elem-1 ast 'cxx-doc)
       (fget-opt-nlist-elem-1 ast 'doc)))

 (define (add-doc ast lines)
   ;;(write-nl lines)
   (alet text (join-lines lines)
	 (if (equal? text "")
	     ast
	     (push ast `(doxy-doc ,text)))))

 (define (desc-tag-to-string tag)
   (join-words (list (symbol->string (car tag))
		     (if (null? (cdr tag))
			 #f
			 (cadr tag)))))

 (define (semi-list-string ll)
   (string-append
    (string-join ll "; ")
    "."))

 (define (nrd-string ast)
   (let ((n (fget-opt-nlist-elem-1 ast 'name))
	 (r (fget-opt-nlist-elem-1 ast 'reason))
	 (d (fget-opt-nlist-elem-1 ast 'description))
	 (nr #f))
     (if n (set! n (to-string n)))
     (if r (set! r (expr-to-string r)))
     (set! nr
	   (cond
	    ((and n r)
	     (string-append n " / " r))
	    (n n)
	    (r r)
	    (else
	     (error "neither error code or symbol given" ast))))
     (if d
	 (string-append nr " (" d ")")
	 nr)))

 (define (func-doc ast)
   (define lines '())

   (define func-t (func-get-func-t ast))

   ;; Arguments.
   ;; 
   ;; Note that due to the nature of the @param tag, it is
   ;; unfortunately not possible to have a nameless argument with
   ;; documentation. Same is true for an "unused" argument, and hence
   ;; in either case, no documentation will be emitted for the
   ;; parameter.
   (let* ((arglist (get-opt-nlist-elem-1up-e func-t 'args)))
     (for-each
      (lambda (arg)
	(define argname (get-opt-nlist-elem-1 arg 'name))
	(define argdoc (get-most-specific-doc arg))
	(when (and argdoc 
		   argname
		   (not (has-true-bool-prop-memb? arg 'unused)))
	      (push-set! lines
			 (join-words "@param"
				     (symbol->string argname)
				     argdoc))))
      arglist))
	    
   ;; Return value.
   (awhen
    returns (fget-opt-nlist-elem func-t 'returns)
    (let ((retdoc (get-most-specific-doc returns))
	  (errors (get-opt-nlist-elem-1up-e ast 'error-codes)))
      (when (or retdoc (not (null? errors)))
	    (let ((words (list "@return" retdoc)))
	      (unless (null? errors)
		      (append-set! 
		       words 
		       (list
			"Possible error codes include:"
			(semi-list-string (map nrd-string errors)))))
	      (push-set! lines (join-words words))))))

   ;; Leaves.
   (let ((leavelist (get-opt-nlist-elem-1up-e ast 'leaves)))
     (for-each
      (lambda (leave)
	(push-set! lines 
		   (join-words (list (symbol->string (car leave))
				     (nrd-string leave)))))
      leavelist))

   ;; Panics.
   (let ((paniclist (get-opt-nlist-elem-1up-e ast 'panics)))
     (for-each
      (lambda (panic)
	(define c (fget-reqd-nlist-elem-1 panic 'category))
	(define r (fget-reqd-nlist-elem-1 panic 'number))
	(define d (fget-opt-nlist-elem-1 panic 'description))
	(push-set! lines 
		   (join-words (list (symbol->string (car panic))
				     (string-append c "-" r)
				     d))))
      paniclist))

   ;; Precondition(s).
   (let* ((tag (get-opt-nlist-elem ast '@pre)))
     (when tag
	   (push-set! lines (desc-tag-to-string tag))))

   ;; Postcondition(s).
   (let* ((tag (get-opt-nlist-elem ast '@post)))
     (when tag
	   (push-set! lines (desc-tag-to-string tag))))

   ;; Realtime guarantee.
   (let* ((tag (get-opt-nlist-elem ast '@realtime)))
     (when tag
	   (push-set! lines (desc-tag-to-string tag))))

   lines)

 (define (generic-doc ast)
   (define lines '())
   
   ;; Freeform text.
   (push-set! lines (get-most-specific-doc ast))

   ;; References.
   (let* ((see-list (get-opt-nlist-elem-1up-e ast 'sees))
	  (targets 
	   (map 
	    (lambda (see)
	      (fget-reqd-nlist-elem-1 see 'see-target))
	    see-list)))
     (for-each
      (lambda (target)
	(push-set! lines (string-append "@see " target)))
      targets))
     
   ;; API lifecycle status.
   (let* ((tag (get-opt-nlist-elem-by-names ast '(@released @deprecated @removed @test @prototype))))
     (when tag
	   (push-set! lines (desc-tag-to-string tag))))

   ;; Documentation recipients.
   (let* ((tag (get-opt-nlist-elem-by-names ast '(@internalTechnology @internalComponent @internalAll @publishedPartner @publishedAll))))
     (when tag
	   (push-set! lines (desc-tag-to-string tag))))

   lines)

 ;; Adds a "doxy-doc" element to "ast" if any documentation is derived
 ;; for it. Returns the changed AST.
 (define* (add-doxy-doc ast)
   (cond
    ((or (not (named-list? ast))
	 (doc-forbidden? ast))
     ast)

    ((list-named-one-of? ast '(global-func class-meth inst-meth cxx-ctor cxx-dtor cxx-oper cxx-conv))
     (add-doc ast (append (generic-doc ast) (func-doc ast))))

    (else
     (add-doc ast (generic-doc ast)))))

) ; end module
