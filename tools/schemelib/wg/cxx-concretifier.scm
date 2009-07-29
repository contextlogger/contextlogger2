;; 
;; cxx-concretifier.scm
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
 cxx-concretifier
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "prop-util.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "compact.scm" "wg"))
 (require (prefix h. (lib "hash.scm" "wg")))
 (require (lib "cxx-sectioner.scm" "wg"))
 (require (lib "settings.scm" "wg"))
 (require (lib "string-util.scm" "wg"))

 (define/kw (sect-key-list-by-ftype ftype separate-inl?)
   (cond
    ((eq? ftype 'h-file)
     (if separate-inl? '((iface vext)) '((iface vext) (impl vext))))
    ((eq? ftype 'inl-file)
     (if separate-inl? '((impl vext)) '()))
    ((eq? ftype 'cpp-file)
     '((iface vint) (impl vint)))
    ((eq? ftype 'one-file)
     '((iface both) (impl both)))
    (else
     (error "illegal file type" ftype))))

 (define (get-sect-key-list ftypes separate-inl)
   (letrec ((sect-list '())
	    (add
	     (lambda (sect)
	       (unless (include-equal? sect-list sect)
		       (set! sect-list (push sect-list sect)))))
	    (add-list
	     (lambda (sects)
	       (for-each add sects))))
     (for-each
      (lambda (ftype)
	(add-list (sect-key-list-by-ftype ftype separate-inl)))
      ftypes)
     sect-list))

 (define/kw (get-sect-map ast ftypes separate-inl sect-op)
   (let ((keys (get-sect-key-list ftypes separate-inl))
	 (sectmap (h.new equal?)))
     (for-each
      (lambda (key)
	(h.store-set! 
	 sectmap key
	 (apply ast-section ast (first key) (second key) sect-op)))
      keys)
     ;;(pretty-nl sectmap)
     sectmap))

 (define (filename-by-ftype basename ftype)
   (string-append
    basename
    (cond
     ((eq? ftype 'h-file) ".h")
     ((eq? ftype 'inl-file) ".inl")
     ((eq? ftype 'cpp-file) (if (in-c-mode) ".c" ".cpp"))
     ((eq? ftype 'one-file) (if (in-c-mode) ".c" ".cpp"))
     (else (error "illegal file type" ftype)))))

 (define (join-bodies bodies)
   (let ((res '(body)))
     (for-each
      (lambda (body)
	(append-set! res (cdr body)))
      bodies)
     res))

 (define (make-include kind file)
   `(include (incl-kind ,kind) (file ,file)))

 ;; Takes an AST representing abstract C++ code, and produces one or
 ;; more ASTs representing concrete C++ code units.
 ;; 
 ;; In more detail, this pass:
 ;; * calls on the sectioner to produce the sections required for the
 ;;   specified file types
 ;; * joins the relevant sections so as to form one cxx-unit per file
 ;;   type
 ;; * copies relevant includes into the unit body
 ;; * defines the filename (derived from unit basename)
 ;; 
 ;; ast^:: A cxx-unit element.
 ;; ftypes:: Any of h-file, inl-file, cpp-file, one-file.
 ;; returns:: A list of cxx-unit elements, one per specified file
 ;;           type, in the same order.
 (define/kw* (ast-concretify ast ftypes
                             #:key separate-inl harness-h signature-h do-not-edit
                             #:other-keys sect-op)
   (let ((basename (get-reqd-nlist-elem-1 ast 'basename))
         ;; We might consider having a pass that fills in this element
         ;; by inspecting references to APIs and any annotations
         ;; stating which include file is required to use each API;
         ;; here we assume that "includes" is already complete, apart
         ;; from any references to files being generated here; we also
         ;; assume that there are no duplicates.
	 (pre-includes (get-opt-nlist-elem ast 'includes))
	 (sect-map (get-sect-map ast ftypes separate-inl sect-op))
	 (bodyless (reject-prop-membs ast 'body)))
     (map
      (lambda (ftype)
	(let* ((filename (filename-by-ftype basename ftype))
	       (keys (sect-key-list-by-ftype ftype separate-inl))
	       (sects (map 
		       (lambda (key)
			 (h.fetch sect-map key))
		       keys))
	       (this-ast (push bodyless `(filename ,filename)))
	       (this-body #f))

	  ;; Construct a "body" for the unit.
	  (let ((bodies '()))
	    (for-each
	     (lambda (sect)
	       (let ((body (get-opt-nlist-elem sect 'body)))
		 (when body
		       (push-set! bodies body))
		 ))
	     sects)
	    (set! this-body (join-bodies bodies)))
	  
	  ;; Set any includes for the unit.
	  (let* ((prei (if pre-includes (cdr pre-includes) '()))
		 (pree (if (null? prei) #f `(together ,@prei))))
	    (cond
	     ((eq? ftype 'h-file)
	      (let ((harness-string (string-append "__" (string-upcase (string-hyphens-to-underscores basename)) "_H__")))
                ;;(write-nl harness-string)
                ;;(write-nl (list "harness-h" harness-h))
		(when pree
		      (set! this-body (prepend-elem this-body pree)))
                (when harness-h
                  (set! this-body (prepend-elem this-body `(together (text-append "#ifndef " ,harness-string) (text-append "#define " ,harness-string)))))
                ;;(when signature-h (set! this-body (prepend-elem this-body `(text ,(if (in-c-mode) "// -*- c -*-" "// -*- c++ -*-")))))
		(when (include-eq? ftypes 'inl-file)
		      (push-set! 
		       this-body 
		       (make-include 'local (filename-by-ftype basename 'inl-file))))
                (when harness-h
                  (push-set! this-body `(text-append "#endif // " ,harness-string)))
                ))
	     ((eq? ftype 'cpp-file)
	      (begin
		(when (include-eq? ftypes 'h-file)
		      (let ((hinc
			     (make-include 'local (filename-by-ftype basename 'h-file))))
			(set! pree hinc)))
		(when pree
		      (set! this-body (prepend-elem this-body pree)))))
	     ((eq? ftype 'one-file)
	      (when pree
		    (set! this-body (prepend-elem this-body pree))))))

          (when do-not-edit
            (alet note '(text "// generated code -- do not edit")
                  (set! this-body (prepend-elem this-body note)))
            (when (eq? ftype 'h-file)
              (when signature-h
                (set! this-body (prepend-elem this-body `(text ,(if (in-c-mode) "// -*- c -*-" "// -*- c++ -*-")))))))
          
	  (push-set! this-ast this-body)

	  this-ast))
      ftypes)))

 ) ; end module
