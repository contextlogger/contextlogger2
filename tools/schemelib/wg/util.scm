;; 
;; util.scm
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

;; Contains generic-purpose utilities.

(module 
 util
 mzscheme

 (require (lib "list.ss" "srfi" "1"))
 (require (lib "pretty.ss"))
 (require (lib "kw.ss"))
 (require (lib "etc.ss"))
 (require (lib "swindle-util.scm" "wg"))
 (require (lib "swindle-kw.scm" "wg"))

 (require (lib "module-util.scm" "wg"))

 (require* (lib "and-let.ss" "srfi" "2"))

 ;; Not good to have a net dependency in such a general purpose module.
 ;; Causes problems in server-side scripting at the very least.
 ;;(require* (planet "aif.ss" ("schematics" "macro.plt" 1 0)))

 ;; from etc.ss
 ;;
 ;; To get the complete expression file path, you may use
 ;;   (path->complete-path
 ;;     (this-expression-file-name)
 ;;     (this-expression-source-directory))
 (provide identity compose nor nand 
	  define-syntax-set begin-with-definitions
          this-expression-file-name this-expression-source-directory)

 ;; Comes from (planet "aif.ss" ("schematics" "macro.plt" 1 0))),
 ;; but only the part that we use.
 (define-syntax* aif
   (syntax-rules ()
     ((_ name test true-arm false-arm)
      (let ((name test))
        (if name
            true-arm
            false-arm)))))
 
 ;; This is important for enabling something like
 ;; 
 ;;   (when (equal? (path-basename program)
 ;;                 (this-expression-file-name))
 ;;      (main))
 ;; 
 ;; To work, this requires that "program" has been set appropriately,
 ;; e.g. with the mzscheme "-N" switch. Even after doing so, it is
 ;; still possible to determine the executable name with
 ;; (find-system-path 'exec-file).
 ;; 
 ;; The above code would be similar to the Ruby idiom
 ;; 
 ;;   if __FILE__ == $0
 ;;     main()
 ;;   end
 (define* (path-basename path)
   (let-values 
       (((base name dir) (split-path path)))
     name))

 (define* != (negate =))

 (define* (true? x)
   (if x #t #f))
 
 (define* (false? x)
   (if x #f #t))

 (define-syntax w-def-hand
   (syntax-rules ()
     ((_ code)
      (with-handlers
       ;; If an undefined identifier.
       ((exn:fail:contract:variable?
	 (lambda (x) #f))

	;; If a symbol that indicates the start of a special syntactic
	;; form.
	(exn:fail:syntax?
	 (lambda (x) #t)))
       code
       #t))))

 ;; Returns #t iff the symbol passed to this special form is defined
 ;; as a variable, and #f otherwise. The symbol is treated literally.
 (define-syntax* defined?
   (syntax-rules ()
     ((_ var)
      (w-def-hand var))))

 ;; An indirect version of defined? Considers the symbol that the
 ;; argument evaluates to, rather than the argument symbol itself.
 ;; 
 ;; Note that this function is implemented using "eval", and that in
 ;; some cases the environment of evaluation may differ from what you
 ;; would expect; this is true for instance within module bodies.
 (define* (defined-i? dsym)
   (w-def-hand (eval dsym)))

 ;; Commonly needed for printing. (For info on setting the output
 ;; port, see "parameterize" and "current-output-port".)
 (define* display-nl
   (lambda (val)
     (display val)
     (newline)))

 ;; Commonly needed for printing.
 (define* write-nl
   (lambda (val)
     (write val)
     (newline)))

 (define* (puts . x)
   (for-each
    write-nl
    x))

 (define* pretty-nl
   (lambda (val)
     (pretty-print val)))

 (define* (debug val)
   (pretty-nl val)
   val)

 ;; For getting (not printing) an object as a string,
 ;; in a "display" form.
 (define* (to-s arg)
   (format "~a" arg))

 ;; For getting (not printing) an object as a string,
 ;; in a Scheme-parseable form.
 (define* (inspect obj) 
   (format "~s" obj))

 ;; Zips two lists of the same length together so that each pair of
 ;; members is paired with "cons".
 (define* (cons-zip a b)
   (map cons a b))

 ;; Zips two lists of the same length together so that each pair of
 ;; members is paired as a list.
 (define* (list-zip a b)
   (map list a b))

 ;; "list-tail" is defined, so why not "list-head"?
 (define* (list-head l n)
   (letrec ((f (lambda (r l n)
		 (if (= n 0)
		     r
		     (f (append r (list (car l))) (cdr l) (- n 1))))))
     (f '() l n)))

 ;; Sets the "n"th element of list "ll" to "v".
 (define* (list-set-nth n v ll)
   ;; This implementation is equivalent to
   ;; (append (take ll n) (list v) (cdr (drop ll n)))
   ;; but possibly more efficient.
   (let-values (((b e) (split-at ll n)))
     (append b (list v) (cdr e))))

 ;; Appends a single member.
 (define* (push l e)
   (append l (list e)))

 ;; Appends a single member if it does not already exist.
 (define* (push-uniq l e cmp)
   (if (list-memb-match? 
	l
	(lambda (x) (cmp x e)))
       l
       (push l e)))

 (define* (push-uniq-eq ll elem)
   (if (include-eq? ll elem)
       ll
       (push ll elem)))

 ;; Appends a single member, like "push" in Ruby.
 ;; Does not modify "l" if "l" is the empty list.
 #;
 (define* (push! l e)
   (append! l (list e)))

 (define-syntax push-set!
   (syntax-rules ()
     ((_ l e)
      (set! l (push l e)))))

 (define* (append-uniq l al cmp)
   (for-each
    (lambda (e)
      (set! l (push-uniq l e cmp)))
    al)
   l)

 (define-syntax* append-set!
   (syntax-rules ()
     ((_ l morel)
      (set! l (append l morel)))))

 ;; Returns the first member of list "l" that satisfies predicate "f",
 ;; or #f if there is no match. An alternative value for the not-found
 ;; case can be specified optionally.
 (define* find-first
   (letrec ((find
	     (lambda (l f notf)
	       (if (null? l)
		   notf
		   (let ((elem (car l)))
		     (if (f elem)
			 elem
			 (find (cdr l) f notf)))))))
     (case-lambda
      ((l f) (find l f #f))
      ((l f notf) (find l f notf)))))
 
 ;; Returns true iff any member of list "ll" yields a true value if
 ;; passed to function "f".
 (define* (list-memb-match? ll f)
   (true? (find-first ll f)))

 (define* (list-membs-all-match? ll pred?)
   (let/ec esc
	   (for-each
	    (lambda (elem)
	      (unless (pred? elem)
		      (esc #f)))
	    ll)
	   (esc #t)))

 ;; Goes through the list "ll", applying "f" to its elements until "f" returns a true value, at which point that value is returned.
 (define* (first-true-result f ll)
   (if (null? ll) #f
       (aif res (f (car ll))
	    res
	    (first-true-result f (cdr ll)))))

 ;; Filters out #f values.
 (define* (compact ll)
   (filter
    (lambda (elem) (not (eq? elem #f)))
    ll))

 ;; Takes a list and recursively folds any immediate sublists into it.
 (define* (flatten ll)
   (apply append (map 
		  (lambda (x)
		    (if (list? x)
			(flatten x)
			(list x)))
		  ll)))

 (define* (include-eq? ll elem)
   (true? (memq elem ll)))
 ;;   (list-memb-match? ll (fix eq? elem)))

 (define* (include-eqv? ll elem)
   (true? (memv elem ll)))
 ;;   (list-memb-match? ll (fix eqv? elem)))

 (define* (include-equal? ll elem)
   (true? (member elem ll)))
 ;;   (list-memb-match? ll (fix equal? elem)))

 (define-syntax map-set!
   (syntax-rules ()
     ((_ var f)
      (set! var (map f var)))))

 ;; Produces an anonymous, specialized version of function "fn" where
 ;; some of the initial arguments have been fixed to specified values.
 ;; This gives us parametric specialization with syntax almost as nice
 ;; as that of Haskell.
 (define-syntax* fix
   (syntax-rules ()
     ((_ fn arg ...)
      (lambda rest (apply fn arg ... rest)))))

 (define-for-syntax (n-symbols n)
   (if (= n 0)
       '()
       (cons (gensym) (n-symbols (- n 1)))))
 
 ;; Fixes the "n"th argument of function "fn" to the specified value,
 ;; returning the partially specialized function.
 ;; 
 ;; Say if "n" is 2, we wish to have the macro expand to (lambda (x y .
 ;; rest) (apply fn x y arg rest)).
 (define-syntax* (fixn stx)
   (define (mk-n n)
     (datum->syntax-object stx (n-symbols n)))
   
   (syntax-case stx ()
     ((_ fn n arg)
      (let ((xs (mk-n (syntax-object->datum (syntax n)))))
        (quasisyntax/loc
         stx
         (lambda ((unsyntax-splicing xs) . rest)
           (apply fn (unsyntax-splicing xs) arg rest)))))))

 (define* (fixr f rest)
   (lambda args (apply f (append args rest))))

 (define-syntax* alet
   (syntax-rules ()
     ((_ name expr exprs ...)
      (let ((name expr))
	exprs ...))))

 (define-syntax* aand*
   (syntax-rules ()
     ((_ value)
      value)
     ((_ id id-calc more ...)
      (aif id id-calc (aand* more ...) #f))))

 (define* (list-join ll sep)
   (cond ((null? ll) '())
         ((null? (cdr ll)) (car ll))
         (else (append (car ll) 
                       sep 
                       (list-join (cdr ll) sep)))))

 (define* (string-concat ll)
   (apply string-append ll))
 
 ;; See also "string.ss"; this definition should be compatible.
 (define* (string-join ll sep)
   (cond ((null? ll) "")
	 ((null? (cdr ll)) (car ll))
	 (else (string-append (car ll) 
			      sep 
			      (string-join (cdr ll) sep)))))

 ;; This name already defined in (lib "misc.ss" "swindle").
 ;;  (define* (symbol-append ll)
 ;;    (string->symbol
 ;;     (apply string-append (map symbol->string ll))))

 (define* (symbol-join ll sep)
   (string->symbol
    (string-join (map symbol->string ll) sep)))

 (define* (string-starts-with? s p)
   (let* ((slen (string-length s))
	  (plen (string-length p)))
     (if (< slen plen)
	 #f
	 (let ((ssub (substring s 0 plen)))
	   (equal? ssub p)))))

 (define* (string-ends-with? s p)
   (let* ((slen (string-length s))
	  (plen (string-length p)))
     (if (< slen plen)
	 #f
	 (let ((ssub (substring s (- slen plen))))
	   (equal? ssub p)))))

 (define* (string-starts-with-ic? s p)
   (let* ((slen (string-length s))
	  (plen (string-length p)))
     (if (< slen plen)
	 #f
	 (let ((ssub (substring s 0 plen)))
	   (string-ci=? ssub p)))))

 (define* (string-ends-with-ic? s p)
   (let* ((slen (string-length s))
	  (plen (string-length p)))
     (if (< slen plen)
	 #f
	 (let ((ssub (substring s (- slen plen))))
	   (string-ci=? ssub p)))))

 (define* (symbol-starts-with? sym p)
   (string-starts-with? (symbol->string sym) p))
 
 (define* (symbol-ends-with? sym p)
   (string-ends-with? (symbol->string sym) p))
 
 (define* do-nothing
   (lambda args #f))

 (define* (carry flist arglist)
   (if (null? flist)
       arglist
       (carry (cdr flist) (apply (car flist) arglist))))

 ;; Reverse compose.
 (define* (compose-r . flist)
   (apply compose (reverse flist)))
 
 ;; Reverse "compose-l".
 (define* (compose-rl . flist)
   (lambda arglist
     (carry flist arglist)))

 ;; Normal "compose", as defined in "etc.ss", expects the use of
 ;; "values" when returning multiple values. This version works when
 ;; multiple return values are returned as a list.
 ;;
 ;;  ((compose
 ;;    (lambda (x y) (list x y))
 ;;    (lambda (x y) (values x y)))
 ;;   "x" "y")
 ;;  
 ;;  ((compose-l
 ;;    (lambda (x y) (list x y))
 ;;    (lambda (x y) (list x y)))
 ;;   "x" "y")
 (define* (compose-l . flist)
   (apply compose-rl (reverse flist)))

 ;; xxx deprecated, use compose-rl instead
 (define* compose-carry compose-rl)

 (define* (error-f fmt . args)
   (error (apply format (cons fmt args))))
 
 ;; Like "case", but uses "eq?" for comparisons, not "eqv?".
 (define-syntax* case-eq
   (syntax-rules (else)
     ((_ e (a b) ... (else c))
      (cond ((eq? e a) b) ... (else c)))
     ((_ e (a b) ...)
      (cond ((eq? e a) b) ...))))

 ;; Cf. "aif".
 (define-syntax* awhen
   (syntax-rules ()
     ((_ name test expr ...)
      (let ((name test))
	(when name
	      expr ...)))))

 ;; "f" is executed "n" times for its side effects.
 (define* (times n f)
   (unless (= n 0)
     (f)
     (times (- n 1) f)))

 ;; "f" is executed "n" times, and each result is collected into a
 ;; list.
 (define* (map-times n f)
   (if (= n 0)
       '()
       (cons (f) (map-times (- n 1) f))))

 ;; Like (map-times n (thunk elem)), but possibly more efficient.
 (define* (times-elem n elem)
   (if (= n 0)
       '()
       (cons elem (times-elem (- n 1) elem))))

 (define* (map-with-index f list)
   (define index 0)
   
   ;; The order in which "map" processes the list elements is
   ;; unspecified, so we shall use "fold" instead. We could also
   ;; consider using "fold-right" to allow us to use "cons" instead
   ;; of "push", but then the counter would need decrementing rather
   ;; than incrementing.
   (fold
    (lambda (elem res)
      (alet nres (push res (f index elem))
            (set! index (+ index 1))
            nres))
    '()
    list))
 
 ;; True iff any of the specified predicates matches the specified
 ;; datum. Implemented using a macro for efficiency.
 (define-syntax* any-pred
   (syntax-rules ()
     ((_ x) #f)
     ((_ p x) (p x))
     ((_ p ... x) (or (p x) ...))))

 ;; True iff all of the specified predicates match the specified
 ;; datum. Implemented using a macro for efficiency.
 (define-syntax* every-pred
   (syntax-rules ()
     ((_ x) #t)
     ((_ p x) (p x))
     ((_ p ... x) (and (p x) ...))))

 (define* (assert-fail)
   (error "assertion failed"))

 (define* (assert-cond c)
   (when (not c) (assert-fail)))

 (provide map-set! push-set!))
