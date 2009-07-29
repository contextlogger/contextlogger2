;; 
;; environment.scm
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
 environment
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (prefix h. (lib "hash.scm" "wg")))
 (require (lib "type-unification.scm" "wg"))
 (require (lib "declarations.scm" "wg"))

 ;; On indexing overloads
 ;; 
 ;; Alternative 1: Function names as non-unique keys
 ;; 
 ;; For handling function overloads, we must allow multiple entries
 ;; per name, but generally, a name may not be redefined, at least not
 ;; in the same environment. The question is, do we use a hash
 ;; allowing multiple values per key for all names, or do we have a
 ;; separate hash just for functions. And this question applies to our
 ;; local environment handling as well, since overloaded methods can
 ;; be introduced in nested scopes within classes and namespaces. We
 ;; should not allow a name to be redefined, generally, but in the
 ;; case of functions, introducing the same name should be allowed,
 ;; long as the parameters are different; or we can just leave such
 ;; checking for the C++ compiler. Perhaps, for better abstraction, we
 ;; shall simply create a structure representing an environment, and
 ;; use that structure both when dealing with global and local
 ;; environments. The structure can decide on how things are
 ;; represented.
 ;; 
 ;; Alternative 2: Function signatures as unique keys
 ;; 
 ;; An issue with overload resolution is that type equality depends on
 ;; the set of types that have been defined (typedefs, inheritance),
 ;; and hence the equality of two types could effectively change over
 ;; time as more definitions are processed. Hence using a hash keyed
 ;; by (function name, function type) is not an option unless we build
 ;; our function table only after information about non-function
 ;; declarations have already been collected. Which we could do, no
 ;; problem, if we had a fixed set of code to analyze, but given that
 ;; we are dealing with code manipulation here, such an approach might
 ;; lead to problems. Just think of MOPs making it easy to manipulate
 ;; inheritance hierarchies and the like.
 ;; 
 ;; If we went this route, Richard Cobbe's environment.ss (at Planet)
 ;; might be a useful basis for the environment implementation.
 ;; 
 ;; I suppose we must accept the fact that some AST changes must be
 ;; reflected in "tables", but having to rebuild "tables" or parts of
 ;; it because it has become inconsistent is something to be avoided.

 ;; We use a representation in which function names map to ordered
 ;; (for determinism) lists of declaration IDs, and other names map to
 ;; single declaration IDs. No name may refer to both a function and a
 ;; declaration of some other kind.
 ;; 
 ;; Note that the #f macro argument here states that all the fields of
 ;; the structure should be visible, but structure printing is still
 ;; not enabled by default, so this alone is not sufficient to get the
 ;; structure printing.
 (define-struct env (names parent) #f)

 ;; Creates an empty environment. Any parent environment may be given.
 (define* new
   (case-lambda
    (() (new #f))
    ((parent) (make-env (h.new) parent))))

 ;; Makes a copy of the topmost frame, and returns it. This is
 ;; essentially a shallow copy of the entire environment.
 (define* (copy env)
   (make-env (env-names env) (env-parent env)))

 ;; Pushes the specified frame into the environment. Modifies "frame".
 (define* (push-frame! env frame)
   (set-env-parent! frame env)
   frame)

 (define* (push-frame env frame)
   (push-frame! env (copy frame)))

 (define* (push-stack! env stack)
   (aif parent (env-parent stack)
	(push-stack! env parent)
	(set-env-parent! stack env))
   stack)

 (define* (unshift-frame! env frame)
   (aif parent (env-parent env)
	(unshift-frame! parent frame)
	(set-env-parent! env frame)))

 ;; Adds an entry to the topmost frame.
 (define/kw* (add-binding! env name entry #:key is-func)
   (if (not (symbol? entry)) (error "entry must be a symbol"))
   (if is-func
       (alet f-list (lookup-overloads env name)
	     (set! f-list (cons entry f-list))
	     (set-env-names! env (h.store (env-names env) name f-list)))
       (set-env-names! env (h.store (env-names env) name entry)))
   env)

 (define* (add-binding env name entry . kw)
   (apply add-binding! (copy env) name entry kw))

 (define (lookup-name env name)
   (h.fetch (env-names env) name))

 (define* (lookup-name-r env name)
   (aif id (lookup-name env name)
	id
	(aif parent (env-parent env)
	     (lookup-name-r parent name)
	     #f)))

 (define (error-wrong-kind name)
   (error-f "name ~s is defined, but not of expected kind" name))

 (define (error-no-name name)
   (error-f "no binding named ~s in environment" name))

 (define (lookup-non-func env name)
   (aif id (lookup-name-r env name)
	(begin
	  ;;(write-nl (list "lookup" id))
	  (if (symbol? id)
	      id
	      (error-wrong-kind name)))
	#f))

 (define (lookup-overloads env name)
   (aif id-list (lookup-name env name)
	(if (list? id-list)
	    id-list
	    (error-wrong-kind name))
	'()))

 (define (lookup-overload-r env name match?)
   (define id-list (lookup-overloads env name))
   (define id (find-first id-list match?))
   (or id
       (aif parent (env-parent env)
	    (lookup-overload-r parent name match?)
	    #f)))

 ;; Returns all copies of the named function appearing in the
 ;; environment; the results are not compared for equality, so the
 ;; same function signature might appear in the result more than once.
 (define* (lookup-overloads-r env name)
   (define (f res env)
     (awhen id-list (lookup-name env name)
	    (when (list? id-list)
		  (set! res (append res id-list))))
     (aif parent (env-parent env)
	  (f res parent)
	  res))
   (f '() env))
   
 ;; Passed function type should define both return value type and
 ;; argument types, where applicable. Any types that are not known
 ;; should be specified as "tvar".
 (define (lookup-called-func env name functype td-texpr d-table)
   (lookup-overload-r 
    env name
    (lambda (id)
      (alet decl (get-decl-by-id d-table id)
	    (texpr-eq? (fget-reqd-nlist-elem-1 decl 'type)
		       functype 
		       #:td-texpr td-texpr)))))

 (define/kw* (lookup env name #:key func-type td-texpr d-table reqd)
   (alet res (if func-type
		 (lookup-called-func env name func-type td-texpr d-table)
		 (lookup-non-func env name))
	 (if (and reqd (not res))
	     (error-no-name name)
	     res)))

) ; end module

#;
(begin
  (require environment)
  (require (lib "util.scm" "wg"))
  (parameterize ((print-struct #t))
		(let ((env1 (new))
		      (env2 (new)))
		  (add-binding! env1 'a 'a-entry)
		  (add-binding! env2 'b 'b-entry)
		  (write-nl (push-frame env1 env2))
		  )))
		