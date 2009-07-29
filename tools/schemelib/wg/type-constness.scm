;; 
;; type-constness.scm
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
 type-constness
 mzscheme

 (require "usual.scm")
 (require "ast-util.scm")

 ;; How to determine if a variable is a constant? A value of a basic
 ;; type is const if declared const. An array is const if declared
 ;; const. A variable may not have a function type since functions
 ;; cannot be assigned to variables. A variable may be of a function
 ;; pointer type, and in that case the constness of the function
 ;; arguments or return values does not matter. A pointer is const if
 ;; declared const. Also a pointer to a pointer is const if declared
 ;; const. There can be no reference references, and presumably a
 ;; reference to a const type is const, and a reference to a non-const
 ;; type is non const.
 ;; 
 ;; Why would we need to know something like this? Well, if a variable
 ;; is a constant, it will not get exported (apparently). Thus, one
 ;; has to put the variable in a header file, and initialize it there.
 ;; (Constants must be initialized, so if we do not, and we must
 ;; unless using "extern", the compiler will eventually complain about
 ;; an undefined constant if we refer to it.) But we do not want to go
 ;; initializing a non-const variable in any header. So we must know
 ;; constness to make correct decisions about what to put in a header
 ;; file, and what not to put there.
 (define* (type-is-const? ast^)
   (letrec
       ((get-type
	 (lambda (ast)
	   (get-reqd-nlist-memb-1 ast 'type)))

	(trav
	 ;; c:: Indicates whether there was a just preceding "const".
	 (lambda (c ast)
	   (cond
	    
	    ((list-named? ast 'const)
	     (trav #t (get-type ast)))

	    ;; No effect to constness.
	    ((list-named-one-of? ast '(ref-to volatile))
	     (trav c (get-type ast)))

            ;; For a pointer to be truly const, the pointer itself
            ;; must be const, and it must point to something const.
	    ((list-named? ast 'ptr-to)
	     (if c
		 (trav #f (get-type ast))
		 #f))

	    ;; An array type is const if its elements are.
	    ((list-named? ast 'array-of)
	     (trav #f (get-type ast)))
	    
	    ;; Named types also require a preceding const, although I
	    ;; guess we would need to resolve typedefs to know that the
	    ;; name in itself does not refer to a const type. Let us
	    ;; assume that that has been done.
	    ((list-named? ast 'tname-ref)
	     c)

	    ;; A function type is constant, since in C++ you cannot
	    ;; assign another function to a name naming a function.
	    ((list-named? ast 'func-t)
	     #t)

	    (else
	     (error "illegal type" ast)))))
	)

     (if (list-named? ast^ 'type)
	 (set! ast^ (cadr ast^)))

     ;;(write-nl ast^)
     (trav #f ast^)))

)
