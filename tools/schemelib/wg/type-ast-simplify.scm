;; 
;; type-ast-simplify.scm
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
 type-ast-simplify 
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))

 ;; Strips out annotations from the passed type expression, and
 ;; inlines each type attribute into its parent element, as without
 ;; annotations the extra level of indirection is mostly unneccessary
 ;; (in cases where an element has more than one type attribute,
 ;; position must be assigned meaning, though). Doing so takes away
 ;; annotatibility, but may improve readability, and makes things like
 ;; pattern matching easier.
 (define* (simplify-texpr ast)
   ;;(write-nl ast)
   (cond

    ((list-named? ast 'type)
     (simplify-texpr (second ast)))

    ((list-named-one-of? ast '(ptr-to ref-to const volatile class class-member returns))
     (list (car ast)
	   (simplify-texpr (get-reqd-nlist-elem-1 ast 'type))))

    ((list-named? ast 'array-of)
     (let ((new-ast `(array-of ,(simplify-texpr (get-reqd-nlist-elem-1 ast 'type)))))
       (awhen elem (get-opt-nlist-elem-1 ast 'num-elems)
 	      (set! new-ast (push new-ast elem)))
       new-ast))

    ((list-named? ast 'tname-ref)
     (list 'tname-ref (get-reqd-nlist-elem-1 ast 'name)))

    ((list-named? ast 'ptr-to-member)
     (let* ((class (get-reqd-nlist-elem-1 ast 'class))
	    (member (get-reqd-nlist-elem-1 ast 'class-member)))
       (list 'ptr-to-member (simplify-texpr class) (simplify-texpr member))))

    ;; Note that for ctors and dtors, the return type component is 'none.
    ((list-named? ast 'func-t)
     (list 
      'func-t
      (alet args (get-reqd-nlist-elem ast 'args)
	    (simplify-texpr args))
      (aif returns (get-opt-nlist-elem ast 'returns)
	   (alet rtype (get-reqd-nlist-elem-1 returns 'type)
		 (simplify-texpr rtype))
	   'none)))

    ((list-named? ast 'args)
     (map simplify-texpr (cdr ast)))

    ((list-named? ast 'arg)
     (simplify-texpr (get-reqd-nlist-elem-1 ast 'type)))

    ((list-named? ast 'tvar)
     ast)

    (else (error "not a valid texpr component" ast))))

 (define* (simplify-telem ast)
   (cond

    ((list-named? ast 'type)
     (list 'type (simplify-texpr (second ast))))

    (else (error "not a type element" ast))))

) ; end module


; Note that "#;" comments out the next datum.
#;
(begin
  (require (lib "util.scm" "wg"))
  (require (lib "miso-cxx.scm" "wg"))
  (require type-ast-simplify)

  (define (do-prog prog)
    (let ((ast (parse-c prog)))
      (pretty-nl ast)
      (pretty-nl (simplify-telem ast))))

  (do-prog '(type (ptr-to char)))
  (do-prog '(type (array-of int)))
  (do-prog '(type (array-of int 555)))
  (do-prog '(type (ptr-to-member CFoo void)))
  (do-prog '(type (const (ref-to (array-of (ptr-to char) 99)))))
  (do-prog '(type (func-t ((arga int) (arga char) (arga (const (ptr-to char)))) void)))
)