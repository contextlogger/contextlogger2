;; 
;; code-print.scm
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

;; Routines useful for printing source code. Mostly geared towards
;; imperative languages. The routines are mostly language-independent,
;; but there is support for some common comment styles, for instance.
;; 
;; Based on pp-simple. An instance of a pp-simple pretty printer
;; object must exist for printing.
(module
  code-print
  mzscheme

  (require (lib "usual.scm" "wg"))
  (require (lib "string-util.scm" "wg"))
  (require (prefix pp. "pp-simple.scm"))
  (require (lib "plt-match.ss"))
  
  ;;; General purpose.

  ;; Note that the argument order has been chosen for maximum
  ;; compatibility with for-each.
  (define* (for-each-sep sepact elemact list)
    (define first #t)
    (for-each
     (lambda (elem)
       (if first
           (set! first #f)
           (when sepact (sepact)))
       (when elemact (elemact elem)))
     list))

  ;;; Additional printing functions.
  
  (define (py-docstring pp s)
    (pp.text-nl pp "\"\"\"")
    (pp.text-with-nl pp (rstrip s))
    (pp.nl pp)
    (pp.text pp "\"\"\""))

  (define (py-comment pp s)
    (pp.indent pp
               (thunk (pp.text-with-nl pp (rstrip s)))
               #:label "# "))
  
  (define (c-block-comment pp s)
    (pp.text pp "/** ")
    (pp.align pp (thunk (pp.text-with-nl pp (rstrip s))))
    (pp.nl pp)
    (pp.text pp " */"))

  (define* (print-banner pp s)
    (pp.text-nl pp "---------------")
    (pp.text-nl pp s)
    (pp.text-nl pp "- - - - - - - -"))

  ;;; Model printing.

  ;; Checks if there is anything in the model that results in output.
  (define* (model-matters? model)
    ;; xxx not 100% accurate at present
    (match
     model
     (#f #f)
     ((list 'seq m ...) (list-memb-match? m model-matters?))
     ((list 'sep _ m ...) (list-memb-match? m model-matters?))
     (_ #t)))
    
  ;; This function compacts more aggressively than "compact". Namely,
  ;; it ensures that empty sequences do not count as something
  ;; significant enough to justify inserting a separator.
  (define* (compact-model-list ll)
   (filter model-matters? ll))
  
  (define* (model->string sn . op)
    (let* ((port (open-output-string))
           (pp (apply pp.new #:port port op)))
      (print-model pp sn)
      (get-output-string port)))
  
  ;; The code emitter of the compiler is expected to produce a data
  ;; structure that can be processed here. The model is designed to be
  ;; easy to produce with quasiquoting, and only concerns printing,
  ;; not language semantics.
  ;; 
  ;; The reason for supporting a low-level model like this is that
  ;; using one gives us an extra level of indirection, which is
  ;; helpful for getting the separators right, for instance. To
  ;; further assist in this, we ignore false values in lists. Another
  ;; useful thing here is that we then do not need to explicitly pass
  ;; around the pretty-printer object, nor do we need to use monads or
  ;; something equally convoluted to pass around that object.
  (define* (print-model pp model)
    (define text (fix pp.text pp))
    (define text-nl (fix pp.text-nl pp))
    (define nl (fix pp.nl pp))
    (define indent (fix pp.indent pp))
    (define exdent (fix pp.exdent pp))

    (define (print-list ll)
      (for-each print ll))

    (define (print-sep-list ll sepact)
      (for-each-sep sepact print (compact-model-list ll)))
    
    (define (print model)
      (match
       model
       (#f void)
       ((list 'seq m ...) (print-list m))
       ((list 'sep 'nl m ...)
        (print-sep-list m nl))
       ((list 'sep 'skip m ...)
        (print-sep-list m (thunk (nl) (nl))))
       ((list 'sep 'comma m ...)
        (print-sep-list m (thunk (text ", "))))
       ((list 'sep 'space m ...)
        (print-sep-list m (thunk (text " "))))
       ((list 'sep 'comma-nl m ...)
        (print-sep-list m (thunk (text ",") (nl))))
       ((list 'sep 'comma-skip m ...)
        (print-sep-list m (thunk (text ",") (nl) (nl))))
       ((list 'sep 'equ m ...)
        (print-sep-list m (thunk (text " = "))))
       ((list 'text s) (text s))
       ((list 'text-append s ...) (text (string-concat s)))
       ((list 'text-nl s) (text-nl s))
       ((list 'text-with-nl s) (pp.text-with-nl pp s))
       ((list 'nl) (nl))
       ((list 'indent m ...)
        (indent (thunk (print-list m))))
       ((list 'exdent m ...)
        (exdent (thunk (print-list m))))
       ((list 'indent0 m ...)
        (pp.indent0 pp (thunk (print-list m))))
       ((list 'py-docstring s)
        (py-docstring pp s))
       ((list 'py-comment s)
        (py-comment pp s))
       ((list 'c-block-comment s)
        (c-block-comment pp s))
       ((list 'c-block sep-kind body-list ...)
        (begin
          (set! body-list (compact-model-list body-list))
          ;; This recursive definition makes it easier to say what we
          ;; want, at the cost of efficiency.
          (print
           `(seq
             (text "{")
             (nl)
             ,(and (not (null? body-list))
                   `(seq
                     (indent
                      (sep ,sep-kind
                           ,@body-list))
                     (nl)))
             (text "}")))
          ))
       ))

    (print model))
  
  ) ; end module
