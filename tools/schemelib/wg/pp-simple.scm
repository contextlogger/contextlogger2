;; 
;; pp-simple.scm
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

;; A very simple pretty-printing library. Tailored to our present
;; needs. Just takes care of proper indentation, and does nothing else
;; really. But the "algorithm" is efficient. In particular, there is
;; no logic for automated line-breaking. In an ideal world the editor
;; should take care of soft line breaking as required.
;; 
;; In short, the features are: Supports "indent", but not "nest". Does
;; not support soft line breaking. Supports labels (in indentation)
;; and alignment. Optionally allows newlines in strings to print,
;; provided the appropriate function is used.
(module pp-simple mzscheme
  (require (lib "usual.scm" "wg"))

  ;;; Primitives.
  
  ;; Indentation is specified as number of steps, and this parameter
  ;; defines how many spaces per step to insert.
  (define* current-step (make-parameter 2))

  (define* current-newline (make-parameter "\n"))

  (define-struct ps
    (
     port ;; output port
     newline ;; newline string
     step ;; indentation step integer
     commit ;; whether indentation string has been committed
     istack ;; indentation as a string stack
     istring ;; indentation string
     ilevel ;; indentation level as an integer
     column ;; current column as an integer
     ) #f)

  (define/kw* (new #:key
                   (port (current-output-port))
                   (newline (current-newline))
                   (step (current-step)))
    (make-ps port newline step #f '() "" 0 0))

  (define (set-istack state stack)
    (let* ((string (apply string-append (reverse stack)))
           (level (string-length string)))
      (set-ps-istack! state stack)
      (set-ps-istring! state string)
      (set-ps-ilevel! state level)))

  (define (istack-push state string)
    (define stack (ps-istack state))
    (set-istack state (cons string stack)))

  ;; Returns the popped element.
  (define (istack-pop state)
    (define stack (ps-istack state))
    (when (null? stack)
      (error "no indentation to pop" stack))
    (alet elem (car stack)
          (set-istack state (cdr stack))
          elem))

  ;; Swaps stacks, returning the old stack.
  (define (istack-swap state new-stack)
    (define stack (ps-istack state))
    (set-istack state new-stack)
    stack)
    
  (define (indent-string state string chars steps)
    (cond
     (string string)
     (chars (spaces chars))
     (else
      (spaces (* (or steps 1) (ps-step state))))))

  (define* (level state)
    (ps-ilevel state))
  
  (define* (column state)
    (ps-column state))
  
  (define* (nl state)
    (display (ps-newline state) (ps-port state))
    (set-ps-commit! state #f)
    (set-ps-column! state 0))

  (define (spaces n)
    (make-string n #\space))

  (define (inc-column state n)
    (set-ps-column! state (+ (ps-column state) n)))

  (define (display-string state string)
    (display string (ps-port state))
    (inc-column state (string-length string)))
  
  (define (print-indent state)
    (display-string state (ps-istring state)))

  (define* (text state string)
    (unless (ps-commit state)
      (print-indent state)
      (set-ps-commit! state #t))
    (display-string state string))
  
  (define/kw* (indent state
                      #:optional f
                      #:key steps chars label)
    (define string (indent-string state label chars steps))
    (istack-push state string)
    (when f
      (f)
      (istack-pop state)))

  (define/kw* (exdent state
                      #:optional f)
    (define string (istack-pop state))
    (when f
      (f)
      (istack-push state string)))

  (define/kw* (indent0 state #:optional f)
    (define keep-stack (istack-swap state '()))
    (when f
      (f)
      (istack-swap state keep-stack)))

  ;;; Convenience functions.

  (define* (text-nl state string)
    (text state string)
    (nl state))

  (define* (text-with-nl state input-string)
    (define chars (string->list input-string))

    (define buffer '())

    (define (bpush char)
      (set! buffer (cons char buffer)))
    
    (define (flush)
      (unless (null? buffer)
        (text state (apply string (reverse buffer)))
        (set! buffer '())))
    
    (for-each
     (lambda (char)
       ;;(write-nl char)
       (cond
        ((char=? char #\return) void)
        ((char=? char #\newline) (begin (flush) (nl state)))
        (else (bpush char))))
     chars)
    (flush))
  
  (define/kw* (align state #:optional f)
    (define chars (- (column state) (level state)))
    (indent state f #:chars chars))
  
  ) ; end module

#;
(begin
  (require (lib "usual.scm" "wg"))
  (require (prefix pp. pp-simple))

  (define pp (pp.new))
  (define text (fix pp.text pp))
  (define text-nl (fix pp.text-nl pp))
  (define nl (fix pp.nl pp))
  (define indent (fix pp.indent pp))
  (define exdent (fix pp.exdent pp))

  (define-syntax inthunk
    (syntax-rules ()
      ((_ code ...)
       (indent (thunk code ...)))))
  
  (define-syntax exthunk
    (syntax-rules ()
      ((_ code ...)
       (exdent (thunk code ...)))))
  
  (text "first hello") (nl)
  (indent)
    (text "indented") (nl)
    (indent) (text "indented more") (nl) (exdent)
  (exdent) (text "indented less") (nl) (indent)
    (text "indented") (nl)
  (exdent)

  (text "second hello") (nl)
  (indent
   (thunk
    (text "indented") (nl)
    (indent (thunk (text "indented more") (nl)))
    (exdent (thunk (text "indented less") (nl)))
    (text "indented") (nl)
    ))

  (text-nl "third hello")
  (inthunk
    (text-nl "indented")
    (inthunk (text-nl "indented more"))
    (exthunk (text-nl "indented less"))
    (text-nl "indented")
    )

  (text-nl "fourth hello")
  (inthunk
    (text "indented: ")
    (pp.align pp
              (thunk
               (text-nl "aligned line")
               (text-nl "another aligned line")))
    (text-nl "indented")
    )

  (text-nl "fifth")
  (inthunk
    (text-nl "indented")
    (indent (thunk
             (text-nl "commented line")
             (text-nl "another commented line"))
            #:label "# ")
    (text-nl "indented")
    )

  (text-nl "sixth")
  (text-nl "{")
  (inthunk
   (text-nl "goto label;")
   (exthunk (text-nl "label:"))
   (text-nl "Function();"))
  (text-nl "}")

  (text-nl "seventh")
  (inthunk
   (text-nl "indented")
   (indent
    (thunk
     (pp.text-with-nl pp "This\nis commented\r\ntext!\n"))
    #:label "// ")
   (text-nl "this no longer is commented"))

  ) ; end test code
