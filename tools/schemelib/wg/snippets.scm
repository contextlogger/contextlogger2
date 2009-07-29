;; 
;; snippets.scm
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

;; This API is mostly deprecated, and exists primarily for backward
;; compatibility. Preferably use code-print.scm instead.
(module 
 snippets
 mzscheme

 (require (lib "ast-util.scm" "wg"))
 (require (lib "usual.scm" "wg"))
 (require (lib "code-print.scm" "wg"))

 ;; This module assumes that #f, (text STRING), and (seq SNIPPET ...)
 ;; are snippet types known to the pretty printer.

 (define* no-snippet #f)

 (define* (to-snippet arg)
   (cond
    ((not arg) #f)
    ((string? arg)
     `(text ,arg))
    ((symbol? arg)
     `(text ,(to-string arg)))
    ((named-list? arg)
     ;; Assuming a snippet, as cannot know. Only the pretty printer
     ;; implementation does.
     arg)
    (else
     (error "cannot coerce to snippet" arg))))

 (define* (snippet-list . ll)
   `(seq ,@(map to-snippet ll)))

 (define* (snippet->string sn)
   (model->string sn))

 (define (separate ll sep)
   (cond ((null? ll) '())
	 ((null? (cdr ll)) ll)
	 (else (cons (car ll) (cons sep (separate (cdr ll) sep))))))

 (define (reject-no-snippet ll)
   (compact ll))

 (define* (snippet-join ll sep)
   `(seq ,@(separate (map to-snippet (reject-no-snippet ll))
                     (to-snippet sep))))

) ; end module

#;
(begin
  (require (lib "usual.scm" "wg"))
  (let ((s (open-output-string)))
    (parameterize ((current-output-port s))
      (display "hello"))
    (write-nl s)
    (write-nl (list 1 (get-output-string s))))
) ; end test code
