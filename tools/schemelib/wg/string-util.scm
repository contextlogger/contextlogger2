;; 
;; string-util.scm
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
  string-util
  mzscheme

  (require (lib "usual.scm" "wg"))
  (require (lib "string.ss"))

  (define* (rstrip s)
    (define (f chars)
      (cond
       ((null? chars) "")
       ((char-whitespace? (car chars)) (f (cdr chars)))
       (else (list->string (reverse chars)))))

    (f (reverse (string->list s))))

  (define* (string-drop-from-end n s)
    (substring s 0 (- (string-length s) n)))

  (define* (string-split-space s)
    (regexp-split #rx" +" s))

  (define* (string-capitalize s)
    (if (= (string-length s) 0)
        s
        (string-append
         (string-upcase (substring s 0 1))
         (substring s 1 (string-length s)))))

  (define* (string-hyphens-to-underscores s)
    (regexp-replace* "-" s "_"))
  
  (define tab-column 8)
  
  (define (next-tab-column n)
    (* (+ (quotient n tab-column) 1) tab-column))

  (define/kw* (text-width s #:optional (initlv 0))
    (define (end-column column chlist)
      (cond
       ((null? chlist) column)
       ((equal? (car chlist) #\tab) (end-column (next-tab-column column) (cdr chlist)))
       (else (end-column (+ column 1) (cdr chlist)))))
    
    (let ((chars (string->list s)))
      (- (end-column initlv chars) initlv)))

) ; end module

#;
(begin
  (require string-util)
  (require "usual.scm")
  (write-nl (string-drop-from-end 0 "foobar"))
  (write-nl (string-drop-from-end 6 "foobar"))
  (write-nl (string-drop-from-end 2 "foobar"))
  ;;(write-nl (string-drop-from-end 7 "foobar"))
  ;;(write-nl (string-drop-from-end -1 "foobar"))
  (write-nl (string-split-space "foo   bar baz"))
  (write-nl (string-capitalize "foobar"))
  (write-nl (string-capitalize ""))

  (write-nl (text-width "foo"))
  (write-nl (text-width "bar" 1))
  (write-nl (text-width "\t"))
  (write-nl (text-width "\t" 2))
  )
  