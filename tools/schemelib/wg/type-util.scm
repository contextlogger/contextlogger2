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

;; Provides utilities for checking whether a given datum is of the
;; desired basic Scheme data type. Also provides utilities for
;; coercing Scheme data types to others.
(module 
 type-util
 mzscheme 

 (require (lib "usual.scm" "wg"))

 ;; Queries.

 (define* (true-boolean? x)
   (and (boolean? x) (true? x)))

 (define* (false-boolean? x)
   (and (boolean? x) (false? x)))

 (define* (string-of-length? x n)
   (and (string? x) (= (string-length x) n)))

 (define* (non-null? x)
   (not (null? x)))

 (define* (non-empty-list? x)
   (and (list? x) (non-null? x)))

 (define* (named-list? x)
   (and (non-empty-list? x) (symbol? (car x))))

 (define* (list-named? x name)
   (and (named-list? x) (eq? (car x) name)))

 (define* (list-named-one-of? x names)
   (and (named-list? x)
	(include-eq? names (car x))))

 ;; Enforcing checks.

 (define* (check-with f msg x)
   (if (f x) x (error (format msg x))))

 (define* (check-boolean x)
   (check-with boolean? "~s is not a boolean" x))

 (define* (check-number x)
   (check-with number? "~s is not a number" x))

 (define* (check-integer x)
   (check-with integer? "~s is not an integer" x))

 (define* (check-symbol x)
   (check-with symbol? "~s is not a symbol" x))

 (define* (check-string x)
   (check-with string? "~s is not a string" x))

 (define* (check-char x)
   (check-with char? "~s is not a character" x))

 (define* (check-list x)
   (check-with list? "~s is not a list" x))

 (define* (check-named-list x)
   (check-with named-list? "~s is not a named list" x))

 (define* (check-list-named x name)
   (check-with
    (lambda (x) (list-named? x name))
    (format "~~s is not a list named ~s" name) x))

 ;; Coercions.

 ;; Coerces a string-like object to a string.
 (define* (to-string obj)
   (cond ((string? obj) obj)
	 ((symbol? obj) (symbol->string obj))
         ((path? obj) (path->string obj))
	 (else (error (format "cannot coerce ~s to a string" obj)))))

 ;; Coerces a string-like object to a symbol.
 (define* (to-symbol obj)
   (cond ((symbol? obj) obj)
	 ((string? obj) (string->symbol obj))
         ((path? obj) (string->symbol (path->string obj)))
	 (else (error (format "cannot coerce ~s to a symbol" obj)))))

 ;; Coerces a character-like object to a character.
 (define* (to-char obj)
   (cond ((char? obj) obj)
	 ((integer? obj) (integer->char obj))
	 ((string-of-length? obj 1) (string-ref obj 0))
	 (else (error (format "cannot coerce ~s to a character" obj)))))

)


; (require (lib "util.scm" "wg"))
; (require type-util)
; (write-nl (check-integer 1.2))

