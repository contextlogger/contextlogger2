;; 
;; hash.scm
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

;; Provides a data type similar to Ruby's Hash. Does not support
;; nearly as many operations, though, but more can be added as
;; required. One notable difference is that the comparison function
;; may be freely chosen; it defaults to "eq?".
;; 
;; Example:
;; 
;;   (require (lib "util.scm" "wg"))
;;   (require (prefix hm. (lib "hash.scm" "wg")))
;; 
;;   (let ((cmap (hm.new)))
;;     (write-nl cmap)
;;     (write-nl (hm.has-key? cmap 'a))
;;     (set! cmap (hm.store cmap 'a 1))
;;     (write-nl cmap)
;;     (write-nl (hm.has-key? cmap 'a))
;;     (set! cmap (hm.store cmap 'b 2))
;;     (set! cmap (hm.store cmap 'b 3))
;;     (write-nl cmap)
;;     (set! cmap (hm.delete cmap 'b))
;;     (write-nl cmap)
;;     (write-nl (hm.fetch cmap 'a))
;;   )
(module 
 hash
 mzscheme
	
 (require (all-except (lib "usual.scm" "wg") delete))
	
 (define* new
   (case-lambda
    (() (cons eq? '()))
    ((cmp) (cons cmp '()))))

 ;; Returns a predicate that holds true iff the given element of
 ;; "cmap" matches "key".
 (define (get-pred cmap key)
   (lambda (elem) ((car cmap) (car elem) key)))

 (define (get-entry cmap key)
   (find-first (cdr cmap) (get-pred cmap key)))

 (define* (has-key? cmap key)
   (true? (get-entry cmap key)))

 (define* (has-key-error cmap key)
   (error (format "key ~s already in ~s" key cmap)))

 (define* (no-key-error cmap key)
   (error (format "no key ~s in ~s" key cmap)))

 (define* (check-has-key cmap key)
   (unless (fetch cmap key)
	   (no-key-error cmap key)))

 (define* (check-no-key cmap key)
   (when (fetch cmap key)
	 (has-key-error cmap key)))

 ;; Actually, in Ruby, the default action upon IndexError is to throw
 ;; an exception, but that is not the case here. See fetch-reqd
 ;; instead.
 (define* fetch
   (let ((f
	  (lambda (cmap key defval)
	    (let ((entry (get-entry cmap key)))
	      (if entry
		  (cadr entry)
		  defval)))))
     (case-lambda
      ((cmap key) (f cmap key #f))
      ((cmap key defval) (f cmap key defval)))))

 (define* (fetch-reqd cmap key)
   (if (has-key? cmap key)
       (fetch cmap key)
       (error "key not found" key)))

 (define (delete-first l f)
   (letrec 
       ((df 
	 (lambda (r l)
	   (if (null? l)
	       r
	       (if (f (car l))
		   (append r (cdr l))
		   (df (push r (car l)) (cdr l)))))))
     (df '() l)))

 (define* (delete cmap key)
   (cons (car cmap)
	 (delete-first (cdr cmap) (get-pred cmap key))))

 (define* (store cmap key value)
   (if (has-key? cmap key)
       (set! cmap (delete cmap key)))
   (cons (car cmap) (cons (list key value) (cdr cmap))))

 (define-syntax* store-set!
   (syntax-rules ()
     ((_ cmap key value)
      (set! cmap (store cmap key value)))))

 (define* (store-all cmap alist)
   (for-each
    (lambda (entry)
      (store-set! cmap (car entry) (cadr entry)))
    alist)
   cmap)

 (define* (all-keys cmap)
   (map first (cdr cmap)))

 (define* (all-values cmap)
   (map second (cdr cmap)))

 (define* (each cmap f)
   (for-each
    (lambda (pair) (f (first pair) (second pair)))
    (cdr cmap)))

 (define* (update map1 map2)
   (let ((cmap map1))
     (each map2
	   (lambda (key value)
	     (set! cmap (store cmap key value))))
     cmap))

)
