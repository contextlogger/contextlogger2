#lang scheme

(require common/class)
(require common/usual-4)
(require "util.ss")

;; These values provide a way to indicate whether an attribute is
;; defined or not, without specifying any actual value. Sometimes one
;; only cares about whether something is defined or not -- consider
;; #ifdef __EPOC32__.
(define-struct attr-defined% ())
(define-struct attr-undefined% ())

(define* attr-defined (make-attr-defined%))
(define* attr-undefined (make-attr-undefined%))

(define* attr-defined? attr-defined%?)
(define* attr-undefined? attr-undefined%?)

(define (attr-getter-name attr-name)
  (string->symbol
   (string-append (symbol->string attr-name) ".attr")))

(define attr-getter-re #rx"^(.*)[.]attr$")

;; We could also consider using (get-fields/hasheq object) if it were
;; useful, but for now only particularly named methods are expected to
;; return attribute values. Also, get-attrs may return multiple (key
;; value) attribute pairs as a hasheq.

(define (object-attr-method-pairs object)
  (let ((mnames (interface->method-names (object-interface object))))
    (for/fold
     ((res '()))
     ((mname mnames))
     (aif m (regexp-match attr-getter-re (symbol->string mname))
          (cons (list (string->symbol (second m)) mname) res)
          res))))

(define (object-attr-method-names object)
  (map second (object-attr-method-pairs object)))

;; Does not include any names returned by any get-attrs method.
(define (object-attr-names/no-get-attrs object)
  (map first (object-attr-method-pairs object)))

(define (get-attrs object)
  (if (respond-to? object get-attrs)
      (send object get-attrs)
      #hasheq()))

(define* (object-attr-names object)
  (let ((res (hash-copy (get-attrs object))))
    (for-each
     (lambda (name) (hash-set! res name #t))
     (object-attr-names/no-get-attrs object))
    (hash-map res (lambda (k v) k))))

(define* (object-has-attr? object attr-name)
  (or (true? (hash-ref (get-attrs object) attr-name #f))
      (let ((method-name (attr-getter-name attr-name)))
        (has-method? object method-name))))

(define* (object-get-attr object attr-name (fail #f))
  (let ((method-name (attr-getter-name attr-name)))
    (if (has-method? object method-name)
        (call-method object method-name)
        (hash-ref (get-attrs object) attr-name fail))))

(define* (object-attr-values/hasheq object)
  (let ((pairs (object-attr-method-pairs object))
        (res (hash-copy (get-attrs object))))
    (for
     ((pair pairs))
     (let ((name (first pair))
           (mname (second pair)))
       (hash-set! res name (call-method object mname))))
    res))

(define* (object-attr-values/sorted object)
  (let* ((h (object-attr-values/hasheq object))
         (lst (hash-map h (lambda (k v) (list k v)))))
    (sort lst symbol<? #:key first)))

#|

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
