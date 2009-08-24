#lang scheme

#|

This file defines the API that a variant specification must implement.

There can be variation in feature sets and target platforms, for
example. Variation in toolchains typically would not be reflected
here, unless such variation means that the actual code of the project
must be different (as opposed to just using a different set of build
scripts).

The variant specifications should strive to be fairly high-level, so
that only the actual variable choices can be made there. A
specification should pretty much be just a set of key/value pairs,
which we call attributes.

To avoid duplication in specifications, inheritance is supported.
Each variant spec should be an instance of a subclass of variant%.
Macros are provided for making it easier to define such subclasses.

|#

(require common/usual-4)
(require common/class)

(define* current-variant (make-parameter #f))

;; There is nothing here really, as a variant specification really is
;; project specific. However, a common baseclass for variants may turn
;; out to be of use.
(define* variant%
  (class object%
    (inspect #f) ;; transparent
    
    (field (name #f))
    
    (super-new)

    (define/public (variant-name.attr)
      (symbol->string name))

    ;; This can be useful for more reflective configuration setups.
    ;; Note, though, that these attributes can still be overridden
    ;; with .attr methods.
    (define/public (get-attrs)
      #hasheq())
    ))

(define-syntax* variant-class
  (syntax-rules ()
    ((_ super body ...)
     (class super
       (inspect #f)
       body ...))))

(define-syntax* define-variant
  (syntax-rules ()
    ((_ name super body ...)
     (define name
       (class super
         (inspect #f)
         body ...)))))

(define-syntax* define-variant*
  (syntax-rules ()
    ((_ name rest ...)
     (begin
       (define-variant name rest ...)
       (provide name)))))

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

(define attr-getter-re "^(.*)[.]attr$")

(define (variant-attr-names variant)
  ;; We could also consider using (get-fields/hasheq variant) if it
  ;; were useful, but for now only particularly named methods are
  ;; expected to return attribute values.
  (let ((mnames 
         (interface->method-names (object-interface variant))))
    (for/fold
     ((res '()))
     ((mname mnames))
     (aif m (regexp-match attr-getter-re (symbol->string mname))
          (cons (string->symbol (second m)) res) res))))

(define* (variant-attr-values/hasheq variant)
  (let ((mnames 
         (interface->method-names (object-interface variant)))
        ;;(res (make-hasheq))
        (res (hash-copy (send variant get-attrs)))
        )
    (for
     ((mname mnames))
     (awhen m (regexp-match attr-getter-re (symbol->string mname))
            (hash-set! res 
                       (string->symbol (second m))
                       (call-method variant mname))))
    res))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define* (variant-attr-values/sorted variant)
  (let* ((h (variant-attr-values/hasheq variant))
         (lst (hash-map h (lambda (k v) (list k v)))))
    (sort lst symbol<? #:key first)))

(define-struct* hexnum (num))

