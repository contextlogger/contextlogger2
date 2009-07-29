#lang scheme

(provide
 ;; A summary of generic functions and macros relating to boolean
 ;; conditions and predicates. Note that some of these are built in to
 ;; Scheme, and hence commented out here.

 true?
 ;;false? already defined in "scheme"
 
 ;; Basic logical operators.

 identity ;; procedure
 ;; not ;; procedure
 ;; and ;; syntax
 ;; or ;; syntax
 nor ;; syntax
 nand ;; syntax
 xor ;; procedure

 all-true ;; procedure
 one-true ;; procedure
 some-true ;; procedure
 none-true ;; syntax (not some-true)
 
 ;; Applying a predicate to multiple values.

 for-all ;; procedure
 for-some ;; procedure
 for-none ;; syntax (not exists)

 ;; Applying multiple predicates to a value.

 all-hold ;; procedure
 some-hold ;; procedure
 none-hold ;; syntax (not some-hold)

 pred-or ;; syntax
 pred-and ;; syntax
 
 ;; Functions for creating logical operators. In this context, do not
 ;; forget our "fix" macro, which should we very useful in
 ;; constructing predicates out of things like for-all, all-hold, etc.

 ;; compose ;; procedure
 identity ;; procedure
 ;; negate ;; procedure
)

(require (only-in (lib "util.scm" "wg")
                  identity nor nand true? false?
                  (any-pred pred-or) (every-pred pred-and)))

(define (xor . xs)
  (one-true xs))

(define (all-true xs)
  (if (null? xs)
      #t
      (if (car xs)
          (all-true (cdr xs))
          #f)))

(define (one-true xs)
  (if (null? xs)
      #f
      (if (car xs)
          (none-true (cdr xs))
          (one-true (cdr xs)))))

(define (some-true xs)
  (if (null? xs)
      #f
      (if (car xs)
          #t
          (some-true (cdr xs)))))

(define-syntax none-true
  (syntax-rules ()
    ((_ x)
     (not (some-true x)))))

(define (for-all p xs)
  (if (null? xs)
      #t
      (if (p (car xs))
          (for-all p (cdr xs))
          #f)))

(define (for-some p xs)
  (if (null? xs)
      #f
      (if (p (car xs))
          #t
          (for-all p (cdr xs)))))

(define-syntax for-none
  (syntax-rules ()
    ((_ p xs)
     (not (for-some p xs)))))

(define (all-hold ps x)
  (if (null? ps)
      #t
      (if ((car ps) x)
          (all-hold (cdr ps) x)
          #f)))

(define (some-hold ps x)
  (if (null? ps)
      #f
      (if ((car ps) x)
          #t
          (some-hold (cdr ps) x))))

(define-syntax none-hold
  (syntax-rules ()
    ((_ ps x)
     (not (some-hold ps x)))))
